use crate::{dma, RspError, RspResult};
use mips_lifter::target::rsp::{
    register::{
        control::{
            DmaBusy, DmaFull, DmaRdramAddress, DmaReadLength, DmaSpAddress, DmaWriteLength,
            InterruptRequest, MemoryBank, Semaphore, Status,
        },
        special::ProgramCounter,
    },
    ControlRegisterBank, SpecialRegisterBank,
};
use n64_common::{
    log::{info, trace},
    memory::Section,
    InterruptDevice, SideEffects,
};

#[derive(Debug)]
pub(crate) struct Registers {
    /// The control registers, exposed as coprocessor registers to the RSP.
    pub control: ControlRegisterBank,
    /// Special registers such as the program counter. Not exposed to the RSP.
    pub special: SpecialRegisterBank,
    /// The state of any DMA transfers in progress.
    dma_states: [Option<dma::State>; 2],
}

impl Registers {
    const CURRENT: usize = 0;

    pub fn new(control: ControlRegisterBank, special: SpecialRegisterBank) -> Self {
        Self {
            control,
            special,
            dma_states: [None, None],
        }
    }

    pub fn read(&mut self, offset: usize) -> RspResult<u32> {
        trace!("reading RSP register at {offset:#x}");
        Ok(match offset {
            DmaSpAddress::OFFSET => u32::from(self.control.read_parsed::<DmaSpAddress>()),
            DmaRdramAddress::OFFSET => u32::from(self.control.read_parsed::<DmaRdramAddress>()),
            DmaReadLength::OFFSET => u32::from(self.control.read_parsed::<DmaReadLength>()),
            DmaWriteLength::OFFSET => u32::from(self.control.read_parsed::<DmaWriteLength>()),
            Status::OFFSET => u32::from(self.control.read_parsed::<Status>()),
            DmaBusy::OFFSET => u32::from(self.control.read_parsed::<Status>().dma_busy()),
            DmaFull::OFFSET => u32::from(self.control.read_parsed::<Status>().dma_full()),
            Semaphore::OFFSET => u32::from(self.control.read_write_semaphore()),
            ProgramCounter::OFFSET => self.special.read_program_counter(),
            _ => Err(RspError::RegisterOffsetOutOfBounds { offset })?,
        })
    }

    pub fn write(&mut self, offset: usize, value: u32) -> RspResult<SideEffects> {
        trace!("writing to RSP register at {offset:#x}: {value:#x}");
        let mut effects = SideEffects::default();
        match offset {
            DmaSpAddress::OFFSET => {
                self.control.write_parsed::<DmaSpAddress>(value.into());
            }

            DmaRdramAddress::OFFSET => {
                self.control.write_parsed::<DmaRdramAddress>(value.into());
            }

            DmaReadLength::OFFSET => {
                self.control.write_parsed::<DmaReadLength>(value.into());
                self.queue_dma(dma::Direction::ToSpMemory);
            }

            DmaWriteLength::OFFSET => {
                self.control.write_parsed::<DmaWriteLength>(value.into());
                self.queue_dma(dma::Direction::ToRdram);
            }

            Status::OFFSET => {
                const DEV: InterruptDevice = InterruptDevice::Rsp;
                let mut status: Status = self.control.read_parsed();
                effects = match status.write(value) {
                    Some(InterruptRequest::Raise) => effects.with_raise_interrupt(DEV),
                    Some(InterruptRequest::Lower) => effects.with_lower_interrupt(DEV),
                    None => effects,
                };
                info!("CPU wrote RSP status register: {status:#x?}");
                self.control.write_parsed(status);
            }

            DmaFull::OFFSET | DmaBusy::OFFSET => {
                // Read-only mirrors of the status register
            }

            Semaphore::OFFSET => self.control.clear_semaphore(),
            ProgramCounter::OFFSET => self.special.write_program_counter(value),
            _ => Err(RspError::RegisterOffsetOutOfBounds { offset })?,
        }
        Ok(effects)
    }

    pub(crate) fn queue_dma(&mut self, direction: dma::Direction) {
        let mut status: Status = self.control.read_parsed();
        let is_busy = status.dma_busy();
        if status.dma_full() {
            // Ignore the request, there is already a DMA transfer in progress and one pending
            return;
        } else if is_busy {
            status.set_dma_full(true);
        } else {
            status.set_dma_busy(true);
        }
        self.control.write_parsed(status);

        let index = usize::from(is_busy);
        debug_assert!(self.dma_states[index].is_none());
        self.dma_states[index] = Some(dma::State {
            direction,
            cycles: {
                let len = match direction {
                    dma::Direction::ToRdram => {
                        self.control.read_parsed::<DmaWriteLength>().length()
                    }
                    dma::Direction::ToSpMemory => {
                        self.control.read_parsed::<DmaReadLength>().length()
                    }
                };
                (len / 3) as usize // ~3 bytes are transferred per cycle
            },
        });
    }

    pub fn tick(&mut self, ctx: &mut dma::TickContext) -> RspResult<SideEffects> {
        // Transfer in chunks of 8 bytes so we can both account for wrapping, and (in the future) support count/skip
        const CHUNK: usize = 8;
        let mut effects = SideEffects::default();
        let Some(mut state) = self.dma_states[Self::CURRENT].take() else {
            return Ok(effects);
        };

        if !state.tick_is_ready(ctx) {
            self.dma_states[Self::CURRENT] = Some(state);
            return Ok(effects);
        }

        self.dma_finished_update_status();
        let mut read_length: DmaReadLength = self.control.read_parsed();
        let mut write_length: DmaWriteLength = self.control.read_parsed();
        let mut rdram_address: DmaRdramAddress = self.control.read_parsed();
        let mut sp_address: DmaSpAddress = self.control.read_parsed();

        let bytes = match state.direction {
            dma::Direction::ToRdram => {
                let addr = rdram_address.address() as usize;
                let len = write_length.length() as usize;
                let range = addr..addr + len;
                effects.set_dirty_section(Section::RdramMemory, range);
                len
            }
            dma::Direction::ToSpMemory => {
                let addr = sp_address.address() as usize;
                let len = read_length.length() as usize;
                let range = addr..addr + len;
                {
                    let section = match sp_address.bank() {
                        MemoryBank::IMem => {
                            ctx.cpu.invalidate_imem(range.clone());
                            Section::RspIMemory
                        }
                        MemoryBank::DMem => Section::RspDMemory,
                    };
                    effects.set_dirty_section(section, range);
                }
                len
            }
        };

        for _ in 0..(bytes / CHUNK) {
            let bank = sp_address.bank();
            let rdram_slice = &mut ctx.rdram[rdram_address.address() as usize..][..CHUNK];
            let sp_slice = &mut ctx.memory.write(bank)?[sp_address.address() as usize..][..CHUNK];

            let (src, dst) = match state.direction {
                dma::Direction::ToRdram => {
                    write_length.decrement();
                    (sp_slice, rdram_slice)
                }

                dma::Direction::ToSpMemory => {
                    read_length.decrement();
                    (rdram_slice, sp_slice)
                }
            };

            dst.copy_from_slice(src);
            rdram_address.increment();
            sp_address.increment();
            self.control.write_parsed(rdram_address);
            self.control.write_parsed(sp_address);
        }

        write_length.decrement(); // TODO: verify this
        self.control.write_parsed(write_length);
        self.control.write_parsed(read_length);

        Ok(effects)
    }

    fn dma_finished_update_status(&mut self) {
        let mut status: Status = self.control.read_parsed();
        if status.dma_full() {
            status.set_dma_full(false);
            // Swap which DMA state is active (index zero)
            std::mem::swap(
                &mut self.dma_states[0].take(),
                &mut self.dma_states[1].take(),
            );
        } else {
            status.set_dma_busy(false);
        }
        self.control.write_parsed(status);
    }
}
