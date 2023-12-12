use self::definitions::{
    DmaRdramAddress, DmaReadLength, DmaSpAddress, DmaWriteLength, SpDmaBusy, SpDmaFull,
    SpSemaphore, SpStatus,
};
use crate::{dma, MemoryBank, RspError, RspResult, SideEffects};

mod definitions;

#[derive(Debug)]
struct DoubleBuffered<T> {
    current: T,
    next: T,
}

impl<T> DoubleBuffered<T> {
    fn get_next_if(&mut self, next: bool) -> &mut T {
        if next {
            &mut self.next
        } else {
            &mut self.current
        }
    }

    fn next(&mut self) {
        // This isn't very efficient for large structs, but its not a problem for our usecase
        std::mem::swap(&mut self.current, &mut self.next);
    }
}

impl<T: Default> Default for DoubleBuffered<T> {
    fn default() -> Self {
        Self {
            current: T::default(),
            next: T::default(),
        }
    }
}

#[derive(Debug, Default)]
struct DmaRegisters {
    state: Option<dma::State>,

    sp_address: DmaSpAddress,
    dram_address: DmaRdramAddress,
    read_length: DmaReadLength,
    write_length: DmaWriteLength,
}

impl DmaRegisters {
    pub fn start_dma(&mut self, direction: dma::Direction) {
        debug_assert!(self.state.is_none(), "DMA transfer already in progress");
        self.state = Some(dma::State {
            // ~3 bytes are transferred per cycle
            cycles: (match direction {
                dma::Direction::ToRdram => self.write_length.length(),
                dma::Direction::ToSpMemory => self.read_length.length(),
            } / 3) as usize,
            direction,
        });
    }

    pub fn tick(&mut self, ctx: &mut dma::TickContext) -> Option<SideEffects> {
        let Some(state) = &mut self.state else {
            // No DMA transfer in progress
            return None;
        };

        if !state.tick_is_ready(ctx) {
            // The transfer is not yet within our cycle budget
            return None;
        }

        // Get the number of bytes to transfer, and mark the affected memory ranges as dirty
        let mut side_effects = SideEffects::default();
        let bytes = match state.direction {
            dma::Direction::ToRdram => {
                let bytes = self.write_length.length();
                {
                    let range = self.dram_address.address()..self.dram_address.address() + bytes;
                    side_effects.mutated_rdram = Some(range);
                }
                bytes
            }
            dma::Direction::ToSpMemory => {
                let bytes = self.read_length.length();
                {
                    let range = self.sp_address.address()..self.sp_address.address() + bytes;
                    side_effects.mutated_spmem = Some((self.sp_address.bank(), range));
                }
                bytes
            }
        };

        // Transfer in chunks of 8 bytes so we can both account for wrapping, and (in the future) support count/skip
        for _ in 0..(bytes / 8) {
            let rdram_slice = &mut ctx.rdram[self.dram_address.address() as usize..][..8];
            let sp_slice = &mut match self.sp_address.bank() {
                MemoryBank::IMem => &mut *ctx.imem,
                MemoryBank::DMem => &mut *ctx.dmem,
            }[self.sp_address.address() as usize..][..8];

            let (src, dst) = match state.direction {
                dma::Direction::ToRdram => {
                    self.write_length.decrement();
                    (sp_slice, rdram_slice)
                }

                dma::Direction::ToSpMemory => {
                    self.read_length.decrement();
                    (rdram_slice, sp_slice)
                }
            };

            dst.copy_from_slice(src);
            self.dram_address.increment();
            self.sp_address.increment();
        }

        self.state.take();
        Some(side_effects)
    }
}

#[derive(Debug, Default)]
pub struct Registers {
    dma: DoubleBuffered<DmaRegisters>,
    // Note that [`SpDmaFull`] and [`SpDmaBusy`] are omitted here, since they're mirrors of [`SpStatus`]
    sp_status: SpStatus,
    sp_semaphore: SpSemaphore,
}

impl Registers {
    const OFFSET_MASK: usize = 0b11100;

    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    pub fn read(&mut self, offset: usize) -> RspResult<u32> {
        Ok(match offset & Self::OFFSET_MASK {
            DmaSpAddress::OFFSET => self.dma.current.sp_address.into(),
            DmaRdramAddress::OFFSET => self.dma.current.dram_address.into(),
            DmaReadLength::OFFSET => self.dma.current.read_length.into(),
            DmaWriteLength::OFFSET => self.dma.current.write_length.into(),
            SpStatus::OFFSET => self.sp_status.into(),
            SpDmaFull::OFFSET => self.sp_status.dma_full().into(),
            SpDmaBusy::OFFSET => self.sp_status.dma_busy().into(),
            SpSemaphore::OFFSET => self.sp_semaphore.read().into(),
            _ => Err(RspError::RegisterOffsetOutOfBounds { offset })?,
        })
    }

    pub fn write(&mut self, offset: usize, value: u32) -> RspResult<SideEffects> {
        let mut side_effects = SideEffects::default();
        match offset & Self::OFFSET_MASK {
            DmaSpAddress::OFFSET => {
                self.dma.get_next_if(self.sp_status.dma_busy()).sp_address = value.into();
            }

            DmaRdramAddress::OFFSET => {
                self.dma.get_next_if(self.sp_status.dma_busy()).dram_address = value.into();
            }

            DmaReadLength::OFFSET => {
                self.dma.get_next_if(self.sp_status.dma_busy()).read_length = value.into();
                self.queue_dma(dma::Direction::ToSpMemory);
            }

            DmaWriteLength::OFFSET => {
                self.dma.get_next_if(self.sp_status.dma_busy()).write_length = value.into();
                self.queue_dma(dma::Direction::ToRdram);
            }

            SpStatus::OFFSET => side_effects.interrupt = self.sp_status.write(value),
            SpSemaphore::OFFSET => self.sp_semaphore = value.into(),
            SpDmaFull::OFFSET | SpDmaBusy::OFFSET => {
                // Read-only mirrors of [`SpStatus`]
            }

            _ => Err(RspError::RegisterOffsetOutOfBounds { offset })?,
        }
        Ok(side_effects)
    }

    fn queue_dma(&mut self, direction: dma::Direction) {
        if self.sp_status.dma_full() {
            // Ignore the request, there is already a DMA transfer in progress and one pending
            return;
        }

        self.dma
            .get_next_if(self.sp_status.dma_busy())
            .start_dma(direction);

        if self.sp_status.dma_busy() {
            self.sp_status.set_dma_full(true);
        } else {
            self.sp_status.set_dma_busy(true);
        }
    }

    pub fn tick(&mut self, ctx: &mut dma::TickContext) -> SideEffects {
        if let Some(side_effects) = self.dma.current.tick(ctx) {
            // An ongoing transfer was completed, switch to the next buffer if there is another pending DMA transfer
            debug_assert!(self.sp_status.dma_busy());
            if self.sp_status.dma_full() {
                self.sp_status.set_dma_full(false);
                self.dma.next();
            } else {
                self.sp_status.set_dma_busy(false);
            }
            side_effects
        } else {
            SideEffects::default()
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn double_buffered() {
        let mut db = DoubleBuffered::default();
        assert_eq!(db.current, 0);
        assert_eq!(db.next, 0);

        *db.get_next_if(true) = 1;
        assert_eq!(db.current, 0);
        assert_eq!(db.next, 1);

        *db.get_next_if(false) = 2;
        assert_eq!(db.current, 2);
        assert_eq!(db.next, 1);

        db.next();
        assert_eq!(db.current, 1);
        assert_eq!(db.next, 2);
    }
}
