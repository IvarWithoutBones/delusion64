use crate::input::{Controller, ControllerEvent};
use emgui::context::{self, ReceiveItem, SendItem};
use mips_lifter::{
    gdb::MonitorCommand,
    runtime::bus::{
        Address, Bus as BusInterface, BusResult, BusValue, Int, MemorySection, Mirroring,
        PanicAction,
    },
};
use n64_cartridge::Cartridge;
use n64_common::{
    memory::{PhysicalAddress, Section},
    utils::{boxed_array, thiserror},
    InterruptDevice,
};
use n64_mi::{MiError, MipsInterface};
use n64_pi::{BusDevice as PiDevice, PeripheralInterface, PiError};
use n64_rsp::{MemoryBank as RspBank, Rsp, RspError};
use n64_si::{Channel, PifError, SerialInterface, SiError};
use n64_vi::VideoInterface;
use std::ops::Range;

/// Newtype wrapper for a `Section` to implement `MemorySection`.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
#[repr(transparent)]
pub struct SectionWrapper(pub Section);

impl MemorySection for SectionWrapper {
    fn range(&self) -> Range<PhysicalAddress> {
        self.0.range()
    }

    fn mirroring(&self) -> Mirroring<Self> {
        // TODO: this does not consider imem/dmem mirroring
        Mirroring::None
    }

    fn auto_invalidate_written_addresses(&self) -> bool {
        true
    }
}

/// The amount of CPU instructions to execute before checking for GUI events.
const GUI_POLL_RATE: usize = 100_000;

pub struct Bus {
    rdram: Box<[u8; Section::RdramMemory.len()]>,
    rsp: Rsp,
    pi: PeripheralInterface,
    mi: MipsInterface,
    vi: VideoInterface,
    si: SerialInterface,
    // Buffer to view the result of unit tests from n64_systemtest. See https://github.com/lemmy-64/n64-systemtest#isviewer.
    n64_systemtest_isviewer_buffer: Box<[u8; 0x200]>,

    // GUI stuff
    pub context: context::Emulator<ControllerEvent>,
    gui_connected: bool,
    gui_poll_counter: usize,
}

impl Bus {
    pub fn new(
        context: context::Emulator<ControllerEvent>,
        cartridge: Cartridge,
        gui_connected: bool,
    ) -> Self {
        let cartridge_rom = {
            let mut array: Box<[u8; Section::CartridgeRom.len()]> = boxed_array();
            let rom = cartridge.read().unwrap();
            let len = array.len().min(rom.len());
            array[..len].copy_from_slice(&rom[..len]);
            array
        };

        // Write the length of RDRAM to the appropriate offset, simulating IPL.
        let mut rdram = boxed_array();
        if let Some(offset) = cartridge.cic.as_ref().unwrap().rdram_len_offset() {
            const RDRAM_LEN: &[u8; 4] = &0x800000_u32.to_be_bytes();
            rdram[offset..offset + 4].copy_from_slice(RDRAM_LEN);
        }

        let controller = n64_si::controller::StandardController::default();
        let mut si = SerialInterface::new(cartridge.cic.unwrap().seed());
        si.pif
            .channels
            .attach_controller(Channel::Controller1, controller)
            .unwrap();

        Self {
            gui_poll_counter: GUI_POLL_RATE,
            rdram,
            rsp: Rsp::new(cartridge_rom.as_ref()),
            pi: PeripheralInterface::new(cartridge_rom, cartridge.header.pi_bsd_domain_1_flags),
            mi: MipsInterface::new(),
            vi: VideoInterface::new(),
            si,
            n64_systemtest_isviewer_buffer: boxed_array(),
            context,
            gui_connected,
        }
    }

    pub fn gdb_monitor_commands() -> Vec<MonitorCommand<Self>> {
        vec![
            MonitorCommand {
                name: "mi",
                description: "print the MIPS interface registers",
                handler: Box::new(|bus, out, _args| {
                    writeln!(out, "{:#x?}", bus.mi)?;
                    Ok(())
                }),
            },
            MonitorCommand {
                name: "pi",
                description: "print the peripheral interface registers",
                handler: Box::new(|bus, out, _args| {
                    writeln!(out, "{:#x?}", bus.pi)?;
                    Ok(())
                }),
            },
            MonitorCommand {
                name: "si",
                description: "print the serial interface registers",
                handler: Box::new(|bus, out, _args| {
                    writeln!(out, "{:#x?}", bus.si)?;
                    Ok(())
                }),
            },
            MonitorCommand {
                name: "vi",
                description: "print the video interface registers",
                handler: Box::new(|bus, out, _args| {
                    writeln!(out, "{:#x?}", bus.vi)?;
                    Ok(())
                }),
            },
            MonitorCommand {
                name: "rsp",
                description: "print the RSP status",
                handler: Box::new(|bus, out, _args| {
                    writeln!(out, "{:#x?}", bus.rsp)?;
                    Ok(())
                }),
            },
        ]
    }

    fn update_controller(&mut self) -> Result<(), BusError> {
        if let Some(controller_state) = self.context.receive() {
            self.si
                .pif
                .channels
                .update_controller(
                    Channel::Controller1,
                    Controller::new(controller_state).input(),
                )
                .map_err(BusError::PifError)?;
        }
        Ok(())
    }

    fn maybe_poll_gui_events(&mut self, cycles: usize, v: &mut BusValue<()>) {
        self.gui_poll_counter = self
            .gui_poll_counter
            .checked_sub(cycles)
            .unwrap_or_else(|| {
                if let Some(stop) = self.context.receive() {
                    // TODO: allow placing generics on receive()
                    let _: emgui::context::Stop = stop;
                    v.request_exit = true;
                }

                GUI_POLL_RATE
            });
    }

    fn handle<T>(&mut self, side_effects: n64_common::SideEffects, bus_val: &mut BusValue<T>) {
        let n64_common::SideEffects { interrupt, dirty } = side_effects;

        bus_val.mutated = dirty;
        if let Some(interrupt) = interrupt {
            match interrupt {
                n64_common::InterruptRequest::Raise(dev) => {
                    if self.mi.raise_interrupt(dev) {
                        bus_val.interrupt = Some(MipsInterface::INTERRUPT_PENDING_MASK);
                    }
                }
                n64_common::InterruptRequest::Lower(dev) => {
                    self.mi.lower_interrupt(dev);
                }
            }
        }
    }
}

#[derive(thiserror::Error, Debug)]
pub enum BusError {
    #[error("MIPS interface error: {0}")]
    MipsInterfaceError(#[from] MiError),
    #[error("Serial interface error: {0}")]
    PeripheralInterfaceError(#[from] PiError),
    #[error("Serial interface error: {0}")]
    SerialInterfaceError(#[from] SiError),
    #[error("PIF error: {0}")]
    PifError(#[from] PifError),
    #[error("RSP error: {0}")]
    RspError(#[from] RspError),
    #[error("Physical address {0:#x} is not mapped")]
    UnmappedAddress(u32),
    #[error("Attempted to write to read-only region {0:?}")]
    ReadOnlyRegionWrite(Section),
    #[error("Attempted to read from write-only region {0:?}")]
    WriteOnlyRegionRead(Section),
    #[error("Offset {0:#x?} is out of bounds for its section")]
    OffsetOutOfBounds(Address<SectionWrapper>),
    #[error("Unimplemented section {0:?}")]
    UnimplementedSection(Section),
}

impl BusInterface for Bus {
    type Error = BusError;
    type Section = SectionWrapper;

    const SECTIONS: &'static [Self::Section] = &[
        SectionWrapper(Section::RdramMemory),
        SectionWrapper(Section::RdramRegisters),
        SectionWrapper(Section::RdramRegistersWriteOnly),
        SectionWrapper(Section::RspDMemory),
        SectionWrapper(Section::RspIMemory),
        SectionWrapper(Section::RspMemoryMirrors),
        SectionWrapper(Section::RspRegisters),
        SectionWrapper(Section::RdpCommandRegisters),
        SectionWrapper(Section::RspSpanRegisters),
        SectionWrapper(Section::MipsInterface),
        SectionWrapper(Section::VideoInterface),
        SectionWrapper(Section::AudioInterface),
        SectionWrapper(Section::PeripheralInterface),
        SectionWrapper(Section::RdramInterface),
        SectionWrapper(Section::SerialInterface),
        SectionWrapper(Section::DiskDriveRegisters),
        SectionWrapper(Section::DiskDriveIpl4Rom),
        SectionWrapper(Section::CartridgeSram),
        SectionWrapper(Section::CartridgeRom),
        SectionWrapper(Section::PifRom),
        SectionWrapper(Section::PifRam),
    ];

    fn read_memory<const SIZE: usize>(
        &mut self,
        address: Address<Self::Section>,
    ) -> BusResult<Int<SIZE>, Self::Error> {
        let value = match address.section.0 {
            Section::RdramMemory => Int::from_slice(&self.rdram[address.offset..]),

            // TODO: dont stub these so naively, only here so namco museum and libdragon assume its initialized.
            Section::AudioInterface => Int::from_slice(&u32::MAX.to_be_bytes()),
            Section::RdramInterface => Int::from_slice(&u32::MAX.to_be_bytes()),

            Section::MipsInterface => Int::new(
                self.mi
                    .read(address.offset)
                    .map_err(BusError::MipsInterfaceError)?,
            ),

            Section::VideoInterface => Int::new(
                self.vi
                    .read(address.offset)
                    .ok_or(BusError::OffsetOutOfBounds(address))?,
            ),

            Section::PeripheralInterface => Int::new(
                self.pi
                    .read_register(address.offset)
                    .map_err(BusError::PeripheralInterfaceError)?,
            ),

            Section::SerialInterface => Int::new(
                self.si
                    .read(address.offset)
                    .map_err(BusError::SerialInterfaceError)?,
            ),

            Section::PifRam => Int::new(
                self.si
                    .read_pif_ram::<SIZE>(address.offset)
                    .map_err(BusError::SerialInterfaceError)?,
            ),

            Section::PifRom => Int::new(
                self.si
                    .read_pif_rom::<SIZE>(address.offset)
                    .map_err(BusError::SerialInterfaceError)?,
            ),

            Section::RspDMemory => Int::from_slice(
                self.rsp
                    .read_sp_memory::<SIZE>(RspBank::DMem, address.offset)
                    .map_err(BusError::RspError)?,
            ),

            Section::RspIMemory => Int::from_slice(
                self.rsp
                    .read_sp_memory::<SIZE>(RspBank::IMem, address.offset)
                    .map_err(BusError::RspError)?,
            ),

            Section::RspRegisters => Int::new(
                self.rsp
                    .read_register(address.offset)
                    .map_err(BusError::RspError)?,
            ),

            Section::CartridgeRom => Int::new(
                self.pi
                    .read_bus::<SIZE>(PiDevice::CartridgeRom, address.offset)
                    .map_err(BusError::PeripheralInterfaceError)?,
            ),

            Section::CartridgeSram => Int::new(
                self.pi
                    .read_bus::<SIZE>(PiDevice::CartridgeSram, address.offset)
                    .map_err(BusError::PeripheralInterfaceError)?,
            ),

            Section::RdramRegistersWriteOnly => {
                Err(BusError::WriteOnlyRegionRead(address.section.0))?
            }

            Section::RspMemoryMirrors => unreachable!("rsp memory mirrors should be resolved"),

            _ => {
                eprintln!(
                    "STUB: memory read at {:#x} = {address:#x?}",
                    address.physical_address()
                );
                Ok(Int::default())
            }
        };
        Ok(value?.into())
    }

    fn write_memory<const SIZE: usize>(
        &mut self,
        address: Address<Self::Section>,
        value: Int<SIZE>,
    ) -> BusResult<(), Self::Error> {
        let mut result = BusValue::default();

        {
            // n64-systemtest isviewer output support
            const ISVIEWER_RANGE: std::ops::RangeInclusive<u32> = 0x13FF0020..=0x13FF0220;
            const ISVIEWER_WRITE: u32 = 0x13FF0014;
            let paddr = address.physical_address();
            if ISVIEWER_RANGE.contains(&paddr) || ISVIEWER_RANGE.contains(&(paddr + SIZE as u32)) {
                // Write to buffer
                let offset = (paddr - ISVIEWER_RANGE.start()) as usize;
                self.n64_systemtest_isviewer_buffer[offset..offset + SIZE]
                    .copy_from_slice(value.as_slice());
                return Ok(result);
            } else if paddr == ISVIEWER_WRITE {
                // Print the buffer out
                let len: u32 = value.try_into()?;
                let str =
                    String::from_utf8_lossy(&self.n64_systemtest_isviewer_buffer[..len as usize]);
                print!("{str}");
                self.n64_systemtest_isviewer_buffer.fill(0);
                return Ok(result);
            }
        }

        let range = address.offset..address.offset + SIZE;
        match address.section.0 {
            Section::RdramMemory => self.rdram[range].copy_from_slice(value.as_slice()),

            Section::RdpCommandRegisters => {
                // TODO: Properly implement the RDP. This is a hack to get past rdp_detach from libdragon.
                if address.offset == 4 {
                    self.mi.raise_interrupt(InterruptDevice::Rdp);
                    result.interrupt = Some(MipsInterface::INTERRUPT_PENDING_MASK);
                }
            }

            Section::PifRam => self
                .si
                .write_pif_ram(address.offset, value.as_slice())
                .map_err(BusError::SerialInterfaceError)?,

            Section::MipsInterface => self
                .mi
                .write(address.offset, value.try_into()?)
                .map_err(BusError::MipsInterfaceError)?,

            Section::RspDMemory => self
                .rsp
                .write_sp_memory::<SIZE>(RspBank::DMem, address.offset, value.as_slice())
                .map_err(BusError::RspError)?,

            Section::RspIMemory => self
                .rsp
                .write_sp_memory::<SIZE>(RspBank::IMem, address.offset, value.as_slice())
                .map_err(BusError::RspError)?,

            Section::RspRegisters => {
                let side_effects = self
                    .rsp
                    .write_register(address.offset, value.try_into()?)
                    .map_err(BusError::RspError)?;
                self.handle(side_effects, &mut result)
            }

            Section::VideoInterface => {
                let side_effects = self
                    .vi
                    .write(address.offset, value.try_into()?)
                    .ok_or(BusError::OffsetOutOfBounds(address))?;
                self.handle(side_effects, &mut result)
            }

            Section::SerialInterface => {
                let side_effects = self
                    .si
                    .write(address.offset, value.try_into()?, self.rdram.as_mut_slice())
                    .map_err(BusError::SerialInterfaceError)?;
                self.handle(side_effects, &mut result)
            }

            Section::PeripheralInterface => {
                let side_effects = self
                    .pi
                    .write_register(address.offset, value.try_into()?, self.rdram.as_mut_slice())
                    .map_err(BusError::PeripheralInterfaceError)?;
                self.handle(side_effects, &mut result)
            }

            Section::CartridgeRom => self
                .pi
                .write_bus::<SIZE>(PiDevice::CartridgeRom, address.offset, value.as_slice())
                .map_err(BusError::PeripheralInterfaceError)?,

            Section::CartridgeSram => self
                .pi
                .write_bus::<SIZE>(PiDevice::CartridgeSram, address.offset, value.as_slice())
                .map_err(BusError::PeripheralInterfaceError)?,

            Section::DiskDriveIpl4Rom => Err(BusError::ReadOnlyRegionWrite(address.section.0))?,

            Section::RspMemoryMirrors => {
                unreachable!("rsp memory mirrors should be resolved")
            }

            _ => {
                eprintln!(
                    "stub: memory write of {value:#x?} at {:#x} = {address:#x?}",
                    address.physical_address()
                )
            }
        };
        Ok(result)
    }

    fn tick(&mut self, cycles: usize) -> BusResult<(), Self::Error> {
        // TODO: how does timing compare to the CPU?
        let mut result = BusValue::default();
        self.maybe_poll_gui_events(cycles, &mut result);

        if self.vi.vblank {
            if let Some(fb) = self.vi.framebuffer(self.rdram.as_slice()) {
                self.context.send(context::Framebuffer {
                    width: VideoInterface::SCREEN_WIDTH,
                    height: VideoInterface::SCREEN_HEIGHT,
                    pixels: fb.pixels,
                });
            }

            // TODO: only call this on PIF DMA, it will not be read until then.
            self.update_controller()?;
        }

        let vi_side_effects = self.vi.tick(cycles);
        self.handle(vi_side_effects, &mut result);

        let rsp_side_effects = self.rsp.tick(cycles, self.rdram.as_mut_slice());
        self.handle(rsp_side_effects, &mut result);

        let pi_side_effects = self.pi.tick(cycles);
        self.handle(pi_side_effects, &mut result);

        let si_side_effects = self.si.tick(cycles);
        self.handle(si_side_effects, &mut result);

        Ok(result)
    }

    fn on_panic(&mut self, error: mips_lifter::runtime::bus::BusError<Self::Error>) -> PanicAction {
        if self.gui_connected {
            let msg = format!("{error:?}");
            self.context.send(context::error::Error::new(msg));
            PanicAction::Idle
        } else {
            // Let the JIT runtime kill the process (unless GDB is attached)
            PanicAction::Kill
        }
    }
}
