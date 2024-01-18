use crate::input::{Controller, ControllerEvent};
use emgui::context::{self, ReceiveItem, SendItem};
use mips_lifter::{
    gdb::MonitorCommand,
    runtime::bus::{Bus as BusInterface, BusResult, BusValue, Int, IntError, PanicAction},
};
use n64_cartridge::Cartridge;
use n64_common::{
    memory::{PhysicalAddress, Section, SectionParseError},
    utils::{boxed_array, thiserror},
    InterruptDevice,
};
use n64_mi::{MiError, MipsInterface};
use n64_pi::{BusDevice as PiDevice, PeripheralInterface, PiError};
use n64_rsp::{MemoryBank as RspBank, Rsp, RspError};
use n64_si::{Channel, PifError, SerialInterface, SiError};
use n64_vi::{ViError, VideoInterface};

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
        if let Some(controller_state) = self.context.receive().expect("channel closed") {
            self.si.pif.channels.update_controller(
                Channel::Controller1,
                Controller::new(controller_state).input(),
            )?
        }
        Ok(())
    }

    fn maybe_poll_gui_events(&mut self, cycles: usize, v: &mut BusValue<()>) {
        self.gui_poll_counter = self
            .gui_poll_counter
            .checked_sub(cycles)
            .unwrap_or_else(|| {
                if let Some(stop) = self.context.receive().expect("channel closed") {
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
    #[error("Video interface error: {0}")]
    VideoInterfaceError(#[from] ViError),
    #[error("PIF error: {0}")]
    PifError(#[from] PifError),
    #[error("RSP error: {0}")]
    RspError(#[from] RspError),
    #[error("Attempted to write to read-only section {0:?}")]
    ReadOnlySectionWrite(Section),
    #[error("Attempted to read from write-only section {0:?}")]
    WriteOnlySectionRead(Section),
    #[error("Offset {offset:#x?} is out of bounds for section {section:?}")]
    OffsetOutOfBounds { section: Section, offset: usize },
    #[error("Failed to parse address: {0}")]
    AddressParseError(#[from] SectionParseError),
    #[error("Int error {0:?}")]
    IntError(#[from] IntError),
    #[error("Unimplemented section {0:?}")]
    UnimplementedSection(Section),
}

impl BusInterface for Bus {
    type Error = BusError;

    fn read_memory<const SIZE: usize>(
        &mut self,
        address: PhysicalAddress,
    ) -> BusResult<Int<SIZE>, Self::Error> {
        let section = Section::from_address(address)?;
        let offset = section.distance_from_start(address);

        let value = match section {
            Section::RdramMemory => Int::from_slice(&self.rdram[offset..])?,
            Section::RdramRegistersWriteOnly => Err(BusError::WriteOnlySectionRead(section))?,
            Section::MipsInterface => Int::new(self.mi.read(offset)?)?,
            Section::PeripheralInterface => Int::new(self.pi.read_register(offset)?)?,
            Section::SerialInterface => Int::new(self.si.read(offset)?)?,
            Section::PifRam => Int::new(self.si.read_pif_ram::<SIZE>(offset)?)?,
            Section::PifRom => Int::new(self.si.read_pif_rom::<SIZE>(offset)?)?,
            Section::RspRegisters => Int::new(self.rsp.read_register(offset)?)?,
            Section::VideoInterface => Int::new(self.vi.read(offset)?)?,

            // TODO: dont stub these so naively, only here so namco museum and libdragon assume its initialized.
            Section::AudioInterface => Int::from_slice(&u32::MAX.to_be_bytes())?,
            Section::RdramInterface => Int::from_slice(&u32::MAX.to_be_bytes())?,

            Section::RspDMemory => {
                Int::from_slice(&self.rsp.read_sp_memory::<SIZE>(RspBank::DMem, offset)?)?
            }

            Section::RspIMemory => {
                Int::from_slice(&self.rsp.read_sp_memory::<SIZE>(RspBank::IMem, offset)?)?
            }

            Section::CartridgeRom => {
                Int::new(self.pi.read_bus::<SIZE>(PiDevice::CartridgeRom, offset)?)?
            }

            Section::CartridgeSram => {
                Int::new(self.pi.read_bus::<SIZE>(PiDevice::CartridgeSram, offset)?)?
            }

            _ => {
                eprintln!(
                    "STUB: memory read at {address:#x} (section {section:?}, offset {offset:#x})",
                );
                Int::default()
            }
        };
        Ok(value.into())
    }

    fn write_memory<const SIZE: usize>(
        &mut self,
        address: PhysicalAddress,
        value: Int<SIZE>,
    ) -> BusResult<(), Self::Error> {
        let mut result = BusValue::default();
        let section = Section::from_address(address)?;
        let offset = section.distance_from_start(address);

        {
            // n64-systemtest isviewer output support
            const ISVIEWER_RANGE: std::ops::RangeInclusive<u32> = 0x13FF0020..=0x13FF0220;
            const ISVIEWER_WRITE: u32 = 0x13FF0014;
            if ISVIEWER_RANGE.contains(&address) {
                // Write to buffer
                let offset = (address - ISVIEWER_RANGE.start()) as usize;
                self.n64_systemtest_isviewer_buffer[offset..offset + SIZE]
                    .copy_from_slice(value.as_slice());
                return Ok(result);
            } else if address == ISVIEWER_WRITE {
                // Print the buffer out
                let len: u32 = value.try_into()?;
                let str =
                    String::from_utf8_lossy(&self.n64_systemtest_isviewer_buffer[..len as usize]);
                print!("{str}");
                self.n64_systemtest_isviewer_buffer.fill(0);
                return Ok(result);
            }
        }

        let range = offset..offset + SIZE;
        match section {
            Section::RdramMemory => self.rdram[range].copy_from_slice(value.as_slice()),

            Section::RdpCommandRegisters => {
                // TODO: Properly implement the RDP. This is a hack to get past rdp_detach from libdragon.
                if offset == 4 {
                    self.mi.raise_interrupt(InterruptDevice::Rdp);
                    result.interrupt = Some(MipsInterface::INTERRUPT_PENDING_MASK);
                }
            }

            Section::PifRam => self.si.write_pif_ram(offset, value.as_slice())?,
            Section::MipsInterface => self.mi.write(offset, value.try_into()?)?,

            Section::RspDMemory => {
                self.rsp
                    .write_sp_memory::<SIZE>(RspBank::DMem, offset, value.as_slice())?
            }

            Section::RspIMemory => {
                self.rsp
                    .write_sp_memory::<SIZE>(RspBank::IMem, offset, value.as_slice())?
            }

            Section::RspRegisters => {
                let side_effects = self.rsp.write_register(offset, value.try_into()?)?;
                self.handle(side_effects, &mut result)
            }

            Section::VideoInterface => {
                let side_effects = self.vi.write(offset, value.try_into()?)?;
                self.handle(side_effects, &mut result)
            }

            Section::SerialInterface => {
                let rdram = self.rdram.as_mut_slice();
                let side_effects = self.si.write(offset, value.try_into()?, rdram)?;
                self.handle(side_effects, &mut result)
            }

            Section::PeripheralInterface => {
                let rdram = self.rdram.as_mut_slice();
                let side_effects = self.pi.write_register(offset, value.try_into()?, rdram)?;
                self.handle(side_effects, &mut result)
            }

            Section::CartridgeRom => {
                self.pi
                    .write_bus::<SIZE>(PiDevice::CartridgeRom, offset, value.as_slice())?
            }

            Section::CartridgeSram => {
                self.pi
                    .write_bus::<SIZE>(PiDevice::CartridgeSram, offset, value.as_slice())?
            }

            Section::DiskDriveIpl4Rom => Err(BusError::ReadOnlySectionWrite(section))?,

            _ => {
                eprintln!(
                    "stub: memory write of {value:#x?} at {address:#x} (section {section:?}, offset {offset:#x})",
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
            if let Ok(fb) = self.vi.framebuffer(self.rdram.as_slice()) {
                self.context
                    .send(context::Framebuffer {
                        width: VideoInterface::SCREEN_WIDTH,
                        height: VideoInterface::SCREEN_HEIGHT,
                        pixels: fb.pixels,
                    })
                    .expect("channel closed");
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
            self.context
                .send(context::error::Error::new(msg))
                .expect("channel closed");
            PanicAction::Idle
        } else {
            // Let the JIT runtime kill the process (unless GDB is attached)
            PanicAction::Kill
        }
    }
}
