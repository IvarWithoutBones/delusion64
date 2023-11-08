use self::location::BusSection;
use mips_lifter::{
    gdb::MonitorCommand,
    runtime::bus::{Address, Bus as BusInterface, BusResult, BusValue, Int, MemorySection},
};
use n64_cartridge::Cartridge;
use n64_mi::{InterruptType, MiError, MipsInterface};
use n64_pi::{DmaStatus, PeripheralInterface, PiError};
use n64_si::{SerialInterface, SiError};
use n64_vi::VideoInterface;

pub mod location;

/// Allocates a fixed-sized boxed array of a given length.
fn boxed_array<T: Default + Clone, const LEN: usize>() -> Box<[T; LEN]> {
    // Use a Vec to allocate directly onto the heap. Using an array will allocate on the stack,
    // which can cause a stack overflow. SAFETY: We're sure the input size matches the output size.
    let result = vec![Default::default(); LEN].into_boxed_slice();
    unsafe { result.try_into().unwrap_unchecked() }
}

pub struct Bus {
    rdram: Box<[u8; BusSection::RdramMemory.len()]>,
    rsp_dmem: Box<[u8; BusSection::RspDMemory.len()]>,
    rsp_imem: Box<[u8; BusSection::RspIMemory.len()]>,
    pi: PeripheralInterface,
    mi: MipsInterface,
    vi: VideoInterface,
    si: SerialInterface,
    // TODO: remove once a proper interface is implemented, this is just here for input using GDB.
    controller: n64_si::controller::StandardController,
    // Buffer to view the result of unit tests from n64_systemtest. See https://github.com/lemmy-64/n64-systemtest#isviewer.
    n64_systemtest_isviewer_buffer: Box<[u8; 0x200]>,
}

impl Bus {
    pub fn new(cartridge: Cartridge) -> Self {
        let cartridge_rom = {
            let mut array: Box<[u8; BusSection::CartridgeRom.len()]> = boxed_array();
            let rom = cartridge.read().unwrap();
            let len = array.len().min(rom.len());
            array[..len].copy_from_slice(&rom[..len]);
            array
        };

        // Copy the first 0x1000 bytes of the PIF ROM to the RSP DMEM, simulating IPL2.
        let mut rsp_dmem = boxed_array();
        let len = rsp_dmem.len().min(cartridge_rom.len());
        rsp_dmem[..len].copy_from_slice(&cartridge_rom[..len]);

        // Write the length of RDRAM to the appropriate offset, simulating IPL.
        let mut rdram = boxed_array();
        if let Some(offset) = cartridge.cic.as_ref().unwrap().rdram_len_offset() {
            const RDRAM_LEN: &[u8; 4] = &0x800000_u32.to_be_bytes();
            rdram[offset..offset + 4].copy_from_slice(RDRAM_LEN);
        }

        let controller = n64_si::controller::StandardController::default();
        let mut si = SerialInterface::new(cartridge.cic.unwrap().seed());
        si.pif.channels.attach_controller(0, controller).unwrap();

        Self {
            rdram,
            rsp_dmem,
            rsp_imem: boxed_array(),
            pi: PeripheralInterface::new(cartridge_rom, cartridge.header.pi_bsd_domain_1_flags),
            mi: MipsInterface::new(),
            vi: VideoInterface::new(),
            si,
            controller,
            n64_systemtest_isviewer_buffer: boxed_array(),
        }
    }

    pub fn gdb_monitor_commands() -> Vec<MonitorCommand<Self>> {
        vec![
            MonitorCommand {
                name: "controller",
                description: "change or print the controller state",
                handler: Box::new(|bus, out, args| {
                    if let Some(button) = args.next() {
                        // TODO: parse joystick
                        match button {
                            "a" => bus.controller.set_a(!bus.controller.a()),
                            "b" => bus.controller.set_b(!bus.controller.b()),
                            "z" => bus.controller.set_z(!bus.controller.z()),
                            "start" => bus.controller.set_start(!bus.controller.start()),
                            "l" => bus.controller.set_l(!bus.controller.l()),
                            "r" => bus.controller.set_r(!bus.controller.r()),
                            "dpad_up" | "du" => {
                                bus.controller.set_dpad_up(!bus.controller.dpad_up())
                            }
                            "dpad_down" | "dd" => {
                                bus.controller.set_dpad_down(!bus.controller.dpad_down())
                            }
                            "dpad_left" | "dl" => {
                                bus.controller.set_dpad_left(!bus.controller.dpad_left())
                            }
                            "dpad_right" | "dr" => {
                                bus.controller.set_dpad_right(!bus.controller.dpad_right())
                            }
                            "c_up" | "cu" => bus.controller.set_c_up(!bus.controller.c_up()),
                            "c_down" | "cd" => bus.controller.set_c_down(!bus.controller.c_down()),
                            "c_left" | "cl" => bus.controller.set_c_left(!bus.controller.c_left()),
                            "c_right" | "cr" => {
                                bus.controller.set_c_right(!bus.controller.c_right())
                            }
                            _ => Err(format!("unknown button: {button}"))?,
                        }
                        bus.si
                            .pif_update_controller(0, bus.controller)
                            .map_err(|err| format!("failed to update controller: {err:#?}"))?;
                        writeln!(out, "toggled {button}: {:#x?}", bus.controller)?;
                    } else {
                        writeln!(out, "{:#x?}", bus.controller)?;
                    }
                    Ok(())
                }),
            },
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
                name: "dump-fb",
                description: "dump the VI framebuffer to a file. usage: dump-fb <filename>",
                handler: Box::new(|bus, out, args| {
                    let path = std::path::Path::new(args.next().ok_or("expected filename")?);
                    let fb = bus
                        .vi
                        .framebuffer(bus.rdram.as_slice())
                        .ok_or("failed to generate framebuffer")?;
                    std::fs::write(path, fb.pixels)
                        .map_err(|err| format!("failed to write file: {err}"))?;
                    writeln!(out, "wrote raw framebuffer to {path:?}")?;
                    Ok(())
                }),
            },
        ]
    }
}

#[derive(Debug)]
pub enum BusError {
    MipsInterfaceError(MiError),
    PeripheralInterfaceError(PiError),
    SerialInterfaceError(SiError),
    UnmappedAddress(u32),
    ReadOnlyRegionWrite(BusSection),
    WriteOnlyRegionRead(BusSection),
    OffsetOutOfBounds(Address<BusSection>),
    UnimplementedSection(BusSection),
}

impl BusInterface for Bus {
    type Error = BusError;
    type Section = BusSection;

    const SECTIONS: &'static [Self::Section] = &[
        BusSection::RdramMemory,
        BusSection::RdramRegisters,
        BusSection::RdramRegistersWriteOnly,
        BusSection::RspDMemory,
        BusSection::RspIMemory,
        BusSection::RspMemoryMirrors,
        BusSection::RspRegisters,
        BusSection::RdpCommandRegisters,
        BusSection::RspSpanRegisters,
        BusSection::MipsInterface,
        BusSection::VideoInterface,
        BusSection::AudioInterface,
        BusSection::PeripheralInterface,
        BusSection::RdramInterface,
        BusSection::SerialInterface,
        BusSection::DiskDriveRegisters,
        BusSection::DiskDriveIpl4Rom,
        BusSection::CartridgeSram,
        BusSection::CartridgeRom,
        BusSection::PifRom,
        BusSection::PifRam,
    ];

    fn tick(&mut self) -> BusResult<(), Self::Error> {
        let mut result = BusValue::default();

        // TODO: how does timing compare to the CPU?

        let vi_side_effects = self.vi.tick();
        if vi_side_effects.raise_interrupt {
            // println!("delusion64: vi halfline interrupt");
            if self.mi.raise_interrupt(InterruptType::VideoInterface) {
                result.interrupt = Some(MipsInterface::INTERRUPT_PENDING_MASK);
            }
        }

        if self.pi.tick() == DmaStatus::Finished {
            println!("delusion64: pi dma finished");
            if self.mi.raise_interrupt(InterruptType::PeripheralInterface) {
                result.interrupt = Some(MipsInterface::INTERRUPT_PENDING_MASK);
            }
        }

        if self.si.tick() == n64_si::DmaStatus::Completed {
            println!("delusion64: si dma finished");
            if self.mi.raise_interrupt(InterruptType::SerialInterface) {
                result.interrupt = Some(MipsInterface::INTERRUPT_PENDING_MASK);
            }
        }

        Ok(result)
    }

    fn read_memory<const SIZE: usize>(
        &mut self,
        address: Address<Self::Section>,
    ) -> BusResult<Int<SIZE>, Self::Error> {
        let value = match address.section {
            BusSection::RdramMemory => Int::from_slice(&self.rdram[address.offset..]),
            BusSection::RspDMemory => Int::from_slice(&self.rsp_dmem[address.offset..]),
            BusSection::RspIMemory => Int::from_slice(&self.rsp_imem[address.offset..]),

            // TODO: dont stub these so naively, only here so namco museum and libdragon assume its initialized.
            BusSection::AudioInterface => Int::from_slice(&u32::MAX.to_be_bytes()),
            BusSection::RdramInterface => Int::from_slice(&u32::MAX.to_be_bytes()),

            BusSection::MipsInterface => Int::new(
                self.mi
                    .read(address.offset)
                    .map_err(BusError::MipsInterfaceError)?,
            ),

            BusSection::VideoInterface => Int::new(
                self.vi
                    .read(address.offset)
                    .ok_or(BusError::OffsetOutOfBounds(address))?,
            ),

            BusSection::PeripheralInterface => Int::new(
                self.pi
                    .read_register(address.offset)
                    .map_err(BusError::PeripheralInterfaceError)?,
            ),

            BusSection::SerialInterface => Int::new(
                self.si
                    .read(address.offset)
                    .map_err(BusError::SerialInterfaceError)?,
            ),

            BusSection::PifRam => Int::new(
                self.si
                    .read_pif_ram::<SIZE>(address.offset)
                    .map_err(BusError::SerialInterfaceError)?,
            ),

            BusSection::PifRom => Int::new(
                self.si
                    .read_pif_rom::<SIZE>(address.offset)
                    .map_err(BusError::SerialInterfaceError)?,
            ),

            BusSection::CartridgeRom | BusSection::CartridgeSram => Int::new(
                self.pi
                    .read_bus::<SIZE>(address.section.into(), address.offset)
                    .map_err(BusError::PeripheralInterfaceError)?,
            ),

            BusSection::RdramRegistersWriteOnly => {
                Err(BusError::WriteOnlyRegionRead(*address.section))?
            }

            BusSection::RspMemoryMirrors => unreachable!("rsp memory mirrors should be resolved"),

            section => Ok(section
                .safe_to_stub()
                .then(|| {
                    eprintln!(
                        "STUB: memory read at {:#x} = {address:#x?}",
                        address.physical_address()
                    );
                    Int::default()
                })
                .ok_or(BusError::UnimplementedSection(*section))?),
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
        match address.section {
            BusSection::RdramMemory => self.rdram[range].copy_from_slice(value.as_slice()),

            BusSection::RspDMemory | BusSection::RspIMemory => {
                // 64-bit values are truncated to 32 bits, while 8-bit and 16-bit values are zero-extended.
                let range = address.offset..address.offset + 4;
                let mut slice = [0_u8; 4];
                let len = slice.len().min(SIZE);
                slice[..len].copy_from_slice(&value.as_slice()[..len]);

                match address.section {
                    BusSection::RspDMemory => self.rsp_dmem[range].copy_from_slice(&slice),
                    BusSection::RspIMemory => self.rsp_imem[range].copy_from_slice(&slice),
                    _ => unreachable!(),
                }
            }

            BusSection::RdpCommandRegisters => {
                // TODO: Properly implement the RDP. This is a hack to get past rdp_detach from libdragon.
                if address.offset == 4 {
                    self.mi.raise_interrupt(InterruptType::RdpSync);
                    result.interrupt = Some(MipsInterface::INTERRUPT_PENDING_MASK);
                }
            }

            BusSection::PifRam => self
                .si
                .write_pif_ram(address.offset, value.as_slice())
                .map_err(BusError::SerialInterfaceError)?,

            BusSection::MipsInterface => self
                .mi
                .write(address.offset, value.try_into()?)
                .map_err(BusError::MipsInterfaceError)?,

            BusSection::VideoInterface => {
                let side_effects = self
                    .vi
                    .write(address.offset, value.try_into()?)
                    .ok_or(BusError::OffsetOutOfBounds(address))?;
                if side_effects.lower_interrupt {
                    self.mi.lower_interrupt(InterruptType::VideoInterface);
                }
            }

            BusSection::SerialInterface => {
                let side_effects = self
                    .si
                    .write(address.offset, value.try_into()?, self.rdram.as_mut_slice())
                    .map_err(BusError::SerialInterfaceError)?;
                if side_effects.lower_interrupt {
                    self.mi.lower_interrupt(InterruptType::SerialInterface);
                }
            }

            BusSection::PeripheralInterface => {
                let side_effects = self
                    .pi
                    .write_register(address.offset, value.try_into()?, self.rdram.as_mut_slice())
                    .map_err(BusError::PeripheralInterfaceError)?;

                // Invalidate JIT blocks if RDRAM was mutated, since code may reside there.
                if let Some(range) = side_effects.mutated_rdram {
                    let rdram_base = BusSection::RdramMemory.range().start;
                    result.mutated = Some(range.start + rdram_base..range.end + rdram_base);
                }

                if side_effects.lower_interrupt {
                    self.mi.lower_interrupt(InterruptType::PeripheralInterface);
                }
            }

            BusSection::CartridgeRom | BusSection::CartridgeSram => self
                .pi
                .write_bus::<SIZE>(address.section.into(), address.offset, value.as_slice())
                .map_err(BusError::PeripheralInterfaceError)?,

            BusSection::DiskDriveIpl4Rom => Err(BusError::ReadOnlyRegionWrite(*address.section))?,

            BusSection::RspMemoryMirrors => {
                unreachable!("rsp memory mirrors should be resolved")
            }

            section => section
                .safe_to_stub()
                .then(|| {
                    eprintln!(
                        "STUB: memory write of {value:#x?} at {:#x} = {address:#x?}",
                        address.physical_address()
                    )
                })
                .ok_or(BusError::UnimplementedSection(*section))?,
        };
        Ok(result)
    }
}
