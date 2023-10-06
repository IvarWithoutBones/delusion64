use self::location::BusSection;
use mips_lifter::{
    gdb::MonitorCommand,
    runtime::bus::{Address, Bus as BusInterface, BusResult, BusValue, Int, MemorySection},
};
use n64_cartridge::Cartridge;
use n64_mi::{InterruptType, MiError, MipsInterface};
use n64_pi::{DmaStatus, PeripheralInterface, PiError};
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
    pif_ram: Box<[u8; BusSection::PifRam.len()]>,
    pi: PeripheralInterface,
    mi: MipsInterface,
    vi: VideoInterface,
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

        Self {
            rdram: boxed_array(),
            rsp_imem: boxed_array(),
            pif_ram: boxed_array(),
            rsp_dmem,
            pi: PeripheralInterface::new(cartridge_rom, cartridge.header.pi_bsd_domain_1_flags),
            mi: MipsInterface::new(),
            vi: VideoInterface::new(),
            n64_systemtest_isviewer_buffer: boxed_array(),
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
                    let fb = &bus.rdram[bus.vi.framebuffer_range()];
                    std::fs::write(path, fb)
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
    /// The address is not mapped to any memory region.
    UnmappedAddress(u32),
    /// A read-only region was written to.
    ReadOnlyRegionWrite(BusSection),
    /// A write-only region was read from.
    WriteOnlyRegionRead(BusSection),
    /// An error occurred while accessing the mips interface.
    MipsInterfaceError(MiError),
    /// An error occurred while accessing the peripheral interface.
    PeripheralInterfaceError(PiError),
    /// The offset is out of bounds for the given region.
    /// This is an internal error which can only occur if the `MemorySection` was improperly created.
    OffsetOutOfBounds(Address<BusSection>),
    /// The memory section is not yet implemented, and cannot be stubbed.
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
        BusSection::RspCommandRegisters,
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
        self.vi.tick();
        if self.pi.tick() == DmaStatus::Finished {
            let ty = InterruptType::PeripheralInterface;
            if self.mi.raise_interrupt(ty) {
                result.interrupt = Some(ty.mask())
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
            BusSection::PifRam => Int::from_slice(&self.pif_ram[address.offset..]),

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

            BusSection::CartridgeRom | BusSection::CartridgeSram | BusSection::PifRom => Int::new(
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
            BusSection::RspDMemory => self.rsp_dmem[range].copy_from_slice(value.as_slice()),
            BusSection::RspIMemory => self.rsp_imem[range].copy_from_slice(value.as_slice()),
            BusSection::PifRam => self.pif_ram[range].copy_from_slice(value.as_slice()),

            BusSection::MipsInterface => self
                .mi
                .write(address.offset, value.try_into()?)
                .map_err(BusError::MipsInterfaceError)?,

            BusSection::VideoInterface => self
                .vi
                .write(address.offset, value.try_into()?)
                .ok_or(BusError::OffsetOutOfBounds(address))?,

            BusSection::PeripheralInterface => {
                let mutated = self
                    .pi
                    .write_register(address.offset, value.try_into()?, self.rdram.as_mut_slice())
                    .map_err(BusError::PeripheralInterfaceError)?;

                // Invalidate JIT blocks if RDRAM was mutated, since code may reside there.
                if let Some(range) = mutated.rdram {
                    let rdram_base = BusSection::RdramMemory.range().start;
                    result.mutated = Some(range.start + rdram_base..range.end + rdram_base);
                }
            }

            BusSection::CartridgeRom | BusSection::CartridgeSram | BusSection::PifRom => self
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
