use self::location::BusSection;
use mips_lifter::{
    gdb::MonitorCommand,
    runtime::bus::{Address, Bus as BusInterface, BusResult, BusValue, Int, MemorySection},
};
use n64_cartridge::Cartridge;
use n64_mi::{MiError, MipsInterface};
use n64_pi::PeripheralInterface;
use n64_vi::VideoInterface;
use std::fmt;

pub mod location;

/// Allocates a fixed-sized boxed array of a given length.
fn boxed_array<T: Default + Clone, const LEN: usize>() -> Box<[T; LEN]> {
    // Use a Vec to allocate directly onto the heap. Using an array will allocate on the stack,
    // which can cause a stack overflow. SAFETY: We're sure the input size matches the output size.
    let result = vec![Default::default(); LEN].into_boxed_slice();
    unsafe { result.try_into().unwrap_unchecked() }
}

pub struct Bus {
    pub rdram: Box<[u8; BusSection::RdramMemory.len()]>,
    pub rsp_dmem: Box<[u8; BusSection::RspDMemory.len()]>,
    pub rsp_imem: Box<[u8; BusSection::RspIMemory.len()]>,
    pub cartridge_rom: Box<[u8]>,
    pub pi: PeripheralInterface,
    pub mi: MipsInterface,
    pub vi: VideoInterface,
}

impl Bus {
    pub fn new(cartridge: Cartridge) -> Self {
        let cartridge_rom = cartridge.read().unwrap();

        // Copy the first 0x1000 bytes of the PIF ROM to the RSP DMEM, simulating IPL2.
        let mut rsp_dmem = boxed_array();
        let len = rsp_dmem.len().min(cartridge_rom.len());
        rsp_dmem[..len].copy_from_slice(&cartridge_rom[..len]);

        Self {
            rdram: boxed_array(),
            rsp_imem: boxed_array(),
            rsp_dmem,
            cartridge_rom,
            pi: PeripheralInterface::new(cartridge.header.pi_bsd_domain_1_flags),
            mi: MipsInterface::new(),
            vi: VideoInterface::new(),
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

pub enum BusError {
    /// The address is not mapped to any memory region.
    UnmappedAddress(u32),
    /// A read-only region was written to.
    ReadOnlyRegionWrite(BusSection),
    /// A write-only region was read from.
    WriteOnlyRegionRead(BusSection),
    /// An error occurred while accessing the mips interface.
    MipsInterfaceError(MiError),
    /// The offset is out of bounds for the given region.
    /// This is an internal error which can only occur if the `MemoryLocation` was improperly created.
    OffsetOutOfBounds(Address<BusSection>),
    /// The memory region is not yet implemented, and cannot be stubbed.
    Unimplemented,
}

impl fmt::Debug for BusError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            BusError::UnmappedAddress(addr) => write!(f, "unmapped address {addr:#x}"),
            BusError::ReadOnlyRegionWrite(region) => write!(f, "read-only region {region:?}"),
            BusError::WriteOnlyRegionRead(region) => write!(f, "write-only region {region:?}"),
            BusError::MipsInterfaceError(err) => write!(f, "MI error: {err:?}"),
            BusError::Unimplemented => write!(f, "unimplemented (cannot stub)"),
            BusError::OffsetOutOfBounds(location) => {
                write!(f, "internal error: offset out of bounds for {location:?}",)
            }
        }
    }
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
        self.vi.tick(); // TODO: how does timing compare to the CPU?
        Ok(Default::default())
    }

    fn read_memory<const SIZE: usize>(
        &mut self,
        address: Address<Self::Section>,
    ) -> BusResult<Int<SIZE>, Self::Error> {
        let value = match address.section {
            BusSection::RdramMemory => Int::from_slice(&self.rdram[address.offset..]),
            BusSection::RspDMemory => Int::from_slice(&self.rsp_dmem[address.offset..]),
            BusSection::RspIMemory => Int::from_slice(&self.rsp_imem[address.offset..]),

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
                    .read(address.offset)
                    .ok_or(BusError::OffsetOutOfBounds(address))?,
            ),

            // An address not within the mapped cartridge ROM range should return zero, as games differentiate in size.
            BusSection::CartridgeRom => Int::from_slice(&self.cartridge_rom[address.offset..]),

            section @ BusSection::RdramRegistersWriteOnly => {
                Err(BusError::WriteOnlyRegionRead(*section))?
            }

            BusSection::RspMemoryMirrors => unreachable!("rsp memory mirrors should be resolved"),

            section => Ok(section
                .safe_to_stub()
                .then(|| {
                    eprintln!("STUB: memory read at {address:#x?}");
                    Int::default()
                })
                .ok_or(BusError::Unimplemented)?),
        };
        Ok(value?.into())
    }

    fn write_memory<const SIZE: usize>(
        &mut self,
        address: Address<Self::Section>,
        value: Int<SIZE>,
    ) -> BusResult<(), Self::Error> {
        let mut result = BusValue::default();
        let range = address.offset..address.offset + SIZE;
        match address.section {
            BusSection::RdramMemory => self.rdram[range].copy_from_slice(value.as_slice()),
            BusSection::RspDMemory => self.rsp_dmem[range].copy_from_slice(value.as_slice()),
            BusSection::RspIMemory => self.rsp_imem[range].copy_from_slice(value.as_slice()),

            BusSection::MipsInterface => self
                .mi
                .write(address.offset, value.try_into()?)
                .map_err(BusError::MipsInterfaceError)?,

            BusSection::VideoInterface => self
                .vi
                .write(address.offset, value.try_into()?)
                .ok_or(BusError::OffsetOutOfBounds(address))?,

            // TODO: invalidate JIT blocks in the case of a DMA transfer.
            BusSection::PeripheralInterface => {
                let mutated = self
                    .pi
                    .write(
                        address.offset,
                        value.try_into()?,
                        self.rdram.as_mut_slice(),
                        self.cartridge_rom.as_mut(),
                    )
                    .ok_or(BusError::OffsetOutOfBounds(address))?;

                // Invalidate JIT blocks if RDRAM was mutated, since code may reside there.
                if let Some(range) = mutated.rdram {
                    let rdram_base = BusSection::RdramMemory.range().start;
                    result.mutated = Some(range.start + rdram_base..range.end + rdram_base);
                    println!("{result:#x?}");
                    // if result.mutated.as_ref().unwrap().contains(&0x00000180) {
                    //     panic!();
                    // }
                }
            }

            BusSection::DiskDriveIpl4Rom | BusSection::CartridgeRom | BusSection::PifRom => {
                Err(BusError::ReadOnlyRegionWrite(*address.section))?
            }

            BusSection::RspMemoryMirrors => {
                unreachable!("rsp memory mirrors should be resolved")
            }

            section => section
                .safe_to_stub()
                .then(|| eprintln!("STUB: memory write of {value:#x?} at {address:#x?}"))
                .ok_or(BusError::Unimplemented)?,
        };
        Ok(result)
    }
}
