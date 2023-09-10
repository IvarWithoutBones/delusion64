use self::location::{MemoryLocation, MemoryRegion, MemoryType, MemoryValue};
use mips_lifter::{
    gdb::{util::str_to_u64, MonitorCommand},
    runtime::Memory,
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
    pub rdram: Box<[u8; MemoryRegion::RdramMemory.len()]>,
    pub rsp_dmem: Box<[u8; MemoryRegion::RspDMemory.len()]>,
    pub rsp_imem: Box<[u8; MemoryRegion::RspIMemory.len()]>,
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
            MonitorCommand {
                name: "read-paddr",
                description: "read a u32 from a physical address. usage: read-paddr <paddr>",
                handler: Box::new(|bus, out, args| {
                    let location = str_to_u64(args.next().ok_or("expected physical address")?)
                        .map(|addr| addr as u32)
                        .map_err(|err| format!("invalid physical address: {err}"))?
                        .try_into()
                        .map_err(|err| format!("error: {err:#?}"))?;
                    let value = bus.read(location, MemoryType::U32).map_err(|err| {
                        format!("failed to read from location {location:#?}: {err:#?}")
                    })?;
                    writeln!(out, "{value:#x?}")?;
                    Ok(())
                }),
            },
        ]
    }

    fn write<MemVal>(&mut self, location: MemoryLocation, raw_value: MemVal) -> Result<(), BusError>
    where
        MemVal: Into<MemoryValue>,
    {
        let value = raw_value.into();
        let region = location.region;
        let offset = location.offset;
        let map_err = |o: Option<()>| o.ok_or(BusError::OffsetOutOfBounds(location));

        match region {
            MemoryRegion::RdramMemory => {
                map_err(value.write_into(self.rdram.as_mut_slice(), offset))
            }

            MemoryRegion::RspDMemory => {
                map_err(value.write_into(self.rsp_dmem.as_mut_slice(), offset))
            }

            MemoryRegion::RspIMemory => {
                map_err(value.write_into(self.rsp_imem.as_mut_slice(), offset))
            }

            MemoryRegion::PeripheralInterface => map_err(self.pi.write(
                offset,
                value.try_into().unwrap(),
                self.rdram.as_mut_slice(),
                self.cartridge_rom.as_mut(),
            )),

            MemoryRegion::MipsInterface => self
                .mi
                .write(offset, value.try_into().unwrap())
                .map_err(BusError::MipsInterfaceError),

            MemoryRegion::VideoInterface => {
                map_err(self.vi.write(offset, value.try_into().unwrap()))
            }

            MemoryRegion::CartridgeRom | MemoryRegion::DiskDriveIpl4Rom | MemoryRegion::PifRom => {
                Err(BusError::ReadOnlyRegionWrite(region))
            }

            _ => region
                .safe_to_stub()
                .then(|| {
                    eprintln!("STUB: memory write of {value:#x?} at {location:#x?}");
                })
                .ok_or(BusError::Unimplemented),
        }
    }

    fn read(&self, location: MemoryLocation, ty: MemoryType) -> Result<MemoryValue, BusError> {
        let region = location.region;
        let offset = location.offset;
        let map_err = |o: Option<MemoryValue>| o.ok_or(BusError::OffsetOutOfBounds(location));

        match region {
            MemoryRegion::RdramMemory => map_err(ty.read_from(self.rdram.as_slice(), offset)),
            MemoryRegion::RspDMemory => map_err(ty.read_from(self.rsp_dmem.as_slice(), offset)),
            MemoryRegion::RspIMemory => map_err(ty.read_from(self.rsp_imem.as_slice(), offset)),
            MemoryRegion::PeripheralInterface => {
                map_err(self.pi.read(offset).map(|value| value.into()))
            }

            MemoryRegion::MipsInterface => self
                .mi
                .read(offset)
                .map_err(BusError::MipsInterfaceError)
                .map(|value| value.into()),

            MemoryRegion::CartridgeRom => Ok(
                // An address not within the mapped cartridge ROM range should return zero.
                ty.read_from(&self.cartridge_rom, offset)
                    .unwrap_or(ty.zero()),
            ),

            MemoryRegion::VideoInterface => map_err(self.vi.read(offset).map(|value| value.into())),

            MemoryRegion::RdramRegistersWriteOnly => Err(BusError::WriteOnlyRegionRead(region)),

            _ => region
                .safe_to_stub()
                .then(|| {
                    eprintln!("STUB: memory read of {ty:?} at {location:#x?}");
                    ty.zero()
                })
                .ok_or(BusError::Unimplemented),
        }
    }
}

pub enum BusError {
    /// The address is not mapped to any memory region.
    UnmappedAddress(u32),
    /// A read-only region was written to.
    ReadOnlyRegionWrite(MemoryRegion),
    /// A write-only region was read from.
    WriteOnlyRegionRead(MemoryRegion),
    /// An error occurred while accessing the mips interface.
    MipsInterfaceError(MiError),
    /// The offset is out of bounds for the given region.
    /// This is an internal error which can only occur if the `MemoryLocation` was improperly created.
    OffsetOutOfBounds(MemoryLocation),
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
                write!(
                    f,
                    "internal error: offset {:#x} out of bounds for {:?}",
                    location.offset, location.region
                )
            }
        }
    }
}

// TODO: move usage MemoryType/MemoryValue into the Memory trait, unify these functions
impl Memory for Bus {
    type AccessError = BusError;

    fn read_u8(&self, paddr: u32) -> Result<u8, Self::AccessError> {
        self.read(paddr.try_into()?, MemoryType::U8).map(|value| {
            // SAFETY: we're sure the returned value matches the MemoryType.
            unsafe { value.try_into().unwrap_unchecked() }
        })
    }

    fn read_u16(&self, paddr: u32) -> Result<u16, Self::AccessError> {
        self.read(paddr.try_into()?, MemoryType::U16).map(|value| {
            // SAFETY: we're sure the returned value matches the MemoryType.
            unsafe { value.try_into().unwrap_unchecked() }
        })
    }

    fn read_u32(&self, paddr: u32) -> Result<u32, Self::AccessError> {
        self.read(paddr.try_into()?, MemoryType::U32).map(|value| {
            // SAFETY: we're sure the returned value matches the MemoryType.
            unsafe { value.try_into().unwrap_unchecked() }
        })
    }

    fn read_u64(&self, paddr: u32) -> Result<u64, Self::AccessError> {
        self.read(paddr.try_into()?, MemoryType::U64).map(|value| {
            // SAFETY: we're sure the returned value matches the MemoryType.
            unsafe { value.try_into().unwrap_unchecked() }
        })
    }

    fn write_u8(&mut self, paddr: u32, value: u8) -> Result<(), Self::AccessError> {
        self.write(paddr.try_into()?, value)
    }

    fn write_u16(&mut self, paddr: u32, value: u16) -> Result<(), Self::AccessError> {
        self.write(paddr.try_into()?, value)
    }

    fn write_u32(&mut self, paddr: u32, value: u32) -> Result<(), Self::AccessError> {
        self.write(paddr.try_into()?, value)
    }

    fn write_u64(&mut self, paddr: u32, value: u64) -> Result<(), Self::AccessError> {
        self.write(paddr.try_into()?, value)
    }
}
