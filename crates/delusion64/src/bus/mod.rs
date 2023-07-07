use self::location::{MemoryLocation, MemoryRegion, MemoryType, MemoryValue};
use crate::pi::PeripheralInterface;
use mips_lifter::runtime::Memory;

pub mod location;

/// Allocates a boxed slice of a given length.
fn boxed_slice<const LEN: usize>() -> Box<[u8; LEN]> {
    // Use `vec!` to avoid stack allocations, which may overflow.
    let result = vec![0; LEN].into_boxed_slice();
    // This is safe because we're sure the input length matches the output length.
    unsafe { result.try_into().unwrap_unchecked() }
}

pub struct Bus {
    pub rdram: Box<[u8; MemoryRegion::RdramMemory.len()]>,
    pub rsp_dmem: Box<[u8; MemoryRegion::RspDMemory.len()]>,
    pub rsp_imem: Box<[u8; MemoryRegion::RspIMemory.len()]>,
    pub cartridge_rom: Box<[u8]>,

    pub pi: PeripheralInterface,
}

impl Bus {
    pub fn new(cartridge_rom: Box<[u8]>) -> Self {
        Self {
            cartridge_rom,
            rdram: boxed_slice(),
            rsp_dmem: boxed_slice(),
            rsp_imem: boxed_slice(),
            pi: PeripheralInterface::new(),
        }
    }

    fn write<MemVal>(&mut self, location: MemoryLocation, raw_value: MemVal)
    where
        MemVal: Into<MemoryValue>,
    {
        let region = location.region;
        let offset = location.offset;
        let value = raw_value.into();

        match region {
            MemoryRegion::RdramMemory => value.write_into(self.rdram.as_mut_slice(), offset),
            MemoryRegion::RspDMemory => value.write_into(self.rsp_dmem.as_mut_slice(), offset),
            MemoryRegion::RspIMemory => value.write_into(self.rsp_imem.as_mut_slice(), offset),

            MemoryRegion::CartridgeRom | MemoryRegion::DiskDriveIpl4Rom | MemoryRegion::PifRom => {
                panic!("read-only {region:?} write at offset {offset:#x} = {value:?}")
            }

            MemoryRegion::PeripheralInterface => {
                self.pi.write(
                    offset,
                    value.try_into().unwrap(),
                    self.rdram.as_mut_slice(),
                    self.cartridge_rom.as_mut(),
                );

                println!("{:#x?}", self.pi);
            }

            _ => {
                println!("stub: {region:?} write at offset {offset:#x} = {value:?}");
            }
        }
    }

    fn read(&self, location: MemoryLocation, ty: MemoryType) -> MemoryValue {
        let region = location.region;
        let offset = location.offset;
        match region {
            MemoryRegion::CartridgeRom => ty.read_from(&self.cartridge_rom, offset),
            MemoryRegion::RdramMemory => ty.read_from(self.rdram.as_slice(), offset),
            MemoryRegion::RspDMemory => ty.read_from(self.rsp_dmem.as_slice(), offset),
            MemoryRegion::RspIMemory => ty.read_from(self.rsp_imem.as_slice(), offset),

            MemoryRegion::RdramRegistersWriteOnly => {
                panic!("write-only {region:?} read at offset {offset:#x}");
            }

            MemoryRegion::PeripheralInterface => {
                println!("{:#x?}", self.pi);
                self.pi.read(offset).unwrap().into()
            }

            _ => {
                println!("stub: {region:?} read at offset {offset:#x}");
                ty.zero()
            }
        }
    }
}

// TODO: move usage MemoryType/MemoryValue into the Memory trait, unify these functions
impl Memory for Bus {
    #[inline]
    fn read_u8(&self, addr: u64) -> u8 {
        let location = addr
            .try_into()
            .unwrap_or_else(|_| panic!("invalid memory read at address {addr:#x}"));
        let result = self.read(location, MemoryType::U8).try_into();
        // This is safe because we're sure the returned value matches the MemoryType.
        unsafe { result.unwrap_unchecked() }
    }

    #[inline]
    fn read_u16(&self, addr: u64) -> u16 {
        let location = addr
            .try_into()
            .unwrap_or_else(|_| panic!("invalid memory read at address {addr:#x}"));
        let result = self.read(location, MemoryType::U16).try_into();
        // This is safe because we're sure the returned value matches the MemoryType.
        unsafe { result.unwrap_unchecked() }
    }

    #[inline]
    fn read_u32(&self, addr: u64) -> u32 {
        let location = addr
            .try_into()
            .unwrap_or_else(|_| panic!("invalid memory read at address {addr:#x}"));
        let result = self.read(location, MemoryType::U32).try_into();
        // This is safe because we're sure the returned value matches the MemoryType.
        unsafe { result.unwrap_unchecked() }
    }

    #[inline]
    fn read_u64(&self, addr: u64) -> u64 {
        let location = addr
            .try_into()
            .unwrap_or_else(|_| panic!("invalid memory read at address {addr:#x}"));
        let result = self.read(location, MemoryType::U64).try_into();
        // This is safe because we're sure the returned value matches the MemoryType.
        unsafe { result.unwrap_unchecked() }
    }

    #[inline]
    fn write_u8(&mut self, addr: u64, value: u8) {
        let location = addr
            .try_into()
            .unwrap_or_else(|_| panic!("invalid memory write at address {addr:#x}"));
        self.write(location, value)
    }

    #[inline]
    fn write_u16(&mut self, addr: u64, value: u16) {
        let location = addr
            .try_into()
            .unwrap_or_else(|_| panic!("invalid memory write at address {addr:#x}"));
        self.write(location, value)
    }

    #[inline]
    fn write_u32(&mut self, addr: u64, value: u32) {
        let location = addr
            .try_into()
            .unwrap_or_else(|_| panic!("invalid memory write at address {addr:#x}"));
        self.write(location, value)
    }

    #[inline]
    fn write_u64(&mut self, addr: u64, value: u64) {
        let location = addr
            .try_into()
            .unwrap_or_else(|_| panic!("invalid memory write at address {addr:#x}"));
        self.write(location, value)
    }
}
