//! Handling of physical memory locations and their regions.

use std::{fmt, mem::size_of, ops::Range};
use strum::{EnumIter, IntoEnumIterator};

use super::BusError;

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum MemoryType {
    U8,
    U16,
    U32,
    U64,
}

impl MemoryType {
    pub const fn zero(&self) -> MemoryValue {
        match self {
            Self::U8 => MemoryValue::U8(0),
            Self::U16 => MemoryValue::U16(0),
            Self::U32 => MemoryValue::U32(0),
            Self::U64 => MemoryValue::U64(0),
        }
    }

    pub fn read_from(&self, slice: &[u8], offset: usize) -> Option<MemoryValue> {
        // SAFETY: We're that the slice is the correct length, the offset will still be boundary checked.
        unsafe {
            match self {
                Self::U8 => Some((*slice.get(offset)?).into()),

                Self::U16 => Some(
                    u16::from_be_bytes(
                        slice
                            .get(offset..offset + size_of::<u16>())?
                            .try_into()
                            .unwrap_unchecked(),
                    )
                    .into(),
                ),

                Self::U32 => Some(
                    u32::from_be_bytes(
                        slice
                            .get(offset..offset + size_of::<u32>())?
                            .try_into()
                            .unwrap_unchecked(),
                    )
                    .into(),
                ),

                Self::U64 => Some(
                    u64::from_be_bytes(
                        slice
                            .get(offset..offset + size_of::<u64>())?
                            .try_into()
                            .unwrap_unchecked(),
                    )
                    .into(),
                ),
            }
        }
    }
}

#[derive(Copy, Clone, PartialEq, Eq)]
pub enum MemoryValue {
    U8(u8),
    U16(u16),
    U32(u32),
    U64(u64),
}

impl MemoryValue {
    pub fn write_into(&self, slice: &mut [u8], offset: usize) -> Option<()> {
        match self {
            Self::U8(value) => *slice.get_mut(offset)? = *value,
            Self::U16(value) => slice
                .get_mut(offset..offset + size_of::<u16>())?
                .copy_from_slice(&value.to_be_bytes()),
            Self::U32(value) => slice
                .get_mut(offset..offset + size_of::<u32>())?
                .copy_from_slice(&value.to_be_bytes()),
            Self::U64(value) => slice
                .get_mut(offset..offset + size_of::<u64>())?
                .copy_from_slice(&value.to_be_bytes()),
        }

        Some(())
    }
}

impl fmt::Debug for MemoryValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::U8(value) => write!(f, "U8({value:#x})"),
            Self::U16(value) => write!(f, "U16({value:#x})"),
            Self::U32(value) => write!(f, "U32({value:#x})"),
            Self::U64(value) => write!(f, "U64({value:#x})"),
        }
    }
}

macro_rules! impl_from {
    ($struct:ident :: $field:ident, $ty:ident) => {
        impl From<$ty> for $struct {
            fn from(value: $ty) -> Self {
                Self::$field(value)
            }
        }
    };
}

macro_rules! impl_try_from {
    ($ty:ident, $struct:ident :: $field:ident) => {
        impl TryFrom<$struct> for $ty {
            type Error = ();

            fn try_from(value: $struct) -> Result<Self, Self::Error> {
                match value {
                    $struct::$field(value) => Ok(value),
                    _ => Err(()),
                }
            }
        }
    };
}

impl_from!(MemoryValue::U8, u8);
impl_from!(MemoryValue::U16, u16);
impl_from!(MemoryValue::U32, u32);
impl_from!(MemoryValue::U64, u64);

impl_try_from!(u8, MemoryValue::U8);
impl_try_from!(u16, MemoryValue::U16);
impl_try_from!(u32, MemoryValue::U32);
impl_try_from!(u64, MemoryValue::U64);

/// Memory regions in the physical memory map of the N64.
/// See https://n64brew.dev/wiki/Memory_map#Physical_Memory_Map.
#[derive(EnumIter, Debug, Copy, Clone, PartialEq, Eq)]
pub enum MemoryRegion {
    RdramMemory,
    RdramRegisters,
    RdramRegistersWriteOnly,

    RspDMemory,
    RspIMemory,
    RspRegisters,
    RspCommandRegisters,
    RspSpanRegisters,

    MipsInterface,
    VideoInterface,
    AudioInterface,
    PeripheralInterface,
    RdramInterface,
    SerialInterface,

    DiskDriveRegisters,
    DiskDriveIpl4Rom,

    CartridgeSram,
    CartridgeRom,

    PifRom,
    PifRam,
}

impl MemoryRegion {
    const RSP_MEM_MIRRORS: Range<u64> = 0x04002000..0x04040000;

    pub fn new(addr: u64) -> Option<Self> {
        let addr = Self::resolve_mirror(addr);
        Self::iter().find(|&region| region.contains(addr))
    }

    /// Returns true for regions that can safely ignore writes, and return zero when read from.
    /// This is used for regions that are not emulated, such as the RdramRegisters.
    pub const fn safe_to_stub(&self) -> bool {
        // TODO: Dont just stub everything
        true
    }

    pub const fn range(&self) -> Range<u64> {
        // See https://n64brew.dev/wiki/Memory_map#Physical_Memory_Map.
        match self {
            MemoryRegion::RdramMemory => 0x00000000..0x03F00000,
            MemoryRegion::RdramRegisters => 0x03F00000..0x03F80000,
            MemoryRegion::RdramRegistersWriteOnly => 0x03F80000..0x04000000,
            MemoryRegion::RspDMemory => 0x04000000..0x04001000,
            MemoryRegion::RspIMemory => 0x04001000..0x04002000,
            MemoryRegion::RspRegisters => 0x04040000..0x040C0000,
            MemoryRegion::RspCommandRegisters => 0x04100000..0x041FFFFF,
            MemoryRegion::RspSpanRegisters => 0x04200000..0x04300000,
            MemoryRegion::MipsInterface => 0x04300000..0x04400000,
            MemoryRegion::VideoInterface => 0x04400000..0x04500000,
            MemoryRegion::AudioInterface => 0x04500000..0x04600000,
            MemoryRegion::PeripheralInterface => 0x04600000..0x04700000,
            MemoryRegion::RdramInterface => 0x04700000..0x04800000,
            MemoryRegion::SerialInterface => 0x04800000..0x048FFFFF,
            MemoryRegion::DiskDriveRegisters => 0x05000000..0x06000000,
            MemoryRegion::DiskDriveIpl4Rom => 0x06000000..0x08000000,
            MemoryRegion::CartridgeSram => 0x08000000..0x10000000,
            MemoryRegion::CartridgeRom => 0x10000000..0x1FC00000,
            MemoryRegion::PifRom => 0x1FC00000..0x1FC007C0,
            MemoryRegion::PifRam => 0x1FC007C0..0x1FC00800,
        }
    }

    /// Resolves mirrored RSP memory, if needed.
    // Its kind of unfortunate we need to add a special case for this,
    // but resolving this automatically seems like it will make for a better API.
    fn resolve_mirror(addr: u64) -> u64 {
        if Self::RSP_MEM_MIRRORS.contains(&addr) {
            let normalised = addr % (Self::RspDMemory.len() + Self::RspIMemory.len()) as u64;
            normalised + Self::RspDMemory.start()
        } else {
            addr
        }
    }

    pub const fn start(&self) -> u64 {
        self.range().start
    }

    pub const fn end(&self) -> u64 {
        self.range().end
    }

    #[allow(clippy::len_without_is_empty)] // Would always be false.
    pub const fn len(&self) -> usize {
        (self.end() - self.start()) as usize
    }

    pub fn offset(&self, addr: u64) -> usize {
        let addr = Self::resolve_mirror(addr);
        (addr - self.start()) as usize
    }

    pub fn contains(&self, addr: u64) -> bool {
        let addr = Self::resolve_mirror(addr);
        self.range().contains(&addr)
    }
}

#[derive(Copy, Clone, PartialEq, Eq)]
pub struct MemoryLocation {
    pub region: MemoryRegion,
    pub offset: usize,
}

impl MemoryLocation {
    pub fn new(addr: u64) -> Option<Self> {
        MemoryRegion::new(addr).map(|region| Self {
            region,
            offset: region.offset(addr),
        })
    }
}

impl TryFrom<u64> for MemoryLocation {
    type Error = BusError;

    fn try_from(addr: u64) -> Result<Self, Self::Error> {
        Self::new(addr).ok_or(BusError::UnmappedAddress(addr))
    }
}

impl fmt::Debug for MemoryLocation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("MemoryLocation")
            .field("region", &self.region)
            .field("offset", &format_args!("{:#x}", self.offset))
            .finish()
    }
}
