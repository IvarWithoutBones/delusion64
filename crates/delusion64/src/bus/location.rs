//! Handling of physical memory locations and their regions.

use mips_lifter::runtime::bus::{MemorySection, Mirroring, PhysicalAddress};
use std::ops::Range;

/// Memory regions in the physical memory map of the N64.
/// See https://n64brew.dev/wiki/Memory_map#Physical_Memory_Map.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum BusSection {
    RdramMemory,
    RdramRegisters,
    RdramRegistersWriteOnly,

    RspDMemory,
    RspIMemory,
    RspMemoryMirrors,
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

impl MemorySection for BusSection {
    fn range(&self) -> Range<PhysicalAddress> {
        self.memory_map()
    }

    fn mirroring(&self) -> Mirroring<Self> {
        match self {
            Self::RspMemoryMirrors => Mirroring::AcrossSections {
                start: &Self::RspDMemory,
                end: &Self::RspIMemory,
            },
            _ => Mirroring::None,
        }
    }

    fn auto_invalidate_written_addresses(&self) -> bool {
        true
    }
}

impl BusSection {
    const fn memory_map(&self) -> Range<PhysicalAddress> {
        match self {
            BusSection::RdramMemory => 0x00000000..0x03F00000,
            BusSection::RdramRegisters => 0x03F00000..0x03F80000,
            BusSection::RdramRegistersWriteOnly => 0x03F80000..0x04000000,
            BusSection::RspDMemory => 0x04000000..0x04001000,
            BusSection::RspIMemory => 0x04001000..0x04002000,
            BusSection::RspMemoryMirrors => 0x04002000..0x04040000,
            BusSection::RspRegisters => 0x04040000..0x040C0000,
            BusSection::RspCommandRegisters => 0x04100000..0x041FFFFF,
            BusSection::RspSpanRegisters => 0x04200000..0x04300000,
            BusSection::MipsInterface => 0x04300000..0x04400000,
            BusSection::VideoInterface => 0x04400000..0x04500000,
            BusSection::AudioInterface => 0x04500000..0x04600000,
            BusSection::PeripheralInterface => 0x04600000..0x04700000,
            BusSection::RdramInterface => 0x04700000..0x04800000,
            BusSection::SerialInterface => 0x04800000..0x048FFFFF,
            BusSection::DiskDriveRegisters => 0x05000000..0x06000000,
            BusSection::DiskDriveIpl4Rom => 0x06000000..0x08000000,
            BusSection::CartridgeSram => 0x08000000..0x10000000,
            BusSection::CartridgeRom => 0x10000000..0x1FC00000,
            BusSection::PifRom => 0x1FC00000..0x1FC007C0,
            BusSection::PifRam => 0x1FC007C0..0x1FC00800,
        }
    }

    #[allow(clippy::len_without_is_empty)]
    pub const fn len(&self) -> usize {
        let range = self.memory_map();
        (range.end - range.start) as usize
    }

    pub const fn safe_to_stub(&self) -> bool {
        matches!(
            self,
            |BusSection::RdramInterface| BusSection::RdramRegisters
                | BusSection::RdramRegistersWriteOnly
                | BusSection::RspRegisters
                | BusSection::AudioInterface
        )
    }
}

impl<'a> From<&'a BusSection> for n64_pi::BusDevice {
    fn from(section: &'a BusSection) -> Self {
        match section {
            BusSection::CartridgeRom => n64_pi::BusDevice::CartridgeRom,
            BusSection::CartridgeSram => n64_pi::BusDevice::CartridgeSram,
            BusSection::PifRom => n64_pi::BusDevice::PifRom,
            _ => unimplemented!(),
        }
    }
}
