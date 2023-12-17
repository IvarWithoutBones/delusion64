use std::ops::Range;

#[derive(thiserror::Error, Debug, Copy, Clone, PartialEq, Eq)]
pub enum SectionParseError {
    #[error("The address {0:#x} is not mapped to any section.")]
    UnmappedAddress(PhysicalAddress),
}

/// A physical address, used to physically address memory.
pub type PhysicalAddress = u32;

/// Sections of memory in the memory map of the Nintendo 64.
/// See [n64brew](https://n64brew.dev/wiki/Memory_map#Physical_Memory_Map).
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Section {
    RdramMemory,
    RdramRegisters,
    RdramRegistersWriteOnly,
    RspDMemory,
    RspIMemory,
    RspMemoryMirrors,
    RspRegisters,
    RdpCommandRegisters,
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

impl Section {
    // Used to iterate over all sections without dependencies (e.g. strum's `EnumIter`)
    const ALL: &'static [Self] = &[
        Self::RdramMemory,
        Self::RdramRegisters,
        Self::RdramRegistersWriteOnly,
        Self::RspDMemory,
        Self::RspIMemory,
        Self::RspMemoryMirrors,
        Self::RspRegisters,
        Self::RdpCommandRegisters,
        Self::RspSpanRegisters,
        Self::MipsInterface,
        Self::VideoInterface,
        Self::AudioInterface,
        Self::PeripheralInterface,
        Self::RdramInterface,
        Self::SerialInterface,
        Self::DiskDriveRegisters,
        Self::DiskDriveIpl4Rom,
        Self::CartridgeSram,
        Self::CartridgeRom,
        Self::PifRom,
        Self::PifRam,
    ];

    /// Returns the section which contains the given address.
    ///
    /// # Errors
    /// Returns an error if the address is not mapped to any section.
    pub fn from_address(addr: PhysicalAddress) -> Result<Self, SectionParseError> {
        Self::ALL
            .iter()
            .find(|section| section.contains(addr))
            .ok_or(SectionParseError::UnmappedAddress(addr))
            .copied()
    }

    /// The range of addresses which this section occupies.
    #[must_use]
    pub const fn range(self) -> Range<PhysicalAddress> {
        match self {
            Section::RdramMemory => 0x0000_0000..0x03F0_0000,
            Section::RdramRegisters => 0x03F0_0000..0x03F8_0000,
            Section::RdramRegistersWriteOnly => 0x03F8_0000..0x0400_0000,
            Section::RspDMemory => 0x0400_0000..0x0400_1000,
            Section::RspIMemory => 0x0400_1000..0x0400_2000,
            Section::RspMemoryMirrors => 0x0400_2000..0x0404_0000,
            Section::RspRegisters => 0x0404_0000..0x040C_0000,
            Section::RdpCommandRegisters => 0x0410_0000..0x041F_FFFF,
            Section::RspSpanRegisters => 0x0420_0000..0x0430_0000,
            Section::MipsInterface => 0x0430_0000..0x0440_0000,
            Section::VideoInterface => 0x0440_0000..0x0450_0000,
            Section::AudioInterface => 0x0450_0000..0x0460_0000,
            Section::PeripheralInterface => 0x0460_0000..0x0470_0000,
            Section::RdramInterface => 0x0470_0000..0x0480_0000,
            Section::SerialInterface => 0x0480_0000..0x048F_FFFF,
            Section::DiskDriveRegisters => 0x0500_0000..0x0600_0000,
            Section::DiskDriveIpl4Rom => 0x0600_0000..0x0800_0000,
            Section::CartridgeSram => 0x0800_0000..0x1000_0000,
            Section::CartridgeRom => 0x1000_0000..0x1FC0_0000,
            Section::PifRom => 0x1FC0_0000..0x1FC0_07C0,
            Section::PifRam => 0x1FC0_07C0..0x1FC0_0800,
        }
    }

    /// The address this section starts at.
    #[must_use]
    pub const fn start(self) -> PhysicalAddress {
        self.range().start
    }

    /// The address this section ends at.
    #[must_use]
    pub const fn end(self) -> PhysicalAddress {
        self.range().end
    }

    /// The size of this section, in bytes.
    #[allow(clippy::len_without_is_empty)] // This is not a collection.
    #[must_use]
    pub const fn len(&self) -> usize {
        (self.end() - self.start()) as usize
    }

    /// Returns whether the given address resides within this section of memory.
    #[must_use]
    pub const fn contains(self, addr: PhysicalAddress) -> bool {
        // `range.contains()` is not sadly not const-friendly, so we have to do this manually.
        (addr >= self.start()) && (addr < self.end())
    }

    /// The address within this section which is `offset` bytes from the sections start.
    #[allow(clippy::cast_possible_truncation)]
    #[must_use]
    pub const fn offset(self, offset: usize) -> PhysicalAddress {
        self.start() + offset as PhysicalAddress
    }

    /// The amount of bytes from the start of this section to the given address.
    #[must_use]
    pub const fn distance(self, addr: PhysicalAddress) -> usize {
        (addr - self.start()) as usize
    }
}

impl TryFrom<PhysicalAddress> for Section {
    type Error = SectionParseError;

    fn try_from(addr: PhysicalAddress) -> Result<Self, Self::Error> {
        Self::from_address(addr)
    }
}
