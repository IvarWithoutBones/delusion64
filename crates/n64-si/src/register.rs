//! Register definitions for the Serial Interface.

use n64_common::utils::tartan_bitfield::{bitfield, bitfield_without_debug};
use std::fmt;

bitfield_without_debug! {
    /// Known as `SI_DRAM_ADDR`.
    /// See [n64brew](https://n64brew.dev/wiki/Serial_Interface#0x0480_0000_-_SI_DRAM_ADDR) for more information.
    pub struct DramAddress(u32) {
        /// Base address of RDRAM for SI DMAs, without the least significant bits which must be zero.
        [3..=23] writable_address: u32,
    }
}

impl DramAddress {
    /// Base address of RDRAM for SI DMAs.
    pub fn address(&self) -> u32 {
        self.writable_address() << 3
    }
}

impl fmt::Debug for DramAddress {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("DramAddress")
            .field("<value>", &self.0)
            .field("address", &self.address())
            .finish()
    }
}

impl DramAddress {
    pub const OFFSET: usize = 0x0;
}

bitfield_without_debug! {
    /// Known as `SI_PIF_AD_RD64B`.
    /// See [n64brew](https://n64brew.dev/wiki/Serial_Interface#0x0480_0004_-_SI_PIF_AD_RD64B) for more information.
    pub struct PifAddressRead64(u32) {
        /// Offset in PIF_RAM/PIF_ROM to fetch data from, without the least significant bit which must be zero.
        [1..=10] writable_offset: u16,
    }
}

impl PifAddressRead64 {
    pub const OFFSET: usize = 0x4;

    /// Offset in PIF-RAM/PIF-ROM to fetch data from.
    pub fn offset(&self) -> u32 {
        (self.writable_offset() as u32) << 1
    }
}

impl fmt::Debug for PifAddressRead64 {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("PifAddressRead64")
            .field("<value>", &self.0)
            .field("offset", &self.offset())
            .finish()
    }
}

bitfield! {
    /// Known as `SI_PIF_AD_WR4B`.
    /// See [n64brew](https://n64brew.dev/wiki/Serial_Interface#0x0480_0008_-_SI_PIF_AD_WR4B) for more information.
    pub struct PifAddressWrite4(u32) {
        /// The data to be transferred to PIF-RAM.
        [0..=31] pub data: u32,
    }
}

impl PifAddressWrite4 {
    pub const OFFSET: usize = 0x8;
}

bitfield_without_debug! {
    /// Known as `SI_PIF_AD_WR64B`.
    /// See [n64brew](https://n64brew.dev/wiki/Serial_Interface#0x0480_0010_-_SI_PIF_AD_WR64B) for more information.
    pub struct PifAddressWrite64(u32) {
        /// The data to be transferred to PIF-RAM, without the least significant bit which must be zero.
        [1..=31] writable_data: u32,
    }
}

impl PifAddressWrite64 {
    /// The data to be transferred to PIF-RAM.
    pub fn address(&self) -> u32 {
        self.writable_data() << 1
    }
}

impl fmt::Debug for PifAddressWrite64 {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("PifAddressWrite64")
            .field("<value>", &self.0)
            .field("address", &self.address())
            .finish()
    }
}

impl PifAddressWrite64 {
    pub const OFFSET: usize = 0x10;
}

bitfield! {
    /// Known as `SI_PIF_AD_RD4B`.
    /// See [n64brew](https://n64brew.dev/wiki/Serial_Interface#0x0480_0014_-_SI_PIF_AD_RD4B) for more information.
    pub struct PifAddressRead4(u32) {
        /// The data to be transferred to PIF-RAM.
        [0..=31] pub data: u32,
    }
}

impl PifAddressRead4 {
    pub const OFFSET: usize = 0x14;
}

bitfield! {
    /// Known as `SI_STATUS`.
    /// See [n64brew](https://n64brew.dev/wiki/Serial_Interface#0x0480_0018_-_SI_STATUS) for more information.
    pub struct Status(u32) {
        /// Set when a read/write DMA, or an IO write is in progress.
        [0] pub dma_busy,
        /// Set when a direct memory write to PIF-RAM is in progress.
        [1] pub io_busy,
        /// Set when an IO read occurs while an IO/DMA write is in progress.
        [2] pub read_pending,
        /// Set when overlapping DMA requests occur.
        [3] pub dma_error,
        /// Set when a DMA completes.
        [12] pub interrupt,
    }
}

impl Status {
    pub const OFFSET: usize = 0x18;
}
