//! Memory-mapped registers for the Peripheral Interface (PI).
//! See https://n64brew.dev/wiki/Peripheral_Interface#Registers

use super::Domain;
use tartan_bitfield::bitfield;

bitfield! {
    /// Note that DMA transfers are buggy if DRAM_ADDR[2:0] are not all zero.
    /// https://n64brew.dev/wiki/Peripheral_Interface#0x0460_0000_-_PI_DRAM_ADDR
    pub struct DramAddress(u32) {
        /// Base address of RDRAM for PI DMAs. Bit 0 must always be 0.
        [0..=23] pub address: u32,
    }
}

bitfield! {
    /// https://n64brew.dev/wiki/Peripheral_Interface#0x0460_0004_-_PI_CART_ADDR
    pub struct CartAddress(u32) {
        /// Base address of the PI bus (e.g. cartridge) for PI DMAs. Bit 0 must always be 0.
        [0..=31] pub address: u32,
    }
}

bitfield! {
    /// Writing to this register will start the DMA transfer. Reading appears to always return `0x7F`.
    /// https://n64brew.dev/wiki/Peripheral_Interface#0x0460_0008_-_PI_RD_LEN
    pub struct ReadLength(u32) {
        /// Number of bytes, minus one, to be transferred from RDRAM, to the PI bus.
        [0..=23] pub length: u32,
    }
}

bitfield! {
    /// Writing to this register will start the DMA transfer. Reading appears to always return `0x7F`.
    /// https://n64brew.dev/wiki/Peripheral_Interface#0x0460_000C_-_PI_WR_LEN
    pub struct WriteLength(u32) {
        /// Number of bytes, minus one, to be transferred from the PI bus, into RDRAM.
        [0..=23] pub length: u32,
    }
}

bitfield! {
    /// https://n64brew.dev/wiki/Peripheral_Interface#0x0460_0010_-_PI_STATUS
    pub struct Status(u32) {
        // For reading
        [0] pub dma_busy,
        [1] pub io_busy,
        [2] pub dma_error,
        [3] pub interrupt,
        // For writing
        [0] pub reset_dma,
        [1] pub clear_interrupt,
    }
}

impl Status {
    pub fn write(&mut self, value: u32) {
        // Retain the read-only bits.
        let write = Status(value);
        self.set_reset_dma(write.reset_dma());
        self.set_clear_interrupt(write.clear_interrupt());
    }
}

bitfield! {
    /// https://n64brew.dev/wiki/Peripheral_Interface#0x0460_00n4_-_PI_BSD_DOMn_LAT
    // TODO: Configured to use the value from the cartridge header during IPL2.
    pub struct Latch(u32) {
        /// The number of RCP cycles, minus one, after the address has been sent (falling edge of `ALE_L`),
        /// and before the first read or write may start (falling edge of `/RD` or `/WR`).
        [0..=7] pub latch: u32,
    }
}

bitfield! {
    /// During IPL2, the N64 will initialize `Domain::One`'s `PulseWidth` using data read from the cartridge ROM header,
    /// All official ROMs set `PulseWidth` to `18`.
    /// https://n64brew.dev/wiki/Peripheral_Interface#0x0460_00n8_-_PI_BSD_DOMn_PWD
    pub struct PulseWidth(u32) {
        /// The number of RCP cycles, minus one, the `/RD` or `/WR` signals are held low.
        [0..=7] pub pulse_width: u32,
    }
}

bitfield! {
    /// During IPL2, the N64 will initialize `Domain::One`'s `PageSize` using data read from the cartridge ROM header,
    /// all official ROMs set `PageSize` to `7`.
    /// Page Size only matters for DMA transfers; all direct accesses via the PI are only ever 32 bits wide.
    /// The maximum number of transfers will only happen when the address's least significant bits are all 0.
    /// https://n64brew.dev/wiki/Peripheral_Interface#0x0460_00nC_-_PI_BSD_DOMn_PGS
    pub struct PageSize(u32) {
        /// The number of bytes that can be sequentially read/written on the bus, before sending the next base address.
        /// Note that this value must be converted to a byte count by the following formula: Size = 2^(PGS+2) bytes.
        [0..=3] page_size: u32,
    }
}

impl PageSize {
    /// The number of bytes that can be sequentially read/written on the bus, before sending the next base address.
    pub fn read(&self) -> u32 {
        // See the formula from the `page_size` field.
        2 ^ (self.page_size() + 2)
    }
}

bitfield! {
    /// During IPL2, the N64 will initialize `Domain::One`'s `Release` using data read from the cartridge ROM header,
    /// all official ROMs set `Release` to `3`.
    /// https://n64brew.dev/wiki/Peripheral_Interface#0x0460_00n0_-_PI_BSD_DOMn_RLS
    pub struct Release(u32) {
        /// the number of RCP cycles, minus one, that the `/RD` or `/WR` signals are held high between each 16-bits of data.
        [0..=1] pub release: u32,
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Register {
    DramAddress,
    CartAddress,
    ReadLength,
    WriteLength,
    Status,
    Latch(Domain),
    PulseWidth(Domain),
    PageSize(Domain),
    Release(Domain),
}

impl Register {
    pub const fn new(offset: usize) -> Option<Self> {
        match offset {
            0x0 => Some(Register::DramAddress),
            0x4 => Some(Register::CartAddress),
            0x8 => Some(Register::ReadLength),
            0xC => Some(Register::WriteLength),
            0x10 => Some(Register::Status),
            0x14 => Some(Register::Latch(Domain::One)),
            0x18 => Some(Register::PulseWidth(Domain::One)),
            0x1C => Some(Register::PageSize(Domain::One)),
            0x20 => Some(Register::Release(Domain::One)),
            0x24 => Some(Register::Latch(Domain::Two)),
            0x28 => Some(Register::PulseWidth(Domain::Two)),
            0x2C => Some(Register::PageSize(Domain::Two)),
            0x30 => Some(Register::Release(Domain::Two)),
            _ => None,
        }
    }
}

impl TryFrom<usize> for Register {
    type Error = ();

    fn try_from(offset: usize) -> Result<Self, Self::Error> {
        Register::new(offset).ok_or(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn dram_address() {
        let mut dram_address = DramAddress(0);
        dram_address.set_address(0x123456);
        assert_eq!(dram_address.address(), 0x123456);
    }

    #[test]
    fn cart_address() {
        let mut cart_address = CartAddress(0);
        cart_address.set_address(0x12345678);
        assert_eq!(cart_address.address(), 0x12345678);
    }

    #[test]
    fn read_len() {
        let read_len = ReadLength(u32::MAX);
        assert_ne!(read_len.length(), u32::MAX);
        assert_eq!(read_len.length(), 0b1111_1111_1111_1111_1111_1111);
    }

    #[test]
    fn write_len() {
        let write_len = WriteLength(u32::MAX);
        assert_ne!(write_len.length(), u32::MAX);
        assert_eq!(write_len.length(), 0b1111_1111_1111_1111_1111_1111);
    }

    #[test]
    fn status() {
        let mut status = Status(u32::MAX);
        assert!(status.dma_busy());
        assert!(status.io_busy());
        assert!(status.dma_error());
        assert!(status.interrupt());

        status.set_reset_dma(false);
        status.set_clear_interrupt(false);
        assert!(!status.reset_dma());
        assert!(!status.clear_interrupt());
    }
}
