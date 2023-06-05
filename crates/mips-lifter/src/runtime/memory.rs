//! Memory access for runtime environment.

use std::mem::size_of;
// use std::ops::Range;

// const KSEG0: Range<u64> = 0x8000_0000..0xA000_0000;
// const KSEG1: Range<u64> = 0xA000_0000..0xC000_0000;

/// A trait providing memory access for runtime environment.
pub trait Memory {
    /// Read a byte from the given physical address.
    fn read_u8(&self, addr: u64) -> u8;

    /// Write a byte to the given physical address.
    fn write_u8(&mut self, addr: u64, value: u8);

    /// Read a u16 from the given physical address.
    #[inline]
    fn read_u16(&self, mut addr: u64) -> u16 {
        u16::from_be_bytes([0; size_of::<u16>()].map(|_| {
            addr += 1;
            self.read_u8(addr - 1)
        }))
    }

    /// Read a u32 from the given physical address.
    #[inline]
    fn read_u32(&self, mut addr: u64) -> u32 {
        u32::from_be_bytes([0; size_of::<u32>()].map(|_| {
            addr += 1;
            self.read_u8(addr - 1)
        }))
    }

    /// Read a u64 from the given physical address.
    #[inline]
    fn read_u64(&self, mut addr: u64) -> u64 {
        u64::from_be_bytes([0; size_of::<u64>()].map(|_| {
            addr += 1;
            self.read_u8(addr - 1)
        }))
    }

    /// Write a u16 to the given physical address.
    #[inline]
    fn write_u16(&mut self, addr: u64, value: u16) {
        for (i, v) in value.to_be_bytes().iter().enumerate() {
            self.write_u8(addr + i as u64, *v);
        }
    }

    /// Write a u32 to the given physical address.
    #[inline]
    fn write_u32(&mut self, addr: u64, value: u32) {
        for (i, v) in value.to_be_bytes().iter().enumerate() {
            self.write_u8(addr + i as u64, *v);
        }
    }

    /// Write a u64 to the given physical address.
    #[inline]
    fn write_u64(&mut self, addr: u64, value: u64) {
        for (i, v) in value.to_be_bytes().iter().enumerate() {
            self.write_u8(addr + i as u64, *v);
        }
    }
}
