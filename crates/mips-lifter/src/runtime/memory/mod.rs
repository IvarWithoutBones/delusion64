//! Memory access for runtime environment.

use self::tlb::AccessMode;
use super::Environment;
use std::fmt;

pub(crate) mod tlb;

/// A trait providing memory access for runtime environment.
pub trait Memory {
    /// The type of error that can occur when accessing memory.
    type AccessError: std::fmt::Debug;

    /// Read a u8 from the given physical address.
    fn read_u8(&self, paddr: u32) -> Result<u8, Self::AccessError>;

    /// Read a u16 from the given physical address.
    fn read_u16(&self, paddr: u32) -> Result<u16, Self::AccessError>;

    /// Read a u32 from the given physical address.
    fn read_u32(&self, paddr: u32) -> Result<u32, Self::AccessError>;

    /// Read a u64 from the given physical address.
    fn read_u64(&self, paddr: u32) -> Result<u64, Self::AccessError>;

    /// Write a u8 to the given physical address.
    fn write_u8(&mut self, paddr: u32, value: u8) -> Result<(), Self::AccessError>;

    /// Write a u16 to the given physical address.
    fn write_u16(&mut self, paddr: u32, value: u16) -> Result<(), Self::AccessError>;

    /// Write a u32 to the given physical address.
    fn write_u32(&mut self, paddr: u32, value: u32) -> Result<(), Self::AccessError>;

    /// Write a u64 to the given physical address.
    fn write_u64(&mut self, paddr: u32, value: u64) -> Result<(), Self::AccessError>;
}

impl<'ctx, Mem> Environment<'ctx, Mem>
where
    Mem: Memory,
{
    fn panic_read_failed<E>(&mut self, err: E, vaddr: u64, paddr: u32) -> !
    where
        E: fmt::Debug,
    {
        let message = format!("memory read failed at vaddr={vaddr:#x} paddr={paddr:#x}: {err:?}");
        self.panic_update_debugger(&message)
    }

    fn panic_write_failed<T, E>(&mut self, err: E, vaddr: u64, paddr: u32, value: T) -> !
    where
        T: fmt::LowerHex,
        E: fmt::Debug,
    {
        let message = format!(
            "memory write of {value:#x} failed at vaddr={vaddr:#x} paddr={paddr:#x}: {err:?}",
        );
        self.panic_update_debugger(&message)
    }

    /*
        Runtime functions. These are not meant to be called directly, but rather by JIT'ed code.
    */

    pub(crate) unsafe extern "C" fn read_u8(&mut self, vaddr: u64) -> u8 {
        let paddr = self.virtual_to_physical_address(vaddr, AccessMode::Read);
        self.memory
            .read_u8(paddr)
            .unwrap_or_else(|err| self.panic_read_failed(err, vaddr, paddr))
    }

    pub(crate) unsafe extern "C" fn read_u16(&mut self, vaddr: u64) -> u16 {
        let paddr = self.virtual_to_physical_address(vaddr, AccessMode::Read);
        self.memory
            .read_u16(paddr)
            .unwrap_or_else(|err| self.panic_read_failed(err, vaddr, paddr))
    }

    pub(crate) unsafe extern "C" fn read_u32(&mut self, vaddr: u64) -> u32 {
        let paddr = self.virtual_to_physical_address(vaddr, AccessMode::Read);
        self.memory
            .read_u32(paddr)
            .unwrap_or_else(|err| self.panic_read_failed(err, vaddr, paddr))
    }

    pub(crate) unsafe extern "C" fn read_u64(&mut self, vaddr: u64) -> u64 {
        let paddr = self.virtual_to_physical_address(vaddr, AccessMode::Read);
        self.memory
            .read_u64(paddr)
            .unwrap_or_else(|err| self.panic_read_failed(err, vaddr, paddr))
    }

    pub(crate) unsafe extern "C" fn write_u8(&mut self, vaddr: u64, value: u8) {
        let paddr = self.virtual_to_physical_address(vaddr, AccessMode::Write);
        self.memory
            .write_u8(paddr, value)
            .unwrap_or_else(|err| self.panic_write_failed(err, vaddr, paddr, value))
    }

    pub(crate) unsafe extern "C" fn write_u16(&mut self, vaddr: u64, value: u16) {
        let paddr = self.virtual_to_physical_address(vaddr, AccessMode::Write);
        self.memory
            .write_u16(paddr, value)
            .unwrap_or_else(|err| self.panic_write_failed(err, vaddr, paddr, value))
    }

    pub(crate) unsafe extern "C" fn write_u32(&mut self, vaddr: u64, value: u32) {
        let paddr = self.virtual_to_physical_address(vaddr, AccessMode::Write);
        self.memory
            .write_u32(paddr, value)
            .unwrap_or_else(|err| self.panic_write_failed(err, vaddr, paddr, value))
    }

    pub(crate) unsafe extern "C" fn write_u64(&mut self, vaddr: u64, value: u64) {
        let paddr = self.virtual_to_physical_address(vaddr, AccessMode::Write);
        self.memory
            .write_u64(paddr, value)
            .unwrap_or_else(|err| self.panic_write_failed(err, vaddr, paddr, value))
    }
}
