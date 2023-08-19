//! Memory access for runtime environment.

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
