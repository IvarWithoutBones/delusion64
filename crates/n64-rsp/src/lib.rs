#![warn(clippy::all, clippy::pedantic)]

use self::register::Registers;
use std::ops::Range;
use thiserror::Error;

pub mod register;

// TODO: deduplicate, this is stolen from delusion64::bus.
/// Allocates a fixed-sized boxed array of a given length.
fn boxed_array<T: Default + Clone, const LEN: usize>() -> Box<[T; LEN]> {
    // Use a Vec to allocate directly onto the heap. Using an array will allocate on the stack,
    // which can cause a stack overflow. SAFETY: We're sure the input size matches the output size.
    let result = vec![Default::default(); LEN].into_boxed_slice();
    unsafe { result.try_into().unwrap_unchecked() }
}

/// Errors which can occur when interacting with the RSP
#[derive(Error, Debug)]
pub enum RspError {
    #[error("Out of bounds register offset: {offset:#x}")]
    RegisterOffsetOutOfBounds { offset: usize },
    #[error("Out of bounds SP memory range for bank {bank:#?}: {range:#x?}")]
    MemoryRangeOutOfBounds {
        range: Range<usize>,
        bank: MemoryBank,
    },
}

pub type RspResult<T> = Result<T, RspError>;

/// The bank of memory accessed by a DMA transfer
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum MemoryBank {
    IMem,
    DMem,
}

impl MemoryBank {
    /// Get the size of the memory bank in bytes
    #[must_use]
    #[allow(clippy::len_without_is_empty)] // Does not make sense on enums
    pub const fn len(self) -> usize {
        0x1000
    }
}

/// A change which should be applied to the MI's interrupt state
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum InterruptChange {
    Set,
    Clear,
}

#[derive(Debug, Default, Copy, Clone, PartialEq, Eq)]
pub struct SideEffects {
    /// Whether or not the MI interrupt state should be changed
    pub interrupt: Option<InterruptChange>,
}

#[derive(Debug)]
pub struct Rsp {
    registers: Registers,
    dmem: Box<[u8; MemoryBank::DMem.len()]>,
    imem: Box<[u8; MemoryBank::IMem.len()]>,
}

impl Rsp {
    // This copies the first 0x1000 bytes of the PIF ROM to the RSP DMEM, simulating IPL2.
    #[must_use]
    pub fn new(pif_rom: &[u8]) -> Self {
        let mut dmem = boxed_array();
        let len = dmem.len().min(pif_rom.len()).min(0x1000);
        dmem[..len].copy_from_slice(&pif_rom[..len]);

        Self {
            registers: Registers::new(),
            imem: boxed_array(),
            dmem,
        }
    }

    /// Read a 32-bit integer from the RSP's memory-mapped COP0 registers
    ///
    /// # Errors
    /// Returns an error if the given offset is out of bounds
    pub fn read_register(&mut self, offset: usize) -> RspResult<u32> {
        self.registers.read(offset)
    }

    /// Write a 32-bit integer to the RSP's memory-mapped COP0 registers
    ///
    /// # Errors
    /// Returns an error if the given offset is out of bounds
    pub fn write_register(&mut self, offset: usize, value: u32) -> RspResult<SideEffects> {
        self.registers.write(offset, value)
    }

    /// Read `SIZE` bytes from the RSP's memory at the given bank and offset within said bank
    ///
    /// # Errors
    /// Returns an error if `offset..offset + SIZE` is out of bounds for the given bank
    pub fn read_sp_memory<const SIZE: usize>(
        &mut self,
        bank: MemoryBank,
        offset: usize,
    ) -> RspResult<&[u8; SIZE]> {
        let range = offset..offset + SIZE;
        match bank {
            MemoryBank::IMem => &self.imem,
            MemoryBank::DMem => &self.dmem,
        }
        .get(range.clone())
        .ok_or(RspError::MemoryRangeOutOfBounds { range, bank })
        // SAFETY: `slice.get()` returns a slice with the correct length
        .map(|slice| unsafe { slice.try_into().unwrap_unchecked() })
    }

    /// Write `SIZE.min(4)` bytes from the RSP's memory at the given bank and offset within said bank
    ///
    /// # Errors
    /// Returns an error if `offset..offset + SIZE.min(4)` is out of bounds for the given bank
    pub fn write_sp_memory<const SIZE: usize>(
        &mut self,
        bank: MemoryBank,
        offset: usize,
        value: &[u8; SIZE],
    ) -> RspResult<()> {
        // Emulate a hardware quirk: 64-bit values are truncated to 32 bits, while 8-bit and 16-bit values are zero-extended.
        let value = {
            let mut res = [0_u8; 4];
            let len = res.len().min(SIZE);
            res[..len].copy_from_slice(&value[..len]);
            res
        };

        let range = offset..offset + value.len();
        match bank {
            MemoryBank::IMem => &mut self.imem,
            MemoryBank::DMem => &mut self.dmem,
        }
        .get_mut(range.clone())
        .ok_or(RspError::MemoryRangeOutOfBounds { range, bank })?
        .copy_from_slice(&value);

        Ok(())
    }
}
