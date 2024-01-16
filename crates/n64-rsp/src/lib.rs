#![warn(clippy::all, clippy::pedantic)]
#![allow(clippy::missing_panics_doc)] // TODO remove

use self::register::Registers;
use n64_common::{
    memory::Section,
    utils::{boxed_array, thiserror::Error},
    SideEffects,
};
use std::{
    fmt,
    ops::Range,
    sync::{Arc, RwLock},
};

mod cpu;
mod dma;
mod register;

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
    #[must_use]
    pub fn as_section(self) -> Section {
        match self {
            Self::IMem => Section::RspIMemory,
            Self::DMem => Section::RspDMemory,
        }
    }

    /// Get the size of the memory bank in bytes
    #[must_use]
    #[allow(clippy::len_without_is_empty)] // Does not make sense on enums
    pub const fn len(self) -> usize {
        0x1000
    }
}

pub struct Rsp {
    registers: Registers,
    cpu: cpu::Handle,
    // TODO: remove the Box here, Arc is already heap-allocated
    dmem: Arc<RwLock<Box<[u8; MemoryBank::DMem.len()]>>>,
    imem: Arc<RwLock<Box<[u8; MemoryBank::IMem.len()]>>>,
}

impl Rsp {
    /// This copies the first 0x1000 bytes of the PIF ROM to the RSP DMEM, simulating IPL2.
    #[allow(clippy::similar_names)]
    #[must_use]
    pub fn new(pif_rom: &[u8]) -> Self {
        let dmem: Arc<_> = {
            let mut dmem = boxed_array();
            let len = dmem.len().min(pif_rom.len()).min(0x1000);
            dmem[..len].copy_from_slice(&pif_rom[..len]);
            RwLock::new(dmem).into()
        };
        let imem: Arc<_> = RwLock::new(boxed_array()).into();
        let cpu = cpu::Handle::new(dmem.clone(), imem.clone());

        Self {
            registers: Registers::new(),
            cpu,
            imem,
            dmem,
        }
    }

    /// Read a 32-bit integer from the RSP's memory-mapped COP0 registers
    ///
    /// # Errors
    /// Returns an error if the given offset is out of bounds
    pub fn read_register(&mut self, offset: usize) -> RspResult<u32> {
        if offset == 0x10 {
            // TODO: actual impl
            static mut FIRST: bool = true;
            unsafe {
                if FIRST {
                    FIRST = false;
                    self.cpu.set_sp_status(1);
                }
            }
            Ok(self.cpu.sp_status())
        } else if offset == 0x40000 {
            #[allow(clippy::cast_possible_truncation)]
            Ok(self.cpu.pc() as u32)
        } else {
            self.registers.read(offset)
        }
    }

    /// Write a 32-bit integer to the RSP's memory-mapped COP0 registers
    ///
    /// # Errors
    /// Returns an error if the given offset is out of bounds
    pub fn write_register(&mut self, offset: usize, value: u32) -> RspResult<SideEffects> {
        if offset == 0x10 {
            // TODO: actual impl
            let mut sp_status = register::definitions::SpStatus::from(self.cpu.sp_status());
            sp_status.write(value);
            self.cpu.set_sp_status(sp_status.into());
            Ok(SideEffects::default())
        } else if offset == 0x40000 {
            self.cpu.set_pc(u64::from(value));
            Ok(SideEffects::default())
        } else {
            self.registers.write(offset, value)
        }
    }

    /// Read `SIZE` bytes from the RSP's memory at the given bank and offset within said bank
    ///
    /// # Errors
    /// Returns an error if `offset..offset + SIZE` is out of bounds for the given bank
    pub fn read_sp_memory<const SIZE: usize>(
        &mut self,
        bank: MemoryBank,
        offset: usize,
    ) -> RspResult<[u8; SIZE]> {
        let range = offset..offset + SIZE;
        let slice = match bank {
            MemoryBank::IMem => &self.imem,
            MemoryBank::DMem => &self.dmem,
        }
        .read()
        .unwrap();
        slice
            .get(range.clone())
            .ok_or(RspError::MemoryRangeOutOfBounds { range, bank })
            // SAFETY: `slice.get()` returns a slice with the correct length
            .map(|slice| unsafe { slice.try_into().unwrap_unchecked() })
    }

    /// Write `SIZE.min(4).max(4)` bytes into the RSP's memory, at the given bank and offset within said bank
    ///
    /// # Errors
    /// Returns an error if `offset..offset + SIZE.min(4).max(4)` is out of bounds for the given bank
    pub fn write_sp_memory<const SIZE: usize>(
        &mut self,
        bank: MemoryBank,
        offset: usize,
        value: &[u8; SIZE],
    ) -> RspResult<()> {
        // Emulate a hardware quirk: 64-bit values are truncated to 32 bits, while 8-bit and 16-bit values are zero-extended to 32-bits.
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
        .write()
        .unwrap()
        .get_mut(range.clone())
        .ok_or(RspError::MemoryRangeOutOfBounds { range, bank })?
        .copy_from_slice(&value);

        Ok(())
    }

    pub fn tick(&mut self, cycles: usize, rdram: &mut [u8]) -> SideEffects {
        if (self.cpu.sp_status() & 1) != 1 {
            // if !self.registers.halted() {
            self.cpu.tick(cycles);
        }

        self.registers.tick(&mut dma::TickContext {
            cycles,
            rdram,
            dmem: self.dmem.write().unwrap().as_mut_slice(),
            imem: self.imem.write().unwrap().as_mut_slice(),
        })
    }
}

impl fmt::Debug for Rsp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("RSP")
            .field("registers", &self.registers)
            .finish_non_exhaustive()
    }
}
