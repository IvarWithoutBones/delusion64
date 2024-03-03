#![warn(clippy::all, clippy::pedantic)]

use crate::{memory::Memory, register::Registers};
use mips_lifter::target::rsp::register::control::Status;
use n64_common::{utils::thiserror, SideEffects};
use std::{net::TcpStream, ops::Range};

pub use mips_lifter::target::rsp::register::control::MemoryBank;

mod dma;
mod jit;
mod memory;
mod register;

#[derive(thiserror::Error, Debug)]
pub enum RspError {
    #[error("Out of bounds register offset: {offset:#x}")]
    RegisterOffsetOutOfBounds { offset: usize },
    #[error("Out of bounds SP memory range for bank {bank:#?}: {range:#x?}")]
    MemoryRangeOutOfBounds {
        range: Range<usize>,
        bank: MemoryBank,
    },
    #[error("Failed to acquire read lock for bank {bank:#?}")]
    ReadPoisonError { bank: MemoryBank },
    #[error("Failed to acquire write lock for bank {bank:#?}")]
    WritePoisonError { bank: MemoryBank },
}

pub type RspResult<T> = Result<T, RspError>;

#[derive(Debug)]
pub struct Rsp {
    registers: Registers,
    cpu: jit::Handle,
    memory: Memory,
}

impl Rsp {
    /// This copies the first 0x1000 bytes of the PIF ROM to the RSP DMEM, simulating IPL2.
    #[must_use]
    pub fn new(pif_rom: &[u8], gdb: Option<TcpStream>) -> Self {
        let memory = Memory::new(pif_rom);
        let (cpu, registers) = jit::Handle::new(memory.clone(), gdb);
        Self {
            registers,
            cpu,
            memory,
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
    ) -> RspResult<[u8; SIZE]> {
        let range = offset..offset + SIZE;
        self.memory
            .read(bank)?
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
        self.memory
            .write(bank)?
            .get_mut(range.clone())
            .ok_or(RspError::MemoryRangeOutOfBounds { range, bank })?
            .copy_from_slice(&value);
        Ok(())
    }

    /// Tick the RSP for the given number of cycles.
    ///
    /// # Errors
    /// Returns an error if the RSP is in an invalid state
    pub fn tick(&mut self, cycles: usize, rdram: &mut [u8]) -> RspResult<SideEffects> {
        if !self.registers.control.read_parsed::<Status>().halted() {
            self.cpu.tick(cycles);
        }

        self.registers.tick(&mut dma::TickContext {
            memory: &mut self.memory,
            cycles,
            rdram,
        })
    }
}
