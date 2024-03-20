use crate::{RspError, RspResult};
use mips_lifter::target::rsp::register::control::MemoryBank;
use n64_common::utils::boxed_array;
use std::{
    cmp::min,
    sync::{Arc, RwLock, RwLockReadGuard, RwLockWriteGuard},
};

type MemoryBankStorage = Box<[u8; MemoryBank::LEN]>;
type MemoryBankHandle = Arc<RwLock<MemoryBankStorage>>;

/// RSP memory, consisting of data memory and instruction memory.
/// Cloning this creates another handle to the same memory, and is fairly cheap.
#[derive(Clone)]
pub(crate) struct Memory {
    /// Data memory, known as `DMEM`
    pub data: MemoryBankHandle,
    /// Instruction memory, known as `IMEM`
    pub instruction: MemoryBankHandle,
}

impl Memory {
    pub fn new(pif_rom: &[u8]) -> Self {
        let data = {
            // Copy the first 0x1000 bytes of the PIF ROM to the RSP DMEM, simulating IPL2.
            let mut dmem = boxed_array();
            let len = min(pif_rom.len(), MemoryBank::LEN);
            dmem[..len].copy_from_slice(&pif_rom[..len]);
            Arc::from(RwLock::new(dmem))
        };
        let instruction = Arc::from(RwLock::new(boxed_array()));
        Self { data, instruction }
    }

    pub fn read(&self, bank: MemoryBank) -> RspResult<RwLockReadGuard<'_, MemoryBankStorage>> {
        match bank {
            MemoryBank::IMem => &self.instruction,
            MemoryBank::DMem => &self.data,
        }
        .read()
        .map_err(|_| RspError::ReadPoisonError { bank })
    }

    pub fn write(
        &mut self,
        bank: MemoryBank,
    ) -> RspResult<RwLockWriteGuard<'_, MemoryBankStorage>> {
        match bank {
            MemoryBank::IMem => &self.instruction,
            MemoryBank::DMem => &self.data,
        }
        .write()
        .map_err(|_| RspError::WritePoisonError { bank })
    }
}

impl std::fmt::Debug for Memory {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // Avoid printing the entire contents of memory
        f.debug_struct("Memory").finish_non_exhaustive()
    }
}
