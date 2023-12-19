//! Memory access for runtime environment.

use self::tlb::AccessMode;
use super::{
    bus::{Bus, Int},
    Environment, ValidRuntime,
};
use crate::target::Target;

pub(crate) mod tlb;

impl<'ctx, T: Target, B: Bus> Environment<'ctx, T, B>
where
    for<'a> Environment<'a, T, B>: ValidRuntime,
{
    pub fn read<const SIZE: usize>(
        &mut self,
        vaddr: u64,
        paddr: u32,
    ) -> Result<Int<SIZE>, B::Error> {
        self.debugger_signal_read(vaddr, SIZE);
        self.bus
            .read_memory(paddr)
            .map(|result| result.handle(self))
    }

    pub fn write<const SIZE: usize>(
        &mut self,
        vaddr: u64,
        paddr: u32,
        value: Int<SIZE>,
    ) -> Result<(), B::Error> {
        let vaddr_range = vaddr..(vaddr + SIZE as u64);
        self.codegen.labels.remove_within_range(vaddr_range);
        self.debugger_signal_write(vaddr, SIZE);
        self.bus
            .write_memory(paddr, value)
            .map(|result| result.handle(self))
    }

    pub(crate) fn read_or_panic<const SIZE: usize>(&mut self, vaddr: u64) -> Int<SIZE> {
        let paddr = self.virtual_to_physical_address(vaddr, AccessMode::Read);
        self.read(vaddr, paddr).unwrap_or_else(|err| {
            let msg = &format!("memory read failed (vaddr={vaddr:#x} paddr={paddr:#x}): {err:#x?}");
            self.panic_update_debugger(msg);
        })
    }

    fn read_paddr_or_panic<const SIZE: usize>(&mut self, paddr: u32) -> Int<SIZE> {
        let vaddr = self.physical_to_virtual_address(paddr, AccessMode::Read);
        self.read(vaddr, paddr).unwrap_or_else(|err| {
            let msg = &format!("memory read failed (vaddr={vaddr:#x} paddr={paddr:#x}): {err:#x?}");
            self.panic_update_debugger(msg);
        })
    }

    pub(crate) fn write_or_panic<const SIZE: usize>(&mut self, vaddr: u64, value: Int<SIZE>) {
        let paddr = self.virtual_to_physical_address(vaddr, AccessMode::Write);
        self.write(vaddr, paddr, value).unwrap_or_else(|err| {
            let msg = format!("memory write failed (vaddr={vaddr:#x} paddr={paddr:#x}): {err:#x?}");
            self.panic_update_debugger(&msg);
        })
    }

    fn write_paddr_or_panic<const SIZE: usize>(&mut self, paddr: u32, value: Int<SIZE>) {
        let vaddr = self.physical_to_virtual_address(paddr, AccessMode::Write);
        self.write(vaddr, paddr, value).unwrap_or_else(|err| {
            let msg = format!("memory write failed (vaddr={vaddr:#x} paddr={paddr:#x}): {err:#x?}");
            self.panic_update_debugger(&msg);
        })
    }

    /*
        Runtime functions. These are not meant to be called directly, but rather by JIT'ed code.
    */

    pub(crate) unsafe extern "C" fn read_u8(&mut self, vaddr: u64) -> u8 {
        self.read_or_panic(vaddr).into()
    }

    pub(crate) unsafe extern "C" fn read_u16(&mut self, vaddr: u64) -> u16 {
        self.read_or_panic(vaddr).into()
    }

    pub(crate) unsafe extern "C" fn read_u32(&mut self, vaddr: u64) -> u32 {
        self.read_or_panic(vaddr).into()
    }

    pub(crate) unsafe extern "C" fn read_u64(&mut self, vaddr: u64) -> u64 {
        self.read_or_panic(vaddr).into()
    }

    pub(crate) unsafe extern "C" fn write_u8(&mut self, vaddr: u64, value: u8) {
        self.write_or_panic(vaddr, value.into())
    }

    pub(crate) unsafe extern "C" fn write_u16(&mut self, vaddr: u64, value: u16) {
        self.write_or_panic(vaddr, value.into())
    }

    pub(crate) unsafe extern "C" fn write_u32(&mut self, vaddr: u64, value: u32) {
        self.write_or_panic(vaddr, value.into())
    }

    pub(crate) unsafe extern "C" fn write_u64(&mut self, vaddr: u64, value: u64) {
        self.write_or_panic(vaddr, value.into())
    }

    pub(crate) unsafe extern "C" fn read_physical_u8(&mut self, paddr: u32) -> u8 {
        self.read_paddr_or_panic(paddr).into()
    }

    pub(crate) unsafe extern "C" fn read_physical_u16(&mut self, paddr: u32) -> u16 {
        self.read_paddr_or_panic(paddr).into()
    }

    pub(crate) unsafe extern "C" fn read_physical_u32(&mut self, paddr: u32) -> u32 {
        self.read_paddr_or_panic(paddr).into()
    }

    pub(crate) unsafe extern "C" fn read_physical_u64(&mut self, paddr: u32) -> u32 {
        self.read_paddr_or_panic(paddr).into()
    }

    pub(crate) unsafe extern "C" fn write_physical_u8(&mut self, paddr: u32, value: u8) {
        self.write_paddr_or_panic(paddr, value.into())
    }

    pub(crate) unsafe extern "C" fn write_physical_u16(&mut self, paddr: u32, value: u16) {
        self.write_paddr_or_panic(paddr, value.into())
    }

    pub(crate) unsafe extern "C" fn write_physical_u32(&mut self, paddr: u32, value: u32) {
        self.write_paddr_or_panic(paddr, value.into())
    }

    pub(crate) unsafe extern "C" fn write_physical_u64(&mut self, paddr: u32, value: u64) {
        self.write_paddr_or_panic(paddr, value.into())
    }
}
