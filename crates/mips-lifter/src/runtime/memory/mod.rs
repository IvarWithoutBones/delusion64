//! Memory access for runtime environment.

use self::tlb::AccessMode;
use super::{
    bus::{self, Address, BusError, Int, MemorySection},
    Environment,
};
use std::ops::Range;

pub(crate) mod tlb;

impl<'ctx, Bus: bus::Bus> Environment<'ctx, Bus> {
    fn find_section(&self, paddr: u32) -> Result<&'static Bus::Section, BusError<Bus::Error>> {
        Bus::SECTIONS
            .iter()
            .find(|section| section.range().contains(&paddr))
            .ok_or(BusError::AddressNotMapped { address: paddr })
    }

    pub fn read<const SIZE: usize>(
        &mut self,
        paddr: u32,
    ) -> Result<Int<SIZE>, BusError<Bus::Error>> {
        self.find_section(paddr).and_then(|section| {
            let addr = Address::new(section, paddr);
            self.bus.read_memory(addr).map(|result| result.handle(self))
        })
    }

    pub fn write<const SIZE: usize>(
        &mut self,
        paddr: u32,
        value: Int<SIZE>,
    ) -> Result<(), BusError<Bus::Error>> {
        self.find_section(paddr).and_then(|section| {
            let addr = Address::new(section, paddr);
            self.bus.write_memory(addr, value).map(|result| {
                if section.auto_invalidate_written_addresses() {
                    // Use Address here to account for mirroring
                    let paddr = Address::new(section, paddr).physical_address();
                    self.invalidate(paddr..(paddr + SIZE as u32));
                }
                result.handle(self)
            })
        })
    }

    pub(crate) fn invalidate(&mut self, paddr_range: Range<u32>) {
        let vaddr_range = {
            let base = self
                .tlb
                .translate_paddr(paddr_range.start)
                .unwrap_or_else(|e| {
                    let msg = &format!("failed to convert paddr to vaddr: {e:#x?}");
                    self.panic_update_debugger(msg);
                }) as usize;
            base..(base + paddr_range.len())
        };

        self.codegen.get_mut().as_mut().unwrap().labels.retain(|l| {
            let start = l.label.range().start * 4;
            let end = l.label.range().end * 4;
            (start >= vaddr_range.start) || (end <= vaddr_range.end)
        });
    }

    fn read_or_panic<const SIZE: usize>(&mut self, vaddr: u64, paddr: u32) -> Int<SIZE> {
        self.read(paddr).unwrap_or_else(|err| {
            let message =
                format!("memory read failed at vaddr={vaddr:#x} paddr={paddr:#x}: {err:#x?}");
            self.panic_update_debugger(&message)
        })
    }

    fn write_or_panic<const SIZE: usize>(&mut self, vaddr: u64, paddr: u32, value: Int<SIZE>) {
        self.write(paddr, value).unwrap_or_else(|err| {
            let message = format!(
                "memory write of {value:#?} at vaddr={vaddr:#x} paddr={paddr:#x} failed: {err:#x?}"
            );
            self.panic_update_debugger(&message)
        })
    }

    /*
        Runtime functions. These are not meant to be called directly, but rather by JIT'ed code.
    */

    pub(crate) unsafe extern "C" fn read_u8(&mut self, vaddr: u64) -> u8 {
        let paddr = self.virtual_to_physical_address(vaddr, AccessMode::Read);
        self.read_or_panic(vaddr, paddr).into()
    }

    pub(crate) unsafe extern "C" fn read_u16(&mut self, vaddr: u64) -> u16 {
        let paddr = self.virtual_to_physical_address(vaddr, AccessMode::Read);
        self.read_or_panic(vaddr, paddr).into()
    }

    pub(crate) unsafe extern "C" fn read_u32(&mut self, vaddr: u64) -> u32 {
        let paddr = self.virtual_to_physical_address(vaddr, AccessMode::Read);
        self.read_or_panic(vaddr, paddr).into()
    }

    pub(crate) unsafe extern "C" fn read_u64(&mut self, vaddr: u64) -> u64 {
        let paddr = self.virtual_to_physical_address(vaddr, AccessMode::Read);
        self.read_or_panic(vaddr, paddr).into()
    }

    pub(crate) unsafe extern "C" fn write_u8(&mut self, vaddr: u64, value: u8) {
        let paddr = self.virtual_to_physical_address(vaddr, AccessMode::Write);
        self.write_or_panic(vaddr, paddr, value.into())
    }

    pub(crate) unsafe extern "C" fn write_u16(&mut self, vaddr: u64, value: u16) {
        let paddr = self.virtual_to_physical_address(vaddr, AccessMode::Write);
        self.write_or_panic(vaddr, paddr, value.into())
    }

    pub(crate) unsafe extern "C" fn write_u32(&mut self, vaddr: u64, value: u32) {
        let paddr = self.virtual_to_physical_address(vaddr, AccessMode::Write);
        self.write_or_panic(vaddr, paddr, value.into())
    }

    pub(crate) unsafe extern "C" fn write_u64(&mut self, vaddr: u64, value: u64) {
        let paddr = self.virtual_to_physical_address(vaddr, AccessMode::Write);
        self.write_or_panic(vaddr, paddr, value.into())
    }
}
