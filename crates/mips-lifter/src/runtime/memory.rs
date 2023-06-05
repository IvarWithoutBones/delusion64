#![allow(dead_code)]
//! Memory access for runtime environment.

use std::{mem::size_of, ops::Range};
use tartan_bitfield::bitfield;

const KSEG0: Range<u64> = 0x8000_0000..0xA000_0000;
const KSEG1: Range<u64> = 0xA000_0000..0xC000_0000;

bitfield! {
    /// A 32-bit physical address.
    pub struct PhysicalAddress32(u32) {
        [0..=11] offset: u16,
        [12..=31] physical_frame_number: u32,
    }
}

bitfield! {
    /// A 32-bit virtual address for 4KB pages.
    pub struct VirtualAddress32(u64) {
        [0..=11] offset: u16,
        [12..=29] virtual_page_number: u32,
        [30..=31] mode: u8,
        [32..=39] asid: u8,
    }
}

bitfield! {
    /// CP0 register `EntryLo0`/`EntryLo1`
    pub struct EntryLo32(u32) {
        /// `G` bit
        [0] global,
        /// `V` bit
        [1] valid,
        /// `D` bit
        [2] dirty,
        /// `C` bit
        [3..=5] page_attr: u8,
        /// `PFN` bits
        [6..=25] physical_frame_number: u32,
    }
}

bitfield! {
    /// CP0 register `EntryHi`
    pub struct EntryHi32(u32) {
        /// `ASID` bits
        [0..=7] asid: u8,
        /// `G` bit
        [13] global,
        /// `VPN2` bits
        [14..=31] virtual_page_number: u32,
    }
}

bitfield! {
    /// CP0 register `PageMask`
    pub struct PageMask(u32) {
        /// `MASK` bits
        [13..=25] mask: u16,
    }
}

bitfield! {
    /// A TLB entry in 32-bit mode.
    pub struct TlbEntry32(u128) {
        /// CP0 register `EntryLo0`
        [0..=31] entry_lo_0: u32 as EntryLo32,
        /// CP0 register `EntryLo1`
        [32..=63] entry_lo_1: u32 as EntryLo32,
        /// CP0 register `EntryHi`
        [64..=95] entry_hi: u32 as EntryHi32,
        /// CP0 register `PageMask`
        [96..=127] page_mask: u32 as PageMask,
    }
}

impl TlbEntry32 {
    pub fn global(&self) -> bool {
        self.entry_lo_0().global() && self.entry_lo_1().global()
    }
}

#[derive(Default)]
pub struct TranslationLookasideBuffer {
    entries: [TlbEntry32; 32],
}

impl TranslationLookasideBuffer {
    fn find(&self, vaddr: VirtualAddress32, cp0_asid: u8) -> Option<&TlbEntry32> {
        self.entries.iter().find(|entry| {
            let vpn_match = entry.entry_hi().virtual_page_number() == vaddr.virtual_page_number();
            let asid_match = entry.global() || (entry.entry_hi().asid() == cp0_asid);
            vpn_match && asid_match
        })
    }

    fn probe(&self, vaddr_raw: u64, cp0_asid: u8) -> Option<u64> {
        let vaddr = VirtualAddress32(vaddr_raw);
        let entry = self.find(vaddr, cp0_asid)?;
        let even = (vaddr_raw & ((entry.page_mask().mask() + 1) as u64)) == 0;

        // TODO: error handling in case the page is not valid
        let pfn = if even {
            entry.entry_lo_0().physical_frame_number()
        } else {
            entry.entry_lo_1().physical_frame_number()
        };

        let paddr = PhysicalAddress32::default()
            .with_physical_frame_number(pfn)
            .with_offset(vaddr.offset());
        Some(paddr.0 as _)
    }

    /// Translate a virtual address to a physical address.
    pub fn translate(&self, vaddr: u64, cp0_entry_hi: u64) -> Option<u64> {
        match vaddr {
            // Physically addressed segments
            _ if KSEG0.contains(&vaddr) => Some(vaddr - KSEG0.start),
            _ if KSEG1.contains(&vaddr) => Some(vaddr - KSEG1.start),

            // TLB mapped segments
            _ => {
                let cp0_asid = EntryHi32(cp0_entry_hi as _).asid();
                self.probe(vaddr, cp0_asid)
            }
        }
    }
}

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
