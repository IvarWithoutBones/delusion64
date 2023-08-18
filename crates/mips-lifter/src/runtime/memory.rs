//! Memory access for runtime environment.

use std::{fmt, ops::Range};
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
        [0] pub global,
        /// `V` bit
        [1] pub valid,
        /// `D` bit
        [2] pub dirty,
        /// `C` bits
        [3..=5] pub page_attr: u8,
        /// `PFN` bits
        [6..=25] pub physical_frame_number: u32,
    }
}

bitfield! {
    /// CP0 register `EntryHi`
    pub struct EntryHi32(u32) {
        /// `ASID` bits
        [0..=7] pub asid: u8,
        /// `G` bit
        [13] pub global,
        /// `VPN2` bits
        [14..=31] pub virtual_page_number: u32,
    }
}

bitfield! {
    /// CP0 register `PageMask`
    pub struct PageMask(u32) {
        /// `MASK` bits
        [13..=25] pub mask: u16,
    }
}

bitfield! {
    /// A TLB entry in 32-bit mode.
    pub struct TlbEntry32(u128) {
        /// CP0 register `EntryLo0`
        [0..=31] pub entry_lo_0: u32 as EntryLo32,
        /// CP0 register `EntryLo1`
        [32..=63] pub entry_lo_1: u32 as EntryLo32,
        /// CP0 register `EntryHi`
        [64..=95] pub entry_hi: u32 as EntryHi32,
        /// CP0 register `PageMask`
        [96..=127] pub page_mask: u32 as PageMask,
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
    fn load(&self, vaddr: u64) -> Option<u64> {
        for entry in &self.entries {
            let mask: u32 = (entry.page_mask().mask() as u32 >> 1) | 0x0FFF;
            let page_size: u32 = mask + 1;

            let tmp: u16 = (entry.page_mask().mask() | 0x1FFF) as _;
            let vpn: u32 = (entry.entry_hi().0 & ((!tmp) as u32)) as _;
            let masked_vaddr: u32 = (vaddr & (vpn as u64)) as _;

            // println!("\nvaddr:        {vaddr:064b} ({vaddr:#x})\nmasked vaddr: {masked_vaddr:032b} ({masked_vaddr:#x})\nvpn:          {vpn:032b} ({vpn:#x})\npage size:    {page_size:032b} ({page_size:#x})");

            if masked_vaddr != vpn {
                println!("vpn mismatch");
                continue;
            }

            let odd: u32 = (vaddr & (page_size as u64)) as _;

            let pfn: u32 = {
                let entry_lo = if odd == 0 {
                    entry.entry_lo_0()
                } else {
                    entry.entry_lo_1()
                };

                if (entry_lo.0 & 0x02) == 0 {
                    println!("tlb entry invalid ({vaddr:#x})");
                    continue;
                }

                (entry_lo.0 >> 6) & 0x00FF_FFFF
            };

            let paddr = 0x80000000 | (pfn * page_size) | (vaddr & mask as u64) as u32;
            println!("paddr: {paddr:#x}");
            return Some(paddr as _);
        }

        None
    }

    fn translate_unmapped(vaddr: u64) -> Option<u64> {
        match vaddr {
            // Physically addressed segments
            _ if KSEG0.contains(&vaddr) => Some(vaddr - KSEG0.start),
            _ if KSEG1.contains(&vaddr) => Some(vaddr - KSEG1.start),
            _ => None,
        }
    }

    /// Translate a virtual address to a physical address.
    pub fn translate(&self, vaddr: u64) -> Option<u64> {
        if let Some(paddr) = Self::translate_unmapped(vaddr) {
            Some(paddr)
        } else {
            self.load(vaddr)
        }
    }

    pub fn set_entry(&mut self, index: usize, entry: TlbEntry32) -> Option<()> {
        *self.entries.get_mut(index)? = entry;
        Some(())
    }
}

/// A trait providing memory access for runtime environment.
pub trait Memory {
    /// The type of error that can occur when accessing memory.
    type AccessError: fmt::Debug;

    /// Read a u8 from the given physical address.
    fn read_u8(&self, addr: u64) -> Result<u8, Self::AccessError>;

    /// Read a u16 from the given physical address.
    fn read_u16(&self, addr: u64) -> Result<u16, Self::AccessError>;

    /// Read a u32 from the given physical address.
    fn read_u32(&self, addr: u64) -> Result<u32, Self::AccessError>;

    /// Read a u64 from the given physical address.
    fn read_u64(&self, addr: u64) -> Result<u64, Self::AccessError>;

    /// Write a u8 to the given physical address.
    fn write_u8(&mut self, addr: u64, value: u8) -> Result<(), Self::AccessError>;

    /// Write a u16 to the given physical address.
    fn write_u16(&mut self, addr: u64, value: u16) -> Result<(), Self::AccessError>;

    /// Write a u32 to the given physical address.
    fn write_u32(&mut self, addr: u64, value: u32) -> Result<(), Self::AccessError>;

    /// Write a u64 to the given physical address.
    fn write_u64(&mut self, addr: u64, value: u64) -> Result<(), Self::AccessError>;
}
