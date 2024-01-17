use crate::{runtime::register_bank::RegIndex, target};
use mips_decomp::register::{
    self,
    cpu::cp0::{Bits, EntryHi, EntryLo, Index, OperatingMode, PageMask},
};
use std::ops::Range;
use tartan_bitfield::bitfield;

/// An error that can occur when translating a virtual address to a physical address.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TranslationError {
    /// The address was not found in the TLB.
    Miss,
    /// The address was found in the TLB, but the entry was invalid.
    EntryInvalid,
    /// The address was found in the TLB, but the entry was not dirty and the access was a write.
    Modification,
    /// The address is illegal (TODO: handle this).
    #[allow(dead_code)]
    Address,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AccessMode {
    Read,
    Write,
}

impl<'a> From<&'a target::cpu::Registers> for Bits {
    fn from(regs: &'a target::cpu::Registers) -> Self {
        match regs.status().is_64_bits() {
            true => Bits::Bits64,
            false => Bits::Bits32,
        }
    }
}

bitfield! {
    pub struct VirtualAddress(u64) {
        [0..=28] virtual_page_number_and_offset_32: u64,
        [29..=31] mode_32: u8 as OperatingMode,
        [0..=39] pub virtual_page_number_and_offset_64: u64,
        [62..=63] pub mode_64: u8 as OperatingMode,
    }
}

impl VirtualAddress {
    pub fn new(vaddr: u64) -> Self {
        Self(vaddr)
    }

    fn virtual_page_number_and_offset(&self, bits: Bits) -> u64 {
        match bits {
            Bits::Bits32 => self.virtual_page_number_and_offset_32(),
            Bits::Bits64 => self.virtual_page_number_and_offset_64(),
        }
    }

    pub fn offset(&self, mask: PageMask, bits: Bits) -> u32 {
        // The virtual page number and offset are dynamic in size, depending on the page mask.
        (self.virtual_page_number_and_offset(bits) & mask.mask()) as u32
    }

    pub fn virtual_page_number(&self, mask: PageMask, bits: Bits) -> u32 {
        (self.virtual_page_number_and_offset(bits) >> (mask.mask().count_ones() + 1)) as u32
    }
}

#[derive(Default, Debug)]
struct Entry {
    page_mask: PageMask,
    hi: EntryHi,
    lo: [EntryLo; 2],
}

impl Entry {
    fn entry_lo(&self, vaddr: VirtualAddress, bits: Bits) -> EntryLo {
        // EntryLo0 is used for even virtual pages and EntryLo1 for odd virtual pages.
        let odd = (vaddr.virtual_page_number(self.page_mask, bits) % 2) as usize;
        unsafe { *self.lo.get(odd).unwrap_unchecked() } // SAFETY: The index cannot be out of bounds.
    }

    /// Generates a physical address from the given virtual address.
    fn physical_address(
        &self,
        vaddr: VirtualAddress,
        bits: Bits,
        mode: AccessMode,
    ) -> Result<u32, TranslationError> {
        let lo = self.entry_lo(vaddr, bits);
        if !lo.valid() {
            return Err(TranslationError::EntryInvalid);
        } else if mode == AccessMode::Write && !lo.dirty() {
            return Err(TranslationError::Modification);
        }

        let pfn = lo.page_frame_number() << self.page_mask.mask().count_ones();
        Ok(pfn | vaddr.offset(self.page_mask, bits))
    }

    fn matches(&self, vaddr: VirtualAddress, bits: Bits, cp0_hi: Option<EntryHi>) -> bool {
        if vaddr.virtual_page_number(self.page_mask, bits) != self.hi.virtual_page_number(bits) {
            return false;
        }

        // If we did not receive the cp0's EntryHi register we dont need to check the ASID.
        if let Some(cp0_hi) = cp0_hi {
            if !self.hi.global() && (cp0_hi.address_space_id() != self.hi.address_space_id()) {
                return false;
            }
        }

        true
    }
}

#[derive(Default)]
pub(crate) struct TranslationLookasideBuffer {
    entries: [Entry; 32],
}

impl TranslationLookasideBuffer {
    // Physically addressed (unmapped) segments
    const KSEG0: Range<u32> = 0x8000_0000..0xA000_0000;
    const KSEG1: Range<u32> = 0xA000_0000..0xC000_0000;

    fn translate_unmapped_vaddr(vaddr: u64) -> Option<u32> {
        let vaddr = vaddr as u32;
        match vaddr {
            _ if Self::KSEG0.contains(&vaddr) => Some(vaddr - Self::KSEG0.start),
            _ if Self::KSEG1.contains(&vaddr) => Some(vaddr - Self::KSEG1.start),
            _ => None,
        }
    }

    fn translate_unmapped_paddr(paddr: u32) -> Option<u64> {
        let kseg0 = paddr + Self::KSEG0.start;
        let kseg1 = paddr + Self::KSEG1.start;
        if Self::KSEG0.contains(&kseg0) {
            Some(kseg0 as u64)
        } else if Self::KSEG1.contains(&kseg1) {
            Some(kseg1 as u64)
        } else {
            None
        }
    }

    /// Translate a physical address to a virtual address.
    pub fn translate_paddr(&self, paddr: u32) -> Result<u64, TranslationError> {
        if let Some(vaddr) = Self::translate_unmapped_paddr(paddr) {
            Ok(vaddr)
        } else {
            todo!("translate_paddr for TLB mapped addresses")
        }
    }

    /// Translate a virtual address to a physical address.
    pub fn translate_vaddr(
        &self,
        vaddr: u64,
        mode: AccessMode,
        regs: &target::cpu::Registers,
    ) -> Result<u32, TranslationError> {
        if let Some(paddr) = Self::translate_unmapped_vaddr(vaddr) {
            return Ok(paddr);
        }

        // The ASID should only be checked if the global bit is not set on both EntryLo registers of CP0.
        let hi = [
            regs.read(register::cpu::Cp0::EntryLo0),
            regs.read(register::cpu::Cp0::EntryLo1),
        ]
        .iter()
        .all(|lo| EntryLo::new(*lo).global())
        .then(|| EntryHi::new(regs.read(register::cpu::Cp0::EntryHi)));
        let vaddr = VirtualAddress(vaddr);
        let bits = Bits::from(regs);

        self.entries
            .iter()
            .find(|pte| pte.matches(vaddr, bits, hi))
            .ok_or(TranslationError::Miss)
            .and_then(|pte| pte.physical_address(vaddr, bits, mode))
    }

    pub fn probe(&mut self, regs: &target::cpu::Registers) -> Index {
        let bits = Bits::from(regs);
        let hi = EntryHi::new(regs.read(register::cpu::Cp0::EntryHi));

        let mut index = Index::new(regs.read(register::cpu::Cp0::Index) as u32);
        let mut found = false;
        for (i, entry) in self.entries.iter().enumerate() {
            if entry.hi.virtual_page_number(bits) != hi.virtual_page_number(bits) {
                continue;
            }
            if !entry.hi.global() && (hi.address_space_id() != entry.hi.address_space_id()) {
                continue;
            }

            found = true;
            index.set_index(i as u8);
            break;
        }
        index.with_probe_successfull(found)
    }

    pub fn read_entry(
        &mut self,
        index: usize,
        registers: &mut target::cpu::Registers,
    ) -> Option<()> {
        self.entries.get(index).map(|entry| {
            registers.write(register::cpu::Cp0::EntryHi, entry.hi.into());
            // For TLBR: The G bit read from the TLB is written into both of the EntryLo0 and EntryLo1 registers.
            let global = entry.hi.global();
            registers.write(
                register::cpu::Cp0::EntryLo0,
                entry.lo[0].clone().with_global(global).into(),
            );
            registers.write(
                register::cpu::Cp0::EntryLo1,
                entry.lo[1].clone().with_global(global).into(),
            );
        })
    }

    pub fn write_entry(&mut self, index: usize, registers: &target::cpu::Registers) -> Option<()> {
        self.entries.get_mut(index).map(|entry| {
            *entry = Entry {
                page_mask: PageMask::new(registers.read(register::cpu::Cp0::PageMask) as u32),
                hi: EntryHi::new(registers.read(register::cpu::Cp0::EntryHi)),
                lo: [
                    EntryLo::new(registers.read(register::cpu::Cp0::EntryLo0)),
                    EntryLo::new(registers.read(register::cpu::Cp0::EntryLo1)),
                ],
            }
        })
    }
}

impl std::fmt::Debug for TranslationLookasideBuffer {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("TranslationLookasideBuffer")
            .field("entries", &format_args!("{:#x?}", self.entries))
            .finish()
    }
}
