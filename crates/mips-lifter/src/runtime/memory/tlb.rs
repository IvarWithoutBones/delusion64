use crate::runtime::Registers;
use mips_decomp::register::{
    self,
    cp0::{EntryHi, EntryLo, OperatingMode, PageMask},
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Bits {
    Bits32,
    Bits64,
}

impl<'a> From<&'a Registers> for Bits {
    fn from(regs: &'a Registers) -> Self {
        match regs.status().is_64_bits() {
            true => Bits::Bits64,
            false => Bits::Bits32,
        }
    }
}

bitfield! {
    struct VirtualAddress(u64) {
        [0..=28] virtual_page_number_and_offset_32: u64,
        [29..=31] mode_32: u8 as OperatingMode,
        [0..=39] virtual_page_number_and_offset_64: u64,
        [62..=63] mode_64: u8 as OperatingMode,
    }
}

impl VirtualAddress {
    fn virtual_page_number_and_offset(&self, bits: Bits) -> u64 {
        match bits {
            Bits::Bits32 => self.virtual_page_number_and_offset_32(),
            Bits::Bits64 => self.virtual_page_number_and_offset_64(),
        }
    }

    fn offset(&self, mask: PageMask, bits: Bits) -> u32 {
        // The virtual page number and offset are dynamic in size, depending on the page mask.
        (self.virtual_page_number_and_offset(bits) & mask.mask()) as u32
    }

    fn virtual_page_number(&self, mask: PageMask, bits: Bits) -> u32 {
        (self.virtual_page_number_and_offset(bits) >> mask.mask().count_ones()) as u32
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
        let vpn = match bits {
            Bits::Bits32 => self.hi.virtual_page_number_32(),
            Bits::Bits64 => self.hi.virtual_page_number_64(),
        };
        if vaddr.virtual_page_number(self.page_mask, bits) != vpn {
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

    fn translate_unmapped(vaddr: u64) -> Option<u32> {
        let vaddr = vaddr as u32;
        match vaddr {
            _ if Self::KSEG0.contains(&vaddr) => Some(vaddr - Self::KSEG0.start),
            _ if Self::KSEG1.contains(&vaddr) => Some(vaddr - Self::KSEG1.start),
            _ => None,
        }
    }

    /// Translate a virtual address to a physical address.
    pub fn translate(
        &self,
        vaddr: u64,
        mode: AccessMode,
        regs: &Registers,
    ) -> Result<u32, TranslationError> {
        if let Some(paddr) = Self::translate_unmapped(vaddr) {
            return Ok(paddr);
        }

        // The ASID should only be checked if the global bit is not set on both EntryLo registers of CP0.
        let hi = [regs[register::Cp0::EntryLo0], regs[register::Cp0::EntryLo1]]
            .iter()
            .all(|lo| EntryLo::new(*lo).global())
            .then(|| EntryHi::new(regs[register::Cp0::EntryHi]));
        let vaddr = VirtualAddress(vaddr);
        let bits = Bits::from(regs);

        self.entries
            .iter()
            .find(|pte| pte.matches(vaddr, bits, hi))
            .ok_or(TranslationError::Miss)
            .and_then(|pte| pte.physical_address(vaddr, bits, mode))
    }

    pub fn set_entry(&mut self, index: usize, registers: &Registers) -> Option<()> {
        self.entries.get_mut(index).map(|entry| {
            *entry = Entry {
                page_mask: PageMask::new(registers[register::Cp0::PageMask] as u32),
                hi: EntryHi::new(registers[register::Cp0::EntryHi]),
                lo: [
                    EntryLo::new(registers[register::Cp0::EntryLo0]),
                    EntryLo::new(registers[register::Cp0::EntryLo1]),
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
