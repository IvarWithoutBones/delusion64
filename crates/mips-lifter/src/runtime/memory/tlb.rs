use crate::runtime::Registers;
use mips_decomp::register;
use std::{fmt, ops::Range};
use strum::FromRepr;
use tartan_bitfield::{bitfield, bitfield_without_debug};

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

#[derive(FromRepr, Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum OperatingMode {
    User = 0b00,
    Supervisor = 0b01,
    Kernel = 0b10,
}

bitfield! {
    struct VirtualAddress64(u64) {
        [0..=39] virtual_page_number_and_offset: u64,
        [62..=63] mode: u8,
    }
}

bitfield! {
    struct VirtualAddress32(u64) {
        [0..=28] virtual_page_number_and_offset: u64,
        [29..=31] mode: u8,
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum VirtualAddress {
    Bits32(VirtualAddress32),
    Bits64(VirtualAddress64),
}

impl VirtualAddress {
    fn mode(&self) -> Option<OperatingMode> {
        match self {
            VirtualAddress::Bits32(vaddr) => OperatingMode::from_repr(vaddr.mode()),
            VirtualAddress::Bits64(vaddr) => OperatingMode::from_repr(vaddr.mode()),
        }
    }

    fn virtual_page_number_and_offset(&self) -> u64 {
        match self {
            VirtualAddress::Bits32(vaddr) => vaddr.virtual_page_number_and_offset(),
            VirtualAddress::Bits64(vaddr) => vaddr.virtual_page_number_and_offset(),
        }
    }

    fn offset(&self, mask: PageMask) -> u32 {
        // The virtual page number and offset are dynamic in size, depending on the page mask.
        (self.virtual_page_number_and_offset() & mask.mask()) as u32
    }

    fn virtual_page_number(&self, mask: PageMask) -> u32 {
        // The virtual page number and offset are dynamic in size, depending on the page mask.
        (self.virtual_page_number_and_offset() >> mask.mask().count_ones()) as u32
    }
}

impl From<VirtualAddress32> for VirtualAddress {
    fn from(val: VirtualAddress32) -> Self {
        VirtualAddress::Bits32(val)
    }
}

impl From<VirtualAddress64> for VirtualAddress {
    fn from(val: VirtualAddress64) -> Self {
        VirtualAddress::Bits64(val)
    }
}

bitfield_without_debug! {
    /// Configures the size of a TLB entry.
    struct PageMask(u32) {
        [13..=24] raw: u16,
    }
}

impl PageMask {
    pub fn mask(&self) -> u64 {
        // The mask covers at least 12 bits, up to 12 + raw bits. See Table 5-7 in the VR4300 manual.
        debug_assert!(self.raw().count_ones() % 2 == 0);
        debug_assert!(self.raw().count_ones() <= 12);
        const START: u64 = 0b1111_1111_1111;
        ((self.raw() as u64) << START.count_ones()) | START
    }
}

bitfield_without_debug! {
    struct EntryHi(u64) {
        [0..=7] address_space_id: u8,
        /// NOTE: Only defined for TLB entries.
        [12] global,
        [13..=39] virtual_page_number_64: u32,
        [62..=63] mode_64: u8,
    }
}

impl EntryHi {
    fn virtual_page_number_32(&self) -> u32 {
        // The `VPN2` field is 19 bits wide in 32-bit mode and 27 bits wide in 64-bit mode.
        self.virtual_page_number_64() & 0b111_1111_1111_1111_1111
    }

    fn virtual_page_number(&self, vaddr: VirtualAddress) -> u32 {
        match vaddr {
            VirtualAddress::Bits32(_) => self.virtual_page_number_32(),
            VirtualAddress::Bits64(_) => self.virtual_page_number_64(),
        }
    }

    fn mode(&self) -> Option<OperatingMode> {
        OperatingMode::from_repr(self.mode_64())
    }
}

bitfield! {
    struct EntryLo(u64) {
        /// NOTE: Not defined for TLB entries.
        [0] global,
        [1] valid,
        [2] dirty,
        [3..=5] attributes: u8,
        [6..=25] page_frame_number: u32,
    }
}

#[derive(Default, Debug)]
struct Entry {
    page_mask: PageMask,
    hi: EntryHi,
    lo: [EntryLo; 2],
}

impl Entry {
    fn entry_lo(&self, vaddr: VirtualAddress) -> EntryLo {
        // EntryLo0 is used for even virtual pages and EntryLo1 for odd virtual pages.
        let odd = (vaddr.virtual_page_number(self.page_mask) % 2) as usize;
        unsafe { *self.lo.get(odd).unwrap_unchecked() } // SAFETY: The index cannot be out of bounds.
    }

    /// Generates a physical address from the given virtual address.
    pub fn physical_address(
        &self,
        vaddr: VirtualAddress,
        mode: AccessMode,
    ) -> Result<u32, TranslationError> {
        let lo = self.entry_lo(vaddr);
        if !lo.valid() {
            return Err(TranslationError::EntryInvalid);
        } else if mode == AccessMode::Write && !lo.dirty() {
            return Err(TranslationError::Modification);
        }

        let pfn = lo.page_frame_number() << self.page_mask.mask().count_ones();
        Ok(pfn | vaddr.offset(self.page_mask))
    }

    pub fn matches(&self, vaddr: VirtualAddress, regs: &Registers) -> bool {
        if vaddr.virtual_page_number(self.page_mask) != self.hi.virtual_page_number(vaddr) {
            return false;
        }

        // The ASID should only be checked if the global bit is not set on both EntryLo registers of CP0, or the EntryHi of the TLB.
        if !self.hi.global()
            && [regs[register::Cp0::EntryLo0], regs[register::Cp0::EntryLo1]]
                .iter()
                .any(|lo| !EntryLo(*lo).global())
        {
            let cp0_hi = EntryHi(regs[register::Cp0::EntryHi]);
            if cp0_hi.address_space_id() != self.hi.address_space_id() {
                return false;
            }
        }

        true
    }
}

#[derive(Default, Debug)]
pub struct TranslationLookasideBuffer {
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
        registers: &Registers,
    ) -> Result<u32, TranslationError> {
        if let Some(paddr) = Self::translate_unmapped(vaddr) {
            return Ok(paddr);
        }

        let vaddr = VirtualAddress32(vaddr).into();
        self.entries
            .iter()
            .find(|pte| pte.matches(vaddr, registers))
            .ok_or(TranslationError::Miss)
            .and_then(|pte| pte.physical_address(vaddr, mode))
    }

    pub fn set_entry(&mut self, index: usize, registers: &Registers) -> Option<()> {
        self.entries.get_mut(index).map(|entry| {
            *entry = Entry {
                page_mask: PageMask(registers[register::Cp0::PageMask] as u32),
                hi: EntryHi(registers[register::Cp0::EntryHi]),
                lo: [
                    EntryLo(registers[register::Cp0::EntryLo0]),
                    EntryLo(registers[register::Cp0::EntryLo1]),
                ],
            }
        })
    }
}

impl fmt::Debug for VirtualAddress {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("VirtualAddress")
            .field(
                "virtual_page_number_and_offset",
                &self.virtual_page_number_and_offset(),
            )
            .field("mode", &self.mode())
            .finish()
    }
}

impl fmt::Debug for PageMask {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("PageMask")
            .field("mask", &self.mask())
            .finish()
    }
}

impl fmt::Debug for EntryHi {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("EntryHi")
            .field("address_space_id", &self.address_space_id())
            .field("global", &self.global())
            .field("virtual_page_number_32", &self.virtual_page_number_32())
            .field("virtual_page_number_64", &self.virtual_page_number_64())
            .field("mode_64", &self.mode())
            .finish()
    }
}
