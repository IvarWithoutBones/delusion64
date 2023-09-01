//! Contains the definitions of bitfields for the CP0 registers.

use std::fmt;
use strum::FromRepr;
use tartan_bitfield::{bitfield, bitfield_without_debug};

#[derive(FromRepr, Debug, Clone, Copy, PartialEq, Eq, Ord, PartialOrd, Hash)]
#[repr(u8)]
pub enum OperatingMode {
    Kernel = 0,
    Supervisor = 0b01,
    User = 0b10,
    Undefined = 0b11,
}

impl From<OperatingMode> for u8 {
    fn from(mode: OperatingMode) -> Self {
        mode as u8
    }
}

impl From<u8> for OperatingMode {
    fn from(value: u8) -> Self {
        Self::from_repr(value).unwrap()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Bits {
    Bits32,
    Bits64,
}

bitfield! {
    /// The format of the CP0 register Index.
    #[derive(Ord, PartialOrd, Hash)]
    pub struct Index(u32) {
        /// Index of the TLB entry affected by the TLBR and TLBWI instructions.
        [0..=6] pub index: u8,
        /// Set to 1 when the previous TLBP instruction was unsuccessful, and set to 0 when successful
        [31] pub probe_successfull,
    }
}

impl Index {
    pub const fn new(raw: u32) -> Self {
        Self(raw)
    }
}

impl From<Index> for u64 {
    fn from(value: Index) -> Self {
        value.0 as u64
    }
}

bitfield! {
    /// The format of the CP0 register and TLB entry, EntryHi.
    #[derive(Ord, PartialOrd, Hash)]
    pub struct EntryHi(u64) {
        /// The ASID of the process that owns the page table entry.
        [0..=7] pub address_space_id: u8,
        /// Controls whether an ASID match should occur. Only defined for TLB entries, not the CP0 register.
        [12] pub global,
        /// The virtual page number divided by two (maps to two pages), in 32-bit mode.
        [13..=31] pub virtual_page_number_32: u32,
        /// The virtual page number divided by two (maps to two pages), in 64-bit mode.
        [13..=39] pub virtual_page_number_64: u32,
        /// The owner of the page table entry. Only defined in 64-bit mode.
        [62..=63] pub mode: u8 as OperatingMode,
    }
}

impl EntryHi {
    pub const fn new(raw: u64) -> Self {
        Self(raw)
    }

    pub fn virtual_page_number(&self, bits: Bits) -> u32 {
        match bits {
            Bits::Bits32 => self.virtual_page_number_32(),
            Bits::Bits64 => self.virtual_page_number_64(),
        }
    }
}

bitfield! {
    /// The format of the CP0 register and TLB entry, EntryLo.
    #[derive(Ord, PartialOrd, Hash)]
    pub struct EntryLo(u64) {
        /// Controls whether an ASID match should occur. Only defined for the CP0 registers, not TLB entries.
        [0] pub global,
        /// If unset, a TLB miss occurs when the page is accessed.
        [1] pub valid,
        /// A write-protect bit which if unset, causes an modification exception when the page is written to.
        [2] pub dirty,
        /// The page attributes, which specify the cache coherency algorithm to use.
        [3..=5] pub attributes: u8,
        /// The high-order bits of the physical address.
        [6..=25] pub page_frame_number: u32,
    }
}

impl EntryLo {
    pub const fn new(raw: u64) -> Self {
        Self(raw)
    }
}

bitfield_without_debug! {
    /// The format of the CP0 register and TLB entry, PageMask.
    #[derive(Ord, PartialOrd, Hash)]
    pub struct PageMask(u32) {
        /// Page comparison mask, determines the virtual page size of an TLB entry.
        /// This is the raw value, it does not include the initial bits which make it start at 4KiB.
        [13..=24] pub raw_mask: u16,
    }
}

impl PageMask {
    pub const fn new(raw: u32) -> Self {
        Self(raw)
    }

    /// Page comparison mask, determines the virtual page size of the corresponding entry.
    /// Includes the initial bits which make it start at 4KiB.
    pub fn mask(&self) -> u64 {
        const START: u64 = 0b1111_1111_1111;
        ((self.raw_mask() as u64) << START.count_ones()) | START
    }
}

impl fmt::Debug for PageMask {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("PageMask")
            .field("raw_mask", &self.raw_mask())
            .field("mask", &self.mask())
            .finish()
    }
}

bitfield! {
    /// The format of the `DP` field of the CP0 register Status.
    #[derive(Ord, PartialOrd, Hash)]
    pub struct DiagnosticStatus(u16) {
        /// The condition bit (`CH`).
        [2] cp0_condition,
        /// Indicates if a soft reset has occurred (`SR`).
        [4] soft_reset,
        /// Indicates TLB shutdown has occurred (`TS`).
        [5] tlb_shutdown,
        /// Controls the location of TLB miss and general purpose exception vectors (`BEV`).
        [6] bootstrap_exception_vectors,
        /// Enables Instruction Trace Support (`ITS`).
        [8] instruction_trace_support,
    }
}

bitfield! {
    /// The format of the CP0 register Status, also known as `SR`.
    #[derive(Ord, PartialOrd, Hash)]
    pub struct Status(u32) {
        /// Enables or disables interrupts (`IE`).
        [0] pub interrupts_enabled,
        /// Triggers more exceptions (`EXL`).
        [1] pub exception_level,
        /// Triggers more errors (`ERL`).
        [2] pub error_level,
        /// The current operating mode (`KSU`).
        [3..=4] pub mode: u8 as OperatingMode,
        /// Enables 64-bit addressing and operations in User mode (`UX`).
        [5] pub user_64_bits,
        /// Enables 64-bit addressing and operations in Supervisor mode (`SX`).
        [6] pub supervisor_64_bits,
        /// Enables 64-bit addressing and operations in Kernel mode (`KX`).
        [7] pub kernel_64_bits,
        /// Mask bits for software interrupts and `IP` of the Cause register (apart of `IM`).
        [8..=9] pub software_interrupt_mask: u8,
        /// Mask bits for external interrupts, or external write requests (apart of `IM`).
        [10..=14] pub external_interrupt_mask: u8,
        /// Mask bit for timer interrupt (apart of `IM`).
        [15] pub timer_interrupt_mask,
        /// The diagnostic status (`DS`).
        [16..=24] pub diagnostic_status: u16 as DiagnosticStatus,
        /// Enables reverse of system endianness in User mode (`RE`).
        [25] pub reverse_endian,
        /// Enables additional floating-point registers (`FR`).
        [26] pub floating_point_registers,
        /// Enables low-power operation by reducing the internal clock frequency to one-quarter speed (`RP`).
        [27] pub low_power_mode,
        /// Enables coprocessor 0 (apart of `CU`).
        [28] pub coprocesor_0_enabled,
        /// Enables coprocessor 1 (apart of `CU`).
        [29] pub coprocesor_1_enabled,
        /// Enables coprocessor 2 (apart of `CU`).
        [30] pub coprocesor_2_enabled,
        /// Enables coprocessor 3 (apart of `CU`).
        [31] pub coprocesor_3_enabled,
    }
}

impl Status {
    pub const fn new(raw: u32) -> Self {
        Self(raw)
    }

    /// Checks whether 64-bit addressing and operations are enabled in the current operating mode.
    pub fn is_64_bits(&self) -> bool {
        match self.mode() {
            OperatingMode::Kernel => self.kernel_64_bits(),
            OperatingMode::Supervisor => self.supervisor_64_bits(),
            OperatingMode::User => self.user_64_bits(),
            OperatingMode::Undefined => unreachable!(),
        }
    }
}
