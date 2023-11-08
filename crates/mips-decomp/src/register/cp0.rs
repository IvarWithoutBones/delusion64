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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum Bits {
    Bits32,
    #[default]
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
    /// A mask that covers all the writable bits of the register.
    pub const WRITE_MASK: u64 = 0b1000_0000_0000_0000_0000_0000_0011_1111;

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
        [2] pub cp0_condition,
        /// Indicates if a soft reset has occurred (`SR`).
        [4] pub soft_reset,
        /// Indicates TLB shutdown has occurred (`TS`).
        [5] pub tlb_shutdown,
        /// Controls the location of TLB miss and general purpose exception vectors (`BEV`).
        [6] pub bootstrap_exception_vectors,
        /// Enables Instruction Trace Support (`ITS`).
        [8] pub instruction_trace_support,
    }
}

bitfield! {
    /// The `IP` (Interrupt Pending) field of the register Cause, and the `IM` (Interrupt Mask) field of the register Status.
    #[derive(Ord, PartialOrd, Hash)]
    pub struct InterruptStatus(u8) {
        [0] pub software_1,
        [1] pub software_2,
        [2] pub external_1,
        [3] pub external_2,
        [4] pub external_3,
        [5] pub external_4,
        [6] pub external_5,
        [7] pub timer,
    }
}

impl InterruptStatus {
    pub const TIMER_MASK: u64 = 1 << 7;

    pub const fn new(raw: u8) -> Self {
        Self(raw)
    }

    pub const fn raw(&self) -> u8 {
        self.0
    }

    pub const fn check_mask(&self, mask: Self) -> bool {
        (self.0 & mask.0) != 0
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
        // /// Mask bits for software, external and timers interrupts. Corresponds to `IP` of the Cause register (`IM`).
        [8..=15] pub interrupt_mask: u8 as InterruptStatus,
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
    /// A mask that covers all the writable bits of the register.
    pub const WRITE_MASK: u64 = 0b1111_1111_1111_0111_1111_1111_1111_1111;

    pub const FR_MASK: u64 = 1 << 26;

    pub const COPROCESSOR_0_ENABLED_MASK: u64 = 1 << 28;
    pub const COPROCESSOR_1_ENABLED_MASK: u64 = 1 << 29;
    pub const COPROCESSOR_2_ENABLED_MASK: u64 = 1 << 30;
    pub const COPROCESSOR_3_ENABLED_MASK: u64 = 1 << 31;

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

bitfield! {
    /// The format of the CP0 register Cause, also known as `CR`.
    #[derive(Ord, PartialOrd, Hash)]
    pub struct Cause(u32) {
        [2..=6] pub exception_code: u8,
        [8..=15] pub interrupt_pending: u8 as InterruptStatus,
        [28..=29] pub coprocessor_error: u8,
        [31] pub branch_delay,
    }
}

impl Cause {
    pub const INTERRUPT_PENDING_SHIFT: u32 = 8;
    pub const INTERRUPT_PENDING_TIMER_MASK: u64 =
        InterruptStatus::TIMER_MASK << Self::INTERRUPT_PENDING_SHIFT;

    pub const fn new(raw: u32) -> Self {
        Self(raw)
    }

    pub const fn raw(&self) -> u32 {
        self.0
    }
}

bitfield! {
    /// The format of the CP0 register Context, for 64-bit mode.
    #[derive(Ord, PartialOrd, Hash)]
    pub struct Context(u64) {
        /// Page number of virtual address whose translation is invalid divided by 2
        [4..=22] pub bad_virtual_page_number: u32,
        /// Base address of page table entry.
        [23..=63] pub page_table_entry_base_address: u64,
    }
}

impl Context {
    /// A mask that covers BadVPN2 and prior reserved bits of the register. This should not be writable by software.
    pub const READ_ONLY_MASK: u64 = 0b111_1111_1111_1111_1111_1111;

    /// A mask that covers all the writable bits of the register.
    pub const PAGE_TABLE_ENTRY_BASE_MASK: u64 = !Self::READ_ONLY_MASK;

    pub const fn new(raw: u64) -> Self {
        Self(raw)
    }
}

bitfield! {
    /// The format of the CP0 register XContext, for 64-bit mode.
    #[derive(Ord, PartialOrd, Hash)]
    pub struct XContext(u64) {
        /// Page number of virtual address whose translation is invalid divided by 2
        [4..=30] pub bad_virtual_page_number: u32,
        /// The ASID of the process that owns the page table entry.
        [31..=32] pub address_space_id: u8,
        /// Base address of page table entry.
        [33..=63] pub page_table_entry_base_address: u32,
    }
}

impl XContext {
    /// A mask that covers BadVPN2 and prior reserved bits of the register. This should not be writable by software.
    pub const READ_ONLY_MASK: u64 = 0b1_1111_1111_1111_1111_1111_1111_1111_1111;

    /// A mask that covers all the writable bits of the register.
    pub const PAGE_TABLE_ENTRY_BASE_MASK: u64 = !Self::READ_ONLY_MASK;

    pub const fn new(raw: u64) -> Self {
        Self(raw)
    }
}

bitfield! {
    /// The format of the CP0 register Wired.
    #[derive(Ord, PartialOrd, Hash)]
    pub struct Wired(u32) {
        /// TLB Wired boundary
        [0..=5] pub wired: u8,
    }
}

impl Wired {
    /// A mask that covers all the writable bits of the register.
    pub const WRITE_MASK: u64 = 0b11_1111;

    pub const fn new(raw: u32) -> Self {
        Self(raw)
    }
}

bitfield! {
    /// The format of the CP0 register LLAddr.
    #[derive(Ord, PartialOrd, Hash)]
    pub struct LLAddr(u32) {
        /// The physical address read by the most recent Load Linked instruction.
        [0..=31] pub physical_address: u32,
    }
}

impl LLAddr {
    /// A mask that covers all the writable bits of the register.
    pub const WRITE_MASK: u64 = u32::MAX as u64;

    pub const fn new(raw: u32) -> Self {
        Self(raw)
    }
}

bitfield! {
    /// The format of the CP0 register Config.
    #[derive(Ord, PartialOrd, Hash)]
    pub struct Config(u32) {
        /// Describes the kseg0 cache coherency algorithm.
        [0..=2] pub kseg0_cache_coherency: u8,
        /// Reserved for future use, but can be read and written by software.
        [3] pub reserved,
        /// The endianness of the system. When this is set, big-endian, when unset, little-endian.
        [15] pub big_endian,
        /// When zero, the "D" data rate is used. When 6, "DxxDxx" is used instead. All other values are reserved for future use.
        [24..=27] pub data_transfer_pattern: u8,
        /// The value displayed corresponds to the frequency ratio set by the DivMode pins on power application.
        [28..=30] pub frequency_ratio: u8,
    }
}

impl Config {
    /// A mask that covers all the writable bits of the register.
    pub const WRITE_MASK: u64 = 0b1111_0000_0000_1000_0000_0000_1111;

    pub const fn new(raw: u32) -> Self {
        Self(raw)
    }
}

bitfield! {
    /// The format of the CP0 register PRId, the Processor Revision Identifier.
    #[derive(Ord, PartialOrd, Hash)]
    pub struct PRId(u32) {
        /// The processors revision number
        [0..=7] pub revision: u8,
        /// The processors identifier number.
        [8..=15] pub identifier: u8,
    }
}

impl PRId {
    pub const fn new(raw: u32) -> Self {
        Self(raw)
    }
}

bitfield! {
    /// The format of the CP0 register PErr, which is unused by the VR4300.
    #[derive(Ord, PartialOrd, Hash)]
    pub struct PErr(u32) {
        /// The self-diagnostic area.
        [0..=7] pub diagnostic: u8,
    }
}

impl PErr {
    pub const WRITE_MASK: u64 = 0b1111_1111;

    pub const fn new(raw: u32) -> Self {
        Self(raw)
    }
}

bitfield! {
    /// The format of the CP0 register Compare.
    #[derive(Ord, PartialOrd, Hash)]
    pub struct Compare(u32) {
        /// The value Count would need to hit to trigger a timer interrupt.
        [0..=31] pub compare: u32,
    }
}

impl Compare {
    pub const WRITE_MASK: u64 = u32::MAX as u64;
}
