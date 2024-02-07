//! Register definitions for the N64's CPU.

use strum::{EnumCount, EnumIter, FromRepr, VariantNames};

pub mod cp0;
pub mod fpu;

pub use super::GeneralPurpose;

/// A MIPS 3 coprocessor 0 register.
#[derive(EnumCount, EnumIter, VariantNames, FromRepr, Debug, PartialEq, Eq, Clone, Copy)]
#[repr(u8)]
pub enum Cp0 {
    /// Programmable pointer into TLB array
    Index,
    /// Pseudorandom pointer into TLB array (read only)
    Random,
    /// Low half of TLB entry for even virtual addresses
    EntryLo0,
    /// Low half of TLB entry for odd virtual addresses
    EntryLo1,
    /// Pointer to kernel virtual page table entry in 32-bit mode
    Context,
    /// Page size specification
    PageMask,
    /// Number of wired TLB entries
    Wired,
    /// Reserved for future use
    #[strum(serialize = "Reserved")]
    Reserved7,
    /// Display of virtual address that occurred an error last
    BadVAddr,
    /// Timer Count
    Count,
    /// High half of TLB entry
    EntryHi,
    /// Timer Compare Value
    Compare,
    /// Operation status setting
    Status,
    /// Display of cause of last exception
    Cause,
    /// Exception Program Counter
    EPC,
    /// Processor Revision Identifier
    PRId,
    /// Memory system mode setting
    Config,
    /// Load Linked instruction address display
    LLAddr,
    /// Memory reference trap address low bits
    WatchLo,
    /// Memory reference trap address high bits
    WatchHi,
    /// Pointer to kernel virtual page table entry in 64-bit mode
    XContext,
    /// Reserved for future use
    #[strum(serialize = "Reserved")]
    Reserved21,
    /// Reserved for future use
    #[strum(serialize = "Reserved")]
    Reserved22,
    /// Reserved for future use
    #[strum(serialize = "Reserved")]
    Reserved23,
    /// Reserved for future use
    #[strum(serialize = "Reserved")]
    Reserved24,
    /// Reserved for future use
    #[strum(serialize = "Reserved")]
    Reserved25,
    /// Error Cache parity bits
    PErr,
    /// Error Cache Error and Status register
    CacheErr,
    /// Cache Tag register low
    TagLo,
    /// Cache Tag register high
    TagHi,
    /// Error Exception Program Counter
    ErrorEPC,
    /// Reserved for future use
    #[strum(serialize = "Reserved")]
    Reserved31,
}

impl_reg!(Cp0);

impl Cp0 {
    #[must_use]
    pub const fn is_reserved(self) -> bool {
        matches!(
            self,
            Self::Reserved7
                | Self::Reserved21
                | Self::Reserved22
                | Self::Reserved23
                | Self::Reserved24
                | Self::Reserved25
                | Self::Reserved31
        )
    }
}

/// A general purpose MIPS 3 FPU (coprocessor 1, or CP1) register.
#[derive(EnumCount, EnumIter, VariantNames, FromRepr, Debug, PartialEq, Eq, Clone, Copy)]
#[strum(serialize_all = "snake_case")]
#[repr(u8)]
pub enum Fpu {
    F0,
    F1,
    F2,
    F3,
    F4,
    F5,
    F6,
    F7,
    F8,
    F9,
    F10,
    F11,
    F12,
    F13,
    F14,
    F15,
    F16,
    F17,
    F18,
    F19,
    F20,
    F21,
    F22,
    F23,
    F24,
    F25,
    F26,
    F27,
    F28,
    F29,
    F30,
    F31,
}

impl_reg!(Fpu);

/// A MIPS 3 FPU (coprocessor 1, or CP1) control register, or `FCR`.
#[derive(EnumCount, EnumIter, VariantNames, FromRepr, Debug, PartialEq, Eq, Clone, Copy)]
#[repr(u8)]
pub enum FpuControl {
    /// FPU Implementation/Revision Register `FCR0`.
    ImplementationRevision,
    /// Reserved for future use
    #[strum(serialize = "Reserved")]
    Reserved1,
    /// Reserved for future use
    #[strum(serialize = "Reserved")]
    Reserved2,
    /// Reserved for future use
    #[strum(serialize = "Reserved")]
    Reserved3,
    /// Reserved for future use
    #[strum(serialize = "Reserved")]
    Reserved4,
    /// Reserved for future use
    #[strum(serialize = "Reserved")]
    Reserved5,
    /// Reserved for future use
    #[strum(serialize = "Reserved")]
    Reserved6,
    /// Reserved for future use
    #[strum(serialize = "Reserved")]
    Reserved7,
    /// Reserved for future use
    #[strum(serialize = "Reserved")]
    Reserved8,
    /// Reserved for future use
    #[strum(serialize = "Reserved")]
    Reserved9,
    /// Reserved for future use
    #[strum(serialize = "Reserved")]
    Reserved10,
    /// Reserved for future use
    #[strum(serialize = "Reserved")]
    Reserved11,
    /// Reserved for future use
    #[strum(serialize = "Reserved")]
    Reserved12,
    /// Reserved for future use
    #[strum(serialize = "Reserved")]
    Reserved13,
    /// Reserved for future use
    #[strum(serialize = "Reserved")]
    Reserved14,
    /// Reserved for future use
    #[strum(serialize = "Reserved")]
    Reserved15,
    /// Reserved for future use
    #[strum(serialize = "Reserved")]
    Reserved16,
    /// Reserved for future use
    #[strum(serialize = "Reserved")]
    Reserved17,
    /// Reserved for future use
    #[strum(serialize = "Reserved")]
    Reserved18,
    /// Reserved for future use
    #[strum(serialize = "Reserved")]
    Reserved19,
    /// Reserved for future use
    #[strum(serialize = "Reserved")]
    Reserved20,
    /// Reserved for future use
    #[strum(serialize = "Reserved")]
    Reserved21,
    /// Reserved for future use
    #[strum(serialize = "Reserved")]
    Reserved22,
    /// Reserved for future use
    #[strum(serialize = "Reserved")]
    Reserved23,
    /// Reserved for future use
    #[strum(serialize = "Reserved")]
    Reserved24,
    /// Reserved for future use
    #[strum(serialize = "Reserved")]
    Reserved25,
    /// Reserved for future use
    #[strum(serialize = "Reserved")]
    Reserved26,
    /// Reserved for future use
    #[strum(serialize = "Reserved")]
    Reserved27,
    /// Reserved for future use
    #[strum(serialize = "Reserved")]
    Reserved28,
    /// Reserved for future use
    #[strum(serialize = "Reserved")]
    Reserved29,
    /// Reserved for future use
    #[strum(serialize = "Reserved")]
    Reserved30,
    /// FPU Control/Status Register `FCR31`.
    ControlStatus,
}

impl_reg!(FpuControl);

impl FpuControl {
    #[must_use]
    pub const fn is_reserved(&self) -> bool {
        !matches!(self, Self::ImplementationRevision | Self::ControlStatus)
    }
}

/// A miscellaneous MIPS 3 register, which does not nicely fit into any other category.
#[derive(EnumCount, EnumIter, VariantNames, FromRepr, Debug, PartialEq, Eq, Clone, Copy)]
#[strum(serialize_all = "snake_case")]
#[repr(u8)]
pub enum Special {
    /// The program counter, which is not directly accessible but here for bookkeeping purposes.
    Pc,
    /// Used by the `mult` and `multu` instructions.
    Hi,
    /// Used by the `mult` and `multu` instructions.
    Lo,
    /// A bit used by the `sc` family of instructions.
    LoadLink,
}

impl_reg!(Special);

/// A MIPS 3 register.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Register {
    GeneralPurpose(GeneralPurpose),
    Special(Special),
    Cp0(Cp0),
    Fpu(Fpu),
    FpuControl(FpuControl),
}

impl Register {
    /// The name of this register.
    #[must_use]
    pub const fn name(self) -> &'static str {
        match self {
            Self::GeneralPurpose(r) => r.name(),
            Self::Special(r) => r.name(),
            Self::Cp0(r) => r.name(),
            Self::Fpu(r) => r.name(),
            Self::FpuControl(r) => r.name(),
        }
    }

    /// The index this register has in its register file.
    #[must_use]
    pub const fn to_repr(self) -> usize {
        match self {
            Self::GeneralPurpose(r) => r.to_repr(),
            Self::Special(r) => r.to_repr(),
            Self::Cp0(r) => r.to_repr(),
            Self::Fpu(r) => r.to_repr(),
            Self::FpuControl(r) => r.to_repr(),
        }
    }
}

impl From<GeneralPurpose> for Register {
    fn from(r: GeneralPurpose) -> Self {
        Self::GeneralPurpose(r)
    }
}

impl From<Special> for Register {
    fn from(r: Special) -> Self {
        Self::Special(r)
    }
}

impl From<Cp0> for Register {
    fn from(r: Cp0) -> Self {
        Self::Cp0(r)
    }
}

impl From<Fpu> for Register {
    fn from(r: Fpu) -> Self {
        Self::Fpu(r)
    }
}

impl From<FpuControl> for Register {
    fn from(r: FpuControl) -> Self {
        Self::FpuControl(r)
    }
}
