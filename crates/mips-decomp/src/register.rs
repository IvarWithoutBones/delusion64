use strum::{EnumCount, EnumVariantNames, FromRepr, VariantNames};

// Generates helper functions/impls that are common to all registers.
macro_rules! impl_reg {
    ($enum:ident) => {
        impl $enum {
            /// Returns the register at the given index, or panics if the index is out of bounds.
            /// If you want to handle out of bounds indices, use `from_repr` instead.
            pub fn new(index: usize) -> Self {
                Self::from_repr(index as _).unwrap()
            }

            /// Returns the index of the register.
            pub const fn to_repr(self) -> usize {
                self as usize
            }

            /// Returns the name of the register as a static string slice.
            pub const fn name(&self) -> &'static str {
                Self::name_from_index(self.to_repr())
            }

            /// Returns the name of the register at the given index, or panics if the index is out of bounds.
            pub const fn name_from_index(index: usize) -> &'static str {
                Self::VARIANTS[index]
            }

            /// Returns the total number of registers.
            pub const fn count() -> usize {
                Self::COUNT
            }
        }

        impl ::std::fmt::Display for $enum {
            fn fmt(&self, f: &mut ::std::fmt::Formatter<'_>) -> ::std::fmt::Result {
                write!(f, "{}", self.name())
            }
        }

        impl TryFrom<u8> for $enum {
            type Error = ();

            fn try_from(v: u8) -> Result<Self, Self::Error> {
                Self::from_repr(v as _).ok_or(())
            }
        }

        impl TryFrom<u16> for $enum {
            type Error = ();

            fn try_from(v: u16) -> Result<Self, Self::Error> {
                Self::from_repr(v as _).ok_or(())
            }
        }

        impl TryFrom<u32> for $enum {
            type Error = ();

            fn try_from(v: u32) -> Result<Self, Self::Error> {
                Self::from_repr(v as _).ok_or(())
            }
        }

        impl TryFrom<u64> for $enum {
            type Error = ();

            fn try_from(v: u64) -> Result<Self, Self::Error> {
                Self::from_repr(v as _).ok_or(())
            }
        }

        impl TryFrom<u128> for $enum {
            type Error = ();

            fn try_from(v: u128) -> Result<Self, Self::Error> {
                Self::from_repr(v as _).ok_or(())
            }
        }

        impl TryFrom<usize> for $enum {
            type Error = ();

            fn try_from(v: usize) -> Result<Self, Self::Error> {
                Self::from_repr(v as _).ok_or(())
            }
        }

        impl From<$enum> for u8 {
            fn from(v: $enum) -> Self {
                v.to_repr() as _
            }
        }

        impl From<$enum> for u16 {
            fn from(v: $enum) -> Self {
                v.to_repr() as _
            }
        }

        impl From<$enum> for u32 {
            fn from(v: $enum) -> Self {
                v.to_repr() as _
            }
        }

        impl From<$enum> for u64 {
            fn from(v: $enum) -> Self {
                v.to_repr() as _
            }
        }

        impl From<$enum> for u128 {
            fn from(v: $enum) -> Self {
                v.to_repr() as _
            }
        }

        impl From<$enum> for usize {
            fn from(v: $enum) -> Self {
                v.to_repr()
            }
        }
    };
}

/// A general purpose MIPS 3 register (`gpr`).
#[derive(EnumCount, FromRepr, EnumVariantNames, Debug, PartialEq, Eq, Clone, Copy)]
#[strum(serialize_all = "snake_case")]
#[repr(u8)]
pub enum GeneralPurpose {
    Zero,
    At,
    V0,
    V1,
    A0,
    A1,
    A2,
    A3,
    T0,
    T1,
    T2,
    T3,
    T4,
    T5,
    T6,
    T7,
    S0,
    S1,
    S2,
    S3,
    S4,
    S5,
    S6,
    S7,
    T8,
    T9,
    K0,
    K1,
    Gp,
    Sp,
    S8,
    Ra,
}

impl_reg!(GeneralPurpose);

/// A miscellaneous MIPS 3 register, which does not nicely fit into any other category.
#[derive(EnumCount, FromRepr, EnumVariantNames, Debug, PartialEq, Eq, Clone, Copy)]
#[strum(serialize_all = "snake_case")]
#[repr(u8)]
pub enum Special {
    /// The program counter, which is not directly accessible but here for bookkeeping purposes.
    Pc,
    /// For the `mult` and `multu` instructions.
    Hi,
    /// For the `mult` and `multu` instructions.
    Lo,
    /// For the `sc` family of instructions.
    LoadLink,
    /// FPU Control/Status Register `FCR31`.
    FpuControlStatus,
    /// FPU Implementation/Revision Register `FCR0`.
    FpuImplementationRevision,
}

impl_reg!(Special);

/// A MIPS 3 coprocessor 0 register.
#[derive(EnumCount, FromRepr, EnumVariantNames, Debug, PartialEq, Eq, Clone, Copy)]
pub enum Cp0 {
    /// Programmable pointer into TLB array
    Index,
    /// Pseudorandom pointer into TLB array (read only)
    Random,
    /// Low half of TLB entry for even virtual address (VPN)
    EntryLo0,
    /// Low half of TLB entry for odd virtual address (VPN)
    EntryLo1,
    /// Pointer to kernel virtual page table entry (PTE) in 32-bit mode
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
    /// High half of TLB entry (including ASID)
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
    /// Pointer to Kernel virtual PTE table in 64-bit mode
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
    Parity,
    /// Error Cache Error and Status register
    Cache,
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

impl Cp0 {
    /// Returns whether the register is reserved for future use, and thus should not be used.
    pub const fn is_reserved(&self) -> bool {
        matches!(self, |Self::Reserved7| Self::Reserved21
            | Self::Reserved22
            | Self::Reserved23
            | Self::Reserved24
            | Self::Reserved25
            | Self::Reserved31)
    }
}

impl_reg!(Cp0);

/// A MIPS 3 FPU (coprocessor 1, or CP1) register.
#[derive(EnumCount, FromRepr, EnumVariantNames, Debug, PartialEq, Eq, Clone, Copy)]
#[strum(serialize_all = "snake_case")]
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

/// A MIPS 3 register.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Register {
    GeneralPurpose(GeneralPurpose),
    Special(Special),
    Cp0(Cp0),
    Fpu(Fpu),
}

impl Register {
    pub const fn name(self) -> &'static str {
        match self {
            Self::GeneralPurpose(v) => v.name(),
            Self::Special(v) => v.name(),
            Self::Cp0(v) => v.name(),
            Self::Fpu(v) => v.name(),
        }
    }

    pub const fn to_repr(self) -> usize {
        match self {
            Self::GeneralPurpose(v) => v.to_repr(),
            Self::Special(v) => v.to_repr(),
            Self::Cp0(v) => v.to_repr(),
            Self::Fpu(v) => v.to_repr(),
        }
    }
}

impl From<GeneralPurpose> for Register {
    fn from(v: GeneralPurpose) -> Self {
        Self::GeneralPurpose(v)
    }
}

impl From<Special> for Register {
    fn from(v: Special) -> Self {
        Self::Special(v)
    }
}

impl From<Cp0> for Register {
    fn from(v: Cp0) -> Self {
        Self::Cp0(v)
    }
}

impl From<Fpu> for Register {
    fn from(v: Fpu) -> Self {
        Self::Fpu(v)
    }
}
