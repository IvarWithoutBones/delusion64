use strum::{EnumCount, EnumVariantNames, FromRepr, VariantNames};

// Generates helper functions/impls that are common to all registers.
macro_rules! impl_reg {
    ($enum:ident) => {
        impl $enum {
            /// Returns the number of registers.
            pub const fn count() -> usize {
                Self::COUNT
            }

            /// Returns the register number.
            pub const fn to_repr(self) -> u8 {
                self as u8
            }

            /// Returns the name of the register at the given index.
            pub const fn name_from_index(index: u8) -> &'static str {
                Self::VARIANTS[index as usize]
            }

            /// Returns the name of the register.
            pub const fn name(&self) -> &'static str {
                Self::name_from_index(self.to_repr())
            }
        }

        impl ::std::fmt::Display for $enum {
            fn fmt(&self, f: &mut ::std::fmt::Formatter<'_>) -> ::std::fmt::Result {
                write!(f, "{}", self.name())
            }
        }

        impl From<$enum> for u8 {
            fn from(v: $enum) -> Self {
                v.to_repr()
            }
        }

        impl From<$enum> for u16 {
            fn from(v: $enum) -> Self {
                v.to_repr().into()
            }
        }

        impl From<$enum> for u32 {
            fn from(v: $enum) -> Self {
                v.to_repr().into()
            }
        }

        impl From<$enum> for u64 {
            fn from(v: $enum) -> Self {
                v.to_repr().into()
            }
        }
    };
}

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

#[derive(EnumCount, FromRepr, EnumVariantNames, Debug, PartialEq, Eq, Clone, Copy)]
#[strum(serialize_all = "snake_case")]
#[repr(u8)]
pub enum Special {
    Hi,
    Lo,
    LoadLink,
    Pc,
}

impl_reg!(Special);

#[derive(EnumCount, FromRepr, EnumVariantNames, Debug, PartialEq, Eq, Clone, Copy)]
pub enum Coprocessor {
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
    /// for future use
    Reserved,
}

impl_reg!(Coprocessor);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Register {
    GeneralPurpose(GeneralPurpose),
    Special(Special),
    Coprocessor(Coprocessor),
}

impl Register {
    pub const fn name(self) -> &'static str {
        match self {
            Self::GeneralPurpose(v) => v.name(),
            Self::Special(v) => v.name(),
            Self::Coprocessor(v) => v.name(),
        }
    }

    pub const fn to_repr(self) -> u8 {
        match self {
            Self::GeneralPurpose(v) => v.to_repr(),
            Self::Special(v) => v.to_repr(),
            Self::Coprocessor(v) => v.to_repr(),
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

impl From<Coprocessor> for Register {
    fn from(v: Coprocessor) -> Self {
        Self::Coprocessor(v)
    }
}
