use std::{fmt, ops::Index};

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

    pub const fn to_u8(self) -> u8 {
        match self {
            Self::GeneralPurpose(v) => v.to_u8(),
            Self::Special(v) => v.to_u8(),
            Self::Coprocessor(v) => v.to_u8(),
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

// Based off of https://stackoverflow.com/a/57578431, thanks!
macro_rules! enum_from_u8 {
    ($(#[$meta:meta])* $vis:vis enum $name:ident {
        $($(#[$vmeta:meta])* $vname:ident $(= $val:expr)?,)*
    }) => {
        $(#[$meta])*
        $vis enum $name {
            $($(#[$vmeta])* $vname $(= $val)?,)*
        }

        impl $name {
            pub const fn to_u8(&self) -> u8 {
                *self as u8
            }

            pub const fn from_u8(v: u8) -> Option<Self> {
                match v {
                    $(x if x == $name::$vname as u8 => Some($name::$vname),)*
                    _ => None,
                }
            }

            pub const fn count() -> usize {
                // Count the number of variants
                0 $( + { let _ = $name::$vname; 1 } )+
            }
        }

        impl std::convert::TryFrom<u8> for $name {
            type Error = ();

            fn try_from(v: u8) -> Result<Self, Self::Error> {
                Self::from_u8(v).ok_or(())
            }
        }

        impl std::convert::From<$name> for u64 {
            fn from(v: $name) -> Self {
                v.to_u8().into()
            }
        }
    }
}

enum_from_u8!(
    #[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
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
);

impl GeneralPurpose {
    // TODO: it would be nice to generate this automatically
    pub const fn name_from_index(index: u8) -> &'static str {
        match index {
            0 => "zero",
            1 => "at",
            2 => "v0",
            3 => "v1",
            4 => "a0",
            5 => "a1",
            6 => "a2",
            7 => "a3",
            8 => "t0",
            9 => "t1",
            10 => "t2",
            11 => "t3",
            12 => "t4",
            13 => "t5",
            14 => "t6",
            15 => "t7",
            16 => "s0",
            17 => "s1",
            18 => "s2",
            19 => "s3",
            20 => "s4",
            21 => "s5",
            22 => "s6",
            23 => "s7",
            24 => "t8",
            25 => "t9",
            26 => "k0",
            27 => "k1",
            28 => "gp",
            29 => "sp",
            30 => "s8",
            31 => "ra",
            _ => unreachable!(),
        }
    }

    pub const fn name(&self) -> &'static str {
        Self::name_from_index(self.to_u8())
    }
}

impl fmt::Display for GeneralPurpose {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.name())
    }
}

#[derive(Debug, Default)]
#[repr(transparent)]
pub struct GeneralPurposeRegisters(pub [u64; GeneralPurpose::count()]);

impl Index<GeneralPurpose> for GeneralPurposeRegisters {
    type Output = u64;

    fn index(&self, index: GeneralPurpose) -> &Self::Output {
        &self.0[index as usize]
    }
}

enum_from_u8!(
    #[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
    #[repr(u8)]
    pub enum Special {
        Hi,
        Lo,
        LoadLink,
    }
);

impl Special {
    pub const fn name_from_index(index: u8) -> &'static str {
        match index {
            0 => "hi",
            1 => "lo",
            2 => "load_link",
            _ => unreachable!(),
        }
    }

    pub const fn name(&self) -> &'static str {
        Self::name_from_index(self.to_u8())
    }
}

impl fmt::Display for Special {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.name())
    }
}

#[derive(Debug, Default)]
#[repr(transparent)]
pub struct SpecialRegisters(pub [u64; Special::count()]);

impl Index<Special> for SpecialRegisters {
    type Output = u64;

    fn index(&self, index: Special) -> &Self::Output {
        &self.0[index as usize]
    }
}

enum_from_u8!(
    #[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
    #[repr(u8)]
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
        ExceptionProgramCounter,
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
        Reserved21,
        /// Reserved for future use
        Reserved22,
        /// Reserved for future use
        Reserved23,
        /// Reserved for future use
        Reserved24,
        /// Reserved for future use
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
        ErrorExceptionProgramCounter,
        /// for future use
        Reserved,
    }
);

impl Coprocessor {
    // Effectively just the Debug implementation, but const
    pub const fn name_from_index(index: u8) -> &'static str {
        match index {
            0 => "Index",
            1 => "Random",
            2 => "EntryLo0",
            3 => "EntryLo1",
            4 => "Context",
            5 => "PageMask",
            6 => "Wired",
            7 => "Reserved",
            8 => "BadVAddr",
            9 => "Count",
            10 => "EntryHi",
            11 => "Compare",
            12 => "Status",
            13 => "Cause",
            14 => "EPC",
            15 => "PRId",
            16 => "Config",
            17 => "LLAddr",
            18 => "WatchLo",
            19 => "WatchHi",
            20 => "XContext",
            21..=25 => "Reserved",
            26 => "Parity",
            27 => "Cache",
            28 => "TagLo",
            29 => "TagHi",
            30 => "ErrorEPC",
            31 => "Reserved",
            _ => unreachable!(),
        }
    }

    pub const fn name(&self) -> &'static str {
        Self::name_from_index(self.to_u8())
    }
}

impl fmt::Display for Coprocessor {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.name())
    }
}

#[derive(Debug, Default)]
#[repr(transparent)]
pub struct CoprocessorRegisters(pub [u64; Coprocessor::count()]);

impl Index<Coprocessor> for CoprocessorRegisters {
    type Output = u64;

    fn index(&self, index: Coprocessor) -> &Self::Output {
        &self.0[index as usize]
    }
}

#[cfg(test)]
mod test {
    use super::*;

    macro_rules! assert_from_num {
        ($num:expr, $enum:ident :: $expected:ident) => {
            let gpr = $enum::try_from($num).unwrap();
            assert_eq!(gpr, $enum::$expected);
        };
    }

    #[test]
    fn gpr_count() {
        assert_eq!(GeneralPurpose::count(), 32);
    }

    #[test]
    fn gpr_all_valid() {
        for i in 0..GeneralPurpose::count() {
            assert!(GeneralPurpose::from_u8(i as u8).is_some());
        }
    }

    #[test]
    fn gpr_from_u8() {
        assert_from_num!(0, GeneralPurpose::Zero);
        assert_from_num!(1, GeneralPurpose::At);
        assert_from_num!(2, GeneralPurpose::V0);
        assert_from_num!(3, GeneralPurpose::V1);
        assert_from_num!(4, GeneralPurpose::A0);
        assert_from_num!(29, GeneralPurpose::Sp);
        assert_from_num!(30, GeneralPurpose::S8);
        assert_from_num!(31, GeneralPurpose::Ra);
    }

    #[test]
    fn cp0_count() {
        assert_eq!(Coprocessor::count(), 32);
    }

    #[test]
    fn cp0_all_valid() {
        for i in 0..Coprocessor::count() {
            assert!(Coprocessor::from_u8(i as u8).is_some());
        }
    }

    #[test]
    fn cp0_from_u8() {
        assert_from_num!(0, Coprocessor::Index);
        assert_from_num!(1, Coprocessor::Random);
        assert_from_num!(2, Coprocessor::EntryLo0);
        assert_from_num!(3, Coprocessor::EntryLo1);
        assert_from_num!(4, Coprocessor::Context);
        assert_from_num!(5, Coprocessor::PageMask);
        assert_from_num!(6, Coprocessor::Wired);
    }
}
