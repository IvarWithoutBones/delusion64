//! Register definitions for the N64's CPU and RSP.

#![warn(clippy::all, clippy::pedantic)]

use strum::{EnumCount, EnumIter, EnumVariantNames, FromRepr};

// Note that these macros must be defined before the modules that use them:
// https://doc.rust-lang.org/reference/macros-by-example.html#textual-scope
macro_rules! impl_from_and_try_from {
    ($ty:ident, $($num:ident),+) => {
        $(
            impl TryFrom<$num> for $ty {
                type Error = String;

                fn try_from(v: $num) -> Result<Self, Self::Error> {
                    Self::from_repr(v as _).ok_or_else(|| format!("invalid register index: {v}"))
                }
            }

            impl From<$ty> for $num {
                fn from(v: $ty) -> Self {
                    v.to_repr() as _
                }
            }
        )+
    };
}

macro_rules! impl_reg {
    ($ty:ident) => {
        impl $ty {
            /// The index this register has in the register file.
            #[must_use]
            pub const fn to_repr(self) -> usize {
                self as usize
            }

            /// The name of this register.
            #[must_use]
            pub const fn name(self) -> &'static str {
                Self::name_from_index(self.to_repr())
            }

            /// Returns the name of the register at the given index, or panics if the index is out of bounds.
            #[must_use]
            pub const fn name_from_index(index: usize) -> &'static str {
                <Self as strum::VariantNames>::VARIANTS[index]
            }

            /// The total number of registers.
            #[must_use]
            pub const fn count() -> usize {
                <Self as strum::EnumCount>::COUNT
            }

            /// Returns an iterator over all registers.
            pub fn iter() -> impl Iterator<Item = Self> {
                <Self as strum::IntoEnumIterator>::iter()
            }
        }

        impl ::std::fmt::Display for $ty {
            fn fmt(&self, f: &mut ::std::fmt::Formatter<'_>) -> ::std::fmt::Result {
                f.write_str(self.name())
            }
        }

        impl_from_and_try_from!(
            $ty, u8, i8, u16, i16, u32, i32, u64, i64, u128, i128, usize, isize
        );
    };
}

pub mod cpu;
pub mod rsp;

/// A general purpose MIPS 3 register, also known as an `gpr`. This definition applies to both the RSP and CPU.
#[derive(EnumCount, EnumIter, EnumVariantNames, FromRepr, Debug, PartialEq, Eq, Clone, Copy)]
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
