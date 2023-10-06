//! Contains the definitions of bitfields for the FPU control registers.

use std::fmt;
use strum::FromRepr;
use tartan_bitfield::bitfield;

bitfield! {
    /// The FPU Implementation/Revision register, or FCR0.
    #[derive(Ord, PartialOrd, Hash)]
    pub struct ImplementationRevision(u32) {
        /// The minor revision number
        [0..=3] minor: u8,
        /// The major revision number
        [4..=7] major: u8,
        /// The implementation number
        [8..=15] implementation: u8,
    }
}

impl fmt::Display for ImplementationRevision {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{:#04x}: v{}.{}",
            self.implementation(),
            self.major(),
            self.minor()
        )
    }
}

/// The rounding mode that the FPU uses for all floating-point operations.
#[derive(FromRepr, Debug, Clone, Copy, PartialEq, Eq, Ord, PartialOrd, Hash)]
#[repr(u8)]
pub enum RoundingMode {
    /// Round result to nearest representable value: round to value with least-significant bit 0 when
    /// the two nearest representable values are equally near.
    Nearest,
    /// Round toward 0: round to value closest to and not greater in magnitude than the infinitely precise result.
    Zero,
    /// Round toward + ¥: round to value closest to and not less than the infinitely precise result.
    Plus,
    // Round toward – ¥: round to value closest to and not greater than the infinitely precise result.
    Minus,
}

impl fmt::Display for RoundingMode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            RoundingMode::Nearest => write!(f, "RN"),
            RoundingMode::Zero => write!(f, "RZ"),
            RoundingMode::Plus => write!(f, "RP"),
            RoundingMode::Minus => write!(f, "RM"),
        }
    }
}

impl From<RoundingMode> for u8 {
    fn from(mode: RoundingMode) -> Self {
        mode as u8
    }
}

impl From<u8> for RoundingMode {
    fn from(value: u8) -> Self {
        Self::from_repr(value).unwrap()
    }
}

bitfield! {
    /// The FPU Control register (FCR31) exception flags. Used by the Cause, Enable, and Flag fields.
    #[derive(Ord, PartialOrd, Hash)]
    pub struct Exceptions(u8) {
        [0] inexact_operation,
        [1] underflow,
        [2] overflow,
        [3] divison_by_zero,
        [4] invalid_operation,
    }
}

bitfield! {
    /// The FPU Control register (FCR31) Cause field.
    #[derive(Ord, PartialOrd, Hash)]
    pub struct Cause(u8) {
        [0..=4] exceptions: u8 as Exceptions,
        [5] unimplemented_operation,
    }
}

bitfield! {
    /// The FPU Control register Control/Status, or FCR31.
    #[derive(Ord, PartialOrd, Hash)]
    pub struct ControlStatus(u32) {
        /// The rounding mode that is used for all floating-point operations.
        [0..=1] pub rounding_mode: u8 as RoundingMode,
        /// Indicate the exceptions that were raised after reset.
        [2..=6] pub flags: u8 as Exceptions,
        /// A floating-point exception is generated any time a Cause bit and the corresponding Enable bit are set.
        [7..=11] pub enables: u8 as Exceptions,
        /// Reflects the results of the most recently executed floating-point instruction.
        [12..=17] pub cause: u8 as Cause,
        /// The condition used for floating-point comparisons.
        [23] pub condition,
        /// When the enable bit is not set for the underflow exception and illegal exception,
        /// the unimplemented operation exception does not occur. The result will be flushed according to the rounding mode.
        [24] pub flush,
    }
}

impl ControlStatus {
    /// A mask that covers all the writable bits of the register.
    pub const WRITE_MASK: u64 = 0b1_1000_0011_1111_1111_1111_1111;

    /// The offset of the condition bit in the register.
    pub const CONDITION_BIT: u64 = 23;

    pub const fn new(raw: u32) -> Self {
        Self(raw)
    }
}
