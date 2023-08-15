//! Contains the operand types for the MIPS instruction set.

use super::{BitRange, InstructionParseError, InstructionResult};
use crate::register;
use strum::EnumCount;

/// An error which can occur when the instruction has an invalid operand, such as a register which is out of bounds.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum InvalidOperandError {
    Source,
    Target,
    Destination,
}

impl InvalidOperandError {
    const fn validate<const START: usize, const END: usize>(
        &self,
        range: BitRange<START, END>,
        raw: u32,
    ) -> InstructionResult<()> {
        // Note that we're always doing at least two `extract_from` calls when reading the operands,
        // but the function is probably cheap enough for it to be quicker than caching and reading from memory.
        if register::GeneralPurpose::is_valid(range.extract_from(raw) as usize) {
            Ok(())
        } else {
            Err(InstructionParseError::InvalidOperand(*self))
        }
    }
}

#[inline]
const fn general_purpose_register<const START: usize, const END: usize>(
    range: BitRange<START, END>,
    raw: u32,
) -> Option<register::GeneralPurpose> {
    register::GeneralPurpose::from_repr(range.extract_from(raw) as u8)
}

/// The Immediate instruction format (`I-Type`).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(transparent)]
pub struct Immediate {
    raw: u32,
}

impl Immediate {
    const RS_RANGE: BitRange<21, 25> = BitRange::new();
    const RT_RANGE: BitRange<16, 20> = BitRange::new();
    const IMMEDIATE_RANGE: BitRange<0, 15> = BitRange::new();

    /// Creates the `Immediate` operands for the given raw instruction.
    pub fn new(raw: u32) -> InstructionResult<Self> {
        InvalidOperandError::Source.validate(Self::RS_RANGE, raw)?;
        InvalidOperandError::Target.validate(Self::RT_RANGE, raw)?;
        Ok(Self { raw })
    }

    /// Extracts the source register (`rs`) from the instruction.
    #[inline]
    pub fn rs(&self) -> register::GeneralPurpose {
        // SAFETY: This type can only be constructed if the register is valid.
        unsafe { general_purpose_register(Self::RS_RANGE, self.raw).unwrap_unchecked() }
    }

    /// Extracts the target register (`rt`) from the instruction.
    #[inline]
    pub fn rt(&self) -> register::GeneralPurpose {
        // SAFETY: This type can only be constructed if the register is valid.
        unsafe { general_purpose_register(Self::RT_RANGE, self.raw).unwrap_unchecked() }
    }

    /// Extracts the immediate value from the instruction.
    #[inline]
    pub fn immediate(&self) -> u16 {
        Self::IMMEDIATE_RANGE.extract_from(self.raw) as u16
    }
}

/// The Jump instruction format (`J-Type`).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(transparent)]
pub struct Jump {
    raw: u32,
}

impl Jump {
    const TARGET_RANGE: BitRange<0, 25> = BitRange::new();

    /// Creates the `Jump` operands for the given raw instruction.
    pub fn new(raw: u32) -> InstructionResult<Self> {
        Ok(Self { raw })
    }

    /// Extracts the target value from the instruction.
    #[inline]
    pub fn target(&self) -> u32 {
        Self::TARGET_RANGE.extract_from(self.raw)
    }
}

/// The Register instruction format (`R-Type`).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(transparent)]
pub struct Register {
    raw: u32,
}

impl Register {
    const RS_RANGE: BitRange<21, 25> = BitRange::new();
    const RT_RANGE: BitRange<16, 20> = BitRange::new();
    const RD_RANGE: BitRange<11, 15> = BitRange::new();

    /// Creates the `Register` operands for the given raw instruction.
    pub fn new(raw: u32) -> InstructionResult<Self> {
        InvalidOperandError::Source.validate(Self::RS_RANGE, raw)?;
        InvalidOperandError::Target.validate(Self::RT_RANGE, raw)?;
        InvalidOperandError::Destination.validate(Self::RD_RANGE, raw)?;
        Ok(Self { raw })
    }

    /// Extracts the source register (`rs`) from the instruction.
    #[inline]
    pub fn rs(&self) -> register::GeneralPurpose {
        // SAFETY: This type can only be constructed if the register is valid.
        unsafe { general_purpose_register(Self::RS_RANGE, self.raw).unwrap_unchecked() }
    }

    /// Extracts the target register (`rt`) from the instruction.
    #[inline]
    pub fn rt(&self) -> register::GeneralPurpose {
        // SAFETY: This type can only be constructed if the register is valid.
        unsafe { general_purpose_register(Self::RT_RANGE, self.raw).unwrap_unchecked() }
    }

    /// Extracts the destination register (`rd`) from the instruction.
    #[inline]
    pub fn rd(&self) -> register::GeneralPurpose {
        // SAFETY: This type can only be constructed if the register is valid.
        unsafe { general_purpose_register(Self::RD_RANGE, self.raw).unwrap_unchecked() }
    }
}

/// The instruction type for the MIPS instruction set, with validated operands.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum InstructionType {
    Immediate(Immediate),
    Register(Register),
    Jump(Jump),
}

impl InstructionType {
    /// Creates a new Immediate (`I-Type`) operand.
    pub fn immediate(raw: u32) -> InstructionResult<Self> {
        Immediate::new(raw).map(Self::Immediate)
    }

    /// Creates a new Register (`R-Type`) operand.
    pub fn register(raw: u32) -> InstructionResult<Self> {
        Register::new(raw).map(Self::Register)
    }

    /// Creates a new Jump (`J-Type`) operand.
    pub fn jump(raw: u32) -> InstructionResult<Self> {
        Jump::new(raw).map(Self::Jump)
    }
}

/// The type of operand that an instruction can have, used for formatting.
#[derive(EnumCount, Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum Operand {
    Source,
    Target,
    Destination,
    Immediate,
    JumpTarget,
    Base,
    Offset,
    CacheOpcode,
    CacheSubject,
    FloatFormat,
    FloatSource,
    FloatTarget,
    FloatDestination,
    FloatCondition,
}

impl Operand {
    const fn is_general_purpose_register(&self) -> bool {
        matches!(
            self,
            Self::Source | Self::Target | Self::Destination | Self::Base
        )
    }
}

/// The type of an immediate, used for formatting.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) enum Signedness {
    Signed32,
    Signed16,
    Unsigned32,
    Unsigned16,
}

impl Signedness {
    pub fn format(&self, num: u32) -> String {
        match self {
            Signedness::Signed32 => format!("{}", num as i32),
            Signedness::Signed16 => format!("{}", num as i16),
            Signedness::Unsigned32 => format!("{:#x}", num),
            Signedness::Unsigned16 => format!("{:#x}", num as u16),
        }
    }
}

pub(crate) type OperandFormatInfo = &'static [(Operand, Signedness)];
