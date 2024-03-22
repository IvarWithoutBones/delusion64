use std::fmt;

use crate::instruction::ParsedInstruction;
use strum::FromRepr;

pub mod cache;
pub mod instruction;
mod label;
mod pattern;
pub mod register;

pub use crate::label::{Label, LabelList};

pub const INSTRUCTION_SIZE: usize = 4;
pub const REGISTER_COUNT: usize = 32;

pub const EXCEPTION_VECTOR_BASE: usize = 0x8000_0000;
pub const GENERAL_EXCEPTION_VECTOR: usize = EXCEPTION_VECTOR_BASE + 0x180;

/// See table 6-2 of the VR4300 manual.
#[derive(Debug, Clone, Copy, PartialEq, Eq, FromRepr)]
#[repr(u8)]
pub enum Exception {
    Interrupt = 0,
    TlbModification = 1,
    TlbMissLoad = 2,
    TlbMissStore = 3,
    AddressLoad = 4,
    AddressStore = 5,
    BusInstructionFetch = 6,
    BusDataLoadStore = 7,
    SystemCall = 8,
    Breakpoint = 9,
    ReservedInstruction = 10,
    CoprocessorUnusable = 11,
    ArithmeticOverflow = 12,
    Trap = 13,
    FloatingPoint = 15,
    Watch = 23,
}

impl Exception {
    /// The exception vector when BEV (Bootstrap Exception Vector) is disabled.
    pub const fn vector(&self) -> usize {
        match self {
            Exception::TlbMissLoad | Exception::TlbMissStore => EXCEPTION_VECTOR_BASE,
            _ => GENERAL_EXCEPTION_VECTOR,
        }
    }
}

impl From<Exception> for u8 {
    fn from(exception: Exception) -> Self {
        exception as u8
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum MaybeInstruction {
    Instruction(ParsedInstruction),
    Invalid(u32),
}

impl MaybeInstruction {
    pub const fn new(raw: u32) -> Self {
        if let Some(parsed) = ParsedInstruction::new(raw) {
            Self::Instruction(parsed)
        } else {
            Self::Invalid(raw)
        }
    }

    #[inline]
    pub const fn try_resolve_static_jump(&self, pc: u64) -> Option<u64> {
        match self {
            Self::Instruction(instr) => instr.try_resolve_constant_jump(pc),
            Self::Invalid(_) => None,
        }
    }

    #[inline]
    pub const fn ends_block(&self) -> bool {
        match self {
            Self::Instruction(instr) => instr.ends_block(),
            Self::Invalid(_) => true,
        }
    }

    #[inline]
    pub const fn has_delay_slot(&self) -> bool {
        match self {
            Self::Instruction(instr) => instr.has_delay_slot(),
            Self::Invalid(_) => false,
        }
    }

    #[inline]
    pub const fn discards_delay_slot(&self) -> bool {
        match self {
            Self::Instruction(instr) => instr.discards_delay_slot(),
            Self::Invalid(_) => false,
        }
    }

    #[inline]
    pub const fn is_valid(&self) -> bool {
        match self {
            Self::Instruction(_) => true,
            Self::Invalid(_) => false,
        }
    }

    #[inline]
    pub const fn unwrap(self) -> ParsedInstruction {
        match self {
            Self::Instruction(instr) => instr,
            Self::Invalid(_) => panic!("tried to unwrap invalid instruction"),
        }
    }
}

impl From<u32> for MaybeInstruction {
    fn from(value: u32) -> Self {
        if let Ok(instr) = value.try_into() {
            Self::Instruction(instr)
        } else {
            Self::Invalid(value)
        }
    }
}

impl fmt::Display for MaybeInstruction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Instruction(instr) => write!(f, "{instr}"),
            Self::Invalid(value) => write!(f, "unknown: {value:#034b}"),
        }
    }
}

#[derive(thiserror::Error, Debug)]
pub enum Error {
    #[error("invalid instruction: {raw} at offset {offset}")]
    InvalidInstruction { raw: RawInstruction, offset: usize },
    #[error("failed to read instruction data")]
    ReadInstructionData,
    #[error("label list is empty")]
    EmptyLabelList,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(transparent)]
pub struct RawInstruction(pub u32);

impl fmt::Display for RawInstruction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // 01010101010101010101010101010101 -> 0101_0101_0101_0101_0101_0101_0101_0101
        let [(a1, a2), (b1, b2), (c1, c2), (d1, d2)] =
            self.0.to_be_bytes().map(|byte| (byte >> 4, byte & 0b1111));
        write!(
            f,
            "0b{a1:04b}_{a2:04b}_{b1:04b}_{b2:04b}_{c1:04b}_{c2:04b}_{d1:04b}_{d2:04b}"
        )
    }
}

pub fn read_labels(count: usize, data: &mut impl Iterator<Item = u32>) -> Result<LabelList, Error> {
    let mut instrs = Vec::new();
    for _ in 0..count {
        let mut break_after = None;
        let mut offset = 0;
        loop {
            let value = data.next().ok_or(Error::ReadInstructionData)?;
            offset += 1;
            let Ok(instr) = ParsedInstruction::try_from(value) else {
                return Err(Error::InvalidInstruction {
                    raw: RawInstruction(value),
                    offset,
                });
            };

            if instr.has_delay_slot() {
                // Include the delay slot in this label
                break_after = Some(1);
            } else if instr.ends_block() {
                break_after = Some(0);
            }

            instrs.push(MaybeInstruction::Instruction(instr));
            if let Some(break_after) = &mut break_after {
                if *break_after == 0 {
                    break;
                } else {
                    *break_after -= 1;
                }
            }
        }
    }

    let labels = LabelList::new(&instrs);
    if labels.is_empty() {
        Err(Error::EmptyLabelList)
    } else {
        Ok(labels)
    }
}
