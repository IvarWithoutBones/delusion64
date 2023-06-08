use crate::instruction::ParsedInstruction;

pub mod instruction;
mod label;
mod pattern;
pub mod register;

pub use crate::label::{Label, LabelList};

pub const INSTRUCTION_SIZE: usize = 4;
pub const REGISTER_COUNT: usize = 32;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum MaybeInstruction {
    Instruction(ParsedInstruction),
    Invalid(u32),
}

impl MaybeInstruction {
    pub fn try_resolve_static_jump(&self, pc: u64) -> Option<u64> {
        match self {
            Self::Instruction(instr) => instr.try_resolve_static_jump(pc),
            Self::Invalid(_) => None,
        }
    }

    pub const fn ends_block(&self) -> bool {
        match self {
            Self::Instruction(instr) => instr.ends_block(),
            Self::Invalid(_) => true,
        }
    }

    pub const fn has_delay_slot(&self) -> bool {
        match self {
            Self::Instruction(instr) => instr.has_delay_slot(),
            Self::Invalid(_) => false,
        }
    }

    pub const fn discards_delay_slot(&self) -> bool {
        match self {
            Self::Instruction(instr) => instr.discards_delay_slot(),
            Self::Invalid(_) => false,
        }
    }

    pub const fn is_valid(&self) -> bool {
        match self {
            Self::Instruction(_) => true,
            Self::Invalid(_) => false,
        }
    }

    #[inline]
    pub fn unwrap(self) -> ParsedInstruction {
        match self {
            Self::Instruction(instr) => instr,
            Self::Invalid(value) => panic!("tried to unwrap invalid instruction: {value:#034b}"),
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

impl std::fmt::Display for MaybeInstruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Instruction(instr) => write!(f, "{instr}"),
            Self::Invalid(value) => write!(f, "unknown: {value:#034b}"),
        }
    }
}

#[derive(Default, Debug, Clone, Copy, PartialEq, Eq)]
pub enum Endian {
    #[default]
    Big,
    Little,
}

pub struct Decompiler<'a> {
    /// The binary to disassemble.
    pub bin: &'a [u8],
    /// The endianness of the binary.
    pub endian: Endian,
    /// The current position in the binary.
    pos: usize,
}

impl<'a> Decompiler<'a> {
    pub fn new(bin: &'a [u8], endian: Endian) -> Self {
        Self {
            bin,
            endian,
            pos: 0,
        }
    }

    pub fn next_instruction(&mut self) -> Option<MaybeInstruction> {
        let raw_instr = self.read_u32(self.pos)?;
        self.pos += INSTRUCTION_SIZE;
        raw_instr.try_into().ok()
    }

    pub fn instruction_at(&self, pos: usize) -> Option<MaybeInstruction> {
        self.read_u32(pos)?.try_into().ok()
    }

    pub fn iter(&self) -> impl Iterator<Item = MaybeInstruction> + '_ {
        let mut pos = 0;
        std::iter::from_fn(move || {
            let instr = self.instruction_at(pos)?;
            pos += INSTRUCTION_SIZE;
            Some(instr)
        })
    }

    fn read_u32(&self, from_pos: usize) -> Option<u32> {
        debug_assert!(from_pos % INSTRUCTION_SIZE == 0);

        let data = [
            *self.bin.get(from_pos)?,
            *self.bin.get(from_pos + 1)?,
            *self.bin.get(from_pos + 2)?,
            *self.bin.get(from_pos + 3)?,
        ];

        Some(match self.endian {
            Endian::Big => u32::from_be_bytes(data),
            Endian::Little => u32::from_le_bytes(data),
        })
    }
}

impl<'a> From<&'a [u8]> for Decompiler<'a> {
    fn from(bin: &'a [u8]) -> Self {
        Self {
            bin,
            pos: 0,
            // Most MIPS binaries are big endian.
            endian: Endian::Big,
        }
    }
}
