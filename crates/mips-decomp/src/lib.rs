use crate::instruction::ParsedInstruction;

pub mod instruction;
mod pattern;
pub mod register;

pub const INSTRUCTION_SIZE: usize = 4;
pub const REGISTER_COUNT: usize = 32;

pub type Block = Vec<MaybeInstruction>;
pub type BlockList = Vec<Block>;

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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Endian {
    Big,
    Little,
}

pub struct Decompiler<'a> {
    /// The binary to disassemble.
    bin: &'a [u8],
    /// The endianness of the binary.
    endian: Endian,
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

    pub fn next_instruction(&mut self) -> Option<ParsedInstruction> {
        let raw_instr = self.read_u32(self.pos)?;
        self.pos += INSTRUCTION_SIZE;
        raw_instr.try_into().ok()
    }

    pub fn instruction_at(&self, pos: usize) -> Option<ParsedInstruction> {
        self.read_u32(pos)?.try_into().ok()
    }

    pub fn iter(&self) -> impl Iterator<Item = ParsedInstruction> + '_ {
        let mut pos = 0;
        std::iter::from_fn(move || {
            let instr = self.instruction_at(pos)?;
            pos += INSTRUCTION_SIZE;
            Some(instr)
        })
    }

    pub fn blocks(&self) -> impl Iterator<Item = Block> + '_ {
        let mut pos = 0;
        let mut found_end = false;
        std::iter::from_fn(move || {
            if found_end {
                return None;
            }

            let mut block = Vec::new();
            loop {
                if let Some(raw) = self.read_u32(pos) {
                    pos += INSTRUCTION_SIZE;
                    let (instr, ends_block) = {
                        let instr: MaybeInstruction = raw.into();
                        let ends_block = instr.ends_block();
                        (instr, ends_block)
                    };

                    block.push(instr);
                    if ends_block {
                        break;
                    }
                } else {
                    found_end = true;
                    break;
                }
            }

            Some(block)
        })
    }

    pub fn pretty_print(&self, blocks: &BlockList) {
        let mut pos = 0;
        for (i, block) in blocks.iter().enumerate() {
            println!("block {i}:");

            for instr in block.iter() {
                let formatted = format!("  {pos:#04x}    {instr}");

                if instr.is_valid() && instr.ends_block() {
                    // Last instruction in block, usually because of a jump.
                    if let Some(target) = instr.try_resolve_static_jump(pos) {
                        println!("{formatted: <40}-> {target:#04x}");
                    } else {
                        println!("{formatted: <40}-> ???");
                    }
                } else {
                    println!("{formatted}");
                }

                pos += INSTRUCTION_SIZE as u64;
            }
        }
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

pub fn reorder_delay_slots(blocks: BlockList) -> BlockList {
    // Cloning isnt very efficient, but can be fixed later.
    let mut result = blocks.clone();
    for (block_idx, block) in blocks.iter().enumerate() {
        if block.is_empty() || block_idx + 1 >= result.len() {
            // No next block, so we can't swap.
            continue;
        }

        let instr = block.last().unwrap();
        if !instr.is_valid() {
            // Can't swap invalid instructions.
            continue;
        }

        if instr.has_delay_slot() {
            // Swap the last instruction with the first instruction of the next block.
            let next_instr = result[block_idx + 1].first().unwrap();
            if !next_instr.is_valid() {
                // Can't swap invalid instructions.
                continue;
            }

            let next_instr = result[block_idx + 1].remove(0);
            let instr = result[block_idx].pop().unwrap();

            result[block_idx].push(next_instr);
            result[block_idx].push(instr);

            // Remove empty blocks
            if result[block_idx + 1].is_empty() {
                result.remove(block_idx + 1);
            }
        }
    }
    result
}
