use crate::instruction::Instruction;

pub mod format;
pub mod instruction;

pub use instruction::SIZE as INSTRUCTION_SIZE;
pub const REGISTER_COUNT: usize = 32;
pub type Block = Vec<Instruction>;
pub type BlockList = Vec<Block>;

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

    pub fn next_instruction(&mut self) -> Option<Instruction> {
        let raw_instr = self.read_u32(self.pos)?;
        self.pos += INSTRUCTION_SIZE;
        Some(raw_instr.into())
    }

    pub fn instruction_at(&self, pos: usize) -> Option<Instruction> {
        Some(self.read_u32(pos)?.into())
    }

    pub fn iter(&self) -> impl Iterator<Item = Instruction> + '_ {
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
                if let Some(instr) = self.instruction_at(pos) {
                    pos += INSTRUCTION_SIZE;
                    block.push(instr.clone());
                    if instr.ends_block() {
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

                if instr.ends_block() {
                    // Last instruction in block, usually because of a jump.
                    if let Some(target) = instr.try_resolve_static_jump(pos) {
                        println!("{formatted: <40}-> {target:#04x}");
                    } else {
                        println!("{formatted: <40}-> ???");
                    }
                } else {
                    println!("{formatted}");
                }

                pos += INSTRUCTION_SIZE as u32;
            }
        }
    }

    fn read_u32(&self, from_pos: usize) -> Option<u32> {
        debug_assert!(from_pos % 4 == 0);

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
        if block.last().unwrap().has_delay_slot() {
            // Swap the last instruction with the first instruction of the next block.
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
