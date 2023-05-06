use crate::instruction::{self as instr, Instruction};

mod format;
pub mod instruction;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Endian {
    Big,
    Little,
}

pub struct Disassembler<'a> {
    /// The binary to disassemble.
    bin: &'a [u8],
    /// The endianness of the binary.
    endian: Endian,
    /// The current position in the binary.
    pos: usize,
}

impl<'a> Disassembler<'a> {
    pub fn new(bin: &'a [u8], endian: Endian) -> Self {
        Self {
            bin,
            endian,
            pos: 0,
        }
    }

    pub fn next_instruction(&mut self) -> Option<Instruction> {
        let raw_instr = self.read_u32(self.pos)?;
        self.pos += instr::SIZE;
        Some(raw_instr.into())
    }

    pub fn instruction_at(&self, pos: usize) -> Option<Instruction> {
        Some(self.read_u32(pos)?.into())
    }

    fn read_u32(&self, from_pos: usize) -> Option<u32> {
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

impl<'a> From<&'a [u8]> for Disassembler<'a> {
    fn from(bin: &'a [u8]) -> Self {
        Self {
            bin,
            pos: 0,
            // Most MIPS binaries are big endian.
            endian: Endian::Big,
        }
    }
}

impl<'a> Iterator for Disassembler<'a> {
    type Item = Instruction;

    fn next(&mut self) -> Option<Self::Item> {
        self.next_instruction()
    }
}
