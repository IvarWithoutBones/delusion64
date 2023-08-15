use self::operand::{InvalidOperandError, InstructionType};

mod mnemonic;
pub mod operand;

pub use mnemonic::Mnemonic;

/// An inclusive range of bits in a 32-bit integer.
struct BitRange<const START: usize, const END: usize> {
    // Manual construction circumvents the asserts
    _please_dont_manually_construct_me: (),
}

impl<const START: usize, const END: usize> BitRange<START, END> {
    const ASSERT_START_IS_LESS_THAN_END: () =
        assert!(START < END, "start of range must be less than end of range");
    const ASSERT_FITS_IN_32_BITS: () =
        assert!(END < u32::BITS as usize, "range must fit in 32 bits");

    const fn new() -> Self {
        // Ensure we validate our constraints at compile time, will not do anything at runtime.
        // Note that we use associated constants, from there we can use the outer const generics.
        #[allow(path_statements)]
        #[allow(clippy::no_effect)]
        {
            Self::ASSERT_START_IS_LESS_THAN_END;
            Self::ASSERT_FITS_IN_32_BITS;
        }

        Self {
            _please_dont_manually_construct_me: (),
        }
    }

    const MASK: u32 = 2_u32.pow(END as u32 - START as u32) - 1;

    #[inline]
    const fn extract_from(&self, raw: u32) -> u32 {
        (raw >> START) & Self::MASK
    }
}

/// An error which can occur when parsing an instruction.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum InstructionParseError {
    /// There was no matching instruction found for the given opcode.
    UnknownOpcode,
    /// The instruction had an invalid operand, such as a register which is out of bounds.
    InvalidOperand(InvalidOperandError),
}

pub type InstructionResult<T> = Result<T, InstructionParseError>;

/// An instruction, with its operands.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Instruction {
    /// The name of the instruction.
    pub mnemonic: Mnemonic,
    /// The operands of the instruction.
    pub operands: InstructionType,
}

pub fn decode(num: u32) -> InstructionResult<Instruction> {
    // TODO: is it needed to create a lookup table here? Need to check if LLVM will do the work for us.
    match num {
        0x0000_0000 => Ok(Instruction {
            mnemonic: Mnemonic::Add,
            operands: InstructionType::register(num)?,
        }),

        _ => Err(InstructionParseError::UnknownOpcode),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn bit_range_zero_sized() {
        let range = BitRange::<0, 1>::new();
        assert_eq!(std::mem::size_of_val(&range), 0);
    }

    #[test]
    fn bit_range_extract() {
        let range = BitRange::<0, 1>::new();
        assert_eq!(range.extract_from(0b0), 0b0);
        assert_eq!(range.extract_from(0b1), 0b1);
        assert_eq!(range.extract_from(0b11), 0b1);
    }

    #[test]
    fn bit_range_extract_multiple_bits() {
        let range = BitRange::<0, 2>::new();
        assert_eq!(range.extract_from(0b0), 0b0);
        assert_eq!(range.extract_from(0b1), 0b1);
        assert_eq!(range.extract_from(0b11), 0b11);
        assert_eq!(range.extract_from(0b111), 0b11);
    }

    #[test]
    fn bit_range_extract_multiple_bits_from_middle() {
        let range = BitRange::<1, 3>::new();
        assert_eq!(range.extract_from(0b0), 0b0);
        assert_eq!(range.extract_from(0b1), 0b0);
        assert_eq!(range.extract_from(0b11), 0b1);
        assert_eq!(range.extract_from(0b111), 0b11);
        assert_eq!(range.extract_from(0b1111), 0b11);
    }
}
