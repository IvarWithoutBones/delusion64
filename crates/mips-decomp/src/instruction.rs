/// The size of a single instruction, in bytes.
pub const SIZE: usize = 4;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Operand {
    Register {
        source: u8,
        target: u8,
        destination: u8,
        shift: u8,
    },

    Immediate {
        source: u8,
        target: u8,
        immediate: u16,
    },

    #[allow(dead_code)]
    Jump { target: u32 },
}

impl Operand {
    #[inline]
    const fn register(from: u32) -> Self {
        Self::Register {
            source: ((from >> 21) & 0b1_1111) as u8,
            target: ((from >> 16) & 0b1_1111) as u8,
            destination: ((from >> 11) & 0b1_1111) as u8,
            shift: ((from >> 6) & 0b1_1111) as u8,
        }
    }

    #[inline]
    const fn immediate(from: u32) -> Self {
        Self::Immediate {
            source: ((from >> 21) & 0b1_1111) as u8,
            target: ((from >> 16) & 0b1_1111) as u8,
            immediate: (from & 0b1111_1111_1111_1111) as u16,
        }
    }

    #[allow(dead_code)]
    #[inline]
    const fn jump(from: u32) -> Self {
        Self::Jump {
            target: from & 0b11_1111_1111_1111_1111_1111_1111,
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Mnemonic {
    // Register
    Daddu,
    Jr,
    Sll,
    Sltu,
    Dsll,

    // Immediate
    Bne,
    Beq,
    Lb,
    Lw,
    Ld,
    Sd,
    Sw,
    Lui,
    Ori,
    Addi,
    Daddiu,
    Sltiu,

    // Special
    Mtc0,
}

impl Mnemonic {
    pub const fn ends_block(&self) -> bool {
        matches!(self, Self::Jr | Self::Beq | Self::Bne)
    }

    pub const fn has_delay_slot(&self) -> bool {
        matches!(self, Self::Jr | Self::Beq | Self::Bne)
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Instruction {
    pub mnemonic: Mnemonic,
    pub operand: Operand,
}

impl Instruction {
    /// The `opcode` field for a raw instruction.
    #[inline]
    const fn opcode(value: u32) -> u8 {
        (value >> 26) as u8
    }

    /// The `function` field for a raw register instruction.
    #[inline]
    const fn register_function(value: u32) -> u8 {
        (value & 0b11_1111) as u8
    }

    /// The `function` field for a raw coprocessor instruction.
    #[inline]
    const fn coprocessor_function(value: u32) -> u8 {
        ((value >> 21) & 0b1_1111) as u8
    }

    #[inline]
    pub const fn ends_block(&self) -> bool {
        self.mnemonic.ends_block()
    }

    #[inline]
    pub const fn has_delay_slot(&self) -> bool {
        self.mnemonic.has_delay_slot()
    }

    pub const fn try_resolve_static_jump(&self, pc: u32) -> Option<u32> {
        match self.mnemonic {
            Mnemonic::Beq | Mnemonic::Bne => {
                if let Operand::Immediate { immediate, .. } = self.operand {
                    Some((pc as i32 + (immediate as i16 * SIZE as i16) as i32) as u32)
                } else {
                    unreachable!();
                }
            }

            _ => None,
        }
    }
}

impl From<u32> for Instruction {
    fn from(value: u32) -> Self {
        let opcode = Self::opcode(value);
        let (mnemonic, operand) = match opcode {
            // Register
            0b00_0000 => {
                let func = Self::register_function(value);
                let mnemonic = match func {
                    0b10_1101 => Mnemonic::Daddu,
                    0b00_1000 => Mnemonic::Jr,
                    0b00_0000 => Mnemonic::Sll,
                    0b10_1011 => Mnemonic::Sltu,
                    0b11_1000 => Mnemonic::Dsll,
                    _ => todo!("register opcode {func:#6b} {func:#x}"),
                };

                (mnemonic, Operand::register(value))
            }

            // Coprocessor
            0b01_0000 => {
                let func = Self::coprocessor_function(value);
                let mnemonic = match func {
                    0b0_0100 => Mnemonic::Mtc0,
                    _ => todo!("coprocessor opcode {func:#6b} {func:#x}"),
                };

                (mnemonic, Operand::register(value))
            }

            // Immediate
            _ => {
                let mnemonic = match opcode {
                    0b00_1011 => Mnemonic::Sltiu,
                    0b00_0100 => Mnemonic::Beq,
                    0b01_1001 => Mnemonic::Daddiu,
                    0b10_0000 => Mnemonic::Lb,
                    0b10_0011 => Mnemonic::Lw,
                    0b10_1011 => Mnemonic::Sw,
                    0b00_0101 => Mnemonic::Bne,
                    0b00_1111 => Mnemonic::Lui,
                    0b00_1101 => Mnemonic::Ori,
                    0b00_1000 => Mnemonic::Addi,
                    0b11_1111 => Mnemonic::Sd,
                    0b11_0111 => Mnemonic::Ld,
                    _ => todo!("opcode {opcode:#6b} {opcode:#x}"),
                };

                (mnemonic, Operand::immediate(value))
            }
        };

        Self { mnemonic, operand }
    }
}
