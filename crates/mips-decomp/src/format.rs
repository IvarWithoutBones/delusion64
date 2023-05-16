use crate::instruction::{Instruction, Mnemonic, Operand};
use core::fmt;

pub const fn register_name(index: u8) -> &'static str {
    assert!(index <= 31);
    match index {
        0 => "zero",
        1 => "at",
        2 => "v0",
        3 => "v1",
        4 => "a0",
        5 => "a1",
        6 => "a2",
        7 => "a3",
        8 => "t0",
        9 => "t1",
        10 => "t2",
        11 => "t3",
        12 => "t4",
        13 => "t5",
        14 => "t6",
        15 => "t7",
        16 => "s0",
        17 => "s1",
        18 => "s2",
        19 => "s3",
        20 => "s4",
        21 => "s5",
        22 => "s6",
        23 => "s7",
        24 => "t8",
        25 => "t9",
        26 => "k0",
        27 => "k1",
        28 => "gp",
        29 => "sp",
        30 => "s8",
        31 => "ra",
        _ => unreachable!(),
    }
}

impl fmt::Display for Operand {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Register {
                source,
                target,
                destination,
                shift,
            } => {
                if *shift != 0 {
                    // Shifting instructions
                    write!(
                        f,
                        "{}, {}, {}",
                        register_name(*destination),
                        register_name(*target),
                        *shift,
                    )
                } else {
                    write!(
                        f,
                        "{}, {}, {}",
                        register_name(*destination),
                        register_name(*source),
                        register_name(*target),
                    )
                }
            }

            Self::Immediate {
                source,
                target,
                immediate,
            } => write!(
                f,
                "{}, {}, {}",
                register_name(*target),
                register_name(*source),
                *immediate as i16,
            ),

            Self::Jump { target } => write!(f, "{target:#x}"),
        }
    }
}

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.mnemonic {
            Mnemonic::Nop => write!(f, "nop"),
            Mnemonic::Jr => match self.operand {
                Operand::Register { source, .. } => {
                    write!(f, "{: <7} {}", "jr", register_name(source))
                }
                _ => unreachable!(),
            },

            _ => {
                let mnemonic = format!("{:?}", self.mnemonic).to_lowercase();
                let operand = &self.operand;
                write!(f, "{mnemonic: <7} {operand}")
            }
        }
    }
}
