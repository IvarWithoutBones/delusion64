use crate::{
    pattern::{InstructionPattern, Operand},
    register, INSTRUCTION_SIZE,
};
use std::fmt;
use strum::{EnumVariantNames, VariantNames};

#[derive(EnumVariantNames, Debug, Clone, Copy, PartialEq, Eq)]
#[strum(serialize_all = "lowercase")]
pub enum Mnenomic {
    Add,
    Addi,
    Addiu,
    Addu,
    And,
    Andi,
    Bczf,
    Bczfl,
    Bczt,
    Bcztl,
    Beq,
    Beql,
    Bgez,
    Bgezal,
    Bgezall,
    Bgezl,
    Bgtz,
    Bgtzl,
    Blez,
    Blezl,
    Bltz,
    Bltzal,
    Bltzall,
    Bltzl,
    Bne,
    Bnel,
    Break,
    Cache,
    Cfcz,
    Copz,
    Ctcz,
    Dadd,
    Daddi,
    Daddiu,
    Daddu,
    Ddiv,
    Ddivu,
    Div,
    Divu,
    Dmfc0,
    Dmtc0,
    Dmult,
    Dmultu,
    Dsll,
    Dsllv,
    Dsll32,
    Dsra,
    Dsrav,
    Dsra32,
    Dsrl,
    Dsrlv,
    Dsrl32,
    Dsub,
    Dsubu,
    Eret,
    J,
    Jal,
    Jalr,
    #[strum(serialize = "jalr")]
    JalrR31,
    Jr,
    Lb,
    Lbu,
    Ld,
    Ldcz,
    Ldl,
    Ldr,
    Lh,
    Lhu,
    Ll,
    Lld,
    Lui,
    Lw,
    Lwcz,
    Lwl,
    Lwr,
    Lwu,
    Mfc0,
    Mfcz,
    Mfhi,
    Mflo,
    Mtc0,
    Mtcz,
    Mthi,
    Mtlo,
    Mult,
    Multu,
    Nor,
    Or,
    Ori,
    Sb,
    Sc,
    Scd,
    Sd,
    Sdcz,
    Sdl,
    Sdr,
    Sh,
    Sll,
    Sllv,
    Slt,
    Slti,
    Sltiu,
    Sltu,
    Sra,
    Srav,
    Srl,
    Srlv,
    Sub,
    Subu,
    Sw,
    Swcz,
    Swl,
    Swr,
    Sync,
    Syscall,
    Teq,
    Teqi,
    Tge,
    Tgei,
    Tgeiu,
    Tgeu,
    Tlbp,
    Tlbr,
    Tlbwi,
    Tlbwr,
    Tlt,
    Tlti,
    Tltiu,
    Tltu,
    Tne,
    Tnei,
    Xor,
    Xori,

    // Floating point
    AbsFmt,
    AddFmt,
    CCondFmtFs,
    Bc1f,
    Bc1fl,
    Bc1t,
    Bc1tl,
}

impl Mnenomic {
    // All cp0 instructions use rd as the coprocessor register.
    pub const fn uses_cp0_destination(&self) -> bool {
        matches!(
            self,
            Mnenomic::Mfc0
                | Mnenomic::Mfcz
                | Mnenomic::Mtc0
                | Mnenomic::Mtcz
                | Mnenomic::Dmfc0
                | Mnenomic::Dmtc0
        )
    }

    pub const fn is_branch(&self) -> bool {
        matches!(
            self,
            Mnenomic::Bczf
                | Mnenomic::Bczfl
                | Mnenomic::Bczt
                | Mnenomic::Bcztl
                | Mnenomic::Beq
                | Mnenomic::Beql
                | Mnenomic::Bgez
                | Mnenomic::Bgezal
                | Mnenomic::Bgezall
                | Mnenomic::Bgezl
                | Mnenomic::Bgtz
                | Mnenomic::Bgtzl
                | Mnenomic::Blez
                | Mnenomic::Blezl
                | Mnenomic::Bltz
                | Mnenomic::Bltzal
                | Mnenomic::Bltzall
                | Mnenomic::Bltzl
                | Mnenomic::Bne
                | Mnenomic::Bnel
        )
    }

    pub const fn ends_block(&self) -> bool {
        self.is_branch()
            | matches!(self, |Mnenomic::Break| Mnenomic::Eret
                | Mnenomic::J
                | Mnenomic::Jal
                | Mnenomic::Jalr
                | Mnenomic::JalrR31
                | Mnenomic::Jr
                | Mnenomic::Syscall
                | Mnenomic::Teq
                | Mnenomic::Teqi
                | Mnenomic::Tge
                | Mnenomic::Tgei
                | Mnenomic::Tgeiu
                | Mnenomic::Tgeu)
    }

    pub const fn discards_delay_slot(&self) -> bool {
        matches!(
            self,
            Mnenomic::Bczfl
                | Mnenomic::Bcztl
                | Mnenomic::Beql
                | Mnenomic::Bgezall
                | Mnenomic::Bgezl
                | Mnenomic::Bgtzl
                | Mnenomic::Blezl
                | Mnenomic::Bltzall
                | Mnenomic::Bltzl
                | Mnenomic::Bnel
        )
    }

    pub const fn has_delay_slot(&self) -> bool {
        self.ends_block()
    }

    pub const fn name(&self) -> &'static str {
        Self::VARIANTS[*self as usize]
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Signedness {
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Instruction {
    pattern: InstructionPattern,
    mnenomic: Mnenomic,
    operands: &'static [(Operand, Signedness)],
}

impl Instruction {
    pub const fn new(
        mnenomic: Mnenomic,
        pattern: &str,
        operands: &'static [(Operand, Signedness)],
    ) -> Self {
        Self {
            pattern: InstructionPattern::new(pattern),
            mnenomic,
            operands,
        }
    }

    pub fn try_resolve_static_jump(&self, raw: u32, pc: u64) -> Option<u64> {
        if !self.mnenomic.is_branch() {
            return None;
        }

        match self.operands {
            [.., (Operand::Offset, Signedness::Signed16)] => {
                // Branch instructions, these jump from the delay slot + the offset (which can be negative)
                let offset = ((self.pattern.get(Operand::Offset, raw)? as i16) << 2) as i64;
                Some(((pc + INSTRUCTION_SIZE as u64) as i64 + offset) as u64)
            }

            _ => todo!(
                "instruction {:?} has static jump but could not resolve",
                self.mnenomic
            ),
        }
    }

    pub fn format(&self, raw: u32) -> String {
        if raw == 0 {
            // Psuedo instruction
            return "nop".to_string();
        }

        let mut result = format!("{: <8}", self.mnenomic.name());
        for (i, (op, sign)) in self.operands.iter().enumerate() {
            let num = self
                .pattern
                .get(*op, raw)
                .unwrap_or_else(|| panic!("failed to get operand {op:?} for {:?}", self.mnenomic));

            if op.is_register() {
                if self.mnenomic.uses_cp0_destination() && op == &Operand::Destination {
                    result.push_str(register::Cp0::name_from_index(num as _));
                } else {
                    result.push_str(register::GeneralPurpose::name_from_index(num as _));
                }
            } else {
                result.push_str(&sign.format(num));
            }

            if i < self.operands.len() - 1 {
                result.push_str(", ");
            }
        }

        result
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParsedInstruction {
    instr: &'static Instruction,
    raw: u32,
}

impl ParsedInstruction {
    pub fn try_resolve_static_jump(&self, pc: u64) -> Option<u64> {
        self.instr.try_resolve_static_jump(self.raw, pc)
    }

    pub const fn mnemonic(&self) -> Mnenomic {
        self.instr.mnenomic
    }

    pub const fn ends_block(&self) -> bool {
        self.instr.mnenomic.ends_block()
    }

    pub const fn has_delay_slot(&self) -> bool {
        self.instr.mnenomic.has_delay_slot()
    }

    pub const fn discards_delay_slot(&self) -> bool {
        self.instr.mnenomic.discards_delay_slot()
    }

    pub fn get(&self, op: Operand) -> Option<u32> {
        self.instr.pattern.get(op, self.raw)
    }

    pub fn rt(&self) -> u32 {
        self.get(Operand::Target).unwrap_or_else(|| {
            panic!(
                "failed to get target register for instruction {:?}",
                self.instr.mnenomic.name()
            )
        })
    }

    pub fn rs(&self) -> u32 {
        self.get(Operand::Source).unwrap_or_else(|| {
            panic!(
                "failed to get source register for instruction {:?}",
                self.instr.mnenomic.name()
            )
        })
    }

    pub fn rd(&self) -> u32 {
        self.get(Operand::Destination).unwrap_or_else(|| {
            panic!(
                "failed to get destination register for instruction {:?}",
                self.instr.mnenomic.name()
            )
        })
    }

    pub fn sa(&self) -> u32 {
        self.get(Operand::Immediate).unwrap_or_else(|| {
            panic!(
                "failed to get shift amount for instruction {:?}",
                self.instr.mnenomic.name()
            )
        })
    }

    pub fn immediate(&self) -> u32 {
        self.get(Operand::Immediate).unwrap_or_else(|| {
            panic!(
                "failed to get immediate for instruction {:?}",
                self.instr.mnenomic.name()
            )
        })
    }

    pub fn offset(&self) -> u32 {
        self.get(Operand::Offset).unwrap_or_else(|| {
            panic!(
                "failed to get offset for instruction {:?}",
                self.instr.mnenomic.name()
            )
        })
    }

    pub fn base(&self) -> u32 {
        self.get(Operand::Base).unwrap_or_else(|| {
            panic!(
                "failed to get base for instruction {:?}",
                self.instr.mnenomic.name()
            )
        })
    }

    pub fn coprocessor(&self) -> u32 {
        self.get(Operand::Coprocessor).unwrap_or_else(|| {
            panic!(
                "failed to get coprocessor for instruction {:?}",
                self.instr.mnenomic.name()
            )
        })
    }
}

impl TryFrom<u32> for ParsedInstruction {
    type Error = ();

    fn try_from(value: u32) -> Result<Self, Self::Error> {
        decode(value)
            .map(|instr| Self { instr, raw: value })
            .ok_or(())
    }
}

impl fmt::Display for ParsedInstruction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.instr.format(self.raw))
    }
}

macro_rules! operand {
    ($name:ident) => {
        (Operand::$name, Signedness::Unsigned32)
    };

    ($name:ident, $sign:ident) => {
        (Operand::$name, Signedness::$sign)
    };
}

macro_rules! instr {
    ($mnenomic:ident, $pattern:expr) => {
        Instruction::new(Mnenomic::$mnenomic, $pattern, &[])
    };

    ($mnenomic:ident, $pattern:expr, $($operands:tt)*) => {
        Instruction::new(Mnenomic::$mnenomic, $pattern,&[$(operand! $operands),*])
    };
}

/// Copied from the fantastic n64brew wiki, thanks!
/// https://n64brew.dev/wiki/MIPS_III_instructions#CPU_Instruction_Set
#[rustfmt::skip]
const INSTRUCTIONS: &[Instruction] = &[
    instr!(Add,     "0000 00ss ssst tttt dddd d000 0010 0000", (Destination)(Source)(Target)),
    instr!(Addi,    "0010 00ss ssst tttt kkkk kkkk kkkk kkkk", (Target)(Source)(Immediate, Signed16)),
    instr!(Addiu,   "0010 01ss ssst tttt kkkk kkkk kkkk kkkk", (Target)(Source)(Immediate, Signed16)),
    instr!(Addu,    "0000 00ss ssst tttt dddd d000 0010 0001", (Destination)(Source)(Target)),
    instr!(And,     "0000 00ss ssst tttt dddd d000 0010 0100", (Destination)(Source)(Target)),
    instr!(Andi,    "0011 00ss ssst tttt kkkk kkkk kkkk kkkk", (Target)(Source)(Immediate)),
    instr!(Bczf,    "0100 xx01 0000 0000 ffff ffff ffff ffff", (Coprocessor)(Offset, Signed16)),
    instr!(Bczfl,   "0100 xx01 0000 0010 ffff ffff ffff ffff", (Coprocessor)(Offset, Signed16)),
    instr!(Bczt,    "0100 xx01 0000 0001 ffff ffff ffff ffff", (Coprocessor)(Offset, Signed16)),
    instr!(Bcztl,   "0100 xx01 0000 0011 ffff ffff ffff ffff", (Coprocessor)(Offset, Signed16)),
    instr!(Beq,     "0001 00ss ssst tttt ffff ffff ffff ffff", (Source)(Target)(Offset, Signed16)),
    instr!(Beql,    "0101 00ss ssst tttt ffff ffff ffff ffff", (Source)(Target)(Offset, Signed16)),
    instr!(Bgez,    "0000 01ss sss0 0001 ffff ffff ffff ffff", (Source)(Offset, Signed16)),
    instr!(Bgezal,  "0000 01ss sss1 0001 ffff ffff ffff ffff", (Source)(Offset, Signed16)),
    instr!(Bgezall, "0000 01ss sss1 0011 ffff ffff ffff ffff", (Source)(Offset, Signed16)),
    instr!(Bgezl,   "0000 01ss sss0 0011 ffff ffff ffff ffff", (Source)(Offset, Signed16)),
    instr!(Bgtz,    "0001 11ss sss0 0000 ffff ffff ffff ffff", (Source)(Offset, Signed16)),
    instr!(Bgtzl,   "0101 11ss sss0 0000 ffff ffff ffff ffff", (Source)(Offset, Signed16)),
    instr!(Blez,    "0001 10ss sss0 0000 ffff ffff ffff ffff", (Source)(Offset, Signed16)),
    instr!(Blezl,   "0101 10ss sss0 0000 ffff ffff ffff ffff", (Source)(Offset, Signed16)),
    instr!(Bltz,    "0000 01ss sss0 0000 ffff ffff ffff ffff", (Source)(Offset, Signed16)),
    instr!(Bltzal,  "0000 01ss sss1 0000 ffff ffff ffff ffff", (Source)(Offset, Signed16)),
    instr!(Bltzall, "0000 01ss sss1 0010 ffff ffff ffff ffff", (Source)(Offset, Signed16)),
    instr!(Bltzl,   "0000 01ss sss0 0010 ffff ffff ffff ffff", (Source)(Offset, Signed16)),
    instr!(Bne,     "0001 01ss ssst tttt ffff ffff ffff ffff", (Source)(Target)(Offset, Signed16)),
    instr!(Bnel,    "0101 01ss ssst tttt ffff ffff ffff ffff", (Source)(Target)(Offset, Signed16)),
    instr!(Break,   "0000 00kk kkkk kkkk kkkk kkkk kk00 1101"),
    instr!(Cache,   "1011 11bb bbbk kkkk ffff ffff ffff ffff", (Immediate)(Offset, Signed16)(Base)),
    instr!(Cfcz,    "0100 xx00 010t tttt dddd d000 0000 0000", (Coprocessor)(Target)(Destination)),
    instr!(Copz,    "0100 xx1k kkkk kkkk kkkk kkkk kkkk kkkk", (Coprocessor)(Immediate)),
    instr!(Ctcz,    "0100 xx00 110t tttt dddd d000 0000 0000", (Coprocessor)(Target)(Destination)),
    instr!(Dadd,    "0000 00ss ssst tttt dddd d000 0010 1100", (Destination)(Source)(Target)),
    instr!(Daddi,   "0110 00ss ssst tttt kkkk kkkk kkkk kkkk", (Target)(Source)(Immediate, Signed16)),
    instr!(Daddiu,  "0110 01ss ssst tttt kkkk kkkk kkkk kkkk", (Target)(Source)(Immediate, Signed16)),
    instr!(Daddu,   "0000 00ss ssst tttt dddd d000 0010 1101", (Destination)(Source)(Target)),
    instr!(Ddiv,    "0000 00ss ssst tttt 0000 0000 0001 1110", (Source)(Target)),
    instr!(Ddivu,   "0000 00ss ssst tttt 0000 0000 0001 1111", (Source)(Target)),
    instr!(Div,     "0000 00ss ssst tttt 0000 0000 0001 1010", (Source)(Target)),
    instr!(Divu,    "0000 00ss ssst tttt 0000 0000 0001 1011", (Source)(Target)),
    instr!(Dmfc0,   "0100 0000 001t tttt dddd d000 0000 0000", (Target)(Destination)),
    instr!(Dmtc0,   "0100 0000 101t tttt dddd d000 0000 0000", (Target)(Destination)),
    instr!(Dmult,   "0000 00ss ssst tttt 0000 0000 0001 1100", (Source)(Target)),
    instr!(Dmultu,  "0000 00ss ssst tttt 0000 0000 0001 1101", (Source)(Target)),
    instr!(Dsll,    "0000 0000 000t tttt dddd dkkk kk11 1000", (Destination)(Target)(Immediate)),
    instr!(Dsllv,   "0000 00ss ssst tttt dddd d000 0001 0100", (Destination)(Target)(Source)),
    instr!(Dsll32,  "0000 0000 000t tttt dddd dkkk kk11 1100", (Destination)(Target)(Immediate)),
    instr!(Dsra,    "0000 0000 000t tttt dddd dkkk kk11 1011", (Destination)(Target)(Immediate)),
    instr!(Dsrav,   "0000 00ss ssst tttt dddd d000 0001 0111", (Destination)(Target)(Source)),
    instr!(Dsra32,  "0000 0000 000t tttt dddd dkkk kk11 1111", (Destination)(Target)(Immediate)),
    instr!(Dsrl,    "0000 0000 000t tttt dddd dkkk kk11 1010", (Destination)(Target)(Immediate)),
    instr!(Dsrlv,   "0000 00ss ssst tttt dddd d000 0001 0110", (Destination)(Target)(Source)),
    instr!(Dsrl32,  "0000 0000 000t tttt dddd dkkk kk11 1110", (Destination)(Target)(Immediate)),
    instr!(Dsub,    "0000 00ss ssst tttt dddd d000 0010 1110", (Destination)(Source)(Target)),
    instr!(Dsubu,   "0000 00ss ssst tttt dddd d000 0010 1111", (Destination)(Source)(Target)),
    instr!(Eret,    "0100 0010 0000 0000 0000 0000 0001 1000"),
    instr!(J,       "0000 10kk kkkk kkkk kkkk kkkk kkkk kkkk", (Immediate)),
    instr!(Jal,     "0000 11kk kkkk kkkk kkkk kkkk kkkk kkkk", (Immediate)),
    instr!(Jalr,    "0000 00ss sss0 0000 dddd d000 0000 1001", (Destination)(Source)),
    instr!(JalrR31, "0000 00ss sss0 0000 1111 1000 0000 1001", (Source)),
    instr!(Jr,      "0000 00ss sss0 0000 0000 0000 0000 1000", (Source)),
    instr!(Lb,      "1000 00bb bbbt tttt ffff ffff ffff ffff", (Target)(Offset, Signed16)(Base)),
    instr!(Lbu,     "1001 00bb bbbt tttt ffff ffff ffff ffff", (Target)(Offset, Signed16)(Base)),
    instr!(Ld,      "1101 11bb bbbt tttt ffff ffff ffff ffff", (Target)(Offset, Signed16)(Base)),
    instr!(Ldcz,    "1101 xxbb bbbt tttt ffff ffff ffff ffff", (Coprocessor)(Target)(Offset, Signed16)(Base)),
    instr!(Ldl,     "0110 10bb bbbt tttt ffff ffff ffff ffff", (Target)(Offset, Signed16)(Base)),
    instr!(Ldr,     "0110 11bb bbbt tttt ffff ffff ffff ffff", (Target)(Offset, Signed16)(Base)),
    instr!(Lh,      "1000 01bb bbbt tttt ffff ffff ffff ffff", (Target)(Offset, Signed16)(Base)),
    instr!(Lhu,     "1001 01bb bbbt tttt ffff ffff ffff ffff", (Target)(Offset, Signed16)(Base)),
    instr!(Ll,      "1100 00bb bbbt tttt ffff ffff ffff ffff", (Target)(Offset, Signed16)(Base)),
    instr!(Lld,     "1101 00bb bbbt tttt ffff ffff ffff ffff", (Target)(Offset, Signed16)(Base)),
    instr!(Lui,     "0011 1100 000t tttt kkkk kkkk kkkk kkkk", (Target)(Immediate, Signed16)),
    instr!(Lw,      "1000 11bb bbbt tttt ffff ffff ffff ffff", (Target)(Offset, Signed16)(Base)),
    instr!(Lwcz,    "1100 xxbb bbbt tttt ffff ffff ffff ffff", (Coprocessor)(Target)(Offset, Signed16)(Base)),
    instr!(Lwl,     "1000 10bb bbbt tttt ffff ffff ffff ffff", (Target)(Offset, Signed16)(Base)),
    instr!(Lwr,     "1001 10bb bbbt tttt ffff ffff ffff ffff", (Target)(Offset, Signed16)(Base)),
    instr!(Lwu,     "1001 11bb bbbt tttt ffff ffff ffff ffff", (Target)(Offset, Signed16)(Base)),
    instr!(Mfc0,    "0100 0000 000t tttt dddd d000 0000 0000", (Target)(Destination)),
    instr!(Mfcz,    "0100 xx00 000t tttt dddd d000 0000 0000", (Coprocessor)(Target)(Destination)),
    instr!(Mfhi,    "0000 0000 0000 0000 dddd d000 0001 0000", (Destination)),
    instr!(Mflo,    "0000 0000 0000 0000 dddd d000 0001 0010", (Destination)),
    instr!(Mtc0,    "0100 0000 100t tttt dddd d000 0000 0000", (Target)(Destination)),
    instr!(Mtcz,    "0100 xx00 100t tttt dddd d000 0000 0000", (Coprocessor)(Destination)(Destination)),
    instr!(Mthi,    "0000 00ss sss0 0000 0000 0000 0001 0001", (Source)),
    instr!(Mtlo,    "0000 00ss sss0 0000 0000 0000 0001 0011", (Source)),
    instr!(Mult,    "0000 00ss ssst tttt 0000 0000 0001 1000", (Source)(Target)),
    instr!(Multu,   "0000 00ss ssst tttt 0000 0000 0001 1001", (Source)(Target)),
    instr!(Nor,     "0000 00ss ssst tttt dddd d000 0010 0111", (Destination)(Source)(Target)),
    instr!(Or,      "0000 00ss ssst tttt dddd d000 0010 0101", (Destination)(Source)(Target)),
    instr!(Ori,     "0011 01ss ssst tttt kkkk kkkk kkkk kkkk", (Target)(Source)(Immediate)),
    instr!(Sb,      "1010 00bb bbbt tttt ffff ffff ffff ffff", (Target)(Offset, Signed16)(Base)),
    instr!(Sc,      "1110 00bb bbbt tttt ffff ffff ffff ffff", (Target)(Offset, Signed16)(Base)),
    instr!(Scd,     "1111 00bb bbbt tttt ffff ffff ffff ffff", (Target)(Offset, Signed16)(Base)),
    instr!(Sd,      "1111 11bb bbbt tttt ffff ffff ffff ffff", (Target)(Offset, Signed16)(Base)),
    instr!(Sdcz,    "1111 xxbb bbbt tttt ffff ffff ffff ffff", (Coprocessor)(Target)(Offset, Signed16)(Base)),
    instr!(Sdl,     "1011 00bb bbbt tttt ffff ffff ffff ffff", (Target)(Offset, Signed16)(Base)),
    instr!(Sdr,     "1011 01bb bbbt tttt ffff ffff ffff ffff", (Target)(Offset, Signed16)(Base)),
    instr!(Sh,      "1010 01bb bbbt tttt ffff ffff ffff ffff", (Target)(Offset, Signed16)(Base)),
    instr!(Sll,     "0000 0000 000t tttt dddd dkkk kk00 0000", (Destination)(Target)(Immediate)),
    instr!(Sllv,    "0000 00ss ssst tttt dddd d000 0000 0100", (Destination)(Target)(Source)),
    instr!(Slt,     "0000 00ss ssst tttt dddd d000 0010 1010", (Destination)(Source)(Target)),
    instr!(Slti,    "0010 10ss ssst tttt kkkk kkkk kkkk kkkk", (Target)(Source)(Immediate)),
    instr!(Sltiu,   "0010 11ss ssst tttt kkkk kkkk kkkk kkkk", (Target)(Source)(Immediate)),
    instr!(Sltu,    "0000 00ss ssst tttt dddd d000 0010 1011", (Destination)(Source)(Target)),
    instr!(Sra,     "0000 0000 000t tttt dddd dkkk kk00 0011", (Destination)(Target)(Immediate)),
    instr!(Srav,    "0000 00ss ssst tttt dddd d000 0000 0111", (Destination)(Target)(Source)),
    instr!(Srl,     "0000 0000 000t tttt dddd dkkk kk00 0010", (Destination)(Target)(Immediate)),
    instr!(Srlv,    "0000 00ss ssst tttt dddd d000 0000 0110", (Destination)(Target)(Source)),
    instr!(Sub,     "0000 00ss ssst tttt dddd d000 0010 0010", (Destination)(Source)(Target)),
    instr!(Subu,    "0000 00ss ssst tttt dddd d000 0010 0011", (Destination)(Source)(Target)),
    instr!(Sw,      "1010 11bb bbbt tttt ffff ffff ffff ffff", (Target)(Offset, Signed16)(Base)),
    instr!(Swcz,    "1110 xxbb bbbt tttt ffff ffff ffff ffff", (Coprocessor)(Target)(Offset, Signed16)(Base)),
    instr!(Swl,     "1010 10bb bbbt tttt ffff ffff ffff ffff", (Target)(Offset, Signed16)(Base)),
    instr!(Swr,     "1011 10bb bbbt tttt ffff ffff ffff ffff", (Target)(Offset, Signed16)(Base)),
    instr!(Sync,    "0000 0000 0000 0000 0000 0000 0000 1111"),
    instr!(Syscall, "0000 00kk kkkk kkkk kkkk kkkk kk00 1100"),
    instr!(Teq,     "0000 00ss ssst tttt kkkk kkkk kk11 0100", (Source)(Target)),
    instr!(Teqi,    "0000 01ss sss0 1100 kkkk kkkk kkkk kkkk", (Source)(Immediate)),
    instr!(Tge,     "0000 00ss ssst tttt kkkk kkkk kk11 0000", (Source)(Target)),
    instr!(Tgei,    "0000 01ss sss0 1000 kkkk kkkk kkkk kkkk", (Source)(Immediate)),
    instr!(Tgeiu,   "0000 01ss sss0 1001 kkkk kkkk kkkk kkkk", (Source)(Immediate)),
    instr!(Tgeu,    "0000 00ss ssst tttt kkkk kkkk kk11 0001", (Source)(Target)),
    instr!(Tlbp,    "0100 0010 0000 0000 0000 0000 0000 1000"),
    instr!(Tlbr,    "0100 0010 0000 0000 0000 0000 0000 0001"),
    instr!(Tlbwi,   "0100 0010 0000 0000 0000 0000 0000 0010"),
    instr!(Tlbwr,   "0100 0010 0000 0000 0000 0000 0000 0110"),
    instr!(Tlt,     "0000 00ss ssst tttt kkkk kkkk kk11 0010", (Source)(Target)),
    instr!(Tlti,    "0000 01ss sss0 1010 kkkk kkkk kkkk kkkk", (Source)(Immediate)),
    instr!(Tltiu,   "0000 01ss sss0 1011 kkkk kkkk kkkk kkkk", (Source)(Immediate)),
    instr!(Tltu,    "0000 00ss ssst tttt kkkk kkkk kk11 0011", (Source)(Target)),
    instr!(Tne,     "0000 00ss ssst tttt kkkk kkkk kk11 0110", (Source)(Target)),
    instr!(Tnei,    "0000 01ss sss0 1110 kkkk kkkk kkkk kkkk", (Source)(Immediate)),
    instr!(Xor,     "0000 00ss ssst tttt dddd d000 0010 0110", (Destination)(Source)(Target)),
    instr!(Xori,    "0011 10ss ssst tttt kkkk kkkk kkkk kkkk", (Target)(Source)(Immediate)),

    // Floating point
    instr!(AbsFmt,     "0100 01aa aaa0 0000 ssss sddd dd00 0101", (Destination)(Source)),
    instr!(AddFmt,     "0100 01aa aaat tttt ssss sddd dd00 0000", (Destination)(Source)),
    instr!(CCondFmtFs, "0100 01aa aaat tttt ssss s000 0011 cccc", (Source)(Target)(Condition)),
    instr!(Bc1f,       "0100 0101 0000 0000 ffff ffff ffff ffff", (Offset)),
    instr!(Bc1fl,      "0100 0101 0000 0010 ffff ffff ffff ffff", (Offset)),
    instr!(Bc1t,       "0100 0101 0000 0001 ffff ffff ffff ffff", (Offset)),
    instr!(Bc1tl,      "0100 0101 0000 0011 ffff ffff ffff ffff", (Offset)),
];

fn decode(instr: u32) -> Option<&'static Instruction> {
    // This is inefficient, if it proves to be a bottleneck we can optimise it in a few ways:
    // - Eliminate matching ranges if they are found not to match.
    // - Cache the result of `bit_range` on the input, to be reused for matching ranges.
    INSTRUCTIONS.iter().find(|i| i.pattern.matches(instr))
}
