use super::operand::{Operand, OperandFormatInfo, Signedness};
use strum::{EnumVariantNames, VariantNames};

/// An instruction mnemonic.
#[derive(EnumVariantNames, Debug, Clone, Copy, PartialEq, Eq)]
#[strum(serialize_all = "lowercase")]
pub enum Mnemonic {
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
    Cfc1,
    Cfc2,
    Cop2,
    Ctc1,
    Ctc2,
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
    Ldc1,
    Ldl,
    Ldr,
    Lh,
    Lhu,
    Ll,
    Lld,
    Lui,
    Lw,
    Lwc1,
    Lwc2,
    Lwl,
    Lwr,
    Lwu,
    Mfc0,
    Mfc2,
    Mfhi,
    Mflo,
    Mtc0,
    Mtc2,
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
    Sdc1,
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
    Swc1,
    Swc2,
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

    // Floating point unit
    #[strum(serialize = "abs.fmt")]
    AbsFmt,
    #[strum(serialize = "add.fmt")]
    AddFmt,
    Bc1f,
    Bc1fl,
    Bc1t,
    Bc1tl,
    #[strum(serialize = "c.cond.fmt")]
    CCondFmtFs,
    #[strum(serialize = "ceil.l.fmt")]
    CeilLFmt,
    #[strum(serialize = "ceil.w.fmt")]
    CeilWFmt,
    #[strum(serialize = "cvt.d.fmt")]
    CvtDFmt,
    #[strum(serialize = "cvt.l.fmt")]
    CvtLFmt,
    #[strum(serialize = "cvt.s.fmt")]
    CvtSFmt,
    #[strum(serialize = "cvt.w.fmt")]
    CvtWFmt,
    #[strum(serialize = "div.fmt")]
    DivFmt,
    #[strum(serialize = "floor.l.fmt")]
    FloorLFmt,
    #[strum(serialize = "floor.w.fmt")]
    FloorWFmt,
    #[strum(serialize = "mov.fmt")]
    MovFmt,
    #[strum(serialize = "mul.fmt")]
    MulFmt,
    #[strum(serialize = "neg.fmt")]
    NegFmt,
    #[strum(serialize = "round.l.fmt")]
    RoundLFmt,
    #[strum(serialize = "round.w.fmt")]
    RoundWFmt,
    #[strum(serialize = "sqrt.fmt")]
    SqrtFmt,
    #[strum(serialize = "sub.fmt")]
    SubFmt,
    #[strum(serialize = "trunc.l.fmt")]
    TruncLFmt,
    #[strum(serialize = "trunc.w.fmt")]
    TruncWFmt,
}

impl Mnemonic {
    pub const fn name(&self) -> &'static str {
        Self::VARIANTS[*self as usize]
    }

    pub const fn is_branch(&self) -> bool {
        matches!(
            self,
            Mnemonic::Bczf
                | Mnemonic::Bczfl
                | Mnemonic::Bczt
                | Mnemonic::Bcztl
                | Mnemonic::Beq
                | Mnemonic::Beql
                | Mnemonic::Bgez
                | Mnemonic::Bgezal
                | Mnemonic::Bgezall
                | Mnemonic::Bgezl
                | Mnemonic::Bgtz
                | Mnemonic::Bgtzl
                | Mnemonic::Blez
                | Mnemonic::Blezl
                | Mnemonic::Bltz
                | Mnemonic::Bltzal
                | Mnemonic::Bltzall
                | Mnemonic::Bltzl
                | Mnemonic::Bne
                | Mnemonic::Bnel
        )
    }

    pub const fn ends_block(&self) -> bool {
        self.is_branch()
            || matches!(self, |Mnemonic::Break| Mnemonic::Eret
                | Mnemonic::J
                | Mnemonic::Jal
                | Mnemonic::Jalr
                | Mnemonic::JalrR31
                | Mnemonic::Jr
                | Mnemonic::Syscall
                | Mnemonic::Teq
                | Mnemonic::Teqi
                | Mnemonic::Tge
                | Mnemonic::Tgei
                | Mnemonic::Tgeiu
                | Mnemonic::Tgeu)
    }

    pub const fn discards_delay_slot(&self) -> bool {
        matches!(
            self,
            Mnemonic::Bczfl
                | Mnemonic::Bcztl
                | Mnemonic::Beql
                | Mnemonic::Bgezall
                | Mnemonic::Bgezl
                | Mnemonic::Bgtzl
                | Mnemonic::Blezl
                | Mnemonic::Bltzall
                | Mnemonic::Bltzl
                | Mnemonic::Bnel
        )
    }

    pub const fn has_delay_slot(&self) -> bool {
        self.ends_block()
    }

    /// Returns the operands for this instruction, in the order they appear when written in assembly.
    #[rustfmt::skip]
    pub const fn format_info(&self) -> OperandFormatInfo {
        use Mnemonic::*;
        use Operand::*;
        use Signedness::*;

        macro_rules! operand {
            ($name:ident, $sign:ident) => {
                ($name, $sign)
            };

            ($name:ident) => {
                ($name, Signed32)
            };
        }

        macro_rules! operands {
            [$($name:tt),*] => {
                &[
                    $(operand! $name),*
                ]
            };
        }

        // This is really begging for a derive macro, but im not sure its worth it just for this...
        match self {
            Add        => operands![(Destination), (Source), (Target)],
            Addi       => operands![(Target), (Source), (Immediate, Signed16)],
            Addiu      => operands![(Target), (Source), (Immediate, Signed16)],
            Addu       => operands![(Destination), (Source), (Target)],
            And        => operands![(Destination), (Source), (Target)],
            Andi       => operands![(Target), (Source), (Immediate)],
            // TODO: Skipping CPZ likely branches, need to split coprocessor versions
            Beq        => operands![(Source), (Target), (Offset, Signed16)],
            Beql       => operands![(Source), (Target), (Offset, Signed16)],
            Bgez       => operands![(Source), (Offset, Signed16)],
            Bgezal     => operands![(Source), (Offset, Signed16)],
            Bgezall    => operands![(Source), (Offset, Signed16)],
            Bgezl      => operands![(Source), (Offset, Signed16)],
            Bgtz       => operands![(Source), (Offset, Signed16)],
            Bgtzl      => operands![(Source), (Offset, Signed16)],
            Blez       => operands![(Source), (Offset, Signed16)],
            Blezl      => operands![(Source), (Offset, Signed16)],
            Bltz       => operands![(Source), (Offset, Signed16)],
            Bltzal     => operands![(Source), (Offset, Signed16)],
            Bltzall    => operands![(Source), (Offset, Signed16)],
            Bltzl      => operands![(Source), (Offset, Signed16)],
            Bne        => operands![(Source), (Target), (Offset, Signed16)],
            Bnel       => operands![(Source), (Target), (Offset, Signed16)],
            Break      => operands![],
            Cache      => operands![(CacheOpcode), (CacheSubject), (Offset, Signed16), (Base)],
            Cfc1       => operands![(Target), (Destination)],
            Cfc2       => operands![(Target), (Destination)],
            Cop2       => operands![(Immediate)],
            Ctc1       => operands![(Target), (Destination)],
            Ctc2       => operands![(Target), (Destination)],
            Dadd       => operands![(Destination), (Source), (Target)],
            Daddi      => operands![(Target), (Source), (Immediate, Signed16)],
            Daddiu     => operands![(Target), (Source), (Immediate, Signed16)],
            Daddu      => operands![(Destination), (Source), (Target)],
            Ddiv       => operands![(Source), (Target)],
            Ddivu      => operands![(Source), (Target)],
            Div        => operands![(Source), (Target)],
            Divu       => operands![(Source), (Target)],
            Dmfc0      => operands![(Target), (Destination)],
            Dmtc0      => operands![(Target), (Destination)],
            Dmult      => operands![(Source), (Target)],
            Dmultu     => operands![(Source), (Target)],
            Dsll       => operands![(Destination), (Target), (Immediate)],
            Dsllv      => operands![(Destination), (Target), (Source)],
            Dsll32     => operands![(Destination), (Target), (Immediate)],
            Dsra       => operands![(Destination), (Target), (Immediate)],
            Dsrav      => operands![(Destination), (Target), (Source)],
            Dsra32     => operands![(Destination), (Target), (Immediate)],
            Dsrl       => operands![(Destination), (Target), (Immediate)],
            Dsrlv      => operands![(Destination), (Target), (Source)],
            Dsrl32     => operands![(Destination), (Target), (Immediate)],
            Dsub       => operands![(Destination), (Source), (Target)],
            Dsubu      => operands![(Destination), (Source), (Target)],
            Eret       => operands![],
            J          => operands![(Immediate)],
            Jal        => operands![(Immediate)],
            Jalr       => operands![(Destination), (Source)],
            JalrR31    => operands![(Source)],
            Jr         => operands![(Source)],
            Lb         => operands![(Target), (Offset, Signed16), (Base)],
            Lbu        => operands![(Target), (Offset, Signed16), (Base)],
            Ld         => operands![(Target), (Offset, Signed16), (Base)],
            Ldc1       => operands![(Target), (Offset, Signed16), (Base)],
            Ldl        => operands![(Target), (Offset, Signed16), (Base)],
            Ldr        => operands![(Target), (Offset, Signed16), (Base)],
            Lh         => operands![(Target), (Offset, Signed16), (Base)],
            Lhu        => operands![(Target), (Offset, Signed16), (Base)],
            Ll         => operands![(Target), (Offset, Signed16), (Base)],
            Lld        => operands![(Target), (Offset, Signed16), (Base)],
            Lui        => operands![(Target), (Immediate, Signed16)],
            Lw         => operands![(Target), (Offset, Signed16), (Base)],
            Lwc1       => operands![(Target), (Offset, Signed16), (Base)],
            Lwc2       => operands![(Target), (Offset, Signed16), (Base)],
            Lwl        => operands![(Target), (Offset, Signed16), (Base)],
            Lwr        => operands![(Target), (Offset, Signed16), (Base)],
            Lwu        => operands![(Target), (Offset, Signed16), (Base)],
            Mfc0       => operands![(Target), (Destination)],
            Mfc2       => operands![(Target), (Destination)],
            Mfhi       => operands![(Destination)],
            Mflo       => operands![(Destination)],
            Mtc0       => operands![(Target), (Destination)],
            Mtc2       => operands![(Target), (Destination)],
            Mthi       => operands![(Source)],
            Mtlo       => operands![(Source)],
            Mult       => operands![(Source), (Target)],
            Multu      => operands![(Source), (Target)],
            Nor        => operands![(Destination), (Source), (Target)],
            Or         => operands![(Destination), (Source), (Target)],
            Ori        => operands![(Target), (Source), (Immediate)],
            Sb         => operands![(Target), (Offset, Signed16), (Base)],
            Sc         => operands![(Target), (Offset, Signed16), (Base)],
            Scd        => operands![(Target), (Offset, Signed16), (Base)],
            Sd         => operands![(Target), (Offset, Signed16), (Base)],
            Sdc1       => operands![(Target), (Offset, Signed16), (Base)],
            Sdl        => operands![(Target), (Offset, Signed16), (Base)],
            Sdr        => operands![(Target), (Offset, Signed16), (Base)],
            Sh         => operands![(Target), (Offset, Signed16), (Base)],
            Sll        => operands![(Destination), (Target), (Immediate)],
            Sllv       => operands![(Destination), (Target), (Source)],
            Slt        => operands![(Destination), (Source), (Target)],
            Slti       => operands![(Target), (Source), (Immediate)],
            Sltiu      => operands![(Target), (Source), (Immediate)],
            Sltu       => operands![(Destination), (Source), (Target)],
            Sra        => operands![(Destination), (Target), (Immediate)],
            Srav       => operands![(Destination), (Target), (Source)],
            Srl        => operands![(Destination), (Target), (Immediate)],
            Srlv       => operands![(Destination), (Target), (Source)],
            Sub        => operands![(Destination), (Source), (Target)],
            Subu       => operands![(Destination), (Source), (Target)],
            Sw         => operands![(Target), (Offset, Signed16), (Base)],
            Swc1       => operands![(Target), (Offset, Signed16), (Base)],
            Swc2       => operands![(Target), (Offset, Signed16), (Base)],
            Swl        => operands![(Target), (Offset, Signed16), (Base)],
            Swr        => operands![(Target), (Offset, Signed16), (Base)],
            Sync       => operands![],
            Syscall    => operands![],
            Teq        => operands![(Source), (Target)],
            Teqi       => operands![(Source), (Immediate)],
            Tge        => operands![(Source), (Target)],
            Tgei       => operands![(Source), (Immediate)],
            Tgeiu      => operands![(Source), (Immediate)],
            Tgeu       => operands![(Source), (Target)],
            Tlbp       => operands![],
            Tlbr       => operands![],
            Tlbwi      => operands![],
            Tlbwr      => operands![],
            Tlt        => operands![(Source), (Target)],
            Tlti       => operands![(Source), (Immediate)],
            Tltiu      => operands![(Source), (Immediate)],
            Tltu       => operands![(Source), (Target)],
            Tne        => operands![(Source), (Target)],
            Tnei       => operands![(Source), (Immediate)],
            Xor        => operands![(Destination), (Source), (Target)],
            Xori       => operands![(Target), (Source), (Immediate)],

            // Floating point instructions
            AbsFmt     => operands![(FloatFormat), (FloatDestination), (FloatSource)],
            AddFmt     => operands![(FloatFormat), (FloatDestination), (FloatSource), (FloatTarget)],
            Bc1f       => operands![(Offset)],
            Bc1fl      => operands![(Offset)],
            Bc1t       => operands![(Offset)],
            Bc1tl      => operands![(Offset)],
            CCondFmtFs => operands![(FloatFormat), (FloatSource), (FloatTarget), (FloatCondition)],
            CeilLFmt   => operands![(FloatFormat), (FloatDestination), (FloatSource)],
            CeilWFmt   => operands![(FloatFormat), (FloatDestination), (FloatSource)],
            CvtDFmt    => operands![(FloatFormat), (FloatDestination), (FloatSource)],
            CvtLFmt    => operands![(FloatFormat), (FloatDestination), (FloatSource)],
            CvtSFmt    => operands![(FloatFormat), (FloatDestination), (FloatSource)],
            CvtWFmt    => operands![(FloatFormat), (FloatDestination), (FloatSource)],
            DivFmt     => operands![(FloatFormat), (FloatDestination), (FloatSource), (FloatTarget)],
            FloorLFmt  => operands![(FloatFormat), (FloatDestination), (FloatSource)],
            FloorWFmt  => operands![(FloatFormat), (FloatDestination), (FloatSource)],
            MovFmt     => operands![(FloatFormat), (FloatDestination), (FloatSource)],
            MulFmt     => operands![(FloatFormat), (FloatDestination), (FloatSource), (FloatTarget)],
            NegFmt     => operands![(FloatFormat), (FloatDestination), (FloatSource)],
            RoundLFmt  => operands![(FloatFormat), (FloatDestination), (FloatSource)],
            RoundWFmt  => operands![(FloatFormat), (FloatDestination), (FloatSource)],
            SqrtFmt    => operands![(FloatFormat), (FloatDestination), (FloatSource)],
            SubFmt     => operands![(FloatFormat), (FloatDestination), (FloatSource), (FloatTarget)],
            TruncLFmt  => operands![(FloatFormat), (FloatDestination), (FloatSource)],
            TruncWFmt  => operands![(FloatFormat), (FloatDestination), (FloatSource)],

            _ => todo!(),
        }
    }
}
