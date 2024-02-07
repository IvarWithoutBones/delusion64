use crate::{
    codegen::{CodeGen, CompilationResult},
    runtime::{
        bus::PhysicalAddress,
        memory::tlb::{AccessMode, TranslationError, TranslationLookasideBuffer},
    },
    target,
};
use mips_decomp::instruction::ParsedInstruction;
use std::fmt;

mod codegen;
pub(crate) mod gdb; // TODO make private
mod recompiler;
mod registers;
mod runtime;

pub use mips_decomp::register::cpu as register;
pub use registers::Registers;

#[derive(Default)]
#[repr(transparent)]
pub(crate) struct Memory {
    pub tlb: TranslationLookasideBuffer,
}

impl target::Memory for Memory {
    type Registers = Registers;

    fn virtual_to_physical_address(
        &self,
        vaddr: u64,
        access_mode: AccessMode,
        registers: &Self::Registers,
    ) -> Result<PhysicalAddress, TranslationError> {
        self.tlb.translate_vaddr(vaddr, access_mode, registers)
    }

    fn physical_to_virtual_address(
        &self,
        paddr: PhysicalAddress,
        _access_mode: AccessMode,
        _registers: &Self::Registers,
    ) -> Result<u64, TranslationError> {
        self.tlb.translate_paddr(paddr)
    }
}

/// A single MIPS instruction.
#[derive(Debug, Clone)]
#[repr(transparent)]
pub(crate) struct Instruction(pub ParsedInstruction);

impl target::Instruction for Instruction {
    fn has_delay_slot(&self) -> bool {
        self.0.has_delay_slot()
    }
}

impl TryFrom<u32> for Instruction {
    type Error = ();

    fn try_from(value: u32) -> Result<Self, Self::Error> {
        value.try_into().map(Self)
    }
}

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl From<ParsedInstruction> for Instruction {
    fn from(value: ParsedInstruction) -> Self {
        Self(value)
    }
}

/// A block of instructions that can be jumped to.
#[derive(Debug, Clone)]
#[repr(transparent)]
pub(crate) struct Label(pub mips_decomp::Label);

impl target::Label for Label {
    type Instruction = Instruction;

    fn instructions(&self) -> impl DoubleEndedIterator<Item = Self::Instruction> + '_ {
        self.0.instructions.iter().map(|i| Instruction(i.clone()))
    }

    fn len(&self) -> usize {
        self.0.len()
    }

    fn start(&self) -> u64 {
        self.0.start() as u64
    }

    fn end(&self) -> u64 {
        self.0.end() as u64
    }

    fn fallthrough_offset(&self) -> Option<usize> {
        self.0.fallthrough_offset
    }
}

#[derive(Debug)]
#[repr(transparent)]
pub(crate) struct LabelList(pub mips_decomp::LabelList);

impl target::LabelList for LabelList {
    type Label = Label;

    fn from_iter(iter: impl IntoIterator<Item = PhysicalAddress>) -> Option<Self> {
        mips_decomp::read_labels(1, &mut iter.into_iter()).map(Self)
    }

    fn iter(&self) -> impl DoubleEndedIterator<Item = Self::Label> + '_ {
        self.0.iter().map(|l| Label(l.clone()))
    }

    #[allow(clippy::cast_possible_truncation)]
    fn set_start(&mut self, start: u64) {
        self.0.set_start(start as usize);
    }

    #[allow(clippy::cast_possible_truncation)]
    fn get_label(&self, vaddr: u64) -> Option<Self::Label> {
        self.0.get(vaddr as usize).map(|l| Label(l.clone()))
    }
}

#[derive(Debug, Default)]
#[repr(transparent)]
pub struct Cpu {
    pub(crate) interrupt_pending: bool,
}

impl target::Target for Cpu {
    type Registers = Registers;
    type Memory = Memory;

    type Instruction = Instruction;
    type Label = Label;
    type LabelList = LabelList;

    fn compile_instruction(
        codegen: &CodeGen<Self>,
        instr: &Self::Instruction,
    ) -> CompilationResult<()> {
        recompiler::compile_instruction(codegen, &instr.0)
    }

    fn compile_instruction_with_delay_slot(
        codegen: &CodeGen<Self>,
        pc: u64,
        instr: &Self::Instruction,
        delay_slot_instr: &Self::Instruction,
        on_instruction: impl Fn(u64) -> CompilationResult<()>,
    ) -> CompilationResult<()> {
        recompiler::compile_instruction_with_delay_slot(
            codegen,
            pc,
            &instr.0,
            &delay_slot_instr.0,
            on_instruction,
        )
    }
}
