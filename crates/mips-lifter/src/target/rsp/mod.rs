use crate::{codegen::CodeGen, target};
pub use mips_decomp::register::rsp as register;
pub use registers::Registers;

mod codegen;
mod gdb;
mod recompiler;
mod registers;
mod runtime;

#[derive(Default)]
pub(crate) struct Memory;

impl target::Memory for Memory {
    type Registers = Registers;
}

#[derive(Default, Debug)]
pub struct Rsp;

impl target::Target for Rsp {
    type Registers = Registers;
    type Memory = Memory;

    type Instruction = super::cpu::Instruction;
    type Label = super::cpu::Label;
    type LabelList = super::cpu::LabelList;

    fn compile_instruction(codegen: &CodeGen<Self>, instr: &Self::Instruction) -> Option<()> {
        recompiler::compile_instruction(codegen, &instr.0)
    }

    fn compile_instruction_with_delay_slot(
        codegen: &CodeGen<Self>,
        pc: u64,
        instr: &Self::Instruction,
        delay_slot_instr: &Self::Instruction,
        on_instruction: impl Fn(u64),
    ) {
        recompiler::compile_instruction_with_delay_slot(
            codegen,
            pc,
            &instr.0,
            &delay_slot_instr.0,
            on_instruction,
        );
    }
}