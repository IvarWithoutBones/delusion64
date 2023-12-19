//! The guest machine code target
#![allow(dead_code)]
#![warn(clippy::all, clippy::pedantic)]

use crate::{
    codegen::CodeGen,
    runtime::{
        bus::PhysicalAddress,
        memory::tlb::{AccessMode, TranslationError},
    },
};
use inkwell::{execution_engine::ExecutionEngine, module::Module, values::PointerValue};

pub(crate) mod cpu;

pub use cpu::Cpu;

pub(crate) trait RegisterStorage: std::fmt::Debug {
    type Globals<'ctx>: Globals<'ctx>;

    fn read_program_counter(&self) -> u64;

    fn build_globals<'ctx>(
        &self,
        module: &Module<'ctx>,
        exec_engine: &ExecutionEngine<'ctx>,
    ) -> Self::Globals<'ctx>;
}

pub(crate) trait Globals<'ctx>: std::fmt::Debug {
    type Id;

    fn ptr_value<T: Target>(
        &self,
        codegen: &CodeGen<'ctx, T>,
        index: Self::Id,
    ) -> PointerValue<'ctx>;

    fn program_counter_ptr<T: Target>(&self, codegen: &CodeGen<'ctx, T>) -> PointerValue<'ctx>;
}

pub(crate) trait Memory: Default {
    type Registers: RegisterStorage;

    fn virtual_to_physical_address(
        &self,
        vaddr: u64,
        access_mode: AccessMode,
        registers: &Self::Registers,
    ) -> Result<PhysicalAddress, TranslationError>;

    fn physical_to_virtual_address(
        &self,
        paddr: PhysicalAddress,
        access_mode: AccessMode,
        registers: &Self::Registers,
    ) -> Result<u64, TranslationError>;
}

pub(crate) trait Instruction:
    std::fmt::Debug + std::fmt::Display + TryFrom<u32> + Clone
{
    fn has_delay_slot(&self) -> bool;
}

pub(crate) trait Label: std::fmt::Debug + Clone {
    type Instruction: Instruction;

    // TODO: replace with return position impl trait when updating the compiler
    fn instructions(&self) -> Box<dyn DoubleEndedIterator<Item = Self::Instruction> + '_>;

    fn len(&self) -> usize;

    fn fallthrough_offset(&self) -> Option<usize>;

    fn start(&self) -> u64;

    fn end(&self) -> u64;

    fn range(&self) -> std::ops::Range<u64> {
        self.start()..self.end()
    }
}

pub(crate) trait LabelList: std::fmt::Debug {
    type Inner: Label;

    fn from_iter<I>(iter: I) -> Option<Self>
    where
        Self: Sized,
        I: IntoIterator<Item = PhysicalAddress>;

    // TODO: replace with return position impl trait when updating the compiler
    fn iter(&self) -> Box<dyn DoubleEndedIterator<Item = Self::Inner> + '_>;

    /// Get the label with its starting address corresponding to a given address, or None.
    fn get_label(&self, vaddr: u64) -> Option<Self::Inner>;

    fn set_start(&mut self, vaddr: u64);
}

pub(crate) trait Target: std::fmt::Debug {
    // TODO: Is it possible bind Self::Registers::Globals::Target: Self, without specifying the lifetime 'ctx for Globals?
    type Registers: RegisterStorage;
    type Memory: Memory<Registers = Self::Registers>;

    type Instruction: Instruction;
    type Label: Label<Instruction = Self::Instruction>;
    type LabelList: LabelList<Inner = Self::Label>;

    fn compile_instruction(codegen: &CodeGen<Self>, instr: &Self::Instruction) -> Option<()>
    where
        Self: Sized;

    // TODO: generic func to unify this
    fn compile_instruction_with_delay_slot(
        codegen: &CodeGen<Self>,
        pc: u64,
        instr: &Self::Instruction,
        delay_slot_instr: &Self::Instruction,
        on_instruction: impl Fn(u64),
    ) where
        Self: Sized;
}
