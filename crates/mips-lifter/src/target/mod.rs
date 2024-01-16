//! The guest machine code target interface, describing registers, memory, and instructions.
#![warn(clippy::all, clippy::pedantic)]

use crate::{
    codegen::CodeGen,
    runtime::{
        bus::PhysicalAddress,
        memory::tlb::{AccessMode, TranslationError},
    },
};
use inkwell::{execution_engine::ExecutionEngine, module::Module, values::PointerValue};
use std::fmt;

pub(crate) mod cpu;
pub(crate) mod rsp;

pub use cpu::{Cpu, Registers as CpuRegisters};
pub use rsp::{Registers as RspRegisters, Rsp};

/// The register ID type for the given target.
pub(crate) type RegisterID<'ctx, T> =
    <<<T as Target>::Registers as RegisterStorage>::Globals<'ctx> as Globals<'ctx>>::RegisterID;

pub(crate) trait RegisterStorage: fmt::Debug {
    type Globals<'ctx>: Globals<'ctx>;

    fn read_program_counter(&self) -> u64;

    fn build_globals<'ctx>(
        &self,
        module: &Module<'ctx>,
        exec_engine: &ExecutionEngine<'ctx>,
    ) -> Self::Globals<'ctx>;
}

pub(crate) trait Globals<'ctx>: fmt::Debug {
    type RegisterID: fmt::Debug;

    const PROGRAM_COUNTER_ID: Self::RegisterID;

    fn pointer_value<T: Target>(
        &self,
        codegen: &CodeGen<'ctx, T>,
        index: &Self::RegisterID,
    ) -> PointerValue<'ctx>;

    fn is_atomic(&self, bank: Self::RegisterID) -> bool;
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

pub(crate) trait Instruction: fmt::Debug + fmt::Display + TryFrom<u32> + Clone {
    fn has_delay_slot(&self) -> bool;
}

pub(crate) trait Label: fmt::Debug + Clone {
    type Instruction: Instruction;

    fn instructions(&self) -> impl DoubleEndedIterator<Item = Self::Instruction> + '_;

    fn len(&self) -> usize;

    fn fallthrough_offset(&self) -> Option<usize>;

    fn start(&self) -> u64;

    fn end(&self) -> u64;

    fn range(&self) -> std::ops::Range<u64> {
        self.start()..self.end()
    }
}

pub(crate) trait LabelList: Sized + fmt::Debug {
    type Label: Label;

    fn from_iter(iter: impl IntoIterator<Item = PhysicalAddress>) -> Option<Self>;

    fn iter(&self) -> impl DoubleEndedIterator<Item = Self::Label> + '_;

    /// Get the label with its starting address corresponding to a given address, or None.
    fn get_label(&self, vaddr: u64) -> Option<Self::Label>;

    fn set_start(&mut self, vaddr: u64);
}

pub(crate) trait Target: Sized + Default + fmt::Debug {
    type Registers: RegisterStorage;
    type Memory: Memory<Registers = Self::Registers>;

    type Instruction: Instruction;
    type Label: Label<Instruction = Self::Instruction>;
    type LabelList: LabelList<Label = Self::Label>;

    fn compile_instruction(codegen: &CodeGen<Self>, instr: &Self::Instruction) -> Option<()>;

    fn compile_instruction_with_delay_slot(
        codegen: &CodeGen<Self>,
        pc: u64,
        instr: &Self::Instruction,
        delay_slot_instr: &Self::Instruction,
        on_instruction: impl Fn(u64),
    );
}
