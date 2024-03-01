//! The guest machine code target interface, describing registers, memory, and instructions.
#![warn(clippy::all, clippy::pedantic)]

use crate::{
    codegen::{CodeGen, CompilationResult},
    runtime::{
        bus::PhysicalAddress,
        memory::tlb::{AccessMode, TranslationError},
    },
};
use inkwell::{execution_engine::ExecutionEngine, module::Module, values::PointerValue};
use std::fmt;

/// Helper for implementing the `RegIndex` trait for a target.
macro_rules! impl_reg_index {
    ($target:path, $(($ty:ty, $output:ty, $field:tt)),*) => {
        $(
            impl crate::runtime::register_bank::RegIndex<$ty> for $target {
                type Output = $output;

                fn read(&self, index: $ty) -> Self::Output {
                    self.$field.read_relaxed(index.into()).expect("register read failed")
                }

                fn write(&mut self, index: $ty, value: Self::Output) {
                    self.$field.write_relaxed(index.into(), value).expect("register write failed");
                }
            }
        )*
    };
}

macro_rules! impl_reg_bank_wrapper {
    ($name:ident, $id:ty, $inner:ty, $len:expr, $doc:expr) => {
        #[doc = $doc]
        #[derive(Debug, Default)]
        #[repr(transparent)]
        pub struct $name(pub RegisterBank<$inner, $len>);

        impl $name {
            pub fn share(&self) -> Option<Self> {
                self.0.share().map(Self)
            }

            #[must_use]
            pub fn read_relaxed(&self, index: usize) -> Option<$inner> {
                self.0.read_relaxed(index)
            }

            #[must_use]
            pub fn write_relaxed(&self, index: usize, value: $inner) -> Option<()> {
                self.0.write_relaxed(index, value)
            }
        }

        impl From<RegisterBank<$inner, $len>> for $name {
            fn from(inner: RegisterBank<$inner, $len>) -> Self {
                Self(inner)
            }
        }

        impl_reg_index!($name, ($id, $inner, 0));
    };
}

pub mod cpu;
pub mod rsp;

pub(crate) trait RegisterID: Sized + fmt::Debug {
    const PROGRAM_COUNTER: Self;

    fn name(&self) -> &'static str;
}

pub(crate) trait RegisterStorage: fmt::Debug {
    type RegisterID: RegisterID;
    type Globals<'ctx>: Globals<'ctx, RegisterID = Self::RegisterID>;

    fn read_program_counter(&self) -> u64;

    fn build_globals<'ctx>(
        &self,
        module: &Module<'ctx>,
        exec_engine: &ExecutionEngine<'ctx>,
    ) -> Self::Globals<'ctx>;
}

pub(crate) trait Globals<'ctx>: fmt::Debug {
    type RegisterID: RegisterID;

    fn pointer_value<T: Target>(
        &self,
        codegen: &CodeGen<'ctx, T>,
        reg: &Self::RegisterID,
    ) -> PointerValue<'ctx>;

    fn is_atomic(&self, reg: Self::RegisterID) -> bool;
}

pub(crate) trait Memory: Default {
    type Registers: RegisterStorage;

    fn virtual_to_physical_address(
        &self,
        vaddr: u64,
        _access_mode: AccessMode,
        _registers: &Self::Registers,
    ) -> Result<PhysicalAddress, TranslationError> {
        #[allow(clippy::cast_possible_truncation)]
        Ok(vaddr as PhysicalAddress)
    }

    fn physical_to_virtual_address(
        &self,
        paddr: PhysicalAddress,
        _access_mode: AccessMode,
        _registers: &Self::Registers,
    ) -> Result<u64, TranslationError> {
        Ok(u64::from(paddr))
    }
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
    const NAME: &'static str;

    type Registers: RegisterStorage;
    type Memory: Memory<Registers = Self::Registers>;

    type Instruction: Instruction;
    type Label: Label<Instruction = Self::Instruction>;
    type LabelList: LabelList<Label = Self::Label>;

    fn compile_instruction(
        codegen: &CodeGen<Self>,
        instr: &Self::Instruction,
    ) -> CompilationResult<()>;

    fn compile_instruction_with_delay_slot(
        codegen: &CodeGen<Self>,
        pc: u64,
        instr: &Self::Instruction,
        delay_slot_instr: &Self::Instruction,
        on_instruction: impl Fn(u64) -> CompilationResult<()>,
    ) -> CompilationResult<()>;
}
