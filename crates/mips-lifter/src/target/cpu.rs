use super::{Globals, RegisterStorage, Target};
use crate::{
    codegen::{CodeGen, RegisterGlobals},
    recompiler,
    runtime::{
        bus::PhysicalAddress,
        memory::tlb::{AccessMode, TranslationError},
        registers::Registers,
    },
};
use inkwell::{execution_engine::ExecutionEngine, module::Module, values::PointerValue};

impl RegisterStorage for Registers {
    type Globals<'ctx> = RegisterGlobals<'ctx>;

    fn read_program_counter(&self) -> u64 {
        self.special
            .read_relaxed(mips_decomp::register::Special::Pc.into())
            .expect("PC is in bounds")
    }

    fn build_globals<'ctx>(
        &self,
        module: &Module<'ctx>,
        exec: &ExecutionEngine<'ctx>,
    ) -> Self::Globals<'ctx> {
        RegisterGlobals {
            general_purpose: self.general_purpose.map_into(
                module,
                exec,
                "general_purpose_registers",
            ),
            cp0: self.cp0.map_into(module, exec, "coprocessor_0_registers"),
            fpu: self.fpu.map_into(module, exec, "floating_point_registers"),
            fpu_control: self.fpu_control.map_into(
                module,
                exec,
                "floating_point_control_registers",
            ),
            special: self.special.map_into(module, exec, "special_registers"),
        }
    }
}

impl<'ctx> Globals<'ctx> for RegisterGlobals<'ctx> {
    type Id = mips_decomp::register::Register;

    fn ptr_value<T: Target>(
        &self,
        codegen: &CodeGen<'ctx, T>,
        index: Self::Id,
    ) -> PointerValue<'ctx> {
        use mips_decomp::register::Register;
        let gep = |ptr| unsafe {
            let i64_type = codegen.context.i64_type();
            codegen.builder.build_in_bounds_gep(
                i64_type,
                ptr,
                &[i64_type.const_int(index.to_repr() as u64, false)],
                "gpr_todo_name",
            )
        };
        match index {
            Register::Cp0(_) => gep(self.cp0.as_pointer_value()),
            Register::Special(_) => gep(self.special.as_pointer_value()),
            Register::Fpu(_) => gep(self.fpu.as_pointer_value()),
            Register::FpuControl(_) => gep(self.fpu_control.as_pointer_value()),
            Register::GeneralPurpose(_) => gep(self.general_purpose.as_pointer_value()),
        }
    }

    fn program_counter_ptr<T: Target>(&self, codegen: &CodeGen<'ctx, T>) -> PointerValue<'ctx> {
        self.ptr_value(
            codegen,
            mips_decomp::register::Register::Special(mips_decomp::register::Special::Pc),
        )
    }
}

#[allow(clippy::module_name_repetitions)] // TODO
#[derive(Default)]
pub struct CpuMemory {
    pub tlb: crate::runtime::memory::tlb::TranslationLookasideBuffer,
}

impl super::Memory for CpuMemory {
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

#[derive(Debug)]
pub struct Cpu;

#[derive(Debug, Clone)]
pub(crate) struct Label(pub mips_decomp::Label);

#[derive(Debug, Clone)]
pub(crate) struct Instruction(pub mips_decomp::instruction::ParsedInstruction);

impl super::Instruction for Instruction {
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

impl std::fmt::Display for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

impl From<mips_decomp::instruction::ParsedInstruction> for Instruction {
    fn from(value: mips_decomp::instruction::ParsedInstruction) -> Self {
        Self(value)
    }
}

impl super::Label for Label {
    type Instruction = Instruction;

    fn instructions(&self) -> Box<dyn DoubleEndedIterator<Item = Self::Instruction> + '_> {
        Box::new(self.0.instructions.iter().map(|i| Instruction(i.clone())))
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
pub(crate) struct LabelList(pub mips_decomp::LabelList);

impl super::LabelList for LabelList {
    type Inner = Label;

    fn from_iter<I>(iter: I) -> Option<Self>
    where
        Self: Sized,
        I: IntoIterator<Item = PhysicalAddress>,
    {
        mips_decomp::read_labels(1, &mut iter.into_iter()).map(Self)
    }

    fn iter(&self) -> Box<dyn DoubleEndedIterator<Item = Self::Inner> + '_> {
        Box::new(self.0.iter().map(|l| Label(l.clone())))
    }

    #[allow(clippy::cast_possible_truncation)]
    fn set_start(&mut self, start: u64) {
        self.0.set_start(start as usize);
    }

    #[allow(clippy::cast_possible_truncation)]
    fn get_label(&self, vaddr: u64) -> Option<Self::Inner> {
        self.0.get(vaddr as usize).map(|l| Label(l.clone()))
    }
}

impl Target for Cpu {
    type Registers = Registers;
    type Memory = CpuMemory;

    type Instruction = Instruction;
    type Label = Label;
    type LabelList = LabelList;

    fn compile_instruction(codegen: &CodeGen<Self>, instr: &Self::Instruction) -> Option<()> {
        recompiler::cpu::compile_instruction(codegen, &instr.0)
    }

    fn compile_instruction_with_delay_slot(
        codegen: &CodeGen<Self>,
        pc: u64,
        instr: &Self::Instruction,
        delay_slot_instr: &Self::Instruction,
        on_instruction: impl Fn(u64),
    ) where
        Self: Sized,
    {
        recompiler::compile_instruction_with_delay_slot(
            codegen,
            pc,
            &instr.0,
            &delay_slot_instr.0,
            on_instruction,
        );
    }
}
