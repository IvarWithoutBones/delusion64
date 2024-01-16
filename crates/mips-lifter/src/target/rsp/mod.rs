use crate::{
    codegen::CodeGen,
    runtime::{
        bus::{Bus, PhysicalAddress},
        memory::tlb,
        register_bank::RegisterBankMapping,
        Environment, GdbIntegration, InterruptHandler, RuntimeFunction, TargetDependantCallbacks,
    },
    RegisterBank,
};
use inkwell::{execution_engine::ExecutionEngine, module::Module, values::PointerValue};

#[allow(dead_code)]
#[derive(Debug)]
pub(crate) enum RspRegister {
    GeneralPurpose(mips_decomp::register::GeneralPurpose),
    Cp0(usize),
    Vector(usize),
    ProgramCounter,
}

#[derive(Debug, Default)]
pub struct Registers {
    pub general_purpose: RegisterBank<u32, 32>,
    pub cp0: RegisterBank<u32, 32>,
    // In reality these are 32 128-bit registers, but Rust does not have an AtomicU128 type.
    pub vector: RegisterBank<u64, 64>,
    pub special: RegisterBank<u64, 1>,
}

impl super::RegisterStorage for Registers {
    type Globals<'ctx> = RegisterGlobals<'ctx>;

    fn read_program_counter(&self) -> u64 {
        self.special.read_relaxed(0).unwrap()
    }

    fn build_globals<'ctx>(
        &self,
        module: &Module<'ctx>,
        exec_engine: &ExecutionEngine<'ctx>,
    ) -> Self::Globals<'ctx> {
        RegisterGlobals {
            general_purpose: self
                .general_purpose
                .map_into(module, exec_engine, "general_purpose"),
            cp0: self.cp0.map_into(module, exec_engine, "cp0"),
            vector: self.vector.map_into(module, exec_engine, "vector"),
            special: self.special.map_into(module, exec_engine, "special"),
        }
    }
}

#[derive(Debug)]
pub(crate) struct RegisterGlobals<'ctx> {
    general_purpose: RegisterBankMapping<'ctx>,
    cp0: RegisterBankMapping<'ctx>,
    vector: RegisterBankMapping<'ctx>,
    special: RegisterBankMapping<'ctx>,
}

impl<'ctx> super::Globals<'ctx> for RegisterGlobals<'ctx> {
    type RegisterID = RspRegister;

    const PROGRAM_COUNTER_ID: Self::RegisterID = RspRegister::ProgramCounter;

    fn pointer_value<T: super::Target>(
        &self,
        codegen: &CodeGen<'ctx, T>,
        index: &Self::RegisterID,
    ) -> PointerValue<'ctx> {
        let i32_type = codegen.context.i32_type();
        let i64_type = codegen.context.i64_type();
        match index {
            RspRegister::GeneralPurpose(r) => unsafe {
                codegen.builder.build_in_bounds_gep(
                    i32_type,
                    self.general_purpose.pointer_value(),
                    &[i32_type.const_int(r.to_repr() as u64, false)],
                    &format!("{}_", r.name()),
                )
            },
            RspRegister::Cp0(i) => unsafe {
                codegen.builder.build_in_bounds_gep(
                    i32_type,
                    self.cp0.pointer_value(),
                    &[i32_type.const_int(*i as u64, false)],
                    &format!("cp0_{i}_"),
                )
            },
            RspRegister::Vector(i) => unsafe {
                codegen.builder.build_in_bounds_gep(
                    i64_type,
                    self.vector.pointer_value(),
                    &[i64_type.const_int(*i as u64, false)],
                    &format!("vec_{i}_"),
                )
            },
            RspRegister::ProgramCounter => self.special.pointer_value(),
        }
    }

    fn is_atomic(&self, reg: Self::RegisterID) -> bool {
        match reg {
            RspRegister::GeneralPurpose(_) => self.general_purpose.is_atomic(),
            RspRegister::Cp0(_) => self.cp0.is_atomic(),
            RspRegister::Vector(_) => self.vector.is_atomic(),
            RspRegister::ProgramCounter => self.special.is_atomic(),
        }
    }
}

#[derive(Default)]
pub struct Memory;

impl super::Memory for Memory {
    type Registers = Registers;

    #[allow(clippy::cast_possible_truncation)]
    fn virtual_to_physical_address(
        &self,
        vaddr: u64,
        _access_mode: tlb::AccessMode,
        _registers: &Self::Registers,
    ) -> Result<PhysicalAddress, tlb::TranslationError> {
        Ok(vaddr as u32)
    }

    fn physical_to_virtual_address(
        &self,
        paddr: PhysicalAddress,
        _access_mode: tlb::AccessMode,
        _registers: &Self::Registers,
    ) -> Result<u64, tlb::TranslationError> {
        Ok(u64::from(paddr))
    }
}

#[derive(Default, Debug)]
pub struct Rsp;

impl super::Target for Rsp {
    type Registers = Registers;
    type Memory = Memory;

    type Instruction = super::cpu::Instruction;
    type Label = super::cpu::Label;
    type LabelList = super::cpu::LabelList;

    fn compile_instruction(codegen: &CodeGen<Self>, instr: &Self::Instruction) -> Option<()> {
        let i32_type = codegen.context.i32_type();

        let instr = &instr.0;
        match instr.mnemonic() {
            mips_decomp::instruction::Mnenomic::Break => {
                codegen.throw_exception(mips_decomp::Exception::Breakpoint, None, None);
            }

            mips_decomp::instruction::Mnenomic::Sll => {
                // Shift rt left by sa bits, store result in rd
                let shift = i32_type.const_int(u64::from(instr.sa()), false);
                let target = codegen.read_register_raw(
                    i32_type,
                    RspRegister::GeneralPurpose(instr.rt().try_into().unwrap()),
                );
                let result = codegen.builder.build_left_shift(target, shift, "sll_shift");
                codegen.write_register_raw(
                    RspRegister::GeneralPurpose(instr.rd().try_into().unwrap()),
                    result,
                );
            }

            mips_decomp::instruction::Mnenomic::Ori => {
                // OR rs and zero-extended 16-bit immediate, store result in rt
                let immediate = i32_type.const_int(u64::from(instr.immediate()), false);
                let source = codegen.read_register_raw(
                    i32_type,
                    RspRegister::GeneralPurpose(instr.rs().try_into().unwrap()),
                );

                let result = codegen.builder.build_or(source, immediate, "ori_res");
                codegen.write_register_raw(
                    RspRegister::GeneralPurpose(instr.rt().try_into().unwrap()),
                    result,
                );
            }

            _ => todo!("RSP compile_instruction {instr}"),
        }
        Some(())
    }

    fn compile_instruction_with_delay_slot(
        _codegen: &CodeGen<Self>,
        _pc: u64,
        _instr: &Self::Instruction,
        _delay_slot_instr: &Self::Instruction,
        _on_instruction: impl Fn(u64),
    ) {
        todo!()
    }
}

impl<B: Bus> Environment<'_, Rsp, B> {
    unsafe extern "C" fn handle_exception_jit(
        &mut self,
        code: u64,
        has_coprocessor: bool,
        _coprocessor: u8,
        has_bad_vaddr: bool,
        _bad_vaddr: u64,
    ) -> usize {
        #[allow(clippy::cast_possible_truncation)]
        let code = mips_decomp::Exception::from_repr(code as u8).unwrap();
        assert_eq!(
            code,
            mips_decomp::Exception::Breakpoint,
            "RSP only supports breakpoint exception"
        );
        debug_assert!(!has_coprocessor);
        debug_assert!(!has_bad_vaddr);

        // set halted/broke
        let status = self.registers.cp0.read_relaxed(4).unwrap() | 0b11;
        self.registers.cp0.write_relaxed(4, status).unwrap();

        // increment pc
        let pc = self.registers.special.read_relaxed(0).unwrap() + 4;
        self.registers.special.write_relaxed(0, pc).unwrap();

        while (self.registers.cp0.read_relaxed(4).unwrap() & 0b1) != 0 {
            std::thread::sleep(std::time::Duration::from_millis(100));
        }

        self.get_function_ptr(self.registers.special.read_relaxed(0).unwrap())
    }
}

impl<B: Bus> TargetDependantCallbacks for Environment<'_, Rsp, B> {
    fn callback_ptr(&self, func: RuntimeFunction) -> *const u8 {
        match func {
            RuntimeFunction::HandleException => Self::handle_exception_jit as *const u8,
            _ => std::ptr::null(),
        }
    }

    fn on_block_entered(&mut self, _instructions_in_block: usize) -> usize {
        // TODO: mechanism for the CPU to mark blocks as dirty
        self.codegen.labels.remove_within_range(0..0x1000);
        0
    }
}

impl<B: Bus> InterruptHandler for Environment<'_, Rsp, B> {
    fn handle_interrupt(&mut self, interrupt_pending: u8) {
        unreachable!("RSP does not support interrupts (IP: {interrupt_pending:#b})")
    }
}

impl<B: Bus> GdbIntegration for Environment<'_, Rsp, B> {
    type Arch = super::cpu::gdb::GdbArch;

    fn gdb_read_register(
        &mut self,
        _reg_id: <<Self as GdbIntegration>::Arch as gdbstub::arch::Arch>::RegId,
        _buf: &mut [u8],
    ) -> gdbstub::target::TargetResult<usize, Self> {
        todo!("RSP GDB support")
    }

    fn gdb_write_register(
        &mut self,
        _reg_id: <<Self as GdbIntegration>::Arch as gdbstub::arch::Arch>::RegId,
        _value: &[u8],
    ) -> gdbstub::target::TargetResult<(), Self> {
        todo!("RSP GDB support")
    }

    fn gdb_read_registers(
        &mut self,
        _regs: &mut <<Self as gdbstub::target::Target>::Arch as gdbstub::arch::Arch>::Registers,
    ) -> gdbstub::target::TargetResult<(), Self> {
        todo!("RSP GDB support")
    }

    fn gdb_write_registers(
        &mut self,
        _regs: &<<Self as gdbstub::target::Target>::Arch as gdbstub::arch::Arch>::Registers,
    ) -> gdbstub::target::TargetResult<(), Self> {
        todo!("RSP GDB support")
    }
}
