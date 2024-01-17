use crate::{
    codegen::CodeGen,
    runtime::{
        bus::Bus, Environment, GdbIntegration, InterruptHandler, RuntimeFunction,
        TargetDependantCallbacks,
    },
    target, RegIndex,
};

pub use mips_decomp::register::rsp as register;
pub use registers::Registers;

mod registers;

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

    #[allow(clippy::cast_possible_truncation)]
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
                    register::GeneralPurpose::from_repr(instr.rt() as u8).unwrap(),
                );
                let result = codegen.builder.build_left_shift(target, shift, "sll_shift");
                codegen.write_register_raw(
                    register::GeneralPurpose::from_repr(instr.rd() as u8)
                        .unwrap()
                        .into(),
                    result,
                );
            }

            mips_decomp::instruction::Mnenomic::Ori => {
                // OR rs and zero-extended 16-bit immediate, store result in rt
                let immediate = i32_type.const_int(u64::from(instr.immediate()), false);
                let source = codegen.read_register_raw(
                    i32_type,
                    register::GeneralPurpose::from_repr(instr.rs() as u8).unwrap(),
                );

                let result = codegen.builder.build_or(source, immediate, "ori_res");
                codegen.write_register_raw(
                    register::GeneralPurpose::from_repr(instr.rt() as u8)
                        .unwrap()
                        .into(),
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

        self.registers.increment_pc(4);

        let status = self.registers.status().with_broke(true).with_halted(true);
        self.registers.set_status(status);

        while self.registers.status().halted() {
            std::thread::sleep(std::time::Duration::from_millis(100));
        }

        self.get_function_ptr(self.registers.read(register::Special::ProgramCounter))
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
