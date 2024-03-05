use super::Rsp;
use crate::{
    runtime::{bus::Bus, Environment, InterruptHandler, RuntimeFunction, TargetDependantCallbacks},
    target::RegisterStorage,
};
use mips_decomp::{register::rsp::control::Status, Exception, INSTRUCTION_SIZE};

impl<B: Bus> Environment<'_, Rsp, B> {
    unsafe extern "C" fn handle_exception_jit(
        &mut self,
        code: u64,
        has_coprocessor: bool,
        _coprocessor: u8,
        has_bad_vaddr: bool,
        _bad_vaddr: u64,
    ) -> usize {
        {
            // Ensure this is a breakpoint exception
            debug_assert_eq!(
                Exception::from_repr(code.try_into().unwrap()).unwrap(),
                Exception::Breakpoint
            );
            debug_assert!(!has_coprocessor);
            debug_assert!(!has_bad_vaddr);
        }

        self.registers
            .special
            .increment_program_counter(INSTRUCTION_SIZE.try_into().unwrap());

        let status = self
            .registers
            .control
            .read_parsed::<Status>()
            .with_broke(true)
            .with_halted(true);
        self.registers.control.write_parsed(status);

        while self.registers.control.read_parsed::<Status>().halted() {
            std::thread::sleep(std::time::Duration::from_millis(100));
        }

        self.get_function_ptr(self.registers.read_program_counter())
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
        // TODO: mechanism for the CPU to mark blocks as dirty, this breaks the `llvm-ir` GDB monitor command
        // self.codegen.labels.remove_within_range(0..0x1000);
        0
    }
}

impl<B: Bus> InterruptHandler for Environment<'_, Rsp, B> {
    fn handle_interrupt(&mut self, interrupt_pending: u8) {
        unreachable!("RSP does not support interrupts (IP: {interrupt_pending:#b})")
    }
}
