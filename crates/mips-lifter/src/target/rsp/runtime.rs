use super::{register, Rsp};
use crate::{
    runtime::{bus::Bus, Environment, InterruptHandler, RuntimeFunction, TargetDependantCallbacks},
    RegIndex,
};
use mips_decomp::Exception;

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
            #[allow(clippy::cast_possible_truncation)]
            let code = Exception::from_repr(code as u8).unwrap();
            assert_eq!(code, Exception::Breakpoint);
            debug_assert!(!has_coprocessor);
            debug_assert!(!has_bad_vaddr);
        }

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
