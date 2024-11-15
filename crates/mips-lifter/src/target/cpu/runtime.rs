use super::Cpu;
use crate::runtime::{
    bus::Bus, memory::tlb, register_bank::RegIndex, Environment, InterruptHandler, RuntimeFunction,
    TargetDependantCallbacks,
};
use mips_decomp::{
    register::{self, cpu::cp0::Bits},
    Exception, INSTRUCTION_SIZE,
};

// TODO: move to different file
impl<B: Bus> Environment<'_, Cpu, B> {
    #[allow(clippy::cast_possible_truncation)] // The relevant part of the registers we read is in the lower 32 bits.
    fn update_count(&mut self, instructions: usize) {
        // Trigger an timer interrupt if it would happen anywhere in this block.
        // We're doing this ahead of time to avoid trapping into the runtime environment to check on every instruction.
        let old_count = self.registers.read(register::cpu::Cp0::Count) as u32;
        let compare = self.registers.read(register::cpu::Cp0::Compare) as u32;
        let new_count = old_count.wrapping_add(8 * instructions as u32);
        self.registers
            .write(register::cpu::Cp0::Count, u64::from(new_count));
        if old_count < compare && new_count >= compare {
            let cause = self.registers.cause();
            let ip = cause.interrupt_pending().with_timer(true);
            self.registers.set_cause(cause.with_interrupt_pending(ip));
            if self.registers.trigger_interrupt() {
                self.target.interrupt_pending = true;
            }
        }
    }

    /// Updates the CPU state for the given exception, and returns a host pointer to the JIT'ed function containing the exception handler.
    fn handle_exception(&mut self, exception: Exception, coprocessor: Option<u8>) -> usize {
        if self
            .registers
            .status()
            .diagnostic_status()
            .bootstrap_exception_vectors()
        {
            todo!("bootstrap exception vectors");
        }

        let mut cause = self.registers.cause().with_exception_code(exception.into());
        if let Some(cop) = coprocessor {
            cause.set_coprocessor_error(cop);
        }

        if !self.registers.status().exception_level() {
            let new_status: u32 = self.registers.status().with_exception_level(true).into();
            self.registers
                .write(register::cpu::Cp0::Status, u64::from(new_status));

            // If we are inside of a delay slot BadVAddr should be the address of the previous instruction,
            // so that we dont skip over the branch when we return from the exception. Notify the CPU of this by setting the BD bit.
            let pc = {
                let mut pc = self.registers.read(register::cpu::Special::Pc);
                if self.flags().inside_delay_slot() {
                    pc -= INSTRUCTION_SIZE as u64;
                    cause.set_branch_delay(true);
                } else {
                    cause.set_branch_delay(false);
                }

                #[allow(clippy::cast_possible_truncation, clippy::cast_sign_loss)]
                {
                    // Truncate and sign-extend
                    pc as i32 as u64
                }
            };
            self.registers.write(register::cpu::Cp0::EPC, pc);
        }

        self.registers
            .write(register::cpu::Cp0::Cause, u64::from(cause.raw()));
        self.get_function_ptr(exception.vector() as u64)
    }

    #[allow(clippy::similar_names)] // Just matching the spec
    unsafe extern "C" fn handle_exception_jit(
        &mut self,
        code: u64,
        has_coprocessor: bool,
        coprocessor: u8,
        has_bad_vaddr: bool,
        bad_vaddr: u64,
    ) -> usize {
        let code = u8::try_from(code).expect("exception code should fit in an u8");
        let exception = Exception::from_repr(code).unwrap_or_else(|| {
            let msg = format!("invalid exception code {code:#x}");
            self.panic_update_debugger(&msg)
        });

        if has_bad_vaddr {
            self.registers
                .write(register::cpu::Cp0::BadVAddr, bad_vaddr);
            let vaddr = tlb::VirtualAddress::new(bad_vaddr);
            let vpn = vaddr.virtual_page_number(self.registers.page_mask(), Bits::default());

            {
                let context = self.registers.context().with_bad_virtual_page_number(vpn);
                self.registers
                    .write(register::cpu::Cp0::Context, context.into());

                let xcontext = self
                    .registers
                    .xcontext()
                    .with_bad_virtual_page_number(vpn)
                    .with_address_space_id(vaddr.mode_64().into());
                self.registers
                    .write(register::cpu::Cp0::XContext, xcontext.into());
            }
        }

        let cop = has_coprocessor.then_some(coprocessor);
        self.handle_exception(exception, cop)
    }

    unsafe extern "C" fn probe_tlb_entry(&mut self) {
        let probe = self.memory.tlb.probe(&self.registers);
        self.registers
            .write(register::cpu::Cp0::Index, probe.into());
    }

    unsafe extern "C" fn read_tlb_entry(&mut self, index: u64) {
        let index = usize::try_from(index).expect("u64 should fit in usize");
        self.memory
            .tlb
            .read_entry(index, &mut self.registers)
            .unwrap_or_else(|| {
                let msg = format!("failed to set tlb entry at {index:#x}");
                self.panic_update_debugger(&msg)
            });
    }

    unsafe extern "C" fn write_tlb_entry(&mut self, index: u64) {
        let index = usize::try_from(index).expect("u64 should fit in usize");
        self.memory
            .tlb
            .write_entry(index, &self.registers)
            .unwrap_or_else(|| {
                let msg = format!("failed to set tlb entry at {index:#x}");
                self.panic_update_debugger(&msg)
            });
    }
}

impl<B: Bus> TargetDependantCallbacks for Environment<'_, Cpu, B> {
    fn on_block_entered(&mut self, instructions_in_block: usize) -> usize {
        self.update_count(instructions_in_block);
        if self.target.interrupt_pending {
            self.target.interrupt_pending = false;
            self.handle_exception(Exception::Interrupt, None)
        } else {
            // Resume execution as normal
            0
        }
    }

    fn callback_ptr(&self, func: RuntimeFunction) -> *const u8 {
        match func {
            RuntimeFunction::HandleException => Self::handle_exception_jit as *const u8,
            RuntimeFunction::ProbeTlbEntry => Self::probe_tlb_entry as *const u8,
            RuntimeFunction::ReadTlbEntry => Self::read_tlb_entry as *const u8,
            RuntimeFunction::WriteTlbEntry => Self::write_tlb_entry as *const u8,
            _ => std::ptr::null(),
        }
    }
}

impl<B: Bus> InterruptHandler for Environment<'_, Cpu, B> {
    fn handle_interrupt(&mut self, interrupt_pending: u8) {
        let cause = self.registers.cause();
        let pending = cause.interrupt_pending().raw() | interrupt_pending;
        self.registers
            .set_cause(cause.with_interrupt_pending(pending.into()));
        if self.registers.trigger_interrupt() {
            self.target.interrupt_pending = true;
        }
    }
}
