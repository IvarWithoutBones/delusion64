use super::Rsp;
use crate::{
    runtime::{
        bus::{Bus, DmaDirection, DmaInfo},
        Environment, InterruptHandler, RuntimeFunction, TargetDependantCallbacks,
    },
    target::RegisterStorage,
};
use log::{trace, warn};
use mips_decomp::{
    register::rsp::control::{
        DmaRdramAddress, DmaReadLength, DmaSpAddress, DmaWriteLength, Status,
    },
    Exception, INSTRUCTION_SIZE,
};

fn sleep() {
    std::thread::sleep(std::time::Duration::from_millis(1));
}

impl<B: Bus> Environment<'_, Rsp, B> {
    fn sleep_if_halted(&mut self) {
        let mut slept = false;
        while self.registers.control.read_parsed::<Status>().halted() {
            // TODO: update GDB if connected
            sleep();
            slept = true;
        }
        if slept {
            println!("RSP woke up from halt");
        }
    }

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

        self.sleep_if_halted();
        self.get_function_ptr(self.registers.read_program_counter())
    }

    unsafe extern "C" fn write_status(&mut self, value: u32) {
        let mut status: Status = self.registers.control.read_parsed();
        if let Some(irq_req) = status.write(value) {
            todo!("RSP JIT request MI interrupt {irq_req:?}")
        }
        println!("RSP JIT status: {status:?}");
        self.registers.control.write_parsed(status);
        self.sleep_if_halted();
        std::process::abort();
    }

    unsafe extern "C" fn request_dma(&mut self, to_rdram: bool) {
        let status = self.registers.control.read_parsed::<Status>();
        if status.dma_full() {
            warn!("RSP DMA request ignored: DMA full");
            return;
        } else if status.dma_busy() {
            self.registers.control.swap_active_dma_buffer();
        }

        let (direction, length) = if to_rdram {
            let len = self
                .registers
                .control
                .read_parsed::<DmaWriteLength>()
                .length();
            (DmaDirection::ToRdram, len)
        } else {
            let len = self
                .registers
                .control
                .read_parsed::<DmaReadLength>()
                .length();
            (DmaDirection::FromRdram, len)
        };

        let rdram_address = self
            .registers
            .control
            .read_parsed::<DmaRdramAddress>()
            .address();
        let other_address = self
            .registers
            .control
            .read_parsed::<DmaSpAddress>()
            .address();

        let info = DmaInfo {
            direction,
            length,
            rdram_address,
            other_address,
        };
        trace!("DMA request: {info:?}");

        let status = self.registers.control.read_parsed::<Status>();
        self.bus
            .request_dma(info)
            .map(|effects| effects.handle(self))
            .unwrap_or_else(|err| {
                let msg = format!("failed to request DMA: {err}");
                self.panic_update_debugger(&msg)
            });

        // Wait for the DMA to start
        if status.dma_busy() {
            while !self.registers.control.read_parsed::<Status>().dma_full() {
                sleep();
            }
        } else {
            while !self.registers.control.read_parsed::<Status>().dma_busy() {
                sleep();
            }
        }
    }
}

impl<B: Bus> TargetDependantCallbacks for Environment<'_, Rsp, B> {
    fn callback_ptr(&self, func: RuntimeFunction) -> *const u8 {
        match func {
            RuntimeFunction::HandleException => Self::handle_exception_jit as *const u8,
            RuntimeFunction::RspWriteStatus => Self::write_status as *const u8,
            RuntimeFunction::RequestDma => Self::request_dma as *const u8,
            _ => std::ptr::null(),
        }
    }

    fn on_block_entered(&mut self, _instructions_in_block: usize) -> usize {
        self.sleep_if_halted();
        if self.check_invalidations() {
            let pc = self.registers.read_program_counter();
            self.get_function_ptr(pc)
        } else {
            0
        }
    }
}

impl<B: Bus> InterruptHandler for Environment<'_, Rsp, B> {
    fn handle_interrupt(&mut self, interrupt_pending: u8) {
        unreachable!("RSP does not support interrupts (IP: {interrupt_pending:#b})")
    }
}
