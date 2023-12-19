//! The runtime environment which generated code can call into.

use self::{
    bus::{Bus, PhysicalAddress},
    gdb::command::MonitorCommand,
    registers::{RegIndex, Registers},
};
use crate::{
    codegen::{self, CodeGen, FallthroughAmount, INSIDE_DELAY_SLOT_STORAGE},
    label::{generate_label_functions, JitFunctionPointer, LabelWithContext},
    runtime::{
        bus::{BusError, PanicAction},
        memory::tlb::AccessMode,
    },
    target::{Cpu, Instruction, Label, LabelList, Memory, RegisterStorage, Target},
    JitBuilder,
};
use inkwell::{context::Context, execution_engine::ExecutionEngine, module::Module};
use mips_decomp::{register, Exception, INSTRUCTION_SIZE};
use std::{collections::HashMap, pin::Pin};
use strum::IntoEnumIterator;

pub(crate) use self::function::RuntimeFunction;

pub mod bus;
mod function;
pub(crate) mod gdb;
pub(crate) mod memory;
pub(crate) mod registers;

pub(crate) trait TargetDependantCallbacks {
    /// Pointers to target-dependant callbacks, to be called by JIT'd code. These may be null if the function is not supported by the target.
    fn callback_ptr(&self, _func: RuntimeFunction) -> *const u8 {
        std::ptr::null()
    }
}

pub(crate) trait InterruptHandler {
    /// Processes the given Interrupt Pending (IP) field, and schedules an interrupt if necessary.
    fn handle_interrupt(&mut self, interrupt_pending: u8);
}

pub(crate) trait GdbIntegration: Sized {
    /// Copy of [`gdbstub::target::ext::base::singlethread::SingleThreadBase::read_registers`]
    /// so that it can be specialised based on the target, see its documentation for more information.
    fn gdb_read_registers(
        &mut self,
        regs: &mut <<Self as gdbstub::target::Target>::Arch as gdbstub::arch::Arch>::Registers,
    ) -> gdbstub::target::TargetResult<(), Self>
    where
        Self: gdbstub::target::Target;

    /// Copy of [`gdbstub::target::ext::base::singlethread::SingleThreadBase::write_registers`]
    /// so that it can be specialised based on the target, see its documentation for more information.
    fn gdb_write_registers(
        &mut self,
        regs: &<<Self as gdbstub::target::Target>::Arch as gdbstub::arch::Arch>::Registers,
    ) -> gdbstub::target::TargetResult<(), Self>
    where
        Self: gdbstub::target::Target;

    /// Extra GDB monitor commands to register.
    fn extra_monitor_commands() -> Vec<MonitorCommand<Self>> {
        Vec::new()
    }
}

/// A shorthand for all the traits that the target must implement on the runtime environment.
pub(crate) trait ValidRuntime:
    TargetDependantCallbacks + InterruptHandler + GdbIntegration
{
}

impl<R: TargetDependantCallbacks + InterruptHandler + GdbIntegration> ValidRuntime for R {}

pub(crate) struct Environment<'ctx, T: Target, B: Bus>
where
    for<'a> Environment<'a, T, B>: ValidRuntime,
{
    pub(crate) registers: T::Registers,
    pub(crate) memory: T::Memory,
    pub(crate) codegen: CodeGen<'ctx, T>,
    pub(crate) bus: B,
    debugger: Option<gdb::Debugger<'ctx, T, B>>,
    pub(crate) interrupt_pending: bool,
    pub(crate) exit_requested: bool,
    // TODO: replace this with a proper logging library
    trace: bool,
}

impl<'ctx, T: Target, B: Bus> Environment<'ctx, T, B>
where
    for<'a> Environment<'a, T, B>: ValidRuntime,
{
    pub(crate) fn new(
        mut builder: JitBuilder<true, T, B>,
        context: &'ctx Context,
        module: Module<'ctx>,
        execution_engine: ExecutionEngine<'ctx>,
    ) -> Pin<Box<Self>> {
        let mut env = Box::new(Self {
            registers: builder.registers(),
            bus: builder.bus,
            trace: builder.trace,
            memory: Default::default(),
            codegen: CodeGen::new(context, module, execution_engine),
            debugger: None,
            interrupt_pending: false,
            exit_requested: false,
        });

        if let Some(gdb) = builder.gdb {
            env.debugger = Some(gdb::Debugger::new(&mut env, gdb));
        }

        let globals = env.map_into(&env.codegen.module, &env.codegen.execution_engine);
        env.codegen.initialise(globals);
        env.into()
    }

    fn map_into(
        &self,
        module: &Module<'ctx>,
        execution_engine: &ExecutionEngine<'ctx>,
    ) -> codegen::Globals<'ctx, T> {
        let context = module.get_context();
        let i64_ptr_type = context.i64_type().ptr_type(Default::default());

        // Map a pointer to the environment struct
        let env_ptr = module.add_global(i64_ptr_type, Default::default(), "env");
        let ptr = self as *const Environment<_, _> as usize;
        execution_engine.add_global_mapping(&env_ptr, ptr);

        // Map the registers into the modules globals
        let registers = self.registers.build_globals(module, execution_engine);

        // Map the runtime functions
        let mut functions = HashMap::new();
        for func in RuntimeFunction::iter() {
            let ptr: *const u8 = match func {
                RuntimeFunction::Panic => Self::panic as _,
                RuntimeFunction::OnInstruction => Self::on_instruction as _,
                RuntimeFunction::GetFunctionPtr => Self::get_function_ptr as _,
                RuntimeFunction::GetPhysicalAddress => Self::get_physical_address as _,

                // See `runtime/memory/mod.rs`.
                RuntimeFunction::ReadI8 => Self::read_u8 as _,
                RuntimeFunction::ReadI16 => Self::read_u16 as _,
                RuntimeFunction::ReadI32 => Self::read_u32 as _,
                RuntimeFunction::ReadI64 => Self::read_u64 as _,
                RuntimeFunction::WriteI8 => Self::write_u8 as _,
                RuntimeFunction::WriteI16 => Self::write_u16 as _,
                RuntimeFunction::WriteI32 => Self::write_u32 as _,
                RuntimeFunction::WriteI64 => Self::write_u64 as _,

                RuntimeFunction::ReadPhysicalI8 => Self::read_physical_u8 as _,
                RuntimeFunction::ReadPhysicalI16 => Self::read_physical_u16 as _,
                RuntimeFunction::ReadPhysicalI32 => Self::read_physical_u32 as _,
                RuntimeFunction::ReadPhysicalI64 => Self::read_physical_u64 as _,
                RuntimeFunction::WritePhysicalI8 => Self::write_physical_u8 as _,
                RuntimeFunction::WritePhysicalI16 => Self::write_physical_u16 as _,
                RuntimeFunction::WritePhysicalI32 => Self::write_physical_u32 as _,
                RuntimeFunction::WritePhysicalI64 => Self::write_physical_u64 as _,

                // Target-dependant runtime functions
                _ => self.callback_ptr(func),
            };

            let value = func.map_into(&context, module, execution_engine, ptr);
            functions.entry(func).or_insert(value);
        }

        codegen::Globals {
            env_ptr,
            registers,
            functions,
        }
    }

    fn panic_update_debugger(&mut self, message: &str) -> ! {
        eprintln!("\n{message}\n{:?}", self.registers);
        let action = self.bus.on_panic(BusError::Jit(message.to_string()));
        if self.debugger.is_some() {
            self.debugger.as_mut().unwrap().signal_panicked();
            loop {
                self.update_debugger()
            }
        } else {
            match action {
                PanicAction::Kill => panic!("unrecoverable CPU error"),
                PanicAction::Idle => loop {
                    // The thread will be killed externally.
                    std::thread::park()
                },
            }
        }
    }

    /// This function panics if the translation fails.
    pub(crate) fn virtual_to_physical_address(
        &mut self,
        vaddr: u64,
        access_mode: AccessMode,
    ) -> PhysicalAddress {
        self.memory
            .virtual_to_physical_address(vaddr, access_mode, &self.registers)
            .unwrap_or_else(|err| {
                let msg = format!("failed to generate paddr for vaddr {vaddr:#06x}: {err:#?}");
                self.panic_update_debugger(&msg)
            })
    }

    /// This function panics if the translation fails.
    pub(crate) fn physical_to_virtual_address(
        &mut self,
        paddr: PhysicalAddress,
        access_mode: AccessMode,
    ) -> u64 {
        self.memory
            .physical_to_virtual_address(paddr, access_mode, &self.registers)
            .unwrap_or_else(|err| {
                let msg = format!("failed to generate vaddr for paddr {paddr:#06x}: {err:#?}");
                self.panic_update_debugger(&msg)
            })
    }

    /*
        Runtime functions. These are not meant to be called directly, but rather by JIT'ed code.
    */

    unsafe extern "C" fn get_physical_address(&mut self, vaddr: u64) -> PhysicalAddress {
        self.virtual_to_physical_address(vaddr, AccessMode::Read)
    }

    unsafe extern "C" fn get_function_ptr(&mut self, vaddr: u64) -> JitFunctionPointer {
        let insert_index = match self.codegen.labels.get(vaddr) {
            Ok(label) => {
                if self.trace {
                    println!("found existing block at {vaddr:#x}");
                }

                let ptr = label.pointer.unwrap();
                return ptr;
            }
            Err(insert_index) => insert_index,
        };

        if self.trace {
            println!("generating block at {vaddr:#x}",);
        }

        let paddr = self.virtual_to_physical_address(vaddr, AccessMode::Read);
        let label_list = {
            // Arbitrary limit on the maximum number of instructions in a block to avoid infinite loops.
            let mut list = {
                let mut iter = bus::u32_iter(&mut self.bus, paddr).take(0x1000);
                <T::LabelList as LabelList>::from_iter(&mut iter)
            }
            .unwrap_or_else(|| {
                let msg = format!("failed to parse instructions for {vaddr:#x}");
                self.panic_update_debugger(&msg)
            });
            list.set_start(vaddr);
            list
        };

        let module = self.codegen.context.create_module("tmp");
        let globals = self.map_into(&module, &self.codegen.execution_engine);
        let lab = self
            .codegen
            .add_dynamic_function(module, globals, |codegen, module| {
                let mut lab: LabelWithContext<'_, T> = {
                    let mut labels = generate_label_functions(label_list, codegen.context, module);
                    labels.pop().ok_or_else(|| {
                        format!("failed to generate label functions for {vaddr:#x}")
                    })?
                };

                if let Ok(fallthrough) = codegen.labels.get(lab.label.end() as u64) {
                    lab.fallthrough_fn = Some(fallthrough.function);
                    if self.trace {
                        println!(
                            "found fallthrough block at {:#x}",
                            fallthrough.label.start(),
                        );
                    }
                } else {
                    let amount = if let Some(second_last) = lab.label.instructions().nth_back(1) {
                        if second_last.has_delay_slot() {
                            FallthroughAmount::Two
                        } else {
                            FallthroughAmount::One
                        }
                    } else {
                        FallthroughAmount::One
                    };
                    lab.fallthrough_fn = Some(codegen.fallthrough_function(amount));
                }

                lab.compile(codegen, self.trace || self.debugger.is_some());
                Ok(lab)
            })
            .unwrap_or_else(|err| {
                self.panic_update_debugger(&format!("failed to generate function: {err:#?}"))
            });

        let ptr = lab.pointer.unwrap();
        self.codegen.labels.insert(insert_index, lab);
        ptr
    }

    // This will only be called if either the debugger or tracing is enabled.
    unsafe extern "C" fn on_instruction(&mut self) {
        let pc_vaddr = self.registers.read_program_counter();

        if self.trace {
            let instr_raw: u32 = self.read_or_panic(pc_vaddr).into();
            let instr: T::Instruction = instr_raw.try_into().unwrap_or_else(|_err| {
                let msg = format!("failed to parse instruction at {pc_vaddr:#06x}");
                self.panic_update_debugger(&msg)
            });
            println!("{pc_vaddr:06x}: {instr}");
        }

        if self.debugger.is_some() {
            self.update_debugger();
            self.debugger.as_mut().unwrap().on_instruction(pc_vaddr);
            while self.debugger.as_ref().unwrap().is_paused() {
                // Block until the debugger tells us to continue
                self.update_debugger();
            }
        }
    }

    unsafe extern "C" fn panic(&mut self, string_ptr: *const u8, len: u64) {
        let string = {
            let slice = std::slice::from_raw_parts(string_ptr, len as usize);
            std::str::from_utf8(slice).unwrap()
        };
        self.panic_update_debugger(&format!("Environment::panic called: {string}"));
    }
}

// TODO: move to different file
impl<B: Bus> Environment<'_, Cpu, B> {
    /// Called whenever a block of JIT'ed code is entered. This returns a pointer to the exception handler if an interrupt is pending, otherwise null.
    unsafe extern "C" fn on_block_entered(&mut self, instructions_in_block: u64) -> usize {
        // Trigger an timer interrupt if it would happen anywhere in this block.
        // We're doing this ahead of time to avoid trapping into the runtime environment to check on every instruction.
        let old_count = self.registers.read(register::Cp0::Count) as u32;
        let compare = self.registers.read(register::Cp0::Compare) as u32;
        let new_count = old_count.wrapping_add(8 * instructions_in_block as u32);
        self.registers.write(register::Cp0::Count, new_count as u64);
        if old_count < compare && new_count >= compare {
            let cause = self.registers.cause();
            let ip = cause.interrupt_pending().with_timer(true);
            self.registers.set_cause(cause.with_interrupt_pending(ip));
            if self.registers.trigger_interrupt() {
                self.interrupt_pending = true;
            }
        }

        self.bus
            .tick(instructions_in_block as usize)
            .unwrap_or_else(|err| {
                let msg = format!("failed to tick: {err:#?}");
                self.panic_update_debugger(&msg)
            })
            .handle(self);

        if self.exit_requested {
            self.codegen.return_from_jit_ptr()
        } else if self.interrupt_pending {
            self.interrupt_pending = false;
            self.handle_exception(Exception::Interrupt, None)
        } else {
            // Null pointer, must be checked by the caller.
            0
        }
    }

    /// Updates the CPU state for the given exception, and returns a host pointer to the JIT'ed function containing the exception handler.
    pub(crate) fn handle_exception(
        &mut self,
        exception: Exception,
        coprocessor: Option<u8>,
    ) -> usize {
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
                .write(register::Cp0::Status, new_status as u64);

            // If we are inside of a delay slot BadVAddr should be the address of the previous instruction,
            // so that we dont skip over the branch when we return from the exception. Notify the CPU of this by setting the BD bit.
            let pc = {
                let mut pc = self.registers.read(register::Special::Pc);
                if self.registers.read(INSIDE_DELAY_SLOT_STORAGE) != 0 {
                    pc -= INSTRUCTION_SIZE as u64;
                    cause.set_branch_delay(true);
                } else {
                    self.registers.write(INSIDE_DELAY_SLOT_STORAGE, 0);
                    cause.set_branch_delay(false);
                }

                pc as i32 as u64 // Truncate and sign-extend
            };
            self.registers.write(register::Cp0::EPC, pc);
        }

        self.registers
            .write(register::Cp0::Cause, cause.raw() as u64);
        unsafe { self.get_function_ptr(exception.vector() as u64) }
    }

    unsafe extern "C" fn handle_exception_jit(
        &mut self,
        code: u64,
        has_coprocessor: bool,
        coprocessor: u8,
        has_bad_vaddr: bool,
        bad_vaddr: u64,
    ) -> usize {
        let exception = Exception::from_repr(code as usize).unwrap_or_else(|| {
            let msg = format!("invalid exception code {code:#x}");
            self.panic_update_debugger(&msg)
        });

        if has_bad_vaddr {
            self.registers.write(register::Cp0::BadVAddr, bad_vaddr);

            let vaddr = memory::tlb::VirtualAddress::new(bad_vaddr);
            let vpn = vaddr.virtual_page_number(self.registers.page_mask(), Default::default());

            let context = self.registers.context().with_bad_virtual_page_number(vpn);
            self.registers.write(register::Cp0::Context, context.into());

            let xcontext = self
                .registers
                .xcontext()
                .with_bad_virtual_page_number(vpn)
                .with_address_space_id(vaddr.mode_64().into());
            self.registers
                .write(register::Cp0::XContext, xcontext.into());
        }

        let cop = has_coprocessor.then_some(coprocessor);
        self.handle_exception(exception, cop)
    }

    unsafe extern "C" fn probe_tlb_entry(&mut self) {
        let probe = self.memory.tlb.probe(&self.registers);
        self.registers.write(register::Cp0::Index, probe.into());
    }

    unsafe extern "C" fn read_tlb_entry(&mut self, index: u64) {
        self.memory
            .tlb
            .read_entry(index as usize, &mut self.registers)
            .unwrap_or_else(|| {
                let msg = format!("failed to set tlb entry at {index:#x}");
                self.panic_update_debugger(&msg)
            });
    }

    unsafe extern "C" fn write_tlb_entry(&mut self, index: u64) {
        self.memory
            .tlb
            .write_entry(index as usize, &self.registers)
            .unwrap_or_else(|| {
                let msg = format!("failed to set tlb entry at {index:#x}");
                self.panic_update_debugger(&msg)
            });
    }
}

impl<B: Bus> TargetDependantCallbacks for Environment<'_, Cpu, B> {
    fn callback_ptr(&self, func: RuntimeFunction) -> *const u8 {
        match func {
            RuntimeFunction::OnBlockEntered => Self::on_block_entered as *const u8,
            RuntimeFunction::HandleException => Self::handle_exception_jit as *const u8,
            RuntimeFunction::ProbeTlbEntry => Self::probe_tlb_entry as *const u8,
            RuntimeFunction::ReadTlbEntry => Self::read_tlb_entry as *const u8,
            RuntimeFunction::WriteTlbEntry => Self::write_tlb_entry as *const u8,
            _ => panic!("unsupported runtime function: {func:?}"),
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
            self.interrupt_pending = true;
        }
    }
}

impl<B: Bus> GdbIntegration for Environment<'_, Cpu, B> {
    fn gdb_read_registers(
        &mut self,
        regs: &mut <<Self as gdbstub::target::Target>::Arch as gdbstub::arch::Arch>::Registers,
    ) -> gdbstub::target::TargetResult<(), Self> {
        regs.pc = self.registers.read(register::Special::Pc) as u32;
        regs.hi = self.registers.read(register::Special::Hi) as u32;
        regs.lo = self.registers.read(register::Special::Lo) as u32;
        regs.cp0.cause = self.registers.read(register::Cp0::CacheErr) as u32;
        regs.cp0.status = self.registers.read(register::Cp0::Status) as u32;
        regs.cp0.badvaddr = self.registers.read(register::Cp0::BadVAddr) as u32;
        for (i, r) in regs.r.iter_mut().enumerate() {
            *r = self.registers.general_purpose.read_relaxed(i).unwrap() as u32;
        }
        for (i, r) in regs.fpu.r.iter_mut().enumerate() {
            *r = self.registers.fpu.read_relaxed(i).unwrap() as u32;
        }
        Ok(())
    }

    fn gdb_write_registers(
        &mut self,
        regs: &<<Self as gdbstub::target::Target>::Arch as gdbstub::arch::Arch>::Registers,
    ) -> gdbstub::target::TargetResult<(), Self> {
        assert!(
            regs.pc == self.registers.read(register::Special::Pc) as _,
            "gdb: attempted to change PC"
        );

        self.registers.write(register::Special::Hi, regs.hi as u64);
        self.registers.write(register::Special::Lo, regs.lo as u64);
        self.registers
            .write(register::Cp0::Cause, regs.cp0.cause as u64);
        self.registers
            .write(register::Cp0::Status, regs.cp0.status as u64);
        self.registers
            .write(register::Cp0::BadVAddr, regs.cp0.badvaddr as u64);

        for (i, r) in regs.r.iter().enumerate() {
            self.registers
                .general_purpose
                .write_relaxed(i, *r as u64)
                .unwrap();
        }

        for (i, r) in regs.fpu.r.iter().enumerate() {
            self.registers.fpu.write_relaxed(i, *r as u64).unwrap();
        }

        Ok(())
    }

    fn extra_monitor_commands() -> Vec<MonitorCommand<Self>> {
        vec![
            MonitorCommand {
                name: "status",
                description: "print the coprocessor 0 status register",
                handler: Box::new(|env, out, _args| {
                    writeln!(out, "{:#?}", env.registers.status())?;
                    Ok(())
                }),
            },
            MonitorCommand {
                name: "cause",
                description: "print the coprocessor 0 cause register",
                handler: Box::new(|env, out, _args| {
                    writeln!(out, "{:#?}", env.registers.cause())?;
                    Ok(())
                }),
            },
            MonitorCommand {
                name: "fpu-status",
                description: "print the FPU control and status register",
                handler: Box::new(|env, out, _args| {
                    writeln!(out, "{:#?}", env.registers.fpu_control_status())?;
                    Ok(())
                }),
            },
            MonitorCommand {
                name: "tlb",
                description: "print every entry in the TLB",
                handler: Box::new(|env, out, _args| {
                    writeln!(out, "{:#?}", env.memory.tlb)?;
                    Ok(())
                }),
            },
        ]
    }
}
