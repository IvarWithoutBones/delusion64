//! The runtime environment which generated code can call into.

use self::{
    memory::tlb::{AccessMode, TranslationLookasideBuffer, VirtualAddress},
    registers::Registers,
};
use crate::{
    codegen::{self, CodeGen, FallthroughAmount},
    label::{generate_label_functions, JitFunctionPointer},
    runtime::bus::{BusError, PanicAction},
    JitBuilder,
};
use inkwell::{context::Context, execution_engine::ExecutionEngine, module::Module};
use mips_decomp::{instruction::ParsedInstruction, register, Exception, INSTRUCTION_SIZE};
use std::{collections::HashMap, pin::Pin};
use strum::IntoEnumIterator;

pub(crate) use self::function::RuntimeFunction;

pub mod bus;
mod function;
pub(crate) mod gdb;
mod memory;
mod registers;

pub struct Environment<'ctx, Bus: bus::Bus> {
    pub(crate) registers: Pin<Box<Registers>>,
    pub(crate) codegen: CodeGen<'ctx>,
    pub(crate) bus: Bus,
    tlb: TranslationLookasideBuffer,
    debugger: Option<gdb::Debugger<'ctx, Bus>>,
    pub(crate) interrupt_pending: bool,
    pub(crate) exit_requested: bool,
    // TODO: replace this with a proper logging library
    trace: bool,
}

impl<'ctx, Bus: bus::Bus> Environment<'ctx, Bus> {
    pub(crate) fn new(
        builder: JitBuilder<true, Bus>,
        context: &'ctx Context,
        module: Module<'ctx>,
        execution_engine: ExecutionEngine<'ctx>,
    ) -> Pin<Box<Self>> {
        let mut registers = Registers::default();
        for (reg, val) in builder.registers() {
            registers[*reg] = *val;
        }

        let mut env = Box::new(Self {
            registers: Box::pin(registers),
            tlb: TranslationLookasideBuffer::default(),
            debugger: None,
            codegen: CodeGen::new(context, module, execution_engine),
            interrupt_pending: false,
            exit_requested: false,
            bus: builder.bus,
            trace: builder.trace,
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
    ) -> codegen::Globals<'ctx> {
        let context = module.get_context();
        let i64_ptr_type = context.i64_type().ptr_type(Default::default());

        // Map a pointer to the environment struct
        let env_ptr = module.add_global(i64_ptr_type, Default::default(), "env");
        let ptr = self as *const Environment<_> as usize;
        execution_engine.add_global_mapping(&env_ptr, ptr);

        // Map the registers into the modules globals
        let registers = self.registers.map_into(&context, module, execution_engine);

        // Map the runtime functions
        let mut functions = HashMap::new();
        for func in RuntimeFunction::iter() {
            let ptr: *const u8 = match func {
                RuntimeFunction::Panic => Self::panic as _,
                RuntimeFunction::OnInstruction => Self::on_instruction as _,
                RuntimeFunction::GetFunctionPtr => Self::get_function_ptr as _,
                RuntimeFunction::OnBlockEntered => Self::on_block_entered as _,

                RuntimeFunction::HandleException => Self::handle_exception_jit as _,
                RuntimeFunction::GetPhysicalAddress => Self::get_physical_address as _,
                RuntimeFunction::ProbeTlbEntry => Self::probe_tlb_entry as _,
                RuntimeFunction::ReadTlbEntry => Self::read_tlb_entry as _,
                RuntimeFunction::WriteTlbEntry => Self::write_tlb_entry as _,

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

    fn virtual_to_physical_address(&mut self, vaddr: u64, mode: AccessMode) -> u32 {
        self.tlb
            .translate_vaddr(vaddr, mode, &self.registers)
            .unwrap_or_else(|err| {
                let msg = format!("failed to generate paddr for vaddr {vaddr:#06x}: {err:#?}");
                self.panic_update_debugger(&msg)
            })
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
            self.registers[register::Cp0::Status] = new_status as u64;

            // If we are inside of a delay slot BadVAddr should be the address of the previous instruction,
            // so that we dont skip over the branch when we return from the exception. Notify the CPU of this by setting the BD bit.
            self.registers[register::Cp0::EPC] = {
                let mut pc = self.registers[register::Special::Pc];
                if self.registers[crate::codegen::INSIDE_DELAY_SLOT_STORAGE] != 0 {
                    pc -= INSTRUCTION_SIZE as u64;
                    cause.set_branch_delay(true);
                } else {
                    self.registers[crate::codegen::INSIDE_DELAY_SLOT_STORAGE] = 0;
                    cause.set_branch_delay(false);
                }

                pc as i32 as u64 // Truncate and sign-extend
            };
        }

        self.registers[register::Cp0::Cause] = cause.raw() as u64;
        unsafe { self.get_function_ptr(exception.vector() as u64) }
    }

    /*
        Runtime functions. These are not meant to be called directly, but rather by JIT'ed code.
    */

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
            self.registers[register::Cp0::BadVAddr] = bad_vaddr;

            let vaddr = VirtualAddress::new(bad_vaddr);
            let vpn = vaddr.virtual_page_number(self.registers.page_mask(), Default::default());

            let context = self.registers.context().with_bad_virtual_page_number(vpn);
            self.registers[register::Cp0::Context] = context.into();

            let xcontext = self
                .registers
                .xcontext()
                .with_bad_virtual_page_number(vpn)
                .with_address_space_id(vaddr.mode_64().into());
            self.registers[register::Cp0::XContext] = xcontext.into();
        }

        let cop = has_coprocessor.then_some(coprocessor);
        self.handle_exception(exception, cop)
    }

    unsafe extern "C" fn get_physical_address(&mut self, vaddr: u64) -> u32 {
        self.virtual_to_physical_address(vaddr, AccessMode::Read)
    }

    // TODO: split this up and prettify it a bit, it is rather unwieldy right now.
    unsafe extern "C" fn get_function_ptr(&mut self, vaddr: u64) -> JitFunctionPointer {
        let vaddr = vaddr & u32::MAX as u64;
        let paddr = self.virtual_to_physical_address(vaddr, AccessMode::Read);

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

        let label_list = {
            let mut labels = {
                // Arbitrary limit on the maximum number of instructions in a block to avoid infinite loops.
                let mut iter = bus::u32_iter(&mut self.bus, paddr).take(0x1000);
                mips_decomp::read_labels(1, &mut iter)
            }
            .unwrap_or_else(|| {
                let msg = format!("failed to read labels for {vaddr:#x}");
                self.panic_update_debugger(&msg)
            });
            labels.set_start(vaddr as usize);
            labels
        };

        let module = self.codegen.context.create_module("tmp");
        let globals = self.map_into(&module, &self.codegen.execution_engine);
        let lab = self
            .codegen
            .add_dynamic_function(module, globals, |codegen, module| {
                let mut lab = {
                    let mut labels = generate_label_functions(label_list, codegen.context, module);
                    labels.pop().ok_or_else(|| {
                        format!("failed to generate label functions for {vaddr:#x}")
                    })?
                };

                if let Ok(fallthrough) = codegen.labels.get(lab.label.end() as u64) {
                    if self.trace {
                        println!(
                            "found fallthrough block at {:#x}",
                            fallthrough.label.start() * 4,
                        );
                    }

                    // See the comment in `label.rs` for more context.
                    lab.fallthrough_fn = if fallthrough.label.instructions.len() == 1 {
                        lab.fallthrough_instr = Some(fallthrough.label.instructions[0].clone());
                        if fallthrough
                            .fallthrough_fn
                            .as_ref()
                            .map(|f| f == &codegen.fallthrough_function(FallthroughAmount::One))
                            .unwrap_or(false)
                        {
                            // Falling through one instruction here will re-execute the fallthrough_instr.
                            Some(codegen.fallthrough_function(FallthroughAmount::Two))
                        } else {
                            fallthrough.fallthrough_fn
                        }
                    } else {
                        Some(fallthrough.function)
                    }
                } else {
                    let amount =
                        if let Some(second_last) = lab.label.instructions.iter().nth_back(1) {
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

    /// Called whenever a block of JIT'ed code is entered. This returns a pointer to the exception handler if an interrupt is pending, otherwise null.
    unsafe extern "C" fn on_block_entered(&mut self, instructions_in_block: u64) -> usize {
        // Trigger an timer interrupt if it would happen anywhere in this block.
        // We're doing this ahead of time to avoid trapping into the runtime environment to check on every instruction.
        let old_count = self.registers[register::Cp0::Count] as u32;
        let compare = self.registers[register::Cp0::Compare] as u32;
        let new_count = old_count.wrapping_add(8 * instructions_in_block as u32);
        self.registers[register::Cp0::Count] = new_count as u64;
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

    // This will only be called if either the debugger or tracing is enabled.
    unsafe extern "C" fn on_instruction(&mut self) {
        let pc_vaddr = self.registers[register::Special::Pc];

        if self.trace {
            let pc_paddr = self.virtual_to_physical_address(pc_vaddr, AccessMode::Read);
            let instr = ParsedInstruction::try_from(u32::from_be_bytes(
                *self.read(pc_paddr).unwrap().as_slice(),
            ))
            .unwrap();
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

    unsafe extern "C" fn probe_tlb_entry(&mut self) {
        self.registers[register::Cp0::Index] = self.tlb.probe(&self.registers).into();
    }

    unsafe extern "C" fn read_tlb_entry(&mut self, index: u64) {
        self.tlb
            .read_entry(index as usize, &mut self.registers)
            .unwrap_or_else(|| {
                let msg = format!("failed to set tlb entry at {index:#x}");
                self.panic_update_debugger(&msg)
            });
    }

    unsafe extern "C" fn write_tlb_entry(&mut self, index: u64) {
        self.tlb
            .write_entry(index as usize, &self.registers)
            .unwrap_or_else(|| {
                let msg = format!("failed to set tlb entry at {index:#x}");
                self.panic_update_debugger(&msg)
            });
    }
}
