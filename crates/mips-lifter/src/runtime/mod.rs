//! The runtime environment which generated code can call into.

use self::{
    bus::{Bus, PhysicalAddress},
    gdb::command::MonitorCommand,
};
use crate::{
    codegen::{self, CodeGen, FallthroughAmount},
    label::{generate_label_functions, JitFunctionPointer, LabelWithContext},
    runtime::{
        bus::{BusError, PanicAction},
        memory::tlb::AccessMode,
    },
    target::{Instruction, Label, LabelList, Memory, RegisterStorage, Target},
    JitBuilder,
};
use inkwell::{context::Context, execution_engine::ExecutionEngine, module::Module};
use std::{cell::UnsafeCell, collections::HashMap, mem::size_of, pin::Pin};
use strum::IntoEnumIterator;

pub(crate) use self::function::RuntimeFunction;

pub mod bus;
mod function;
pub(crate) mod gdb;
pub(crate) mod memory;
pub(crate) mod register_bank;

pub(crate) trait TargetDependantCallbacks {
    /// Called before executing a single instruction of a basic block. Note that you do not need to call [`Bus::tick`] here.
    /// This should return a function pointer to run instead of the block of guest machine code, or null if we can continue as normal.
    fn on_block_entered(&mut self, instructions_in_block: usize) -> usize;

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
    type Arch: gdbstub::arch::Arch<Usize = u64>;

    /// Copy of [`gdbstub::target::ext::base::single_register_access::SingleRegisterAccess::read_register`], see its documentation for more information.
    fn gdb_read_register(
        &mut self,
        reg_id: <<Self as GdbIntegration>::Arch as gdbstub::arch::Arch>::RegId,
        buf: &mut [u8],
    ) -> gdbstub::target::TargetResult<usize, Self>
    where
        Self: gdbstub::target::Target<Arch = <Self as GdbIntegration>::Arch>;

    /// Copy of [`gdbstub::target::ext::base::single_register_access::SingleRegisterAccess::write_register`], see its documentation for more information.
    fn gdb_write_register(
        &mut self,
        reg_id: <<Self as GdbIntegration>::Arch as gdbstub::arch::Arch>::RegId,
        value: &[u8],
    ) -> gdbstub::target::TargetResult<(), Self>
    where
        Self: gdbstub::target::Target<Arch = <Self as GdbIntegration>::Arch>;

    /// Copy of [`gdbstub::target::ext::base::singlethread::SingleThreadBase::read_registers`], see its documentation for more information.
    fn gdb_read_registers(
        &mut self,
        regs: &mut <<Self as gdbstub::target::Target>::Arch as gdbstub::arch::Arch>::Registers,
    ) -> gdbstub::target::TargetResult<(), Self>
    where
        Self: gdbstub::target::Target<Arch = <Self as GdbIntegration>::Arch>;

    /// Copy of [`gdbstub::target::ext::base::singlethread::SingleThreadBase::write_registers`], see its documentation for more information.
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
    pub(crate) bus: B,
    pub(crate) target: T,
    pub(crate) registers: T::Registers,
    pub(crate) memory: T::Memory,
    pub(crate) codegen: CodeGen<'ctx, T>,
    jit_flags: UnsafeCell<codegen::Flags>,
    debugger: Option<gdb::Debugger<'ctx, T, B>>,
    exit_requested: bool,
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
            jit_flags: UnsafeCell::default(),
            registers: builder.registers(),
            bus: builder.bus,
            trace: builder.trace,
            memory: Default::default(),
            target: Default::default(),
            codegen: CodeGen::new(context, module, execution_engine),
            debugger: None,
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

        // Map a pointer to the flags
        let flags_ptr = module.add_global(i64_ptr_type, Default::default(), "flags");
        execution_engine.add_global_mapping(&flags_ptr, self.jit_flags.get() as usize);

        // Map the registers into the modules globals
        let registers = self.registers.build_globals(module, execution_engine);

        // Map the runtime functions
        let mut functions = HashMap::new();
        for func in RuntimeFunction::iter() {
            let ptr: *const u8 = match func {
                RuntimeFunction::Panic => Self::panic as _,
                RuntimeFunction::OnInstruction => Self::on_instruction as _,
                RuntimeFunction::OnBlockEntered => Self::on_block_entered as _,
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
            flags_ptr,
            registers,
            functions,
        }
    }

    pub(crate) fn panic_update_debugger(&mut self, message: &str) -> ! {
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

    pub(crate) fn flags(&self) -> codegen::Flags {
        unsafe { self.jit_flags.get().read() }
    }

    fn read_raw_instruction(&mut self, paddr: PhysicalAddress) -> u32 {
        self.bus
            .read_instruction_memory(paddr)
            .unwrap_or_else(|err| {
                let msg = &format!("failed to read instruction memory at {paddr:#x}: {err:#?}");
                self.panic_update_debugger(msg)
            })
            .handle(self)
            .into()
    }

    fn read_label_list(&mut self, mut paddr: PhysicalAddress, vaddr: u64) -> T::LabelList {
        let iter = std::iter::from_fn(|| {
            let value = self.read_raw_instruction(paddr);
            paddr += size_of::<u32>() as PhysicalAddress;
            Some(value)
        })
        // Arbitrary limit on the maximum number of instructions in a block, to avoid infinitely fetching when there is no terminator.
        .take(0x1000);

        let mut res = T::LabelList::from_iter(iter).unwrap_or_else(|| {
            let msg = &format!("failed to generate label list at {paddr:#x} ({vaddr:#x})");
            self.panic_update_debugger(msg)
        });
        res.set_start(vaddr);
        res
    }

    /*
        Runtime functions. These are not meant to be called directly, but rather by JIT'ed code.
    */

    unsafe extern "C" fn on_block_entered(&mut self, instructions_in_block: u64) -> usize {
        let instructions = instructions_in_block as usize;
        self.bus
            .tick(instructions)
            .unwrap_or_else(|err| {
                let msg = format!("failed to tick the bus: {err:#?}");
                self.panic_update_debugger(&msg)
            })
            .handle(self);

        if self.exit_requested {
            // Redirect execution to a function that simply returns, eventually giving back control to the callee.
            self.codegen.return_from_jit_ptr()
        } else {
            <Self as TargetDependantCallbacks>::on_block_entered(self, instructions)
        }
    }

    unsafe extern "C" fn get_physical_address(&mut self, vaddr: u64) -> PhysicalAddress {
        self.virtual_to_physical_address(vaddr, AccessMode::Read)
    }

    pub(crate) unsafe extern "C" fn get_function_ptr(&mut self, vaddr: u64) -> JitFunctionPointer {
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
            println!("generating block at {vaddr:#x}");
        }

        let paddr = self.virtual_to_physical_address(vaddr, AccessMode::Read);
        let label_list = self.read_label_list(paddr, vaddr);

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
            let paddr = self.virtual_to_physical_address(pc_vaddr, AccessMode::Read);
            let instr_raw = self.read_raw_instruction(paddr);
            let instr: T::Instruction = instr_raw.try_into().unwrap_or_else(|_err| {
                let msg = format!("failed to parse instruction at {pc_vaddr:#x}");
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

    unsafe extern "C" fn panic(&mut self, string_ptr: *const u8, string_len: u64) {
        let string = {
            let slice = std::slice::from_raw_parts(string_ptr, string_len as usize);
            std::str::from_utf8(slice).unwrap()
        };
        self.panic_update_debugger(&format!("Environment::panic called: {string}"));
    }
}
