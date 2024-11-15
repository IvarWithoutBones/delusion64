//! The runtime environment which generated code can call into.

use self::{
    bus::{Bus, PhysicalAddress},
    gdb::command::Command,
};
use crate::{
    codegen::{self, CodeGen, CompilationError},
    label::{generate_labels, JitFunctionPointer},
    runtime::{
        bus::{BusError, PanicAction},
        memory::tlb::AccessMode,
    },
    target::{LabelList, Memory, RegisterStorage, Target},
    JitBuilder,
};
use inkwell::{context::Context, execution_engine::ExecutionEngine, module::Module};
use log::{error, trace};
use mips_decomp::INSTRUCTION_SIZE;
use std::{cell::UnsafeCell, collections::HashMap, pin::Pin};
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

pub(crate) trait GdbIntegration<T: Target>: Sized {
    type Usize: gdb::AsU64;
    type Arch: gdbstub::arch::Arch<
        Usize = Self::Usize,
        Registers = T::Registers,
        RegId = <T::Registers as RegisterStorage>::Id,
    >;

    /// Extra internal GDB monitor commands to register.
    fn extra_monitor_commands() -> Vec<Command<Self>> {
        Vec::new()
    }

    /// Converts the given address from GDB into one that can be used by the runtime.
    /// This is needed to distinguish between instruction/data memory for the RSP.
    fn to_runtime_address(&mut self, address: u64) -> u64 {
        address
    }

    /// Picks a memory type for the given GDB supplied address.
    fn memory_type_for_address(&self, _address: u64) -> memory::Type {
        memory::Type::Unknown
    }
}

/// A shorthand for all the traits that the target must implement on the runtime environment.
pub(crate) trait ValidRuntime<T: Target>:
    TargetDependantCallbacks + InterruptHandler + GdbIntegration<T>
{
}

impl<T, R> ValidRuntime<T> for R
where
    T: Target,
    R: TargetDependantCallbacks + InterruptHandler + GdbIntegration<T>,
{
}

pub(crate) struct Environment<'ctx, T: Target, B: Bus>
where
    for<'a> Environment<'a, T, B>: ValidRuntime<T>,
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
    for<'a> Environment<'a, T, B>: ValidRuntime<T>,
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

        let globals = env.map_into(&env.codegen.main_module, &env.codegen.execution_engine);
        env.codegen
            .initialise(globals)
            .expect("failed to initialise codegen");
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
        error!("{message}{:?}", self.registers);
        std::process::abort();
        let action = self.bus.on_panic(BusError::Jit(message.to_string()));
        if self.debugger.is_some() {
            self.debugger.as_mut().unwrap().signal_panicked();
            loop {
                self.update_debugger()
            }
        } else {
            match action {
                PanicAction::Kill => panic!("unrecoverable {} error", T::NAME),
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

    fn read_raw_instruction(&mut self, paddr: PhysicalAddress) -> (u32, bool) {
        let res = self
            .bus
            .read_instruction_memory(paddr)
            .unwrap_or_else(|err| {
                let msg = &format!("failed to read instruction memory at {paddr:#x}: {err:#?}");
                self.panic_update_debugger(msg)
            });
        let did_mutate = !res.mutated.is_empty();
        (res.handle(self).into(), did_mutate)
    }

    fn read_label_list(&mut self, paddr: PhysicalAddress, vaddr: u64) -> (T::LabelList, bool) {
        let mut invalidate = false;
        let mut offset = paddr;
        let iter = std::iter::from_fn(|| {
            let (value, did_mutate) = self.read_raw_instruction(offset);
            invalidate |= did_mutate;
            offset += INSTRUCTION_SIZE as PhysicalAddress;
            Some(value)
        })
        // Arbitrary limit on the maximum number of instructions in a block, to avoid infinitely fetching when there is no terminator.
        .take(0x1000);

        let mut res = T::LabelList::from_iter(iter).unwrap_or_else(|err| {
            let msg = &format!(
                "failed to generate label list at paddr={paddr:#x} vaddr={vaddr:#x}: {err}"
            );
            self.panic_update_debugger(msg)
        });
        res.set_start(vaddr);
        (res, invalidate)
    }

    pub(crate) fn check_invalidations(&mut self) -> bool {
        let mut invalidated = false;
        let mut jump = false;
        let pc = self.registers.read_program_counter();
        for range in self.bus.ranges_to_invalidate() {
            let start = self.physical_to_virtual_address(range.start, AccessMode::Read);
            let end = self.physical_to_virtual_address(range.end, AccessMode::Read);
            let range = &(start..end);
            self.codegen
                .labels
                .remove_within_range(range, &self.codegen.execution_engine);
            if range.contains(&pc) {
                jump = true;
            }
            invalidated = true;
        }

        if jump {
            // FIXME: This will inevitably overflow the stack. We also need to resume handling the result before jumping.
            error!("jumping to {pc:#x}");
            let pointer = self.get_function_ptr(pc) as *const ();
            let function =
                unsafe { std::mem::transmute::<*const (), extern "fastcall" fn() -> !>(pointer) };
            function();
        }

        invalidated
    }

    /*
        Runtime functions. These are not meant to be called directly, but rather by JIT'ed code.
    */

    unsafe extern "C" fn on_block_entered(&mut self, instructions_in_block: u64) -> usize {
        self.check_invalidations();
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
            self.codegen
                .return_from_jit_ptr()
                .expect("failed to fetch return_from_jit function")
        } else {
            <Self as TargetDependantCallbacks>::on_block_entered(self, instructions)
        }
    }

    unsafe extern "C" fn get_physical_address(&mut self, vaddr: u64) -> PhysicalAddress {
        self.virtual_to_physical_address(vaddr, AccessMode::Read)
    }

    pub(crate) extern "C" fn get_function_ptr(&mut self, vaddr: u64) -> JitFunctionPointer {
        self.check_invalidations();
        let mut insert_index = match self.codegen.labels.get(vaddr) {
            Ok(label) => {
                if self.trace {
                    trace!("found existing block at {vaddr:#x}");
                }
                return label.pointer.expect("label pointer is cached");
            }
            Err(insert_index) => insert_index,
        };

        if self.trace {
            trace!("generating block at {vaddr:#x}");
        }

        let paddr = self.virtual_to_physical_address(vaddr, AccessMode::Read);
        let (label_list, regenerate_index) = self.read_label_list(paddr, vaddr);

        let module = self.codegen.context.create_module("tmp");
        let globals = self.map_into(&module, &self.codegen.execution_engine);
        let lab = self
            .codegen
            .add_dynamic_function(module, globals, |codegen, module| {
                let mut labels = generate_labels(label_list, codegen.context, module);
                let lab = labels
                    .pop()
                    .ok_or(CompilationError::LabelFunctionGeneration { vaddr })?;
                lab.compile(codegen, self.trace || self.debugger.is_some())?;
                Ok(lab)
            })
            .unwrap_or_else(|err| self.panic_update_debugger(err.to_string().as_str()));

        if regenerate_index {
            // If we got a request to invalidate cached labels while disassembling, the `insert_index` may have been invalidated because we removed items.
            insert_index = self
                .codegen
                .labels
                .get(vaddr)
                .expect_err("no labels were inserted while generating block");
        }

        let ptr = lab.pointer.expect("label pointer is cached");
        unsafe {
            // SAFETY: Our insert index is valid.
            self.codegen.labels.insert(insert_index, lab);
        }
        ptr
    }

    // This will only be called if either the debugger or tracing is enabled.
    unsafe extern "C" fn on_instruction(&mut self) {
        let pc_vaddr = self.registers.read_program_counter();

        if self.trace {
            let paddr = self.virtual_to_physical_address(pc_vaddr, AccessMode::Read);
            let (instr_raw, _did_mutate) = self.read_raw_instruction(paddr);
            let instr: T::Instruction = instr_raw.try_into().unwrap_or_else(|_err| {
                let msg = format!("failed to parse instruction at {pc_vaddr:#x}");
                self.panic_update_debugger(&msg)
            });
            trace!("{pc_vaddr:06x}: {instr}");
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
        let slice = std::slice::from_raw_parts(string_ptr, string_len as usize);
        let message = match std::str::from_utf8(slice) {
            Ok(string) => format!("Environment::panic called: {string}"),
            Err(error) => format!("Environment::panic called with an invalid string: {error}"),
        };
        self.panic_update_debugger(&message)
    }
}
