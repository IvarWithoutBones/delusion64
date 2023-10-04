//! The runtime environment which generated code can call into.

use self::memory::tlb::{AccessMode, TranslationLookasideBuffer};
use crate::{
    codegen::{self, CodeGen, FallthroughAmount, RegisterGlobals},
    InitialRegisters,
};
use inkwell::{
    context::ContextRef, execution_engine::ExecutionEngine, module::Module, values::GlobalValue,
};
use mips_decomp::{
    instruction::ParsedInstruction,
    register::{self, Register},
    Exception,
};
use std::{
    cell::{Cell, UnsafeCell},
    fmt,
    pin::Pin,
};
use strum::IntoEnumIterator;

pub(crate) use self::function::RuntimeFunction;

pub mod bus;
mod function;
pub(crate) mod gdb;
mod memory;

pub(crate) struct Registers {
    general_purpose: UnsafeCell<[u64; register::GeneralPurpose::count()]>,
    cp0: UnsafeCell<[u64; register::Cp0::count()]>,
    fpu: UnsafeCell<[u64; register::Fpu::count()]>,
    special: UnsafeCell<[u64; register::Special::count()]>,
}

impl Registers {
    fn map_into<'ctx>(
        &self,
        context: &ContextRef<'ctx>,
        module: &Module<'ctx>,
        execution_engine: &ExecutionEngine<'ctx>,
    ) -> RegisterGlobals<'ctx> {
        let i64_type = context.i64_type();
        let map_array = |name: &str, ptr: *const [u64], len: usize| -> GlobalValue<'ctx> {
            let ty = i64_type.array_type(len as u32);
            let arr = module.add_global(ty, Default::default(), name);
            execution_engine.add_global_mapping(&arr, ptr.cast::<*const u8>() as usize);
            arr
        };

        let cp0 = map_array("cp0_registers", self.cp0.get(), register::Cp0::count());
        let fpu = map_array("fpu_registers", self.fpu.get(), register::Fpu::count());
        let general_purpose = map_array(
            "general_purpose_registers",
            self.general_purpose.get(),
            register::GeneralPurpose::count(),
        );
        let special = map_array(
            "special_registers",
            self.special.get(),
            register::Special::count(),
        );

        RegisterGlobals {
            general_purpose,
            cp0,
            fpu,
            special,
        }
    }

    pub fn general_purpose(&self) -> impl Iterator<Item = u64> {
        unsafe { (*self.general_purpose.get()).iter().copied() }
    }

    pub fn cp0(&self) -> impl Iterator<Item = u64> {
        unsafe { (*self.cp0.get()).iter().copied() }
    }

    pub fn fpu(&self) -> impl Iterator<Item = u64> {
        unsafe { (*self.fpu.get()).iter().copied() }
    }

    pub fn special(&self) -> impl Iterator<Item = u64> {
        unsafe { (*self.special.get()).iter().copied() }
    }

    pub fn status(&self) -> register::cp0::Status {
        register::cp0::Status::new(self[register::Cp0::Status] as u32)
    }

    pub fn cause(&self) -> register::cp0::Cause {
        register::cp0::Cause::new(self[register::Cp0::Cause] as u32)
    }

    pub fn set_cause(&mut self, cause: register::cp0::Cause) {
        let raw: u32 = cause.into();
        self[register::Cp0::Cause] = raw as u64;
    }
}

pub struct Environment<'ctx, Bus: bus::Bus> {
    pub(crate) registers: Pin<Box<Registers>>,
    pub(crate) interrupt_pending: bool,
    bus: Bus,
    tlb: TranslationLookasideBuffer,
    codegen: Cell<Option<CodeGen<'ctx>>>,
    debugger: Option<gdb::Debugger<'ctx, Bus>>,
}

impl<'ctx, Bus: bus::Bus> Environment<'ctx, Bus> {
    pub fn new(
        bus: Bus,
        regs: InitialRegisters,
        gdb: Option<gdb::Connection<Bus>>,
    ) -> Pin<Box<Self>> {
        let mut registers = Registers::default();
        for (reg, val) in regs {
            registers[*reg] = *val;
        }

        let mut env = Self {
            registers: Box::pin(registers),
            tlb: TranslationLookasideBuffer::default(),
            debugger: None,
            codegen: Default::default(),
            bus,
            interrupt_pending: false,
        };

        if let Some(gdb) = gdb {
            env.debugger = Some(gdb::Debugger::new(&mut env, gdb));
        }

        Box::pin(env)
    }

    pub fn map_into(
        &self,
        module: &Module<'ctx>,
        execution_engine: &ExecutionEngine<'ctx>,
    ) -> codegen::Globals<'ctx> {
        let context = module.get_context();
        let i64_ptr_type = context.i64_type().ptr_type(Default::default());

        // Map a pointer to the environment struct
        let env_ptr = module.add_global(i64_ptr_type, Default::default(), "env");
        execution_engine.add_global_mapping(&env_ptr, self as *const Environment<_> as usize);

        // Map the host stack frame pointer.
        let stack_frame = module.add_global(i64_ptr_type, Default::default(), "host_stack_frame");
        let stack_frame_storage = Box::into_raw(Box::new(0u64));
        execution_engine.add_global_mapping(&stack_frame, stack_frame_storage as usize);

        // Map the registers into the modules globals
        let registers = self.registers.map_into(&context, module, execution_engine);

        // Map the runtime functions
        for func in RuntimeFunction::iter() {
            let ptr: *const u8 = match func {
                RuntimeFunction::Panic => Self::panic as _,
                RuntimeFunction::OnInstruction => Self::on_instruction as _,
                RuntimeFunction::GetFunctionPtr => Self::get_function_ptr as _,

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

            func.map_into(&context, module, execution_engine, ptr);
        }

        codegen::Globals {
            stack_frame: (stack_frame, stack_frame_storage),
            env_ptr,
            registers,
        }
    }

    pub fn attach_codegen(&self, codegen: CodeGen<'ctx>) {
        self.codegen.set(Some(codegen));
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
        if self.debugger.is_some() {
            self.debugger.as_mut().unwrap().signal_panicked();
            loop {
                self.update_debugger()
            }
        } else {
            panic!();
        }
    }

    pub(crate) fn handle_exception(&mut self, exception: Exception) {
        if self
            .registers
            .status()
            .diagnostic_status()
            .bootstrap_exception_vectors()
        {
            todo!("bootstrap exception vectors");
        }

        // TODO: handle coprocessor field
        let new_cause: u32 = self
            .registers
            .cause()
            .with_exception_code(exception.into())
            .into();
        self.registers[register::Cp0::Cause] = new_cause as u64;

        if !self.registers.status().exception_level() {
            // TODO: if we are inside of a delay slot, this should be the address of the previous instruction,
            // so that we dont skip over the branch. We also need to set `BD` in the Cause register when this happens.
            let pc_vaddr = self.registers[register::Special::Pc];
            self.registers[register::Cp0::EPC] = pc_vaddr;

            let new_status: u32 = self.registers.status().with_exception_level(true).into();
            self.registers[register::Cp0::Status] = new_status as u64;
        }

        // Will set PC for us
        let jump_function = self.codegen.get_mut().as_ref().unwrap().jump_function();
        unsafe { jump_function.call(exception.vector() as u64) }
    }

    /*
        Runtime functions. These are not meant to be called directly, but rather by JIT'ed code.
    */

    unsafe extern "C" fn get_physical_address(&mut self, vaddr: u64) -> u32 {
        self.virtual_to_physical_address(vaddr, AccessMode::Read)
    }

    // TODO: split this up and prettify it a bit, it is rather unwieldy right now.
    unsafe extern "C" fn get_function_ptr(&mut self, vaddr: u64) -> u64 {
        let vaddr = vaddr & u32::MAX as u64;

        // This is a closure so we can return early if we already a matching block compiled.
        let (codegen, name) = || -> (CodeGen<'ctx>, String) {
            let paddr = self.virtual_to_physical_address(vaddr, AccessMode::Read);
            let offset = vaddr as usize / 4;
            let mut codegen = self.codegen.take().unwrap();

            if let Some(existing) = codegen.labels.iter().find(|l| l.label.start() == offset) {
                // println!("found existing block at {vaddr:#x} (offset={offset:#x})");
                let name = existing.function.get_name().to_str().unwrap().to_string();
                return (codegen, name);
            } else {
                // println!("generating new block at {vaddr:#x} (offset={offset:#x})");
            }

            let bin = {
                let mut addr = paddr;
                let mut break_after = None;
                let mut bin: Vec<u8> = Vec::new();
                loop {
                    let value = u32::from_be_bytes(*self.read(addr).unwrap().as_slice());
                    if let Ok(instr) = ParsedInstruction::try_from(value) {
                        if instr.has_delay_slot() {
                            break_after = Some(2);
                        } else if instr.ends_block() {
                            bin.extend_from_slice(&value.to_be_bytes());
                            break;
                        }
                    } else {
                        break;
                    }

                    bin.extend_from_slice(&value.to_be_bytes());

                    if let Some(break_after) = break_after.as_mut() {
                        *break_after -= 1;
                        if *break_after == 0 {
                            break;
                        }
                    }

                    addr += 4;
                }

                bin.into_boxed_slice()
            };

            let mut label_list = mips_decomp::LabelList::from(&*bin);
            label_list.set_offset(offset);

            let lab = codegen
                .add_dynamic_function(|codegen, module| {
                    // NOTE: we're never going to append to the previous module, so its fine to replace those globals.
                    codegen.globals = self.map_into(module, &codegen.execution_engine);

                    let mut lab =
                        crate::label::generate_label_functions(label_list, codegen.context, module)
                            .pop()
                            .unwrap();
                    if let Some(fallthrough) = codegen
                        .labels
                        .iter()
                        .find(|l| l.label.start() == lab.label.end())
                    {
                        // println!(
                        //     "  found fallthrough block at {:#x}",
                        //     fallthrough.label.start(),
                        // );

                        // See the comment in `label.rs` for more context.
                        if fallthrough.label.instructions.len() == 1 {
                            lab.fallthrough_instr = Some(fallthrough.label.instructions[0].clone());
                            lab.fallthrough_fn = fallthrough.fallthrough_fn;
                        } else {
                            lab.fallthrough_fn = Some(fallthrough.function);
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

                    lab.compile(codegen);

                    lab
                })
                .unwrap_or_else(|err| panic!("{err}"));

            let name = lab.function.get_name().to_str().unwrap().to_string();
            codegen.labels.push(lab);
            codegen.module.print_to_file("test/a.ll").unwrap();
            (codegen, name)
        }();

        let ptr = codegen
            .execution_engine
            .get_function_address(&name)
            .unwrap();
        self.codegen.set(Some(codegen));
        ptr as u64
    }

    unsafe extern "C" fn on_instruction(&mut self) {
        if self.interrupt_pending {
            self.interrupt_pending = false;
            self.handle_exception(Exception::Interrupt);
        }

        let pc_vaddr = self.registers[register::Special::Pc];
        // let pc_paddr = self.virtual_to_physical_address(pc_vaddr, AccessMode::Read);

        // let instr = ParsedInstruction::try_from(u32::from_be_bytes(
        //     *self.read(pc_paddr).unwrap().as_slice(),
        // ))
        // .unwrap();
        // println!("{pc_vaddr:06x}: {instr}");

        self.bus
            .tick()
            .unwrap_or_else(|err| {
                let msg = format!("failed to tick: {err:#?}");
                self.panic_update_debugger(&msg)
            })
            .handle(self);

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

impl Default for Registers {
    fn default() -> Self {
        Self {
            general_purpose: UnsafeCell::new([0; register::GeneralPurpose::count()]),
            cp0: UnsafeCell::new([0; register::Cp0::count()]),
            fpu: UnsafeCell::new([0; register::Fpu::count()]),
            special: UnsafeCell::new([0; register::Special::count()]),
        }
    }
}

impl fmt::Debug for Registers {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "\ngeneral registers:")?;
        for (i, r) in self.general_purpose().enumerate() {
            let name = register::GeneralPurpose::name_from_index(i as _);
            writeln!(f, "{name: <9} = {r:#x}")?;
        }
        writeln!(f, "\ncoprocessor 1 registers:")?;
        for (i, r) in self.cp0().enumerate() {
            let name = register::Cp0::name_from_index(i as _);
            writeln!(f, "{name: <9} = {r:#x}")?;
        }
        writeln!(f, "\nspecial registers:")?;
        for (i, r) in self.special().enumerate() {
            let name = register::Special::name_from_index(i as _);
            writeln!(f, "{name: <9} = {r:#x}")?;
        }
        writeln!(f, "\nfpu registers:")?;
        for (i, r) in self.fpu().enumerate() {
            let name = register::Fpu::name_from_index(i as _);
            writeln!(f, "{name: <9} = {r:#x}")?;
        }
        Ok(())
    }
}

macro_rules! impl_index {
    ($ty:ty, $(($idx:path, $for:ident :: $field:ident)),*) => {
        $(
            impl ::std::ops::Index<$idx> for $for {
                type Output = $ty;

                fn index(&self, idx: $idx) -> &Self::Output {
                    unsafe { &self.$field.get().as_ref().unwrap()[idx as usize] }
                }
            }

            impl ::std::ops::IndexMut<$idx> for $for {
                fn index_mut(&mut self, idx: $idx) -> &mut Self::Output {
                    unsafe { &mut self.$field.get().as_mut().unwrap()[idx as usize] }
                }
            }
        )*
    };
}

impl_index!(
    u64,
    (register::GeneralPurpose, Registers::general_purpose),
    (register::Special, Registers::special),
    (register::Cp0, Registers::cp0),
    (register::Fpu, Registers::fpu)
);

impl std::ops::Index<Register> for Registers {
    type Output = u64;

    fn index(&self, index: Register) -> &Self::Output {
        match index {
            Register::GeneralPurpose(r) => &self[r],
            Register::Special(r) => &self[r],
            Register::Cp0(r) => &self[r],
            Register::Fpu(r) => &self[r],
        }
    }
}

impl std::ops::IndexMut<Register> for Registers {
    fn index_mut(&mut self, index: Register) -> &mut Self::Output {
        match index {
            Register::GeneralPurpose(r) => &mut self[r],
            Register::Special(r) => &mut self[r],
            Register::Cp0(r) => &mut self[r],
            Register::Fpu(r) => &mut self[r],
        }
    }
}
