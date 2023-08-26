//! The runtime environment which generated code can call into.

use self::memory::tlb::{AccessMode, TranslationLookasideBuffer};
use crate::{
    codegen::{self, CodeGen, FallthroughAmount, RegisterGlobals},
    InitialRegisters,
};
use inkwell::{
    context::ContextRef,
    execution_engine::ExecutionEngine,
    module::Module,
    values::{FunctionValue, GlobalValue},
};
use mips_decomp::{
    instruction::ParsedInstruction,
    register::{self, Register},
};
use std::{
    cell::{Cell, UnsafeCell},
    fmt,
    net::TcpStream,
    pin::Pin,
};
use strum::IntoEnumIterator;

pub(crate) use self::function::RuntimeFunction;
pub use self::memory::Memory;

mod function;
mod gdb;
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
}

pub struct Environment<'ctx, Mem>
where
    Mem: Memory,
{
    pub(crate) registers: Pin<Box<Registers>>,
    memory: Mem,
    tlb: TranslationLookasideBuffer,
    codegen: Cell<Option<CodeGen<'ctx>>>,
    debugger: Option<gdb::Debugger<'ctx, Mem>>,
}

impl<'ctx, Mem> Environment<'ctx, Mem>
where
    Mem: Memory,
{
    pub fn new(
        memory: Mem,
        regs: InitialRegisters,
        gdb_stream: Option<TcpStream>,
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
            memory,
        };

        if let Some(stream) = gdb_stream {
            env.debugger = Some(gdb::Debugger::new(&mut env, stream));
        }

        Box::pin(env)
    }

    pub fn map_into(
        &self,
        module: &Module<'ctx>,
        execution_engine: &ExecutionEngine<'ctx>,
    ) -> codegen::Globals<'ctx> {
        let context = module.get_context();
        let ptr_type = context.i64_type().ptr_type(Default::default());

        // Add a mapping to the environment struct
        let env_ptr = module.add_global(ptr_type, Default::default(), "env");
        execution_engine.add_global_mapping(&env_ptr, self as *const Environment<_> as usize);

        // Add a mapping to the host stack frame pointer.
        let stack_frame = module.add_global(ptr_type, Default::default(), "host_stack_frame");
        let stack_frame_storage = Box::into_raw(Box::new(0u64));
        execution_engine.add_global_mapping(&stack_frame, stack_frame_storage as usize);

        // Map the registers into the modules globals
        let registers = self.registers.map_into(&context, module, execution_engine);

        // Add mappings to the runtime functions
        for func in RuntimeFunction::iter() {
            let ptr: *const u8 = match func {
                RuntimeFunction::PrintString => Self::print_string as _,
                RuntimeFunction::Panic => Self::panic as _,
                RuntimeFunction::OnInstruction => Self::on_instruction as _,

                RuntimeFunction::GetFunctionPtr => Self::get_function_ptr as _,
                RuntimeFunction::WriteTlbEntry => Self::write_tlb_entry as _,

                RuntimeFunction::ReadI8 => Self::read_u8 as _,
                RuntimeFunction::ReadI16 => Self::read_u16 as _,
                RuntimeFunction::ReadI32 => Self::read_u32 as _,
                RuntimeFunction::ReadI64 => Self::read_u64 as _,

                RuntimeFunction::WriteI8 => Self::write_u8 as _,
                RuntimeFunction::WriteI16 => Self::write_u16 as _,
                RuntimeFunction::WriteI32 => Self::write_u32 as _,
                RuntimeFunction::WriteI64 => Self::write_u64 as _,
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
            .translate(vaddr, mode, &self.registers)
            .unwrap_or_else(|err| {
                let msg = format!("failed to generate paddr for vaddr {vaddr:#06x}: {err:#?}");
                self.panic_update_debugger(&msg)
            })
    }

    fn panic_update_debugger(&mut self, message: &str) -> ! {
        eprintln!("{:?}", self.registers);
        if self.debugger.is_some() {
            eprintln!("{message}");
            self.debugger.as_mut().unwrap().signal_panicked();
            loop {
                self.update_debugger()
            }
        } else {
            panic!("{message}");
        }
    }

    fn panic_read_failed<E>(&mut self, err: E, vaddr: u64, paddr: u32) -> !
    where
        E: fmt::Debug,
    {
        let message = format!("memory read failed at vaddr={vaddr:#x} paddr={paddr:#x}: {err:?}");
        self.panic_update_debugger(&message)
    }

    fn panic_write_failed<T, E>(&mut self, err: E, vaddr: u64, paddr: u32, value: T) -> !
    where
        T: fmt::LowerHex,
        E: fmt::Debug,
    {
        let message = format!(
            "memory write of {value:#x} failed at vaddr={vaddr:#x} paddr={paddr:#x}: {err:?}",
        );
        self.panic_update_debugger(&message)
    }

    /*
        Runtime functions. These are not meant to be called directly, but rather by generated code.
    */

    // TODO: split this up and prettify it a bit, it is rather unwieldy right now.
    unsafe extern "C" fn get_function_ptr(&mut self, vaddr: u64) -> u64 {
        let vaddr = vaddr & u32::MAX as u64;
        println!("block_id({vaddr:#x})");

        // This is a closure so we can return early if we already a matching block compiled.
        let func = || -> FunctionValue<'ctx> {
            let paddr = self.virtual_to_physical_address(vaddr, AccessMode::Read);

            // This is not accurate, just a hack consistent with label.rs.
            let offset = vaddr as usize / 4;
            let codegen = &mut self.codegen.get_mut().as_mut().unwrap();

            if let Some(existing) = codegen.labels.iter().find(|l| l.label.start() == offset) {
                println!("  found existing block at {vaddr:#x} (offset={offset:#x})");
                let name = existing.function.get_name().to_str().unwrap();
                println!("  using existing function: '{name}'");
                return existing.function;
            } else {
                println!("  generating new block at {vaddr:#x} (offset={offset:#x})");
            }

            let bin = {
                let mut addr = paddr;
                let mut break_after = None;
                let mut bin: Vec<u8> = Vec::new();
                loop {
                    let value = self.memory.read_u32(addr).unwrap();
                    if let Ok(instr) = ParsedInstruction::try_from(value) {
                        if instr.has_delay_slot() {
                            break_after = Some(2);
                        } else if instr.ends_block() {
                            println!("  {:#010x}: {instr}", addr);
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
                    let mut lab =
                        crate::label::generate_label_functions(label_list, codegen.context, module)
                            .pop()
                            .unwrap();
                    if let Some(fallthrough) = codegen
                        .labels
                        .iter()
                        .find(|l| l.label.start() == lab.label.end())
                    {
                        println!(
                            "  found fallthrough block at {:#x}",
                            fallthrough.label.start(),
                        );

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
                .unwrap();

            let func = lab.function;
            codegen.labels.push(lab);

            codegen.module.print_to_file("test/a.ll").unwrap();

            func
        }();

        let name = func.get_name().to_str().unwrap();
        println!("  executing '{name}'");

        self.codegen
            .get_mut()
            .as_mut()
            .unwrap()
            .execution_engine
            .get_function_address(name)
            .unwrap() as _
    }

    unsafe extern "C" fn on_instruction(&mut self) {
        let pc_vaddr = self.registers[register::Special::Pc];
        let pc_paddr = self.virtual_to_physical_address(pc_vaddr, AccessMode::Read);

        let instr = ParsedInstruction::try_from(self.memory.read_u32(pc_paddr).unwrap()).unwrap();
        println!("{pc_vaddr:06x}: {instr}");

        if self.debugger.is_some() {
            self.debugger.as_mut().unwrap().on_instruction(pc_vaddr);
            self.update_debugger();
            while self.debugger.as_ref().unwrap().is_paused() {
                // Block until the debugger tells us to continue
                self.update_debugger();
            }
        }
    }

    unsafe extern "C" fn print_string(&mut self, string_ptr: *const u8, len: u64) {
        let slice = std::slice::from_raw_parts(string_ptr, len as _);
        let string = std::str::from_utf8(slice).unwrap();
        print!("{string}");
    }

    unsafe extern "C" fn panic(&mut self) {
        self.panic_update_debugger("Environment::panic called");
    }

    unsafe extern "C" fn write_tlb_entry(&mut self, index: u64) {
        self.tlb
            .set_entry(index as usize, &self.registers)
            .unwrap_or_else(|| {
                let msg = format!("failed to set tlb entry at {index:#x}");
                self.panic_update_debugger(&msg)
            });
    }

    unsafe extern "C" fn read_u8(&mut self, vaddr: u64) -> u8 {
        let paddr = self.virtual_to_physical_address(vaddr, AccessMode::Read);
        self.memory
            .read_u8(paddr)
            .unwrap_or_else(|err| self.panic_read_failed(err, vaddr, paddr))
    }

    unsafe extern "C" fn read_u16(&mut self, vaddr: u64) -> u16 {
        let paddr = self.virtual_to_physical_address(vaddr, AccessMode::Read);
        self.memory
            .read_u16(paddr)
            .unwrap_or_else(|err| self.panic_read_failed(err, vaddr, paddr))
    }

    unsafe extern "C" fn read_u32(&mut self, vaddr: u64) -> u32 {
        let paddr = self.virtual_to_physical_address(vaddr, AccessMode::Read);
        self.memory
            .read_u32(paddr)
            .unwrap_or_else(|err| self.panic_read_failed(err, vaddr, paddr))
    }

    unsafe extern "C" fn read_u64(&mut self, vaddr: u64) -> u64 {
        let paddr = self.virtual_to_physical_address(vaddr, AccessMode::Read);
        self.memory
            .read_u64(paddr)
            .unwrap_or_else(|err| self.panic_read_failed(err, vaddr, paddr))
    }

    unsafe extern "C" fn write_u8(&mut self, vaddr: u64, value: u8) {
        let paddr = self.virtual_to_physical_address(vaddr, AccessMode::Write);
        self.memory
            .write_u8(paddr, value)
            .unwrap_or_else(|err| self.panic_write_failed(err, vaddr, paddr, value))
    }

    unsafe extern "C" fn write_u16(&mut self, vaddr: u64, value: u16) {
        let paddr = self.virtual_to_physical_address(vaddr, AccessMode::Write);
        self.memory
            .write_u16(paddr, value)
            .unwrap_or_else(|err| self.panic_write_failed(err, vaddr, paddr, value))
    }

    unsafe extern "C" fn write_u32(&mut self, vaddr: u64, value: u32) {
        let paddr = self.virtual_to_physical_address(vaddr, AccessMode::Write);
        self.memory
            .write_u32(paddr, value)
            .unwrap_or_else(|err| self.panic_write_failed(err, vaddr, paddr, value))
    }

    unsafe extern "C" fn write_u64(&mut self, vaddr: u64, value: u64) {
        let paddr = self.virtual_to_physical_address(vaddr, AccessMode::Write);
        self.memory
            .write_u64(paddr, value)
            .unwrap_or_else(|err| self.panic_write_failed(err, vaddr, paddr, value))
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
