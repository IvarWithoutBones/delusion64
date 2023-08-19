//! The runtime environment which generated code can call into.

use self::memory::tlb::{AccessMode, TranslationLookasideBuffer};
use crate::{
    codegen::{self, function_attributes, CodeGen},
    LLVM_CALLING_CONVENTION_FAST,
};
use inkwell::{
    attributes::AttributeLoc, execution_engine::ExecutionEngine, module::Module,
    values::FunctionValue, AddressSpace,
};
use mips_decomp::{instruction::ParsedInstruction, register, INSTRUCTION_SIZE};
use std::{cell::Cell, fmt, net::TcpStream, pin::Pin};
use strum::IntoEnumIterator;

pub(crate) use self::function::RuntimeFunction;
pub use self::memory::Memory;

mod function;
mod gdb;
mod memory;

pub struct Registers {
    general_purpose: Pin<Box<[u64; register::GeneralPurpose::count()]>>,
    special: Pin<Box<[u64; register::Special::count()]>>,
    cp0: Pin<Box<[u64; register::Cp0::count()]>>,
    fpu: Pin<Box<[u64; register::Fpu::count()]>>,
}

pub struct Environment<'ctx, Mem>
where
    Mem: Memory,
{
    pub(crate) registers: Registers,
    memory: Mem,
    tlb: TranslationLookasideBuffer,
    codegen: Cell<Option<CodeGen<'ctx>>>,
    debugger: Option<gdb::Debugger<'ctx, Mem>>,
}

impl<'ctx, Mem> Environment<'ctx, Mem>
where
    Mem: Memory,
{
    pub fn new(memory: Mem, gdb_stream: Option<TcpStream>) -> Pin<Box<Self>> {
        let mut env = Self {
            registers: Registers::default(),
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
        let i64_type = context.i64_type();

        // Add a mapping to the environment struct
        let ptr_type = context.i64_type().ptr_type(AddressSpace::default());
        let env_ptr = module.add_global(ptr_type, None, "env");
        execution_engine.add_global_mapping(&env_ptr, self as *const _ as _);

        // Add a mapping to the general purpose registers
        let gprs_ty = i64_type.array_type(register::GeneralPurpose::count() as _);
        let general_purpose_regs = module.add_global(gprs_ty, None, "general_purpose_registers");
        execution_engine.add_global_mapping(
            &general_purpose_regs,
            self.registers.general_purpose.as_ptr() as _,
        );

        // Add a mapping to the special registers
        let special_regs_ty = i64_type.array_type(register::Special::count() as _);
        let special_regs = module.add_global(special_regs_ty, None, "special_registers");
        execution_engine.add_global_mapping(&special_regs, self.registers.special.as_ptr() as _);

        // Add a mapping to the cp0 registers
        let cp0_regs_ty = i64_type.array_type(register::Cp0::count() as _);
        let cp0_regs = module.add_global(cp0_regs_ty, None, "cp0_registers");
        execution_engine.add_global_mapping(&cp0_regs, self.registers.cp0.as_ptr() as _);

        // Add a mapping to the fpu (cp1) registers
        let fpu_regs_ty = i64_type.array_type(register::Fpu::count() as _);
        let fpu_regs = module.add_global(fpu_regs_ty, None, "fpu_registers");
        execution_engine.add_global_mapping(&fpu_regs, self.registers.fpu.as_ptr() as _);

        // Add a mapping to the host stack frame pointer.
        let stack_frame = module.add_global(ptr_type, None, "host_stack_frame");
        let stack_frame_storage = Box::new(0); // See the `globals` struct for why this is managed by the runtime.
        execution_engine
            .add_global_mapping(&stack_frame, stack_frame_storage.as_ref() as *const _ as _);

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
            general_purpose_regs,
            special_regs,
            cp0_regs,
            fpu_regs,
        }
    }

    pub fn attach_codegen(&self, codegen: CodeGen<'ctx>) {
        self.codegen.set(Some(codegen));
    }

    fn virtual_to_physical_address(&mut self, vaddr: u64, mode: AccessMode) -> u32 {
        self.tlb
            .translate(vaddr, mode, &self.registers)
            .unwrap_or_else(|err| {
                let msg = format!(
                    "failed to generate paddr for vaddr {vaddr:#06x}: {err:?}\n{:?}",
                    self.registers
                );
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
        let message = format!("memory read failed at vaddr={vaddr:#x} paddr={paddr:#x}: {err:?}",);
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

                    // TODO: generate the fallthrough label if none could be found.
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
                        let fallthrough_func = codegen.module.add_function(
                            "fallthrough",
                            codegen.context.void_type().fn_type(&[], false),
                            None,
                        );
                        fallthrough_func.set_call_conventions(LLVM_CALLING_CONVENTION_FAST);
                        for attr in function_attributes(codegen.context) {
                            fallthrough_func.add_attribute(AttributeLoc::Function, attr);
                        }

                        let fallthrough_block = codegen
                            .context
                            .append_basic_block(fallthrough_func, "fallthrough_block");
                        codegen.builder.position_at_end(fallthrough_block);

                        let pc = codegen
                            .read_register(codegen.context.i64_type(), register::Special::Pc);
                        let next = {
                            // If the second-last instruction has a delay slot, we've already executed the last instruction.
                            // In this case we need to skip over it, in order to avoid executing it twice.
                            let offset = if let Some(second_last) =
                                lab.label.instructions.iter().nth_back(1)
                            {
                                if second_last.has_delay_slot() {
                                    2
                                } else {
                                    1
                                }
                            } else {
                                1
                            };

                            codegen.builder.build_int_add(
                                pc,
                                codegen
                                    .context
                                    .i64_type()
                                    .const_int(offset * (INSTRUCTION_SIZE as u64), false),
                                "next_block_addr",
                            )
                        };

                        codegen.build_dynamic_jump(next);
                        lab.fallthrough_fn = Some(fallthrough_func);
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
        println!("{:?}", self.registers);
        panic!("Environment::panic called");
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
            general_purpose: Box::pin([0; register::GeneralPurpose::count()]),
            special: Box::pin([0; register::Special::count()]),
            cp0: Box::pin([0; register::Cp0::count()]),
            fpu: Box::pin([0; register::Fpu::count()]),
        }
    }
}

impl fmt::Debug for Registers {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "\ngeneral registers:")?;
        for (i, r) in self.general_purpose.iter().enumerate() {
            let name = register::GeneralPurpose::name_from_index(i as _);
            writeln!(f, "{name: <9} = {r:#x}")?;
        }

        writeln!(f, "\nspecial registers:")?;
        for (i, r) in self.special.iter().enumerate() {
            let name = register::Special::name_from_index(i as _);
            writeln!(f, "{name: <9} = {r:#x}")?;
        }

        writeln!(f, "\ncoprocessor 1 registers:")?;
        for (i, r) in self.cp0.iter().enumerate() {
            let name = register::Cp0::name_from_index(i as _);
            writeln!(f, "{name: <9} = {r:#x}")?;
        }

        writeln!(f, "\nfpu registers:")?;
        for (i, r) in self.fpu.iter().enumerate() {
            let name = register::Fpu::name_from_index(i as _);
            writeln!(f, "{name: <9} = {r:#x}")?;
        }

        Ok(())
    }
}

macro_rules! impl_index {
    ($ty:ty, $(($name:ident, $struct:ident :: $field:ident)),*) => {
        $(
            impl ::std::ops::Index<register::$name> for $struct {
                type Output = $ty;

                fn index(&self, reg: register::$name) -> &Self::Output {
                    &self.$field[reg as usize]
                }
            }

            impl ::std::ops::IndexMut<register::$name> for $struct {
                fn index_mut(&mut self, reg: register::$name) -> &mut Self::Output {
                    &mut self.$field[reg as usize]
                }
            }
        )*
    };
}

impl_index!(
    u64,
    (GeneralPurpose, Registers::general_purpose),
    (Special, Registers::special),
    (Cp0, Registers::cp0),
    (Fpu, Registers::fpu)
);
