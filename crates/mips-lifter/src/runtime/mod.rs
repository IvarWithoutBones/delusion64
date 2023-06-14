//! The runtime environment for generated code.

use self::memory::TranslationLookasideBuffer;
use crate::{
    codegen::{self, CodeGen},
    label::LabelWithContext,
};
use inkwell::{
    execution_engine::{ExecutionEngine, JitFunction},
    module::Module,
    AddressSpace,
};
use mips_decomp::{instruction::ParsedInstruction, register};
use std::{cell::RefCell, fmt, net::TcpStream, pin::Pin};
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
}

pub struct Environment<'ctx, Mem>
where
    Mem: Memory,
{
    pub(crate) registers: Registers,
    memory: Mem,
    tlb: TranslationLookasideBuffer,

    labels: Vec<LabelWithContext<'ctx>>,
    codegen: RefCell<Option<CodeGen<'ctx>>>,

    debugger: Option<gdb::Debugger<'ctx, Mem>>,
}

impl<'ctx, Mem> Environment<'ctx, Mem>
where
    Mem: Memory,
{
    pub fn new(
        memory: Mem,
        labels: Vec<LabelWithContext<'ctx>>,
        gdb_stream: Option<TcpStream>,
    ) -> Pin<Box<Self>> {
        let mut env = Self {
            registers: Registers::default(),
            tlb: TranslationLookasideBuffer::default(),
            debugger: None,
            codegen: Default::default(),
            labels,
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
        let general_purpose_regs = module.add_global(gprs_ty, None, "general_purpose_regs");
        execution_engine.add_global_mapping(
            &general_purpose_regs,
            self.registers.general_purpose.as_ptr() as _,
        );

        // Add a mapping to the special registers
        let special_regs_ty = i64_type.array_type(register::Special::count() as _);
        let special_regs = module.add_global(special_regs_ty, None, "special_regs");
        execution_engine.add_global_mapping(&special_regs, self.registers.special.as_ptr() as _);

        // Add a mapping to the cp0 registers
        let cp0_regs_ty = i64_type.array_type(register::Cp0::count() as _);
        let cp0_regs = module.add_global(cp0_regs_ty, None, "cp0_regs");
        execution_engine.add_global_mapping(&cp0_regs, self.registers.cp0.as_ptr() as _);

        // Add mappings to the runtime functions
        for func in RuntimeFunction::iter() {
            let ptr: *const u8 = match func {
                RuntimeFunction::GetBlockId => Self::block_id as _,
                RuntimeFunction::PrintString => Self::print_string as _,
                RuntimeFunction::OnInstruction => Self::on_instruction as _,
                RuntimeFunction::Panic => Self::panic as _,
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
            env_ptr,
            general_purpose_regs,
            special_regs,
            cp0_regs,
        }
    }

    pub fn attach_codegen(&self, codegen: CodeGen<'ctx>) {
        *self.codegen.borrow_mut() = Some(codegen);
    }

    pub fn print_registers(&self) {
        println!("{:#?}", self.registers);
    }

    fn translate_vaddr(&mut self, vaddr: u64) -> u64 {
        self.tlb.translate(vaddr).unwrap_or_else(|| {
            self.print_registers();
            panic!("failed to generate paddr for vaddr {vaddr:#06x}");
        })
    }

    /*
        Runtime functions. These are not meant to be called directly, but rather by generated code.
    */

    unsafe extern "C" fn block_id(&mut self, vaddr: u64) -> u64 {
        let paddr = self.translate_vaddr(vaddr);

        println!("\nblock_id FOUND INSTRUCTIONS ({paddr:#x})\n");
        for i in 0..10 {
            let addr = paddr + (i * 4);
            let instr = ParsedInstruction::try_from(self.memory.read_u32(addr)).unwrap();
            let vaddr = vaddr + (i * 4);
            println!("  {vaddr:06x}: {instr}");
        }
        println!();

        // TODO: This is not at all accurate, the physical address for a label could be anywhere.
        // We should instead hash a block by its instructions and do a lookup in a hashmap.
        let offset = ((paddr - 0x4000000) / 4) - 1;
        if let Some(label_with_ctx) = self
            .labels
            .iter()
            .find(|l| l.label.range().contains(&(offset as _)))
        {
            let start = label_with_ctx.label.start() as u64;
            if offset == start {
                // Already compiled a block for this address
                println!("block_id({paddr:#x}) = {start:#x}");
                return start;
            } else {
                // The target instruction lies in the middle of an existing block
                let target_block = start * 4;
                let offset = offset * 4;
                println!("ERROR: target block is {target_block:#x} but tried to jump to the middle ({paddr:#x}). offset={offset:#x}\n");
                return start;

                // panic!();

                let codegen = &mut self.codegen.borrow_mut();
                let codegen = codegen.as_mut().unwrap();

                codegen
                    .dynamic_add_fn(|codegen, module| {
                        let func = module.add_function(
                            "dynamic_block",
                            codegen.context.void_type().fn_type(&[], false),
                            None,
                        );

                        {
                            // https://github.com/llvm/llvm-project/blob/29293e6f9d8d20464ffcb9f9401c968674c90ea2/llvm/lib/Transforms/Utils/CloneFunction.cpp
                            for b in label_with_ctx.function.get_basic_blocks() {
                                let new_block = codegen
                                    .context
                                    .append_basic_block(func, b.get_name().to_str().unwrap());
                                codegen.builder.position_at_end(new_block);

                                let mut instr = b.get_first_instruction().unwrap();
                                println!("instr: {instr}");
                                codegen.builder.insert_instruction(&instr.clone(), None);
                                while let Some(next) = instr.get_next_instruction() {
                                    println!("instr: {next}");
                                    let name = next.get_name().map(|name| name.to_str().unwrap());
                                    codegen.builder.insert_instruction(&next.clone(), name);
                                    instr = next;
                                }
                            }
                        }

                        func
                    })
                    .unwrap_or_else(|e| {
                        codegen.module.print_to_file("test/gen.ll").unwrap();
                        println!("failed to add dynamic block: {e}");
                        panic!();
                    });

                println!("called dynamic_block, recompiling...");
                let block_fn: JitFunction<unsafe extern "C" fn()> = unsafe {
                    codegen
                        .execution_engine
                        .get_function("dynamic_block")
                        .unwrap()
                };
                block_fn.call();
            }
        } else {
            println!("ERROR: no block contains offset {offset:#x}");
        }

        self.print_registers();
        panic!("failed to find label for address {paddr:#x}");
    }

    unsafe extern "C" fn on_instruction(&mut self) {
        let pc_vaddr = self.registers[register::Special::Pc];
        let pc_paddr = self.translate_vaddr(pc_vaddr);

        let instr = ParsedInstruction::try_from(self.memory.read_u32(pc_paddr)).unwrap();
        println!("{pc_vaddr:06x}: {instr}");

        if self.debugger.is_some() {
            self.debugger.as_mut().unwrap().on_instruction(pc_paddr);
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
        self.print_registers();
        panic!("Environment::panic called");
    }

    unsafe extern "C" fn read_u8(&mut self, vaddr: u64) -> u8 {
        let paddr = self.translate_vaddr(vaddr);
        self.memory.read_u8(paddr)
    }

    unsafe extern "C" fn read_u16(&mut self, vaddr: u64) -> u16 {
        let paddr = self.translate_vaddr(vaddr);
        self.memory.read_u16(paddr)
    }

    unsafe extern "C" fn read_u32(&mut self, vaddr: u64) -> u32 {
        let paddr = self.translate_vaddr(vaddr);
        self.memory.read_u32(paddr)
    }

    unsafe extern "C" fn read_u64(&mut self, vaddr: u64) -> u64 {
        let paddr = self.translate_vaddr(vaddr);
        self.memory.read_u64(paddr)
    }

    unsafe extern "C" fn write_u8(&mut self, vaddr: u64, value: u8) {
        let paddr = self.translate_vaddr(vaddr);
        self.memory.write_u8(paddr, value)
    }

    unsafe extern "C" fn write_u16(&mut self, vaddr: u64, value: u16) {
        let paddr = self.translate_vaddr(vaddr);
        self.memory.write_u16(paddr, value)
    }

    unsafe extern "C" fn write_u32(&mut self, vaddr: u64, value: u32) {
        let paddr = self.translate_vaddr(vaddr);
        self.memory.write_u32(paddr, value)
    }

    unsafe extern "C" fn write_u64(&mut self, vaddr: u64, value: u64) {
        let paddr = self.translate_vaddr(vaddr);
        self.memory.write_u64(paddr, value)
    }
}

impl Default for Registers {
    fn default() -> Self {
        Self {
            general_purpose: Box::pin([0; register::GeneralPurpose::count()]),
            special: Box::pin([0; register::Special::count()]),
            cp0: Box::pin([0; register::Cp0::count()]),
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

        writeln!(f, "\ncoprocessor registers:")?;
        for (i, r) in self.cp0.iter().enumerate() {
            let name = register::Cp0::name_from_index(i as _);
            writeln!(f, "{name: <9} = {r:#x}")?;
        }

        Ok(())
    }
}

macro_rules! impl_index {
    ($(($name:ident, $struct:ident :: $field:ident)),*) => {
        $(
            impl ::std::ops::Index<register::$name> for $struct {
                type Output = u64;

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
    (GeneralPurpose, Registers::general_purpose),
    (Special, Registers::special),
    (Cp0, Registers::cp0)
);
