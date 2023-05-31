use self::function::RuntimeFunction;
use crate::{codegen, label::Label};
use inkwell::{execution_engine::ExecutionEngine, module::Module, AddressSpace};
use mips_decomp::register::{
    self, CoprocessorRegisters, GeneralPurposeRegisters, SpecialRegisters,
};
use std::{fmt, pin::Pin};

pub mod function;

pub trait Memory {
    fn read_u8(&self, addr: u64) -> u8;
    fn read_u16(&self, addr: u64) -> u16;
    fn read_u32(&self, addr: u64) -> u32;
    fn read_u64(&self, addr: u64) -> u64;

    fn write_u8(&mut self, addr: u64, value: u8);
    fn write_u16(&mut self, addr: u64, value: u16);
    fn write_u32(&mut self, addr: u64, value: u32);
    fn write_u64(&mut self, addr: u64, value: u64);
}

pub struct Registers {
    general_purpose: Pin<Box<GeneralPurposeRegisters>>,
    special: Pin<Box<SpecialRegisters>>,
    coprocessor: Pin<Box<CoprocessorRegisters>>,
}

impl Default for Registers {
    fn default() -> Self {
        Self {
            general_purpose: Box::pin(GeneralPurposeRegisters::default()),
            special: Box::pin(SpecialRegisters::default()),
            coprocessor: Box::pin(CoprocessorRegisters::default()),
        }
    }
}

impl fmt::Debug for Registers {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "\ngeneral registers:")?;
        for (i, r) in self.general_purpose.0.iter().enumerate() {
            let name = register::GeneralPurpose::name_from_index(i as _);
            writeln!(f, "{name: <9} = {r:#x}")?;
        }

        writeln!(f, "\nspecial registers:")?;
        for (i, r) in self.special.0.iter().enumerate() {
            let name = register::Special::name_from_index(i as _);
            writeln!(f, "{name: <9} = {r:#x}")?;
        }

        writeln!(f, "\ncoprocessor registers:")?;
        for (i, r) in self.coprocessor.0.iter().enumerate() {
            let name = register::Coprocessor::name_from_index(i as _);
            writeln!(f, "{name: <9} = {r:#x}")?;
        }

        Ok(())
    }
}

pub struct Environment<'ctx, Mem: Memory> {
    labels: Vec<Label<'ctx>>,
    memory: Mem,
    pub registers: Registers,
}

impl<'ctx, Mem: Memory> Environment<'ctx, Mem> {
    pub fn new(memory: Mem, labels: Vec<Label<'ctx>>) -> Pin<Box<Self>> {
        Box::pin(Self {
            registers: Registers::default(),
            labels,
            memory,
        })
    }

    pub fn init(
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
            self.registers.general_purpose.0.as_ptr() as _,
        );

        // Add a mapping to the special registers
        let special_regs_ty = i64_type.array_type(register::Special::count() as _);
        let special_regs = module.add_global(special_regs_ty, None, "special_regs");
        execution_engine.add_global_mapping(&special_regs, self.registers.special.0.as_ptr() as _);

        // Add a mapping to the cp0 registers
        let cp0_regs_ty = i64_type.array_type(register::Coprocessor::count() as _);
        let coprocessor_regs = module.add_global(cp0_regs_ty, None, "cp0_regs");
        execution_engine.add_global_mapping(
            &coprocessor_regs,
            self.registers.coprocessor.0.as_ptr() as _,
        );

        let add_fn = |f: RuntimeFunction, ptr: *const u8| {
            f.init(&context, module, execution_engine, ptr);
        };

        // Add mappings to the runtime functions
        add_fn(RuntimeFunction::GetBlockId, Self::block_id as _);
        add_fn(RuntimeFunction::PrintString, Self::print_string as _);
        add_fn(RuntimeFunction::ReadI8, Self::read_u8 as _);
        add_fn(RuntimeFunction::ReadI16, Self::read_u16 as _);
        add_fn(RuntimeFunction::ReadI32, Self::read_u32 as _);
        add_fn(RuntimeFunction::ReadI64, Self::read_u64 as _);
        add_fn(RuntimeFunction::WriteI8, Self::write_u8 as _);
        add_fn(RuntimeFunction::WriteI16, Self::write_u16 as _);
        add_fn(RuntimeFunction::WriteI32, Self::write_u32 as _);
        add_fn(RuntimeFunction::WriteI64, Self::write_u64 as _);

        codegen::Globals {
            env_ptr,
            general_purpose_regs,
            special_regs,
            coprocessor_regs,
        }
    }

    pub fn print_registers(&self) {
        println!("{:#?}", self.registers);
    }

    /*
        Runtime functions
    */

    unsafe extern "C" fn print_string(&mut self, string_ptr: *const u8, len: u64) {
        let slice = std::slice::from_raw_parts(string_ptr, len as _);
        let string = std::str::from_utf8(slice).unwrap();
        print!("{string}");
    }

    unsafe extern "C" fn block_id(&mut self, addr: u64) -> u64 {
        if let Some(label) = self.labels.iter().find(|l| l.start_address == addr) {
            let id = label.id;
            println!("block_id({addr:#x}) = {id}"); // TODO: remove
            id
        } else {
            self.print_registers();
            panic!("failed to find label for address {addr:#x}");
        }
    }

    unsafe extern "C" fn read_u8(&mut self, addr: u64) -> u8 {
        self.memory.read_u8(addr)
    }

    unsafe extern "C" fn read_u16(&mut self, addr: u64) -> u16 {
        self.memory.read_u16(addr)
    }

    unsafe extern "C" fn read_u32(&mut self, addr: u64) -> u32 {
        self.memory.read_u32(addr)
    }

    unsafe extern "C" fn read_u64(&mut self, addr: u64) -> u64 {
        self.memory.read_u64(addr)
    }

    unsafe extern "C" fn write_u8(&mut self, addr: u64, value: u8) {
        self.memory.write_u8(addr, value)
    }

    unsafe extern "C" fn write_u16(&mut self, addr: u64, value: u16) {
        self.memory.write_u16(addr, value)
    }

    unsafe extern "C" fn write_u32(&mut self, addr: u64, value: u32) {
        self.memory.write_u32(addr, value)
    }

    unsafe extern "C" fn write_u64(&mut self, addr: u64, value: u64) {
        self.memory.write_u64(addr, value)
    }
}
