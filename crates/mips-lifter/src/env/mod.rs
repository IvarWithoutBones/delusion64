use self::function::RuntimeFunction;
use crate::label::Label;
use inkwell::{
    execution_engine::ExecutionEngine, module::Module, values::GlobalValue, AddressSpace,
};
use std::pin::Pin;

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

pub struct Environment<'ctx, Mem: Memory> {
    labels: Vec<Label<'ctx>>,
    memory: Mem,

    general_purpose_registers: Pin<Box<[u64; 32]>>,
    cp0_registers: Pin<Box<[u64; 32]>>,
    hi_lo_registers: Pin<Box<[u64; 2]>>,
}

impl<'ctx, Mem: Memory> Environment<'ctx, Mem> {
    pub fn new(memory: Mem, labels: Vec<Label<'ctx>>) -> Pin<Box<Self>> {
        Box::pin(Self {
            labels,
            general_purpose_registers: Box::pin([0; 32]),
            cp0_registers: Box::pin([0; 32]),
            hi_lo_registers: Box::pin([0; 2]),
            memory,
        })
    }

    pub fn init(
        &self,
        module: &Module<'ctx>,
        execution_engine: &ExecutionEngine<'ctx>,
    ) -> (
        GlobalValue<'ctx>,
        GlobalValue<'ctx>,
        GlobalValue<'ctx>,
        GlobalValue<'ctx>,
    ) {
        let context = module.get_context();
        let i64_type = context.i64_type();
        let regs_array = i64_type.array_type(32);

        // Add a mapping to the environment struct
        let ptr_type = context.i64_type().ptr_type(AddressSpace::default());
        let env_struct = module.add_global(ptr_type, None, "env");
        execution_engine.add_global_mapping(&env_struct, self as *const _ as _);

        // Add a mapping to the general purpose registers
        let gprs = module.add_global(regs_array, None, "gprs");
        execution_engine.add_global_mapping(&gprs, self.general_purpose_registers.as_ptr() as _);

        // Add a mapping to the hi/lo registers
        let hi_lo = module.add_global(i64_type.array_type(2), None, "hi_lo");
        execution_engine.add_global_mapping(&hi_lo, self.hi_lo_registers.as_ptr() as _);

        // Add a mapping to the cp0 registers
        let cp0_regs = module.add_global(regs_array, None, "cp0_regs");
        execution_engine.add_global_mapping(&cp0_regs, self.cp0_registers.as_ptr() as _);

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

        (env_struct, gprs, hi_lo, cp0_regs)
    }

    pub fn print_registers(&self) {
        println!("\ngeneral registers:");
        for (i, r) in self.general_purpose_registers.iter().enumerate() {
            let name = mips_decomp::format::general_register_name(i as _);
            println!("{name: <8} = {r:#x}");
        }

        println!(
            "\nspecial registers:\nhi       = {:#x}\nlo       = {:#x}",
            self.hi_lo_registers[0], self.hi_lo_registers[1]
        );

        println!("\ncp0 registers:");
        for (i, r) in self.cp0_registers.iter().enumerate() {
            let name = mips_decomp::format::cp0_register_name(i as _);
            println!("{name: <8} = {r:#x}");
        }
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
