use crate::label::Label;
use inkwell::{
    context::ContextRef, execution_engine::ExecutionEngine, module::Module, types::FunctionType,
    AddressSpace,
};
use std::pin::Pin;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum RuntimeFunction {
    GetMemoryPtr,
    GetBlockId,
    PrintString,
}

impl RuntimeFunction {
    pub const fn name(&self) -> &'static str {
        match self {
            Self::GetMemoryPtr => "get_memory_ptr",
            Self::GetBlockId => "get_block_id",
            Self::PrintString => "print_string",
        }
    }

    pub const fn argument_count(&self) -> usize {
        match self {
            Self::GetMemoryPtr => 1,
            Self::GetBlockId => 1,
            Self::PrintString => 2,
        }
    }

    #[inline]
    fn signature<'ctx>(&self, context: &ContextRef<'ctx>) -> FunctionType<'ctx> {
        let i8_type = context.i8_type();
        let i64_type = context.i64_type();
        let void_type = context.void_type();
        let ptr_type = i64_type.ptr_type(AddressSpace::default());

        match self {
            Self::GetMemoryPtr => {
                // NOTE: Must match the `Environment::memory_ptr()` function!
                i8_type.ptr_type(AddressSpace::default()).fn_type(
                    &[
                        ptr_type.into(), // Environment
                        i64_type.into(), // index
                    ],
                    false,
                )
            }

            Self::GetBlockId => {
                // NOTE: Must match the `Environment::block_id()` function!
                i64_type.fn_type(
                    &[
                        ptr_type.into(), // Environment
                        i64_type.into(), // addr
                    ],
                    false,
                )
            }

            Self::PrintString => {
                // NOTE: Must match the `Environment::print_string()` function!
                void_type.fn_type(
                    &[
                        ptr_type.into(), // Environment
                        ptr_type.into(), // string_ptr
                        i64_type.into(), // len
                    ],
                    false,
                )
            }
        }
    }

    #[inline]
    fn init<'ctx>(
        &self,
        context: &ContextRef<'ctx>,
        module: &Module<'ctx>,
        execution_engine: &ExecutionEngine<'ctx>,
        ptr: *const u8,
    ) {
        let func = module.add_function(self.name(), self.signature(context), None);
        execution_engine.add_global_mapping(&func, ptr as _);
    }
}

pub struct Environment<'ctx, const REG_SIZE: usize, const MEM_SIZE: usize> {
    labels: Vec<Label<'ctx>>,

    general_purpose_registers: Pin<Box<[u64; REG_SIZE]>>,
    cp0_registers: Pin<Box<[u64; REG_SIZE]>>,
    hi_lo_registers: Pin<Box<[u64; 2]>>,

    // TODO: split into a trait for user-defined MMIO
    memory: Box<[u8; MEM_SIZE]>,
}

impl<'ctx, const REG_SIZE: usize, const MEM_SIZE: usize> Environment<'ctx, REG_SIZE, MEM_SIZE> {
    pub fn new(labels: Vec<Label<'ctx>>) -> Pin<Box<Self>> {
        Box::pin(Self {
            labels,
            general_purpose_registers: Box::pin([0; REG_SIZE]),
            cp0_registers: Box::pin([0; REG_SIZE]),
            hi_lo_registers: Box::pin([0; 2]),
            // Allocate memory directly on the heap using a vec to avoid stack overflows
            memory: vec![0; MEM_SIZE].into_boxed_slice().try_into().unwrap(),
        })
    }

    pub fn init(
        &self,
        module: &Module<'ctx>,
        execution_engine: &ExecutionEngine<'ctx>,
        env_global_name: &str,
        gprs_global: &str,
        cp0_regs_global: &str,
        hi_lo_global: &str,
    ) {
        let context = module.get_context();
        let i64_type = context.i64_type();
        let regs_array = i64_type.array_type(REG_SIZE as u32);

        // Add a mapping to the environment struct
        let ptr_type = context.i64_type().ptr_type(AddressSpace::default());
        let env_struct = module.add_global(ptr_type, None, env_global_name);
        execution_engine.add_global_mapping(&env_struct, self as *const _ as _);

        // Add a mapping to the general purpose registers
        let gprs = module.add_global(regs_array, None, gprs_global);
        execution_engine.add_global_mapping(&gprs, self.general_purpose_registers.as_ptr() as _);

        // Add a mapping to the hi/lo registers
        let hi_lo = module.add_global(i64_type.array_type(2), None, hi_lo_global);
        execution_engine.add_global_mapping(&hi_lo, self.hi_lo_registers.as_ptr() as _);

        // Add a mapping to the cp0 registers
        let cp0_regs = module.add_global(regs_array, None, cp0_regs_global);
        execution_engine.add_global_mapping(&cp0_regs, self.cp0_registers.as_ptr() as _);

        let add_fn = |f: RuntimeFunction, ptr: *const u8| {
            f.init(&context, module, execution_engine, ptr);
        };

        // Add mappings to the runtime functions
        add_fn(RuntimeFunction::GetMemoryPtr, Self::memory_ptr as _);
        add_fn(RuntimeFunction::GetBlockId, Self::block_id as _);
        add_fn(RuntimeFunction::PrintString, Self::print_string as _);
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

    unsafe extern "C" fn memory_ptr(&mut self, index: u64) -> *mut u8 {
        self.memory.get_mut(index as usize).unwrap_or_else(|| {
            panic!("memory index out of bounds: {index:#x} (max: {MEM_SIZE:#x})",)
        })
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

    unsafe extern "C" fn print_string(&mut self, string_ptr: *const u8, len: u64) {
        let slice = std::slice::from_raw_parts(string_ptr, len as _);
        let string = std::str::from_utf8(slice).unwrap();
        print!("{string}");
    }
}
