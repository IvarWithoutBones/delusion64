use crate::label::Label;
use inkwell::{
    context::ContextRef, execution_engine::ExecutionEngine, module::Module, types::FunctionType,
    AddressSpace,
};
use std::pin::Pin;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum RuntimeFunction {
    GetGeneralRegisterPtr,
    GetCp0RegisterPtr,
    GetMemoryPtr,
    GetBlockId,
    PrintString,
}

impl RuntimeFunction {
    pub const fn name(&self) -> &'static str {
        match self {
            Self::GetGeneralRegisterPtr => "get_register_ptr",
            Self::GetCp0RegisterPtr => "get_cp0_register_ptr",
            Self::GetMemoryPtr => "get_memory_ptr",
            Self::GetBlockId => "get_block_id",
            Self::PrintString => "print_string",
        }
    }

    pub const fn argument_count(&self) -> usize {
        match self {
            Self::GetGeneralRegisterPtr => 1,
            Self::GetCp0RegisterPtr => 1,
            Self::GetMemoryPtr => 1,
            Self::GetBlockId => 1,
            Self::PrintString => 2,
        }
    }

    #[inline]
    fn init<'ctx, const REG_LEN: usize, const MEM_LEN: usize>(
        &self,
        env: &Environment<'ctx, REG_LEN, MEM_LEN>,
        context: &ContextRef<'ctx>,
        module: &Module<'ctx>,
        execution_engine: &ExecutionEngine<'ctx>,
    ) {
        let (ptr, ty) = match self {
            Self::GetGeneralRegisterPtr => env.general_register_ptr_function(context),
            Self::GetCp0RegisterPtr => env.cp0_register_ptr_function(context),
            Self::GetMemoryPtr => env.memory_ptr_function(context),
            Self::GetBlockId => env.block_id_function(context),
            Self::PrintString => env.print_string_function(context),
        };

        let func = module.add_function(self.name(), ty, None);
        execution_engine.add_global_mapping(&func, ptr as _);
    }
}

pub struct Environment<'ctx, const REG_SIZE: usize, const MEM_SIZE: usize> {
    labels: Vec<Label<'ctx>>,
    // TODO: would be neat to split these into their own trait
    general_registers: Box<[u64; REG_SIZE]>,
    cp0_registers: Box<[u64; 32]>,
    memory: Box<[u8; MEM_SIZE]>,
}

impl<'ctx, const REG_SIZE: usize, const MEM_SIZE: usize> Environment<'ctx, REG_SIZE, MEM_SIZE> {
    pub fn new(labels: Vec<Label<'ctx>>) -> Pin<Box<Self>> {
        Box::pin(Self {
            general_registers: Box::new([0; REG_SIZE]),
            cp0_registers: Box::new([0; 32]),
            // Allocate memory directly on the heap using a vec to avoid stack overflows
            memory: vec![0; MEM_SIZE].into_boxed_slice().try_into().unwrap(),
            labels,
        })
    }

    pub fn init(
        &self,
        module: &Module<'ctx>,
        execution_engine: &ExecutionEngine<'ctx>,
        name: &str,
    ) {
        let context = module.get_context();

        // Add a mapping to the environment struct
        let ptr_type = context.i64_type().ptr_type(AddressSpace::default());
        let env_struct = module.add_global(ptr_type, None, name);
        execution_engine.add_global_mapping(&env_struct, self as *const _ as _);

        // Add mappings to the runtime functions
        RuntimeFunction::GetGeneralRegisterPtr.init(self, &context, module, execution_engine);
        RuntimeFunction::GetCp0RegisterPtr.init(self, &context, module, execution_engine);
        RuntimeFunction::GetMemoryPtr.init(self, &context, module, execution_engine);
        RuntimeFunction::GetBlockId.init(self, &context, module, execution_engine);
        RuntimeFunction::PrintString.init(self, &context, module, execution_engine);
    }

    pub fn print_registers(&self) {
        println!("\ngeneral registers:");
        for (i, r) in self.general_registers.iter().enumerate() {
            let name = mips_decomp::format::general_register_name(i as _);
            println!("{name: <8} = {r:#x}");
        }

        println!("\ncp0 registers:");
        for (i, r) in self.cp0_registers.iter().enumerate() {
            let name = mips_decomp::format::cp0_register_name(i as _);
            println!("{name: <8} = {r:#x}");
        }
    }

    unsafe extern "C" fn general_register_ptr(&mut self, index: u64) -> *mut u64 {
        self.general_registers
            .get_mut(index as usize)
            .unwrap_or_else(
                || panic!("general register index out of bounds: {index:#x} (max: 32)",),
            )
    }

    fn general_register_ptr_function(
        &self,
        context: &ContextRef<'ctx>,
    ) -> (*const u8, FunctionType<'ctx>) {
        let i64_type = context.i64_type();
        let ptr_type = i64_type.ptr_type(AddressSpace::default());

        // NOTE: Signature must match the `general_register_ptr()` function!
        (
            Self::general_register_ptr as *const u8,
            i64_type.ptr_type(AddressSpace::default()).fn_type(
                &[
                    ptr_type.into(), // self
                    i64_type.into(), // index
                ],
                false,
            ),
        )
    }

    unsafe extern "C" fn cp0_register_ptr(&mut self, index: u64) -> *mut u64 {
        self.cp0_registers
            .get_mut(index as usize)
            .unwrap_or_else(|| panic!("cp0 register index out of bounds: {index:#x} (max: 32)",))
    }

    fn cp0_register_ptr_function(
        &self,
        context: &ContextRef<'ctx>,
    ) -> (*const u8, FunctionType<'ctx>) {
        let i64_type = context.i64_type();
        let ptr_type = i64_type.ptr_type(AddressSpace::default());

        // NOTE: Signature must match the `cp0_register_ptr()` function!
        (
            Self::cp0_register_ptr as *const u8,
            i64_type.ptr_type(AddressSpace::default()).fn_type(
                &[
                    ptr_type.into(), // self
                    i64_type.into(), // index
                ],
                false,
            ),
        )
    }

    unsafe extern "C" fn memory_ptr(&mut self, index: u64) -> *mut u8 {
        self.memory.get_mut(index as usize).unwrap_or_else(|| {
            panic!("memory index out of bounds: {index:#x} (max: {MEM_SIZE:#x})",)
        })
    }

    fn memory_ptr_function(&self, context: &ContextRef<'ctx>) -> (*const u8, FunctionType<'ctx>) {
        let i8_type = context.i8_type();
        let i64_type = context.i64_type();
        let ptr_type = i64_type.ptr_type(AddressSpace::default());

        // NOTE: Signature must match the `memory_ptr()` function!
        (
            Self::memory_ptr as *const u8,
            i8_type.ptr_type(AddressSpace::default()).fn_type(
                &[
                    ptr_type.into(), // self
                    i64_type.into(), // index
                ],
                false,
            ),
        )
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

    fn block_id_function(&self, context: &ContextRef<'ctx>) -> (*const u8, FunctionType<'ctx>) {
        let i64_type = context.i64_type();
        let ptr_type = i64_type.ptr_type(AddressSpace::default());

        // NOTE: Signature must match the `block_id()` function!
        (
            Self::block_id as *const u8,
            i64_type.fn_type(
                &[
                    ptr_type.into(), // self
                    i64_type.into(), // addr
                ],
                false,
            ),
        )
    }

    unsafe extern "C" fn print_string(&mut self, string_ptr: *const u8, len: u64) {
        let slice = std::slice::from_raw_parts(string_ptr, len as _);
        let string = std::str::from_utf8(slice).unwrap();
        print!("{string}");
    }

    fn print_string_function(&self, context: &ContextRef<'ctx>) -> (*const u8, FunctionType<'ctx>) {
        let i64_type = context.i64_type();
        let ptr_type = i64_type.ptr_type(AddressSpace::default());

        // NOTE: Signature must match the `print_string()` function!
        (
            Self::print_string as *const u8,
            i64_type.fn_type(
                &[
                    ptr_type.into(), // self
                    ptr_type.into(), // string_ptr
                    i64_type.into(), // len
                ],
                false,
            ),
        )
    }
}
