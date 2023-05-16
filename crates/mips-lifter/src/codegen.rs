use inkwell::{
    builder::Builder,
    context::Context,
    execution_engine::{ExecutionEngine, JitFunction, UnsafeFunctionPointer},
    module::Module,
    types::{BasicMetadataTypeEnum, IntType},
    values::{BasicMetadataValueEnum, BasicValueEnum, GlobalValue, IntValue, PointerValue},
};
use mips_decomp::REGISTER_COUNT;

// TODO: This should obviously not be defined here
const MEMORY_SIZE: usize = 0x1000;

pub struct CodeGen<'ctx> {
    pub context: &'ctx Context,
    pub module: Module<'ctx>,
    pub builder: Builder<'ctx>,
    pub execution_engine: ExecutionEngine<'ctx>,
}

impl<'ctx> CodeGen<'ctx> {
    pub fn new(
        context: &'ctx Context,
        module: Module<'ctx>,
        execution_engine: ExecutionEngine<'ctx>,
    ) -> Self {
        let i64_type = context.i64_type();

        // Initialize the registers to zero
        let regs = module.add_global(i64_type.array_type(REGISTER_COUNT as _), None, "registers");
        regs.set_initializer(&i64_type.array_type(REGISTER_COUNT as _).const_zero());

        // Initialize the memory to zero
        let memory = module.add_global(i64_type.array_type(MEMORY_SIZE as _), None, "memory");
        memory.set_initializer(&i64_type.array_type(MEMORY_SIZE as _).const_zero());

        Self {
            context,
            module,
            execution_engine,
            builder: context.create_builder(),
        }
    }

    pub fn memory_global(&self) -> GlobalValue<'ctx> {
        self.module.get_global("memory").unwrap()
    }

    pub fn memory_ptr(&self, index: IntValue<'ctx>) -> PointerValue<'ctx> {
        let i64_type = self.context.i64_type();

        unsafe {
            self.builder.build_in_bounds_gep(
                i64_type.array_type(MEMORY_SIZE as _).get_element_type(),
                self.memory_global().as_pointer_value(),
                &[index],
                "mem_ptr",
            )
        }
    }

    pub fn load_memory(&self, ty: IntType<'ctx>, index: IntValue<'ctx>) -> BasicValueEnum<'ctx> {
        let name = format!("mem_{}", ty.get_bit_width());
        let memory = self.memory_ptr(index);
        self.builder.build_load(ty, memory, &name)
    }

    pub fn store_memory(&self, index: IntValue<'ctx>, value: BasicValueEnum<'ctx>) {
        let memory = self.memory_ptr(index);
        self.builder.build_store(memory, value);
    }

    pub fn registers_global(&self) -> GlobalValue<'ctx> {
        self.module.get_global("registers").unwrap()
    }

    fn register_ptr<T>(&self, index: T) -> PointerValue<'ctx>
    where
        T: Into<u64>,
    {
        let index = index.into();
        let i64_type = self.context.i64_type();
        debug_assert!(index < REGISTER_COUNT as _);

        unsafe {
            self.builder.build_in_bounds_gep(
                i64_type.array_type(REGISTER_COUNT as _).get_element_type(),
                self.registers_global().as_pointer_value(),
                &[i64_type.const_int(index, false)],
                "reg_ptr",
            )
        }
    }

    pub fn load_register<T>(&self, index: T) -> BasicValueEnum<'ctx>
    where
        T: Into<u64>,
    {
        let index = index.into();
        if index == 0 {
            // Register zero is always zero
            self.context.i64_type().const_zero().into()
        } else {
            let name = mips_decomp::format::register_name(index as _);
            let i64_type = self.context.i64_type();
            let register = self.register_ptr(index);
            self.builder.build_load(i64_type, register, name)
        }
    }

    pub fn store_register<T>(&self, index: T, value: BasicValueEnum<'ctx>)
    where
        T: Into<u64>,
    {
        let index = index.into();
        if index != 0 {
            // Register zero is read-only
            let register = self.register_ptr(index);
            self.builder.build_store(register, value);
        }
    }

    pub fn add_function<F>(&self, name: &str, func: *const F, args: &[BasicMetadataTypeEnum<'ctx>])
    where
        F: UnsafeFunctionPointer,
    {
        let fn_type = self.context.void_type().fn_type(args, false);
        let function = self.module.add_function(name, fn_type, None);
        self.execution_engine
            .add_global_mapping(&function, func as *const _ as _);
    }

    pub fn get_function<F>(&self, name: &str) -> Option<JitFunction<F>>
    where
        F: UnsafeFunctionPointer,
    {
        unsafe { self.execution_engine.get_function(name).ok() }
    }

    pub fn build_call(&self, name: &str, args: &[BasicMetadataValueEnum<'ctx>]) {
        self.builder
            .build_call(self.module.get_function(name).unwrap(), args, name);
    }

    pub fn build_i64<T>(&self, value: T) -> BasicMetadataValueEnum<'ctx>
    where
        T: Into<u64>,
    {
        self.context
            .i64_type()
            .const_int(value.into(), false)
            .into()
    }

    pub fn sign_extend_to_i64(&self, value: IntValue<'ctx>) -> IntValue<'ctx> {
        let i64_type = self.context.i64_type();
        self.builder
            .build_int_s_extend(value, i64_type, "sign_extend")
    }
}
