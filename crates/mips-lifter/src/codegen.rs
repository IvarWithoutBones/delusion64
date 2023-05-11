use inkwell::{
    basic_block::BasicBlock,
    builder::Builder,
    context::Context,
    execution_engine::{ExecutionEngine, JitFunction, UnsafeFunctionPointer},
    module::Module,
    types::BasicMetadataTypeEnum,
    values::{BasicMetadataValueEnum, BasicValueEnum, GlobalValue, PointerValue},
};
use mips_decomp::REGISTER_COUNT;

pub struct CodeGen<'ctx> {
    pub context: &'ctx Context,
    pub module: Module<'ctx>,
    pub builder: Builder<'ctx>,
    pub execution_engine: ExecutionEngine<'ctx>,
    exit_block: Option<BasicBlock<'ctx>>,
}

impl<'ctx> CodeGen<'ctx> {
    pub fn new(
        context: &'ctx Context,
        module: Module<'ctx>,
        execution_engine: ExecutionEngine<'ctx>,
    ) -> Self {
        // Initialize the registers to zero
        let i64_type = context.i64_type();
        let regs = module.add_global(i64_type.array_type(REGISTER_COUNT as _), None, "registers");
        regs.set_initializer(&i64_type.array_type(REGISTER_COUNT as _).const_zero());

        Self {
            context,
            module,
            builder: context.create_builder(),
            execution_engine,
            exit_block: None,
        }
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

    pub fn set_exit_block(&mut self, block: BasicBlock<'ctx>) {
        self.exit_block = Some(block);
    }

    pub fn call_exit_block(&self) {
        self.builder
            .build_unconditional_branch(self.exit_block.unwrap());
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
}
