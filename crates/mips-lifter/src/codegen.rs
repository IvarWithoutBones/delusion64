use crate::env::{Environment, RuntimeFunction};
use inkwell::{
    basic_block::BasicBlock,
    builder::Builder,
    context::Context,
    execution_engine::{ExecutionEngine, JitFunction, UnsafeFunctionPointer},
    module::Module,
    types::{BasicMetadataTypeEnum, IntType},
    values::{BasicMetadataValueEnum, BasicValueEnum, CallSiteValue, IntValue, PointerValue},
};

const ENV_GLOBAL: &str = "env";

pub struct CodeGen<'ctx> {
    pub context: &'ctx Context,
    pub module: Module<'ctx>,
    pub builder: Builder<'ctx>,
    pub execution_engine: ExecutionEngine<'ctx>,
}

impl<'ctx> CodeGen<'ctx> {
    pub fn new<const REG_LEN: usize, const MEM_LEN: usize>(
        context: &'ctx Context,
        module: Module<'ctx>,
        execution_engine: ExecutionEngine<'ctx>,
        env: &Environment<'ctx, REG_LEN, MEM_LEN>,
    ) -> Self {
        env.init(&module, &execution_engine, ENV_GLOBAL);

        Self {
            context,
            module,
            execution_engine,
            builder: context.create_builder(),
        }
    }

    pub fn verify(&self) -> Result<(), String> {
        self.module.verify().map_err(|e| e.to_string())
    }

    pub fn memory_ptr(&self, index: IntValue<'ctx>) -> PointerValue<'ctx> {
        self.build_env_call(RuntimeFunction::GetMemoryPtr, &[index.into()])
            .try_as_basic_value()
            .left()
            .unwrap()
            .into_pointer_value()
    }

    pub fn load_memory(&self, ty: IntType<'ctx>, index: IntValue<'ctx>) -> BasicValueEnum<'ctx> {
        let name = format!("i{}_mem", ty.get_bit_width());
        let memory = self.memory_ptr(index);
        self.builder.build_load(ty, memory, &name)
    }

    pub fn store_memory(&self, index: IntValue<'ctx>, value: BasicValueEnum<'ctx>) {
        let memory = self.memory_ptr(index);
        self.builder.build_store(memory, value);
    }

    fn register_ptr(&self, index: u64) -> PointerValue<'ctx> {
        let i64_type = self.context.i64_type();

        self.build_env_call(
            RuntimeFunction::GetRegisterPtr,
            &[i64_type.const_int(index, false).into()],
        )
        .try_as_basic_value()
        .left()
        .unwrap()
        .into_pointer_value()
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

    pub fn build_call(
        &self,
        name: &str,
        args: &[BasicMetadataValueEnum<'ctx>],
    ) -> CallSiteValue<'ctx> {
        self.builder
            .build_call(self.module.get_function(name).unwrap(), args, name)
    }

    pub fn build_env_call(
        &self,
        function: RuntimeFunction,
        args: &[BasicMetadataValueEnum<'ctx>],
    ) -> CallSiteValue<'ctx> {
        debug_assert_eq!(function.argument_count(), args.len());

        let env_ptr = self
            .module
            .get_global(ENV_GLOBAL)
            .unwrap()
            .as_pointer_value();
        let args = [env_ptr.into()]
            .iter()
            .chain(args.iter())
            .cloned()
            .collect::<Vec<_>>();

        self.build_call(function.name(), &args)
    }

    pub fn print_constant_string(&self, string: &str, storage_name: &str) {
        let ptr = self
            .builder
            .build_global_string_ptr(string, storage_name)
            .as_pointer_value();
        let len = self.context.i64_type().const_int(string.len() as _, false);

        self.build_env_call(RuntimeFunction::PrintString, &[ptr.into(), len.into()]);
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

    pub fn to_i64(&self, value: IntValue<'ctx>) -> IntValue<'ctx> {
        let i64_type = self.context.i64_type();
        self.builder.build_int_truncate(value, i64_type, "to_i64")
    }

    pub fn sign_extend_to_i64(&self, value: IntValue<'ctx>) -> IntValue<'ctx> {
        let i64_type = self.context.i64_type();
        self.builder
            .build_int_s_extend(value, i64_type, "sign_extend")
    }

    pub fn build_basic_block(&self, name: &str) -> Option<BasicBlock<'ctx>> {
        let func = self.builder.get_insert_block()?.get_parent()?;
        Some(self.context.append_basic_block(func, name))
    }
}
