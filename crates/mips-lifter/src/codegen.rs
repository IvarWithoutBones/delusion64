use crate::env::{Environment, RuntimeFunction};
use inkwell::{
    basic_block::BasicBlock,
    builder::Builder,
    context::Context,
    execution_engine::{ExecutionEngine, JitFunction, UnsafeFunctionPointer},
    module::Module,
    types::IntType,
    values::{BasicMetadataValueEnum, BasicValueEnum, CallSiteValue, IntValue, PointerValue},
};
use mips_decomp::instruction::ParsedInstruction;

const ENV_GLOBAL: &str = "env";

enum RegisterType {
    GeneralPurpose,
    Cp0,
    HiLo,
}

impl RegisterType {
    const fn global_name(&self) -> &'static str {
        match self {
            Self::GeneralPurpose => "gprs",
            Self::Cp0 => "cp0_regs",
            Self::HiLo => "hi_lo_regs",
        }
    }

    const fn register_count(&self) -> usize {
        match self {
            Self::GeneralPurpose => 32,
            Self::Cp0 => 32,
            Self::HiLo => 2,
        }
    }

    const fn name(&self, index: u8) -> &'static str {
        match self {
            Self::GeneralPurpose => mips_decomp::format::general_register_name(index),
            Self::Cp0 => mips_decomp::format::cp0_register_name(index),
            Self::HiLo => match index {
                0 => "hi",
                1 => "lo",
                _ => unreachable!(),
            },
        }
    }
}

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum HiLo {
    Hi,
    Lo,
}

pub struct CodeGen<'ctx> {
    pub context: &'ctx Context,
    pub module: Module<'ctx>,
    pub builder: Builder<'ctx>,
    pub execution_engine: ExecutionEngine<'ctx>,
    pub label_not_found: BasicBlock<'ctx>,
}

impl<'ctx> CodeGen<'ctx> {
    pub fn new<const REG_LEN: usize, const MEM_LEN: usize>(
        context: &'ctx Context,
        module: Module<'ctx>,
        execution_engine: ExecutionEngine<'ctx>,
        env: &Environment<'ctx, REG_LEN, MEM_LEN>,
    ) -> Self {
        env.init(
            &module,
            &execution_engine,
            ENV_GLOBAL,
            RegisterType::GeneralPurpose.global_name(),
            RegisterType::Cp0.global_name(),
            RegisterType::HiLo.global_name(),
        );

        let label_not_found =
            context.append_basic_block(module.get_function("main").unwrap(), "label_not_found");

        let codegen = Self {
            context,
            module,
            builder: context.create_builder(),
            execution_engine,
            label_not_found,
        };

        // Initialize the error case for attempting to jump to a label that doesn't exist.
        codegen.builder.position_at_end(label_not_found);
        codegen.print_constant_string(
            "ERROR: unable to fetch basic block\n",
            "label_not_found_str",
        );
        codegen.builder.build_return(None);

        codegen
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

    fn register_pointer<T>(&self, ty: RegisterType, index: T, name: &str) -> PointerValue<'ctx>
    where
        T: Into<u64>,
    {
        let index = index.into();
        assert!(index <= ty.register_count() as _);
        let i64_type = self.context.i64_type();
        let name = format!("{name}_ptr");

        let registers_ptr = self
            .module
            .get_global(ty.global_name())
            .unwrap()
            .as_pointer_value();

        unsafe {
            self.builder.build_in_bounds_gep(
                i64_type,
                registers_ptr,
                &[i64_type.const_int(index, false)],
                &name,
            )
        }
    }

    pub fn load_gpr<T>(&self, index: T) -> BasicValueEnum<'ctx>
    where
        T: Into<u64>,
    {
        let index = index.into();
        if index == 0 {
            // Register zero is always zero
            self.context.i64_type().const_zero().into()
        } else {
            let i64_type = self.context.i64_type();
            let name = mips_decomp::format::general_register_name(index as _);
            let register = self.register_pointer(RegisterType::GeneralPurpose, index, name);
            self.builder
                .build_load(i64_type, register, &format!("{name}_"))
        }
    }

    pub fn store_gpr<T>(&self, index: T, value: BasicValueEnum<'ctx>)
    where
        T: Into<u64>,
    {
        let index = index.into();
        if index != 0 {
            // Register zero is read-only
            let ty = RegisterType::GeneralPurpose;
            let name = ty.name(index as _);
            let register = self.register_pointer(ty, index, name);
            self.builder.build_store(register, value);
        }
    }

    pub fn load_cp0_reg<T>(&self, index: T) -> BasicValueEnum<'ctx>
    where
        T: Into<u64>,
    {
        let index = index.into();
        let i64_type = self.context.i64_type();
        let ty = RegisterType::Cp0;
        let name = ty.name(index as _);
        let register = self.register_pointer(ty, index, name);
        self.builder.build_load(i64_type, register, name)
    }

    pub fn store_cp0_reg<T>(&self, index: T, value: BasicValueEnum<'ctx>)
    where
        T: Into<u64>,
    {
        let index = index.into();
        let ty = RegisterType::Cp0;
        let name = ty.name(index as _);
        let register = self.register_pointer(ty, index, name);
        self.builder.build_store(register, value);
    }

    pub fn load_hi_lo(&self, variant: HiLo) -> BasicValueEnum<'ctx> {
        let i64_type = self.context.i64_type();
        let ty = RegisterType::HiLo;
        let name = ty.name(variant as _);
        let register = self.register_pointer(ty, variant as u32, name);
        self.builder.build_load(i64_type, register, name)
    }

    pub fn store_hi_lo(&self, variant: HiLo, value: BasicValueEnum<'ctx>) {
        let ty = RegisterType::HiLo;
        let name = ty.name(variant as _);
        let register = self.register_pointer(ty, variant as u32, name);
        self.builder.build_store(register, value);
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

    pub fn truncate_to_i64(&self, value: IntValue<'ctx>) -> IntValue<'ctx> {
        let i64_type = self.context.i64_type();
        self.builder.build_int_truncate(value, i64_type, "to_i64")
    }

    pub fn sign_extend_to_i64(&self, value: IntValue<'ctx>) -> IntValue<'ctx> {
        let i64_type = self.context.i64_type();
        self.builder
            .build_int_s_extend(value, i64_type, "sign_extend")
    }

    pub fn zero_extend_to_i64(&self, value: IntValue<'ctx>) -> IntValue<'ctx> {
        let i64_type = self.context.i64_type();
        self.builder
            .build_int_z_extend(value, i64_type, "zero_extend")
    }

    pub fn build_basic_block(&self, name: &str) -> Option<BasicBlock<'ctx>> {
        let func = self.builder.get_insert_block()?.get_parent()?;
        Some(self.context.append_basic_block(func, name))
    }

    pub fn base_plus_offset(&self, instr: &ParsedInstruction, name: &str) -> IntValue<'ctx> {
        let base = self.load_gpr(instr.base()).into_int_value();
        let offset = self.build_i64(instr.offset()).into_int_value();
        self.builder.build_int_add(base, offset, name)
    }
}
