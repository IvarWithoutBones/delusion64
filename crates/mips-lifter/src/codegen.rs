use crate::env::{function::RuntimeFunction, Environment, Memory};
use inkwell::{
    basic_block::BasicBlock,
    builder::Builder,
    context::Context,
    execution_engine::{ExecutionEngine, JitFunction, UnsafeFunctionPointer},
    module::Module,
    types::IntType,
    values::{
        BasicMetadataValueEnum, BasicValueEnum, CallSiteValue, GlobalValue, IntValue, PointerValue,
    },
};
use mips_decomp::{
    instruction::ParsedInstruction,
    register::{self, Register},
};

#[macro_export]
macro_rules! env_call {
    ($codegen:expr, $func:expr, [$($args:expr),*]) => {{
        let args = &[$codegen.globals.env_ptr.as_pointer_value().into(), $($args.into()),*];
        $codegen.build_call($func.name(), args)
    }};
}

#[derive(Debug)]
pub struct Globals<'ctx> {
    pub env_ptr: GlobalValue<'ctx>,
    pub general_purpose_regs: GlobalValue<'ctx>,
    pub special_regs: GlobalValue<'ctx>,
    pub coprocessor_regs: GlobalValue<'ctx>,
}

pub struct CodeGen<'ctx> {
    pub context: &'ctx Context,
    pub module: Module<'ctx>,
    pub builder: Builder<'ctx>,
    pub execution_engine: ExecutionEngine<'ctx>,
    pub label_not_found: BasicBlock<'ctx>,
    pub globals: Globals<'ctx>,
}

impl<'ctx> CodeGen<'ctx> {
    pub fn new<Mem: Memory>(
        context: &'ctx Context,
        module: Module<'ctx>,
        execution_engine: ExecutionEngine<'ctx>,
        env: &Environment<'ctx, Mem>,
    ) -> Self {
        let globals = env.init(&module, &execution_engine);

        let label_not_found =
            context.append_basic_block(module.get_function("main").unwrap(), "label_not_found");

        let codegen = Self {
            context,
            module,
            builder: context.create_builder(),
            execution_engine,
            label_not_found,
            globals,
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

    pub fn print_constant_string(&self, string: &str, storage_name: &str) {
        let ptr = self
            .builder
            .build_global_string_ptr(string, storage_name)
            .as_pointer_value();
        let len = self.context.i64_type().const_int(string.len() as _, false);
        env_call!(self, RuntimeFunction::PrintString, [ptr, len]);
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
        self.builder
            .build_int_truncate(value, i64_type, "truc_to_i64")
    }

    pub fn sign_extend_to_i64(&self, value: IntValue<'ctx>) -> IntValue<'ctx> {
        let i64_type = self.context.i64_type();
        self.builder
            .build_int_s_extend(value, i64_type, "sign_ext_to_i64")
    }

    pub fn zero_extend_to_i64(&self, value: IntValue<'ctx>) -> IntValue<'ctx> {
        let i64_type = self.context.i64_type();
        self.builder
            .build_int_z_extend(value, i64_type, "zero_ext_to_i64")
    }

    pub fn build_basic_block(&self, name: &str) -> Option<BasicBlock<'ctx>> {
        let func = self.builder.get_insert_block()?.get_parent()?;
        Some(self.context.append_basic_block(func, name))
    }

    pub fn base_plus_offset(&self, instr: &ParsedInstruction, name: &str) -> IntValue<'ctx> {
        let base = self.read_general_reg(instr.base()).into_int_value();
        let offset = self.build_i64(instr.offset()).into_int_value();
        self.builder.build_int_add(base, offset, name)
    }

    #[inline]
    fn register_pointer<T: Into<Register>>(&self, reg: T) -> PointerValue<'ctx> {
        let reg = reg.into();
        let i64_type = self.context.i64_type();

        let registers_ptr = match reg {
            Register::GeneralPurpose(_) => self.globals.general_purpose_regs.as_pointer_value(),
            Register::Special(_) => self.globals.special_regs.as_pointer_value(),
            Register::Coprocessor(_) => self.globals.coprocessor_regs.as_pointer_value(),
        };

        unsafe {
            self.builder.build_in_bounds_gep(
                i64_type,
                registers_ptr,
                &[i64_type.const_int(reg.to_repr() as _, false)],
                &format!("{}_ptr", reg.name()),
            )
        }
    }

    pub fn read_memory(&self, ty: IntType<'ctx>, address: IntValue<'ctx>) -> BasicValueEnum<'ctx> {
        let func = match ty.get_bit_width() {
            8 => RuntimeFunction::ReadI8,
            16 => RuntimeFunction::ReadI16,
            32 => RuntimeFunction::ReadI32,
            64 => RuntimeFunction::ReadI64,
            _ => panic!("unimplemented load_memory type: {ty}"),
        };

        env_call!(self, func, [address])
            .try_as_basic_value()
            .left()
            .unwrap()
    }

    pub fn write_memory(&self, address: IntValue<'ctx>, value: BasicValueEnum<'ctx>) {
        let ty = value.get_type().into_int_type();
        let func = match ty.get_bit_width() {
            8 => RuntimeFunction::WriteI8,
            16 => RuntimeFunction::WriteI16,
            32 => RuntimeFunction::WriteI32,
            64 => RuntimeFunction::WriteI64,
            _ => panic!("unimplemented store_memory type: {ty}"),
        };

        env_call!(self, func, [address, value]);
    }

    pub fn read_general_reg<T>(&self, index: T) -> BasicValueEnum<'ctx>
    where
        T: Into<u64>,
    {
        let reg = register::GeneralPurpose::from_repr(index.into() as _).unwrap();
        if reg == register::GeneralPurpose::Zero {
            // Register zero is always zero
            self.context.i64_type().const_zero().into()
        } else {
            let i64_type = self.context.i64_type();
            let register = self.register_pointer(reg);
            self.builder
                .build_load(i64_type, register, &format!("{}_", reg.name()))
        }
    }

    pub fn write_general_reg<T>(&self, index: T, value: BasicValueEnum<'ctx>)
    where
        T: Into<u64>,
    {
        let reg = register::GeneralPurpose::from_repr(index.into() as _).unwrap();
        if reg != register::GeneralPurpose::Zero {
            // Register zero is read-only
            let register = self.register_pointer(reg);
            self.builder.build_store(register, value);
        }
    }

    pub fn read_cp0_reg<T>(&self, index: T) -> BasicValueEnum<'ctx>
    where
        T: Into<u64>,
    {
        let reg = register::Coprocessor::from_repr(index.into() as _).unwrap();
        let i64_type = self.context.i64_type();
        let register = self.register_pointer(reg);
        self.builder
            .build_load(i64_type, register, &format!("{}_", reg.name()))
    }

    pub fn write_cp0_reg<T>(&self, index: T, value: BasicValueEnum<'ctx>)
    where
        T: Into<u64>,
    {
        let reg = register::Coprocessor::from_repr(index.into() as _).unwrap();
        let reg_ptr = self.register_pointer(reg);
        self.builder.build_store(reg_ptr, value);
    }

    pub fn read_special_reg(&self, reg: register::Special) -> BasicValueEnum<'ctx> {
        let i64_type = self.context.i64_type();
        let register = self.register_pointer(reg);
        self.builder
            .build_load(i64_type, register, &format!("{}_", reg.name()))
    }

    pub fn write_special_reg(&self, reg: register::Special, value: BasicValueEnum<'ctx>) {
        let register = self.register_pointer(reg);
        self.builder.build_store(register, value);
    }
}
