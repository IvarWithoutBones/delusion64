use crate::{label::Labels, runtime::RuntimeFunction};
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
    IntPredicate,
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
    pub cp0_regs: GlobalValue<'ctx>,
}

pub struct CodeGen<'ctx> {
    pub context: &'ctx Context,
    pub module: Module<'ctx>,
    pub builder: Builder<'ctx>,
    pub execution_engine: ExecutionEngine<'ctx>,

    /// Global mappings to the runtime environment.
    pub globals: Globals<'ctx>,
    /// The generated labels, with associated basic blocks.
    pub labels: Labels<'ctx>,
    /// The generated label IDs, with associated basic blocks.
    /// Can be used from a switch statement to resolve dynamic jumps.
    pub label_cases: Vec<(IntValue<'ctx>, BasicBlock<'ctx>)>,
    /// The basic block to jump to when a dynamic jump could not be resolved.
    pub label_not_found: BasicBlock<'ctx>,
}

impl<'ctx> CodeGen<'ctx> {
    pub fn new(
        context: &'ctx Context,
        module: Module<'ctx>,
        execution_engine: ExecutionEngine<'ctx>,
        globals: Globals<'ctx>,
        labels: Labels<'ctx>,
    ) -> Self {
        let i64_type = context.i64_type();
        let label_cases = labels
            .values()
            .map(|label| {
                let id = i64_type.const_int(label.id, false);
                (id, label.basic_block)
            })
            .collect::<Vec<_>>();

        let label_not_found =
            context.append_basic_block(module.get_function("main").unwrap(), "label_not_found");

        let codegen = Self {
            context,
            module,
            builder: context.create_builder(),
            execution_engine,
            globals,
            labels,
            label_cases,
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

    /// Gets the basic block associated with the given address, or the error block if none exists.
    pub fn get_basic_block(&self, address: u64) -> BasicBlock<'ctx> {
        self.labels
            .get(&address)
            .map_or_else(|| self.label_not_found, |l| l.basic_block)
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

    /// Performs a comparisons between two integers, returning the result as an i64.
    pub fn build_int_compare_as_i64(
        &self,
        op: IntPredicate,
        lhs: IntValue<'ctx>,
        rhs: IntValue<'ctx>,
        cmp_name: &str,
        i64_name: &str,
    ) -> IntValue<'ctx> {
        let cmp = self.builder.build_int_compare(op, lhs, rhs, cmp_name);
        self.builder
            .build_int_z_extend(cmp, self.context.i64_type(), i64_name)
    }

    /// Generate a basic block that calls the given function, then falls through to the next block.
    /// Returns the newly created block.
    pub fn build_fall_through_block<F>(
        &self,
        name: &str,
        next_block: BasicBlock<'ctx>,
        inside: F,
    ) -> BasicBlock<'ctx>
    where
        F: FnOnce(),
    {
        // Create a new block to call the function in.
        let curr_block = self.builder.get_insert_block().unwrap();
        let new_block = self
            .context
            .append_basic_block(curr_block.get_parent().unwrap(), name);

        self.builder.position_at_end(new_block);
        inside();
        self.builder.build_unconditional_branch(next_block);
        self.builder.position_at_end(curr_block);
        new_block
    }

    /// Generate a conditional branch to the given address, and sets the return address to the next instruction.
    /// This ends the current basic block. Useful for static conditional branches.
    pub fn build_conditional_branch_set_ra(
        &self,
        cmp: IntValue<'ctx>,
        target_pc: u64,
        next_instr_pc: u64,
    ) {
        let i64_type = self.context.i64_type();
        let target_block = self.get_basic_block(target_pc);
        let else_block = self.get_basic_block(next_instr_pc);

        let name = format!("label_{next_instr_pc:6x}_set_ra");
        let then_block = self.build_fall_through_block(&name, target_block, || {
            self.write_general_reg(
                register::GeneralPurpose::Ra,
                i64_type.const_int(next_instr_pc, false).into(),
            );
        });

        self.builder
            .build_conditional_branch(cmp, then_block, else_block);
    }

    /// Generate a conditional branch to the given address, ending the current basic block.
    /// Useful for static conditional branches.
    pub fn build_conditional_branch(&self, cmp: IntValue<'ctx>, then_pc: u64, else_pc: u64) {
        let then_block = self.get_basic_block(then_pc);
        let else_block = self.get_basic_block(else_pc);
        self.builder
            .build_conditional_branch(cmp, then_block, else_block);
    }

    /// Generate a dynamic jump to the given address, ending the current basic block.
    /// Ends execution if no label matching the address could be found.
    pub fn build_dynamic_jump(&self, address: IntValue<'ctx>) {
        let label_id = {
            let result = env_call!(self, RuntimeFunction::GetBlockId, [address]);
            result.try_as_basic_value().left().unwrap().into_int_value()
        };

        // Build a switch statement with all possible labels.
        self.builder
            .build_switch(label_id, self.label_not_found, &self.label_cases);
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
            .build_int_s_extend(value, i64_type, "sign_ext_to_i64")
    }

    pub fn zero_extend_to_i64(&self, value: IntValue<'ctx>) -> IntValue<'ctx> {
        let i64_type = self.context.i64_type();
        self.builder
            .build_int_z_extend(value, i64_type, "zero_ext_to_i64")
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

        let base_ptr = match reg {
            Register::GeneralPurpose(_) => self.globals.general_purpose_regs.as_pointer_value(),
            Register::Special(_) => self.globals.special_regs.as_pointer_value(),
            Register::Cp0(_) => self.globals.cp0_regs.as_pointer_value(),
        };

        unsafe {
            self.builder.build_in_bounds_gep(
                i64_type,
                base_ptr,
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
        let reg = register::Cp0::from_repr(index.into() as _).unwrap();
        let i64_type = self.context.i64_type();
        let register = self.register_pointer(reg);
        self.builder
            .build_load(i64_type, register, &format!("{}_", reg.name()))
    }

    pub fn write_cp0_reg<T>(&self, index: T, value: BasicValueEnum<'ctx>)
    where
        T: Into<u64>,
    {
        let reg = register::Cp0::from_repr(index.into() as _).unwrap();
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
