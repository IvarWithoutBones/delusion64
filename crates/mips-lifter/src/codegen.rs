use crate::{label::LabelWithContext, runtime::RuntimeFunction};
use inkwell::{
    basic_block::BasicBlock,
    builder::Builder,
    context::Context,
    execution_engine::{ExecutionEngine, JitFunction, UnsafeFunctionPointer},
    module::Module,
    types::IntType,
    values::{
        BasicMetadataValueEnum, BasicValueEnum, CallSiteValue, FunctionValue, GlobalValue,
        IntValue, PointerValue,
    },
    IntPredicate,
};
use mips_decomp::{
    instruction::ParsedInstruction,
    register::{self, Register},
    INSTRUCTION_SIZE,
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

    /// The generated labels, with associated functions.
    pub labels: Vec<LabelWithContext<'ctx>>,

    pub dynamic_jump_to_id: FunctionValue<'ctx>,

    /// The function to jump to when a dynamic jump could not be resolved.
    pub label_not_found: FunctionValue<'ctx>,
}

impl<'ctx> CodeGen<'ctx> {
    pub fn new(
        context: &'ctx Context,
        module: Module<'ctx>,
        execution_engine: ExecutionEngine<'ctx>,
        globals: Globals<'ctx>,
        labels: Vec<LabelWithContext<'ctx>>,
    ) -> Self {
        let builder = context.create_builder();

        let label_not_found = module.add_function(
            "label_not_found",
            context.void_type().fn_type(&[], false),
            None,
        );
        label_not_found.set_call_conventions(crate::LLVM_CALLING_CONVENTION_FAST);

        let i64_type = context.i64_type();
        let dynamic_jump_to_id = module.add_function(
            "dynamic_jump_to_id",
            context.void_type().fn_type(&[i64_type.into()], false),
            None,
        );
        dynamic_jump_to_id.set_call_conventions(crate::LLVM_CALLING_CONVENTION_FAST);

        let codegen = Self {
            context,
            module,
            builder,
            execution_engine,
            globals,
            labels,
            label_not_found,
            dynamic_jump_to_id,
        };

        codegen.init_dynamic_jump_to_id();
        codegen.init_label_not_found();
        codegen
    }

    // Initialise the dynamic label lookup function.
    fn init_dynamic_jump_to_id(&self) {
        let main_block = self
            .context
            .append_basic_block(self.dynamic_jump_to_id, "main");

        let error_block = self
            .context
            .append_basic_block(self.dynamic_jump_to_id, "error");
        self.builder.position_at_end(error_block);
        self.print_constant_string(
            "ERROR: unable to fetch label for dynamic jump\n",
            "label_not_found_dynamic_jump_str",
        );
        env_call!(self, RuntimeFunction::Panic, []);
        self.builder.build_return(None);

        let cases = {
            let i64_type = self.context.i64_type();
            let mut cases = Vec::with_capacity(self.labels.len());
            for label in &self.labels {
                let label_id = label.label.start() as u64;
                let name = format!("case_{label_id:06x}",);

                let block = self
                    .context
                    .append_basic_block(self.dynamic_jump_to_id, &name);
                self.builder.position_at_end(block);
                self.call_label(label.function);

                cases.push((i64_type.const_int(label_id, false), block));
            }
            cases
        };

        self.builder.position_at_end(main_block);
        self.builder.build_switch(
            self.dynamic_jump_to_id
                .get_first_param()
                .unwrap()
                .into_int_value(),
            error_block,
            &cases,
        );
    }

    /// Initializes the error case that occurs when attempting to jump to a label that wasnt found.
    fn init_label_not_found(&self) {
        let label_not_found_block = self
            .context
            .append_basic_block(self.label_not_found, "label_not_found");
        self.builder.position_at_end(label_not_found_block);
        self.print_constant_string("ERROR: unable to fetch label\n", "label_not_found_str");
        self.builder.build_return(None);
    }

    /// Verify our module is valid.
    pub fn verify(&self) -> Result<(), String> {
        self.module.verify().map_err(|e| e.to_string())
    }

    /// Recompile the module into the execution engine, generating instructions.
    /// Note that compilation is still lazy, so the first time a function is called, it will be compiled.
    pub fn reload(&self) -> Result<(), String> {
        self.execution_engine
            .remove_module(&self.module)
            .map_err(|e| e.to_string())?;
        self.execution_engine
            .add_module(&self.module)
            .map_err(|_| "Failed to add module")?;
        self.verify()?;
        Ok(())
    }

    /// Link the given module into the current module, and recompile them. The result is verified.
    pub fn link_in_module(&self, module: Module<'ctx>) -> Result<(), String> {
        self.module
            .link_in_module(module)
            .map_err(|e| e.to_string())?;
        self.reload()
    }

    pub fn add_dynamic_function<F>(&mut self, f: F) -> Result<LabelWithContext<'ctx>, String>
    where
        F: FnOnce(&mut CodeGen<'ctx>, &Module<'ctx>) -> LabelWithContext<'ctx>,
    {
        let new_module = self.context.create_module("tmp_dynamic_module");
        let mut lab = f(self, &new_module);
        self.link_in_module(new_module)?;

        // Now that we have linked the function into our existing module, the function pointer has been invalidated.
        let func = self.module.get_last_function().unwrap();
        lab.function = func;

        Ok(lab)
    }

    /// Get the label associated with the given address, or the error function if it doesn't exist.
    pub fn get_label(&self, address: u64) -> FunctionValue<'ctx> {
        self.labels
            .iter()
            .find(|l| l.label.start() == (address as usize / mips_decomp::INSTRUCTION_SIZE))
            .map_or_else(|| self.label_not_found, |l| l.function)
    }

    pub fn call_label(&self, label: FunctionValue<'ctx>) {
        let callsite_value = self.builder.build_call(label, &[], "");
        callsite_value.set_tail_call(true);
        callsite_value.set_call_convention(crate::LLVM_CALLING_CONVENTION_FAST);
        self.builder.build_return(None);
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
    /// Sets the current insert block to the case the comparison is false, to support fallthrough.
    pub fn build_conditional_branch_set_ra(
        &self,
        cmp: IntValue<'ctx>,
        target_pc: u64,
        instr: &ParsedInstruction,
    ) {
        let i64_type = self.context.i64_type();
        let curr_block = self.builder.get_insert_block().unwrap();
        let curr_fn = curr_block.get_parent().unwrap();
        let curr_name = curr_block.get_name().to_str().unwrap().to_string();

        let then_block = {
            let then_name = format!("{curr_name}_cond_branch_then");
            self.context.append_basic_block(curr_fn, &then_name)
        };
        let else_block = {
            let else_name = format!("{curr_name}_cond_branch_else");
            self.context.append_basic_block(curr_fn, &else_name)
        };

        self.builder
            .build_conditional_branch(cmp, then_block, else_block);

        self.builder.position_at_end(then_block);
        {
            let next_instr_pc = {
                let offset = {
                    let value =
                        (if instr.discards_delay_slot() { 2 } else { 1 }) * INSTRUCTION_SIZE;
                    i64_type.const_int(value as u64, false)
                };

                let pc = self
                    .read_special_reg(register::Special::Pc)
                    .into_int_value();
                self.builder.build_int_add(pc, offset, "next_instr_pc")
            };

            self.write_general_reg(register::GeneralPurpose::Ra, next_instr_pc.into());
            self.call_label(self.get_label(target_pc));
        }

        self.builder.position_at_end(else_block);
    }

    /// Generate a conditional branch to the given address, ending the current basic block.
    /// Sets the current insert block to the case the comparison is false, to support fallthrough.
    pub fn build_conditional_branch(
        &self,
        cmp: IntValue<'ctx>,
        target_pc: u64,
    ) -> BasicBlock<'ctx> {
        let curr_block = self.builder.get_insert_block().unwrap();
        let curr_fn = curr_block.get_parent().unwrap();
        let curr_name = curr_block.get_name().to_str().unwrap().to_string();

        let then_name = format!("{curr_name}_cond_branch_then");
        let then_block = self.context.append_basic_block(curr_fn, &then_name);
        let else_name = format!("{curr_name}_cond_branch_else");
        let else_block = self.context.append_basic_block(curr_fn, &else_name);

        self.builder
            .build_conditional_branch(cmp, then_block, else_block);

        self.builder.position_at_end(then_block);
        self.call_label(self.get_label(target_pc));

        self.builder.position_at_end(else_block);

        then_block
    }

    /// Generate a dynamic jump to the given address, ending the current basic block.
    /// Ends execution if no label matching the address could be found.
    pub fn build_dynamic_jump(&self, address: IntValue<'ctx>) {
        let label_id = {
            let result = env_call!(self, RuntimeFunction::GetBlockId, [address]);
            result.try_as_basic_value().left().unwrap().into_int_value()
        };

        // Build a switch statement with all possible labels.
        let callsite_value =
            self.builder
                .build_call(self.dynamic_jump_to_id, &[label_id.into()], "dynamic_jump");
        callsite_value.set_tail_call(true);
        callsite_value.set_call_convention(crate::LLVM_CALLING_CONVENTION_FAST);
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
        // TODO: Dont assume 32-bit mode
        let i32_type = self.context.i32_type();

        let base = {
            let value = self.read_general_reg(instr.base()).into_int_value();
            self.builder.build_int_truncate(value, i32_type, "base")
        };
        let offset = i32_type.const_int(instr.offset() as _, false);

        let result = self.builder.build_int_add(base, offset, name);
        self.zero_extend_to_i64(result)
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
