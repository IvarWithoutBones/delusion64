use crate::{label::LabelWithContext, runtime::RuntimeFunction, LLVM_CALLING_CONVENTION_FAST};
use inkwell::{
    attributes::{Attribute, AttributeLoc},
    basic_block::BasicBlock,
    builder::Builder,
    context::Context,
    execution_engine::{ExecutionEngine, JitFunction, UnsafeFunctionPointer},
    intrinsics::Intrinsic,
    module::Module,
    types::IntType,
    values::{
        BasicMetadataValueEnum, CallSiteValue, FunctionValue, GlobalValue, IntValue, PointerValue,
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

// TODO: replace this with `LazyCell` once its stabilized, see:
// https://github.com/rust-lang/rust/issues/109736
pub fn function_attributes(context: &Context) -> Vec<Attribute> {
    // TODO: would `naked` make sense to skip the prologue/epilogue?
    // `nounwind` here makes panics look a bit funky, but its proobably fine.
    const ATTRIBUTE_NAMES: &[&str] = &["noreturn", "nounwind", "nosync", "nofree", "nocf_check"];

    let mut attributes = Vec::new();
    for attr_name in ATTRIBUTE_NAMES {
        let kind_id = Attribute::get_named_enum_kind_id(attr_name);
        let attr = context.create_enum_attribute(kind_id, 0);
        attributes.push(attr);
    }
    attributes
}

#[derive(Debug)]
pub struct Globals<'ctx> {
    pub env_ptr: GlobalValue<'ctx>,
    pub general_purpose_regs: GlobalValue<'ctx>,
    pub special_regs: GlobalValue<'ctx>,
    pub cp0_regs: GlobalValue<'ctx>,

    // NOTE: The backing memory must be externally managed, so that the pointer remains valid across module reloads.
    pub stack_frame: (GlobalValue<'ctx>, Box<u64>),
}

#[derive(Debug)]
pub struct CodeGen<'ctx> {
    pub context: &'ctx Context,
    pub module: Module<'ctx>,
    pub builder: Builder<'ctx>,
    pub execution_engine: ExecutionEngine<'ctx>,

    /// Global mappings to the runtime environment.
    pub globals: Globals<'ctx>,

    /// The generated labels, with associated functions.
    pub labels: Vec<LabelWithContext<'ctx>>,

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
        label_not_found.set_call_conventions(LLVM_CALLING_CONVENTION_FAST);

        let codegen = Self {
            context,
            module,
            builder,
            execution_engine,
            globals,
            labels,
            label_not_found,
        };

        codegen.init_label_not_found();
        codegen
    }

    /// Initializes the error case that occurs when attempting to jump to a label that wasn't found.
    fn init_label_not_found(&self) {
        let label_not_found_block = self
            .context
            .append_basic_block(self.label_not_found, "label_not_found");
        self.builder.position_at_end(label_not_found_block);
        self.print_constant_string("ERROR: unable to fetch label\n", "label_not_found_str");
        env_call!(self, RuntimeFunction::Panic, []);
        self.builder.build_return(None);
    }

    /// Saves the host stack frame to a global pointer, so that it can be restored later.
    pub fn save_host_stack(&self) {
        let stack_save_fn = {
            let intrinsic = Intrinsic::find("llvm.stacksave").unwrap();
            intrinsic.get_declaration(&self.module, &[]).unwrap()
        };

        let stack_ptr = self
            .builder
            .build_call(stack_save_fn, &[], "stack_save")
            .try_as_basic_value()
            .left()
            .unwrap();

        self.builder
            .build_store(self.globals.stack_frame.0.as_pointer_value(), stack_ptr);
    }

    /// Restores the host stack frame from a global pointer.
    /// Note: This must be called after `save_host_stack`, otherwise a null pointer will be dereferenced.
    pub unsafe fn restore_host_stack(&self) {
        let stack_restore_fn = {
            let intrinsic = Intrinsic::find("llvm.stackrestore").unwrap();
            intrinsic.get_declaration(&self.module, &[]).unwrap()
        };

        let ptr_type = self.context.i64_type().ptr_type(Default::default());
        let stack_ptr = self.builder.build_load(
            ptr_type,
            self.globals.stack_frame.0.as_pointer_value(),
            "stack_ptr",
        );

        self.builder
            .build_call(stack_restore_fn, &[stack_ptr.into()], "stack_restore");
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

    fn set_call_attrs(&self, callsite_value: CallSiteValue<'ctx>) {
        callsite_value.set_tail_call(true);
        callsite_value.set_call_convention(LLVM_CALLING_CONVENTION_FAST);

        for attr in function_attributes(self.context) {
            callsite_value.add_attribute(AttributeLoc::Function, attr);
        }
    }

    /// Generate a dynamic jump to the given virtual address, ending the current basic block.
    pub fn call_label_dynamic(&self, address: IntValue<'ctx>) -> CallSiteValue<'ctx> {
        let fn_ty = self.context.void_type().fn_type(&[], false);
        let ptr_ty = fn_ty.ptr_type(Default::default());

        // Get a pointer to the host function at the given guest pointer, or JIT compile it if its not yet generated.
        let func_ptr_callsite = env_call!(self, RuntimeFunction::GetFunctionPtr, [address]);
        self.set_call_attrs(func_ptr_callsite);

        let raw_ptr = func_ptr_callsite
            .try_as_basic_value()
            .left()
            .unwrap()
            .into_int_value();

        // Build an indirect call to the function pointer.
        let ptr = self.builder.build_int_to_ptr(raw_ptr, ptr_ty, "fn_ptr");
        self.builder
            .build_indirect_call(fn_ty, ptr, &[], "call_fn_ptr")
    }

    /// Generate a jump to the given virtual address, ending the current basic block.
    /// If the label at the given address has already been compiled, it will be called directly.
    /// Otherwise, it will be JIT compiled and indirectly called.
    pub fn build_jump(&self, address: u64) {
        let callsite_value = if let Some(lab) = self
            .labels
            .iter()
            .find(|l| l.label.start() == (address as usize / INSTRUCTION_SIZE))
        {
            self.builder.build_call(lab.function, &[], "call_fn")
        } else {
            let addr = self.context.i64_type().const_int(address, false);
            self.call_label_dynamic(addr)
        };

        self.set_call_attrs(callsite_value);
        self.builder.build_unreachable();
    }

    /// Generate a dynamic jump to the given virtual address, ending the current basic block.
    /// If the label at the given address has already been compiled, it will be called directly.
    /// Otherwise, it will be JIT compiled and indirectly called.
    pub fn build_dynamic_jump(&self, address: IntValue<'ctx>) {
        let address = self.zero_extend_to(self.context.i64_type(), address);
        self.set_call_attrs(self.call_label_dynamic(address));
        self.builder.build_unreachable();
    }

    pub fn call_function(&self, func: FunctionValue<'ctx>) {
        self.set_call_attrs(self.builder.build_call(func, &[], "call_fn"));
        self.builder.build_unreachable();
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

    /// Performs a comparisons between two integers, returning the result as an i32.
    pub fn build_compare_as_i32(
        &self,
        op: IntPredicate,
        lhs: IntValue<'ctx>,
        rhs: IntValue<'ctx>,
        cmp_name: &str,
    ) -> IntValue<'ctx> {
        self.zero_extend_to(
            self.context.i32_type(),
            self.builder.build_int_compare(op, lhs, rhs, cmp_name),
        )
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

                let pc = self.read_special_reg(register::Special::Pc);
                self.builder.build_int_add(pc, offset, "next_instr_pc")
            };

            self.write_general_reg(register::GeneralPurpose::Ra, next_instr_pc);
            self.build_jump(target_pc);
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
        self.build_jump(target_pc);

        self.builder.position_at_end(else_block);

        then_block
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

    /// Sign-extends the given value to the given type.
    pub fn sign_extend_to(&self, ty: IntType<'ctx>, value: IntValue<'ctx>) -> IntValue<'ctx> {
        let name = format!("sign_ext_to_i{}", ty.get_bit_width());
        self.builder.build_int_s_extend(value, ty, &name)
    }

    /// Zero-extends the given value to the given type.
    pub fn zero_extend_to(&self, ty: IntType<'ctx>, value: IntValue<'ctx>) -> IntValue<'ctx> {
        let name = format!("zero_ext_to_i{}", ty.get_bit_width());
        self.builder.build_int_z_extend(value, ty, &name)
    }

    /// Truncates the given value to the given type.
    pub fn truncate_to(&self, ty: IntType<'ctx>, value: IntValue<'ctx>) -> IntValue<'ctx> {
        let name = format!("trunc_to_i{}", ty.get_bit_width());
        self.builder.build_int_truncate(value, ty, &name)
    }

    /// Gets the address of the instruction after the current one,
    /// by adding the instruction size to the program counter (PC).
    pub fn next_instruction_address(&self) -> IntValue<'ctx> {
        let pc = self.read_special_reg(register::Special::Pc);
        let instr_size = self
            .context
            .i32_type()
            .const_int(INSTRUCTION_SIZE as _, false);
        let next_pc = self
            .builder
            .build_int_add(pc, instr_size, "next_instr_addr");
        self.zero_extend_to(self.context.i64_type(), next_pc)
    }

    pub fn base_plus_offset(&self, instr: &ParsedInstruction, add_name: &str) -> IntValue<'ctx> {
        // TODO: Dont assume 32-bit mode
        let i16_type = self.context.i16_type();
        let i32_type = self.context.i32_type();
        let i64_type = self.context.i64_type();

        let base = self.read_general_reg(instr.base());
        let offset = self.sign_extend_to(i32_type, i16_type.const_int(instr.offset() as _, true));
        let result = self.builder.build_int_add(base, offset, add_name);
        self.zero_extend_to(i64_type, result)
    }

    #[inline]
    fn register_pointer<T>(&self, reg: T) -> PointerValue<'ctx>
    where
        T: Into<Register>,
    {
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

    pub fn read_general_reg<T>(&self, index: T) -> IntValue<'ctx>
    where
        T: Into<u64>,
    {
        // TODO: dont hardcode 32-bit register width
        let reg_ty = self.context.i32_type();
        let reg = register::GeneralPurpose::from_repr(index.into() as _).unwrap();
        if reg == register::GeneralPurpose::Zero {
            // Register zero is always zero
            reg_ty.const_zero()
        } else {
            let name = format!("{}_", reg.name());
            let reg_ptr = self.register_pointer(reg);
            self.builder
                .build_load(reg_ty, reg_ptr, &name)
                .into_int_value()
        }
    }

    // TODO: Duplicating this function from `read_general_reg` sucks. Merge them.
    pub fn read_general_reg_i64<T>(&self, index: T) -> IntValue<'ctx>
    where
        T: Into<u64>,
    {
        // TODO: dont hardcode 64-bit register width
        let reg_ty = self.context.i64_type();
        let reg = register::GeneralPurpose::from_repr(index.into() as _).unwrap();
        if reg == register::GeneralPurpose::Zero {
            // Register zero is always zero
            reg_ty.const_zero()
        } else {
            let name = format!("{}_", reg.name());
            let reg_ptr = self.register_pointer(reg);
            self.builder
                .build_load(reg_ty, reg_ptr, &name)
                .into_int_value()
        }
    }

    pub fn read_cp0_reg<T>(&self, index: T) -> IntValue<'ctx>
    where
        T: Into<u64>,
    {
        // TODO: dont hardcode 32-bit register width
        let reg_ty = self.context.i32_type();
        let reg = register::Cp0::from_repr(index.into() as _).unwrap();
        let register = self.register_pointer(reg);
        self.builder
            .build_load(reg_ty, register, &format!("{}_", reg.name()))
            .into_int_value()
    }

    pub fn read_special_reg(&self, reg: register::Special) -> IntValue<'ctx> {
        // TODO: dont hardcode 32-bit register width
        let reg_ty = self.context.i32_type();
        let register = self.register_pointer(reg);
        self.builder
            .build_load(reg_ty, register, &format!("{}_", reg.name()))
            .into_int_value()
    }

    pub fn read_register<T>(&self, reg: T) -> IntValue<'ctx>
    where
        T: Into<Register>,
    {
        match reg.into() {
            Register::GeneralPurpose(reg) => self.read_general_reg(reg),
            Register::Special(reg) => self.read_special_reg(reg),
            Register::Cp0(reg) => self.read_cp0_reg(reg),
        }
    }

    pub fn write_general_reg<T>(&self, index: T, value: IntValue<'ctx>)
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

    pub fn write_cp0_reg<T>(&self, index: T, value: IntValue<'ctx>)
    where
        T: Into<u64>,
    {
        let reg = register::Cp0::from_repr(index.into() as _).unwrap();
        let reg_ptr = self.register_pointer(reg);
        self.builder.build_store(reg_ptr, value);
    }

    pub fn write_special_reg(&self, reg: register::Special, value: IntValue<'ctx>) {
        let register = self.register_pointer(reg);
        self.builder.build_store(register, value);
    }

    pub fn write_register<T>(&self, reg: T, value: IntValue<'ctx>)
    where
        T: Into<Register>,
    {
        match reg.into() {
            Register::GeneralPurpose(reg) => self.write_general_reg(reg, value),
            Register::Special(reg) => self.write_special_reg(reg, value),
            Register::Cp0(reg) => self.write_cp0_reg(reg, value),
        }
    }

    pub fn read_memory(&self, ty: IntType<'ctx>, address: IntValue<'ctx>) -> IntValue<'ctx> {
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
            .into_int_value()
    }

    pub fn write_memory(&self, address: IntValue<'ctx>, value: IntValue<'ctx>) {
        let ty = value.get_type();
        let func = match ty.get_bit_width() {
            8 => RuntimeFunction::WriteI8,
            16 => RuntimeFunction::WriteI16,
            32 => RuntimeFunction::WriteI32,
            64 => RuntimeFunction::WriteI64,
            _ => panic!("unimplemented store_memory type: {ty}"),
        };

        env_call!(self, func, [address, value]);
    }
}
