use crate::{label::LabelWithContext, runtime::RuntimeFunction, LLVM_CALLING_CONVENTION_FAST};
use inkwell::{
    attributes::{Attribute, AttributeLoc},
    basic_block::BasicBlock,
    builder::Builder,
    context::Context,
    execution_engine::ExecutionEngine,
    intrinsics::Intrinsic,
    module::Module,
    types::IntType,
    values::{CallSiteValue, FunctionValue, GlobalValue, IntValue, PointerValue},
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
        // Type-check the arguments.
        let codegen: &CodeGen = $codegen;
        let func: &RuntimeFunction = &$func;
        let args = &[codegen.globals.env_ptr.as_pointer_value().into(), $($args.into()),*];
        let func = codegen.module.get_function(func.name()).unwrap();
        codegen.builder.build_call(func, args, "env_call")
    }};
}

const ATTRIBUTE_NAMES: &[&str] = &["noreturn", "nounwind", "nosync", "nofree", "nocf_check"];
pub fn function_attributes(context: &Context) -> [Attribute; ATTRIBUTE_NAMES.len()] {
    let mut attributes = [None; ATTRIBUTE_NAMES.len()];
    for (attr, name) in attributes.iter_mut().zip(ATTRIBUTE_NAMES.iter()) {
        let kind_id = Attribute::get_named_enum_kind_id(name);
        *attr = Some(context.create_enum_attribute(kind_id, 0));
    }
    // SAFETY: We have initialized all the attributes.
    attributes.map(|a| unsafe { a.unwrap_unchecked() })
}

#[derive(Debug)]
pub struct RegisterGlobals<'ctx> {
    pub general_purpose: GlobalValue<'ctx>,
    pub cp0: GlobalValue<'ctx>,
    pub fpu: GlobalValue<'ctx>,
    pub special: GlobalValue<'ctx>,
}

#[derive(Debug)]
pub struct Globals<'ctx> {
    // NOTE: The backing memory must be externally managed, so that the pointer remains valid across module reloads.
    pub stack_frame: (GlobalValue<'ctx>, *mut u64),
    pub env_ptr: GlobalValue<'ctx>,
    pub registers: RegisterGlobals<'ctx>,
}

#[derive(Debug)]
pub enum FallthroughAmount {
    One,
    Two,
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

    jump_helper: Option<FunctionValue<'ctx>>,
    // Separate functions so that we can patch calls to them at runtime without worrying about arguments.
    fallthrough_one_helper: Option<FunctionValue<'ctx>>,
    fallthrough_two_helper: Option<FunctionValue<'ctx>>,
}

impl<'ctx> CodeGen<'ctx> {
    pub fn new(
        context: &'ctx Context,
        module: Module<'ctx>,
        execution_engine: ExecutionEngine<'ctx>,
        globals: Globals<'ctx>,
    ) -> Self {
        Self {
            context,
            module,
            builder: context.create_builder(),
            execution_engine,
            globals,
            labels: Vec::new(),
            jump_helper: None,
            fallthrough_one_helper: None,
            fallthrough_two_helper: None,
        }
        .with_jump_helper()
        .with_fallthrough_helpers()
    }

    fn with_jump_helper(mut self) -> Self {
        let i64_type = self.context.i64_type();
        let void_type = self.context.void_type();

        // Generate the function signature
        let func = self.set_func_attrs(self.module.add_function(
            "jump_to_vaddr",
            void_type.fn_type(&[i64_type.into()], false),
            None,
        ));

        // Generate the function body.
        let block = self.context.append_basic_block(func, "entry");
        self.builder.position_at_end(block);
        {
            unsafe { self.restore_host_stack() } // Hack to avoid stack overflows

            let address = func.get_first_param().unwrap().into_int_value();
            let func_type = void_type.fn_type(&[], false);
            let ptr_type = func_type.ptr_type(Default::default());

            let ptr = {
                let raw = env_call!(&self, RuntimeFunction::GetFunctionPtr, [address])
                    .try_as_basic_value()
                    .left()
                    .unwrap()
                    .into_int_value();
                self.builder.build_int_to_ptr(raw, ptr_type, "jump_fn_ptr")
            };

            self.set_call_attrs(self.builder.build_indirect_call(
                func_type,
                ptr,
                &[],
                "jump_fn_call",
            ));
            self.builder.build_unreachable(); // The function should never return.
        }

        self.jump_helper = Some(func);
        self
    }

    fn with_fallthrough_helpers(mut self) -> Self {
        let make_func = |codegen: &Self, amount: u64, name: &str| -> FunctionValue<'ctx> {
            // Generate the function declaration.
            let func = codegen.set_func_attrs(codegen.module.add_function(
                name,
                codegen.context.void_type().fn_type(&[], false),
                None,
            ));

            // Generate the function body.
            let block = codegen.context.append_basic_block(func, "entry");
            codegen.builder.position_at_end(block);
            {
                let i64_type = codegen.context.i64_type();
                let pc = codegen.read_register(i64_type, register::Special::Pc);
                let offset = i64_type.const_int(amount * INSTRUCTION_SIZE as u64, false);
                let new_pc = codegen.builder.build_int_add(pc, offset, "new_pc");
                codegen.build_dynamic_jump(new_pc);
            }

            func
        };

        self.fallthrough_one_helper = Some(make_func(&self, 1, "fallthrough_one_instruction"));
        self.fallthrough_two_helper = Some(make_func(&self, 2, "fallthrough_two_instructions"));
        self
    }

    pub fn fallthrough_function(&self, amount: FallthroughAmount) -> FunctionValue<'ctx> {
        match amount {
            FallthroughAmount::One => self.fallthrough_one_helper.unwrap(),
            FallthroughAmount::Two => self.fallthrough_two_helper.unwrap(),
        }
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

    fn set_call_attrs(&self, callsite_value: CallSiteValue<'ctx>) -> CallSiteValue<'ctx> {
        callsite_value.set_tail_call(true);
        callsite_value.set_call_convention(LLVM_CALLING_CONVENTION_FAST);
        for attr in function_attributes(self.context) {
            callsite_value.add_attribute(AttributeLoc::Function, attr);
        }
        callsite_value
    }

    fn set_func_attrs(&self, func: FunctionValue<'ctx>) -> FunctionValue<'ctx> {
        func.set_call_conventions(LLVM_CALLING_CONVENTION_FAST);
        for attr in function_attributes(self.context) {
            func.add_attribute(AttributeLoc::Function, attr);
        }
        func
    }

    fn append_basic_block(&self, name: &str) -> BasicBlock<'ctx> {
        let current_func = self
            .builder
            .get_insert_block()
            .unwrap()
            .get_parent()
            .unwrap();
        self.context.append_basic_block(current_func, name)
    }

    fn current_block_terminated(&self) -> bool {
        self.builder
            .get_insert_block()
            .unwrap()
            .get_terminator()
            .is_some()
    }

    fn build_jump(&self, address: IntValue<'ctx>) {
        self.set_call_attrs(self.builder.build_call(
            self.jump_helper.unwrap(),
            &[address.into()],
            "build_jump",
        ));
        self.builder.build_unreachable();
    }

    /// Generate a dynamic jump to the given dynamic virtual address, ending the current basic block.
    pub fn build_dynamic_jump(&self, address: IntValue<'ctx>) {
        self.build_jump(self.zero_extend_to(self.context.i64_type(), address));
    }

    /// Generate a jump to the given constant virtual address, ending the current basic block.
    pub fn build_constant_jump(&self, address: u64) {
        if let Some(lab) = self
            .labels
            .iter()
            .find(|l| l.label.start() == (address as usize / INSTRUCTION_SIZE))
        {
            // Check if we have previously compiled this function.
            self.set_call_attrs(self.builder.build_call(lab.function, &[], "constant_jump"));
        } else {
            // Let the runtime environment JIT it, then jump to it.
            self.build_dynamic_jump(self.context.i64_type().const_int(address, false));
        };
    }

    pub fn call_function(&self, func: FunctionValue<'ctx>) {
        self.set_call_attrs(self.builder.build_call(func, &[], "call_fn"));
        self.builder.build_unreachable();
    }

    pub fn build_panic(&self, string: &str, storage_name: &str) {
        let ptr = self
            .builder
            .build_global_string_ptr(string, storage_name)
            .as_pointer_value();
        let len = self.context.i64_type().const_int(string.len() as _, false);
        env_call!(self, RuntimeFunction::Panic, [ptr, len]);
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

    /// If the comparison is true, execute the function generated within the given closure, otherwise skip it.
    /// When the true block does not already have a terminator, a jump to the false block is generated.
    /// This positions the builder at the false case. The true case is returned.
    pub fn build_if(
        &self,
        name: &str,
        cmp: IntValue<'ctx>,
        then: impl FnOnce(),
    ) -> BasicBlock<'ctx> {
        let (then_block, else_block) = (
            self.append_basic_block(&format!("{name}_then")),
            self.append_basic_block(&format!("{name}_else")),
        );
        self.builder
            .build_conditional_branch(cmp, then_block, else_block);

        self.builder.position_at_end(then_block);
        then();
        if !self.current_block_terminated() {
            self.builder.build_unconditional_branch(else_block);
        }

        self.builder.position_at_end(else_block);
        then_block
    }

    /// If the comparison is true, execute the first function generated within the given closure,
    /// otherwise the second. If either block does not already have a terminator, a jump to a merge block is generated.
    /// This positions the builder at the merge block. The true and false cases are returned, in order.
    pub fn build_if_else(
        &self,
        name: &str,
        cmp: IntValue<'ctx>,
        then: impl FnOnce(),
        otherwise: impl FnOnce(),
    ) -> (BasicBlock<'ctx>, BasicBlock<'ctx>) {
        let merge_block = self.append_basic_block(&format!("{name}_merge"));
        let then_block = self.build_if(name, cmp, || {
            then();
            if !self.current_block_terminated() {
                self.builder.build_unconditional_branch(merge_block);
            }
        });

        // `build_if` leaves us positioned at the false block, so we can just call `otherwise`.
        let else_block = self.builder.get_insert_block().unwrap();
        otherwise();
        if !self.current_block_terminated() {
            self.builder.build_unconditional_branch(merge_block);
        }

        self.builder.position_at_end(merge_block);
        (then_block, else_block)
    }

    pub fn build_conditional_branch_set_ra(
        &self,
        cmp: IntValue<'ctx>,
        target_pc: u64,
        next_instr_pc: IntValue<'ctx>,
    ) -> BasicBlock<'ctx> {
        self.build_if("conditional_branch_set_ra", cmp, || {
            self.write_register(register::GeneralPurpose::Ra, next_instr_pc);
            self.build_constant_jump(target_pc);
        })
    }

    pub fn build_conditional_branch(
        &self,
        cmp: IntValue<'ctx>,
        target_pc: u64,
    ) -> BasicBlock<'ctx> {
        self.build_if("conditional_branch", cmp, || {
            self.build_constant_jump(target_pc);
        })
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

    pub fn base_plus_offset(&self, instr: &ParsedInstruction, add_name: &str) -> IntValue<'ctx> {
        let i16_type = self.context.i16_type();
        let i32_type = self.context.i32_type();
        let i64_type = self.context.i64_type();

        let base = self.read_general_register(i32_type, instr.base());
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
            Register::GeneralPurpose(_) => {
                self.globals.registers.general_purpose.as_pointer_value()
            }
            Register::Special(_) => self.globals.registers.special.as_pointer_value(),
            Register::Cp0(_) => self.globals.registers.cp0.as_pointer_value(),
            Register::Fpu(_) => self.globals.registers.fpu.as_pointer_value(),
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

    /// Read the general-purpose register (GPR) at the given index.
    /// If the `ty` is less than 64 bits the value will be truncated, and the lower bits returned.
    pub fn read_general_register<T>(&self, ty: IntType<'ctx>, index: T) -> IntValue<'ctx>
    where
        T: Into<u64>,
    {
        let reg = register::GeneralPurpose::from_repr(index.into() as _).unwrap();
        if reg == register::GeneralPurpose::Zero {
            // Register zero is hardwired to zero.
            ty.const_zero()
        } else {
            let name = format!("{}_", reg.name());
            let reg_ptr = self.register_pointer(reg);
            self.builder.build_load(ty, reg_ptr, &name).into_int_value()
        }
    }

    /// Read the CP0 register at the given index.
    /// If the `ty` is less than 64 bits the value will be truncated, and the lower bits returned.
    pub fn read_cp0_register<T>(&self, ty: IntType<'ctx>, index: T) -> IntValue<'ctx>
    where
        T: Into<u64>,
    {
        let reg = register::Cp0::from_repr(index.into() as _).unwrap();
        let name = &format!("{}_", reg.name());
        let register = self.register_pointer(reg);
        self.builder.build_load(ty, register, name).into_int_value()
    }

    /// Read the floating-point unit (FPU) register at the given index.
    /// If the `ty` is less than 64 bits the value will be truncated, and the lower bits returned.
    pub fn read_fpu_register<T>(&self, ty: IntType<'ctx>, index: T) -> IntValue<'ctx>
    where
        T: Into<u64>,
    {
        let reg = register::Fpu::from_repr(index.into() as _).unwrap();
        let name = format!("{}_", reg.name());
        let reg_ptr = self.register_pointer(reg);
        self.builder.build_load(ty, reg_ptr, &name).into_int_value()
    }

    /// Read the specified miscellaneous "special" register.
    /// If the `ty` is less than 64 bits the value will be truncated, and the lower bits returned.
    pub fn read_special_register(
        &self,
        ty: IntType<'ctx>,
        reg: register::Special,
    ) -> IntValue<'ctx> {
        let name = &format!("{}_", reg.name());
        let register = self.register_pointer(reg);
        self.builder.build_load(ty, register, name).into_int_value()
    }

    /// Read the specified register.
    /// If the `ty` is less than 64 bits the value will be truncated, and the lower bits returned.
    pub fn read_register<T>(&self, ty: IntType<'ctx>, reg: T) -> IntValue<'ctx>
    where
        T: Into<Register>,
    {
        match reg.into() {
            Register::GeneralPurpose(reg) => self.read_general_register(ty, reg),
            Register::Special(reg) => self.read_special_register(ty, reg),
            Register::Cp0(reg) => self.read_cp0_register(ty, reg),
            Register::Fpu(reg) => self.read_fpu_register(ty, reg),
        }
    }

    pub fn write_general_register<T>(&self, index: T, value: IntValue<'ctx>)
    where
        T: Into<u64>,
    {
        let reg = register::GeneralPurpose::from_repr(index.into() as _).unwrap();
        if reg != register::GeneralPurpose::Zero {
            // Register zero is hardwired to zero.
            let register = self.register_pointer(reg);
            self.builder.build_store(register, value);
        }
    }

    pub fn write_cp0_register<T>(&self, index: T, value: IntValue<'ctx>)
    where
        T: Into<u64>,
    {
        let reg = register::Cp0::from_repr(index.into() as _).unwrap();
        let reg_ptr = self.register_pointer(reg);
        self.builder.build_store(reg_ptr, value);
    }

    pub fn write_fpu_register<T>(&self, index: T, value: IntValue<'ctx>)
    where
        T: Into<u64>,
    {
        let reg = register::Fpu::from_repr(index.into() as _).unwrap();
        let reg_ptr = self.register_pointer(reg);
        self.builder.build_store(reg_ptr, value);
    }

    pub fn write_special_register(&self, reg: register::Special, value: IntValue<'ctx>) {
        let register = self.register_pointer(reg);
        self.builder.build_store(register, value);
    }

    pub fn write_register<T>(&self, reg: T, value: IntValue<'ctx>)
    where
        T: Into<Register>,
    {
        match reg.into() {
            Register::GeneralPurpose(reg) => self.write_general_register(reg, value),
            Register::Cp0(reg) => self.write_cp0_register(reg, value),
            Register::Fpu(reg) => self.write_fpu_register(reg, value),
            Register::Special(reg) => self.write_special_register(reg, value),
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
