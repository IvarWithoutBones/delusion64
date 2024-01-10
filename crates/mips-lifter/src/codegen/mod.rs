use self::{address_map::VirtualAddressMap, init::Helpers};
use crate::{
    label::{JitFunctionPointer, LabelWithContext},
    runtime::RuntimeFunction,
    target::{self, Globals as _, RegisterID, RegisterStorage, Target},
    LLVM_CALLING_CONVENTION_TAILCC,
};
use inkwell::{
    attributes::{Attribute, AttributeLoc},
    basic_block::BasicBlock,
    builder::Builder,
    context::Context,
    execution_engine::ExecutionEngine,
    module::Module,
    types::{BasicType, FloatType, IntType},
    values::{
        BasicValue, BasicValueEnum, CallSiteValue, FloatValue, FunctionValue, GlobalValue,
        IntValue, PointerValue,
    },
    AtomicOrdering,
};
use std::collections::HashMap;
use tartan_bitfield::bitfield;

#[macro_use]
mod comparison;
pub(crate) mod address_map;
mod init;

#[macro_export]
macro_rules! env_call {
    ($codegen:expr, $func:expr, [$($args:expr),*]) => {{
        // Type-check the arguments.
        let codegen: &$crate::codegen::CodeGen<_> = $codegen;
        let func: &$crate::runtime::RuntimeFunction = &$func;

        let globals = codegen.globals();
        let args = &[globals.env_ptr.as_pointer_value().into(), $($args.into()),*];
        let func = *globals.functions.get(func).expect("runtime functions not initialised");
        codegen.builder.build_call(func, args, concat!("env_call_", stringify!($func)))
    }};
}

pub const FUNCTION_PREFIX: &str = "delusion64_jit_";

const ATTRIBUTE_NAMES: &[&str] = &["noreturn", "nofree", "nocf_check"];
pub fn function_attributes(context: &Context) -> [Attribute; ATTRIBUTE_NAMES.len()] {
    let mut attributes = [None; ATTRIBUTE_NAMES.len()];
    for (attr, name) in attributes.iter_mut().zip(ATTRIBUTE_NAMES.iter()) {
        let kind_id = Attribute::get_named_enum_kind_id(name);
        *attr = Some(context.create_enum_attribute(kind_id, 0));
    }
    // SAFETY: We have initialized all the attributes.
    attributes.map(|a| unsafe { a.unwrap_unchecked() })
}

bitfield! {
    /// Flags used to track the state of the JIT.
    pub(crate) struct Flags(u64) {
        [0] pub inside_delay_slot,
    }
}

/// Mappings to the runtime environment.
#[derive(Debug)]
pub struct Globals<'ctx, T: Target> {
    pub env_ptr: GlobalValue<'ctx>,
    pub flags_ptr: GlobalValue<'ctx>,
    pub registers: <T::Registers as RegisterStorage>::Globals<'ctx>,
    // TODO: Switch to a slice since this gets allocated for every compiled basic block
    pub functions: HashMap<RuntimeFunction, FunctionValue<'ctx>>,
}

#[derive(Debug)]
pub struct CodeGen<'ctx, T: Target> {
    pub context: &'ctx Context,
    pub module: Module<'ctx>,
    pub builder: Builder<'ctx>,
    pub execution_engine: ExecutionEngine<'ctx>,
    pub labels: VirtualAddressMap<'ctx, T>,
    globals: Option<Globals<'ctx, T>>,
    helpers: Helpers<'ctx>,
    /// Every single module we compiled. We need to keep these around to cleanly shut down LLVM.
    pub modules: Vec<Module<'ctx>>,
}

impl<'ctx, T: Target> CodeGen<'ctx, T> {
    pub(crate) fn new(
        context: &'ctx Context,
        module: Module<'ctx>,
        execution_engine: ExecutionEngine<'ctx>,
    ) -> Self {
        Self {
            modules: vec![],
            context,
            module,
            helpers: Helpers::new(),
            builder: context.create_builder(),
            execution_engine,
            globals: None,
            labels: VirtualAddressMap::new(),
        }
    }

    /// Verify our module is valid.
    pub fn verify(&self) -> Result<(), String> {
        self.module.verify().map_err(|e| {
            self.module.print_to_file("./error.ll").unwrap();
            e.to_string()
        })
    }

    fn set_func_attrs(&self, func: FunctionValue<'ctx>) -> FunctionValue<'ctx> {
        func.set_call_conventions(LLVM_CALLING_CONVENTION_TAILCC);
        for attr in function_attributes(self.context) {
            func.add_attribute(AttributeLoc::Function, attr);
        }
        func
    }

    fn set_call_attrs(&self, callsite_value: CallSiteValue<'ctx>) -> CallSiteValue<'ctx> {
        callsite_value.set_tail_call(true);
        callsite_value.set_call_convention(LLVM_CALLING_CONVENTION_TAILCC);
        for attr in function_attributes(self.context) {
            callsite_value.add_attribute(AttributeLoc::Function, attr);
        }
        callsite_value
    }

    pub(crate) fn globals(&self) -> &Globals<'ctx, T> {
        self.globals.as_ref().expect("globals are not initialised")
    }

    fn build_jump(&self, address: IntValue<'ctx>) {
        self.set_call_attrs(self.builder.build_call(
            self.helpers.jump(),
            &[address.into()],
            "build_jump",
        ));
        self.builder.build_return(None);
    }

    pub(crate) fn build_jump_to_jit_func_ptr(&self, host_ptr: IntValue<'ctx>, name: &str) {
        let func_type = self.context.void_type().fn_type(&[], false);
        let ptr_type = func_type.ptr_type(Default::default());
        let ptr = self.builder.build_int_to_ptr(host_ptr, ptr_type, name);

        self.set_call_attrs(self.builder.build_indirect_call(func_type, ptr, &[], name));
        self.builder.build_return(None);
    }

    /// Generate a dynamic jump to the given dynamic virtual address, ending the current basic block.
    pub fn build_dynamic_jump(&self, address: IntValue<'ctx>) {
        self.build_jump(self.zero_extend_to(self.context.i64_type(), address));
    }

    /// Generate a jump to the given constant virtual address, ending the current basic block.
    pub fn build_constant_jump(&self, address: u64) {
        if let Ok(lab) = self.labels.get(address) {
            // If we have previously compiled this function we can insert a direct jump.
            self.set_call_attrs(self.builder.build_call(lab.function, &[], "constant_jump"));
            self.builder.build_return(None);
        } else {
            // Let the runtime environment JIT it, then jump to it.
            self.build_dynamic_jump(self.context.i64_type().const_int(address, false));
        };
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

    pub fn fallthrough_function(&self, amount: FallthroughAmount) -> FunctionValue<'ctx> {
        match amount {
            FallthroughAmount::One => self.helpers.fallthrough_one(),
            FallthroughAmount::Two => self.helpers.fallthrough_two(),
        }
    }

    pub fn get_insert_block(&self) -> BasicBlock<'ctx> {
        self.builder.get_insert_block().unwrap()
    }

    fn current_block_terminated(&self) -> bool {
        self.get_insert_block().get_terminator().is_some()
    }

    fn append_basic_block(&self, name: &str) -> BasicBlock<'ctx> {
        let current_func = self.get_insert_block().get_parent().unwrap();
        self.context.append_basic_block(current_func, name)
    }

    pub fn call_function(&self, func: FunctionValue<'ctx>) {
        self.set_call_attrs(self.builder.build_call(func, &[], "call_fn"));
        self.builder.build_return(None);
    }

    pub fn link_in_module(
        &mut self,
        module: Module<'ctx>,
        lab: &mut LabelWithContext<T>,
    ) -> Result<(), String> {
        // Map all functions from the new module into our execution engine, and add them to our main module.
        self.execution_engine.add_module(&module).unwrap();

        for (i, func) in module
            .get_functions()
            // Only a declaration, not compiled in this module.
            .filter(|func| func.count_basic_blocks() > 0)
            .enumerate()
        {
            // Create a function declaration, without a body.
            let name = func.get_name().to_str().unwrap();
            let func_decl = self.module.add_function(name, func.get_type(), None);
            self.set_func_attrs(func_decl);

            // Map the function pointer to the declaration.
            let ptr = self.execution_engine.get_function_address(name).unwrap();
            if i == 0 {
                // We cache a pointer to the function in the label, but only the first.
                lab.pointer = Some(ptr as JitFunctionPointer);
            } else {
                panic!("multiple function bodies found in label");
            };

            self.execution_engine.add_global_mapping(&func_decl, ptr);
        }

        self.modules.push(module);
        Ok(())
    }

    pub fn add_dynamic_function<F>(
        &mut self,
        module: Module<'ctx>,
        globals: Globals<'ctx, T>,
        f: F,
    ) -> Result<LabelWithContext<'ctx, T>, String>
    where
        F: FnOnce(&CodeGen<'ctx, T>, &Module<'ctx>) -> Result<LabelWithContext<'ctx, T>, String>,
    {
        self.globals = Some(globals);
        self.helpers.map_into(&module);
        let mut lab = f(self, &module)?;
        self.link_in_module(module, &mut lab)?;
        if let Err(e) = self.verify() {
            panic!("failed to verify generated code: {e}");
        }
        Ok(lab)
    }

    pub fn build_panic(&self, string: &str, storage_name: &str) {
        let ptr = self
            .builder
            .build_global_string_ptr(string, storage_name)
            .as_pointer_value();
        let len = self.context.i64_type().const_int(string.len() as _, false);
        env_call!(self, RuntimeFunction::Panic, [ptr, len]);
        self.builder.build_unreachable();
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

    /// Splits the given integer in two, returning the high and low order bits in that order.
    /// The resulting integers will be half the size of the original.
    pub fn split(&self, value: IntValue<'ctx>) -> (IntValue<'ctx>, IntValue<'ctx>) {
        let ty = value.get_type();
        let half_ty = match ty.get_bit_width() {
            16 => self.context.i8_type(),
            32 => self.context.i16_type(),
            64 => self.context.i32_type(),
            128 => self.context.i64_type(),
            _ => unimplemented!("split type {ty}"),
        };

        let hi = {
            let shift = ty.const_int(half_ty.get_bit_width() as u64, false);
            let shifted = self
                .builder
                .build_right_shift(value, shift, false, "split_hi");
            self.truncate_to(half_ty, shifted)
        };
        let lo = self.truncate_to(half_ty, value);
        (hi, lo)
    }

    pub fn build_mask(&self, to_mask: IntValue<'ctx>, mask: u64, name: &str) -> IntValue<'ctx> {
        let mask = to_mask.get_type().const_int(mask, false);
        self.builder.build_and(to_mask, mask, name)
    }

    /// Get a host pointer to a function which simply returns.
    /// Calling this inside of a JIT'ed block will return to the callee of [`JitBuilder::run`].
    pub(crate) fn return_from_jit_ptr(&self) -> JitFunctionPointer {
        const NAME: &str = "return_from_jit_helper";
        let module = self.context.create_module("return_from_jit_helper_module");
        let func = self.set_func_attrs(module.add_function(
            NAME,
            self.context.void_type().fn_type(&[], false),
            None,
        ));

        let block = self.context.append_basic_block(func, "entry");
        self.builder.position_at_end(block);
        {
            // Since every function we call uses tail calls, this should immediately return to the caller.
            self.builder.build_return(None);
        }

        self.execution_engine.add_module(&module).unwrap();
        self.execution_engine.get_function_address(NAME).unwrap() as JitFunctionPointer
    }

    pub fn read_memory(&self, ty: IntType<'ctx>, address: IntValue<'ctx>) -> IntValue<'ctx> {
        let func = match ty.get_bit_width() {
            8 => RuntimeFunction::ReadI8,
            16 => RuntimeFunction::ReadI16,
            32 => RuntimeFunction::ReadI32,
            64 => RuntimeFunction::ReadI64,
            _ => panic!("unimplemented read_memory type: {ty}"),
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
            _ => panic!("unimplemented write_memory type: {ty}"),
        };

        env_call!(self, func, [address, value]);
    }

    pub fn read_physical_memory(
        &self,
        ty: IntType<'ctx>,
        address: IntValue<'ctx>,
    ) -> IntValue<'ctx> {
        let func = match ty.get_bit_width() {
            8 => RuntimeFunction::ReadPhysicalI8,
            16 => RuntimeFunction::ReadPhysicalI16,
            32 => RuntimeFunction::ReadPhysicalI32,
            64 => RuntimeFunction::ReadPhysicalI64,
            _ => panic!("unimplemented read_physical_memory type: {ty}"),
        };

        env_call!(self, func, [address])
            .try_as_basic_value()
            .left()
            .unwrap()
            .into_int_value()
    }

    pub fn write_physical_memory(&self, address: IntValue<'ctx>, value: IntValue<'ctx>) {
        let ty = value.get_type();
        let func = match ty.get_bit_width() {
            8 => RuntimeFunction::WritePhysicalI8,
            16 => RuntimeFunction::WritePhysicalI16,
            32 => RuntimeFunction::WritePhysicalI32,
            64 => RuntimeFunction::WritePhysicalI64,
            _ => panic!("unimplemented write_physical_memory type: {ty}"),
        };

        env_call!(self, func, [address, value]);
    }

    pub fn get_physical_address(&self, vaddr: IntValue<'ctx>) -> IntValue<'ctx> {
        env_call!(self, RuntimeFunction::GetPhysicalAddress, [vaddr])
            .try_as_basic_value()
            .left()
            .unwrap()
            .into_int_value()
    }

    pub fn set_inside_delay_slot(&self, inside: bool) {
        let i64_type = self.context.i64_type();
        let flags_ptr = self.globals().flags_ptr.as_pointer_value();
        let prev = {
            let flags = self
                .builder
                .build_load(i64_type, flags_ptr, "flags_prev")
                .into_int_value();
            // Zero the least significant bit as to overwrite it with the new value.
            self.builder.build_and(
                flags,
                i64_type.const_int(!1, false),
                "flags_prev_delay_zeroed",
            )
        };
        let new = {
            // Set the least significant bit to the new value.
            let inside = i64_type.const_int(inside as u64, false);
            self.builder.build_or(prev, inside, "flags_new_delay")
        };
        self.builder.build_store(flags_ptr, new);
    }

    /*
       Register access helpers, to assist implementing target-specific code.
    */

    pub(crate) fn read_register_pointer(
        &self,
        ptr: PointerValue<'ctx>,
        ty: impl BasicType<'ctx>,
        reg: RegisterID<'ctx, T>,
    ) -> BasicValueEnum<'ctx> {
        // TODO: get the registers name
        let value = self.builder.build_load(ty, ptr, "reg_name");
        if self.globals().registers.is_atomic(reg) {
            let i = value
                .as_instruction_value()
                .expect("build_load returns an InstructionValue");
            i.set_atomic_ordering(AtomicOrdering::Unordered)
                .expect("load instructions can be atomic");
            // If the alignment is not set LLVM will segfault. Underestimating the alignment produces slower code, but is safe.
            i.set_alignment(0).expect("alignment is valid");
        }
        value
    }

    pub(crate) fn write_register_pointer(
        &self,
        value: BasicValueEnum<'ctx>,
        ptr: PointerValue<'ctx>,
        reg: RegisterID<'ctx, T>,
    ) {
        let v = self.builder.build_store(ptr, value);
        if self.globals().registers.is_atomic(reg) {
            v.set_atomic_ordering(AtomicOrdering::Unordered)
                .expect("store instructions can be atomic");
            // If the alignment is not set LLVM will segfault. Underestimating the alignment produces slower code, but is safe.
            v.set_alignment(0).expect("alignment is valid");
        }
    }

    pub(crate) fn read_register_raw(
        &self,
        ty: IntType<'ctx>,
        reg: impl Into<RegisterID<'ctx, T>>,
    ) -> IntValue<'ctx> {
        let reg = reg.into();
        let registers = &self.globals().registers;
        let ptr = registers.pointer_value(self, &reg);
        self.read_register_pointer(ptr, ty, reg).into_int_value()
    }

    pub(crate) fn write_register_raw(&self, reg: RegisterID<'ctx, T>, value: IntValue<'ctx>) {
        let registers = &self.globals().registers;
        let ptr = registers.pointer_value(self, &reg);
        self.write_register_pointer(value.as_basic_value_enum(), ptr, reg);
    }

    pub fn read_program_counter(&self) -> IntValue<'ctx> {
        let pc_id = <<T::Registers as target::RegisterStorage>::Globals<'ctx> as target::Globals>::PROGRAM_COUNTER_ID;
        self.read_register_raw(self.context.i64_type(), pc_id)
    }

    pub fn write_program_counter(&self, value: u64) {
        let pc_id = <<T::Registers as target::RegisterStorage>::Globals<'ctx> as target::Globals>::PROGRAM_COUNTER_ID;
        let value = self.context.i64_type().const_int(value, false);
        self.write_register_raw(pc_id, value);
    }
}

impl<T: Target> Drop for CodeGen<'_, T> {
    fn drop(&mut self) {
        // Without manually removing the modules from the execution engine LLVM segfaults.
        // We use drain here to run their Drop implementation prior to anything else, which prevents out of bounds reads/writes according to valgrind.
        for module in self.modules.drain(..) {
            self.execution_engine.remove_module(&module).unwrap();
        }
        self.execution_engine.remove_module(&self.module).unwrap();
    }
}

#[derive(Debug)]
pub enum FallthroughAmount {
    One,
    Two,
}

pub(crate) trait BitWidth: std::fmt::Display + Copy {
    fn bit_width(&self) -> usize;
}

impl BitWidth for IntType<'_> {
    fn bit_width(&self) -> usize {
        self.get_bit_width() as usize
    }
}

impl BitWidth for usize {
    fn bit_width(&self) -> usize {
        *self
    }
}

pub(crate) trait NumericValue<'ctx>:
    BasicValue<'ctx> + TryFrom<BasicValueEnum<'ctx>> + Copy
{
    type Tag: BasicType<'ctx>;

    fn tag(context: &'ctx Context, bit_width: &impl BitWidth) -> Self::Tag;
}

impl<'ctx> NumericValue<'ctx> for IntValue<'ctx> {
    type Tag = IntType<'ctx>;

    fn tag(context: &'ctx Context, bit_width: &impl BitWidth) -> Self::Tag {
        match bit_width.bit_width() {
            1 => context.bool_type(),
            8 => context.i8_type(),
            16 => context.i16_type(),
            32 => context.i32_type(),
            64 => context.i64_type(),
            128 => context.i128_type(),
            _ => unimplemented!("<IntValue as NumericValue>::tag(): bit_width={bit_width}"),
        }
    }
}

impl<'ctx> NumericValue<'ctx> for FloatValue<'ctx> {
    type Tag = FloatType<'ctx>;

    fn tag(context: &'ctx Context, bit_width: &impl BitWidth) -> Self::Tag {
        match bit_width.bit_width() {
            16 => context.f16_type(),
            32 => context.f32_type(),
            64 => context.f64_type(),
            128 => context.f128_type(),
            _ => unimplemented!("<FloatValue as NumericValue>::tag(): bit_width={bit_width}"),
        }
    }
}
