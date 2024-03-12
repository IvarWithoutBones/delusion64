use self::init::Helpers;
use crate::{
    address_map::VirtualAddressMap,
    label::{JitFunctionPointer, LabelWithContext},
    macros::env_call,
    runtime::RuntimeFunction,
    target::{self, Globals as _, RegisterID, RegisterStorage, Target},
    LLVM_CALLING_CONVENTION_TAILCC,
};
use inkwell::{
    attributes::{Attribute, AttributeLoc},
    basic_block::BasicBlock,
    builder::{Builder, BuilderError},
    context::Context,
    execution_engine::{ExecutionEngine, FunctionLookupError},
    intrinsics::Intrinsic,
    module::Module,
    types::{AnyType, BasicType, BasicTypeEnum, IntType},
    values::{
        BasicMetadataValueEnum, BasicValue, BasicValueEnum, CallSiteValue, FunctionValue,
        GlobalValue, InstructionValue, IntValue, PointerValue,
    },
    AtomicOrdering,
};
use mips_decomp::{instruction::ParsedInstruction, register, Exception};
use std::{collections::HashMap, mem::ManuallyDrop, str::Utf8Error};
use tartan_bitfield::bitfield;

mod arithmetic;
mod control_flow;
mod init;

pub(crate) use arithmetic::{BitWidth, NumericValue};

#[derive(Debug, thiserror::Error)]
pub enum CompilationError {
    #[error("unimplemented instruction: {0}")]
    UnimplementedInstruction(String),
    #[error("Error while building recompiled block: {0}")]
    BuilderError(#[from] BuilderError),
    #[error("Could not find an intrinsic named '{0}'")]
    IntrinsicNotFound(&'static str),
    #[error("Failed to generate label functions for vaddr {vaddr:#x}")]
    LabelFunctionGeneration { vaddr: u64 },
    #[error("Callsite does not have a return value")]
    NoReturnValue,
    #[error("Failed to verify generated code: {0}")]
    VerificationFailed(String),
    #[error("Failed to add module to execution engine")]
    AddModuleFailed,
    #[error("LLVM C-string contains invalid UTF-8: {0}")]
    InvalidUtf8(#[from] Utf8Error),
    #[error("Failed to look up function: '{0}'")]
    FunctionLookup(#[from] FunctionLookupError),
}

pub type CompilationResult<T> = Result<T, CompilationError>;

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
    pub builder: Builder<'ctx>,
    pub execution_engine: ExecutionEngine<'ctx>,
    pub labels: VirtualAddressMap<'ctx, T>,
    globals: Option<Globals<'ctx, T>>,
    helpers: Helpers<'ctx>,

    /// The module containing the main function. Everything branches from here.
    main_module: ManuallyDrop<Module<'ctx>>,
    /// The module we are currently recompiling into. This will get moved into the label once we're done with it.
    active_module: Option<Module<'ctx>>,
}

impl<'ctx, T: Target> CodeGen<'ctx, T> {
    pub(crate) fn new(
        context: &'ctx Context,
        module: Module<'ctx>,
        execution_engine: ExecutionEngine<'ctx>,
    ) -> Self {
        Self {
            context,
            active_module: None,
            main_module: ManuallyDrop::new(module),
            helpers: Helpers::new(),
            builder: context.create_builder(),
            execution_engine,
            globals: None,
            labels: VirtualAddressMap::new(),
        }
    }

    /// Verify our module is valid.
    pub fn verify(&self) -> CompilationResult<()> {
        self.module().verify().map_err(|e| {
            self.module().print_to_file("./error.ll").unwrap();
            CompilationError::VerificationFailed(e.to_string())
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

    /// The currently active module.
    pub fn module(&self) -> &Module<'ctx> {
        self.active_module.as_ref().unwrap_or(&self.main_module)
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

    /// Registers a function in the currently active module, returning the definition for the current module.
    fn register_function(
        &self,
        module: &Module<'ctx>,
        func: FunctionValue<'ctx>,
    ) -> CompilationResult<(FunctionValue<'ctx>, JitFunctionPointer)> {
        let name = func.get_name().to_str()?;
        let func_def = module.add_function(name, func.get_type(), None);
        let ptr = self.execution_engine.get_function_address(name)?;
        self.execution_engine.add_global_mapping(&func_def, ptr);
        Ok((func_def, ptr))
    }

    pub fn call_function(&self, func: FunctionValue<'ctx>) -> CompilationResult<()> {
        let (func, _ptr) = self.register_function(self.module(), func)?;
        self.set_call_attrs(self.builder.build_call(func, &[], "call_fn")?);
        self.builder.build_return(None)?;
        Ok(())
    }

    pub fn link_in_module(&self, lab: &mut LabelWithContext<T>) -> CompilationResult<()> {
        // Map all functions from the new module into our execution engine, and add them to our main module.
        let module = self.module();
        self.execution_engine
            .add_module(module)
            .map_err(|_| CompilationError::AddModuleFailed)?;

        for (i, func) in module
            .get_functions()
            // Only a declaration, not compiled in this module.
            .filter(|func| func.count_basic_blocks() > 0)
            .enumerate()
        {
            // Map the function pointer to the declaration.
            let (_func, ptr) = self.register_function(module, func)?;
            if i == 0 {
                // We cache a pointer to the function in the label, but only the first.
                lab.pointer = Some(ptr as JitFunctionPointer);
            } else {
                panic!("multiple function bodies found in label");
            };
        }

        Ok(())
    }

    pub fn add_dynamic_function(
        &mut self,
        module: Module<'ctx>,
        globals: Globals<'ctx, T>,
        f: impl FnOnce(&CodeGen<'ctx, T>, &Module<'ctx>) -> CompilationResult<LabelWithContext<'ctx, T>>,
    ) -> CompilationResult<LabelWithContext<'ctx, T>> {
        self.globals = Some(globals);
        self.helpers.map_into(&module);
        self.active_module = Some(module);

        let mut lab = f(self, self.module())?;
        self.link_in_module(&mut lab)?;
        lab.module = Some(self.active_module.take().unwrap());

        if cfg!(debug_assertions) {
            self.verify()?;
        }
        Ok(lab)
    }

    fn call_intrinsic(
        &self,
        intrinsic_name: &'static str,
        param_types: &[BasicTypeEnum],
        params: &[BasicMetadataValueEnum<'ctx>],
        call_name: &str,
    ) -> CompilationResult<BasicValueEnum<'ctx>> {
        let func = Intrinsic::find(intrinsic_name)
            .ok_or(CompilationError::IntrinsicNotFound(intrinsic_name))?
            .get_declaration(self.module(), param_types)
            .ok_or(CompilationError::IntrinsicNotFound(intrinsic_name))?;
        self.builder
            .build_call(func, params, call_name)?
            .try_as_basic_value()
            .left()
            .ok_or(CompilationError::NoReturnValue)
    }

    pub fn build_panic(&self, string: &str, storage_name: &str) -> CompilationResult<()> {
        let ptr = self
            .builder
            .build_global_string_ptr(string, storage_name)
            .expect("global string ptr is valid")
            .as_pointer_value();
        let len = self.context.i64_type().const_int(string.len() as _, false);
        env_call!(self, RuntimeFunction::Panic, [ptr, len])?;
        self.builder.build_unreachable()?;
        Ok(())
    }

    pub fn build_stub(&self, name: &str) -> CompilationResult<()> {
        let output = format!("{name} instruction not implemented");
        let storage_name = format!("{name}_stub");
        self.build_panic(&output, &storage_name)?;
        Ok(())
    }

    pub fn read_memory(
        &self,
        ty: IntType<'ctx>,
        address: IntValue<'ctx>,
    ) -> CompilationResult<IntValue<'ctx>> {
        let func = match ty.get_bit_width() {
            8 => RuntimeFunction::ReadI8,
            16 => RuntimeFunction::ReadI16,
            32 => RuntimeFunction::ReadI32,
            64 => RuntimeFunction::ReadI64,
            _ => unimplemented!("read_memory type: {ty}"),
        };

        // The callback API expects a 64-bit address parameter.
        let addr = match address.get_type().get_bit_width() {
            32 => self.zero_extend_to(self.context.i64_type(), address)?,
            64 => address,
            width => unimplemented!("read_memory address bit width: {width}"),
        };

        Ok(env_call!(self, func, [addr])?
            .try_as_basic_value()
            .left()
            .unwrap()
            .into_int_value())
    }

    pub fn write_memory(
        &self,
        address: IntValue<'ctx>,
        value: IntValue<'ctx>,
    ) -> CompilationResult<()> {
        let ty = value.get_type();
        let func = match ty.get_bit_width() {
            8 => RuntimeFunction::WriteI8,
            16 => RuntimeFunction::WriteI16,
            32 => RuntimeFunction::WriteI32,
            64 => RuntimeFunction::WriteI64,
            _ => unimplemented!("write_memory type: {ty}"),
        };

        // The callback API expects a 64-bit address parameter.
        let addr = match address.get_type().get_bit_width() {
            32 => self.zero_extend_to(self.context.i64_type(), address)?,
            64 => address,
            width => unimplemented!("write_memory address bit width: {width}"),
        };

        env_call!(self, func, [addr, value])?;
        Ok(())
    }

    pub fn read_physical_memory(
        &self,
        ty: IntType<'ctx>,
        address: IntValue<'ctx>,
    ) -> CompilationResult<IntValue<'ctx>> {
        let func = match ty.get_bit_width() {
            8 => RuntimeFunction::ReadPhysicalI8,
            16 => RuntimeFunction::ReadPhysicalI16,
            32 => RuntimeFunction::ReadPhysicalI32,
            64 => RuntimeFunction::ReadPhysicalI64,
            _ => panic!("unimplemented read_physical_memory type: {ty}"),
        };

        Ok(env_call!(self, func, [address])?
            .try_as_basic_value()
            .left()
            .unwrap()
            .into_int_value())
    }

    pub fn write_physical_memory(
        &self,
        address: IntValue<'ctx>,
        value: IntValue<'ctx>,
    ) -> CompilationResult<()> {
        let ty = value.get_type();
        let func = match ty.get_bit_width() {
            8 => RuntimeFunction::WritePhysicalI8,
            16 => RuntimeFunction::WritePhysicalI16,
            32 => RuntimeFunction::WritePhysicalI32,
            64 => RuntimeFunction::WritePhysicalI64,
            _ => panic!("unimplemented write_physical_memory type: {ty}"),
        };

        env_call!(self, func, [address, value])?;
        Ok(())
    }

    pub fn get_physical_address(&self, vaddr: IntValue<'ctx>) -> CompilationResult<IntValue<'ctx>> {
        Ok(
            env_call!(self, RuntimeFunction::GetPhysicalAddress, [vaddr])?
                .try_as_basic_value()
                .left()
                .expect("GetPhysicalAddress has a return value")
                .into_int_value(),
        )
    }

    pub fn set_inside_delay_slot(&self, inside: bool) -> CompilationResult<()> {
        let i64_type = self.context.i64_type();
        let flags_ptr = self.globals().flags_ptr.as_pointer_value();
        let prev = {
            let flags = self
                .builder
                .build_load(i64_type, flags_ptr, "flags_prev")?
                .into_int_value();
            // Zero the least significant bit as to overwrite it with the new value.
            self.builder.build_and(
                flags,
                i64_type.const_int(!1, false),
                "flags_prev_delay_zeroed",
            )?
        };
        let new = {
            // Set the least significant bit to the new value.
            let inside = i64_type.const_int(inside as u64, false);
            self.builder.build_or(prev, inside, "flags_new_delay")?
        };
        self.builder.build_store(flags_ptr, new)?;
        Ok(())
    }

    pub fn throw_exception(
        &self,
        exception: Exception,
        coprocessor: Option<u8>,
        bad_vaddr: Option<IntValue<'ctx>>,
    ) -> CompilationResult<()> {
        let i64_type = self.context.i64_type();
        let i8_type = self.context.i8_type();
        let exception = i64_type.const_int(exception as u64, false);

        let has_coprocessor = self
            .context
            .bool_type()
            .const_int(u64::from(coprocessor.is_some()), false);
        let coprocessor = coprocessor.map_or_else(
            || i8_type.const_zero(),
            |cop| {
                assert!(cop <= 3, "invalid coprocessor in throw_exception: {cop}");
                i8_type.const_int(u64::from(cop), false)
            },
        );

        let has_bad_vaddr = self
            .context
            .bool_type()
            .const_int(u64::from(bad_vaddr.is_some()), false);
        let bad_vaddr = bad_vaddr.unwrap_or_else(|| i64_type.const_zero());

        let exception_vec_ptr = env_call!(
            self,
            RuntimeFunction::HandleException,
            [
                exception,
                has_coprocessor,
                coprocessor,
                has_bad_vaddr,
                bad_vaddr
            ]
        )?
        .try_as_basic_value()
        .left()
        .unwrap()
        .into_int_value();
        self.build_jump_to_host_ptr(exception_vec_ptr, "exception_vector")?;
        Ok(())
    }

    /*
       Register access helpers, to assist implementing target-specific code.
    */

    pub fn base_plus_offset(
        &self,
        ty: IntType<'ctx>,
        instr: &ParsedInstruction,
        add_name: &str,
    ) -> CompilationResult<IntValue<'ctx>>
    where
        <T::Registers as RegisterStorage>::Id: From<register::GeneralPurpose>,
    {
        let i16_type = self.context.i16_type();
        let base = self.read_general_register(ty, instr.base())?;
        let offset =
            self.sign_extend_to(ty, i16_type.const_int(u64::from(instr.offset()), true))?;
        Ok(self.builder.build_int_add(base, offset, add_name)?)
    }

    /// If the alignment of atomic load/store instructions is not set LLVM will segfault.
    /// This function sets it to the ABI required alignment.
    fn set_abi_alignment(&self, instr: InstructionValue<'ctx>, ty: &dyn AnyType<'ctx>) {
        let align = self
            .execution_engine
            .get_target_data()
            .get_abi_alignment(ty);
        instr.set_alignment(align).expect("alignment is valid");
    }

    pub(crate) fn read_register_pointer(
        &self,
        ptr: PointerValue<'ctx>,
        ty: impl BasicType<'ctx> + Clone,
        reg: <T::Registers as RegisterStorage>::Id,
    ) -> CompilationResult<BasicValueEnum<'ctx>> {
        let name = &format!("{}_", reg.name());
        let value = self.builder.build_load(ty.clone(), ptr, name)?;
        if self.globals().registers.is_atomic(reg) {
            let i = value
                .as_instruction_value()
                .expect("build_load returns an InstructionValue");
            i.set_atomic_ordering(AtomicOrdering::Monotonic)
                .expect("load instructions can be atomic");
            self.set_abi_alignment(i, &ty);
        }
        Ok(value)
    }

    pub(crate) fn write_register_pointer(
        &self,
        value: BasicValueEnum<'ctx>,
        ptr: PointerValue<'ctx>,
        reg: <T::Registers as RegisterStorage>::Id,
    ) -> CompilationResult<()> {
        let i = self.builder.build_store(ptr, value)?;
        if self.globals().registers.is_atomic(reg) {
            i.set_atomic_ordering(AtomicOrdering::Monotonic)
                .expect("store instructions can be atomic");
            self.set_abi_alignment(i, &value.get_type());
        }
        Ok(())
    }

    pub(crate) fn read_register_raw(
        &self,
        ty: impl BasicType<'ctx> + Clone,
        reg: impl Into<<T::Registers as RegisterStorage>::Id>,
    ) -> CompilationResult<BasicValueEnum<'ctx>> {
        let reg = reg.into();
        let ptr = self.globals().registers.pointer_value(self, &reg);
        self.read_register_pointer(ptr, ty, reg)
    }

    pub(crate) fn write_register_raw(
        &self,
        reg: impl Into<<T::Registers as RegisterStorage>::Id>,
        value: impl Into<BasicValueEnum<'ctx>>,
    ) -> CompilationResult<()> {
        let reg = reg.into();
        let ptr = self.globals().registers.pointer_value(self, &reg);
        self.write_register_pointer(value.into(), ptr, reg)
    }

    /// Read the general-purpose register (GPR) at the given index.
    /// If the `ty` is less than the register width the value will be truncated, and the lower bits returned.
    pub fn read_general_register(
        &self,
        ty: IntType<'ctx>,
        index: impl Into<u64>,
    ) -> CompilationResult<IntValue<'ctx>>
    where
        <T::Registers as RegisterStorage>::Id: From<register::GeneralPurpose>,
    {
        let index = u8::try_from(index.into()).expect("index must be less than 32");
        let reg = register::GeneralPurpose::from_repr(index).unwrap();
        if reg == register::GeneralPurpose::Zero {
            // Register zero is hardwired to zero.
            Ok(ty.const_zero())
        } else {
            Ok(self.read_register_raw(ty, reg)?.into_int_value())
        }
    }

    /// Writes the given value to the general-purpose register (GPR) at the given index.
    pub fn write_general_register(
        &self,
        index: impl Into<u64>,
        value: IntValue<'ctx>,
    ) -> CompilationResult<()>
    where
        <T::Registers as RegisterStorage>::Id: From<register::GeneralPurpose>,
    {
        let index = u8::try_from(index.into()).expect("index must be less than 32");
        let reg = register::cpu::GeneralPurpose::from_repr(index).unwrap();
        if reg != register::cpu::GeneralPurpose::Zero {
            // Register zero is hardwired to zero.
            self.write_register_raw(reg, value)?;
        }
        Ok(())
    }

    pub fn read_program_counter(&self) -> CompilationResult<IntValue<'ctx>> {
        let ty = self.context.i64_type();
        let reg =
            <<T::Registers as target::RegisterStorage>::Id as target::RegisterID>::PROGRAM_COUNTER;
        Ok(self.read_register_raw(ty, reg)?.into_int_value())
    }

    pub fn write_program_counter(&self, value: u64) -> CompilationResult<()> {
        let pc_id =
            <<T::Registers as target::RegisterStorage>::Id as target::RegisterID>::PROGRAM_COUNTER;
        let value = self.context.i64_type().const_int(value, false);
        self.write_register_raw(pc_id, value)
    }
}

impl<T: Target> Drop for CodeGen<'_, T> {
    fn drop(&mut self) {
        // SAFETY: We remove ever module here, so the sort order will be maintained.
        let modules = unsafe { self.labels.inner_mut() }
            .drain(..)
            .flat_map(|l| l.module)
            .chain({
                // SAFETY: We will not reference the module again, so we can take it.
                let module = unsafe { ManuallyDrop::take(&mut self.main_module) };
                std::iter::once(module)
            });

        // Without manually removing the modules from the execution engine LLVM segfaults when restarting the JIT.
        // We use `drain()` to run their `Drop` implementation prior to anything else, which prevents out of bounds reads/writes according to valgrind.
        // Note that `self.active_module` is omitted here, since it does not yet belong to the execution engine.
        for module in modules {
            if let Err(err) = self.execution_engine.remove_module(&module) {
                // Note that to avoid double-panics while unwinding we don't use `panic!` here, even though the error is fatal.
                println!("failed to remove module while dropping CodeGen: {err}");
            }
        }
    }
}

#[derive(Debug)]
pub enum FallthroughAmount {
    One,
    Two,
}
