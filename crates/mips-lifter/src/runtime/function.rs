//! Functions that are called by the generated code, calling into the `Environment` struct.

use inkwell::{
    context::ContextRef, execution_engine::ExecutionEngine, module::Module, types::FunctionType,
    values::FunctionValue,
};
use strum::{EnumIter, VariantNames};

#[derive(VariantNames, EnumIter, Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[strum(serialize_all = "snake_case")]
pub enum RuntimeFunction {
    Panic,
    OnInstruction,
    GetFunctionPtr,
    OnBlockEntered,

    GetPhysicalAddress,
    HandleException,
    ProbeTlbEntry,
    ReadTlbEntry,
    WriteTlbEntry,

    ReadI8,
    ReadI16,
    ReadI32,
    ReadI64,
    WriteI8,
    WriteI16,
    WriteI32,
    WriteI64,
    ReadPhysicalI8,
    ReadPhysicalI16,
    ReadPhysicalI32,
    ReadPhysicalI64,
    WritePhysicalI8,
    WritePhysicalI16,
    WritePhysicalI32,
    WritePhysicalI64,
}

impl RuntimeFunction {
    pub const fn name(&self) -> &'static str {
        Self::VARIANTS[*self as usize]
    }

    #[inline]
    fn signature<'ctx>(
        &self,
        context: &ContextRef<'ctx>,
        execution_engine: &ExecutionEngine<'ctx>,
    ) -> FunctionType<'ctx> {
        let bool_type = context.bool_type();
        let i8_type = context.i8_type();
        let i16_type = context.i16_type();
        let i32_type = context.i32_type();
        let i64_type = context.i64_type();
        let void_type = context.void_type();
        let ptr_type = i64_type.ptr_type(Default::default());
        let ptr_sized_int = context.ptr_sized_int_type(execution_engine.get_target_data(), None);

        // Dirty macro to make the signature generation a bit more readable.
        // The name doesn't do anything, its just there to enforce nicer syntax.
        macro_rules! sig {
            ($ret_ty:expr, [$(
               $name:ident : $arg_ty:expr
            ),* $(,)?]) => {
                $ret_ty.fn_type(&[
                    ptr_type.into(), // Environment pointer (&mut self)
                    $($arg_ty.into()),*
                ], false)
            };
        }

        // NOTE: Must match the signature in `runtime/{mod.rs,memory/mod.rs}`!
        match self {
            // `Environment::panic(&mut self)`
            Self::Panic => sig!(void_type, [string_ptr: ptr_type, len: i64_type]),
            // `Environment::get_function_ptr(&mut self, vaddr: u64) -> usize`
            Self::GetFunctionPtr => sig!(i64_type, [vaddr: ptr_sized_int]),
            // `Environment::on_block_entered(&mut self, instructions_in_block: u64) -> usize`
            Self::OnBlockEntered => sig!(ptr_sized_int, [instructions_in_block: i64_type]),
            // `Environment::on_instruction(&mut self)`
            Self::OnInstruction => sig!(void_type, []),

            // `Environment::handle_exception_jit(
            //     &mut self,
            //     code: u64,
            //     has_coprocessor: bool,
            //     coprocessor: u8,
            //     has_bad_vaddr: bool,
            //     bad_vaddr: u64,
            // ) -> usize`
            Self::HandleException => {
                sig!(ptr_sized_int, [exception_code: i64_type, has_coprocessor: bool_type, coprocessor: i8_type, has_bad_vaddr: bool_type, bad_vaddr: i64_type])
            }
            // `Environment::get_physical_address(&mut self, vaddr: u64) -> u32`
            Self::GetPhysicalAddress => sig!(i32_type, [vaddr: i64_type]),
            // `Environment::probe_tlb_entry(&mut self, index: u64)`
            Self::ProbeTlbEntry => sig!(void_type, []),
            // `Environment::write_tlb_entry(&mut self, index: u64)`
            Self::WriteTlbEntry => sig!(void_type, [index: i64_type]),
            // `Environment::read_tlb_entry(&mut self, index: u64)`
            Self::ReadTlbEntry => sig!(void_type, [index: i64_type]),

            // `Environment::read_u8(&mut self, vaddr: u64) -> u8`
            Self::ReadI8 => sig!(i8_type, [vaddr: i64_type]),
            // `Environment::read_u16(&mut self, vaddr: u64) -> u16`
            Self::ReadI16 => sig!(i16_type, [vaddr: i64_type]),
            // `Environment::read_u32(&mut self, vaddr: u64) -> u32`
            Self::ReadI32 => sig!(i32_type, [vaddr: i64_type]),
            // `Environment::read_u64(&mut self, vaddr: u64) -> u64`
            Self::ReadI64 => sig!(i64_type, [vaddr: i64_type]),
            // `Environment::write_u8(&mut self, vaddr: u64, value: u8)`
            Self::WriteI8 => sig!(void_type, [vaddr: i64_type, value: i8_type]),
            // `Environment::write_u16(&mut self, vaddr: u64, value: u16)`
            Self::WriteI16 => sig!(void_type, [vaddr: i64_type, value: i16_type]),
            // `Environment::write_u32(&mut self, vaddr: u64, value: u32)`
            Self::WriteI32 => sig!(void_type, [vaddr: i64_type, value: i32_type]),
            // `Environment::write_u64(&mut self, vaddr: u64, value: u64)`
            Self::WriteI64 => sig!(void_type, [vaddr: i64_type, value: i64_type]),

            // `Environment::read_physical_u8(&mut self, paddr: u32) -> u8`
            Self::ReadPhysicalI8 => sig!(i8_type, [paddr: i32_type]),
            // `Environment::read_physical_u16(&mut self, paddr: u32) -> u16`
            Self::ReadPhysicalI16 => sig!(i16_type, [paddr: i32_type]),
            // `Environment::read_physical_u32(&mut self, paddr: u32) -> u32`
            Self::ReadPhysicalI32 => sig!(i32_type, [paddr: i32_type]),
            // `Environment::read_physical_u64(&mut self, paddr: u32) -> u64`
            Self::ReadPhysicalI64 => sig!(i64_type, [paddr: i32_type]),
            // `Environment::write_physical_u8(&mut self, paddr: u32, value: u8)`
            Self::WritePhysicalI8 => sig!(void_type, [paddr: i32_type, value: i8_type]),
            // `Environment::write_physical_u16(&mut self, paddr: u32, value: u16)`
            Self::WritePhysicalI16 => sig!(void_type, [paddr: i32_type, value: i16_type]),
            // `Environment::write_physical_u32(&mut self, paddr: u32, value: u32)`
            Self::WritePhysicalI32 => sig!(void_type, [paddr: i32_type, value: i32_type]),
            // `Environment::write_physical_u64(&mut self, paddr: u32, value: u64)`
            Self::WritePhysicalI64 => sig!(void_type, [paddr: i32_type, value: i64_type]),
        }
    }

    /// Maps the function into the given `ExecutionEngine`, at the given pointer.
    pub fn map_into<'ctx>(
        &self,
        context: &ContextRef<'ctx>,
        module: &Module<'ctx>,
        execution_engine: &ExecutionEngine<'ctx>,
        ptr: *const u8,
    ) -> FunctionValue<'ctx> {
        let sig = self.signature(context, execution_engine);
        let func = module.add_function(self.name(), sig, None);
        execution_engine.add_global_mapping(&func, ptr as usize);
        func
    }
}
