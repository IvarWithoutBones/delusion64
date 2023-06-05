//! Functions that are called by the generated code, calling into the `Environment` struct.

use inkwell::{
    context::ContextRef, execution_engine::ExecutionEngine, module::Module, types::FunctionType,
    AddressSpace,
};
use strum::{EnumIter, EnumVariantNames, VariantNames};

#[derive(EnumVariantNames, EnumIter, Debug, Clone, Copy, PartialEq, Eq)]
#[strum(serialize_all = "snake_case")]
pub enum RuntimeFunction {
    GetBlockId,
    PrintString,
    OnInstruction,

    ReadI8,
    ReadI16,
    ReadI32,
    ReadI64,

    WriteI8,
    WriteI16,
    WriteI32,
    WriteI64,
}

impl RuntimeFunction {
    pub const fn name(&self) -> &'static str {
        Self::VARIANTS[*self as usize]
    }

    #[inline]
    fn signature<'ctx>(&self, context: &ContextRef<'ctx>) -> FunctionType<'ctx> {
        let i8_type = context.i8_type();
        let i16_type = context.i16_type();
        let i32_type = context.i32_type();
        let i64_type = context.i64_type();
        let void_type = context.void_type();
        let ptr_type = i64_type.ptr_type(AddressSpace::default());

        // Dirty macro to make the signature generation a bit more readable.
        // The name doesnt do anything, its just there to enforce nicer syntax.
        macro_rules! sig {
            ($ret_ty:expr, [$(
               $name:ident: $arg_ty:expr
            ),* $(,)?]) => {
                $ret_ty.fn_type(&[
                    ptr_type.into(), // Environment pointer (&mut self)
                    $($arg_ty.into()),*
                ], false)
            };
        }

        // NOTE: Must match the specified functions signature in `runtime/mod.rs`!
        match self {
            // `Environment::block_id()`
            Self::GetBlockId => sig!(i64_type, [addr: i64_type]),
            // `Environment::print_string()`
            Self::PrintString => sig!(void_type, [string_ptr: ptr_type, len: i64_type]),
            // `Environment::on_instruction()`
            Self::OnInstruction => sig!(void_type, []),

            // `Environment::read_u8()`
            Self::ReadI8 => sig!(i8_type, [addr: i64_type]),
            // `Environment::read_u16()`
            Self::ReadI16 => sig!(i16_type, [addr: i64_type]),
            // `Environment::read_u32()`
            Self::ReadI32 => sig!(i32_type, [addr: i64_type]),
            // `Environment::read_u64()`
            Self::ReadI64 => sig!(i64_type, [addr: i64_type]),
            // `Environment::write_u8()`
            Self::WriteI8 => sig!(void_type, [addr: i64_type, value: i8_type]),
            // `Environment::write_u16()`
            Self::WriteI16 => sig!(void_type, [addr: i64_type, value: i16_type]),
            // `Environment::write_u32()`
            Self::WriteI32 => sig!(void_type, [addr: i64_type, value: i32_type]),
            // `Environment::write_u64()`
            Self::WriteI64 => sig!(void_type, [addr: i64_type, value: i64_type]),
        }
    }

    /// Maps the function into the given `ExecutionEngine`, at the given pointer.
    #[inline]
    pub fn map_into<'ctx>(
        &self,
        context: &ContextRef<'ctx>,
        module: &Module<'ctx>,
        execution_engine: &ExecutionEngine<'ctx>,
        ptr: *const u8,
    ) {
        let func = module.add_function(self.name(), self.signature(context), None);
        execution_engine.add_global_mapping(&func, ptr as _);
    }
}
