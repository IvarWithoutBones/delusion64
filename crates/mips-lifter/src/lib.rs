use inkwell::{context::Context, OptimizationLevel};
use std::pin::Pin;

pub use self::{
    builder::JitBuilder,
    runtime::registers::{RegIndex, RegisterBank},
};
pub use mips_decomp::register;

pub mod gdb {
    pub use crate::runtime::gdb::{
        command::{MonitorCommand, MonitorCommandHandler, MonitorCommandHandlerError},
        Connection,
    };

    pub mod util {
        //! Utilities for parsing GDB commands.
        pub use crate::runtime::gdb::command::str_to_u64;
    }
}

#[macro_use]
mod codegen;
mod builder;
mod label;
mod recompiler;
pub mod runtime;
pub mod target;

/// The "tailcc" calling convention, as defined by LLVM. It is described as follows:
///
/// "Attempts to make calls as fast as possible while guaranteeing that tail call optimization can always be performed."
///
/// Tail calls are important to us because the functions we generate are direct mappings to blocks of guest machine code,
/// which can arbitrarily jump to other blocks. If we saved the return address on the stack when entering a block, it would overflow very quickly.
///
/// See the LLVM [language reference](https://releases.llvm.org/16.0.0/docs/LangRef.html#calling-conventions),
/// and the `llvm::CallingConv` [namespace documentation](https://llvm.org/doxygen/namespacellvm_1_1CallingConv.html#a25d1d3b199fe3c8c6442ba8947b42be1ad6e9c0ff694f0fca0222e79e772b647e) for more information.
// TODO: Upstream this enum to inkwell.
const LLVM_CALLING_CONVENTION_TAILCC: u32 = 18;

pub(crate) fn run<T, B>(builder: JitBuilder<true, T, B>) -> B
where
    B: runtime::bus::Bus,
    T: target::Target,
    for<'a> runtime::Environment<'a, T, B>: runtime::ValidRuntime,
{
    // Without calling this, the JIT will get optimised out when building with LTO.
    inkwell::execution_engine::ExecutionEngine::link_in_mc_jit();

    // Create the compiler context.
    let context = Context::create();
    let module = context.create_module("codegen");
    let execution_engine = module
        .create_jit_execution_engine(OptimizationLevel::None)
        .unwrap();

    // Create the compilation/runtime environment.
    let env = runtime::Environment::new(builder, &context, module, execution_engine);

    // Start JITing guest code by compiling the given PC register. This function will only return once Bus requests an exit.
    unsafe { env.codegen.main().call() };

    // SAFETY: The runtime environment is pinned because it is self-referential:
    // It owns both the guest registers and the JIT functions which utilise them through raw pointers,
    // if we ever move it our pointers will be invalidated.
    //
    // We can safely unpin it here because both are about to be dropped, and we will never use said pointers again.
    let env = unsafe { Pin::into_inner_unchecked(env) };
    env.bus
}
