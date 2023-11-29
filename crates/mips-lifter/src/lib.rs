use inkwell::{context::Context, execution_engine::JitFunction, OptimizationLevel};

pub use self::builder::JitBuilder;
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

// TODO: move most of this Codegen. `JitBuilder::build()` should create the runtime::Environment,
// which then initialises the entry point and helper functions. `Environment::run()` can start execution.
pub(crate) fn run<Bus>(builder: JitBuilder<true, Bus>) -> !
where
    Bus: runtime::bus::Bus,
{
    // Without calling this, the JIT will get optimised out when building with LTO.
    inkwell::execution_engine::ExecutionEngine::link_in_mc_jit();

    // Create the compiler context.
    let context = Context::create();
    let module = context.create_module("codegen");
    let execution_engine = module
        .create_jit_execution_engine(OptimizationLevel::None)
        .unwrap();

    // Create the main function.
    let fn_type = context.void_type().fn_type(&[], false);
    let i64_type = context.i64_type();
    let main = module.add_function("main", fn_type, None);
    let entry_block = context.append_basic_block(main, "entry");

    // Create the compilation/runtime environment.
    let env = runtime::Environment::new(builder, &context, module, execution_engine);

    // Build the main function.
    {
        env.codegen.builder.position_at_end(entry_block);
        let pc = env.codegen.read_register(i64_type, register::Special::Pc);
        env.codegen.build_dynamic_jump(pc);
    }

    // Ensure the generated LLVM IR is valid.
    if let Err(err) = env.codegen.verify() {
        println!("\nERROR: Generated code failed to verify:\n\n{err}\nTerminating.");
        panic!()
    }

    let main_fn: JitFunction<unsafe extern "C" fn()> =
        unsafe { env.codegen.execution_engine.get_function("main").unwrap() };

    // Run the generated code!
    unsafe { main_fn.call() };
    unreachable!()
}
