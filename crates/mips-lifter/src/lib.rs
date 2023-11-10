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

/// The "fast" calling convention, as defined by LLVM at https://llvm.org/doxygen/namespacellvm_1_1CallingConv.html.
/// "Attempts to make calls as fast as possible (e.g. by passing things in registers)."
// TODO: upstream this to inkwell
const LLVM_CALLING_CONVENTION_FAST: u32 = 8;

// TODO: move most of this to the JitBuilder. `build()` should initialise everything, and `run() -> !` start executing.
pub(crate) fn run<Bus>(builder: JitBuilder<true, Bus>) -> !
where
    Bus: runtime::bus::Bus,
{
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
        // Save the current stack frame, will be restored on every block to ensure no stack overflows occur.
        // Its a rather bruteforce approach, but it works :)
        env.codegen.save_host_stack();
        env.codegen
            .build_dynamic_jump(env.codegen.read_register(i64_type, register::Special::Pc));
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
