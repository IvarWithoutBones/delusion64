use crate::codegen::CodeGen;
use inkwell::{context::Context, execution_engine::JitFunction, OptimizationLevel};

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
mod label;
mod recompiler;
pub mod runtime;

/// The "fast" calling convention, as defined by LLVM at https://llvm.org/doxygen/namespacellvm_1_1CallingConv.html.
/// "Attempts to make calls as fast as possible (e.g. by passing things in registers)."
// TODO: upstream this to inkwell
const LLVM_CALLING_CONVENTION_FAST: u32 = 8;

pub type InitialRegisters<'a> = &'a [(register::Register, u64)];

pub fn run<Bus>(mem: Bus, regs: InitialRegisters, gdb: Option<gdb::Connection<Bus>>)
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
    let (env, codegen) = {
        let env = runtime::Environment::new(mem, regs, gdb);
        let globals = env.map_into(&module, &execution_engine);
        let codegen = CodeGen::new(&context, module, execution_engine, globals);
        (env, codegen)
    };

    // Build the main function.
    {
        codegen.builder.position_at_end(entry_block);
        // Save the current stack frame, will be restored on every block to ensure no stack overflows occur.
        // Its a rather bruteforce approach, but it works :)
        codegen.save_host_stack();
        codegen.build_dynamic_jump(codegen.read_register(i64_type, register::Special::Pc));
    }

    // Ensure the generated LLVM IR is valid.
    if let Err(err) = codegen.verify() {
        println!("\nERROR: Generated code failed to verify:\n\n{err}\nTerminating.");
        panic!()
    }

    let main_fn: JitFunction<unsafe extern "C" fn()> =
        unsafe { codegen.execution_engine.get_function("main").unwrap() };

    env.attach_codegen(codegen);

    // Run the generated code!
    unsafe { main_fn.call() };
    unreachable!()
}
