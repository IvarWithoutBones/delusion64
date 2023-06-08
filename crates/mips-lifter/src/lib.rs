use crate::codegen::CodeGen;
use inkwell::{context::Context, execution_engine::JitFunction, OptimizationLevel};
use mips_decomp::register;
use std::net::TcpStream;

#[macro_use]
mod codegen;
mod label;
mod recompiler;
pub mod runtime;

/// The "fast" calling convention, as defined by LLVM at https://llvm.org/doxygen/namespacellvm_1_1CallingConv.html.
/// "Attempts to make calls as fast as possible (e.g. by passing things in registers)."
// TODO: upstream this to inkwell
const LLVM_CALLING_CONVENTION_FAST: u32 = 8;

// TODO: dont do this all at once, provide a builder API instead.
// TODO: pass a range we can read from `mem` instead of a slice to be more consistent with the runtime.
pub fn run<Mem>(bin: &[u8], mem: Mem, gdb_stream: Option<TcpStream>, ir_path: Option<&str>)
where
    Mem: runtime::Memory,
{
    let labels = mips_decomp::LabelList::from(bin);
    println!("{:#?}", labels);
    // return;

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

    // Create our own compilation/runtime environment.
    let labels_with_context = label::generate_label_functions(labels, &context, &module);
    let (env, globals) = {
        let env = runtime::Environment::new(mem, labels_with_context.to_owned(), gdb_stream);
        let globals = env.map_into(&module, &execution_engine);
        (env, globals)
    };
    let codegen = CodeGen::new(
        &context,
        module,
        execution_engine,
        globals,
        labels_with_context,
    );

    // Build the main function.
    codegen.builder.position_at_end(entry_block);

    // Set the initial register state.
    // TODO: move this to a downstream crate
    codegen.write_general_reg(
        register::GeneralPurpose::Sp,
        i64_type.const_int(0x500, false).into(),
        // i64_type.const_int(0xFFFFFFFFA4001FF0, false).into(),
    );

    codegen.write_general_reg(
        register::GeneralPurpose::S6,
        i64_type.const_int(0x000000000000003F, false).into(),
    );

    codegen.write_general_reg(
        register::GeneralPurpose::S4,
        i64_type.const_int(0x0000000000000001, false).into(),
    );

    codegen.write_general_reg(
        register::GeneralPurpose::T3,
        i64_type.const_int(0xFFFFFFFFA4000040, false).into(),
    );

    codegen.call_label(codegen.get_label(0));

    // Compile the generated functions.
    for (i, label) in codegen.labels.iter().enumerate() {
        println!("compiling label {}/{}", i + 1, codegen.labels.len());
        label.compile(&codegen);
    }

    println!("finished generating instructions");

    // Write the generated LLVM IR to a file, if a path was provided.
    if let Some(path) = ir_path {
        codegen.module.print_to_file(path).unwrap();
        println!("Wrote LLVM IR to '{path}'\n");
    }

    // Ensure the generated LLVM IR is valid.
    if let Err(err) = codegen.verify() {
        println!("\nERROR: Generated code failed to verify:\n\n{err}\nTerminating.");
        panic!()
    }

    // Run the generated code!
    let main_fn: JitFunction<unsafe extern "C" fn()> =
        unsafe { codegen.execution_engine.get_function("main").unwrap() };
    unsafe { main_fn.call() };

    env.print_registers();
}
