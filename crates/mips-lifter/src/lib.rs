use crate::jit::CodeGen;
use inkwell::{context::Context, OptimizationLevel};

pub mod jit;

pub fn jit_test() {
    let context = Context::create();
    let module = context.create_module("codegen");
    let execution_engine = module
        .create_jit_execution_engine(OptimizationLevel::None)
        .unwrap();

    let codegen = CodeGen::new(&context, module, execution_engine);
    let func = codegen.jit_compile_loop().unwrap();

    println!("Generated LLVM IR:\n\n```");
    codegen.module.print_to_stderr();
    println!("```\n");

    let result = unsafe { func.call(10) };
    println!("result: {result}");
}
