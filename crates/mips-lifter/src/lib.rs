use crate::{jit::CodeGen, recompiler::recompile_instruction};
use inkwell::{context::Context, execution_engine::JitFunction, OptimizationLevel};
use mips_decomp::{format::register_name, INSTRUCTION_SIZE, REGISTER_COUNT};

pub mod codegen;
pub mod jit;
pub mod label;
pub mod recompiler;

#[macro_export]
macro_rules! c_fn {
    ($($arg:ident),*) => {
        unsafe extern "C" fn($($arg),*)
    };
}

extern "C" fn print_regs(regs_ptr: *const u64) {
    println!("\nregisters:");
    let regs = unsafe { std::slice::from_raw_parts(regs_ptr, REGISTER_COUNT) };
    for (i, r) in regs.iter().enumerate() {
        let name = register_name(i as _);
        println!("{name: <4} = {r:#x}");
    }
}

pub fn lift(bin: &[u8]) {
    let decomp = mips_decomp::Decompiler::from(bin);
    let blocks = mips_decomp::reorder_delay_slots(decomp.blocks().collect());
    decomp.pretty_print(&blocks);

    // Create an LLVM module and execution engine.
    let context = Context::create();
    let module = context.create_module("codegen");
    let execution_engine = module
        .create_jit_execution_engine(OptimizationLevel::None)
        .unwrap();

    // Create the main function.
    let fn_type = context.void_type().fn_type(&[], false);
    let main = module.add_function("main", fn_type, None);
    let entry_block = context.append_basic_block(main, "entry");
    let exit_block = context.append_basic_block(main, "exit");

    // Generate labels for all blocks, and insert them into the function.
    let mut label_pass = label::LabelPass::new(&context, module, &blocks);
    label_pass.run();
    let (context, module, labels) = label_pass.consume();

    // Set up the code generation context, and register debugging functions.
    let mut codegen = codegen::CodeGen::new(context, module, execution_engine);
    codegen.add_function::<c_fn!(u64)>(
        "print_regs",
        print_regs as *const _,
        &[codegen
            .context
            .i64_type()
            .array_type(REGISTER_COUNT as _)
            .into()],
    );

    // Generate the main functions entry block
    codegen.builder.position_at_end(entry_block);
    codegen
        .builder
        .build_unconditional_branch(labels.get(&0).unwrap().basic_block);
    // Generate the main functions exit block
    codegen.builder.position_at_end(exit_block);
    codegen.build_call(
        "print_regs",
        &[codegen.registers_global().as_pointer_value().into()],
    );
    codegen.builder.build_return(None);
    codegen.set_exit_block(exit_block);

    // Generate code for each label.
    for label in labels.values() {
        let basic_block = label.basic_block;
        codegen.builder.position_at_end(basic_block);

        for (i, instr) in label.instructions.iter().enumerate() {
            let addr = label.start_address + (i * INSTRUCTION_SIZE) as u64;
            recompile_instruction(&codegen, &labels, instr, addr);
        }

        // If the block doesn't end with a terminator, insert a branch to the next block.
        if basic_block.get_terminator().is_none() {
            if let Some(fall_through) = label.fall_through {
                codegen.builder.build_unconditional_branch(fall_through);
            } else {
                codegen.builder.build_unconditional_branch(exit_block);
            }
        }
    }

    // Print the generated LLVM IR.
    println!("\nGenerated LLVM IR:\n```");
    codegen.module.print_to_stderr();
    println!("```\n");

    // Run the generated code.
    let main_func: JitFunction<c_fn!()> = codegen.get_function("main").unwrap();
    unsafe { main_func.call() }
}

// TODO: remove this, the jit module is just here as a reference for PHI nodes
pub fn jit_test() {
    let context = Context::create();
    let module = context.create_module("codegen");
    let execution_engine = module
        .create_jit_execution_engine(OptimizationLevel::None)
        .unwrap();

    let codegen = CodeGen::new(&context, module, execution_engine);
    let func = codegen.jit_compile_add().unwrap();

    println!("Generated LLVM IR:\n\n```");
    codegen.module.print_to_stderr();
    println!("```\n");

    let result = unsafe { func.call(10) };
    println!("result: {result}");
}
