use crate::{codegen::CodeGen, recompiler::recompile_instruction};
use inkwell::{context::Context, execution_engine::JitFunction, OptimizationLevel};
use mips_decomp::{INSTRUCTION_SIZE, REGISTER_COUNT};

pub mod codegen;
pub mod env;
pub mod label;
pub mod recompiler;

macro_rules! c_fn {
    ($($arg:ident),*) => {
        unsafe extern "C" fn($($arg),*)
    };
}

const MEMORY_SIZE: usize = 0x500;

pub fn lift(bin: &[u8], ir_path: Option<&str>, entry_point: u64) {
    let decomp = mips_decomp::Decompiler::from(bin);
    let blocks = mips_decomp::reorder_delay_slots(decomp.blocks().collect());
    #[cfg(feature = "debug-print")]
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

    let env = {
        let labels = labels.values().map(|l| l.to_owned()).collect::<Vec<_>>();
        env::Environment::<REGISTER_COUNT, MEMORY_SIZE>::new(labels)
    };
    let codegen = CodeGen::new(context, module, execution_engine, &env);

    // Generate the main functions entry block
    codegen.builder.position_at_end(entry_block);

    // Set the stack pointer
    codegen.store_register(
        29u32, // Stack pointer
        codegen
            .context
            .i64_type()
            .const_int(MEMORY_SIZE as _, false)
            .into(),
    );

    codegen
        .builder
        .build_unconditional_branch(labels.get(&entry_point).unwrap().basic_block);

    // Generate the main functions exit block
    codegen.builder.position_at_end(exit_block);
    codegen.builder.build_return(None);

    // Generate code for each label.
    for label in labels.values() {
        let basic_block = label.basic_block;
        codegen.builder.position_at_end(basic_block);

        for (i, instr) in label.instructions.iter().enumerate() {
            let addr = label.start_address + (i * INSTRUCTION_SIZE) as u64;

            // Print the instruction if the debug-print feature is enabled.
            #[cfg(feature = "debug-print")]
            {
                let instr = format!("{addr:#06x}: {instr}\n");
                let storage_name = format!("instr_str_{addr:06x}");
                codegen.print_constant_string(&instr, &storage_name);
            }

            recompile_instruction(&codegen, &labels, &instr.clone().unwrap(), addr);
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
    #[cfg(feature = "debug-print")]
    {
        println!("\nGenerated LLVM IR:\n```");
        codegen.module.print_to_stderr();
        println!("```\n");
    }

    // Write the generated LLVM IR to a file, if a path was provided.
    if let Some(path) = ir_path {
        codegen.module.print_to_file(path).unwrap();
        println!("Wrote LLVM IR to '{path}'\n");
    }

    if let Err(err) = codegen.verify() {
        println!("ERROR: Generated code failed to verify:\n\n{err}\nTerminating.");
        return;
    }

    // Run the generated code.
    let main_func: JitFunction<c_fn!()> = codegen.get_function("main").unwrap();
    unsafe { main_func.call() }

    env.print_registers();
}
