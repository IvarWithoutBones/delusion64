use crate::{codegen::CodeGen, recompiler::recompile_instruction, runtime::RuntimeFunction};
use inkwell::{context::Context, execution_engine::JitFunction, OptimizationLevel};
use mips_decomp::{register, MaybeInstruction, INSTRUCTION_SIZE};
use std::net::TcpStream;

mod codegen;
mod label;
mod recompiler;
pub mod runtime;

pub fn lift<Mem>(
    bin: &[u8],
    entry_point: u64,
    mem: Mem,
    ir_path: Option<&str>,
    gdb_stream: Option<TcpStream>,
) where
    Mem: runtime::Memory,
{
    println!("disassembling {:#x} bytes", bin.len());
    let decomp = mips_decomp::Decompiler::from(bin);
    let blocks = mips_decomp::reorder_delay_slots(decomp.blocks().collect());

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

    // Generate labels for all blocks, and insert them into the function.
    println!("generating labels");
    let mut label_pass = label::LabelPass::new(&context, module, &blocks, entry_point);
    label_pass.run();

    // label_pass.dump();
    // return;

    let (context, module, labels) = label_pass.consume();

    let (env, globals) = {
        let labels = labels.values().map(|l| l.to_owned()).collect::<Vec<_>>();
        let env = runtime::Environment::new(mem, labels, gdb_stream);
        let globals = env.map_into(&module, &execution_engine);
        (env, globals)
    };
    let codegen = CodeGen::new(context, module, execution_engine, globals, labels);

    // Generate the main functions entry block
    codegen.builder.position_at_end(entry_block);
    codegen.write_general_reg(
        register::GeneralPurpose::Sp,
        codegen.build_i64(0x500_u32).into_int_value().into(),
    );
    codegen
        .builder
        .build_unconditional_branch(codegen.get_basic_block(entry_point));

    // Generate code for each label.
    for (lab_idx, label) in codegen.labels.values().enumerate() {
        println!(
            "generating instructions for label {}/{}",
            lab_idx + 1,
            codegen.labels.len()
        );

        let basic_block = label.basic_block;
        codegen.builder.position_at_end(basic_block);
        for (i, instr) in label.instructions.iter().enumerate() {
            let addr = label.start_address + (i * INSTRUCTION_SIZE) as u64;

            // Print the instruction if the debug-print feature is enabled.
            #[cfg(feature = "debug-print")]
            {
                let instr_str = if let Some(target) = instr.try_resolve_static_jump(addr) {
                    format!("{addr:#06x}: {instr}\t-> {target:#06x}\n")
                } else {
                    format!("{addr:#06x}: {instr}\n")
                };

                let storage_name = format!("instr_str_{addr:06x}");
                codegen.print_constant_string(&instr_str, &storage_name);
            }

            let pc = codegen.build_i64(addr).into_int_value();
            codegen.write_special_reg(register::Special::Pc, pc.into());

            // Call the `on_instruction` callback from the environment, used for the debugger.
            env_call!(codegen, RuntimeFunction::OnInstruction, []);

            // Recompile the instruction.
            if let MaybeInstruction::Instruction(instr) = instr {
                recompile_instruction(&codegen, instr, addr);
            } else {
                codegen.builder.build_unreachable();
            }
        }

        // If the block doesn't end with a terminator, insert a branch to the next block.
        if basic_block.get_terminator().is_none() {
            if let Some(fall_through) = label.fall_through {
                codegen.builder.build_unconditional_branch(fall_through);
            } else {
                codegen.builder.build_unreachable();
            }
        }
    }

    println!("finished generating instructions");

    // Write the generated LLVM IR to a file, if a path was provided.
    if let Some(path) = ir_path {
        codegen.module.print_to_file(path).unwrap();
        println!("Wrote LLVM IR to '{path}'\n");
    }

    println!("verifying generated code...");

    if let Err(err) = codegen.verify() {
        println!("ERROR: Generated code failed to verify:\n\n{err}\nTerminating.");
        return;
    }

    println!("compiling and running code...");

    // Run the generated code.
    let main_func: JitFunction<unsafe extern "C" fn()> = codegen.get_function("main").unwrap();
    unsafe { main_func.call() }

    env.print_registers();
}
