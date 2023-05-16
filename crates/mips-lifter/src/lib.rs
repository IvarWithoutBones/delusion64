use crate::{codegen::CodeGen, recompiler::recompile_instruction};
use inkwell::{context::Context, execution_engine::JitFunction, OptimizationLevel};
use mips_decomp::INSTRUCTION_SIZE;

pub mod codegen;
pub mod label;
pub mod recompiler;

macro_rules! c_fn {
    ($($arg:ident),*) => {
        unsafe extern "C" fn($($arg),*)
    };
}

#[cfg(feature = "debug-print")]
unsafe extern "C" fn runtime_print_regs(regs_ptr: *const u64) {
    println!("\nregisters:");
    let regs = std::slice::from_raw_parts(regs_ptr, mips_decomp::REGISTER_COUNT);
    for (i, r) in regs.iter().enumerate() {
        let name = mips_decomp::format::register_name(i as _);
        println!("{name: <4} = {r:#x}");
    }
}

#[cfg(feature = "debug-print")]
fn register_print_regs(codegen: &CodeGen<'_>) {
    let i64_type = codegen.context.i64_type();
    codegen.add_function::<c_fn!(u64)>(
        "print_regs",
        runtime_print_regs as *const _,
        &[i64_type.array_type(mips_decomp::REGISTER_COUNT as _).into()],
    );
}

#[cfg(feature = "debug-print")]
fn call_print_regs(codegen: &CodeGen<'_>) {
    codegen.build_call(
        "print_regs",
        &[codegen.registers_global().as_pointer_value().into()],
    );
}

#[cfg(feature = "debug-print")]
unsafe extern "C" fn runtime_print_string(string_ptr: *const u8, len: u64) {
    let slice = std::slice::from_raw_parts(string_ptr, len as _);
    let string = std::str::from_utf8(slice).unwrap();
    print!("{string}");
}

#[cfg(feature = "debug-print")]
fn register_print_string(codegen: &CodeGen<'_>) {
    codegen.add_function::<c_fn!(u64, u64)>(
        "print_string",
        runtime_print_string as *const _,
        &[
            codegen
                .context
                .i64_type()
                .ptr_type(inkwell::AddressSpace::default())
                .into(),
            codegen.context.i64_type().into(),
        ],
    )
}

#[cfg(feature = "debug-print")]
fn call_print_string(codegen: &CodeGen<'_>, string: &str, string_data_name: &str) {
    let instr_ptr = codegen
        .builder
        .build_global_string_ptr(string, string_data_name)
        .as_pointer_value();
    let instr_len = codegen
        .context
        .i64_type()
        .const_int(string.len() as _, false);
    codegen.build_call("print_string", &[instr_ptr.into(), instr_len.into()]);
}

pub fn lift(bin: &[u8]) {
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

    // Set up the code generation context, and register debugging functions.
    let codegen = CodeGen::new(context, module, execution_engine);
    #[cfg(feature = "debug-print")]
    {
        register_print_regs(&codegen);
        register_print_string(&codegen);
    }

    // Generate the main functions entry block
    codegen.builder.position_at_end(entry_block);
    // codegen.store_register(4u32, codegen.context.i64_type().const_int(10, false).into());
    codegen
        .builder
        .build_unconditional_branch(labels.get(&0).unwrap().basic_block);
    // Generate the main functions exit block
    codegen.builder.position_at_end(exit_block);
    #[cfg(feature = "debug-print")]
    call_print_regs(&codegen);
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
                let instr_str = format!("{addr:#06x}: {instr}\n");
                let instr_data_str = format!("instr_str_{addr:06x}");
                call_print_string(&codegen, &instr_str, &instr_data_str);
            }

            recompile_instruction(&codegen, &labels, instr, addr);

            if addr as usize + INSTRUCTION_SIZE == bin.len() {
                codegen.builder.build_unconditional_branch(exit_block);
            }
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

    // Run the generated code.
    let main_func: JitFunction<c_fn!()> = codegen.get_function("main").unwrap();
    unsafe { main_func.call() }
}
