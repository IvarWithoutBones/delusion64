use crate::codegen::CodeGen;
use inkwell::{context::Context, execution_engine::JitFunction, OptimizationLevel};
use mips_decomp::register;
use std::{io::Write, net::TcpStream, ops::Range};

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
pub fn run<Mem>(
    mem: Mem,
    binary_range: Range<u32>,
    gdb_stream: Option<TcpStream>,
    ir_path: Option<&str>,
    disassembly_path: Option<&str>,
) where
    Mem: runtime::Memory,
{
    let bin = binary_range
        .map(|addr| mem.read_u8(addr).unwrap())
        .collect::<Vec<_>>()
        .into_boxed_slice();

    if let Some(path) = disassembly_path {
        let mut labels = mips_decomp::LabelList::from(&*bin);
        labels.set_offset(0x0000_0000_A400_0000 / 4);
        let labels_str = format!("{labels:#?}");

        let mut file = std::fs::File::create(path).unwrap();
        writeln!(file, "{labels_str}").unwrap();
        println!("wrote disassembly to {path}");
        return;
    }

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

    // TODO: Calling `generate_label_functions` here will precompile everything,
    // but as blocks are purely looked up by vaddr this only slows us down for now.
    let labels_with_context = vec![];
    // let labels_with_context = label::generate_label_functions(labels, &context, &module);

    // Create our own compilation/runtime environment.
    let (env, globals) = {
        let env = runtime::Environment::new(mem, gdb_stream);
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

    // Set the initial CPU state, emulating the PIF ROM (IPL1+IPL2). TODO: move this to a downstream crate

    // Copy IPL3 into memory
    for (i, byte) in bin[..0x1000].iter().enumerate() {
        let addr = codegen
            .context
            .i64_type()
            .const_int(i as u64 + 0x0000_0000_A400_0000, false);
        let value = codegen.context.i8_type().const_int(*byte as _, false);
        codegen.write_memory(addr, value);
    }

    codegen.write_register(
        register::GeneralPurpose::Sp,
        i64_type.const_int(0xFFFF_FFFF_A400_1FF0, false),
    );

    codegen.write_register(
        register::GeneralPurpose::T3,
        i64_type.const_int(0xFFFF_FFFF_A400_0040, false),
    );

    codegen.write_register(
        register::GeneralPurpose::S4,
        i64_type.const_int(0x0000_0000_0000_0001, false),
    );

    codegen.write_register(
        register::GeneralPurpose::S6,
        i64_type.const_int(0x0000_0000_0000_003F, false),
    );

    codegen.write_register(
        register::Cp0::Random,
        i64_type.const_int(0x0000_001F, false),
    );

    codegen.write_register(
        register::Cp0::Status,
        i64_type.const_int(0x3400_0000, false),
    );

    codegen.write_register(register::Cp0::PRId, i64_type.const_int(0x0000_0B00, false));

    codegen.write_register(
        register::Cp0::Config,
        i64_type.const_int(0x0006_E463, false),
    );

    codegen.write_register(
        register::Special::Pc,
        i64_type.const_int(0x0000_0000_A400_0040, false),
    );

    // Save the current stack frame, will be restored on every block to ensure no stack overflows occur.
    // Its a rather bruteforce approach, but it works :)
    codegen.save_host_stack();

    codegen.build_dynamic_jump(codegen.read_register(i64_type, register::Special::Pc));

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

    // Compile the entry point, and with that lazily all other codepaths.
    let main_fn: JitFunction<unsafe extern "C" fn()> =
        unsafe { codegen.execution_engine.get_function("main").unwrap() };

    env.attach_codegen(codegen);

    // Run the generated code!
    unsafe { main_fn.call() };
    println!("finished executing code:{:?}", env.registers);
}
