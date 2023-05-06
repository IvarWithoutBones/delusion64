fn main() {
    mips_lifter::jit_test();

    let bin = {
        let path = std::env::args().nth(1).expect("no path given");
        std::fs::read(path).expect("failed to read file")
    };

    println!("\ndisassembly:\n");
    let disasm = mips_decomp::Disassembler::from(&bin[..]);
    for instr in disasm {
        println!("{instr}");
    }
}
