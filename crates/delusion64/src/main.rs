fn main() {
    // mips_lifter::jit_test();

    let bin = {
        let path = std::env::args().nth(1).expect("no path given");
        std::fs::read(path).expect("failed to read file")
    };

    mips_lifter::lift(&bin);
}
