fn main() {
    let mut args = std::env::args().skip(1);

    let bin = {
        let path = args.next().expect("no file provided");
        std::fs::read(path).expect("failed to read file")
    };

    let maybe_output_path = args.next();
    let entry_point = 0;
    mips_lifter::lift(&bin, maybe_output_path.as_deref(), entry_point);
}
