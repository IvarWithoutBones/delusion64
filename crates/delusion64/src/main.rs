use n64_cartridge::Cartridge;

fn main() {
    let mut args = std::env::args().skip(1);

    let bin = {
        let path = args.next().expect("no file provided");
        std::fs::read(path).expect("failed to read file")
    };

    let cart = Cartridge::new(&bin).unwrap_or_else(|e| {
        eprintln!("failed to parse cartridge: {e}");
        std::process::exit(1);
    });
    println!("{cart:#?}");

    let maybe_output_path = args.next();
    let entry_point = 0;
    mips_lifter::lift(
        &cart.ipl3_boot_code.as_slice()[..0xb2c],
        maybe_output_path.as_deref(),
        entry_point,
    );
}
