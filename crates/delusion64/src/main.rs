use crate::emu::Emulator;
use clap::Parser;
use n64_cartridge::Cartridge;
use std::{
    io,
    net::{TcpListener, TcpStream},
};

mod emu;

const DEFAULT_GDB_PORT: u16 = 9001;

/// Blocks until a GDB client connects via TCP
pub fn wait_for_gdb_connection(port: Option<u16>) -> io::Result<TcpStream> {
    let sockaddr = format!("localhost:{}", port.unwrap_or(DEFAULT_GDB_PORT));
    eprintln!("Waiting for a GDB connection on {sockaddr:?}...");
    let sock = TcpListener::bind(sockaddr)?;
    let (stream, addr) = sock.accept()?;

    eprintln!("Debugger connected from {addr}");
    Ok(stream)
}

#[derive(Parser, Debug)]
#[command(author = "IvarWithoutBones")]
struct CommandLineInterface {
    #[clap(short, long)]
    rom: String,

    #[clap(short, long, value_name = "path")]
    llvm_ir_output: Option<String>,

    #[clap(short, long, value_name = "port")]
    gdb: Option<Option<u16>>,
}

fn main() {
    let cli = CommandLineInterface::parse();

    let bin = std::fs::read(cli.rom).expect("failed to read file");
    let cart = Cartridge::new(&bin).unwrap_or_else(|e| {
        eprintln!("failed to parse cartridge: {e}");
        std::process::exit(1);
    });
    println!("{cart:#?}");

    let maybe_gdb_stream = cli
        .gdb
        .map(|port| wait_for_gdb_connection(port).expect("failed to wait for GDB connection"));

    let emulator = Emulator::new(cart.ipl3_boot_code.clone());

    mips_lifter::lift(
        cart.ipl3_boot_code.as_slice(),
        emu::CARTRIDGE_ROM.start,
        emulator,
        cli.llvm_ir_output.as_deref(),
        maybe_gdb_stream,
    );
}
