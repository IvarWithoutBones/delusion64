use crate::bus::Bus;
use clap::Parser;
use mips_lifter::{gdb, register};
use n64_cartridge::Cartridge;
use std::{
    io,
    net::{TcpListener, TcpStream},
};

pub mod bus;

/// Blocks until a GDB client connects via TCP
fn wait_for_gdb_connection(port: u16) -> io::Result<TcpStream> {
    let sockaddr = format!("localhost:{port}");
    eprintln!("waiting for a GDB connection on {sockaddr:?}...");
    let sock = TcpListener::bind(sockaddr)?;
    let (stream, addr) = sock.accept()?;
    eprintln!("debugger connected from {addr}");
    Ok(stream)
}

const DEFAULT_GDB_PORT: u16 = 9001;

#[derive(Parser, Debug)]
#[command(author = "IvarWithoutBones")]
struct CommandLineInterface {
    #[clap(short, long)]
    rom: String,

    #[clap(short, long)]
    show_cartridge: bool,

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

    if cli.show_cartridge {
        println!("{cart:#?}");
        std::process::exit(0);
    }

    // This will copy the first 0x1000 bytes of the PIF ROM to the RSP DMEM, simulating IPL2.
    let bus = Bus::new(cart);

    // The initial state of the registers, simulating the effects of IPL 1+2.
    let regs = &[
        (register::GeneralPurpose::Sp.into(), 0xFFFF_FFFF_A400_1FF0),
        (register::GeneralPurpose::T3.into(), 0xFFFF_FFFF_A400_0040),
        (register::GeneralPurpose::S4.into(), 0x0000_0000_0000_0001),
        (register::GeneralPurpose::S6.into(), 0x0000_0000_0000_003F),
        (register::Cp0::Random.into(), 0x0000_001F),
        (register::Cp0::Status.into(), 0x3400_0000),
        (register::Cp0::PRId.into(), 0x0000_0B00),
        (register::Cp0::Config.into(), 0x7006_E463),
        (register::Fpu::F31.into(), 0x0100_0800),
        (register::Special::Pc.into(), 0x0000_0000_A400_0040),
    ];

    let gdb = cli.gdb.map(|port| {
        let stream = wait_for_gdb_connection(port.unwrap_or(DEFAULT_GDB_PORT))
            .expect("failed to wait for GDB connection");
        gdb::Connection::new(stream, Some(Bus::gdb_monitor_commands())).unwrap()
    });

    mips_lifter::run(bus, regs, gdb)
}
