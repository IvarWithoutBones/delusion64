use crate::bus::Bus;
use clap::Parser;
use emgui::{context, EmulatorHandle, UiBuilder};
use input::ControllerEvent;
use mips_lifter::{gdb, register, JitBuilder};
use n64_cartridge::{Cartridge, Cic};
use std::{
    io,
    net::{TcpListener, TcpStream},
};

pub mod bus;
pub mod input;

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

const FALLBACK_CIC: Cic = Cic::Cic6102;

struct Emulator {
    context: Option<context::Emulator<ControllerEvent>>,
    thread: Option<std::thread::JoinHandle<context::Emulator<ControllerEvent>>>,
    args: CommandLineInterface,
}

impl Emulator {
    fn new(ctx: context::Emulator<ControllerEvent>, args: CommandLineInterface) -> Self {
        Self {
            context: Some(ctx),
            thread: None,
            args,
        }
    }

    fn join_thread(&mut self) {
        let thread = self.thread.take().expect("emulator thread is none");
        self.context = Some(thread.join().expect("failed to join emulator thread"));
    }

    fn run(
        rom: Box<[u8]>,
        ctx: context::Emulator<ControllerEvent>,
        args: CommandLineInterface,
    ) -> context::Emulator<ControllerEvent> {
        println!("spawning emu");
        let mut cart = Cartridge::new(&rom).unwrap_or_else(|e| {
            eprintln!("failed to parse cartridge: {e}");
            std::process::exit(1);
        });

        let gdb = args.gdb.map(|port| {
            let stream = wait_for_gdb_connection(port.unwrap_or(DEFAULT_GDB_PORT))
                .expect("failed to wait for GDB connection");
            gdb::Connection::new(stream, Some(Bus::gdb_monitor_commands()))
                .expect("failed to create to GDB connection")
        });

        cart.cic = cart.cic.or_else(|e| {
            println!("warning: did not recognize CIC variant, assuming {FALLBACK_CIC:#?}: {e:#x?}");
            Ok(FALLBACK_CIC)
        });

        // The initial state of the registers, simulating the effects of IPL 1+2.
        let regs = match cart.cic.as_ref().unwrap() {
            Cic::Cic6102 => [
                (register::GeneralPurpose::Sp.into(), 0xFFFF_FFFF_A400_1FF0),
                (register::GeneralPurpose::T3.into(), 0xFFFF_FFFF_A400_0040),
                (register::GeneralPurpose::S4.into(), 0x0000_0000_0000_0001),
                (register::GeneralPurpose::S6.into(), 0x0000_0000_0000_003F),
                (register::Cp0::Random.into(), 0x0000_001F),
                (register::Cp0::Status.into(), 0x3400_0000),
                (register::Cp0::PRId.into(), 0x0000_0B22),
                (register::Cp0::Config.into(), 0x7006_E463),
                (
                    register::FpuControl::ImplementationRevision.into(),
                    0x0000_0A00,
                ),
                (register::FpuControl::ControlStatus.into(), 0x0100_0800),
                (register::Special::Pc.into(), 0x0000_0000_A400_0040),
            ],
            cic => todo!("{cic:#?} HLE initial registers"),
        };

        // This will copy the first 0x1000 bytes of the PIF ROM to the RSP DMEM, simulating IPL2.
        // It will also write the size of RDRAM.
        let bus = Bus::new(ctx, cart, false);

        JitBuilder::new(bus)
            .maybe_with_gdb(gdb)
            .with_trace(args.trace)
            .with_registers(&regs)
            .run()
            .context
    }
}

impl emgui::EmulatorHandle for Emulator {
    type InputEvent = ControllerEvent;

    fn start(&mut self, rom: Box<[u8]>) {
        let ctx = self.context.take().expect("context is none");
        let args = self.args.clone();
        self.thread = Some(
            std::thread::Builder::new()
                .name("cpu".to_string())
                .spawn(move || Self::run(rom, ctx, args))
                .expect("failed to spawn emulator thread"),
        );
    }

    fn stop(&mut self) {
        self.join_thread();
    }
}

#[derive(Parser, Debug, Clone)]
#[command(author = "IvarWithoutBones")]
struct CommandLineInterface {
    #[clap(short, long)]
    rom: Option<String>,

    #[clap(short, long)]
    trace: bool,

    #[clap(long)]
    headless: bool,

    #[clap(short, long, value_name = "port")]
    gdb: Option<Option<u16>>,
}

fn main() {
    let cli = CommandLineInterface::parse();
    let (emu_context, ui_context) = context::channel();
    let mut emu = Emulator::new(emu_context, cli.clone());

    if cli.headless {
        let rom = if let Some(path) = cli.rom {
            std::fs::read(path)
                .expect("failed to read ROM")
                .into_boxed_slice()
        } else {
            eprintln!("no ROM specified");
            std::process::exit(1);
        };

        emu.start(rom);
        emu.join_thread();
    } else {
        let mut ui = UiBuilder::new("Delusion64", emu, ui_context)
            .with_initial_screen_size([640.0, 480.0])
            .with_input_devices(vec![ControllerEvent::A]);
        if let Some(path) = cli.rom {
            ui = ui.with_rom_path(path);
        }

        // Exit immediately when the GUI is closed.
        ui.run();
    }
}
