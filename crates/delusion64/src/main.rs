use crate::bus::Bus;
use clap::{Parser, ValueEnum};
use emgui::{context, UiBuilder};
use input::ControllerEvent;
use mips_lifter::{
    gdb::{self, util::wait_for_gdb_connection},
    target::cpu::register,
    JitBuilder,
};
use n64_cartridge::{Cartridge, Cic};
use n64_common::log::{error, info};

pub mod bus;
pub mod input;

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
        has_gui: bool,
    ) -> context::Emulator<ControllerEvent> {
        info!("spawning emu");
        let mut cart = Cartridge::new(&rom).unwrap_or_else(|e| {
            eprintln!("failed to parse cartridge: {e}");
            std::process::exit(1);
        });

        let (cpu_gdb, rsp_gdb) = args
            .gdb
            .map(|cmd| match cmd {
                GdbType::Rsp => {
                    let stream = wait_for_gdb_connection(None)
                        .expect("failed to wait for RSP GDB connection");
                    (None, Some(stream))
                }
                GdbType::Cpu => {
                    let stream = wait_for_gdb_connection(None)
                        .expect("failed to wait for CPU GDB connection");
                    let cmds = Some(Bus::gdb_monitor_commands());
                    let conn = gdb::Connection::new_cpu(stream, cmds)
                        .expect("failed to create to GDB connection");
                    (Some(conn), None)
                }
            })
            .unwrap_or((None, None));

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
        let bus = Bus::new(ctx, cart, rsp_gdb, has_gui);

        JitBuilder::new_cpu(bus)
            .maybe_with_gdb(cpu_gdb)
            .with_trace(args.trace)
            .with_cpu_registers(regs.into())
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
                .spawn(move || Self::run(rom, ctx, args, true))
                .expect("failed to spawn emulator thread"),
        );
    }

    fn stop(&mut self) {
        self.join_thread();
    }
}

#[derive(ValueEnum, Debug, Clone)]
enum GdbType {
    Cpu,
    Rsp,
}

#[derive(Parser, Debug, Clone)]
#[command(author = "IvarWithoutBones", version)]
struct CommandLineInterface {
    #[clap(short, long)]
    rom: Option<String>,

    #[clap(short, long)]
    trace: bool,

    #[clap(short, long)]
    log: Option<String>,

    #[clap(long)]
    headless: bool,

    #[clap(long, short)]
    gdb: Option<GdbType>,
}

fn main() {
    let cli = CommandLineInterface::parse();
    env_logger::Builder::new()
        .parse_filters(cli.log.as_deref().unwrap_or("info"))
        .format_timestamp(None)
        .init();

    let (emu_context, ui_context) = context::channel();
    let mut emu = Emulator::new(emu_context, cli.clone());

    if cli.headless {
        let rom = if let Some(path) = &cli.rom {
            std::fs::read(path)
                .expect("failed to read ROM")
                .into_boxed_slice()
        } else {
            error!("no ROM specified");
            std::process::exit(1);
        };

        Emulator::run(rom, emu.context.take().unwrap(), cli, false);
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
