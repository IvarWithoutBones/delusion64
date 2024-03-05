use self::command::{Command, MappedCommand, MonitorCommandMap};
use super::{Bus, Environment, GdbIntegration, ValidRuntime};
use crate::target::{cpu::Cpu, rsp::Rsp, Target};
use gdbstub::{
    conn::ConnectionExt,
    stub::{state_machine::GdbStubStateMachine, GdbStub, SingleThreadStopReason},
    target::ext::breakpoints::WatchKind,
};
use std::{collections::HashSet, net::TcpStream, ops::Range};

pub(crate) mod command;
mod extension;

pub(crate) trait AsU64: Sized + Copy + Clone + std::fmt::Debug + std::fmt::Display {
    fn as_u64(self) -> u64;

    fn from_u64(value: u64) -> Self;
}

impl AsU64 for u64 {
    fn as_u64(self) -> u64 {
        self
    }

    fn from_u64(value: u64) -> Self {
        value
    }
}

impl AsU64 for u32 {
    fn as_u64(self) -> u64 {
        self as u64
    }

    fn from_u64(value: u64) -> Self {
        value as u32
    }
}

/// An error that can occur when creating a GDB connection.
#[derive(thiserror::Error, Debug)]
pub enum GdbConnectionError {
    #[error("empty name at index {0}")]
    EmptyCommandName(usize),
    #[error("command name '{0:?}' has already been defined")]
    DuplicateCommand(&'static str),
    #[error("command name '{0:?}' is reserved for internal use")]
    RedefinedInternalCommand(&'static str),
}

/// A connection to a GDB client.
pub struct Connection<B: Bus> {
    stream: TcpStream,
    monitor_commands: Vec<Command<B>>,
}

impl<B: Bus> Connection<B> {
    fn new_with_target<T: Target>(
        stream: TcpStream,
        monitor_commands: Option<Vec<Command<B>>>,
    ) -> Result<Self, GdbConnectionError>
    where
        for<'a> Environment<'a, T, B>: ValidRuntime<T>,
    {
        if let Some(cmds) = &monitor_commands {
            for (i, cmd) in cmds.iter().enumerate() {
                if cmd.name.is_empty() {
                    return Err(GdbConnectionError::EmptyCommandName(i));
                }
                if cmds.iter().skip(i + 1).any(|other| other.name == cmd.name) {
                    return Err(GdbConnectionError::DuplicateCommand(cmd.name));
                }
                if Environment::<'_, T, B>::monitor_commands()
                    .iter()
                    .any(|other| other.name == cmd.name)
                {
                    return Err(GdbConnectionError::RedefinedInternalCommand(cmd.name));
                }
            }
        }
        Ok(Self {
            stream,
            monitor_commands: monitor_commands.unwrap_or_default(),
        })
    }

    /// Create a new GDB connection for the CPU target, and optionally register custom monitor commands.
    pub fn new_cpu(
        stream: TcpStream,
        monitor_commands: Option<Vec<Command<B>>>,
    ) -> Result<Self, GdbConnectionError> {
        Self::new_with_target::<Cpu>(stream, monitor_commands)
    }

    /// Create a new GDB connection for the RSP target, and optionally register custom monitor commands.
    pub fn new_rsp(
        stream: TcpStream,
        monitor_commands: Option<Vec<Command<B>>>,
    ) -> Result<Self, GdbConnectionError> {
        Self::new_with_target::<Rsp>(stream, monitor_commands)
    }
}

/// MIPS-specific breakpoint kinds.
///
/// This definition was taken from `gdbstub_arch`, thanks to @daniel5151! See the original here:
/// https://github.com/daniel5151/gdbstub/blob/3bc01635466db762dcefc3c6b4ee4096d2ccead7/gdbstub_arch/src/mips/mod.rs#L12
#[derive(Debug)]
pub(crate) enum BreakpointKind {
    /// 16-bit MIPS16 mode breakpoint.
    Mips16,
    /// 16-bit microMIPS mode breakpoint.
    MicroMips16,
    /// 32-bit standard MIPS mode breakpoint.
    Mips32,
    /// 32-bit microMIPS mode breakpoint.
    MicroMips32,
}

impl gdbstub::arch::BreakpointKind for BreakpointKind {
    fn from_usize(kind: usize) -> Option<Self> {
        Some(match kind {
            2 => BreakpointKind::Mips16,
            3 => BreakpointKind::MicroMips16,
            4 => BreakpointKind::Mips32,
            5 => BreakpointKind::MicroMips32,
            _ => return None,
        })
    }
}

#[derive(Debug)]
struct Watchpoint {
    addresses: Range<u64>,
    kind: WatchKind,
}

#[derive(Default)]
enum State {
    Running,
    #[default]
    Paused,
    SingleStep,
}

type Address<'ctx, T, B> = <Environment<'ctx, T, B> as GdbIntegration<T>>::Usize;

pub struct Debugger<'ctx, T: Target, B: Bus>
where
    for<'a> Environment<'a, T, B>: ValidRuntime<T>,
{
    state_machine: Option<GdbStubStateMachine<'ctx, Environment<'ctx, T, B>, TcpStream>>,
    stop_reason: Option<SingleThreadStopReason<Address<'ctx, T, B>>>,
    state: State,
    monitor_commands: MonitorCommandMap<'ctx, T, B>,
    breakpoints: HashSet<u64>,
    watchpoints: Vec<Watchpoint>,
}

impl<'ctx, T: Target, B: Bus> Debugger<'ctx, T, B>
where
    for<'a> Environment<'a, T, B>: ValidRuntime<T>,
{
    pub fn new(env: &mut Environment<'ctx, T, B>, gdb: Connection<B>) -> Self {
        let state_machine = GdbStub::new(gdb.stream).run_state_machine(env).unwrap();
        let monitor_commands = MappedCommand::new_map(gdb.monitor_commands);

        Self {
            state: State::default(),
            breakpoints: HashSet::new(),
            watchpoints: Vec::new(),
            stop_reason: None,
            state_machine: Some(state_machine),
            monitor_commands,
        }
    }

    pub const fn is_paused(&self) -> bool {
        matches!(self.state, State::Paused) || self.stop_reason.is_some()
    }

    pub fn on_instruction(&mut self, pc: u64) {
        if self.breakpoints.contains(&pc) {
            self.stop_reason = Some(SingleThreadStopReason::SwBreak(()));
            self.state = State::Paused;
        }

        match self.state {
            State::SingleStep => {
                self.state = State::Paused;
                self.stop_reason = Some(SingleThreadStopReason::DoneStep);
            }

            State::Paused | State::Running => {}
        }
    }

    pub fn signal_panicked(&mut self) {
        self.state = State::Paused;
        self.stop_reason = Some(SingleThreadStopReason::Signal(
            gdbstub::common::Signal::SIGTERM,
        ));
    }
}

impl<T: Target, B: Bus> Environment<'_, T, B>
where
    for<'a> Environment<'a, T, B>: ValidRuntime<T>,
{
    fn debugger_signal_memory_access(&mut self, addr: impl AsU64, len: usize, kind: WatchKind) {
        let Some(debugger) = self.debugger.as_mut() else {
            return;
        };

        let addr = addr.as_u64();
        if let Some(watch) = debugger.watchpoints.iter().find(|w| {
            let kind_match = (w.kind == kind) || (w.kind == WatchKind::ReadWrite);
            let addr_match =
                (w.addresses.start <= addr) && (w.addresses.end >= (addr + len as u64));
            kind_match && addr_match
        }) {
            debugger.state = State::Paused;
            debugger.stop_reason = Some(SingleThreadStopReason::Watch {
                tid: (),
                kind: watch.kind,
                addr: Address::from_u64(addr),
            });
        }
    }

    pub fn debugger_signal_read(&mut self, address: u64, len: usize) {
        self.debugger_signal_memory_access(address, len, WatchKind::Read);
    }

    pub fn debugger_signal_write(&mut self, address: u64, len: usize) {
        self.debugger_signal_memory_access(address, len, WatchKind::Write);
    }

    pub fn update_debugger(&mut self) {
        self.debugger.as_mut().unwrap().state_machine = match self
            .debugger
            .as_mut()
            .unwrap()
            .state_machine
            .take()
            .unwrap()
        {
            GdbStubStateMachine::Idle(mut gdb) => {
                let byte = gdb.borrow_conn().read().unwrap();
                Some(gdb.incoming_data(self, byte).unwrap())
            }

            GdbStubStateMachine::CtrlCInterrupt(gdb) => {
                self.debugger.as_mut().unwrap().state = State::Paused;
                let stop_reason = Some(SingleThreadStopReason::Signal(
                    gdbstub::common::Signal::SIGINT,
                ));
                Some(gdb.interrupt_handled(self, stop_reason).unwrap())
            }

            GdbStubStateMachine::Running(mut gdb) => {
                let conn = gdb.borrow_conn();
                if conn.peek().map(|b| b.is_some()).unwrap_or(false) {
                    // Handle incoming data
                    let byte = conn.read().unwrap();
                    Some(gdb.incoming_data(self, byte).unwrap())
                } else if let Some(stop_reason) = self.debugger.as_mut().unwrap().stop_reason.take()
                {
                    // Report the reason execution stopped
                    Some(gdb.report_stop(self, stop_reason).unwrap())
                } else {
                    // Continue execution
                    Some(gdb.into())
                }
            }

            GdbStubStateMachine::Disconnected(_gdb) => {
                self.debugger.take();
                self.panic_update_debugger("GDB disconnected");
            }
        };
    }
}
