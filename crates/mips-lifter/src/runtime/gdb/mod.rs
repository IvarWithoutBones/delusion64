use self::command::{Command, MonitorCommand, MonitorCommandMap};
use super::{memory::tlb::AccessMode, Bus, Environment, GdbIntegration, ValidRuntime};
use crate::target::{Memory, Target};
use gdbstub::{
    conn::ConnectionExt,
    stub::{state_machine::GdbStubStateMachine, GdbStub, SingleThreadStopReason},
    target::{
        self,
        ext::{
            base::singlethread::{SingleThreadBase, SingleThreadResume, SingleThreadSingleStep},
            breakpoints::{Breakpoints, HwWatchpoint, SwBreakpoint, WatchKind},
        },
        Target as GdbTarget, TargetResult,
    },
};
use std::{collections::HashSet, net::TcpStream, ops::Range};

pub(crate) mod command;

/// An error that can occur when creating a GDB connection.
pub enum GdbConnectionError {
    EmptyCommandName(usize),
    DuplicateCommand(&'static str),
    RedefinedInternalCommand(&'static str),
}

impl std::fmt::Debug for GdbConnectionError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            GdbConnectionError::EmptyCommandName(i) => write!(f, "empty name at index {i}"),
            GdbConnectionError::DuplicateCommand(name) => {
                write!(f, "duplicate command name {name:?}")
            }
            GdbConnectionError::RedefinedInternalCommand(name) => {
                write!(f, "redefined internal command {name:?}",)
            }
        }
    }
}

/// A connection to a GDB client.
pub struct Connection<B: Bus> {
    stream: TcpStream,
    monitor_commands: Option<Vec<MonitorCommand<B>>>,
}

impl<B: Bus> Connection<B> {
    /// Create a new GDB connection, and optionally register custom monitor commands.
    pub fn new(
        stream: TcpStream,
        monitor_commands: Option<Vec<MonitorCommand<B>>>,
    ) -> Result<Self, GdbConnectionError> {
        if let Some(cmds) = &monitor_commands {
            for (i, cmd) in cmds.iter().enumerate() {
                if cmd.name.is_empty() {
                    return Err(GdbConnectionError::EmptyCommandName(i));
                }
                if cmds.iter().skip(i + 1).any(|other| other.name == cmd.name) {
                    return Err(GdbConnectionError::DuplicateCommand(cmd.name));
                }
                if Environment::<'_, crate::target::Cpu, B>::monitor_commands()
                    .iter()
                    .any(|other| other.name == cmd.name)
                {
                    return Err(GdbConnectionError::RedefinedInternalCommand(cmd.name));
                }
            }
        }
        Ok(Self {
            stream,
            monitor_commands,
        })
    }
}

#[derive(Debug)]
pub struct Watchpoint {
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

pub struct Debugger<'ctx, T: Target, B: Bus>
where
    for<'a> Environment<'a, T, B>: ValidRuntime,
{
    state_machine: Option<GdbStubStateMachine<'ctx, Environment<'ctx, T, B>, TcpStream>>,
    stop_reason: Option<SingleThreadStopReason<u32>>,
    state: State,
    monitor_commands: MonitorCommandMap<'ctx, T, B>,
    breakpoints: HashSet<u64>,
    watchpoints: Vec<Watchpoint>,
}

impl<'ctx, T: Target, B: Bus> Debugger<'ctx, T, B>
where
    for<'a> Environment<'a, T, B>: ValidRuntime,
{
    pub fn new(env: &mut Environment<'ctx, T, B>, gdb: Connection<B>) -> Self {
        let state_machine = GdbStub::new(gdb.stream).run_state_machine(env).unwrap();
        let monitor_commands =
            Command::monitor_command_map(gdb.monitor_commands.unwrap_or_default());

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
        // TODO: The upper 32 bits are almost always set by the JIT, while GDB ignores them causing mismatches.
        // Should we mask them off here or never set them in the first place?
        let pc = pc & u32::MAX as u64;

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
    for<'a> Environment<'a, T, B>: ValidRuntime,
{
    fn debugger_signal_memory_access(&mut self, address: u64, len: usize, kind: WatchKind) {
        let Some(debugger) = self.debugger.as_mut() else {
            return;
        };

        if let Some(watch) = debugger.watchpoints.iter().find(|w| {
            (w.kind == kind || w.kind == WatchKind::ReadWrite)
                && ((w.addresses.start >= address) && (w.addresses.end <= address + len as u64))
        }) {
            debugger.state = State::Paused;
            debugger.stop_reason = Some(SingleThreadStopReason::Watch {
                tid: (),
                kind: watch.kind,
                addr: address as u32,
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
                println!("GDB disconnected, exiting");
                std::process::exit(0);
            }
        };
    }
}

impl<T: Target, B: Bus> GdbTarget for Environment<'_, T, B>
where
    for<'a> Environment<'a, T, B>: ValidRuntime,
{
    // TODO: this is 32-bits, the 64-bit version is giving trouble because of the following issue:
    // https://github.com/daniel5151/gdbstub/issues/97
    type Arch = gdbstub_arch::mips::Mips;
    type Error = &'static str;

    #[inline(always)]
    fn base_ops(&mut self) -> target::ext::base::BaseOps<'_, Self::Arch, Self::Error> {
        target::ext::base::BaseOps::SingleThread(self)
    }

    #[inline(always)]
    fn support_breakpoints(
        &mut self,
    ) -> Option<target::ext::breakpoints::BreakpointsOps<'_, Self>> {
        Some(self)
    }

    #[inline(always)]
    fn support_monitor_cmd(&mut self) -> Option<target::ext::monitor_cmd::MonitorCmdOps<'_, Self>> {
        // See `src/runtime/gdb/command.rs`.
        Some(self)
    }

    // Workaround for https://github.com/daniel5151/gdbstub/issues/89
    #[inline(always)]
    fn guard_rail_single_step_gdb_behavior(&self) -> gdbstub::arch::SingleStepGdbBehavior {
        gdbstub::arch::SingleStepGdbBehavior::Optional
    }
}

impl<T: Target, B: Bus> Breakpoints for Environment<'_, T, B>
where
    for<'a> Environment<'a, T, B>: ValidRuntime,
{
    #[inline(always)]
    fn support_sw_breakpoint(
        &mut self,
    ) -> Option<target::ext::breakpoints::SwBreakpointOps<'_, Self>> {
        Some(self)
    }

    #[inline(always)]
    fn support_hw_watchpoint(
        &mut self,
    ) -> Option<target::ext::breakpoints::HwWatchpointOps<'_, Self>> {
        Some(self)
    }
}

impl<T: Target, B: Bus> SwBreakpoint for Environment<'_, T, B>
where
    for<'a> Environment<'a, T, B>: ValidRuntime,
{
    fn add_sw_breakpoint(
        &mut self,
        vaddr: <Self::Arch as gdbstub::arch::Arch>::Usize,
        _kind: <Self::Arch as gdbstub::arch::Arch>::BreakpointKind,
    ) -> TargetResult<bool, Self> {
        Ok(self
            .debugger
            .as_mut()
            .unwrap()
            .breakpoints
            .insert(vaddr as _))
    }

    fn remove_sw_breakpoint(
        &mut self,
        vaddr: <Self::Arch as gdbstub::arch::Arch>::Usize,
        _kind: <Self::Arch as gdbstub::arch::Arch>::BreakpointKind,
    ) -> TargetResult<bool, Self> {
        Ok(self
            .debugger
            .as_mut()
            .unwrap()
            .breakpoints
            .remove(&(vaddr as _)))
    }
}

impl<T: Target, B: Bus> HwWatchpoint for Environment<'_, T, B>
where
    for<'a> Environment<'a, T, B>: ValidRuntime,
{
    fn add_hw_watchpoint(
        &mut self,
        addr: <Self::Arch as gdbstub::arch::Arch>::Usize,
        len: <Self::Arch as gdbstub::arch::Arch>::Usize,
        kind: WatchKind,
    ) -> TargetResult<bool, Self> {
        let addresses = addr as u64..addr as u64 + len as u64;
        self.debugger
            .as_mut()
            .unwrap()
            .watchpoints
            .push(Watchpoint { addresses, kind });
        Ok(true)
    }

    fn remove_hw_watchpoint(
        &mut self,
        addr: <Self::Arch as gdbstub::arch::Arch>::Usize,
        len: <Self::Arch as gdbstub::arch::Arch>::Usize,
        kind: WatchKind,
    ) -> TargetResult<bool, Self> {
        let addresses = addr as u64..addr as u64 + len as u64;
        let watchpoints = &mut self.debugger.as_mut().unwrap().watchpoints;
        let Some(index) = watchpoints
            .iter()
            .position(|w| w.addresses == addresses && w.kind == kind)
        else {
            return Ok(false);
        };
        watchpoints.remove(index);
        Ok(true)
    }
}

impl<T: Target, B: Bus> SingleThreadBase for Environment<'_, T, B>
where
    for<'a> Environment<'a, T, B>: ValidRuntime,
{
    fn read_addrs(
        &mut self,
        start_addr: <Self::Arch as gdbstub::arch::Arch>::Usize,
        data: &mut [u8],
    ) -> TargetResult<(), Self> {
        for i in (0..data.len()).step_by(4) {
            let end = data.len().min(i + 4);
            let word: u32 = {
                let vaddr = start_addr as u64 + i as u64;
                let paddr = self
                    .memory
                    .virtual_to_physical_address(vaddr, AccessMode::Read, &self.registers)
                    .map_err(|_| ())?;
                self.read(vaddr, paddr).map_err(|_| ())?.into()
            };
            data[i..end].copy_from_slice(&word.to_ne_bytes()[..end - i]);
        }
        Ok(())
    }

    fn write_addrs(
        &mut self,
        start_addr: <Self::Arch as gdbstub::arch::Arch>::Usize,
        data: &[u8],
    ) -> TargetResult<(), Self> {
        for i in (0..data.len()).step_by(4) {
            let end = data.len().min(i + 4);
            let vaddr = start_addr as u64 + i as u64;
            let paddr = self
                .memory
                .virtual_to_physical_address(vaddr, AccessMode::Write, &self.registers)
                .map_err(|_| ())?;
            let word = u32::from_ne_bytes(data[i..end].try_into().unwrap());
            self.write(vaddr, paddr, word.into()).map_err(|_| ())?;
        }

        Ok(())
    }

    fn read_registers(
        &mut self,
        regs: &mut <Self::Arch as gdbstub::arch::Arch>::Registers,
    ) -> TargetResult<(), Self> {
        self.gdb_read_registers(regs)
    }

    fn write_registers(
        &mut self,
        regs: &<Self::Arch as gdbstub::arch::Arch>::Registers,
    ) -> TargetResult<(), Self> {
        self.gdb_write_registers(regs)
    }

    #[inline(always)]
    fn support_resume(
        &mut self,
    ) -> Option<target::ext::base::singlethread::SingleThreadResumeOps<'_, Self>> {
        Some(self)
    }
}

impl<T: Target, B: Bus> SingleThreadResume for Environment<'_, T, B>
where
    for<'a> Environment<'a, T, B>: ValidRuntime,
{
    fn resume(&mut self, signal: Option<gdbstub::common::Signal>) -> Result<(), Self::Error> {
        if let Some(signal) = signal {
            println!("gdb: resume with signal {signal} is not supported");
        }
        self.debugger.as_mut().unwrap().state = State::Running;
        Ok(())
    }

    // Workaround for https://github.com/daniel5151/gdbstub/issues/89
    #[inline(always)]
    fn support_single_step(
        &mut self,
    ) -> Option<target::ext::base::singlethread::SingleThreadSingleStepOps<'_, Self>> {
        Some(self)
    }
}

impl<T: Target, B: Bus> SingleThreadSingleStep for Environment<'_, T, B>
where
    for<'a> Environment<'a, T, B>: ValidRuntime,
{
    fn step(&mut self, signal: Option<gdbstub::common::Signal>) -> Result<(), Self::Error> {
        if let Some(signal) = signal {
            println!("gdb: step with signal {signal} is not supported");
        }
        self.debugger.as_mut().unwrap().state = State::SingleStep;
        Ok(())
    }
}
