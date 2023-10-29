use self::command::{Command, MonitorCommand, MonitorCommandMap};
use super::{bus, memory::tlb::AccessMode, Environment};
use gdbstub::{
    conn::ConnectionExt,
    stub::{state_machine::GdbStubStateMachine, GdbStub, SingleThreadStopReason},
    target::{
        self,
        ext::{
            base::singlethread::{SingleThreadBase, SingleThreadResume, SingleThreadSingleStep},
            breakpoints::{Breakpoints, HwWatchpoint, SwBreakpoint, WatchKind},
        },
        Target, TargetResult,
    },
};
use mips_decomp::register;
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
pub struct Connection<Bus: bus::Bus> {
    stream: TcpStream,
    monitor_commands: Option<Vec<MonitorCommand<Bus>>>,
}

impl<Bus: bus::Bus> Connection<Bus> {
    /// Create a new GDB connection, and optionally register custom monitor commands.
    pub fn new(
        stream: TcpStream,
        monitor_commands: Option<Vec<MonitorCommand<Bus>>>,
    ) -> Result<Self, GdbConnectionError> {
        if let Some(cmds) = &monitor_commands {
            for (i, cmd) in cmds.iter().enumerate() {
                if cmd.name.is_empty() {
                    return Err(GdbConnectionError::EmptyCommandName(i));
                }
                if cmds.iter().skip(i + 1).any(|other| other.name == cmd.name) {
                    return Err(GdbConnectionError::DuplicateCommand(cmd.name));
                }
                if Environment::<'_, Bus>::monitor_commands()
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

pub struct Debugger<'ctx, Bus: bus::Bus> {
    state_machine: Option<GdbStubStateMachine<'ctx, Environment<'ctx, Bus>, TcpStream>>,
    stop_reason: Option<SingleThreadStopReason<u32>>,
    state: State,
    monitor_commands: MonitorCommandMap<'ctx, Bus>,
    breakpoints: HashSet<u64>,
    watchpoints: Vec<Watchpoint>,
}

impl<'ctx, Bus: bus::Bus> Debugger<'ctx, Bus> {
    pub fn new(env: &mut Environment<'ctx, Bus>, gdb: Connection<Bus>) -> Self {
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

impl<Bus: bus::Bus> Environment<'_, Bus> {
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

    // This would be much nicer to implement on the `Debugger` struct, but the borrow checker is throwing a fit :/
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

impl<Bus: bus::Bus> Target for Environment<'_, Bus> {
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

impl<Bus: bus::Bus> Breakpoints for Environment<'_, Bus> {
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

impl<Bus: bus::Bus> SwBreakpoint for Environment<'_, Bus> {
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

impl<Bus: bus::Bus> HwWatchpoint for Environment<'_, Bus> {
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

impl<Bus: bus::Bus> SingleThreadBase for Environment<'_, Bus> {
    fn read_addrs(
        &mut self,
        start_addr: <Self::Arch as gdbstub::arch::Arch>::Usize,
        data: &mut [u8],
    ) -> TargetResult<(), Self> {
        for i in (0..data.len()).step_by(4) {
            let end = data.len().min(i + 4);
            let word: u32 = {
                let paddr = self
                    .tlb
                    .translate_vaddr(
                        start_addr as u64 + i as u64,
                        AccessMode::Read,
                        &self.registers,
                    )
                    .map_err(|_| ())?;
                self.read(paddr).map_err(|_| ())?.into()
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
            let paddr = self
                .tlb
                .translate_vaddr(
                    start_addr as u64 + i as u64,
                    AccessMode::Write,
                    &self.registers,
                )
                .map_err(|_| ())?;
            let word = u32::from_ne_bytes(data[i..end].try_into().unwrap());
            self.write(paddr, word.into()).map_err(|_| ())?;
        }

        Ok(())
    }

    fn read_registers(
        &mut self,
        regs: &mut <Self::Arch as gdbstub::arch::Arch>::Registers,
    ) -> TargetResult<(), Self> {
        regs.pc = self.registers[register::Special::Pc] as _;
        regs.hi = self.registers[register::Special::Hi] as _;
        regs.lo = self.registers[register::Special::Lo] as _;
        regs.cp0.cause = self.registers[register::Cp0::CacheErr] as _;
        regs.cp0.status = self.registers[register::Cp0::Status] as _;
        regs.cp0.badvaddr = self.registers[register::Cp0::BadVAddr] as _;
        for (i, r) in regs.r.iter_mut().enumerate() {
            *r = self.registers[register::GeneralPurpose::try_from(i).unwrap()] as _;
        }
        for (i, r) in regs.fpu.r.iter_mut().enumerate() {
            *r = self.registers[register::Fpu::try_from(i).unwrap()] as _;
        }

        Ok(())
    }

    fn write_registers(
        &mut self,
        regs: &<Self::Arch as gdbstub::arch::Arch>::Registers,
    ) -> TargetResult<(), Self> {
        assert!(
            regs.pc == self.registers[register::Special::Pc] as _,
            "gdb: attempted to change PC"
        );

        self.registers[register::Special::Hi] = regs.hi as _;
        self.registers[register::Special::Lo] = regs.lo as _;
        self.registers[register::Cp0::Cause] = regs.cp0.cause as _;
        self.registers[register::Cp0::Status] = regs.cp0.status as _;
        self.registers[register::Cp0::BadVAddr] = regs.cp0.badvaddr as _;
        for (i, r) in regs.r.iter().enumerate() {
            self.registers[register::GeneralPurpose::try_from(i).unwrap()] = *r as _;
        }
        for (i, r) in regs.fpu.r.iter().enumerate() {
            self.registers[register::Fpu::try_from(i).unwrap()] = *r as _;
        }

        Ok(())
    }

    #[inline(always)]
    fn support_resume(
        &mut self,
    ) -> Option<target::ext::base::singlethread::SingleThreadResumeOps<'_, Self>> {
        Some(self)
    }
}

impl<Bus: bus::Bus> SingleThreadResume for Environment<'_, Bus> {
    fn resume(&mut self, signal: Option<gdbstub::common::Signal>) -> Result<(), Self::Error> {
        assert!(signal.is_none(), "gdb: resume with signal is not supported");
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

impl<Bus: bus::Bus> SingleThreadSingleStep for Environment<'_, Bus> {
    fn step(&mut self, signal: Option<gdbstub::common::Signal>) -> Result<(), Self::Error> {
        assert!(signal.is_none(), "gdb: step with signal is not supported");
        self.debugger.as_mut().unwrap().state = State::SingleStep;
        Ok(())
    }
}
