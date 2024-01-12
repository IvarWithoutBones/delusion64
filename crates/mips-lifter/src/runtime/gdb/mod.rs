use self::command::{Command, MonitorCommand, MonitorCommandMap};
use super::{memory::tlb::AccessMode, Bus, Environment, GdbIntegration, ValidRuntime};
use crate::target::{Cpu, Memory, Target};
use gdbstub::{
    conn::ConnectionExt,
    stub::{state_machine::GdbStubStateMachine, GdbStub, SingleThreadStopReason},
    target::{
        self,
        ext::{
            base::{
                single_register_access::SingleRegisterAccess,
                singlethread::{SingleThreadBase, SingleThreadResume, SingleThreadSingleStep},
            },
            breakpoints::{Breakpoints, HwWatchpoint, SwBreakpoint, WatchKind},
        },
        Target as GdbTarget, TargetResult,
    },
};
use std::{collections::HashSet, net::TcpStream, ops::Range};

pub(crate) mod command;

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
    monitor_commands: Option<Vec<MonitorCommand<B>>>,
}

impl<B: Bus> Connection<B> {
    fn new_with_target<T: Target>(
        stream: TcpStream,
        monitor_commands: Option<Vec<MonitorCommand<B>>>,
    ) -> Result<Self, GdbConnectionError>
    where
        for<'a> Environment<'a, T, B>: ValidRuntime,
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
            monitor_commands,
        })
    }

    /// Create a new GDB connection for the CPU target, and optionally register custom monitor commands.
    pub fn new_cpu(
        stream: TcpStream,
        monitor_commands: Option<Vec<MonitorCommand<B>>>,
    ) -> Result<Self, GdbConnectionError> {
        Self::new_with_target::<Cpu>(stream, monitor_commands)
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
            _ => None?,
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

pub struct Debugger<'ctx, T: Target, B: Bus>
where
    for<'a> Environment<'a, T, B>: ValidRuntime,
{
    state_machine: Option<GdbStubStateMachine<'ctx, Environment<'ctx, T, B>, TcpStream>>,
    stop_reason: Option<SingleThreadStopReason<u64>>,
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
    fn debugger_signal_memory_access(&mut self, addr: u64, len: usize, kind: WatchKind) {
        let Some(debugger) = self.debugger.as_mut() else {
            return;
        };

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
                addr,
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

impl<T: Target, B: Bus> GdbTarget for Environment<'_, T, B>
where
    for<'a> Environment<'a, T, B>: ValidRuntime,
{
    type Arch = <Self as GdbIntegration>::Arch;
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
        Ok(self.debugger.as_mut().unwrap().breakpoints.insert(vaddr))
    }

    fn remove_sw_breakpoint(
        &mut self,
        vaddr: <Self::Arch as gdbstub::arch::Arch>::Usize,
        _kind: <Self::Arch as gdbstub::arch::Arch>::BreakpointKind,
    ) -> TargetResult<bool, Self> {
        Ok(self.debugger.as_mut().unwrap().breakpoints.remove(&vaddr))
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
        let addresses = addr..addr + len;
        let watchpoint = Watchpoint { addresses, kind };
        self.debugger.as_mut().unwrap().watchpoints.push(watchpoint);
        Ok(true)
    }

    fn remove_hw_watchpoint(
        &mut self,
        addr: <Self::Arch as gdbstub::arch::Arch>::Usize,
        len: <Self::Arch as gdbstub::arch::Arch>::Usize,
        kind: WatchKind,
    ) -> TargetResult<bool, Self> {
        let addresses = addr..addr + len;
        let watchpoints = &mut self.debugger.as_mut().unwrap().watchpoints;
        let Some(index) = watchpoints
            .iter()
            .position(|w| (w.addresses == addresses) && (w.kind == kind))
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
    ) -> TargetResult<usize, Self> {
        for i in (0..data.len()).step_by(4) {
            let word: u32 = {
                let vaddr = start_addr + i as u64;
                let paddr = self
                    .memory
                    .virtual_to_physical_address(vaddr, AccessMode::Read, &self.registers)
                    .map_err(|_| ())?;
                self.read(vaddr, paddr).map_err(|_| ())?.into()
            };
            let end = data.len().min(i + 4);
            data[i..end].copy_from_slice(&word.to_be_bytes()[..end - i]);
        }
        Ok(data.len())
    }

    fn write_addrs(
        &mut self,
        start_addr: <Self::Arch as gdbstub::arch::Arch>::Usize,
        data: &[u8],
    ) -> TargetResult<(), Self> {
        for i in (0..data.len()).step_by(4) {
            let value = {
                let end = data.len().min(i + 4);
                u32::from_be_bytes(data[i..end].try_into().unwrap())
            };
            let vaddr = start_addr + i as u64;
            let paddr = self
                .memory
                .virtual_to_physical_address(vaddr, AccessMode::Write, &self.registers)
                .map_err(|_| ())?;
            self.write(vaddr, paddr, value.into()).map_err(|_| ())?;
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
    fn support_single_register_access(
        &mut self,
    ) -> Option<target::ext::base::single_register_access::SingleRegisterAccessOps<'_, (), Self>>
    {
        Some(self)
    }

    #[inline(always)]
    fn support_resume(
        &mut self,
    ) -> Option<target::ext::base::singlethread::SingleThreadResumeOps<'_, Self>> {
        Some(self)
    }
}

impl<T: Target, B: Bus> SingleRegisterAccess<()> for Environment<'_, T, B>
where
    for<'a> Environment<'a, T, B>: ValidRuntime,
{
    fn read_register(
        &mut self,
        _tid: (),
        reg_id: <Self::Arch as gdbstub::arch::Arch>::RegId,
        buf: &mut [u8],
    ) -> TargetResult<usize, Self> {
        self.gdb_read_register(reg_id, buf)
    }

    fn write_register(
        &mut self,
        _tid: (),
        reg_id: <Self::Arch as gdbstub::arch::Arch>::RegId,
        val: &[u8],
    ) -> TargetResult<(), Self> {
        self.gdb_write_register(reg_id, val)
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
