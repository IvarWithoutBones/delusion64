use super::{memory::tlb::AccessMode, Environment, Memory};
use gdbstub::{
    conn::ConnectionExt,
    outputln,
    stub::{state_machine::GdbStubStateMachine, GdbStub, SingleThreadStopReason},
    target::{
        self,
        ext::{
            base::singlethread::{SingleThreadBase, SingleThreadResume, SingleThreadSingleStep},
            breakpoints::{Breakpoints, SwBreakpoint},
            monitor_cmd::MonitorCmd,
        },
        Target, TargetResult,
    },
};
use mips_decomp::register;
use std::{collections::HashSet, net::TcpStream, num::ParseIntError};

type Connection = Box<dyn ConnectionExt<Error = std::io::Error>>;

#[derive(Default)]
enum State {
    Running,
    #[default]
    Paused,
    SingleStep,
}

pub struct Debugger<'ctx, Mem>
where
    Mem: Memory,
{
    state: State,
    breakpoints: HashSet<u64>,
    state_machine: Option<GdbStubStateMachine<'ctx, Environment<'ctx, Mem>, Connection>>,
    stop_reason: Option<SingleThreadStopReason<u32>>,
}

impl<'ctx, Mem> Debugger<'ctx, Mem>
where
    Mem: Memory,
{
    pub fn new(env: &mut Environment<'ctx, Mem>, gdb_stream: TcpStream) -> Self {
        let conn: Connection = Box::new(gdb_stream);
        let state_machine = GdbStub::new(conn).run_state_machine(env).unwrap();

        Self {
            state: State::default(),
            breakpoints: HashSet::new(),
            stop_reason: None,
            state_machine: Some(state_machine),
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

impl<Mem> Environment<'_, Mem>
where
    Mem: Memory,
{
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

impl<Mem> Target for Environment<'_, Mem>
where
    Mem: Memory,
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
        Some(self)
    }

    // Workaround for https://github.com/daniel5151/gdbstub/issues/89
    #[inline(always)]
    fn guard_rail_single_step_gdb_behavior(&self) -> gdbstub::arch::SingleStepGdbBehavior {
        gdbstub::arch::SingleStepGdbBehavior::Optional
    }
}

impl<Mem> Breakpoints for Environment<'_, Mem>
where
    Mem: Memory,
{
    #[inline(always)]
    fn support_sw_breakpoint(
        &mut self,
    ) -> Option<target::ext::breakpoints::SwBreakpointOps<'_, Self>> {
        Some(self)
    }
}

impl<Mem> SwBreakpoint for Environment<'_, Mem>
where
    Mem: Memory,
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

impl<Mem> SingleThreadBase for Environment<'_, Mem>
where
    Mem: Memory,
{
    fn read_addrs(
        &mut self,
        start_addr: <Self::Arch as gdbstub::arch::Arch>::Usize,
        data: &mut [u8],
    ) -> TargetResult<(), Self> {
        for i in (0..data.len()).step_by(4) {
            let end = data.len().min(i + 4);
            let word = {
                let paddr = self
                    .tlb
                    .translate(
                        start_addr as u64 + i as u64,
                        AccessMode::Read,
                        &self.registers,
                    )
                    .map_err(|_| ())?;
                self.memory.read_u32(paddr).map_err(|_| ())?
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
                .translate(
                    start_addr as u64 + i as u64,
                    AccessMode::Write,
                    &self.registers,
                )
                .map_err(|_| ())?;
            let word = u32::from_ne_bytes(data[i..end].try_into().unwrap());
            self.memory.write_u32(paddr, word).map_err(|_| ())?;
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
        regs.cp0.cause = self.registers[register::Cp0::Cache] as _;
        regs.cp0.status = self.registers[register::Cp0::Status] as _;
        regs.cp0.badvaddr = self.registers[register::Cp0::BadVAddr] as _;
        for (i, r) in regs.r.iter_mut().enumerate() {
            *r = self.registers.general_purpose[i] as _;
        }
        for (i, r) in regs.fpu.r.iter_mut().enumerate() {
            *r = self.registers.fpu[i] as _;
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
            self.registers.general_purpose[i] = *r as _;
        }
        for (i, r) in regs.fpu.r.iter().enumerate() {
            self.registers.fpu[i] = *r as _;
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

impl<Mem> SingleThreadResume for Environment<'_, Mem>
where
    Mem: Memory,
{
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

impl<Mem> SingleThreadSingleStep for Environment<'_, Mem>
where
    Mem: Memory,
{
    fn step(&mut self, signal: Option<gdbstub::common::Signal>) -> Result<(), Self::Error> {
        assert!(signal.is_none(), "gdb: step with signal is not supported");
        self.debugger.as_mut().unwrap().state = State::SingleStep;
        Ok(())
    }
}

impl<Mem> MonitorCmd for Environment<'_, Mem>
where
    Mem: Memory,
{
    fn handle_monitor_cmd(
        &mut self,
        cmd: &[u8],
        mut out: target::ext::monitor_cmd::ConsoleOutput<'_>,
    ) -> Result<(), Self::Error> {
        // This is a closure so that we can early-return using `?`, without propagating the error.
        let _ = || -> Result<(), ()> {
            let cmd = std::str::from_utf8(cmd).map_err(|_| {
                outputln!(out, "monitor command is not valid UTF-8");
            })?;

            // Could be much more elegant, but this works fine for now.
            let mut iter = cmd.split_whitespace().peekable();
            match iter.next().unwrap_or("") {
                "registers" | "regs" | "r" => outputln!(out, "{:?}", self.registers),

                "physical_address" | "paddr" => {
                    let vaddr = str_to_u64(
                        iter.next()
                            .ok_or_else(|| outputln!(out, "expected virtual address"))?,
                    )
                    .map_err(|e| outputln!(out, "failed to parse virtual address: {e}"))?;

                    let paddr = self
                        .tlb
                        .translate(vaddr, AccessMode::Read, &self.registers)
                        .map_err(|e| {
                            outputln!(out, "virtual address {vaddr:#x} is not mapped: {e:#?}");
                        })?;
                    outputln!(out, "{paddr:#x}");
                }

                _ => outputln!(out, "unrecognized command: '{cmd}'"),
            };

            if iter.peek().is_some() {
                outputln!(out, "warning: ignoring extra arguments");
            }

            Ok(())
        }();
        Ok(())
    }
}

fn str_to_u64(mut str: &str) -> Result<u64, ParseIntError> {
    const HEX_PREFIX: &str = "0x";
    let radix = if str.starts_with(HEX_PREFIX) {
        str = &str[HEX_PREFIX.len()..];
        16
    } else {
        10
    };
    // Read as a signed integer to support negative numbers
    Ok(i64::from_str_radix(str, radix)? as u64)
}
