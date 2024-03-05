use super::{AsU64, State, Watchpoint};
use crate::{
    runtime::{bus::Bus, memory::tlb::AccessMode, Environment, GdbIntegration, ValidRuntime},
    target::{Memory, RegisterStorage, Target},
};
use gdbstub::{
    outputln,
    target::{
        ext::{self as gdb_target, base as gdb_base},
        TargetResult,
    },
};

impl<T: Target, B: Bus> gdbstub::target::Target for Environment<'_, T, B>
where
    for<'a> Environment<'a, T, B>: ValidRuntime<T>,
{
    type Arch = <Self as GdbIntegration<T>>::Arch;
    type Error = &'static str;

    #[inline(always)]
    fn base_ops(&mut self) -> gdb_base::BaseOps<'_, Self::Arch, Self::Error> {
        gdb_base::BaseOps::SingleThread(self)
    }

    #[inline(always)]
    fn support_breakpoints(&mut self) -> Option<gdb_target::breakpoints::BreakpointsOps<'_, Self>> {
        Some(self)
    }

    #[inline(always)]
    fn support_monitor_cmd(&mut self) -> Option<gdb_target::monitor_cmd::MonitorCmdOps<'_, Self>> {
        Some(self)
    }
}

impl<T: Target, B: Bus> gdb_target::breakpoints::Breakpoints for Environment<'_, T, B>
where
    for<'a> Environment<'a, T, B>: ValidRuntime<T>,
{
    #[inline(always)]
    fn support_sw_breakpoint(
        &mut self,
    ) -> Option<gdb_target::breakpoints::SwBreakpointOps<'_, Self>> {
        Some(self)
    }

    #[inline(always)]
    fn support_hw_watchpoint(
        &mut self,
    ) -> Option<gdb_target::breakpoints::HwWatchpointOps<'_, Self>> {
        Some(self)
    }
}

impl<T: Target, B: Bus> gdb_target::breakpoints::SwBreakpoint for Environment<'_, T, B>
where
    for<'a> Environment<'a, T, B>: ValidRuntime<T>,
{
    fn add_sw_breakpoint(
        &mut self,
        vaddr: <Self::Arch as gdbstub::arch::Arch>::Usize,
        _kind: <Self::Arch as gdbstub::arch::Arch>::BreakpointKind,
    ) -> TargetResult<bool, Self> {
        let addr = self.to_runtime_address(vaddr.as_u64());
        Ok(self.debugger.as_mut().unwrap().breakpoints.insert(addr))
    }

    fn remove_sw_breakpoint(
        &mut self,
        vaddr: <Self::Arch as gdbstub::arch::Arch>::Usize,
        _kind: <Self::Arch as gdbstub::arch::Arch>::BreakpointKind,
    ) -> TargetResult<bool, Self> {
        let addr = &self.to_runtime_address(vaddr.as_u64());
        Ok(self.debugger.as_mut().unwrap().breakpoints.remove(addr))
    }
}

impl<T: Target, B: Bus> gdb_target::breakpoints::HwWatchpoint for Environment<'_, T, B>
where
    for<'a> Environment<'a, T, B>: ValidRuntime<T>,
{
    fn add_hw_watchpoint(
        &mut self,
        addr: <Self as GdbIntegration<T>>::Usize,
        len: <Self as GdbIntegration<T>>::Usize,
        kind: gdb_target::breakpoints::WatchKind,
    ) -> TargetResult<bool, Self> {
        let addr = self.to_runtime_address(addr.as_u64());
        let len = len.as_u64();
        let addresses = addr..addr + len;
        let watchpoint = Watchpoint { addresses, kind };
        self.debugger.as_mut().unwrap().watchpoints.push(watchpoint);
        Ok(true)
    }

    fn remove_hw_watchpoint(
        &mut self,
        addr: <Self as GdbIntegration<T>>::Usize,
        len: <Self as GdbIntegration<T>>::Usize,
        kind: gdb_target::breakpoints::WatchKind,
    ) -> TargetResult<bool, Self> {
        let addr = self.to_runtime_address(addr.as_u64());
        let len = len.as_u64();
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

impl<T: Target, B: Bus> gdb_base::singlethread::SingleThreadBase for Environment<'_, T, B>
where
    for<'a> Environment<'a, T, B>: ValidRuntime<T>,
{
    fn read_addrs(
        &mut self,
        start_addr: <Self as GdbIntegration<T>>::Usize,
        data: &mut [u8],
    ) -> TargetResult<usize, Self> {
        let start_addr = start_addr.as_u64();
        for i in (0..data.len()).step_by(4) {
            let word: u32 = {
                let vaddr = start_addr + i as u64;
                let paddr = self
                    .memory
                    .virtual_to_physical_address(vaddr, AccessMode::Read, &self.registers)
                    .map_err(|_| ())?;
                let ty = self.memory_type_for_address(vaddr);
                self.read(ty, vaddr, paddr).map_err(|_| ())?.into()
            };
            let end = data.len().min(i + 4);
            data[i..end].copy_from_slice(&word.to_be_bytes()[..end - i]);
        }
        Ok(data.len())
    }

    fn write_addrs(
        &mut self,
        start_addr: <Self as GdbIntegration<T>>::Usize,
        data: &[u8],
    ) -> TargetResult<(), Self> {
        let start_addr = start_addr.as_u64();
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
            let ty = self.memory_type_for_address(vaddr);
            self.write(ty, vaddr, paddr, value.into()).map_err(|_| ())?;
        }
        Ok(())
    }

    fn read_registers(
        &mut self,
        regs: &mut <Self::Arch as gdbstub::arch::Arch>::Registers,
    ) -> TargetResult<(), Self> {
        self.registers.copy_into(regs);
        Ok(())
    }

    fn write_registers(
        &mut self,
        regs: &<Self::Arch as gdbstub::arch::Arch>::Registers,
    ) -> TargetResult<(), Self> {
        regs.copy_into(&mut self.registers);
        Ok(())
    }

    #[inline(always)]
    fn support_single_register_access(
        &mut self,
    ) -> Option<gdb_base::single_register_access::SingleRegisterAccessOps<'_, (), Self>> {
        Some(self)
    }

    #[inline(always)]
    fn support_resume(
        &mut self,
    ) -> Option<gdb_base::singlethread::SingleThreadResumeOps<'_, Self>> {
        Some(self)
    }
}

impl<T: Target, B: Bus> gdb_base::single_register_access::SingleRegisterAccess<()>
    for Environment<'_, T, B>
where
    for<'a> Environment<'a, T, B>: ValidRuntime<T>,
{
    fn read_register(
        &mut self,
        _tid: (),
        reg_id: <T::Registers as RegisterStorage>::Id,
        buf: &mut [u8],
    ) -> TargetResult<usize, Self> {
        let reg = self.registers.read_register(reg_id).to_be_bytes();
        let len = buf.len().min(reg.len());
        buf[..len].copy_from_slice(&reg[..len]);
        Ok(len)
    }

    fn write_register(
        &mut self,
        _tid: (),
        reg_id: <T::Registers as RegisterStorage>::Id,
        val: &[u8],
    ) -> TargetResult<(), Self> {
        let value = {
            let mut to_write = [0_u8; 8];
            let len = val.len().min(to_write.len());
            to_write[..len].copy_from_slice(&val[..len]);
            u64::from_be_bytes(to_write)
        };
        self.registers.write_register(reg_id, value);
        Ok(())
    }
}

impl<T: Target, B: Bus> gdb_base::singlethread::SingleThreadResume for Environment<'_, T, B>
where
    for<'a> Environment<'a, T, B>: ValidRuntime<T>,
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
    ) -> Option<gdb_base::singlethread::SingleThreadSingleStepOps<'_, Self>> {
        Some(self)
    }
}

impl<T: Target, B: Bus> gdb_base::singlethread::SingleThreadSingleStep for Environment<'_, T, B>
where
    for<'a> Environment<'a, T, B>: ValidRuntime<T>,
{
    fn step(&mut self, signal: Option<gdbstub::common::Signal>) -> Result<(), Self::Error> {
        if let Some(signal) = signal {
            println!("gdb: step with signal {signal} is not supported");
        }
        self.debugger.as_mut().unwrap().state = State::SingleStep;
        Ok(())
    }
}

impl<T: Target, B: Bus> gdb_target::monitor_cmd::MonitorCmd for Environment<'_, T, B>
where
    for<'a> Environment<'a, T, B>: ValidRuntime<T>,
{
    fn handle_monitor_cmd(
        &mut self,
        cmd: &[u8],
        mut out: gdb_target::monitor_cmd::ConsoleOutput<'_>,
    ) -> Result<(), Self::Error> {
        // This is a closure so that we can early-return using `?`, without propagating the error.
        let _ = || -> Result<(), ()> {
            let cmd = std::str::from_utf8(cmd).map_err(|_| {
                outputln!(out, "monitor command is not valid UTF-8");
            })?;
            let mut iter = cmd.split_whitespace().peekable();

            // Juggle around the command to appease the borrow checker
            if let Some(name) = iter.next() {
                if let Some(mut cmd) = self
                    .debugger
                    .as_mut()
                    .unwrap()
                    .monitor_commands
                    .remove(name)
                {
                    cmd.handle(self, &mut out, &mut iter).unwrap_or_else(|err| {
                        outputln!(out, "{err}");
                    });

                    self.debugger
                        .as_mut()
                        .unwrap()
                        .monitor_commands
                        .insert(cmd.name(), cmd);
                } else {
                    outputln!(out, "unrecognized command: '{cmd}'");
                }
            } else {
                outputln!(out, "no command specified");
            };

            if iter.peek().is_some() {
                outputln!(out, "warning: ignoring extra arguments");
            }
            Ok(())
        }();
        Ok(())
    }
}
