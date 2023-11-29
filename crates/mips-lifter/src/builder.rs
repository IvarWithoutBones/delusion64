use crate::{gdb, run, runtime};
use mips_decomp::register;

pub type InitialRegisters<'a> = &'a [(register::Register, u64)];

pub struct JitBuilder<'a, const READY: bool, Bus: runtime::bus::Bus> {
    pub(crate) bus: Bus,
    pub(crate) registers: Option<InitialRegisters<'a>>,
    pub(crate) gdb: Option<gdb::Connection<Bus>>,
    pub(crate) trace: bool,
}

impl<'a, Bus> JitBuilder<'a, false, Bus>
where
    Bus: runtime::bus::Bus,
{
    pub fn new(bus: Bus) -> Self {
        Self {
            bus,
            registers: None,
            gdb: None,
            trace: false,
        }
    }
}

impl<'a, const READY: bool, Bus> JitBuilder<'a, READY, Bus>
where
    Bus: runtime::bus::Bus,
{
    pub fn maybe_with_trace(self, trace: Option<bool>) -> Self {
        let trace = trace.unwrap_or(self.trace);
        Self { trace, ..self }
    }

    pub fn with_trace(self, trace: bool) -> Self {
        self.maybe_with_trace(Some(trace))
    }

    pub fn maybe_with_gdb(self, gdb: Option<gdb::Connection<Bus>>) -> Self {
        Self { gdb, ..self }
    }

    pub fn with_gdb(self, gdb: gdb::Connection<Bus>) -> Self {
        self.maybe_with_gdb(Some(gdb))
    }
}

impl<'a, Bus: runtime::bus::Bus> JitBuilder<'a, false, Bus> {
    pub fn with_registers(self, registers: InitialRegisters<'a>) -> JitBuilder<'a, true, Bus> {
        JitBuilder {
            registers: Some(registers),
            trace: self.trace,
            bus: self.bus,
            gdb: self.gdb,
        }
    }
}

impl<'a, Bus: runtime::bus::Bus> JitBuilder<'a, true, Bus> {
    pub fn run(self) -> Bus {
        run(self)
    }

    pub(crate) fn registers(&self) -> InitialRegisters<'a> {
        // SAFETY: the const generics ensure this is always Some
        unsafe { self.registers.unwrap_unchecked() }
    }
}
