#![allow(private_bounds)] // TODO: Fix this somehow?

use crate::{
    gdb, run,
    runtime::{self, bus::Bus},
    target::{Cpu, Target},
};

pub struct JitBuilder<const READY: bool, T: Target, B: Bus>
where
    for<'a> runtime::Environment<'a, T, B>: runtime::ValidRuntime,
{
    pub(crate) bus: B,
    pub(crate) registers: Option<T::Registers>,
    pub(crate) gdb: Option<gdb::Connection<B>>,
    pub(crate) trace: bool,
}

impl<B: Bus> JitBuilder<false, Cpu, B> {
    pub fn new_cpu(bus: B) -> Self {
        Self {
            bus,
            registers: None,
            gdb: None,
            trace: false,
        }
    }
}

impl<const READY: bool, T: Target, B: Bus> JitBuilder<READY, T, B>
where
    for<'a> runtime::Environment<'a, T, B>: runtime::ValidRuntime,
{
    pub fn maybe_with_trace(self, trace: Option<bool>) -> Self {
        let trace = trace.unwrap_or(self.trace);
        Self { trace, ..self }
    }

    pub fn with_trace(self, trace: bool) -> Self {
        self.maybe_with_trace(Some(trace))
    }

    pub fn maybe_with_gdb(self, gdb: Option<gdb::Connection<B>>) -> Self {
        Self { gdb, ..self }
    }

    pub fn with_gdb(self, gdb: gdb::Connection<B>) -> Self {
        self.maybe_with_gdb(Some(gdb))
    }
}

impl<B: Bus> JitBuilder<false, Cpu, B> {
    pub fn with_cpu_registers(self, registers: crate::Registers) -> JitBuilder<true, Cpu, B> {
        JitBuilder {
            registers: Some(registers),
            trace: self.trace,
            bus: self.bus,
            gdb: self.gdb,
        }
    }
}

impl<T: Target, B: Bus> JitBuilder<true, T, B>
where
    for<'a> runtime::Environment<'a, T, B>: runtime::ValidRuntime,
{
    pub fn run(self) -> B {
        run(self)
    }

    pub(crate) fn registers(&mut self) -> T::Registers {
        self.registers
            .take()
            .expect("attempted to use registers after they were taken")
    }
}
