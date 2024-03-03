use super::{registers::RegisterID, Cpu};
use crate::{
    gdb::MonitorCommand,
    runtime::{self, bus::Bus, Environment, GdbIntegration},
    RegIndex,
};
use mips_decomp::register;
use std::mem::size_of;

fn write_be_bytes(values: impl IntoIterator<Item = u64>, write_byte: &mut impl FnMut(Option<u8>)) {
    for byte in values.into_iter().flat_map(u64::to_be_bytes) {
        write_byte(Some(byte));
    }
}

impl gdbstub::arch::RegId for RegisterID {
    // See the architecture XML files:
    // https://github.com/bminor/binutils-gdb/blob/213516ef315dc1785e4990ef0fc011abedb38cc0/gdb/features/mips64-cpu.xml
    // https://github.com/bminor/binutils-gdb/blob/213516ef315dc1785e4990ef0fc011abedb38cc0/gdb/features/mips64-cp0.xml
    // https://github.com/bminor/binutils-gdb/blob/213516ef315dc1785e4990ef0fc011abedb38cc0/gdb/features/mips64-fpu.xml
    fn from_raw_id(id: usize) -> Option<(Self, Option<std::num::NonZeroUsize>)> {
        #[allow(clippy::cast_possible_truncation)]
        let reg = RegisterID(match id {
            0..=31 => register::cpu::GeneralPurpose::from_repr(id as u8)?.into(),
            32 => register::cpu::Cp0::Status.into(),
            33 => register::cpu::Special::Lo.into(),
            34 => register::cpu::Special::Hi.into(),
            35 => register::cpu::Cp0::BadVAddr.into(),
            36 => register::cpu::Cp0::Cause.into(),
            37 => register::cpu::Special::Pc.into(),
            38..=69 => register::cpu::Fpu::from_repr((id - 38) as u8)?.into(),
            70 => register::cpu::FpuControl::ControlStatus.into(),
            71 => register::cpu::FpuControl::ImplementationRevision.into(),
            _ => None?,
        });
        let size = size_of::<u64>().try_into().expect("8 bytes is more than 0");
        Some((reg, Some(size)))
    }
}

impl gdbstub::arch::Registers for super::Registers {
    type ProgramCounter = u64;

    fn pc(&self) -> Self::ProgramCounter {
        self.read(register::cpu::Special::Pc)
    }

    // The order we serialize these in is important, it must match the register IDs in ascending order.
    fn gdb_serialize(&self, mut write_byte: impl FnMut(Option<u8>)) {
        write_be_bytes(self.general_purpose.iter_relaxed(), &mut write_byte); // 0..=31
        write_be_bytes(
            [
                self.read(register::cpu::Cp0::Status),   // 32
                self.read(register::cpu::Special::Lo),   // 33
                self.read(register::cpu::Special::Hi),   // 34
                self.read(register::cpu::Cp0::BadVAddr), // 35
                self.read(register::cpu::Cp0::Cause),    // 36
                self.read(register::cpu::Special::Pc),   // 37
            ],
            &mut write_byte,
        );
        write_be_bytes(self.fpu.iter_relaxed(), &mut write_byte); // 38..=69
        write_be_bytes(
            [
                self.read(register::cpu::FpuControl::ControlStatus), // 70
                self.read(register::cpu::FpuControl::ImplementationRevision), // 71
            ],
            &mut write_byte,
        );
    }

    // The order we deserialize these in is important, it must match the register IDs in ascending order.
    fn gdb_deserialize(&mut self, bytes: &[u8]) -> Result<(), ()> {
        let mut iter = bytes
            .chunks_exact(size_of::<u64>())
            .map(|c| u64::from_be_bytes(c.try_into().expect("chunk is 8 bytes")));
        let mut next = || iter.next().ok_or(());

        // 0..=31
        for i in 0..=self.general_purpose.len() {
            self.general_purpose.write_relaxed(i, next()?).ok_or(())?;
        }

        self.write(register::cpu::Cp0::Status, next()?); // 32
        self.write(register::cpu::Special::Lo, next()?); // 33
        self.write(register::cpu::Special::Hi, next()?); // 34
        self.write(register::cpu::Cp0::BadVAddr, next()?); // 35
        self.write(register::cpu::Cp0::Cause, next()?); // 36
        self.write(register::cpu::Special::Pc, next()?); // 37

        // 38..=69
        for i in 0..=self.fpu.len() {
            self.fpu.write_relaxed(i, next()?).ok_or(())?;
        }

        self.write(register::cpu::FpuControl::ControlStatus, next()?); // 70
        self.write(register::cpu::FpuControl::ImplementationRevision, next()?); // 71

        Ok(())
    }
}

pub(crate) enum GdbArch {}

impl gdbstub::arch::Arch for GdbArch {
    type Usize = u64;
    type Registers = super::Registers;
    type BreakpointKind = runtime::gdb::BreakpointKind;
    type RegId = RegisterID;

    fn target_description_xml() -> Option<&'static str> {
        Some(r#"<target version="1.0"><architecture>mips:4300</architecture></target>"#)
    }
}

impl<B: Bus> GdbIntegration<Cpu> for Environment<'_, Cpu, B> {
    type Usize = u64;
    type Arch = GdbArch;

    fn extra_monitor_commands() -> Vec<MonitorCommand<Self>> {
        vec![
            MonitorCommand {
                name: "status",
                description: "print the coprocessor 0 status register",
                handler: Box::new(|env, out, _args| {
                    writeln!(out, "{:#?}", env.registers.status())?;
                    Ok(())
                }),
            },
            MonitorCommand {
                name: "cause",
                description: "print the coprocessor 0 cause register",
                handler: Box::new(|env, out, _args| {
                    writeln!(out, "{:#?}", env.registers.cause())?;
                    Ok(())
                }),
            },
            MonitorCommand {
                name: "fpu-status",
                description: "print the FPU control and status register",
                handler: Box::new(|env, out, _args| {
                    writeln!(out, "{:#?}", env.registers.fpu_control_status())?;
                    Ok(())
                }),
            },
            MonitorCommand {
                name: "tlb",
                description: "print every entry in the TLB",
                handler: Box::new(|env, out, _args| {
                    writeln!(out, "{:#?}", env.memory.tlb)?;
                    Ok(())
                }),
            },
        ]
    }
}
