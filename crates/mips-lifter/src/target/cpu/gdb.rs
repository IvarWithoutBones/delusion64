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

fn copy_reg(
    reg: impl Into<register::Register>,
    src: &super::Registers,
    dst: &mut super::Registers,
) {
    let reg = reg.into();
    dst.write(reg, src.read(reg));
}

impl gdbstub::arch::RegId for RegisterID {
    // See the architecture XML files:
    // https://github.com/bminor/binutils-gdb/blob/213516ef315dc1785e4990ef0fc011abedb38cc0/gdb/features/mips64-cpu.xml
    // https://github.com/bminor/binutils-gdb/blob/213516ef315dc1785e4990ef0fc011abedb38cc0/gdb/features/mips64-cp0.xml
    // https://github.com/bminor/binutils-gdb/blob/213516ef315dc1785e4990ef0fc011abedb38cc0/gdb/features/mips64-fpu.xml
    fn from_raw_id(id: usize) -> Option<(Self, Option<std::num::NonZeroUsize>)> {
        #[allow(clippy::cast_possible_truncation)]
        let reg = RegisterID(match id {
            0..=31 => register::GeneralPurpose::from_repr(id as u8)?.into(),
            32 => register::Cp0::Status.into(),
            33 => register::Special::Lo.into(),
            34 => register::Special::Hi.into(),
            35 => register::Cp0::BadVAddr.into(),
            36 => register::Cp0::Cause.into(),
            37 => register::Special::Pc.into(),
            38..=69 => register::Fpu::from_repr((id - 38) as u8)?.into(),
            70 => register::FpuControl::ControlStatus.into(),
            71 => register::FpuControl::ImplementationRevision.into(),
            _ => None?,
        });
        let size = size_of::<u64>().try_into().expect("8 bytes is more than 0");
        Some((reg, Some(size)))
    }
}

impl gdbstub::arch::Registers for super::Registers {
    type ProgramCounter = u64;

    fn pc(&self) -> Self::ProgramCounter {
        self.read(register::Special::Pc)
    }

    // The order we serialize these in is important, it must match the register IDs in ascending order.
    fn gdb_serialize(&self, mut write_byte: impl FnMut(Option<u8>)) {
        write_be_bytes(self.general_purpose.iter_relaxed(), &mut write_byte); // 0..=31
        write_be_bytes(
            [
                self.read(register::Cp0::Status),   // 32
                self.read(register::Special::Lo),   // 33
                self.read(register::Special::Hi),   // 34
                self.read(register::Cp0::BadVAddr), // 35
                self.read(register::Cp0::Cause),    // 36
                self.read(register::Special::Pc),   // 37
            ],
            &mut write_byte,
        );
        write_be_bytes(self.fpu.iter_relaxed(), &mut write_byte); // 38..=69
        write_be_bytes(
            [
                self.read(register::FpuControl::ControlStatus), // 70
                self.read(register::FpuControl::ImplementationRevision), // 71
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

        self.write(register::Cp0::Status, next()?); // 32
        self.write(register::Special::Lo, next()?); // 33
        self.write(register::Special::Hi, next()?); // 34
        self.write(register::Cp0::BadVAddr, next()?); // 35
        self.write(register::Cp0::Cause, next()?); // 36
        self.write(register::Special::Pc, next()?); // 37

        // 38..=69
        for i in 0..=self.fpu.len() {
            self.fpu.write_relaxed(i, next()?).ok_or(())?;
        }

        self.write(register::FpuControl::ControlStatus, next()?); // 70
        self.write(register::FpuControl::ImplementationRevision, next()?); // 71

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

impl<B: Bus> GdbIntegration for Environment<'_, Cpu, B> {
    type Arch = GdbArch;

    fn gdb_read_register(
        &mut self,
        reg_id: <<Self as GdbIntegration>::Arch as gdbstub::arch::Arch>::RegId,
        buf: &mut [u8],
    ) -> gdbstub::target::TargetResult<usize, Self> {
        let value = self.registers.read(reg_id.0).to_be_bytes();
        let len = buf.len().min(value.len());
        buf[..len].copy_from_slice(&value[..len]);
        Ok(len)
    }

    fn gdb_write_register(
        &mut self,
        reg_id: <<Self as GdbIntegration>::Arch as gdbstub::arch::Arch>::RegId,
        value: &[u8],
    ) -> gdbstub::target::TargetResult<(), Self> {
        let value: [u8; size_of::<u64>()] = value.try_into().expect("guaranteed to be reg size");
        self.registers.write(reg_id.0, u64::from_be_bytes(value));
        Ok(())
    }

    fn gdb_read_registers(
        &mut self,
        regs: &mut <<Self as gdbstub::target::Target>::Arch as gdbstub::arch::Arch>::Registers,
    ) -> gdbstub::target::TargetResult<(), Self> {
        copy_reg(register::Special::Pc, &self.registers, regs);
        copy_reg(register::Special::Hi, &self.registers, regs);
        copy_reg(register::Special::Lo, &self.registers, regs);
        copy_reg(register::Cp0::Cause, &self.registers, regs);
        copy_reg(register::Cp0::Status, &self.registers, regs);
        copy_reg(register::Cp0::BadVAddr, &self.registers, regs);

        for (i, r) in self.registers.general_purpose.iter_relaxed().enumerate() {
            regs.general_purpose.write_relaxed(i, r).unwrap();
        }

        for (i, r) in self.registers.fpu.iter_relaxed().enumerate() {
            regs.fpu.write_relaxed(i, r).unwrap();
        }

        Ok(())
    }

    fn gdb_write_registers(
        &mut self,
        regs: &<<Self as gdbstub::target::Target>::Arch as gdbstub::arch::Arch>::Registers,
    ) -> gdbstub::target::TargetResult<(), Self> {
        assert!(
            regs.read(register::Special::Pc) == self.registers.read(register::Special::Pc),
            "gdb: attempted to change PC"
        );

        copy_reg(register::Special::Hi, regs, &mut self.registers);
        copy_reg(register::Special::Lo, regs, &mut self.registers);
        copy_reg(register::Cp0::Cause, regs, &mut self.registers);
        copy_reg(register::Cp0::Status, regs, &mut self.registers);
        copy_reg(register::Cp0::BadVAddr, regs, &mut self.registers);
        for (i, r) in regs.general_purpose.iter_relaxed().enumerate() {
            self.registers.general_purpose.write_relaxed(i, r).unwrap();
        }
        for (i, r) in regs.fpu.iter_relaxed().enumerate() {
            self.registers.fpu.write_relaxed(i, r).unwrap();
        }
        Ok(())
    }

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
