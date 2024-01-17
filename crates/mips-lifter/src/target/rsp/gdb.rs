use super::Rsp;
use crate::{
    runtime::{bus::Bus, Environment, GdbIntegration},
    target,
};

impl<B: Bus> GdbIntegration for Environment<'_, Rsp, B> {
    // TODO: This is inaccurate, the RSP has different registers.
    type Arch = target::cpu::gdb::GdbArch;

    fn gdb_read_register(
        &mut self,
        _reg_id: <<Self as GdbIntegration>::Arch as gdbstub::arch::Arch>::RegId,
        _buf: &mut [u8],
    ) -> gdbstub::target::TargetResult<usize, Self> {
        todo!("RSP GDB support")
    }

    fn gdb_write_register(
        &mut self,
        _reg_id: <<Self as GdbIntegration>::Arch as gdbstub::arch::Arch>::RegId,
        _value: &[u8],
    ) -> gdbstub::target::TargetResult<(), Self> {
        todo!("RSP GDB support")
    }

    fn gdb_read_registers(
        &mut self,
        _regs: &mut <<Self as gdbstub::target::Target>::Arch as gdbstub::arch::Arch>::Registers,
    ) -> gdbstub::target::TargetResult<(), Self> {
        todo!("RSP GDB support")
    }

    fn gdb_write_registers(
        &mut self,
        _regs: &<<Self as gdbstub::target::Target>::Arch as gdbstub::arch::Arch>::Registers,
    ) -> gdbstub::target::TargetResult<(), Self> {
        todo!("RSP GDB support")
    }
}
