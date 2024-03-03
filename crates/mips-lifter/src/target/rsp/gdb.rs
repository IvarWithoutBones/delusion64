use super::{registers::RegisterID, Registers, Rsp};
use crate::{
    runtime::{bus::Bus, gdb, memory, Environment, GdbIntegration},
    target::{rsp::register, RegisterStorage},
    RegIndex,
};
use std::mem::size_of;

fn write_be_bytes(values: impl IntoIterator<Item = u32>, write_byte: &mut impl FnMut(Option<u8>)) {
    for byte in values.into_iter().flat_map(u32::to_be_bytes) {
        write_byte(Some(byte));
    }
}

impl gdbstub::arch::RegId for RegisterID {
    fn from_raw_id(id: usize) -> Option<(Self, Option<std::num::NonZeroUsize>)> {
        let id: u8 = id.try_into().ok()?;
        let reg = RegisterID(match id {
            0..=31 => register::GeneralPurpose::from_repr(id)?.into(),
            // GDB doesn't understand the remaining registers, and will misinterpret them if we expose them.
            _ => return None,
        });
        let size = size_of::<u32>().try_into().ok()?;
        Some((reg, Some(size)))
    }
}

/// This is a hack to work around the fact that GDB cannot distinguish between `IMEM`/`DMEM`.
/// Both banks map into the same addresses 0x0000..0x1000, but their data does not overlap.
/// We do need to distinguish between them however, as GDB must fetch instructions from `IMEM` and data from `DMEM`.
///
/// We can use a bit of a hack though:
/// Considering that GDB should only access `IMEM` for instruction fetches, and the program counter is only 12 bits wide,
/// we can give GDB a fake program counter with metadata inside of the upper bits. We control the only source of these values!
///
/// If the metadata bit is set we can assume GDB is trying to fetch instructions from `IMEM`, as no guest addresses can be in that range.
/// This has the unfortunate side effect of making GDB think we're executing code at 0x1000..0x2000 instead of 0x0000..0x1000.
const IMEM_BIT_IN_PC: u32 = 1 << 12;

impl gdbstub::arch::Registers for Registers {
    type ProgramCounter = u32;

    fn pc(&self) -> Self::ProgramCounter {
        let value: u32 = self
            .read_program_counter()
            .try_into()
            .expect("invalid program counter");
        value | IMEM_BIT_IN_PC
    }

    fn gdb_serialize(&self, mut write_byte: impl FnMut(Option<u8>)) {
        write_be_bytes(self.general_purpose.iter_relaxed(), &mut write_byte); // 0..=31
        write_be_bytes(
            [
                0,         // 32, unsupported
                0,         // 33, unsupported
                0,         // 34, unsupported
                0,         // 35, unsupported
                0,         // 36, unsupported
                self.pc(), // 37
            ],
            &mut write_byte,
        );
    }

    fn gdb_deserialize(&mut self, bytes: &[u8]) -> Result<(), ()> {
        let mut iter = bytes
            .chunks_exact(size_of::<u32>())
            .map(|c| u32::from_be_bytes(c.try_into().expect("chunk is 4 bytes")));
        let mut next = || iter.next().ok_or(());

        for i in 0..=self.general_purpose.len() {
            self.general_purpose.write_relaxed(i, next()?).ok_or(())?; // 0..=31
        }
        for _ in 32..=36 {
            next()?; // 32..=36, unsupported
        }
        self.write(register::Special::ProgramCounter, next()?.into());

        Ok(())
    }
}

pub(crate) enum GdbArch {}

impl gdbstub::arch::Arch for GdbArch {
    type Usize = u32;
    type Registers = Registers;
    type BreakpointKind = gdb::BreakpointKind;
    type RegId = RegisterID;

    fn target_description_xml() -> Option<&'static str> {
        Some(r#"<target version="1.0"><architecture>mips</architecture></target>"#)
    }
}

impl<B: Bus> GdbIntegration<Rsp> for Environment<'_, Rsp, B> {
    type Arch = GdbArch;
    type Usize = u32;

    fn to_runtime_address(&mut self, address: u64) -> u64 {
        // This address will be used for e.g. breakpoints where GDB supplies the address,
        // so we need to mask off our metadata. See `IMEM_BIT_IN_PC` for details.
        address & u64::from(!IMEM_BIT_IN_PC)
    }

    fn memory_type_for_address(&self, address: u64) -> memory::Type {
        // See `IMEM_BIT_IN_PC` for details.
        if address & u64::from(IMEM_BIT_IN_PC) != 0 {
            memory::Type::Instruction
        } else {
            memory::Type::Unknown
        }
    }
}
