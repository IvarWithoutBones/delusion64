use crate::{runtime::register_bank::RegisterBankMapping, target, RegIndex, RegisterBank};
use mips_decomp::register::rsp as register;
use std::fmt;

#[derive(Default)]
pub struct Registers {
    pub general_purpose: RegisterBank<u32, { register::GeneralPurpose::count() }>,
    pub control: RegisterBank<u32, { register::Control::count() }>,
    /// In reality these are 128-bit registers, but since Rust does not have an AtomicU128 type, RegisterBank cannot be implemented for it.
    /// If you need to access the full 128-bit value, use [`RegIndex`]'s `read` and `write` methods.
    pub vector: RegisterBank<u64, { register::Vector::count() * 2 }>,
    pub special: RegisterBank<u64, { register::Special::count() }>,
}

impl Registers {
    #[must_use]
    pub fn status(&self) -> register::control::StatusRead {
        self.read(register::Control::Status).into()
    }

    pub fn set_status(&mut self, status: register::control::StatusRead) {
        // When writing with values we choose ourselves, no need for `StatusWrite`.
        let new = self.status().raw() | status.raw();
        self.write(register::Control::Status, new);
    }

    #[allow(clippy::cast_possible_truncation)]
    pub fn increment_pc(&mut self, amount: u32) {
        let mut pc: register::special::ProgramCounter =
            (self.read(register::Special::ProgramCounter) as u32).into();
        pc.increment(amount);
        self.write(register::Special::ProgramCounter, pc.read().into());
    }
}

impl_reg_index!(
    Registers,
    (register::GeneralPurpose, u32, general_purpose),
    (register::Control, u32, control),
    (register::Special, u64, special)
);

impl RegIndex<register::Vector> for Registers {
    type Output = u128;

    #[allow(clippy::similar_names)]
    fn read(&self, index: register::Vector) -> Self::Output {
        // Combine the two halves into a single 128-bit value
        let base = (index.to_repr() * 2) & !0b1;
        let lsb = self.vector.read_relaxed(base).unwrap();
        let msb = self.vector.read_relaxed(base + 1).unwrap();
        (u128::from(msb) << 64) | u128::from(lsb)
    }

    #[allow(clippy::cast_possible_truncation, clippy::similar_names)]
    fn write(&mut self, index: register::Vector, value: Self::Output) {
        // Split the 128-bit value into two halves
        let base = (index.to_repr() * 2) & !0b1;
        let lsb = value as u64;
        let msb = (value >> 64) as u64;
        self.vector.write_relaxed(base, lsb).unwrap();
        self.vector.write_relaxed(base + 1, msb).unwrap();
    }
}

impl RegIndex<register::Register> for Registers {
    type Output = u128;

    fn read(&self, index: register::Register) -> Self::Output {
        match index {
            register::Register::GeneralPurpose(r) => u128::from(self.read(r)),
            register::Register::Control(r) => u128::from(self.read(r)),
            register::Register::Vector(r) => self.read(r),
            register::Register::Special(r) => u128::from(self.read(r)),
        }
    }

    #[allow(clippy::cast_possible_truncation)]
    fn write(&mut self, index: register::Register, value: Self::Output) {
        match index {
            register::Register::GeneralPurpose(r) => {
                self.write(r, value.try_into().expect("value too large"));
            }
            register::Register::Control(r) => {
                self.write(r, value.try_into().expect("value too large"));
            }
            register::Register::Vector(r) => self.write(r, value),
            register::Register::Special(r) => {
                self.write(r, value.try_into().expect("value too large"));
            }
        }
    }
}

impl<T> From<T> for Registers
where
    T: IntoIterator<Item = (register::Register, u64)>,
{
    fn from(value: T) -> Self {
        let mut registers = Self::default();
        for (reg, val) in value {
            registers.write(reg, val.into());
        }
        registers
    }
}

impl fmt::Debug for Registers {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let write_reg = |f: &mut fmt::Formatter<'_>, name: &str, reg: u128| -> fmt::Result {
            writeln!(f, "{name: <10} = {reg:#x}")
        };

        writeln!(f, "\ngeneral purpose registers:")?;
        for reg in register::GeneralPurpose::iter() {
            write_reg(f, reg.name(), self.read(reg).into())?;
        }

        writeln!(f, "\ncontrol registers:")?;
        for reg in register::Control::iter() {
            write_reg(f, reg.name(), self.read(reg).into())?;
        }

        writeln!(f, "\nvector registers:")?;
        for reg in register::Vector::iter() {
            write_reg(f, reg.name(), self.read(reg))?;
        }

        writeln!(f, "\nspecial registers:")?;
        for reg in register::Special::iter() {
            write_reg(f, reg.name(), self.read(reg).into())?;
        }

        Ok(())
    }
}

impl target::RegisterStorage for Registers {
    type Globals<'ctx> = RegisterGlobals<'ctx>;

    fn read_program_counter(&self) -> u64 {
        self.read(register::Special::ProgramCounter)
    }

    fn build_globals<'ctx>(
        &self,
        module: &inkwell::module::Module<'ctx>,
        exec_engine: &inkwell::execution_engine::ExecutionEngine<'ctx>,
    ) -> Self::Globals<'ctx> {
        RegisterGlobals {
            general_purpose: self
                .general_purpose
                .map_into(module, exec_engine, "general_purpose"),
            control: self.control.map_into(module, exec_engine, "control"),
            vector: self.vector.map_into(module, exec_engine, "vector"),
            special: self.special.map_into(module, exec_engine, "special"),
        }
    }
}

#[derive(Debug)]
pub(crate) struct RegisterGlobals<'ctx> {
    general_purpose: RegisterBankMapping<'ctx>,
    control: RegisterBankMapping<'ctx>,
    vector: RegisterBankMapping<'ctx>,
    special: RegisterBankMapping<'ctx>,
}

impl<'ctx> target::Globals<'ctx> for RegisterGlobals<'ctx> {
    type RegisterID = register::Register;

    const PROGRAM_COUNTER_ID: Self::RegisterID =
        register::Register::Special(register::Special::ProgramCounter);

    fn pointer_value<T: target::Target>(
        &self,
        codegen: &crate::codegen::CodeGen<'ctx, T>,
        index: &Self::RegisterID,
    ) -> inkwell::values::PointerValue<'ctx> {
        let i32_type = codegen.context.i32_type();
        let i64_type = codegen.context.i64_type();
        match index {
            register::Register::GeneralPurpose(_) => unsafe {
                codegen.builder.build_in_bounds_gep(
                    i32_type,
                    self.general_purpose.pointer_value(),
                    &[i32_type.const_int(index.to_repr() as u64, false)],
                    &format!("{index:?}_"),
                )
            },
            register::Register::Control(_) => unsafe {
                codegen.builder.build_in_bounds_gep(
                    i32_type,
                    self.control.pointer_value(),
                    &[i32_type.const_int(index.to_repr() as u64, false)],
                    &format!("{index:?}_"),
                )
            },
            register::Register::Vector(_) => unsafe {
                codegen.builder.build_in_bounds_gep(
                    i64_type,
                    self.vector.pointer_value(),
                    &[i64_type.const_int(index.to_repr() as u64, false)],
                    &format!("{index:?}_"),
                )
            },
            register::Register::Special(_) => unsafe {
                codegen.builder.build_in_bounds_gep(
                    i64_type,
                    self.special.pointer_value(),
                    &[i64_type.const_int(index.to_repr() as u64, false)],
                    &format!("{index:?}_"),
                )
            },
        }
    }

    fn is_atomic(&self, reg: Self::RegisterID) -> bool {
        match reg {
            register::Register::GeneralPurpose(_) => self.general_purpose.is_atomic(),
            register::Register::Control(_) => self.control.is_atomic(),
            register::Register::Vector(_) => self.vector.is_atomic(),
            register::Register::Special(_) => self.special.is_atomic(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn reg_index_vector() {
        const DUMMY: u128 = 0x1234_5678_90ab_cdef_1234_5678_90ab_cdef;

        let mut registers = Registers::default();
        registers.write(register::Vector::Vpr0, DUMMY);
        assert_eq!(registers.read(register::Vector::Vpr0), DUMMY);
        assert_eq!(registers.read(register::Vector::Vpr1), 0);

        registers.write(register::Vector::Vpr15, DUMMY);
        assert_eq!(registers.read(register::Vector::Vpr14), 0);
        assert_eq!(registers.read(register::Vector::Vpr15), DUMMY);
        assert_eq!(registers.read(register::Vector::Vpr16), 0);

        registers.write(register::Vector::Vpr31, DUMMY);
        assert_eq!(registers.read(register::Vector::Vpr30), 0);
        assert_eq!(registers.read(register::Vector::Vpr31), DUMMY);
    }
}
