//! The register bank definitions for a MIPS VR4300 CPU, as well as its JIT mapping.

use super::codegen::RESERVED_CP0_REGISTER_LATCH;
use crate::{
    codegen::CodeGen,
    runtime::register_bank::{RegIndex, RegisterBankMapping},
    target, RegisterBank,
};
use inkwell::{execution_engine::ExecutionEngine, module::Module, values::PointerValue};
use mips_decomp::register;
use std::fmt;

/// A single MIPS VR4300 register.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(transparent)]
pub(crate) struct RegisterID(pub register::cpu::Register);

impl target::RegisterID for RegisterID {
    const PROGRAM_COUNTER: Self =
        Self(register::cpu::Register::Special(register::cpu::Special::Pc));

    fn name(&self) -> &'static str {
        self.0.name()
    }
}

impl<T> From<T> for RegisterID
where
    T: Into<register::cpu::Register>,
{
    fn from(value: T) -> Self {
        Self(value.into())
    }
}

/// The standard MIPS VR4300 registers.
#[derive(Default, Clone, PartialEq)]
pub struct Registers {
    pub general_purpose: RegisterBank<u64, { register::cpu::GeneralPurpose::count() }>,
    pub cp0: RegisterBank<u64, { register::cpu::Cp0::count() }>,
    pub fpu: RegisterBank<u64, { register::cpu::Fpu::count() }>,
    pub fpu_control: RegisterBank<u64, { register::cpu::FpuControl::count() }>,
    pub special: RegisterBank<u64, { register::cpu::Special::count() }>,
}

impl Registers {
    #[allow(clippy::cast_possible_truncation)] // The relevant part is in the lower 32 bits.
    pub(crate) fn status(&self) -> register::cpu::cp0::Status {
        let value = self.read(register::cpu::Cp0::Status) as u32;
        register::cpu::cp0::Status::new(value)
    }

    #[allow(clippy::cast_possible_truncation)] // The relevant part is in the lower 32 bits.
    pub(crate) fn cause(&self) -> register::cpu::cp0::Cause {
        let value = self.read(register::cpu::Cp0::Cause) as u32;
        register::cpu::cp0::Cause::new(value)
    }

    pub(crate) fn set_cause(&mut self, cause: register::cpu::cp0::Cause) {
        let raw: u32 = cause.into();
        self.write(register::cpu::Cp0::Cause, u64::from(raw));
    }

    #[allow(clippy::cast_possible_truncation)] // The relevant part is in the lower 32 bits.
    pub(crate) fn fpu_control_status(&self) -> register::cpu::fpu::ControlStatus {
        let value = self.read(register::cpu::FpuControl::ControlStatus) as u32;
        register::cpu::fpu::ControlStatus::new(value)
    }

    #[allow(clippy::cast_possible_truncation)] // The relevant part is in the lower 32 bits.
    pub(crate) fn page_mask(&self) -> register::cpu::cp0::PageMask {
        let value = self.read(register::cpu::Cp0::PageMask) as u32;
        register::cpu::cp0::PageMask::new(value)
    }

    pub(crate) fn context(&self) -> register::cpu::cp0::Context {
        let value = self.read(register::cpu::Cp0::Context);
        register::cpu::cp0::Context::new(value)
    }

    pub(crate) fn xcontext(&self) -> register::cpu::cp0::XContext {
        let value = self.read(register::cpu::Cp0::XContext);
        register::cpu::cp0::XContext::new(value)
    }

    pub(crate) fn trigger_interrupt(&self) -> bool {
        let status = self.status();
        let cause = self.cause();
        status.interrupts_enabled()
            && !status.exception_level()
            && !status.error_level()
            && cause
                .interrupt_pending()
                .check_mask(status.interrupt_mask())
    }
}

impl<T> From<T> for Registers
where
    T: IntoIterator<Item = (register::cpu::Register, u64)>,
{
    fn from(value: T) -> Self {
        let mut registers = Self::default();
        for (reg, val) in value {
            registers.write(reg, val);
        }
        registers
    }
}

impl_reg_index!(
    Registers,
    (register::cpu::GeneralPurpose, u64, general_purpose),
    (register::cpu::Cp0, u64, cp0),
    (register::cpu::Fpu, u64, fpu),
    (register::cpu::FpuControl, u64, fpu_control),
    (register::cpu::Special, u64, special)
);

impl RegIndex<register::cpu::Register> for Registers {
    type Output = u64;

    fn read(&self, index: register::cpu::Register) -> Self::Output {
        match index {
            register::cpu::Register::GeneralPurpose(r) => self.read(r),
            register::cpu::Register::Fpu(r) => self.read(r),
            register::cpu::Register::FpuControl(r) => self.read(r),
            register::cpu::Register::Special(r) => self.read(r),
            register::cpu::Register::Cp0(r) => self.read(r),
        }
    }

    fn write(&mut self, index: register::cpu::Register, value: Self::Output) {
        match index {
            register::cpu::Register::GeneralPurpose(r) => self.write(r, value),
            register::cpu::Register::Fpu(r) => self.write(r, value),
            register::cpu::Register::FpuControl(r) => self.write(r, value),
            register::cpu::Register::Special(r) => self.write(r, value),
            register::cpu::Register::Cp0(r) => self.write(r, value),
        }
    }
}

impl fmt::Debug for Registers {
    // The iterators will return less than 256 items
    #[allow(clippy::cast_possible_truncation)]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let write_reg = |f: &mut fmt::Formatter<'_>, name: &str, reg: u64| -> fmt::Result {
            writeln!(f, "{name: <10} = {reg:#x}")
        };

        writeln!(f, "\ngeneral purpose registers:")?;
        for reg in register::cpu::GeneralPurpose::iter() {
            write_reg(f, reg.name(), self.read(reg))?;
        }

        writeln!(f, "\ncoprocessor 0 registers:")?;
        for reg in register::cpu::Cp0::iter().filter(|r| !r.is_reserved()) {
            write_reg(f, reg.name(), self.read(reg))?;
        }
        write_reg(f, "Reserved", self.read(RESERVED_CP0_REGISTER_LATCH))?;

        writeln!(f, "\nfpu general purpose registers:")?;
        for reg in register::cpu::Fpu::iter() {
            write_reg(f, reg.name(), self.read(reg))?;
        }

        writeln!(f, "\nfpu control registers:")?;
        for reg in register::cpu::FpuControl::iter().filter(|r| !r.is_reserved()) {
            write_reg(f, reg.name(), self.read(reg))?;
        }

        writeln!(f, "\nspecial registers:")?;
        for reg in register::cpu::Special::iter() {
            write_reg(f, reg.name(), self.read(reg))?;
        }

        Ok(())
    }
}

impl target::RegisterStorage for Registers {
    type RegisterID = RegisterID;
    type Globals<'ctx> = Globals<'ctx>;

    fn read_program_counter(&self) -> u64 {
        self.read(register::cpu::Special::Pc)
    }

    fn build_globals<'ctx>(
        &self,
        module: &Module<'ctx>,
        exec: &ExecutionEngine<'ctx>,
    ) -> Self::Globals<'ctx> {
        Globals {
            cp0: self.cp0.map_into(module, exec, "coprocessor_0_registers"),
            fpu: self.fpu.map_into(module, exec, "floating_point_registers"),
            special: self.special.map_into(module, exec, "special_registers"),
            general_purpose: self.general_purpose.map_into(
                module,
                exec,
                "general_purpose_registers",
            ),
            fpu_control: self.fpu_control.map_into(
                module,
                exec,
                "floating_point_control_registers",
            ),
        }
    }
}

/// Mappings to the register banks for the JIT.
#[derive(Debug)]
pub struct Globals<'ctx> {
    pub general_purpose: RegisterBankMapping<'ctx>,
    pub cp0: RegisterBankMapping<'ctx>,
    pub special: RegisterBankMapping<'ctx>,
    pub fpu: RegisterBankMapping<'ctx>,
    pub fpu_control: RegisterBankMapping<'ctx>,
}

impl<'ctx> target::Globals<'ctx> for Globals<'ctx> {
    type RegisterID = RegisterID;

    fn pointer_value<T: target::Target>(
        &self,
        codegen: &CodeGen<'ctx, T>,
        reg: &Self::RegisterID,
    ) -> PointerValue<'ctx> {
        let gep = |ptr| unsafe {
            let i64_type = codegen.context.i64_type();
            let name = &format!("{}_", reg.0.name());
            codegen
                .builder
                .build_in_bounds_gep(
                    i64_type,
                    ptr,
                    &[i64_type.const_int(reg.0.to_repr() as u64, false)],
                    name,
                )
                .expect("failed to build GEP")
        };
        match reg.0 {
            register::cpu::Register::Cp0(_) => gep(self.cp0.pointer_value()),
            register::cpu::Register::Special(_) => gep(self.special.pointer_value()),
            register::cpu::Register::Fpu(_) => gep(self.fpu.pointer_value()),
            register::cpu::Register::FpuControl(_) => gep(self.fpu_control.pointer_value()),
            register::cpu::Register::GeneralPurpose(_) => gep(self.general_purpose.pointer_value()),
        }
    }

    fn is_atomic(&self, reg: Self::RegisterID) -> bool {
        // TODO: This needs to hold a specific register, should we add a associated Bank type to avoid that?
        match reg.0 {
            register::cpu::Register::Cp0(_) => self.cp0.is_atomic(),
            register::cpu::Register::GeneralPurpose(_) => self.general_purpose.is_atomic(),
            register::cpu::Register::Special(_) => self.special.is_atomic(),
            register::cpu::Register::Fpu(_) => self.fpu.is_atomic(),
            register::cpu::Register::FpuControl(_) => self.fpu_control.is_atomic(),
        }
    }
}
