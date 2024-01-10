//! The register bank definitions for a MIPS VR4300 CPU, as well as its JIT mapping.

use crate::{
    codegen::{CodeGen, INSIDE_DELAY_SLOT_STORAGE, RESERVED_CP0_REGISTER_LATCH},
    runtime::registers::RegIndex,
    target, RegisterBank,
};
use inkwell::{
    execution_engine::ExecutionEngine,
    module::Module,
    values::{GlobalValue, PointerValue},
};
use mips_decomp::register;
use std::fmt;

#[derive(Default)]
pub struct Registers {
    pub general_purpose: RegisterBank<u64, { register::GeneralPurpose::count() }>,
    pub cp0: RegisterBank<u64, { register::Cp0::count() }>,
    pub fpu: RegisterBank<u64, { register::Fpu::count() }>,
    pub fpu_control: RegisterBank<u64, { register::FpuControl::count() }>,
    pub special: RegisterBank<u64, { register::Special::count() }>,
}

impl Registers {
    #[allow(clippy::cast_possible_truncation)] // The relevant part is in the lower 32 bits.
    pub(crate) fn status(&self) -> register::cp0::Status {
        let value = self.read(register::Cp0::Status) as u32;
        register::cp0::Status::new(value)
    }

    #[allow(clippy::cast_possible_truncation)] // The relevant part is in the lower 32 bits.
    pub(crate) fn cause(&self) -> register::cp0::Cause {
        let value = self.read(register::Cp0::Cause) as u32;
        register::cp0::Cause::new(value)
    }

    pub(crate) fn set_cause(&mut self, cause: register::cp0::Cause) {
        let raw: u32 = cause.into();
        self.write(register::Cp0::Cause, u64::from(raw));
    }

    #[allow(clippy::cast_possible_truncation)] // The relevant part is in the lower 32 bits.
    pub(crate) fn fpu_control_status(&self) -> register::fpu::ControlStatus {
        let value = self.read(register::FpuControl::ControlStatus) as u32;
        register::fpu::ControlStatus::new(value)
    }

    #[allow(clippy::cast_possible_truncation)] // The relevant part is in the lower 32 bits.
    pub(crate) fn page_mask(&self) -> register::cp0::PageMask {
        let value = self.read(register::Cp0::PageMask) as u32;
        register::cp0::PageMask::new(value)
    }

    pub(crate) fn context(&self) -> register::cp0::Context {
        let value = self.read(register::Cp0::Context);
        register::cp0::Context::new(value)
    }

    pub(crate) fn xcontext(&self) -> register::cp0::XContext {
        let value = self.read(register::Cp0::XContext);
        register::cp0::XContext::new(value)
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
    T: IntoIterator<Item = (register::Register, u64)>,
{
    fn from(value: T) -> Self {
        let mut registers = Self::default();
        for (reg, val) in value {
            registers.write(reg, val);
        }
        registers
    }
}

macro_rules! impl_reg_index {
    ($(($ty:ty, $output:ty, $field:ident)),*) => {
        $(
            impl RegIndex<$ty> for Registers {
                type Output = $output;

                fn read(&self, index: $ty) -> Self::Output {
                    self.$field.read_relaxed(index.into()).unwrap()
                }

                fn write(&mut self, index: $ty, value: Self::Output) {
                    self.$field.write_relaxed(index.into(), value).unwrap()
                }
            }
        )*
    };
}

impl_reg_index!(
    (register::GeneralPurpose, u64, general_purpose),
    (register::Cp0, u64, cp0),
    (register::Fpu, u64, fpu),
    (register::FpuControl, u64, fpu_control),
    (register::Special, u64, special)
);

impl RegIndex<register::Register> for Registers {
    type Output = u64;

    fn read(&self, index: register::Register) -> Self::Output {
        match index {
            register::Register::GeneralPurpose(r) => self.read(r),
            register::Register::Fpu(r) => self.read(r),
            register::Register::FpuControl(r) => self.read(r),
            register::Register::Special(r) => self.read(r),
            register::Register::Cp0(r) => self.read(r),
        }
    }

    fn write(&mut self, index: register::Register, value: Self::Output) {
        match index {
            register::Register::GeneralPurpose(r) => self.write(r, value),
            register::Register::Fpu(r) => self.write(r, value),
            register::Register::FpuControl(r) => self.write(r, value),
            register::Register::Special(r) => self.write(r, value),
            register::Register::Cp0(r) => self.write(r, value),
        }
    }
}

impl fmt::Debug for Registers {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let write_reg = |f: &mut fmt::Formatter<'_>, name: &str, reg: u64| -> fmt::Result {
            writeln!(f, "{name: <10} = {reg:#x}")
        };

        writeln!(f, "\ngeneral purpose registers:")?;
        for (i, reg) in self.general_purpose.iter_relaxed().enumerate() {
            let name = register::GeneralPurpose::name_from_index(i);
            write_reg(f, name, reg)?;
        }

        writeln!(f, "\ncoprocessor 0 registers:")?;
        for (i, reg) in self.cp0.iter_relaxed().enumerate() {
            let register = register::Cp0::from_repr(i).unwrap();
            if !register.is_reserved() {
                write_reg(f, register.name(), reg)?;
            }
        }
        write_reg(f, "Reserved", self.read(RESERVED_CP0_REGISTER_LATCH))?;

        writeln!(f, "\nfpu general purpose registers:")?;
        for (i, reg) in self.fpu.iter_relaxed().enumerate() {
            let name = register::Fpu::name_from_index(i);
            write_reg(f, name, reg)?;
        }

        writeln!(f, "\nfpu control registers:")?;
        for (i, reg) in self.fpu_control.iter_relaxed().enumerate() {
            let register = register::FpuControl::from_repr(i).unwrap();
            if !register.is_reserved() {
                write_reg(f, register.name(), reg)?;
            }
        }

        writeln!(f, "\nspecial registers:")?;
        for (i, reg) in self.special.iter_relaxed().enumerate() {
            let name = register::Special::name_from_index(i);
            write_reg(f, name, reg)?;
        }
        write_reg(f, "delay_slot", self.read(INSIDE_DELAY_SLOT_STORAGE))?;

        Ok(())
    }
}

impl target::RegisterStorage for Registers {
    type Globals<'ctx> = Globals<'ctx>;

    fn read_program_counter(&self) -> u64 {
        self.read(register::Special::Pc)
    }

    fn build_globals<'ctx>(
        &self,
        module: &Module<'ctx>,
        exec: &ExecutionEngine<'ctx>,
    ) -> Self::Globals<'ctx> {
        Globals {
            general_purpose: self.general_purpose.map_into(
                module,
                exec,
                "general_purpose_registers",
            ),
            cp0: self.cp0.map_into(module, exec, "coprocessor_0_registers"),
            fpu: self.fpu.map_into(module, exec, "floating_point_registers"),
            fpu_control: self.fpu_control.map_into(
                module,
                exec,
                "floating_point_control_registers",
            ),
            special: self.special.map_into(module, exec, "special_registers"),
        }
    }
}

#[derive(Debug)]
pub struct Globals<'ctx> {
    pub general_purpose: GlobalValue<'ctx>,
    pub cp0: GlobalValue<'ctx>,
    pub special: GlobalValue<'ctx>,
    pub fpu: GlobalValue<'ctx>,
    pub fpu_control: GlobalValue<'ctx>,
}

impl<'ctx> target::Globals<'ctx> for Globals<'ctx> {
    type Id = register::Register;

    fn ptr_value<T: target::Target>(
        &self,
        codegen: &CodeGen<'ctx, T>,
        index: Self::Id,
    ) -> PointerValue<'ctx> {
        let gep = |ptr| unsafe {
            let i64_type = codegen.context.i64_type();
            let name = &format!("{}_", index.name());
            codegen.builder.build_in_bounds_gep(
                i64_type,
                ptr,
                &[i64_type.const_int(index.to_repr() as u64, false)],
                name,
            )
        };
        match index {
            register::Register::Cp0(_) => gep(self.cp0.as_pointer_value()),
            register::Register::Special(_) => gep(self.special.as_pointer_value()),
            register::Register::Fpu(_) => gep(self.fpu.as_pointer_value()),
            register::Register::FpuControl(_) => gep(self.fpu_control.as_pointer_value()),
            register::Register::GeneralPurpose(_) => gep(self.general_purpose.as_pointer_value()),
        }
    }

    fn program_counter_ptr<T: target::Target>(
        &self,
        codegen: &CodeGen<'ctx, T>,
    ) -> PointerValue<'ctx> {
        self.ptr_value(codegen, register::Register::Special(register::Special::Pc))
    }
}
