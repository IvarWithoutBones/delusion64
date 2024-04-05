use crate::{runtime::register_bank::RegisterBankMapping, target, RegIndex, RegisterBank};
use log::trace;
use mips_decomp::register::rsp::{
    self as register,
    control::{DmaRdramAddress, DmaReadLength, DmaSpAddress},
};
use std::fmt;

impl_reg_bank_wrapper!(
    #[derive(Debug)]
    SpecialRegisterBank,
    register::Special,
    u64,
    { register::Special::count() },
    "Special registers such as the program counter."
);

impl SpecialRegisterBank {
    /// Read the contents of the program counter.
    #[must_use]
    pub fn read_program_counter(&self) -> u32 {
        #[allow(clippy::cast_possible_truncation)] // `read()` will mask the irrelevant bits anyways
        let value = self.read(register::Special::ProgramCounter) as u32;
        register::special::ProgramCounter::from(value).read()
    }

    /// Write the given value to the program counter.
    pub fn write_program_counter(&mut self, value: u32) {
        let value = u32::from(register::special::ProgramCounter::from_raw(value));
        self.write(register::Special::ProgramCounter, u64::from(value));
    }

    /// Increments the program counter by the given amount, accounting for wrapping and alignment.
    pub fn increment_program_counter(&mut self, amount: u32) {
        let mut pc = register::special::ProgramCounter::from(self.read_program_counter());
        pc.increment(amount);
        self.write_program_counter(pc.read());
    }
}

impl_reg_bank_wrapper!(
    ControlRegisterBank,
    register::Control,
    u32,
    { register::Control::count() },
    "The control registers, exposed to the CPU over MMIO."
);

impl ControlRegisterBank {
    pub fn swap_active_dma_buffer(&mut self) {
        let value = u32::from(self.read(register::Control::ActiveBuffer) == 0);
        trace!("swapping active DMA buffer, now {value}");
        self.write(register::Control::ActiveBuffer, value);
    }

    /// Reads and parses the register at the given index, resolving the DMA buffer if necessary.
    #[must_use]
    pub fn register_from_index(&self, index: usize, read: bool) -> Option<register::Control> {
        let register = register::Control::from_repr(index.try_into().ok()?)?;
        if register.is_dma_register() {
            // We need to get the "current" register, since the DMA registers are double buffered.
            let offset = register.to_lower_buffer().to_repr()
                + ((if read {
                    // When reading we always want the currently/previously active buffer
                    self.read(register::Control::ActiveBuffer) as usize
                } else {
                    // When writing we want the first buffer unless a DMA transfer is ongoing
                    usize::from(self.read_parsed::<register::control::Status>().dma_busy())
                }) * register::Control::DMA_BUFFER_OFFSET);

            let result = register::Control::from_repr(offset.try_into().ok()?)?;
            debug_assert!(result.is_dma_register());
            Some(result)
        } else {
            Some(register)
        }
    }

    fn normalise_register(&self, register: register::Control, read: bool) -> register::Control {
        self.register_from_index(register.to_repr(), read)
            .expect("register is valid")
    }

    /// Clears the semaphore register.
    pub fn clear_semaphore(&mut self) {
        let value = register::control::Semaphore::default();
        self.write_parsed(value);
    }

    /// Reads and writes the semaphore register, updating the value. Returns the old value.
    #[must_use]
    pub fn read_write_semaphore(&mut self) -> bool {
        let mut semaphore: register::control::Semaphore = self.read_parsed();
        let value = semaphore.read();
        self.write_parsed(semaphore);
        value
    }

    /// Reads and parses the given control register, selecting the active DMA buffer if necessary.
    #[must_use]
    pub fn read_parsed<T: ControlRegisterDefinition>(&self) -> T {
        let reg = self.normalise_register(T::ID, true);
        T::from(self.read(reg))
    }

    /// Writes the given value to the given control register, selecting the active DMA buffer if necessary.
    pub fn write_parsed<T: ControlRegisterDefinition>(&mut self, value: T) {
        let reg = self.normalise_register(T::ID, false);
        self.write(reg, value.into());
    }
}

impl std::fmt::Debug for ControlRegisterBank {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        struct DmaRegs<'a>(&'a ControlRegisterBank, bool);
        impl std::fmt::Debug for DmaRegs<'_> {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                let read = |mut reg: register::Control| -> u32 {
                    if self.1 {
                        reg = reg.to_lower_buffer();
                    }
                    self.0.read(reg)
                };

                f.debug_struct("DmaRegisters")
                    .field(
                        "sp_address",
                        &DmaSpAddress::from(read(register::Control::DmaSpAddress2)),
                    )
                    .field(
                        "rdram_address",
                        &DmaRdramAddress::from(read(register::Control::DmaRdramAddress2)),
                    )
                    .field(
                        "read_length",
                        &DmaReadLength::from(read(register::Control::DmaReadLength2)),
                    )
                    .field(
                        "write_length",
                        &DmaReadLength::from(read(register::Control::DmaWriteLength2)),
                    )
                    .finish()
            }
        }

        f.debug_struct("ControlRegisterBank")
            .field("status", &self.read_parsed::<register::control::Status>())
            .field(
                "semaphore",
                &self.read_parsed::<register::control::Semaphore>(),
            )
            .field("active_buffer", &self.read(register::Control::ActiveBuffer))
            .field("dma_buffers", &[DmaRegs(self, false), DmaRegs(self, true)])
            .finish()
    }
}

pub trait ControlRegisterDefinition: From<u32> + Into<u32> {
    const ID: register::Control;
}

macro_rules! impl_control_reg_def {
    ($(($def: ident, $id: ident)),* $(,)?) => {
        $(
            impl ControlRegisterDefinition for register::control::$def {
                const ID: register::Control = register::Control::$id;
            }
        )*
    }
}

impl_control_reg_def!(
    (DmaRdramAddress, DmaRdramAddress1),
    (DmaSpAddress, DmaSpAddress1),
    (DmaReadLength, DmaReadLength1),
    (DmaWriteLength, DmaWriteLength1),
    (Status, Status),
    (Semaphore, Semaphore),
);

#[derive(Default, Clone, PartialEq)]
pub struct Registers {
    pub general_purpose: RegisterBank<u32, { register::GeneralPurpose::count() }>,
    pub flags: RegisterBank<u16, { register::Flags::count() }>,
    pub control: ControlRegisterBank,
    pub special: SpecialRegisterBank,
    /// In reality these are 128-bit registers, but since Rust does not have an AtomicU128 type, RegisterBank cannot be implemented for it.
    /// If you need to access the full 128-bit value, use [`RegIndex`]'s `read` and `write` methods.
    pub vector: RegisterBank<u64, { register::Vector::count() * 2 }>,
}

impl_reg_index!(
    Registers,
    (register::GeneralPurpose, u32, general_purpose),
    (register::Control, u32, control),
    (register::Special, u64, special),
    (register::Flags, u16, flags)
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
            register::Register::Flags(r) => u128::from(self.read(r)),
        }
    }

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
            register::Register::Flags(r) => {
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

        writeln!(f, "\nflag registers:")?;
        for reg in register::Flags::iter() {
            write_reg(f, reg.name(), self.read(reg).into())?;
        }

        writeln!(f, "\nspecial registers:")?;
        for reg in register::Special::iter() {
            write_reg(f, reg.name(), self.read(reg).into())?;
        }

        Ok(())
    }
}

impl target::RegisterStorage for Registers {
    type Id = RegisterID;
    type Globals<'ctx> = RegisterGlobals<'ctx>;

    fn read_register(&self, reg: Self::Id) -> u64 {
        self.read(reg.0).try_into().expect("value too large")
    }

    fn write_register(&mut self, reg: Self::Id, value: u64) {
        self.write(reg.0, value.into());
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
            control: self.control.0.map_into(module, exec_engine, "control"),
            vector: self.vector.map_into(module, exec_engine, "vector"),
            special: self.special.0.map_into(module, exec_engine, "special"),
            flags: self.flags.map_into(module, exec_engine, "flags"),
        }
    }

    fn copy_into(&self, other: &mut Self) {
        self.general_purpose.copy_into(&other.general_purpose);
        self.vector.copy_into(&other.vector);
        self.control.0.copy_into(&other.control.0);
        self.special.0.copy_into(&other.special.0);
    }
}

/// A single RSP register.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(transparent)]
pub(crate) struct RegisterID(pub(crate) register::Register);

impl target::RegisterID for RegisterID {
    const PROGRAM_COUNTER: Self = Self(register::Register::Special(
        register::Special::ProgramCounter,
    ));

    fn name(&self) -> &'static str {
        self.0.name()
    }
}

impl<T: Into<register::Register>> From<T> for RegisterID {
    fn from(value: T) -> Self {
        Self(value.into())
    }
}

#[derive(Debug)]
pub(crate) struct RegisterGlobals<'ctx> {
    general_purpose: RegisterBankMapping<'ctx>,
    flags: RegisterBankMapping<'ctx>,
    control: RegisterBankMapping<'ctx>,
    vector: RegisterBankMapping<'ctx>,
    special: RegisterBankMapping<'ctx>,
}

impl<'ctx> target::Globals<'ctx> for RegisterGlobals<'ctx> {
    type RegisterID = RegisterID;

    fn pointer_value<T: target::Target>(
        &self,
        codegen: &crate::codegen::CodeGen<'ctx, T>,
        reg: &Self::RegisterID,
    ) -> inkwell::values::PointerValue<'ctx> {
        let i16_type = codegen.context.i16_type();
        let i32_type = codegen.context.i32_type();
        let i64_type = codegen.context.i64_type();
        let i128_type = codegen.context.i128_type();

        let (ty, ptr) = match reg.0 {
            register::Register::Control(_) => (i32_type, self.control.pointer_value()),
            register::Register::Vector(_) => (i128_type, self.vector.pointer_value()),
            register::Register::Flags(_) => (i16_type, self.flags.pointer_value()),
            register::Register::Special(_) => (i64_type, self.special.pointer_value()),
            register::Register::GeneralPurpose(_) => {
                (i32_type, self.general_purpose.pointer_value())
            }
        };

        unsafe {
            codegen
                .builder
                .build_in_bounds_gep(
                    ty,
                    ptr,
                    &[ty.const_int(reg.0.to_repr() as u64, false)],
                    reg.0.name(),
                )
                .expect("failed to build GEP")
        }
    }

    fn is_atomic(&self, reg: Self::RegisterID) -> bool {
        match reg.0 {
            register::Register::GeneralPurpose(_) => self.general_purpose.is_atomic(),
            register::Register::Control(_) => self.control.is_atomic(),
            register::Register::Vector(_) => self.vector.is_atomic(),
            register::Register::Special(_) => self.special.is_atomic(),
            register::Register::Flags(_) => self.flags.is_atomic(),
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
        registers.write(register::Vector::V00, DUMMY);
        assert_eq!(registers.read(register::Vector::V00), DUMMY);
        assert_eq!(registers.read(register::Vector::V01), 0);

        registers.write(register::Vector::V15, DUMMY);
        assert_eq!(registers.read(register::Vector::V14), 0);
        assert_eq!(registers.read(register::Vector::V15), DUMMY);
        assert_eq!(registers.read(register::Vector::V16), 0);

        registers.write(register::Vector::V31, DUMMY);
        assert_eq!(registers.read(register::Vector::V30), 0);
        assert_eq!(registers.read(register::Vector::V31), DUMMY);
    }
}
