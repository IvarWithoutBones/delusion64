use crate::codegen::{RegisterGlobals, INSIDE_DELAY_SLOT_STORAGE, RESERVED_CP0_REGISTER_LATCH};
use inkwell::{
    context::ContextRef, execution_engine::ExecutionEngine, module::Module, values::GlobalValue,
};
use mips_decomp::register::{self, Register};
use std::{cell::UnsafeCell, fmt};

#[repr(C)]
pub(crate) struct Registers {
    pub general_purpose: UnsafeCell<[u64; register::GeneralPurpose::count()]>,
    pub cp0: UnsafeCell<[u64; register::Cp0::count()]>,
    pub fpu: UnsafeCell<[u64; register::Fpu::count()]>,
    pub fpu_control: UnsafeCell<[u64; register::FpuControl::count()]>,
    pub special: UnsafeCell<[u64; register::Special::count()]>,
}

impl Registers {
    pub fn map_into<'ctx>(
        &self,
        context: &ContextRef<'ctx>,
        module: &Module<'ctx>,
        execution_engine: &ExecutionEngine<'ctx>,
    ) -> RegisterGlobals<'ctx> {
        let i64_type = context.i64_type();
        let map_array = |name: &str, ptr: *const [u64], len: usize| -> GlobalValue<'ctx> {
            let ty = i64_type.array_type(len as u32);
            let arr = module.add_global(ty, Default::default(), name);
            execution_engine.add_global_mapping(&arr, ptr.cast::<*const u8>() as usize);
            arr
        };

        let cp0 = map_array("cp0_registers", self.cp0.get(), register::Cp0::count());
        let fpu = map_array("fpu_registers", self.fpu.get(), register::Fpu::count());
        let fpu_control = map_array(
            "fpu_control_registers",
            self.fpu_control.get(),
            register::FpuControl::count(),
        );
        let general_purpose = map_array(
            "general_purpose_registers",
            self.general_purpose.get(),
            register::GeneralPurpose::count(),
        );
        let special = map_array(
            "special_registers",
            self.special.get(),
            register::Special::count(),
        );

        RegisterGlobals {
            general_purpose,
            cp0,
            fpu,
            fpu_control,
            special,
        }
    }

    pub fn general_purpose(&self) -> impl Iterator<Item = u64> {
        unsafe { (*self.general_purpose.get()).iter().copied() }
    }

    pub fn cp0(&self) -> impl Iterator<Item = u64> {
        unsafe { (*self.cp0.get()).iter().copied() }
    }

    pub fn fpu(&self) -> impl Iterator<Item = u64> {
        unsafe { (*self.fpu.get()).iter().copied() }
    }

    pub fn fpu_control(&self) -> impl Iterator<Item = u64> {
        unsafe { (*self.fpu_control.get()).iter().copied() }
    }

    pub fn special(&self) -> impl Iterator<Item = u64> {
        unsafe { (*self.special.get()).iter().copied() }
    }

    pub fn status(&self) -> register::cp0::Status {
        register::cp0::Status::new(self[register::Cp0::Status] as u32)
    }

    pub fn cause(&self) -> register::cp0::Cause {
        register::cp0::Cause::new(self[register::Cp0::Cause] as u32)
    }

    pub fn set_cause(&mut self, cause: register::cp0::Cause) {
        let raw: u32 = cause.into();
        self[register::Cp0::Cause] = raw as u64;
    }

    pub fn fpu_control_status(&self) -> register::fpu::ControlStatus {
        register::fpu::ControlStatus::new(self[register::FpuControl::ControlStatus] as u32)
    }

    pub fn page_mask(&self) -> register::cp0::PageMask {
        register::cp0::PageMask::new(self[register::Cp0::PageMask] as u32)
    }

    pub fn context(&self) -> register::cp0::Context {
        register::cp0::Context::new(self[register::Cp0::Context])
    }

    pub fn xcontext(&self) -> register::cp0::XContext {
        register::cp0::XContext::new(self[register::Cp0::XContext])
    }

    pub fn trigger_interrupt(&self) -> bool {
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

macro_rules! impl_index {
    ($ty:ty, $(($idx:path, $for:ident :: $field:ident)),*) => {
        $(
            impl ::std::ops::Index<$idx> for $for {
                type Output = $ty;

                fn index(&self, idx: $idx) -> &Self::Output {
                    unsafe { &self.$field.get().as_ref().unwrap()[idx as usize] }
                }
            }

            impl ::std::ops::IndexMut<$idx> for $for {
                fn index_mut(&mut self, idx: $idx) -> &mut Self::Output {
                    unsafe { &mut self.$field.get().as_mut().unwrap()[idx as usize] }
                }
            }
        )*
    };
}

impl_index!(
    u64,
    (register::GeneralPurpose, Registers::general_purpose),
    (register::Special, Registers::special),
    (register::Cp0, Registers::cp0),
    (register::Fpu, Registers::fpu),
    (register::FpuControl, Registers::fpu_control)
);

impl std::ops::Index<Register> for Registers {
    type Output = u64;

    fn index(&self, index: Register) -> &Self::Output {
        match index {
            Register::GeneralPurpose(r) => &self[r],
            Register::Special(r) => &self[r],
            Register::Cp0(r) => &self[r],
            Register::Fpu(r) => &self[r],
            Register::FpuControl(r) => &self[r],
        }
    }
}

impl std::ops::IndexMut<Register> for Registers {
    fn index_mut(&mut self, index: Register) -> &mut Self::Output {
        match index {
            Register::GeneralPurpose(r) => &mut self[r],
            Register::Special(r) => &mut self[r],
            Register::Cp0(r) => &mut self[r],
            Register::Fpu(r) => &mut self[r],
            Register::FpuControl(r) => &mut self[r],
        }
    }
}

impl Default for Registers {
    fn default() -> Self {
        Self {
            general_purpose: UnsafeCell::new([0; register::GeneralPurpose::count()]),
            cp0: UnsafeCell::new([0; register::Cp0::count()]),
            fpu: UnsafeCell::new([0; register::Fpu::count()]),
            special: UnsafeCell::new([0; register::Special::count()]),
            fpu_control: UnsafeCell::new([0; register::FpuControl::count()]),
        }
    }
}

impl fmt::Debug for Registers {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let write_reg = |f: &mut fmt::Formatter<'_>, name: &str, reg: u64| -> fmt::Result {
            writeln!(f, "{name: <10} = {reg:#x}")
        };

        writeln!(f, "\ngeneral purpose registers:")?;
        for (i, reg) in self.general_purpose().enumerate() {
            let name = register::GeneralPurpose::name_from_index(i);
            write_reg(f, name, reg)?;
        }

        writeln!(f, "\ncoprocessor 0 registers:")?;
        for (i, reg) in self.cp0().enumerate() {
            let register = register::Cp0::from_repr(i).unwrap();
            if !register.is_reserved() {
                write_reg(f, register.name(), reg)?;
            }
        }
        write_reg(f, "Reserved", self[RESERVED_CP0_REGISTER_LATCH])?;

        writeln!(f, "\nfpu general purpose registers:")?;
        for (i, reg) in self.fpu().enumerate() {
            let name = register::Fpu::name_from_index(i);
            write_reg(f, name, reg)?;
        }

        writeln!(f, "\nfpu control registers:")?;
        for (i, reg) in self.fpu_control().enumerate() {
            let register = register::FpuControl::from_repr(i).unwrap();
            if !register.is_reserved() {
                write_reg(f, register.name(), reg)?;
            }
        }

        writeln!(f, "\nspecial registers:")?;
        for (i, reg) in self.special().enumerate() {
            let name = register::Special::name_from_index(i);
            write_reg(f, name, reg)?;
        }
        write_reg(f, "delay_slot", self[INSIDE_DELAY_SLOT_STORAGE])?;

        Ok(())
    }
}
