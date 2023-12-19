use crate::codegen::{INSIDE_DELAY_SLOT_STORAGE, RESERVED_CP0_REGISTER_LATCH};
use inkwell::{
    context::ContextRef, execution_engine::ExecutionEngine, module::Module, types::IntType,
    values::GlobalValue,
};
use mips_decomp::register;
use std::{
    fmt,
    sync::{atomic, Arc},
};

pub trait Integer: Default + Copy + Clone {
    type Atomic: AtomicInteger<Data = Self>;

    fn ty<'ctx>(context: &ContextRef<'ctx>) -> IntType<'ctx>;
}

pub trait AtomicInteger: Default {
    type Data: Integer<Atomic = Self>;

    fn load(&self, ordering: atomic::Ordering) -> Self::Data;

    fn store(&self, value: Self::Data, ordering: atomic::Ordering);
}

macro_rules! impl_int {
    ($(($ty:tt + $atomic_ty:path, $ctx_field:tt)),* $(,)?) => {
        $(
            impl Integer for $ty {
                type Atomic = $atomic_ty;

                fn ty<'ctx>(context: &ContextRef<'ctx>) -> IntType<'ctx> {
                    context.$ctx_field()
                }
            }

            impl AtomicInteger for $atomic_ty {
                type Data = $ty;

                fn load(&self, ordering: atomic::Ordering) -> Self::Data {
                    self.load(ordering)
                }

                fn store(&self, value: Self::Data, ordering: atomic::Ordering) {
                    self.store(value, ordering);
                }
            }
        )+
    };
}

impl_int!(
    (u8 + atomic::AtomicU8, i8_type),
    (u16 + atomic::AtomicU16, i16_type),
    (u32 + atomic::AtomicU32, i32_type),
    (u64 + atomic::AtomicU64, i64_type),
);

#[derive(Debug, Clone, PartialEq, Eq)]
enum Ownership {
    /// The RegisterBank owns the underlying array, and can mutate or drop it at will without synchronization.
    Exclusive,
    /// The RegisterBank does not own the underlying array, and must atomically access it. A reference count is included to know when to drop it.
    Shared(Arc<()>),
}

pub struct RegisterBank<T: Integer, const LEN: usize> {
    // [T; LEN], but since we manually index it via pointer offsets to avoid creating references, this is nicer to work with.
    // Note that the type layout of AtomicU32 and u32, etc are the same. We can cast between them without issue.
    start_ptr: *mut T,
    ownership: Ownership,
}

impl<T: Integer, const LEN: usize> RegisterBank<T, LEN> {
    /// Creates a new RegisterBank with exclusive access to the underlying array.
    pub fn new_exclusive(registers: Box<[T; LEN]>) -> Self {
        Self {
            start_ptr: Box::into_raw(registers).cast(),
            ownership: Ownership::Exclusive,
        }
    }

    /// Creates two RegisterBanks with shared access to the underlying array. One for the JIT, and one for caller.
    pub fn new_shared(registers: Box<[T::Atomic; LEN]>) -> (Self, Self) {
        let start_ptr = Box::into_raw(registers).cast();
        let ownership = Ownership::Shared(Arc::new(()));
        (
            Self {
                start_ptr,
                ownership: ownership.clone(),
            },
            Self {
                start_ptr,
                ownership,
            },
        )
    }

    /// Reads a value from the underlying array, at the given index, using the given atomic ordering if this is bank is shared.
    ///
    /// # Safety
    /// This function is unsafe because it does not check that the given index is within the bounds of the underlying array.
    pub unsafe fn read_unchecked(&self, index: usize, ordering: atomic::Ordering) -> T {
        let ptr = self.start_ptr.add(index);
        match &self.ownership {
            Ownership::Exclusive => ptr.read(),
            Ownership::Shared(_ref_count) => ptr
                .cast::<T::Atomic>()
                .as_ref()
                .expect("non-null pointer")
                .load(ordering),
        }
    }

    /// Writes a value to the underlying array, at the given index, using the given atomic ordering if this is bank is shared.
    ///
    /// # Safety
    /// This function is unsafe because it does not check that the given index is within the bounds of the underlying array.
    pub unsafe fn write_unchecked(&self, index: usize, value: T, ordering: atomic::Ordering) {
        let ptr = self.start_ptr.add(index);
        match &self.ownership {
            Ownership::Exclusive => ptr.write(value),
            Ownership::Shared(_ref_count) => ptr
                .cast::<T::Atomic>()
                .as_ref()
                .expect("non-null pointer")
                .store(value, ordering),
        }
    }

    #[inline]
    pub fn read(&self, index: usize, ordering: atomic::Ordering) -> Option<T> {
        if index < LEN {
            Some(unsafe { self.read_unchecked(index, ordering) })
        } else {
            None
        }
    }

    #[inline]
    pub fn write(&self, index: usize, value: T, ordering: atomic::Ordering) -> Option<()> {
        if index < LEN {
            unsafe { self.write_unchecked(index, value, ordering) };
            Some(())
        } else {
            None
        }
    }

    #[inline]
    pub fn read_relaxed(&self, index: usize) -> Option<T> {
        self.read(index, atomic::Ordering::Relaxed)
    }

    #[inline]
    pub fn write_relaxed(&self, index: usize, value: T) -> Option<()> {
        self.write(index, value, atomic::Ordering::Relaxed)
    }

    pub fn iter(&self, ordering: atomic::Ordering) -> impl Iterator<Item = T> + '_ {
        let mut index = 0;
        std::iter::from_fn(move || {
            self.read(index, ordering).map(|value| {
                index += 1;
                value
            })
        })
    }

    #[inline]
    pub fn iter_relaxed(&self) -> impl Iterator<Item = T> + '_ {
        self.iter(atomic::Ordering::Relaxed)
    }

    pub(crate) fn map_into<'ctx>(
        &self,
        module: &Module<'ctx>,
        execution_engine: &ExecutionEngine<'ctx>,
        name: &str,
    ) -> GlobalValue<'ctx> {
        let ty = T::ty(&module.get_context()).array_type(LEN as u32);
        let global = module.add_global(ty, None, name);
        execution_engine.add_global_mapping(&global, self.start_ptr as usize);
        global
    }
}

// Omitting Sync since that is only valid when self.ownership == Ownership::Shared.
unsafe impl<T: Integer, const LEN: usize> Send for RegisterBank<T, LEN> {}

impl<T: Integer, const LEN: usize> Drop for RegisterBank<T, LEN> {
    fn drop(&mut self) {
        let free = || {
            let ptr = self.start_ptr.cast::<[T; LEN]>();
            drop(unsafe { Box::from_raw(ptr) });
        };

        match &self.ownership {
            Ownership::Exclusive => free(),
            Ownership::Shared(ref_count) => {
                if Arc::strong_count(ref_count) == 1 {
                    free();
                }
            }
        }
    }
}

impl<T: Integer, const LEN: usize> Default for RegisterBank<T, LEN> {
    fn default() -> Self {
        Self::new_exclusive(Box::new([T::default(); LEN]))
    }
}

#[derive(Default)]
#[repr(C)]
pub struct Registers {
    // TODO: Separate target-dependant registers, e.g. CP0 is 32-bit on the RSP
    pub general_purpose: RegisterBank<u64, { register::GeneralPurpose::count() }>,
    pub cp0: RegisterBank<u64, { register::Cp0::count() }>,
    pub fpu: RegisterBank<u64, { register::Fpu::count() }>,
    pub fpu_control: RegisterBank<u64, { register::FpuControl::count() }>,
    pub special: RegisterBank<u64, { register::Special::count() }>,
}

impl Registers {
    pub(crate) fn status(&self) -> register::cp0::Status {
        let value = self.read(register::Cp0::Status) as u32;
        register::cp0::Status::new(value)
    }

    pub(crate) fn cause(&self) -> register::cp0::Cause {
        let value = self.read(register::Cp0::Cause) as u32;
        register::cp0::Cause::new(value)
    }

    pub(crate) fn set_cause(&mut self, cause: register::cp0::Cause) {
        let raw: u32 = cause.into();
        self.write(register::Cp0::Cause, raw as u64);
    }

    pub(crate) fn fpu_control_status(&self) -> register::fpu::ControlStatus {
        let value = self.read(register::FpuControl::ControlStatus) as u32;
        register::fpu::ControlStatus::new(value)
    }

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
    T: IntoIterator<Item = (mips_decomp::register::Register, u64)>,
{
    fn from(value: T) -> Self {
        let mut registers = Self::default();
        for (reg, val) in value {
            registers.write(reg, val);
        }
        registers
    }
}

pub trait RegIndex<T> {
    type Output;

    fn read(&self, index: T) -> Self::Output;

    fn write(&mut self, index: T, value: Self::Output);
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

impl RegIndex<mips_decomp::register::Register> for Registers {
    type Output = u64;

    fn read(&self, index: mips_decomp::register::Register) -> Self::Output {
        use mips_decomp::register::Register::*;
        match index {
            GeneralPurpose(r) => self.read(r),
            Fpu(r) => self.read(r),
            FpuControl(r) => self.read(r),
            Special(r) => self.read(r),
            Cp0(r) => self.read(r),
        }
    }

    fn write(&mut self, index: mips_decomp::register::Register, value: Self::Output) {
        use mips_decomp::register::Register::*;
        match index {
            GeneralPurpose(r) => self.write(r, value),
            Fpu(r) => self.write(r, value),
            FpuControl(r) => self.write(r, value),
            Special(r) => self.write(r, value),
            Cp0(r) => self.write(r, value),
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
