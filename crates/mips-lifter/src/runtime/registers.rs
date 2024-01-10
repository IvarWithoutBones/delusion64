use inkwell::{
    context::ContextRef, execution_engine::ExecutionEngine, module::Module, types::IntType,
    values::GlobalValue,
};
use std::sync::{atomic, Arc};

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

// We cannot use [`std::ops::Index`] and its mutable counterpart,
// since it is not safe to return a (potentially exclusive) reference to a shared value.
pub trait RegIndex<T> {
    type Output;

    fn read(&self, index: T) -> Self::Output;

    fn write(&mut self, index: T, value: Self::Output);
}
