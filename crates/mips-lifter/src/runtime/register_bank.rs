use inkwell::{
    context::ContextRef,
    execution_engine::ExecutionEngine,
    module::Module,
    types::IntType,
    values::{GlobalValue, PointerValue},
};
use std::{
    fmt,
    sync::{atomic, Arc},
};

pub trait Integer: Default + Copy + Clone + PartialEq + Eq + fmt::Debug {
    type Atomic: AtomicInteger<Data = Self>;

    fn ty<'ctx>(context: &ContextRef<'ctx>) -> IntType<'ctx>;
}

pub trait AtomicInteger: From<Self::Data> + Default + fmt::Debug {
    type Data: Integer<Atomic = Self>;

    fn load(&self, ordering: atomic::Ordering) -> Self::Data;

    fn store(&self, value: Self::Data, ordering: atomic::Ordering);

    unsafe fn from_ptr<'a>(ptr: *mut Self::Data) -> &'a Self;
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

                unsafe fn from_ptr<'a>(ptr: *mut Self::Data) -> &'a Self {
                    Self::from_ptr(ptr)
                }

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

/// A bank of registers which can potentially be shared between the JIT and the caller using atomic operations.
///
/// Note that when [`std::clone::Clone`] is called on any register bank, the contents of the bank are copied into a new allocation.
/// If you instead want to share the same allocation, call [`RegisterBank::share`].
pub struct RegisterBank<T: Integer, const LEN: usize> {
    // [T; LEN], but since we manually index it via pointer offsets to avoid creating references, this is nicer to work with.
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

    /// Creates a new RegisterBank with shared access to the underlying array. To create a second RegisterBank, call [`RegisterBank::share`].
    pub fn new_shared(registers: Box<[T::Atomic; LEN]>) -> Self {
        Self {
            start_ptr: Box::into_raw(registers).cast(),
            ownership: Ownership::Shared(Arc::new(())),
        }
    }

    /// Creates a new RegisterBank with exclusive access to the underlying array, filled with zeros.
    pub fn zeroed_exclusive() -> Self {
        let array = Box::new(std::array::from_fn(|_| T::default()));
        Self::new_exclusive(array)
    }

    /// Creates a new RegisterBank with shared access to the underlying array, filled with zeros.
    pub fn zeroed_shared() -> Self {
        let array = Box::new(std::array::from_fn(|_| T::Atomic::default()));
        Self::new_shared(array)
    }

    /// Creates a new RegisterBank with shared access to the same underlying array as this one, or [`None`] if this bank is exclusive.
    pub fn share(&self) -> Option<Self> {
        match &self.ownership {
            Ownership::Exclusive => None,
            Ownership::Shared(ref_count) => Some(Self {
                start_ptr: self.start_ptr,
                ownership: Ownership::Shared(ref_count.clone()),
            }),
        }
    }

    /// Returns the number of registers in this bank.
    pub const fn len(&self) -> usize {
        LEN
    }

    /// Returns true when this bank contains no registers.
    pub const fn is_empty(&self) -> bool {
        LEN == 0
    }

    /// Returns true when this register bank is shared, and atomic operations are required to access it.
    pub fn is_shared(&self) -> bool {
        matches!(self.ownership, Ownership::Shared(_))
    }

    /// Returns true when this register bank is exclusive, and atomic operations are not required to access it.
    pub fn is_exclusive(&self) -> bool {
        matches!(self.ownership, Ownership::Exclusive)
    }

    /// Reads a value from the underlying array, at the given index, using the given atomic ordering if this is bank is shared.
    ///
    /// # Safety
    /// This function is unsafe because it does not check that the given index is within the bounds of the underlying array.
    pub unsafe fn read_unchecked(&self, index: usize, ordering: atomic::Ordering) -> T {
        let ptr = self.start_ptr.add(index);
        match &self.ownership {
            Ownership::Exclusive => ptr.read(),
            Ownership::Shared(_ref_count) => T::Atomic::from_ptr(ptr).load(ordering),
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
            Ownership::Shared(_ref_count) => T::Atomic::from_ptr(ptr).store(value, ordering),
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
    ) -> RegisterBankMapping<'ctx> {
        let ty = T::ty(&module.get_context()).array_type(LEN as u32);
        let pointer = module.add_global(ty, None, name);
        execution_engine.add_global_mapping(&pointer, self.start_ptr as usize);
        RegisterBankMapping {
            pointer,
            atomic: self.is_shared(),
        }
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

impl<T: Integer, const LEN: usize> Clone for RegisterBank<T, LEN> {
    fn clone(&self) -> Self {
        match &self.ownership {
            Ownership::Exclusive => {
                let registers = self.iter_relaxed().collect::<Vec<_>>().into_boxed_slice();
                let boxed_array: Box<[T; LEN]> = registers.try_into().expect("LEN is correct");
                Self::new_exclusive(boxed_array)
            }
            Ownership::Shared(_) => {
                let boxed_array = self
                    .iter_relaxed()
                    .map(T::into)
                    .collect::<Vec<_>>()
                    .into_boxed_slice()
                    .try_into()
                    .expect("LEN is correct");
                Self::new_shared(boxed_array)
            }
        }
    }
}

impl<T: Integer, const LEN: usize> PartialEq for RegisterBank<T, LEN> {
    fn eq(&self, other: &Self) -> bool {
        self.iter_relaxed()
            .zip(other.iter_relaxed())
            .all(|(a, b)| a == b)
    }
}

impl<T: Integer, const LEN: usize> Default for RegisterBank<T, LEN> {
    fn default() -> Self {
        Self::zeroed_exclusive()
    }
}

impl<T: Integer, const LEN: usize> fmt::Debug for RegisterBank<T, LEN> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // We want to call `f.debug_list()` inside of `f.debug_struct()`, but we can't because both mutably borrow `f`.
        // Instead we can obtain another `Formatter` by creating a wrapper type that implements `fmt::Debug`.
        // This would be much nicer with "debug_closure_helpers": https://github.com/rust-lang/rust/issues/117729
        struct RegisterFormatter<'a, T: Integer, const LEN: usize>(&'a RegisterBank<T, LEN>);
        impl<'a, T: Integer, const LEN: usize> fmt::Debug for RegisterFormatter<'a, T, LEN> {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                f.debug_list().entries(self.0.iter_relaxed()).finish()
            }
        }

        f.debug_struct("RegisterBank")
            .field("ownership", &self.ownership)
            .field("registers", &RegisterFormatter(self))
            .finish()
    }
}

// We cannot use [`std::ops::Index`] and its mutable counterpart,
// since it is not safe to return a (potentially exclusive) reference to a shared value.
pub trait RegIndex<T> {
    type Output;

    fn read(&self, index: T) -> Self::Output;

    fn write(&mut self, index: T, value: Self::Output);
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct RegisterBankMapping<'ctx> {
    pointer: GlobalValue<'ctx>,
    atomic: bool,
}

impl<'ctx> RegisterBankMapping<'ctx> {
    pub fn pointer_value(&self) -> PointerValue<'ctx> {
        self.pointer.as_pointer_value()
    }

    pub fn is_atomic(&self) -> bool {
        self.atomic
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn read_exclusive() {
        let mut regs: Box<[u32; 32]> = Box::new([0; 32]);
        regs[0] = 1;
        regs[1] = 2;
        let regs = RegisterBank::new_exclusive(regs);
        assert_eq!(regs.read_relaxed(0), Some(1));
        assert_eq!(regs.read_relaxed(1), Some(2));
        assert_eq!(regs.read_relaxed(2), Some(0));
        assert_eq!(regs.read_relaxed(32), None);
    }

    #[test]
    fn read_write_exclusive() {
        let regs = RegisterBank::<u32, 10>::new_exclusive(Box::new([0; 10]));
        regs.write_relaxed(0, 3);
        let regs2 = regs.clone();

        regs.write_relaxed(1, 4);
        regs.write_relaxed(9, 5);

        assert_eq!(regs.read_relaxed(0), Some(3));
        assert_eq!(regs.read_relaxed(1), Some(4));
        assert_eq!(regs.read_relaxed(2), Some(0));
        assert_eq!(regs.read_relaxed(9), Some(5));
        assert_eq!(regs.read_relaxed(10), None);

        assert_eq!(regs2.read_relaxed(0), Some(3));
        assert!(regs2.iter_relaxed().skip(1).all(|value| value == 0));
    }

    #[test]
    fn read_write_shared() {
        let atomic_array = std::array::from_fn(|_| atomic::AtomicU32::default());
        let regs = RegisterBank::<u32, 10>::new_shared(Box::new(atomic_array));
        let regs2 = regs.share().unwrap();

        regs.write_relaxed(0, 3);
        regs.write_relaxed(1, 4);

        assert_eq!(regs2.read_relaxed(0), Some(3));
        assert_eq!(regs2.read_relaxed(1), Some(4));
        assert_eq!(regs2.read_relaxed(2), Some(0));

        assert_eq!(regs.read_relaxed(0), Some(3));
        assert_eq!(regs.read_relaxed(1), Some(4));
        assert_eq!(regs.read_relaxed(2), Some(0));
    }

    #[test]
    fn read_write_shared_multi_threaded() {
        let atomic_array = std::array::from_fn(|_| atomic::AtomicU32::default());
        let regs = RegisterBank::<u32, 10>::new_shared(Box::new(atomic_array));
        let regs2 = regs.share().unwrap();

        let thread = std::thread::spawn(move || {
            regs2.write_relaxed(0, 3);
            regs2.write_relaxed(1, 4);
        });

        regs.write_relaxed(2, 5);
        thread.join().unwrap();

        assert_eq!(regs.read_relaxed(0), Some(3));
        assert_eq!(regs.read_relaxed(1), Some(4));
        assert_eq!(regs.read_relaxed(2), Some(5));
    }

    #[test]
    fn iter() {
        let array: Box<[u32; 5]> = Box::new([0, 1, 2, 3, 4]);
        let regs = RegisterBank::new_exclusive(array);
        let mut iter = regs.iter_relaxed();
        assert_eq!(iter.next(), Some(0));
        assert_eq!(iter.next(), Some(1));
        assert_eq!(iter.next(), Some(2));
        assert_eq!(iter.next(), Some(3));
        assert_eq!(iter.next(), Some(4));
        assert_eq!(iter.next(), None);
    }

    #[test]
    fn is_inclusive_exclusive() {
        let regs = RegisterBank::<u32, 10>::new_exclusive(Box::new([0; 10]));
        assert!(regs.is_exclusive());
        assert!(!regs.is_shared());

        let atomic_array = std::array::from_fn(|_| atomic::AtomicU32::default());
        let regs = RegisterBank::<u32, 10>::new_shared(Box::new(atomic_array));
        assert!(!regs.is_exclusive());
        assert!(regs.is_shared());
    }
}
