use super::{memory::tlb::AccessMode, Environment, InterruptHandler, ValidRuntime};
use crate::target::{Memory, Target};
use std::{fmt, mem::size_of, ops::Range};

// TODO: Returning a BusValue for every single tick/memory access seems wasteful.
// Maybe we should give a mutable reference to a struct with side-effects instead?

pub type PhysicalAddress = u32;

/// The value returned by a memory read or write operation. Optionally contains a range of physical addresses that were mutated.
#[derive(Debug, Default)]
pub struct BusValue<T> {
    /// The value returned by the memory operation.
    inner: T,
    /// The range of physical addresses that were mutated by this operation.
    /// When modifying memory this *must* be set so that the relevant JIT blocks can be invalidated.
    pub mutated: Vec<Range<PhysicalAddress>>,
    /// The mask of the external interrupt(s) that was triggered by this operation, if any.
    /// Only external interrupts should be set, which occupy bits 2..=6.
    pub interrupt: Option<u8>,
    /// Whether the JIT should exit after this operation, returning to the caller of [`crate::builder::JitBuilder::run`].
    pub request_exit: bool,
}

impl<V> BusValue<V> {
    pub const fn new(value: V) -> Self {
        Self {
            inner: value,
            mutated: Vec::new(),
            interrupt: None,
            request_exit: false,
        }
    }

    pub fn mutated(&mut self, range: Range<PhysicalAddress>) {
        self.mutated.push(range);
    }

    pub(crate) fn handle<T: Target, B: Bus>(self, env: &mut Environment<T, B>) -> V
    where
        for<'a> Environment<'a, T, B>: ValidRuntime<T>,
    {
        for paddrs in self.mutated {
            let vaddr_start = env
                .memory
                .physical_to_virtual_address(paddrs.start, AccessMode::Write, &env.registers)
                .expect("invalid physical address");
            let len = (paddrs.end - paddrs.start) as u64;
            let vaddr_range = vaddr_start..vaddr_start + len;
            env.codegen.labels.remove_within_range(vaddr_range);
        }

        if let Some(interrupt_mask) = self.interrupt {
            env.handle_interrupt(interrupt_mask);
        }

        if self.request_exit {
            env.exit_requested = true;
        }

        self.inner
    }
}

impl<T> From<T> for BusValue<T> {
    fn from(value: T) -> Self {
        Self::new(value)
    }
}

#[derive(thiserror::Error, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum BusError<E: std::error::Error> {
    // TODO: make this strongly typed
    #[error("Internal error while JITing: {0}")]
    Jit(String),
    #[error("Error during memory operation: {0:#x}")]
    Bus(#[from] E),
}

pub type BusResult<T, E> = Result<BusValue<T>, E>;

#[derive(thiserror::Error, Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum IntError {
    #[error("Unexpected size: expected {expected}, but got {got}")]
    UnexpectedSize { expected: usize, got: usize },
}

/// A trait for types that can be converted into an [`Int`]. Much like [`std::convert::Into`], but with a const generic size.
pub trait IntoInt<const SIZE: usize> {
    fn into_int(self) -> Int<SIZE>;
}

impl<const SIZE: usize> IntoInt<SIZE> for [u8; SIZE] {
    #[inline]
    fn into_int(self) -> Int<SIZE> {
        Int(self)
    }
}

impl<'a, const SIZE: usize> IntoInt<SIZE> for &'a [u8; SIZE] {
    #[inline]
    fn into_int(self) -> Int<SIZE> {
        Int(*self)
    }
}

/// A wrapper around a byte array that represents an integer. Note that the bytes are always interpreted as the systems endianness.
///
/// Used instead of an enum with a variant for each size for a few reasons:
/// It avoids the need of padding to the size of the largest variant, and it allows the size to be inferred from the type.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(transparent)]
pub struct Int<const SIZE: usize>([u8; SIZE]);

impl<const SIZE: usize> Int<SIZE> {
    /// Creates a new integer with the specified value, automatically resizing it to the expected (usually inferred) size.
    #[inline]
    pub fn new<T, const EXPECTED_SIZE: usize>(value: T) -> Result<Int<EXPECTED_SIZE>, IntError>
    where
        T: IntoInt<SIZE>,
    {
        value.into_int().resize()
    }

    /// Converts the integer into a value of the specified (usually inferred) type, by resizing it to the expected size.
    #[inline]
    pub fn try_into<T, const REQUIRED_SIZE: usize>(self) -> Result<T, IntError>
    where
        T: From<Int<REQUIRED_SIZE>>,
    {
        Ok(T::from(self.resize()?))
    }

    #[inline]
    pub fn resize<const EXPECTED_SIZE: usize>(self) -> Result<Int<EXPECTED_SIZE>, IntError> {
        if SIZE == EXPECTED_SIZE {
            // This is required because the compiler cannot infer the size is equal, we effectively cast the const generics.
            // When compiled with optimizations, this should be a no-op. SAFETY: we just checked the size is equal.
            Ok(unsafe { (&self as *const Self).cast::<Int<EXPECTED_SIZE>>().read() })
        } else {
            Err(IntError::UnexpectedSize {
                expected: EXPECTED_SIZE,
                got: SIZE,
            })
        }
    }

    #[inline]
    pub fn from_array(array: [u8; SIZE]) -> Self {
        Self(array)
    }

    #[inline]
    pub fn from_slice(slice: &[u8]) -> Result<Self, IntError> {
        let slice = slice.get(..SIZE).ok_or(IntError::UnexpectedSize {
            expected: SIZE,
            got: slice.len(),
        })?;
        // SAFETY: we just checked the size is equal.
        let sized: &[u8; SIZE] = unsafe { slice.try_into().unwrap_unchecked() };
        Self::new(*sized)
    }

    #[inline]
    pub fn as_slice(&self) -> &[u8; SIZE] {
        &self.0
    }
}

impl<const SIZE: usize> Default for Int<SIZE> {
    #[inline]
    fn default() -> Self {
        Self([0_u8; SIZE])
    }
}

impl<const SIZE: usize> From<Int<SIZE>> for [u8; SIZE] {
    #[inline]
    fn from(value: Int<SIZE>) -> Self {
        value.0
    }
}

impl<const SIZE: usize> From<[u8; SIZE]> for Int<SIZE> {
    #[inline]
    fn from(value: [u8; SIZE]) -> Self {
        Self(value)
    }
}

macro_rules! impl_int_conversion {
    ($($ty: ty),+) => {
        $(
            impl IntoInt<{ size_of::<$ty>() }> for $ty {
                #[inline]
                fn into_int(self) -> Int<{ size_of::<$ty>() }> {
                    Int(self.to_be_bytes())
                }
            }

            impl From<Int<{ size_of::<$ty>() }>> for $ty {
                #[inline]
                fn from(value: Int<{ size_of::<$ty>() }>) -> Self {
                    <$ty>::from_be_bytes(value.0)
                }
            }

            impl From<$ty> for Int<{ size_of::<$ty>() }> {
                #[inline]
                fn from(value: $ty) -> Self {
                    value.into_int()
                }
            }
        )+
    };
}

impl_int_conversion!(u8, u16, u32, u64, i8, i16, i32, i64, u128, i128);

macro_rules! format_int {
    ($(($trait: path, $fmt_str: expr)),+) => {
        $(
            impl<const SIZE: usize> $trait for Int<SIZE> {
                fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                    format_int!(@impl self, f, $fmt_str, $trait, u8, u16, u32, u64, u128)
                }
            }
        )+
    };

    (@impl $self: ident, $fmt: expr, $fmt_str: expr, $trait: path, $($ty: ty),+) => {
        $(
            if SIZE == size_of::<$ty>() {
                // This can never fail, we just checked the size is equal.
                let value: $ty = $self.resize().unwrap().into();
                write!($fmt, concat!("{:", $fmt_str, "}_", stringify!($ty)), value)
            } else
        )+ {
            $fmt.debug_list().entries($self.0.iter()).finish()
        }
    };
}

format_int!(
    (fmt::Display, ""),
    (fmt::Debug, "#x"),
    (fmt::LowerHex, "x"),
    (fmt::UpperHex, "X"),
    (fmt::Binary, "b")
);

/// Decides whom will kill the thread when an unrecoverable error occurs.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum PanicAction {
    /// Kill the thread immediately after calling [`Bus::on_panic`].
    Kill,
    /// Sleep in an idle loop after calling [`Bus::on_panic`], blocking forever. The thread is expected to be killed externally.
    // TODO: should probably return from the jit instead.
    Idle,
}

/// A bus that can be used to read and write memory.
pub trait Bus {
    /// The error type that can be returned by the bus.
    type Error: std::error::Error;

    /// Read a value from data memory.
    fn read_memory<const SIZE: usize>(
        &mut self,
        address: PhysicalAddress,
    ) -> BusResult<Int<SIZE>, Self::Error>;

    /// Write a value to data memory.
    fn write_memory<const SIZE: usize>(
        &mut self,
        address: PhysicalAddress,
        value: Int<SIZE>,
    ) -> BusResult<(), Self::Error>;

    /// Read a value from instruction memory. By default [`Bus::read_memory`] will be used for both data/instruction reads, but this can be specialised.
    fn read_instruction_memory<const SIZE: usize>(
        &mut self,
        address: PhysicalAddress,
    ) -> BusResult<Int<SIZE>, Self::Error> {
        self.read_memory(address)
    }

    /// Writes a value into instruction memory. By default [`Bus::write_memory`] will be used for both data/instruction writes, but this can be specialised.
    fn write_instruction_memory<const SIZE: usize>(
        &mut self,
        address: PhysicalAddress,
        value: Int<SIZE>,
    ) -> BusResult<(), Self::Error> {
        self.write_memory(address, value)
    }

    /// Performs a single tick, used to emulate clock cycles.
    fn tick(&mut self, cycles: usize) -> BusResult<(), Self::Error>;

    /// Should return any ranges of physical addresses that were mutated since the last time mutations were reported.
    /// This currently only gets called prior to looking up cached JIT blocks.
    #[inline(always)]
    fn ranges_to_invalidate(&mut self) -> Vec<Range<PhysicalAddress>> {
        Vec::new()
    }

    /// Called when an unrecoverable error occurs in the CPU. This can be used to notify the rest of the system of a shutdown.
    /// If GDB integration is enabled, its connection will be kept alive and the [`PanicAction`] is ignored.
    /// This means the function can be called multiple times, as in rare cases GDB could cause a panic.
    fn on_panic(&mut self, _error: BusError<Self::Error>) -> PanicAction {
        PanicAction::Kill
    }
}
