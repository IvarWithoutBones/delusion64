use std::{fmt, mem::size_of, ops::Range};

pub type PhysicalAddress = u32;

/// The value returned by a memory read or write operation. Optionally contains a range of physical addresses that were mutated.
#[derive(Debug, Default)]
pub struct BusValue<T> {
    pub inner: T,
    /// The range of physical addresses that were mutated by this operation.
    /// When modifying memory this *must* be set, so that the relevant JIT blocks can be invalidated.
    pub mutated: Option<Range<PhysicalAddress>>,
}

impl<T> BusValue<T> {
    pub fn new(value: T) -> Self {
        Self {
            inner: value,
            mutated: None,
        }
    }

    pub fn with_mutated(mut self, mutated: Range<PhysicalAddress>) -> Self {
        self.mutated = Some(mutated);
        self
    }
}

impl<T> From<T> for BusValue<T> {
    fn from(value: T) -> Self {
        Self::new(value)
    }
}

#[derive(Debug)]
pub enum BusError<T> {
    UnexpectedSize { expected: usize, got: usize },
    AddressNotMapped { address: PhysicalAddress },
    Other(T),
}

impl<T> From<T> for BusError<T> {
    fn from(value: T) -> Self {
        Self::Other(value)
    }
}

impl From<BusError<fmt::Error>> for fmt::Error {
    fn from(value: BusError<fmt::Error>) -> Self {
        match value {
            BusError::Other(result) => result,
            _ => unimplemented!(),
        }
    }
}

pub type BusResult<T, E> = Result<BusValue<T>, BusError<E>>;

/// A trait for types that can be converted into an Int<SIZE>. Much like std::convert::Into, but with a const generic size.
pub trait IntoInt<const SIZE: usize> {
    fn into_int(self) -> Int<SIZE>;
}

impl<const SIZE: usize> IntoInt<SIZE> for [u8; SIZE] {
    #[inline]
    fn into_int(self) -> Int<SIZE> {
        Int(self)
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
    pub fn new<T, E, const EXPECTED_SIZE: usize>(
        value: T,
    ) -> Result<Int<EXPECTED_SIZE>, BusError<E>>
    where
        T: IntoInt<SIZE>,
    {
        value.into_int().resize()
    }

    /// Converts the integer into a value of the specified (usually inferred) type, by resizing it to the expected size.
    #[inline]
    pub fn try_into<T, E, const REQUIRED_SIZE: usize>(self) -> Result<T, BusError<E>>
    where
        T: From<Int<REQUIRED_SIZE>>,
    {
        Ok(T::from(self.resize()?))
    }

    #[inline]
    pub fn resize<E, const EXPECTED_SIZE: usize>(self) -> Result<Int<EXPECTED_SIZE>, BusError<E>> {
        if SIZE == EXPECTED_SIZE {
            // This is required because the compiler cannot infer the size is equal, we effectively cast the const generics.
            // When compiled with optimizations, this should be a no-op. SAFETY: we just checked the size is equal.
            Ok(unsafe { (&self as *const Self).cast::<Int<EXPECTED_SIZE>>().read() })
        } else {
            Err(BusError::UnexpectedSize {
                expected: EXPECTED_SIZE,
                got: SIZE,
            })
        }
    }

    #[inline]
    pub fn from_slice<E>(slice: &[u8]) -> Result<Self, BusError<E>> {
        let slice = slice.get(..SIZE).ok_or(BusError::UnexpectedSize {
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
    fn default() -> Self {
        Self([0_u8; SIZE])
    }
}

macro_rules! impl_int_conversion {
    ($($ty:ty),+) => {
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

// u128/i128 are omitted because the guest does not support them.
impl_int_conversion!(u8, u16, u32, u64, i8, i16, i32, i64);

macro_rules! format_int {
    ($(($trait:path, $fmt:expr)),+) => {
        $(
            impl<const SIZE: usize> $trait for Int<SIZE> {
                fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                    if SIZE == size_of::<u8>() {
                        let value: u8 = self.resize()?.into();
                        write!(f, concat!("{:", $fmt, "}_u8"), value)
                    } else if SIZE == size_of::<u16>() {
                        let value: u16 = self.resize()?.into();
                        write!(f, concat!("{:", $fmt, "}_u16"), value)
                    } else if SIZE == size_of::<u32>() {
                        let value: u32 = self.resize()?.into();
                        write!(f, concat!("{:", $fmt, "}_u32"), value)
                    } else if SIZE == size_of::<u64>() {
                        let value: u64 = self.resize()?.into();
                        write!(f, concat!("{:", $fmt, "}_u64"), value)
                    } else {
                        write!(f, concat!("{:", $fmt, "?}"), self.0)
                    }
                }
            }
        )+
    };
}

format_int!((fmt::Display, ""), (fmt::Debug, "#"), (fmt::LowerHex, "x"), (fmt::UpperHex, "X"));

/// A mirroring configuration for a memory section.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Mirroring<Section: MemorySection + 'static> {
    /// Mirror the section at the specified offset.
    AtOffset(u32),
    /// The section is mirrored at the specified offset, spanning across multiple sections.
    /// The two sections must be adjacent.
    AcrossSections {
        start: &'static Section,
        end: &'static Section,
    },
    /// No mirroring is used.
    None,
}

impl<Section: MemorySection> Mirroring<Section> {
    pub fn resolve(&self, section: &'static Section, paddr: PhysicalAddress) -> Address<Section> {
        let (section, offset) = match self {
            Self::AtOffset(offset) => (section, paddr % offset),
            Self::AcrossSections { start, end } => {
                let offset = paddr % (end.range().end - start.range().start);
                let addr = start.range().start + offset;
                start
                    .range()
                    .contains(&addr)
                    .then_some((*start, offset))
                    .or_else(|| end.range().contains(&addr).then_some((*end, offset)))
                    .unwrap_or_else(|| {
                        panic!("address {paddr:#x} is not mapped to any of the specified sections")
                    })
            }
            Self::None => (section, paddr - section.range().start),
        };
        Address {
            offset: offset as usize,
            section,
        }
    }
}

/// A section of memory that can be accessed by a bus. This is meant to be implemented on an enum.
pub trait MemorySection: fmt::Debug {
    /// The range of physical addresses that this section occupies.
    fn range(&self) -> Range<PhysicalAddress>;

    /// The mirroring configuration for this section.
    fn mirroring(&self) -> Mirroring<Self>
    where
        Self: Sized;

    /// Whether the range of written bytes should be invalidated when writing to this section.
    /// If this returns false, the physical addresses (if any) must be invalidated manually.
    fn auto_invalidate_written_addresses(&self) -> bool;
}

/// A physical address in memory, mapped to a specific section.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Address<Section: MemorySection + 'static> {
    /// The offset from the start of the section.
    pub offset: usize,
    /// The section that this address is mapped to.
    /// This can optionally specify mirroring, which the offset will be adjusted for.
    pub section: &'static Section,
}

impl<Section: MemorySection> Address<Section> {
    pub fn new(section: &'static Section, paddr: PhysicalAddress) -> Self {
        section.mirroring().resolve(section, paddr)
    }

    pub fn physical_address(&self) -> PhysicalAddress {
        self.section.range().start + self.offset as PhysicalAddress
    }
}

/// A bus that can be used to read and write memory.
pub trait Bus {
    /// The error type that can be returned by the bus.
    type Error: fmt::Debug;

    /// The type of memory sections that the bus can access.
    type Section: MemorySection + 'static;

    /// The sections of memory that the bus can access.
    const SECTIONS: &'static [Self::Section];

    /// Performs a single tick, used to emulate clock cycles. Called after each CPU instruction.
    fn tick(&mut self) -> BusResult<(), Self::Error>;

    /// Reads a value from memory.
    fn read_memory<const SIZE: usize>(
        &mut self,
        address: Address<Self::Section>,
    ) -> BusResult<Int<SIZE>, Self::Error>;

    /// Writes a value to memory.
    fn write_memory<const SIZE: usize>(
        &mut self,
        address: Address<Self::Section>,
        value: Int<SIZE>,
    ) -> BusResult<(), Self::Error>;
}
