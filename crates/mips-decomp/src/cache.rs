//! Parsing for the `cache` instructions's operands. See the `cache` instruction in the ISA manual for more information.

use std::fmt;
use strum::{EnumVariantNames, FromRepr, IntoStaticStr, VariantNames};

/// The type of cache the instruction refers to. Meant to be parsed using `Operand::CacheSubject`.
/// Can be combined with the `CacheOpcode` to create a `CacheOperation`.
#[derive(FromRepr, EnumVariantNames, Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum CacheSubject {
    Instruction = 0,
    Data = 1,
}

impl CacheSubject {
    pub const fn name_from_index(index: usize) -> &'static str {
        Self::VARIANTS[index]
    }

    pub const fn name(self) -> &'static str {
        Self::name_from_index(self as usize)
    }
}

impl fmt::Display for CacheSubject {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.name())
    }
}

/// The unique opcode identifying the cache operation. Meant to be parsed using `Operand::CacheOpcode`.
/// Can be combined with the `CacheSubject` to create a `CacheOperation`.
#[derive(FromRepr, EnumVariantNames, Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum CacheOpcode {
    IndexInvalidate = 0,
    LoadTag = 1,
    StoreTag = 2,
    CreateDirtyExclusive = 3,
    HitInvalidate = 4,
    HitWriteBackInvalidateOrFill = 5,
    HitWriteBack = 6,
}

impl CacheOpcode {
    pub const fn name_from_index(index: usize) -> &'static str {
        Self::VARIANTS[index]
    }

    pub const fn name(self) -> &'static str {
        Self::name_from_index(self as usize)
    }
}

impl fmt::Display for CacheOpcode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.name())
    }
}

/// The cache operation which fully describes the cache instruction.
/// This can be created by combining a `CacheOpcode` and a `CacheSubject`.
#[derive(Debug, IntoStaticStr, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum CacheOperation {
    /// Set the cache state of the cache block to Invalid.
    IndexInvalidate,
    /// Examine the cache state of the data cache block at the Invalidate index specified by the virtual address.
    /// If the state is not Invalid, then write back the block to main memory.
    /// The address to write is taken from the cache tag. Set cache state of cache block to Invalid.
    IndexWriteBackInvalidate,
    /// Read the tag for the cache block at the specified index and place it into the `TagLo` register of the CP0.
    IndexLoadTag(CacheSubject),
    /// Write the contents of the `Lo` register of the CP0 register to the tag for the cache block at the specified index.
    IndexStoreTag(CacheSubject),
    /// This operation is used to load as little data as possible from main memory when writing new data into the entire cache block where the coherency is kept.
    /// If the cache block does not contain the specified address, and the block is dirty, write it back to main memory.
    /// In all cases, set the cache block tag to the specified physical address, set the cache state to dirty.
    CreateDirtyExclusive,
    /// If the cache block contains the specified address, set the cache block state invalid.
    HitInvalidate(CacheSubject),
    /// If the cache block contains the specified address, write back the data if it is dirty, and set the cache block state invalid.
    HitWriteBackInvalidate,
    /// Fill the instruction cache block with the data from main memory.
    Fill,
    /// If the cache block contains the specified address and the cache state is in the dirty state, write back the data to main memory.
    HitWriteBackDirty,
    /// If the cache block contains the specified address, write back the data unconditionally.
    HitWriteBack,
}

impl CacheOperation {
    pub fn new(subject: CacheSubject, opcode: CacheOpcode) -> Option<Self> {
        match opcode {
            CacheOpcode::IndexInvalidate => match subject {
                CacheSubject::Instruction => Some(Self::IndexInvalidate),
                CacheSubject::Data => Some(Self::IndexWriteBackInvalidate),
            },

            CacheOpcode::HitWriteBack => match subject {
                CacheSubject::Instruction => Some(Self::HitWriteBack),
                CacheSubject::Data => Some(Self::HitWriteBackDirty),
            },

            CacheOpcode::HitWriteBackInvalidateOrFill => match subject {
                CacheSubject::Instruction => Some(Self::Fill),
                CacheSubject::Data => Some(Self::HitWriteBackInvalidate),
            },

            CacheOpcode::CreateDirtyExclusive => match subject {
                CacheSubject::Instruction => None,
                CacheSubject::Data => Some(Self::CreateDirtyExclusive),
            },

            CacheOpcode::HitInvalidate => Some(Self::HitInvalidate(subject)),
            CacheOpcode::LoadTag => Some(Self::IndexLoadTag(subject)),
            CacheOpcode::StoreTag => Some(Self::IndexStoreTag(subject)),
        }
    }

    pub fn name(&self) -> &'static str {
        // Because this enum contains variants with associated data, we have to use `IntoStaticStr` instead of `FromRepr` + `EnumVariantNames`.
        // That sadly means this function cannot be a const fn, as the `From` trait cannot be used from a const context.
        self.into()
    }
}

impl fmt::Display for CacheOperation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let name = self.name();
        match self {
            Self::IndexLoadTag(subject)
            | Self::IndexStoreTag(subject)
            | Self::HitInvalidate(subject) => {
                write!(f, "{name}({subject})")
            }

            _ => write!(f, "{name}"),
        }
    }
}
