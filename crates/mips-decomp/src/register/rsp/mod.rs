//! Register definitions for the N64's RSP.

use std::mem::size_of;
use strum::{EnumCount, EnumIter, FromRepr, VariantNames};

pub mod control;
pub mod special;

#[doc(inline)]
pub use super::GeneralPurpose;

/// The RSP's control registers, mapped to coprocessor 0 as well as the CPU's address space via MMIO.
#[derive(EnumCount, EnumIter, VariantNames, FromRepr, Debug, PartialEq, Eq, Clone, Copy)]
#[repr(u8)]
pub enum Control {
    /// Address in `IMEM`/`DMEM` for a DMA transfer. This refers to one of the two double-buffered registers.
    DmaSpAddress1,
    /// Address in `RDRAM` for a DMA transfer. This refers to one of the two double-buffered registers.
    DmaRdramAddress1,
    /// Length of a DMA transfer. Writing this register triggers a DMA transfer from `RDRAM` to `IMEM`/`DMEM`.
    /// This refers to one of the two double-buffered registers.
    DmaReadLength1,
    /// Length of a DMA transfer. Writing this register triggers a DMA transfer from `IMEM`/`DMEM` to `RDRAM`.
    DmaWriteLength1,
    /// General status register.
    Status,
    /// Report whether there is a pending DMA transfer (mirror of the `DMA_FULL` bit of [`Self::Status`]).
    DmaFull,
    /// Report whether there is a DMA transfer in progress (mirror of the `DMA_BUSY` bit of [`Self::Status`]).
    DmaBusy,
    /// Register to assist implementing a simple mutex between VR4300 and RSP.
    Semaphore,

    // Note that the index of double-buffered registers does not particularly matter, just needs to be consistent.
    /// Address in `IMEM`/`DMEM` for a DMA transfer. This refers to one of the two double-buffered registers.
    DmaSpAddress2,
    /// Address in `RDRAM` for a DMA transfer. This refers to one of the two double-buffered registers.
    DmaRdramAddress2,
    /// Length of a DMA transfer. Writing this register triggers a DMA transfer from `RDRAM` to `IMEM`/`DMEM`.
    /// This refers to one of the two double-buffered registers.
    DmaReadLength2,
    /// Length of a DMA transfer. Writing this register triggers a DMA transfer from `IMEM`/`DMEM` to `RDRAM`.
    /// This refers to one of the two double-buffered registers.
    DmaWriteLength2,
}

impl_reg!(Control);

impl Control {
    /// The distance between the two double-buffered registers, to be used as an offset.
    pub const DMA_BUFFER_OFFSET: usize =
        Self::DmaSpAddress2.to_repr() - Self::DmaSpAddress1.to_repr();

    /// The offset from the base address of the RSP's control registers, for the CPU's MMIO.
    #[must_use]
    pub const fn offset(self) -> usize {
        self.to_repr() * size_of::<u32>()
    }

    /// Whether this register is a DMA register or not.
    #[must_use]
    pub const fn is_dma_register(self) -> bool {
        matches!(
            self,
            Self::DmaSpAddress1
                | Self::DmaRdramAddress1
                | Self::DmaReadLength1
                | Self::DmaWriteLength1
                | Self::DmaSpAddress2
                | Self::DmaRdramAddress2
                | Self::DmaReadLength2
                | Self::DmaWriteLength2
        )
    }

    /// Returns the lowest register of the double-buffered pair.
    #[must_use]
    pub const fn to_lower_buffer(self) -> Self {
        match self {
            Self::DmaSpAddress2 => Self::DmaSpAddress1,
            Self::DmaRdramAddress2 => Self::DmaRdramAddress1,
            Self::DmaReadLength2 => Self::DmaReadLength1,
            Self::DmaWriteLength2 => Self::DmaWriteLength1,
            _ => self,
        }
    }
}

/// The RSP's 128-bit vector registers used for fixed-point SIMD instructions, mapped to coprocessor 2. These are also referred to as `VPR`s.
#[derive(EnumCount, EnumIter, VariantNames, FromRepr, Debug, PartialEq, Eq, Clone, Copy)]
#[strum(serialize_all = "snake_case")]
#[repr(u8)]
pub enum Vector {
    V00,
    V01,
    V02,
    V03,
    V04,
    V05,
    V06,
    V07,
    V08,
    V09,
    V10,
    V11,
    V12,
    V13,
    V14,
    V15,
    V16,
    V17,
    V18,
    V19,
    V20,
    V21,
    V22,
    V23,
    V24,
    V25,
    V26,
    V27,
    V28,
    V29,
    V30,
    V31,
}

impl_reg!(Vector);

/// An miscellaneous register that doesn't fit into any other category.
#[derive(EnumCount, EnumIter, VariantNames, FromRepr, Debug, PartialEq, Eq, Clone, Copy)]
#[repr(u8)]
pub enum Special {
    /// The 12-bit program counter.
    #[strum(serialize = "pc")]
    ProgramCounter,
}

impl_reg!(Special);

/// One of the RSP's registers.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Register {
    GeneralPurpose(GeneralPurpose),
    Control(Control),
    Vector(Vector),
    Special(Special),
}

impl Register {
    /// The name of this register.
    #[must_use]
    pub const fn name(self) -> &'static str {
        match self {
            Self::GeneralPurpose(r) => r.name(),
            Self::Control(r) => r.name(),
            Self::Vector(r) => r.name(),
            Self::Special(r) => r.name(),
        }
    }

    /// The index this register has in its register file.
    #[must_use]
    pub const fn to_repr(self) -> usize {
        match self {
            Self::GeneralPurpose(r) => r.to_repr(),
            Self::Control(r) => r.to_repr(),
            Self::Vector(r) => r.to_repr(),
            Self::Special(r) => r.to_repr(),
        }
    }
}

impl From<GeneralPurpose> for Register {
    fn from(r: GeneralPurpose) -> Self {
        Self::GeneralPurpose(r)
    }
}

impl From<Control> for Register {
    fn from(r: Control) -> Self {
        Self::Control(r)
    }
}

impl From<Special> for Register {
    fn from(r: Special) -> Self {
        Self::Special(r)
    }
}

impl From<Vector> for Register {
    fn from(r: Vector) -> Self {
        Self::Vector(r)
    }
}
