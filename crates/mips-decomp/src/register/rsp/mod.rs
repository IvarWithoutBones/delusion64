//! Register definitions for the N64's RSP.

use std::mem::size_of;
use strum::{EnumCount, EnumIter, EnumVariantNames, FromRepr};

pub mod control;
pub mod special;

#[doc(inline)]
pub use super::GeneralPurpose;

/// The RSP's control registers, mapped to coprocessor 0 as well as the CPU's address space via MMIO.
#[derive(EnumCount, EnumIter, EnumVariantNames, FromRepr, Debug, PartialEq, Eq, Clone, Copy)]
#[repr(u8)]
pub enum Control {
    /// Address in `IMEM`/`DMEM` for a DMA transfer.
    DmaSpAddress,
    /// Address in `RDRAM` for a DMA transfer.
    DmaRdramAddress,
    /// Length of a DMA transfer. Writing this register triggers a DMA transfer from `RDRAM` to `IMEM`/`DMEM`.
    DmaReadLength,
    /// Length of a DMA transfer. Writing this register triggers a DMA transfer from `IMEM`/`DMEM` to `RDRAM`.
    DmaWriteLength,
    /// General status register.
    Status,
    /// Report whether there is a pending DMA transfer (mirror of the `DMA_FULL` bit of [`Self::Status`]).
    DmaFull,
    /// Report whether there is a DMA transfer in progress (mirror of the `DMA_BUSY` bit of [`Self::Status`]).
    DmaBusy,
    /// Register to assist implementing a simple mutex between VR4300 and RSP.
    Semaphore,
}

impl_reg!(Control);

impl Control {
    /// The offset from the base address of the RSP's control registers, for the CPU's MMIO.
    #[must_use]
    pub const fn offset(self) -> usize {
        self.to_repr() * size_of::<u32>()
    }
}

/// The RSP's 128-bit vector registers used for fixed-point SIMD instructions, mapped to coprocessor 2. These are also referred to as `VPR`s.
#[derive(EnumCount, EnumIter, EnumVariantNames, FromRepr, Debug, PartialEq, Eq, Clone, Copy)]
#[repr(u8)]
pub enum Vector {
    Vpr0,
    Vpr1,
    Vpr2,
    Vpr3,
    Vpr4,
    Vpr5,
    Vpr6,
    Vpr7,
    Vpr8,
    Vpr9,
    Vpr10,
    Vpr11,
    Vpr12,
    Vpr13,
    Vpr14,
    Vpr15,
    Vpr16,
    Vpr17,
    Vpr18,
    Vpr19,
    Vpr20,
    Vpr21,
    Vpr22,
    Vpr23,
    Vpr24,
    Vpr25,
    Vpr26,
    Vpr27,
    Vpr28,
    Vpr29,
    Vpr30,
    Vpr31,
}

impl_reg!(Vector);

/// An miscellaneous register that doesn't fit into any other category.
#[derive(EnumCount, EnumIter, EnumVariantNames, FromRepr, Debug, PartialEq, Eq, Clone, Copy)]
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
