//! The RSP's control registers, corresponding to a value in [`Control`].

use super::Control;
use std::fmt;
use tartan_bitfield::{bitfield, bitfield_without_debug};

/// The bank of memory accessed by a DMA transfer
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum MemoryBank {
    IMem,
    DMem,
}

impl MemoryBank {
    /// The sizes of a bank of memory, in bytes.
    pub const LEN: usize = 0x1000;
}

bitfield_without_debug! {
    /// Address in IMEM/DMEM for a DMA transfer.
    /// See [n64brew](https://n64brew.dev/wiki/Reality_Signal_Processor/Interface#SP_DMA_SPADDR) for more information.
    pub struct DmaSpAddress(u32) {
        [0..=11] unmasked_address: u32,
        [12] raw_bank,
    }
}

impl DmaSpAddress {
    pub const OFFSET: usize = Control::DmaSpAddress1.offset();

    /// DMEM or IMEM address used in SP DMAs.
    #[must_use]
    pub fn address(self) -> u32 {
        self.unmasked_address() & !0b111
    }

    /// The memory bank accessed by SP DMA transfers.
    #[must_use]
    pub fn bank(self) -> MemoryBank {
        if self.raw_bank() {
            MemoryBank::IMem
        } else {
            MemoryBank::DMem
        }
    }

    /// Increment the address by 8 bytes, wrapping if necessary.
    pub fn increment(&mut self) {
        self.set_unmasked_address(self.address() + 8);
        if self.address() as usize > MemoryBank::LEN {
            // Overflows wrap around within the same bank
            self.set_unmasked_address(0);
        }
    }
}

impl fmt::Debug for DmaSpAddress {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("DmaSpAddress")
            .field("address", &self.address())
            .field("bank", &self.bank())
            .finish()
    }
}

bitfield_without_debug! {
    /// Address in RDRAM for a DMA transfer.
    /// See [n64brew](https://n64brew.dev/wiki/Reality_Signal_Processor/Interface#SP_DMA_RAMADDR) for more information.
    pub struct DmaRdramAddress(u32) {
        [0..=23] unmasked_address: u32,
    }
}

impl DmaRdramAddress {
    pub const OFFSET: usize = Control::DmaRdramAddress1.offset();

    /// RDRAM address used in SP DMAs.
    #[must_use]
    pub fn address(self) -> u32 {
        self.unmasked_address() & !0b111
    }

    /// Increment the address by 8 bytes.
    pub fn increment(&mut self) {
        self.set_unmasked_address(self.address() + 8);
    }
}

impl fmt::Debug for DmaRdramAddress {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("DmaRdramAddress")
            .field("address", &self.address())
            .finish()
    }
}

bitfield! {
    /// The combined definition of DmaReadLength and DmaWriteLength, they are identical.
    struct DmaLength(u32) {
        [0..=11] unmasked_length: u32,
        /// Number of rows to transfer minus 1.
        [12..=19] pub count: u8,
        [21..=31] unmasked_skip: u32,
    }
}

impl DmaLength {
    // This the last block of a DMA transfer will set our length counter to -8, which translates to 0xFF8 for 11-bit integers.
    const MINUS_8: u32 = 0xFF8;

    /// Number of bytes to transfer for each row minus 1
    #[must_use]
    pub fn length(self) -> u32 {
        // From n64-systemtest: "[..] 1 is added and then it is rounded up to the next multiple of 8 (e.g. 0..7 ==> 8 bytes, 8..15 => 16 bytes)"
        const ALIGN: u32 = 0b111;
        (self.unmasked_length() + ALIGN + 1) & !ALIGN
    }

    /// Number of bytes to skip in RDRAM after each row
    #[must_use]
    pub fn skip(self) -> u32 {
        self.unmasked_skip() & !0b111
    }

    pub fn decrement(&mut self) {
        let len = self
            .unmasked_length()
            .checked_sub(8)
            .unwrap_or(Self::MINUS_8);
        self.set_unmasked_length(len);
    }
}

/// Length of a DMA transfer. Writing this register triggers a DMA transfer from RDRAM to IMEM/DMEM.
/// See [n64brew](https://n64brew.dev/wiki/Reality_Signal_Processor/Interface#SP_DMA_RDLEN) for more information.
#[derive(Default, Copy, Clone, PartialEq, Eq)]
#[repr(transparent)]
pub struct DmaReadLength(DmaLength);

impl DmaReadLength {
    pub const OFFSET: usize = Control::DmaReadLength1.offset();
}

impl DmaReadLength {
    /// Number of bytes to transfer for each row, minus 1.
    #[must_use]
    pub fn length(self) -> u32 {
        self.0.length()
    }

    /// Number of bytes to skip in RDRAM after each row.
    #[must_use]
    pub fn skip(self) -> u32 {
        self.0.skip()
    }

    /// Decrement the length counter by 8 bytes.
    pub fn decrement(&mut self) {
        self.0.decrement();
    }
}

impl fmt::Debug for DmaReadLength {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("DmaReadLength")
            .field("length", &self.length())
            .field("count", &self.0.count())
            .field("skip", &self.skip())
            .finish()
    }
}

impl From<u32> for DmaReadLength {
    fn from(raw: u32) -> Self {
        Self(DmaLength(raw))
    }
}

impl From<DmaReadLength> for u32 {
    fn from(len: DmaReadLength) -> Self {
        len.0.into()
    }
}

/// Length of a DMA transfer. Writing this register triggers a DMA transfer from IMEM/DMEM to RDRAM.
/// See [n64brew](https://n64brew.dev/wiki/Reality_Signal_Processor/Interface#SP_DMA_WRLEN) for more information.
#[derive(Default, Copy, Clone, PartialEq, Eq)]
#[repr(transparent)]
pub struct DmaWriteLength(DmaLength);

impl DmaWriteLength {
    pub const OFFSET: usize = Control::DmaWriteLength1.offset();
}

impl DmaWriteLength {
    /// Number of bytes to transfer for each row minus 1
    #[must_use]
    pub fn length(self) -> u32 {
        self.0.length()
    }

    /// Number of bytes to skip in RDRAM after each row
    #[must_use]
    pub fn skip(self) -> u32 {
        self.0.skip()
    }

    /// Decrement the length counter by 8 bytes
    pub fn decrement(&mut self) {
        self.0.decrement();
    }
}

impl fmt::Debug for DmaWriteLength {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct(stringify!(DmaWriteLength))
            .field("length", &self.length())
            .field("count", &self.0.count())
            .field("skip", &self.skip())
            .finish()
    }
}

impl From<u32> for DmaWriteLength {
    fn from(raw: u32) -> Self {
        Self(DmaLength(raw))
    }
}

impl From<DmaWriteLength> for u32 {
    fn from(len: DmaWriteLength) -> Self {
        len.0.into()
    }
}

/// Status bits that can be freely used by software. Corresponds to [`SpStatus::signals`].
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
#[repr(transparent)]
pub struct Signals([bool; 8]);

impl Signals {
    /// Write the given value from [`StatusWrite::raw_signals`] as the signals field of [`Status`].
    fn write(mut self, raw: u16) -> Self {
        // When writing, two bits are used per signal, if both are set no change occurs.
        // 0: Clear signal
        // 1: Set signal
        for i in 0..8 {
            let base = i * 2;
            let clear = (raw >> base) & 1 == 1;
            let set = (raw >> (base + 1)) & 1 == 1;
            if clear && !set {
                self.0[i] = false;
            } else if set && !clear {
                self.0[i] = true;
            }
        }
        self
    }
}

impl From<u8> for Signals {
    fn from(raw: u8) -> Self {
        let mut result = [false; 8];
        for (i, sig) in result.iter_mut().enumerate() {
            *sig = (raw >> i) & 1 == 1;
        }
        Signals(result)
    }
}

impl From<Signals> for u8 {
    fn from(signals: Signals) -> Self {
        signals
            .0
            .iter()
            .enumerate()
            .fold(0, |acc, (i, &signal)| acc | (u8::from(signal) << i))
    }
}

/// A request to raise or lower an interrupt.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum InterruptRequest {
    Raise,
    Lower,
}

bitfield! {
    /// The RSP status register.
    /// This definition is only accurate when reading. When writing, use [`StatusWrite`] or the [`Status::write`] method to correctly apply changes.
    /// See [n64brew](https://n64brew.dev/wiki/Reality_Signal_Processor/Interface#SP_STATUS) for more information.
    pub struct Status(u32) {
        /// Indicates whether the RSP is currently running, or halted
        [0] pub halted,
        /// Set when the RSP executes a BREAK instruction
        [1] pub broke,
        /// Set when there is a DMA transfer in progress. Mirrored in [`DmaBusy`]
        [2] pub dma_busy,
        /// Set when there is a DMA transfer pending, and there is one in progress as well. Mirrored in [`DmaFull`]
        [3] pub dma_full,
        /// Set when the RSP is accessing either DMEM or IMEM
        [4] pub io_busy,
        /// Set when single-step mode is active. In single-step mode, RSP auto-halts itself after a single opcode is ran
        [5] pub single_step,
        /// When set, trigger a RSP MI interrupt when the BREAK instruction is ran
        [6] pub interrupt_break,
        /// State bits that can freely be used by software
        [7..=14] pub signals: u8 as Signals,
    }
}

impl Status {
    pub const DMA_BUSY_SHIFT: u64 = 2;
    pub const DMA_FULL_SHIFT: u64 = 3;
}

bitfield! {
    /// The RSP status register.
    /// This definition is only accurate when writing. When reading, use [`Status`] instead.
    /// See [n64brew](https://n64brew.dev/wiki/Reality_Signal_Processor/Interface#SP_STATUS) for more information.
    pub struct StatusWrite(u32) {
        /// Start running RSP code from the current RSP PC, and clear the [`Status::halted`] flag
        [0] pub clear_halted,
        /// Pause execution of RSP code, and set the [`Status::halted`] pub flag
        [1] pub set_halted,
        /// Clear the [`Status::broke`] flag
        [2] pub clear_broke,
        /// Acknowledge an RSP MI interrupt
        [3] pub clear_interrupt,
        /// Manually trigger an RSP MI interrupt
        [4] pub set_interrupt,
        /// Disable single-step mode, and clear the [`Status::single_step`] pub flag
        [5] pub clear_single_step,
        /// Enable single-step mode, and set the [`Status::single_step`] pub flag. In single-step mode, RSP auto-halts itself after a single opcode is ran
        [6] pub set_single_step,
        /// Disable triggering an RSP MI interrupt when the BREAK instruction is ran
        [7] pub clear_interrupt_break,
        /// Enable triggering an RSP MI interrupt when the BREAK instruction is ran
        [8] pub set_interrupt_break,
        /// Set or clear state bits that can freely be used by software. Must be written using [`Signals::write`]
        [9..=24] pub raw_signals: u16,
    }
}

impl Status {
    pub const OFFSET: usize = Control::Status.offset();

    #[must_use]
    pub fn raw(&self) -> u32 {
        self.0
    }

    /// Write the given value into the register, correctly applying changes.
    #[must_use]
    pub fn write(&mut self, x: impl Into<StatusWrite>) -> Option<InterruptRequest> {
        let x: StatusWrite = x.into();
        self.set_signals(self.signals().write(x.raw_signals()));

        if x.clear_broke() {
            self.set_broke(false);
        }

        // Set or clear flags with toggles, if both set and clear are set, no change occurs
        if !(x.set_halted() && x.clear_halted()) {
            self.set_halted((self.halted() && !x.clear_halted()) || x.set_halted());
        }

        if !(x.set_single_step() && x.clear_single_step()) {
            self.set_single_step(
                (self.single_step() && !x.clear_single_step()) || x.set_single_step(),
            );
        }

        if !(x.set_interrupt_break() && x.clear_interrupt_break()) {
            self.set_interrupt_break(
                (self.interrupt_break() && !x.clear_interrupt_break()) || x.set_interrupt_break(),
            );
        }

        if x.clear_interrupt() && !x.set_interrupt() {
            Some(InterruptRequest::Lower)
        } else if x.set_interrupt() && !x.clear_interrupt() {
            Some(InterruptRequest::Raise)
        } else {
            None
        }
    }
}

bitfield! {
    /// Report whether there is a pending DMA transfer.
    /// See [n64brew](https://n64brew.dev/wiki/Reality_Signal_Processor/Interface#SP_DMA_FULL) for more information.
    pub struct DmaFull(u32) {
        /// Mirror of [`Status::dma_full`]
        [0] pub dma_full,
    }
}

impl DmaFull {
    pub const OFFSET: usize = Control::DmaFull.offset();
}

bitfield! {
    /// Report whether there is a DMA transfer in progress.
    /// See [n64brew](https://n64brew.dev/wiki/Reality_Signal_Processor/Interface#SP_DMA_BUSY) for more information.
    pub struct DmaBusy(u32) {
        /// Mirror of [`Status::dma_busy`]
        [0] pub dma_busy,
    }
}

impl DmaBusy {
    pub const OFFSET: usize = Control::DmaBusy.offset();
}

bitfield! {
    /// Register to assist implementing a simple mutex between VR4300 and RSP.
    /// See [n64brew](https://n64brew.dev/wiki/Reality_Signal_Processor/Interface#SP_SEMAPHORE) for more information.
    pub struct Semaphore(u32) {
        /// After each read, this is always automatically set to true while returning the previous value.
        /// When writing, it is set to false, regardless of the value written.
        [0] semaphore,
    }
}

impl Semaphore {
    pub const OFFSET: usize = Control::Semaphore.offset();

    /// Resets the semaphore to false.
    pub fn write(&mut self) {
        self.set_semaphore(false);
    }

    /// Returns the semaphore, and sets it to true.
    pub fn read(&mut self) -> bool {
        let res = self.semaphore();
        self.set_semaphore(true);
        res
    }
}
