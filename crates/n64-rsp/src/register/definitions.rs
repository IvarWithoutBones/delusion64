use crate::{InterruptChange, MemoryBank};
use std::fmt;
use tartan_bitfield::{bitfield, bitfield_without_debug};

bitfield_without_debug! {
    /// Address in IMEM/DMEM for a DMA transfer.
    /// See [n64brew](https://n64brew.dev/wiki/Reality_Signal_Processor/Interface#SP_DMA_SPADDR) for more information.
    pub struct DmaSpAddress(u32) {
        [0..=11] unmasked_address: u32,
        [12] raw_bank,
    }
}

impl DmaSpAddress {
    pub const OFFSET: usize = 0x00;

    /// DMEM or IMEM address used in SP DMAs.
    #[must_use]
    pub fn address(self) -> u32 {
        self.unmasked_address() & !0b111
    }

    /// Bank accessed by SP DMA transfers
    #[must_use]
    pub fn bank(self) -> MemoryBank {
        if self.raw_bank() {
            MemoryBank::IMem
        } else {
            MemoryBank::DMem
        }
    }

    pub fn increment(&mut self) {
        self.set_unmasked_address(self.address() + 8);
        if self.address() as usize > self.bank().len() {
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
    pub const OFFSET: usize = 0x04;

    /// RDRAM address used in SP DMAs
    #[must_use]
    pub fn address(self) -> u32 {
        self.unmasked_address() & !0b111
    }

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
        (self.unmasked_length() + 1 + 7) & !0b111
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
    pub const OFFSET: usize = 0x08;
}

impl DmaReadLength {
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
    pub const OFFSET: usize = 0x0C;
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
    fn write(mut self, raw: u16) -> Self {
        // When writing, two bits are used per signal, starting from the LSB:
        // 0: Clear signal
        // 1: Set signal
        for i in 0..8 {
            let base = i * 2;
            let clear = (raw >> base) & 1 == 1;
            let set = (raw >> (base + 1)) & 1 == 1;
            if clear {
                self.0[i] = false;
            } else if set {
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

bitfield! {
    /// The RSP status register.
    /// This definition is only accurate when reading. When writing, use the [`SpStatus::write`] method to correctly apply changes.
    /// See [n64brew](https://n64brew.dev/wiki/Reality_Signal_Processor/Interface#SP_STATUS) for more information.
    pub struct SpStatus(u32) {
        /// Indicates whether the RSP is currently running, or halted
        [0] pub halted,
        /// Set when the RSP executes a BREAK instruction
        [1] pub broke,
        /// Set when there is a DMA transfer in progress
        [2] pub dma_busy,
        /// Set when there is a DMA transfer pending, and there is one in progress as well
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

bitfield! {
    /// This definition is only accurate when writing.
    /// See [n64brew](https://n64brew.dev/wiki/Reality_Signal_Processor/Interface#SP_STATUS) for more information.
    struct SpStatusWrite(u32) {
        /// Start running RSP code from the current RSP PC, and clear the [`SpStatus::halted`] flag
        [0] clear_halted,
        /// Pause execution of RSP code, and set the [`SpStatus::halted`] flag
        [1] set_halted,
        /// Clear the [`SpStatus::broke`] flag
        [2] clear_broke,
        /// Acknowledge an RSP MI interrupt
        [3] clear_interrupt,
        /// Manually trigger an RSP MI interrupt
        [4] set_interrupt,
        /// Disable single-step mode, and clear the [`SpStatus::single_step`] flag
        [5] clear_single_step,
        /// Enable single-step mode, and set the [`SpStatus::single_step`] flag. In single-step mode, RSP auto-halts itself after a single opcode is ran
        [6] set_single_step,
        /// Disable triggering an RSP MI interrupt when the BREAK instruction is ran
        [7] clear_interrupt_break,
        /// Enable triggering an RSP MI interrupt when the BREAK instruction is ran
        [8] set_interrupt_break,
        /// Set or clear state bits that can freely be used by software. Must be written using [`Signals::write`]
        [9..=24] raw_signals: u16,
    }
}

impl SpStatus {
    pub const OFFSET: usize = 0x10;

    pub fn write(&mut self, raw: u32) -> Option<InterruptChange> {
        let x = SpStatusWrite(raw);
        self.set_signals(self.signals().write(x.raw_signals()));

        if x.clear_broke() {
            self.set_broke(false);
        }

        // Set or clear flags with toggles, where setting takes precedence over clearing
        self.set_halted((self.halted() && !x.clear_halted()) || x.set_halted());
        self.set_single_step((self.single_step() && !x.clear_single_step()) || x.set_single_step());
        self.set_interrupt_break(
            (self.interrupt_break() && !x.clear_interrupt_break()) || x.set_interrupt_break(),
        );

        if x.clear_interrupt() {
            Some(InterruptChange::Clear)
        } else if x.set_interrupt() {
            Some(InterruptChange::Set)
        } else {
            None
        }
    }
}

bitfield! {
    /// Report whether there is a pending DMA transfer.
    /// See [n64brew](https://n64brew.dev/wiki/Reality_Signal_Processor/Interface#SP_DMA_FULL) for more information.
    pub struct SpDmaFull(u32) {
        /// Mirror of [`SpStatus::dma_full`]
        [0] pub dma_full,
    }
}

impl SpDmaFull {
    pub const OFFSET: usize = 0x14;
}

bitfield! {
    /// Report whether there is a DMA transfer in progress.
    /// See [n64brew](https://n64brew.dev/wiki/Reality_Signal_Processor/Interface#SP_DMA_BUSY) for more information.
    pub struct SpDmaBusy(u32) {
        /// Mirror of [`SpStatus::dma_busy`]
        [0] pub dma_busy,
    }
}

impl SpDmaBusy {
    pub const OFFSET: usize = 0x18;
}

bitfield! {
    /// Register to assist implementing a simple mutex between VR4300 and RSP.
    /// See [n64brew](https://n64brew.dev/wiki/Reality_Signal_Processor/Interface#SP_SEMAPHORE) for more information.
    pub struct SpSemaphore(u32) {
        /// Can be read and written, but after each read, it is always automatically set to true.
        [0] pub semaphore,
    }
}

impl SpSemaphore {
    pub const OFFSET: usize = 0x1C;

    pub fn read(&mut self) -> bool {
        let res = self.semaphore();
        self.set_semaphore(true);
        res
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn read_semaphore() {
        let mut semaphore = SpSemaphore::default();
        assert!(!semaphore.read());
        assert!(semaphore.read());
    }

    #[test]
    fn write_signals() {
        let set_all_raw = 0b1010_1010_1010_1010;
        let mut signals = Signals([false; 8]);
        signals = signals.write(set_all_raw);
        assert_eq!(signals.0, [true; 8]);

        let clear_all_raw = 0b0101_0101_0101_0101;
        let mut signals = Signals([false; 8]);
        signals = signals.write(clear_all_raw);
        assert_eq!(signals.0, [false; 8]);

        let set_msb_raw = 0b1000_0000_0000_0000;
        let mut signals = Signals([false; 8]);
        signals = signals.write(set_msb_raw);
        assert_eq!(
            signals.0,
            [false, false, false, false, false, false, false, true]
        );
    }

    #[test]
    fn write_sp_status() {
        let mut status = SpStatus::default().with_interrupt_break(true);
        let new = SpStatusWrite::default().with_clear_interrupt_break(true);
        let maybe_irq_change = status.write(new.into());
        assert!(!status.interrupt_break());
        assert!(maybe_irq_change.is_none());

        let mut status = SpStatus::default();
        let new = SpStatusWrite::default().with_set_interrupt_break(true);
        let maybe_irq_change = status.write(new.into());
        assert!(status.interrupt_break());
        assert!(maybe_irq_change.is_none());

        let mut status = SpStatus::default();
        let new = SpStatusWrite::default().with_set_interrupt(true);
        let maybe_irq_change = status.write(new.into());
        assert!(maybe_irq_change == Some(InterruptChange::Set));

        let mut status = SpStatus::default();
        let new = SpStatusWrite::default().with_clear_interrupt(true);
        let maybe_irq_change = status.write(new.into());
        assert!(maybe_irq_change == Some(InterruptChange::Clear));

        let mut status = SpStatus::default().with_interrupt_break(true);
        let new = SpStatusWrite::default().with_set_interrupt(true);
        let maybe_irq_change = status.write(new.into());
        assert!(status.interrupt_break());
        assert!(maybe_irq_change == Some(InterruptChange::Set));
    }

    #[test]
    fn dma_len() {
        for i in 0..=7 {
            let read = DmaReadLength::default().0.with_unmasked_length(i);
            assert_eq!(read.length(), 8);
            let write = DmaWriteLength::default().0.with_unmasked_length(i);
            assert_eq!(write.length(), 8);
        }

        for i in 8..=15 {
            let read = DmaReadLength::default().0.with_unmasked_length(i);
            assert_eq!(read.length(), 16);
            let write = DmaWriteLength::default().0.with_unmasked_length(i);
            assert_eq!(write.length(), 16);
        }

        for i in 16..=23 {
            let read = DmaReadLength::default().0.with_unmasked_length(i);
            assert_eq!(read.length(), 24);
            let write = DmaWriteLength::default().0.with_unmasked_length(i);
            assert_eq!(write.length(), 24);
        }
    }
}
