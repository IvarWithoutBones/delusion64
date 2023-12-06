use crate::{InterruptChange, MemoryBank, RspError, RspResult, SideEffects};
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
    pub struct DmaDramAddress(u32) {
        [0..=23] unmasked_address: u32,
    }
}

impl DmaDramAddress {
    pub const OFFSET: usize = 0x04;

    /// RDRAM address used in SP DMAs
    #[must_use]
    pub fn address(self) -> u32 {
        self.unmasked_address() & !0b111
    }
}

impl fmt::Debug for DmaDramAddress {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("DmaDramAddress")
            .field("address", &self.address())
            .finish()
    }
}

bitfield_without_debug! {
    /// Length of a DMA transfer. Writing this register triggers a DMA transfer from RDRAM to IMEM/DMEM.
    /// See [n64brew](https://n64brew.dev/wiki/Reality_Signal_Processor/Interface#SP_DMA_RDLEN) for more information.
    pub struct DmaReadLength(u32) {
        [0..=11] unmasked_length: u32,
        /// Number of rows to transfer minus 1.
        [12..=19] pub count: u8,
        [21..=31] unmasked_skip: u32,
    }
}

impl DmaReadLength {
    pub const OFFSET: usize = 0x08;

    /// Number of bytes to transfer for each row minus 1
    #[must_use]
    pub fn length(self) -> u32 {
        self.unmasked_length() & !0b111
    }

    /// Number of bytes to skip in RDRAM after each row
    #[must_use]
    pub fn skip(self) -> u32 {
        self.unmasked_skip() & !0b111
    }
}

impl fmt::Debug for DmaReadLength {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("DmaReadLength")
            .field("length", &self.length())
            .field("count", &self.count())
            .field("skip", &self.skip())
            .finish()
    }
}

bitfield_without_debug! {
    /// Length of a DMA transfer. Writing this register triggers a DMA transfer from IMEM/DMEM to RDRAM.
    /// See [n64brew](https://n64brew.dev/wiki/Reality_Signal_Processor/Interface#SP_DMA_WRLEN) for more information.
    pub struct DmaWriteLength(u32) {
        [0..=11] unmasked_length: u32,
        /// Number of rows to transfer minus 1.
        [12..=19] pub count: u8,
        [21..=31] unmasked_skip: u32,
    }
}

impl DmaWriteLength {
    pub const OFFSET: usize = 0x0C;

    /// Number of bytes to transfer for each row minus 1
    #[must_use]
    pub fn length(self) -> u32 {
        self.unmasked_length() & !0b111
    }

    /// Number of bytes to skip in RDRAM after each row
    #[must_use]
    pub fn skip(self) -> u32 {
        self.unmasked_skip() & !0b111
    }
}

impl fmt::Debug for DmaWriteLength {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("DmaWriteLength")
            .field("length", &self.length())
            .field("count", &self.count())
            .field("skip", &self.skip())
            .finish()
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

#[derive(Debug)]
struct DoubleBuffered<T> {
    current: T,
    next: T,
}

impl<T: Default> Default for DoubleBuffered<T> {
    fn default() -> Self {
        Self {
            current: T::default(),
            next: T::default(),
        }
    }
}

impl<T> DoubleBuffered<T> {
    fn get(&self) -> &T {
        &self.current
    }

    fn get_mut(&mut self) -> &mut T {
        &mut self.current
    }

    fn next(&mut self) {
        std::mem::swap(&mut self.current, &mut self.next);
    }
}

#[derive(Debug, Default)]
pub struct Registers {
    dma_sp_address: DoubleBuffered<DmaSpAddress>,
    dma_dram_address: DoubleBuffered<DmaDramAddress>,
    dma_read_length: DoubleBuffered<DmaReadLength>,
    dma_write_length: DoubleBuffered<DmaWriteLength>,
    // Note that [`SpDmaFull`] and [`SpDmaBusy`] are omitted here, since they're mirrors of [`SpStatus`]
    sp_status: SpStatus,
    sp_semaphore: SpSemaphore,
}

impl Registers {
    const OFFSET_MASK: usize = 0x1C;

    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    /// Read a 32-bit integer from the RSP's memory-mapped COP0 registers
    ///
    /// # Errors
    /// Returns an error if the given offset is out of bounds
    pub fn read(&mut self, offset: usize) -> RspResult<u32> {
        Ok(match offset & Self::OFFSET_MASK {
            DmaSpAddress::OFFSET => self.dma_sp_address.get().to_owned().into(),
            DmaDramAddress::OFFSET => self.dma_dram_address.get().to_owned().into(),
            DmaReadLength::OFFSET => self.dma_read_length.get().to_owned().into(),
            DmaWriteLength::OFFSET => self.dma_write_length.get().to_owned().into(),
            SpStatus::OFFSET => self.sp_status.into(),
            SpDmaFull::OFFSET => self.sp_status.dma_full().into(),
            SpDmaBusy::OFFSET => self.sp_status.dma_busy().into(),
            SpSemaphore::OFFSET => self.sp_semaphore.read().into(),
            _ => Err(RspError::RegisterOffsetOutOfBounds { offset })?,
        })
    }

    /// Write a 32-bit integer to the RSP's memory-mapped COP0 registers
    ///
    /// # Errors
    /// Returns an error if the given offset is out of bounds
    pub fn write(&mut self, offset: usize, value: u32) -> RspResult<SideEffects> {
        // TODO: DMA transfers
        let mut side_effects = SideEffects::default();
        match offset & Self::OFFSET_MASK {
            DmaSpAddress::OFFSET => *self.dma_sp_address.get_mut() = value.into(),
            DmaDramAddress::OFFSET => *self.dma_dram_address.get_mut() = value.into(),
            DmaReadLength::OFFSET => *self.dma_read_length.get_mut() = value.into(),
            DmaWriteLength::OFFSET => *self.dma_write_length.get_mut() = value.into(),
            SpStatus::OFFSET => side_effects.interrupt = self.sp_status.write(value),
            SpDmaFull::OFFSET => self.sp_status.set_dma_full(SpDmaFull(value).dma_full()),
            SpDmaBusy::OFFSET => self.sp_status.set_dma_busy(SpDmaBusy(value).dma_busy()),
            SpSemaphore::OFFSET => self.sp_semaphore = value.into(),
            _ => Err(RspError::RegisterOffsetOutOfBounds { offset })?,
        }
        Ok(side_effects)
    }

    #[allow(dead_code)]
    fn start_dma(&mut self) {
        self.dma_sp_address.next();
        self.dma_dram_address.next();
        self.dma_read_length.next();
        self.dma_write_length.next();
    }
}

#[cfg(test)]
mod test {
    use super::*;

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
}
