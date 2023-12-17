#![warn(clippy::all, clippy::pedantic)]

use self::memory::{PhysicalAddress, Section};
use std::ops::Range;

pub mod memory;
pub mod utils;

/// A device which can raise or lower an interrupt.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum InterruptDevice {
    SerialInterface,
    AudioInterface,
    VideoInterface,
    PeripheralInterface,
    Rsp,
    Rdp,
}

/// A request to modify the MI interrupt state, either raising or lowering an interrupt.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum InterruptRequest {
    Raise(InterruptDevice),
    Lower(InterruptDevice),
}

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct SideEffects {
    /// A range of memory which has been mutated.
    pub dirty: Option<Range<PhysicalAddress>>,
    /// The interrupt modification to apply to the MI.
    pub interrupt: Option<InterruptRequest>,
}

impl SideEffects {
    #[must_use]
    pub const fn new() -> Self {
        Self {
            dirty: None,
            interrupt: None,
        }
    }

    /// Mark the given range of memory as dirty, which is required when it has been written to.
    ///
    /// # Panics
    /// Panics if the given range is not contiguous, or is not entirely mapped.
    pub fn set_dirty(&mut self, dirty_memory: Range<PhysicalAddress>) {
        assert!(dirty_memory.start < dirty_memory.end);
        assert!(
            Section::from_address(dirty_memory.start).is_ok()
                && Section::from_address(dirty_memory.end - 1).is_ok()
        );
        self.dirty = Some(dirty_memory);
    }

    /// Mark the given range of offsets in the given section as dirty, which is required when it has been written to.
    ///
    /// # Panics
    /// Panics if the given range is not contiguous, or the offsets are not in the sections bounds.
    pub fn set_dirty_section(&mut self, section: Section, offsets: Range<usize>) {
        let range = section.offset(offsets.start)..section.offset(offsets.end);
        self.set_dirty(range);
    }

    /// Raise or lower the given interrupt.
    pub fn set_interrupt(&mut self, interrupt: InterruptRequest) {
        self.interrupt = Some(interrupt);
    }

    /// Raise an interrupt for the given device.
    pub fn raise_interrupt(&mut self, device: InterruptDevice) {
        self.set_interrupt(InterruptRequest::Raise(device));
    }

    /// Lower an interrupt for the given device.
    pub fn lower_interrupt(&mut self, device: InterruptDevice) {
        self.set_interrupt(InterruptRequest::Lower(device));
    }
}
