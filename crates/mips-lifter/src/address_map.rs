#![warn(clippy::all, clippy::pedantic)]

use crate::{
    label::LabelWithContext,
    target::{Label, Target},
};
use inkwell::execution_engine::ExecutionEngine;
use log::trace;
use std::ops::Range;

#[allow(clippy::module_name_repetitions)]
#[derive(Debug, Default)]
pub struct VirtualAddressMap<'ctx, T: Target> {
    inner: Vec<LabelWithContext<'ctx, T>>,
}

impl<'ctx, T: Target> VirtualAddressMap<'ctx, T> {
    pub const fn new() -> Self {
        Self { inner: Vec::new() }
    }

    /// Finds a label that starts with the given virtual address. If no such label exists, the index where it could be inserted is returned.
    pub fn get(&self, vaddr: u64) -> Result<&LabelWithContext<'ctx, T>, usize> {
        self.inner
            .binary_search_by_key(&vaddr, |l| l.label.start())
            // SAFETY: binary_search_by_key returns the index of the found element, which is always in bounds.
            .map(|i| unsafe { self.inner.get_unchecked(i) })
    }

    /// An iterator over all labels that contain the given virtual address.
    pub fn get_containing(&self, vaddr: u64) -> impl Iterator<Item = &LabelWithContext<'ctx, T>> {
        #[allow(clippy::range_plus_one)] // `indices_containing` does not accept RangeInclusive.
        self.indices_containing(&(vaddr..vaddr + 1))
            .into_iter()
            .flat_map(|range| self.inner[range].iter())
    }

    /// Inserts the given label at the given index. After insertion, the labels must still be sorted by starting address.
    /// The index can safely be obtained by calling [`Self::get`], assuming no mutation occurs between the calls.
    pub unsafe fn insert(&mut self, index: usize, label: LabelWithContext<'ctx, T>) {
        self.inner.insert(index, label);
    }

    /// Get a mutable reference to the inner vector.
    ///
    /// # Safety
    /// The caller must ensure that the vector is not modified in a way that would break the sort order.
    pub unsafe fn inner_mut(&mut self) -> &mut Vec<LabelWithContext<'ctx, T>> {
        &mut self.inner
    }

    /// Removes every label that resides within the given range of virtual addresses.
    pub fn remove_within_range(&mut self, range: &Range<u64>, exec: &ExecutionEngine<'ctx>) {
        if let Some(indices) = self.indices_containing(range) {
            for label in self.inner.drain(indices) {
                trace!("removing label at {:#x?}", label.label.range());
                let module = label.module.expect("module should be present");
                exec.remove_module(&module)
                    .expect("failed to remove module");
            }
        }
    }

    fn indices_containing(&self, addrs: &Range<u64>) -> Option<Range<usize>> {
        // This is a bit tricky because of the backtracking, this is the idea:
        // 1. Find the index of the first label that starts at the given address, or the place it could be inserted.
        // 2. Iterate backwards until the address no longer falls within the range of the label.
        //    This is needed because different labels may overlap due to branches.
        // 3. Take the index of the matching label with the lowest starting address, or return None.
        // 4. Iterate forwards until the address no longer falls within the range of the label.
        let overlaps = |a: &Range<u64>, b: &Range<u64>| (a.start < b.end) && (b.start < a.end);
        let start = (0..=self
            .inner
            .binary_search_by_key(&addrs.start, |l| l.label.start())
            .map_or_else(|i| i.saturating_sub(1), |i| i))
            .rev()
            .take_while(|&i| {
                self.inner
                    .get(i)
                    .is_some_and(|l| overlaps(&l.label.range(), addrs))
            })
            .last()?;
        let end = self.inner[start..]
            .iter()
            .take_while(|l| overlaps(&l.label.range(), addrs))
            .count()
            .checked_add(start)?;
        Some(start..end)
    }
}
