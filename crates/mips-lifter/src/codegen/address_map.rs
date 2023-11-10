#![deny(clippy::all, clippy::pedantic)]

use crate::label::LabelWithContext;
use std::ops::Range;

#[allow(clippy::module_name_repetitions)]
#[derive(Debug, Default)]
pub struct VirtualAddressMap<'ctx> {
    labels: Vec<LabelWithContext<'ctx>>,
}

impl<'ctx> VirtualAddressMap<'ctx> {
    pub const fn new() -> Self {
        Self { labels: Vec::new() }
    }

    /// On success this returns the label that contains the given virtual address,
    /// otherwise the index where it should be inserted while maintaining the sort order, which is meant to be used with [`insert`].
    pub fn get(&self, vaddr: u64) -> Result<&LabelWithContext<'ctx>, usize> {
        self.labels
            .binary_search_by_key(&vaddr, |l| l.label.start() as u64)
            // SAFETY: binary_search_by_key returns the index of the found element, which is always in bounds.
            .map(|i| unsafe { self.labels.get_unchecked(i) })
    }

    /// Inserts the given label at the given index. After insertion, the labels must still be sorted by starting address.
    /// The index is obtained by the error case of the [`get`] method, when no existing label could be found.
    pub unsafe fn insert(&mut self, index: usize, label: LabelWithContext<'ctx>) {
        self.labels.insert(index, label);
    }

    /// Removes every label that resides within the given range of virtual addresses.
    pub fn remove_within_range(&mut self, range: Range<u64>) {
        let start = {
            let start = self
                .labels
                .partition_point(|l| (l.label.start() as u64) < range.start);
            (0..start)
                .rev()
                .take_while(|&i| {
                    // SAFETY: We iterate backwards from start, the index is always in bounds.
                    let label = unsafe { self.labels.get_unchecked(i) };
                    #[allow(clippy::cast_possible_truncation)] // Limited by range() already
                    label.label.range().contains(&(range.start as usize))
                })
                .last()
                .unwrap_or(start)
        };

        let end = {
            let end = self
                .labels
                .partition_point(|l| (l.label.start() as u64) < range.end);
            (end..self.labels.len())
                .take_while(|&i| {
                    // SAFETY: We iterate until labels.len(), the index is always in bounds.
                    let label = unsafe { self.labels.get_unchecked(i) };
                    #[allow(clippy::cast_possible_truncation)] // Limited by range() already
                    label.label.range().contains(&(range.end as usize))
                })
                .last()
                .unwrap_or(end)
        };

        if start != end {
            self.labels.drain(start..end);
        }
    }
}
