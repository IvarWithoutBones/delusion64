//! Common tools for implementing the N64's hardware.

pub use tartan_bitfield;
pub use thiserror;

/// Create a fixed-sized boxed array of the given length with default values.
///
/// # Examples
/// ```
/// let array: Box<[u8; 4]> = n64_common::utils::boxed_array();
/// assert_eq!(*array, [0, 0, 0, 0]);
/// ```
#[must_use]
pub fn boxed_array<T: Default, const LEN: usize>() -> Box<[T; LEN]> {
    // Use a Vec to allocate directly onto the heap, avoiding potential stack overflows.
    // Note that using `vec![]` would impose a `T: Clone` bound, which we don't need when collecting from an iterator.
    let result = (0..LEN)
        .map(|_| T::default())
        .collect::<Vec<_>>()
        .into_boxed_slice();
    // SAFETY: The length of the array matches `LEN`.
    unsafe { result.try_into().unwrap_unchecked() }
}
