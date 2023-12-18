use n64_common::utils::tartan_bitfield::bitfield;

use crate::ViError;

const fn offset(index: usize) -> usize {
    index * std::mem::size_of::<u32>()
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum PixelType {
    /// 32-bit RGBA
    RGBA8888,
    /// 18-bit RGBA
    RGBA5551,
    /// Should not be used
    Reserved,
    /// No data and no sync
    Blank,
}

impl PixelType {
    pub fn bytes_per_pixel(self) -> Result<usize, ViError> {
        match self {
            PixelType::RGBA8888 => Ok(4),
            PixelType::RGBA5551 => Ok(2),
            PixelType::Reserved | PixelType::Blank => {
                Err(ViError::InvalidPixelType { pixel_type: self })
            }
        }
    }
}

impl From<u8> for PixelType {
    fn from(value: u8) -> Self {
        match value {
            0b11 => Self::RGBA8888,
            0b10 => Self::RGBA5551,
            0b01 => Self::Reserved,
            0b00 => Self::Blank,
            _ => unreachable!(),
        }
    }
}

impl From<PixelType> for u8 {
    fn from(value: PixelType) -> Self {
        match value {
            PixelType::RGBA8888 => PixelType::RGBA8888.into(),
            PixelType::RGBA5551 => PixelType::RGBA5551.into(),
            PixelType::Reserved => PixelType::Reserved.into(),
            PixelType::Blank => PixelType::Blank.into(),
        }
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum AntiAliasingMode {
    /// Anti-aliasing and resampling is disabled.
    NoInterpolation,
    /// Anti-aliasing is disabled, and resampling enabled. Operate as if everything is covered.
    EverythingCovered,
    /// Anti-aliasing and resampling enabled. Only fetches extra lines as needed.
    FetchLinesAsNeeded,
    /// Anti-aliasing and resampling enabled. Always fetches extra lines.
    AlwaysFetchLines,
}

impl From<u8> for AntiAliasingMode {
    fn from(value: u8) -> Self {
        match value {
            0b11 => AntiAliasingMode::NoInterpolation,
            0b10 => AntiAliasingMode::EverythingCovered,
            0b01 => AntiAliasingMode::FetchLinesAsNeeded,
            0b00 => AntiAliasingMode::AlwaysFetchLines,
            _ => unreachable!(),
        }
    }
}

impl From<AntiAliasingMode> for u8 {
    fn from(value: AntiAliasingMode) -> Self {
        match value {
            AntiAliasingMode::NoInterpolation => AntiAliasingMode::NoInterpolation.into(),
            AntiAliasingMode::EverythingCovered => AntiAliasingMode::EverythingCovered.into(),
            AntiAliasingMode::FetchLinesAsNeeded => AntiAliasingMode::FetchLinesAsNeeded.into(),
            AntiAliasingMode::AlwaysFetchLines => AntiAliasingMode::AlwaysFetchLines.into(),
        }
    }
}

bitfield! {
    /// See [n64brew](https://n64brew.dev/wiki/Video_Interface#0x0440_0000_-_VI_CTRL).
    pub struct Control(u32) {
        [0..=1] pub pixel_type: u8 as PixelType,
        [2] pub gamma_dither,
        [3] pub gamma,
        [4] pub divot,
        [5] pub vbus_clock,
        [6] pub serrate,
        [7] pub test_mode,
        [8..=9] pub anti_aliasing_mode: u8 as AntiAliasingMode,
        [11] pub kill,
        [12..=15] pub pixel_advance: u8,
        [16] pub dither_filter,
    }
}

impl Control {
    pub const OFFSET: usize = offset(0);
}

bitfield! {
    /// See [n64brew](https://n64brew.dev/wiki/Video_Interface#0x0440_0004_-_VI_ORIGIN).
    pub struct Origin(u32) {
        [0..=23] pub origin: u32,
    }
}

impl Origin {
    pub const OFFSET: usize = offset(1);
}

bitfield! {
    /// See [n64brew](https://n64brew.dev/wiki/Video_Interface#0x0440_0008_-_VI_WIDTH).
    pub struct Width(u32) {
        [0..=11] pub width: u16,
    }
}

impl Width {
    pub const OFFSET: usize = offset(2);
}

bitfield! {
    /// See [n64brew](https://n64brew.dev/wiki/Video_Interface#0x0440_000C_-_VI_V_INTR).
    pub struct Interrupt(u32) {
        [0..=9] pub halfline: u16,
    }
}

impl Interrupt {
    pub const OFFSET: usize = offset(3);

    pub const fn new() -> Self {
        Self(0x3FF)
    }
}

bitfield! {
    /// See [n64brew](https://n64brew.dev/wiki/Video_Interface#0x0440_0010_-_VI_V_CURRENT).
    pub struct Current(u32) {
        [0] pub field,
        [1..=9] pub halfline: u16,
    }
}

impl Current {
    pub const OFFSET: usize = offset(4);
}

bitfield! {
    /// See [n64brew](https://n64brew.dev/wiki/Video_Interface#0x0440_0014_-_VI_BURST).
    pub struct Burst(u32) {
        [0..=7] pub hsync_width: u8,
        [8..=15] pub burst_width: u8,
        [16..=19] pub vsync_width: u8,
        [20..=29] pub start: u16,
    }
}

impl Burst {
    pub const OFFSET: usize = offset(5);

    pub const fn new() -> Self {
        Self(0x1)
    }
}

bitfield! {
    /// See [n64brew](https://n64brew.dev/wiki/Video_Interface#0x0440_0018_-_VI_V_SYNC).
    pub struct VerticalSync(u32) {
        [0] pub field,
        [1..=9] pub scanlines: u16,
    }
}

impl VerticalSync {
    pub const OFFSET: usize = offset(6);
}

bitfield! {
    /// See [n64brew](https://n64brew.dev/wiki/Video_Interface#0x0440_001C_-_VI_H_SYNC).
    pub struct HorizontalSync(u32) {
        [0..=11] pub line_width: u16,
        [16..=20] pub leap: u8,
    }
}

impl HorizontalSync {
    pub const OFFSET: usize = offset(7);

    pub const fn new() -> Self {
        Self(0x7FF)
    }
}

bitfield! {
    /// See [n64brew](https://n64brew.dev/wiki/Video_Interface#0x0440_0020_-_VI_H_SYNC_LEAP).
    pub struct HorizontalSyncLeap(u32) {
        [0..=11] pub a: u16,
        [16..=27] pub b: u16,
    }
}

impl HorizontalSyncLeap {
    pub const OFFSET: usize = offset(8);
}

bitfield! {
    /// See [n64brew](https://n64brew.dev/wiki/Video_Interface#0x0440_0024_-_VI_H_VIDEO).
    pub struct HorizontalVideo(u32) {
        [0..=9] pub end: u16,
        [16..=25] pub start: u16,
    }
}

impl HorizontalVideo {
    pub const OFFSET: usize = offset(9);
}

bitfield! {
    /// See [n64brew](https://n64brew.dev/wiki/Video_Interface#0x0440_0028_-_VI_V_VIDEO).
    pub struct VerticalVideo(u32) {
        [0..=9] pub end: u16,
        [16..=25] pub start: u16,
    }
}

impl VerticalVideo {
    pub const OFFSET: usize = offset(10);
}

bitfield! {
    /// See [n64brew](https://n64brew.dev/wiki/Video_Interface#0x0440_002C_-_VI_V_BURST).
    pub struct VerticalBurst(u32) {
        [0..=9] pub end: u16,
        [16..=25] pub start: u16,
    }
}

impl VerticalBurst {
    pub const OFFSET: usize = offset(11);
}

bitfield! {
    /// See [n64brew](https://n64brew.dev/wiki/Video_Interface#0x0440_0030_-_VI_X_SCALE).
    pub struct XScale(u32) {
        // TODO: parse 2.10 format
        [0..=11] pub scale: u16,
        [16..=27] pub subpixel: u16,
    }
}

impl XScale {
    pub const OFFSET: usize = offset(12);
}

bitfield! {
    /// See [n64brew](https://n64brew.dev/wiki/Video_Interface#0x0440_0034_-_VI_Y_SCALE).
    pub struct YScale(u32) {
        // TODO: parse 2.10 format
        [0..=11] pub scale: u16,
        [16..=27] pub subpixel: u16,
    }
}

impl YScale {
    pub const OFFSET: usize = offset(13);
}

bitfield! {
    /// See [n64brew](https://n64brew.dev/wiki/Video_Interface#0x0440_0038_-_VI_TEST_ADDR).
    pub struct TestAddress(u32) {
        [0..=6] pub address: u8,
    }
}

impl TestAddress {
    pub const OFFSET: usize = offset(14);
}

bitfield! {
    /// See [n64brew](https://n64brew.dev/wiki/Video_Interface#0x0440_003C_-_VI_STAGED_DATA).
    pub struct StagedData(u32) {
        [0..=31] pub data: u32,
    }
}

impl StagedData {
    pub const OFFSET: usize = offset(15);
}
