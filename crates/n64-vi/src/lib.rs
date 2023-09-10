//! The Video Interface (VI), used to control the frame buffer of the N64.

use std::ops::RangeInclusive;
use tartan_bitfield::bitfield;

const fn offset(index: usize) -> usize {
    index * std::mem::size_of::<u32>()
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum PixelType {
    /// 32-bit RGBA
    R8G8B8A8,
    /// 18-bit RGBA
    R5G5B5A3,
    /// Should not be used
    Reserved,
    /// No data and no sync
    Blank,
}

impl From<u8> for PixelType {
    fn from(value: u8) -> Self {
        match value {
            0b11 => Self::R8G8B8A8,
            0b10 => Self::R5G5B5A3,
            0b01 => Self::Reserved,
            0b00 => Self::Blank,
            _ => unreachable!(),
        }
    }
}

impl From<PixelType> for u8 {
    fn from(value: PixelType) -> Self {
        match value {
            PixelType::R8G8B8A8 => PixelType::R8G8B8A8.into(),
            PixelType::R5G5B5A3 => PixelType::R5G5B5A3.into(),
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
    /// https://n64brew.dev/wiki/Video_Interface#0x0440_0000_-_VI_CTRL
    pub struct Control(u32) {
        [0..1] pixel_type: u8 as PixelType,
        [2] gamma_dither,
        [3] gamma,
        [4] divot,
        [5] vbus_clock,
        [6] serrate,
        [7] test_mode,
        [8..=9] anti_aliasing_mode: u8 as AntiAliasingMode,
        [11] kill,
        [12..=15] pixel_advance: u8,
        [16] dither_filter,
    }
}

impl Control {
    const OFFSET: usize = offset(0);
}

bitfield! {
    /// https://n64brew.dev/wiki/Video_Interface#0x0440_0004_-_VI_ORIGIN
    pub struct Origin(u32) {
        [0..=23] origin: u32,
    }
}

impl Origin {
    const OFFSET: usize = offset(1);
}

bitfield! {
    /// https://n64brew.dev/wiki/Video_Interface#0x0440_0008_-_VI_WIDTH
    pub struct Width(u32) {
        [0..=11] width: u16,
    }
}

impl Width {
    const OFFSET: usize = offset(2);
}

bitfield! {
    /// https://n64brew.dev/wiki/Video_Interface#0x0440_000C_-_VI_V_INTR
    pub struct Interrupt(u32) {
        [0..=9] half_line: u16,
    }
}

impl Interrupt {
    const OFFSET: usize = offset(3);

    const fn new() -> Self {
        Self(0x3FF)
    }
}

bitfield! {
    /// https://n64brew.dev/wiki/Video_Interface#0x0440_0010_-_VI_V_CURRENT
    pub struct Current(u32) {
        [0..=9] half_line: u16,
    }
}

impl Current {
    const OFFSET: usize = offset(4);
}

bitfield! {
    /// https://n64brew.dev/wiki/Video_Interface#0x0440_0014_-_VI_BURST
    pub struct Burst(u32) {
        [0..=7] hsync_width: u8,
        [8..=15] burst_width: u8,
        [16..=19] vsync_width: u8,
        [20..=29] start: u16,
    }
}

impl Burst {
    const OFFSET: usize = offset(5);

    const fn new() -> Self {
        Self(0x1)
    }
}

bitfield! {
    /// https://n64brew.dev/wiki/Video_Interface#0x0440_0018_-_VI_V_SYNC
    pub struct VerticalSync(u32) {
        [0..=9] scanlines: u16,
    }
}

impl VerticalSync {
    const OFFSET: usize = offset(6);
}

bitfield! {
    /// https://n64brew.dev/wiki/Video_Interface#0x0440_001C_-_VI_H_SYNC
    pub struct HorizontalSync(u32) {
        [0..=11] line_width: u16,
        [16..=20] leap: u8,
    }
}

impl HorizontalSync {
    const OFFSET: usize = offset(7);

    const fn new() -> Self {
        Self(0x7FF)
    }
}

bitfield! {
    /// https://n64brew.dev/wiki/Video_Interface#0x0440_0020_-_VI_H_SYNC_LEAP
    pub struct HorizontalSyncLeap(u32) {
        [0..=11] a: u16,
        [16..=27] b: u16,
    }
}

impl HorizontalSyncLeap {
    const OFFSET: usize = offset(8);
}

bitfield! {
    /// https://n64brew.dev/wiki/Video_Interface#0x0440_0024_-_VI_H_VIDEO
    pub struct HorizontalVideo(u32) {
        [0..=9] end: u16,
        [16..=25] start: u16,
    }
}

impl HorizontalVideo {
    const OFFSET: usize = offset(9);
}

bitfield! {
    /// https://n64brew.dev/wiki/Video_Interface#0x0440_0028_-_VI_V_VIDEO
    pub struct VerticalVideo(u32) {
        [0..=9] end: u16,
        [16..=25] start: u16,
    }
}

impl VerticalVideo {
    const OFFSET: usize = offset(10);
}

bitfield! {
    /// https://n64brew.dev/wiki/Video_Interface#0x0440_002C_-_VI_V_BURST
    pub struct VerticalBurst(u32) {
        [0..=9] end: u16,
        [16..=25] start: u16,
    }
}

impl VerticalBurst {
    const OFFSET: usize = offset(11);
}

bitfield! {
    /// https://n64brew.dev/wiki/Video_Interface#0x0440_0030_-_VI_X_SCALE
    pub struct XScale(u32) {
        // TODO: parse 2.10 format
        [0..=11] scale_factor: u16,
        [16..=27] subpixel_offset: u16,
    }
}

impl XScale {
    const OFFSET: usize = offset(12);
}

bitfield! {
    /// https://n64brew.dev/wiki/Video_Interface#0x0440_0034_-_VI_Y_SCALE
    pub struct YScale(u32) {
        // TODO: parse 2.10 format
        [0..=11] scale_factor: u16,
        [16..=27] subpixel_offset: u16,
    }
}

impl YScale {
    const OFFSET: usize = offset(13);
}

bitfield! {
    /// https://n64brew.dev/wiki/Video_Interface#0x0440_0038_-_VI_TEST_ADDR
    pub struct TestAddress(u32) {
        [0..=6] address: u8,
    }
}

impl TestAddress {
    const OFFSET: usize = offset(14);
}

bitfield! {
    /// https://n64brew.dev/wiki/Video_Interface#0x0440_003C_-_VI_STAGED_DATA
    pub struct StagedData(u32) {
        [0..=31] data: u32,
    }
}

impl StagedData {
    const OFFSET: usize = offset(15);
}

#[derive(Debug)]
pub struct VideoInterface {
    control: Control,
    origin: Origin,
    width: Width,
    interrupt: Interrupt,
    current: Current,
    burst: Burst,
    vertical_sync: VerticalSync,
    horizontal_sync: HorizontalSync,
    horizontal_sync_leap: HorizontalSyncLeap,
    horizontal_video: HorizontalVideo,
    vertical_video: VerticalVideo,
    vertical_burst: VerticalBurst,
    xscale: XScale,
    yscale: YScale,
    test_address: TestAddress,
    staged_data: StagedData,
}

impl VideoInterface {
    pub fn new() -> Self {
        Self {
            control: Control::default(),
            origin: Origin::default(),
            width: Width::default(),
            interrupt: Interrupt::new(),
            current: Current::default(),
            burst: Burst::new(),
            vertical_sync: VerticalSync::default(),
            horizontal_sync: HorizontalSync::new(),
            horizontal_sync_leap: HorizontalSyncLeap::default(),
            horizontal_video: HorizontalVideo::default(),
            vertical_video: VerticalVideo::default(),
            vertical_burst: VerticalBurst::default(),
            xscale: XScale::default(),
            yscale: YScale::default(),
            test_address: TestAddress::default(),
            staged_data: StagedData::default(),
        }
    }

    const fn normalise_offset(offset: usize) -> usize {
        offset % StagedData::OFFSET // StagedData is the last register, so we mirror from there.
    }

    pub fn read(&self, offset: usize) -> Option<u32> {
        let value = match Self::normalise_offset(offset) {
            Control::OFFSET => self.control.into(),
            Origin::OFFSET => self.origin.into(),
            Width::OFFSET => self.width.into(),
            Interrupt::OFFSET => self.interrupt.into(),
            Current::OFFSET => self.current.into(),
            Burst::OFFSET => self.burst.into(),
            VerticalSync::OFFSET => self.vertical_sync.into(),
            HorizontalSync::OFFSET => self.horizontal_sync.into(),
            HorizontalSyncLeap::OFFSET => self.horizontal_sync_leap.into(),
            HorizontalVideo::OFFSET => self.horizontal_video.into(),
            VerticalVideo::OFFSET => self.vertical_video.into(),
            VerticalBurst::OFFSET => self.vertical_burst.into(),
            XScale::OFFSET => self.xscale.into(),
            YScale::OFFSET => self.yscale.into(),
            TestAddress::OFFSET => self.test_address.into(),
            StagedData::OFFSET => self.staged_data.into(),
            _ => unreachable!(),
        };
        Some(value)
    }

    pub fn write(&mut self, offset: usize, value: u32) -> Option<()> {
        match Self::normalise_offset(offset) {
            Control::OFFSET => self.control = Control::from(value),
            Origin::OFFSET => self.origin = Origin::from(value),
            Width::OFFSET => self.width = Width::from(value),
            Interrupt::OFFSET => self.interrupt = Interrupt::from(value),
            Current::OFFSET => self.current = Current::from(value),
            Burst::OFFSET => self.burst = Burst::from(value),
            VerticalSync::OFFSET => self.vertical_sync = VerticalSync::from(value),
            HorizontalSync::OFFSET => self.horizontal_sync = HorizontalSync::from(value),
            HorizontalSyncLeap::OFFSET => {
                self.horizontal_sync_leap = HorizontalSyncLeap::from(value)
            }
            HorizontalVideo::OFFSET => self.horizontal_video = HorizontalVideo::from(value),
            VerticalVideo::OFFSET => self.vertical_video = VerticalVideo::from(value),
            VerticalBurst::OFFSET => self.vertical_burst = VerticalBurst::from(value),
            XScale::OFFSET => self.xscale = XScale::from(value),
            YScale::OFFSET => self.yscale = YScale::from(value),
            TestAddress::OFFSET => self.test_address = TestAddress::from(value),
            StagedData::OFFSET => self.staged_data = StagedData::from(value),
            _ => unreachable!(),
        }
        Some(())
    }

    pub fn origin(&self) -> usize {
        // TODO: this is a hack to get the vaddr instead of paddr, assumes the entire vaddr is written
        self.origin.0 as usize
    }

    pub fn width(&self) -> usize {
        let scale = (self.xscale.scale_factor() / 1024) as usize;
        let length = (self.horizontal_video.end() - self.horizontal_video.start()) as usize;
        length * scale
    }

    pub fn height(&self) -> usize {
        let scale = (self.yscale.scale_factor() / 1024) as usize;
        let length = ((self.vertical_video.end() - self.vertical_video.start()) >> 1) as usize;
        let result = length * scale;
        match result {
            474 => 480, // TODO: this is obviously wrong, maybe because we're not parsing the 2.10 format?
            _ => result,
        }
    }

    pub fn framebuffer_range(&self) -> RangeInclusive<usize> {
        let origin = self.origin.origin() as usize;
        let pixel_size = 4; // TODO: dont assume 32-bit RGBA
        origin..=(origin + (self.width() * self.height()) * pixel_size)
    }
}

impl Default for VideoInterface {
    fn default() -> Self {
        Self::new()
    }
}
