//! The Video Interface (VI), used to control the frame buffer of the N64.

use self::register::{
    Burst, Control, Current, HorizontalSync, HorizontalSyncLeap, HorizontalVideo, Interrupt,
    Origin, StagedData, TestAddress, VerticalBurst, VerticalSync, VerticalVideo, Width, XScale,
    YScale,
};
use std::ops::RangeInclusive;

mod register;

const COUNTER_START: u32 = ((62500000.0 / 60.0) + 1.0) as u32;
const BLANKING_DONE: u32 = (COUNTER_START as f32 - (COUNTER_START as f32 / (525.0 * 39.0))) as u32;

#[derive(Debug, Default)]
pub struct SideEffects {
    pub lower_interrupt: bool,
    pub raise_interrupt: bool,
}

#[derive(Debug)]
pub struct VideoInterface {
    // Registers
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

    field: bool,
    counter: u32,
    interrupt_counter: u32,
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
            field: false,
            counter: COUNTER_START,
            interrupt_counter: 0,
        }
    }

    const fn normalise_offset(offset: usize) -> usize {
        offset % StagedData::OFFSET // StagedData is the last register, so we mirror from there.
    }

    fn current_halfline(&self) -> u16 {
        let vsync_lines = COUNTER_START / self.vertical_sync.scanlines() as u32;
        ((COUNTER_START - self.counter) / vsync_lines) as u16
    }

    pub fn read(&mut self, offset: usize) -> Option<u32> {
        if self.vertical_sync.scanlines() != 0 {
            let mut current = Current::default().with_halfline(self.current_halfline());
            if !self.vertical_sync.field() {
                current = current.with_field(true);
            }

            self.current = current;
        } else {
            self.current = Current::default();
        }

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

    pub fn write(&mut self, offset: usize, value: u32) -> Option<SideEffects> {
        let mut side_effects = SideEffects::default();
        match Self::normalise_offset(offset) {
            Control::OFFSET => self.control = Control::from(value),
            Origin::OFFSET => self.origin = Origin::from(value),
            Width::OFFSET => self.width = Width::from(value),
            Interrupt::OFFSET => {
                self.interrupt = Interrupt::from(value);
                self.interrupt_counter = COUNTER_START - (COUNTER_START / (525 * (value >> 1)));
            }
            Current::OFFSET => side_effects.lower_interrupt = true,
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
        Some(side_effects)
    }

    #[must_use]
    pub fn tick(&mut self) -> SideEffects {
        self.counter = self.counter.checked_sub(1).unwrap_or(COUNTER_START);
        if self.counter == BLANKING_DONE {
            self.field = !self.field;
        }

        SideEffects {
            raise_interrupt: self.counter == self.interrupt_counter,
            ..Default::default()
        }
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
