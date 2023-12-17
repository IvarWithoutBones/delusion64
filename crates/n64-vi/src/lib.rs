//! The Video Interface (VI), used to control the frame buffer of the N64.

use self::register::{
    Burst, Control, Current, HorizontalSync, HorizontalSyncLeap, HorizontalVideo, Interrupt,
    Origin, PixelType, StagedData, TestAddress, VerticalBurst, VerticalSync, VerticalVideo, Width,
    XScale, YScale,
};
use n64_common::{InterruptDevice, SideEffects};

mod register;

const COUNTER_START: u32 = ((62500000.0 / 60.0) + 1.0) as u32;
const BLANKING_DONE: u32 = (COUNTER_START as f32 - (COUNTER_START as f32 / (525.0 * 39.0))) as u32;

const fn rgba5551_to_rgba8888_color(mut color: u16) -> u8 {
    color &= 0b1_1111;
    (color | (color << 3)) as u8
}

#[derive(Debug)]
pub struct FrameBuffer {
    pub width: usize,
    pub height: usize,
    pub pixels: Box<[u8]>,
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
    pub vblank: bool,
}

impl VideoInterface {
    pub const SCREEN_WIDTH: usize = 640;
    pub const SCREEN_HEIGHT: usize = 480;

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
            vblank: false,
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
            Current::OFFSET => side_effects.lower_interrupt(InterruptDevice::VideoInterface),
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
    pub fn tick(&mut self, cycles: usize) -> SideEffects {
        let old_counter = self.counter;
        self.counter = self
            .counter
            .checked_sub(cycles as u32)
            .unwrap_or(COUNTER_START);

        let raise_interrupt =
            old_counter > self.interrupt_counter && self.counter <= self.interrupt_counter;

        let vblank = old_counter > BLANKING_DONE && self.counter <= BLANKING_DONE;
        self.vblank = vblank;
        if vblank {
            self.field = !self.field;
        }

        let mut side_effects = SideEffects::new();
        if raise_interrupt {
            side_effects.raise_interrupt(InterruptDevice::VideoInterface);
        }
        side_effects
    }

    /// Returns the guest framebuffer as RGBA8888 pixels.
    pub fn framebuffer(&self, rdram: &[u8]) -> Option<FrameBuffer> {
        let (src_x_offset, dst_x_offset, dst_width) = {
            const HSCAN_MIN: usize = 108;
            const HSCAN_MAX: usize = HSCAN_MIN + VideoInterface::SCREEN_WIDTH;
            let hstart = self.horizontal_video.start() as usize;
            let hend = self.horizontal_video.end() as usize;
            let x0 = hstart.max(HSCAN_MIN);
            let x1 = hend.min(HSCAN_MAX);
            (x0 - hstart, x0 - HSCAN_MIN, x1 - x0)
        };

        let (src_y_offset, dst_y_offset, dst_height) = {
            const VSCAN_MIN: usize = 34;
            const VSCAN_MAX: usize = VSCAN_MIN + VideoInterface::SCREEN_HEIGHT;
            let vstart = self.vertical_video.start() as usize;
            let vend = self.vertical_video.end() as usize;
            let y0 = vstart.max(VSCAN_MIN);
            let y1 = if vend < vstart {
                VSCAN_MAX
            } else {
                vend.min(VSCAN_MAX)
            };
            (y0 - vstart, y0 - VSCAN_MIN, y1 - y0)
        };

        let xscale = self.xscale.scale() as usize;
        let yscale = self.yscale.scale() as usize;

        let pixel_type = self.control.pixel_type();
        let bytes_per_pixel = pixel_type.bytes_per_pixel()?; // For the guest, host always uses RGBA8888
        let mut pixels =
            vec![0_u8; (Self::SCREEN_WIDTH * Self::SCREEN_HEIGHT) * 4].into_boxed_slice();

        let mut src_y = (src_y_offset * yscale) + self.yscale.subpixel() as usize;
        for y in 0..dst_height {
            let is_odd = ((dst_y_offset + y) & 1) != 0;
            if !self.control.serrate() || self.field != is_odd {
                let dst_offset = (y * Self::SCREEN_WIDTH) + dst_x_offset;
                let src_offset = {
                    let rdram_offset = self.origin.origin() as usize;
                    let pitch = self.width.width() as usize * bytes_per_pixel;
                    rdram_offset + ((src_y >> 11) * pitch)
                };

                let mut src_x = (src_x_offset * xscale) + self.xscale.subpixel() as usize;
                for pixel_chunk in pixels.chunks_exact_mut(4).skip(dst_offset).take(dst_width) {
                    let addr = src_offset + ((src_x >> 10) * bytes_per_pixel);
                    let range = addr..addr + bytes_per_pixel;
                    match pixel_type {
                        PixelType::RGBA8888 => pixel_chunk.copy_from_slice(&rdram[range]),
                        PixelType::RGBA5551 => {
                            let pixel = u16::from_be_bytes(rdram[range].try_into().unwrap());
                            let r = rgba5551_to_rgba8888_color(pixel >> 11);
                            let g = rgba5551_to_rgba8888_color(pixel >> 6);
                            let b = rgba5551_to_rgba8888_color(pixel >> 1);
                            let a = u8::MAX;
                            pixel_chunk.copy_from_slice(&[r, g, b, a]);
                        }
                        PixelType::Reserved | PixelType::Blank => return None,
                    }
                    src_x += xscale;
                }
                src_y += yscale;
            }
        }

        Some(FrameBuffer {
            width: dst_width,
            height: dst_height,
            pixels,
        })
    }
}

impl Default for VideoInterface {
    fn default() -> Self {
        Self::new()
    }
}
