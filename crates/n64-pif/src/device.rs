use tartan_bitfield::bitfield;

#[derive(Debug, Default, PartialEq, Eq, PartialOrd, Ord)]
pub struct Device {
    pub skip: bool,
    pub reset: bool,
    pub address: u8,
}

pub const DEVICES_LEN: usize = 4;

pub type Devices = [Device; DEVICES_LEN];

bitfield! {
    /// See [n64brew](https://n64brew.dev/wiki/Joybus_Protocol#Standard_Controller).
    pub struct StandardController(u32) {
        [0..=7] pub y_axis: u8,
        [8..=15] pub x_axis: u8,
        [16] pub c_right,
        [17] pub c_left,
        [18] pub c_down,
        [19] pub c_up,
        [20] pub r,
        [21] pub l,
        [23] pub reset_signal,
        [24] pub dpad_right,
        [25] pub dpad_left,
        [26] pub dpad_down,
        [27] pub dpad_up,
        [28] pub start,
        [29] pub z,
        [30] pub b,
        [31] pub a,
    }
}

impl StandardController {
    pub fn normalise(&mut self) {
        // Ensure mutually exclusive dpad buttons cannot be pressed at the same time
        self.set_dpad_right(self.dpad_right() & !self.dpad_left());
        self.set_dpad_left(self.dpad_left() & !self.dpad_right());
        self.set_dpad_down(self.dpad_down() & !self.dpad_up());
        self.set_dpad_up(self.dpad_up() & !self.dpad_down());

        if self.start() && self.l() && self.r() {
            self.set_reset_signal(true);
            self.set_x_axis(0);
            self.set_y_axis(0);
        }
    }
}
