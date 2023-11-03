use super::parse_len;
use crate::Region;
use strum::FromRepr;
use tartan_bitfield::bitfield;

#[derive(FromRepr)]
#[repr(u8)]
enum ControlByte {
    ChannelSkip = 0x00,
    EndOfCommands = 0xFE,
    ChannelReset = 0xFD,
    Padding = 0xFF,
}

#[derive(Debug, Default, PartialEq, Eq, PartialOrd, Ord)]
pub struct Channel {
    pub reset: bool,
    pub address: usize,
}

#[derive(Debug, Default, PartialEq, Eq, PartialOrd, Ord)]
pub struct Channels {
    pub channels: [Option<Channel>; Self::CHANNELS_LEN],
}

impl Channels {
    pub const CHANNELS_LEN: usize = 4;
    pub const CONTROLLERS_LEN: usize = 3;

    pub fn new() -> Self {
        Self::default()
    }

    pub fn new_initialised(ram: &[u8]) -> Self {
        let mut channels = Self::new();
        channels.initialise(ram);
        channels
    }

    fn initialise(&mut self, ram: &[u8]) {
        let mut addr = 0;
        let mut channel_iter = self.channels.iter_mut().peekable();

        while let Some(channel) = channel_iter.peek_mut() {
            if addr >= Region::Ram.len() {
                break;
            }

            let (address, input_len) = {
                let maybe = ram[addr];
                if let Some(ctrl) = ControlByte::from_repr(maybe) {
                    match ctrl {
                        ControlByte::EndOfCommands => break,
                        ControlByte::ChannelSkip => {
                            channel_iter.next();
                        }
                        ControlByte::ChannelReset => {
                            if let Some(channel) = channel {
                                channel.reset = true;
                            }
                            channel_iter.next();
                        }
                        _ => {}
                    }

                    addr += 1;
                    continue;
                } else {
                    (addr, parse_len(maybe))
                }
            };
            let output_len = parse_len(ram[address + 1]);

            addr += 2 + input_len + output_len;
            if addr < Region::Ram.len() {
                **channel = Some(Channel {
                    address,
                    reset: false,
                });
                channel_iter.next();
            }
        }
    }

    pub fn controllers(&self) -> impl Iterator<Item = &Channel> {
        self.channels.iter().take(Self::CONTROLLERS_LEN).flatten()
    }
}

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
