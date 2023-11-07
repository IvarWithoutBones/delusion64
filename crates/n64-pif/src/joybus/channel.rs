use super::{parse_len, response::ControllerState};
use crate::{PifError, PifResult, Region};
use strum::FromRepr;

#[derive(FromRepr)]
#[repr(u8)]
enum ControlByte {
    ChannelSkip = 0x00,
    EndOfCommands = 0xFE,
    ChannelReset = 0xFD,
    Padding = 0xFF,
}

#[derive(Debug, Default, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub struct Channel {
    pub reset: bool,
    pub address: usize,
}

#[derive(Debug, Default, PartialEq, Eq, PartialOrd, Ord)]
pub struct Channels {
    pub channels: [Option<Channel>; Self::CHANNELS_LEN],
    pub controllers: [Option<ControllerState>; Self::CONTROLLERS_LEN],
}

impl Channels {
    pub const CHANNELS_LEN: usize = 4;
    pub const CONTROLLERS_LEN: usize = 3;

    pub(crate) fn new() -> Self {
        Self::default()
    }

    pub(crate) fn parse(&mut self, ram: &[u8]) {
        self.channels = Default::default();
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

    pub(crate) fn controllers(&self) -> impl Iterator<Item = (&Channel, &ControllerState)> {
        self.channels
            .iter()
            .take(Self::CONTROLLERS_LEN)
            .flatten()
            .zip(self.controllers.iter().flatten())
    }

    pub fn attach_controller(
        &mut self,
        channel: usize,
        state: impl Into<ControllerState>,
    ) -> PifResult<()> {
        self.controllers
            .get_mut(channel)
            .map(|c| *c = Some(state.into()))
            .ok_or(PifError::InvalidChannel { channel })
    }

    pub fn detach_controller(&mut self, channel: usize) -> PifResult<()> {
        self.controllers
            .get_mut(channel)
            .map(|c| *c = None)
            .ok_or(PifError::InvalidChannel { channel })
    }

    pub fn update_controller(
        &mut self,
        channel: usize,
        state: impl Into<ControllerState>,
    ) -> PifResult<()> {
        self.controllers
            .get_mut(channel)
            .and_then(|c| c.as_mut().map(|c| *c = state.into()))
            .ok_or(PifError::InvalidChannel { channel })
    }
}
