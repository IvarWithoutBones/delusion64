use super::{response::ControllerState, PacketMeta};
use crate::{PifError, PifResult, Region};
use strum::{EnumCount, FromRepr};

#[derive(FromRepr, EnumCount, Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Channel {
    Controller1 = 0,
    Controller2 = 1,
    Controller3 = 2,
    Controller4 = 3,
    Cartridge = 4,
}

impl Channel {
    pub const CONTROLLER_COUNT: usize = 4;
}

pub(crate) struct Device<'a> {
    channels: &'a Channels,
    index: Channel,
}

impl Device<'_> {
    pub fn controller(&self) -> Option<&ControllerState> {
        self.channels.controllers.get(self.index as usize)?.as_ref()
    }

    pub fn address(&self) -> usize {
        self.channels.channels[self.index as usize].unwrap().address
    }
}

#[derive(FromRepr)]
#[repr(u8)]
enum ControlByte {
    ChannelSkip = 0x00,
    EndOfCommands = 0xFE,
    ChannelReset = 0xFD,
    Padding = 0xFF,
}

#[derive(Debug, Default, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
struct ChannelMeta {
    reset: bool,
    address: usize,
}

#[derive(Debug, Default, PartialEq, Eq, PartialOrd, Ord)]
pub struct Channels {
    channels: [Option<ChannelMeta>; Channel::COUNT],
    controllers: [Option<ControllerState>; Channel::CONTROLLER_COUNT],
}

impl Channels {
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
                    (addr, PacketMeta(maybe).len())
                }
            };
            let output_len = PacketMeta(ram[address + 1]).len();

            addr += 2 + input_len + output_len;
            if addr < Region::Ram.len() {
                **channel = Some(ChannelMeta {
                    address,
                    reset: false,
                });
                channel_iter.next();
            }
        }
    }

    pub(crate) fn connected_devices(&self) -> impl Iterator<Item = Device> {
        self.channels.iter().enumerate().filter_map(|(i, c)| {
            c.as_ref().map(|_| Device {
                channels: self,
                index: Channel::from_repr(i).unwrap(),
            })
        })
    }

    pub fn attach_controller(
        &mut self,
        channel: Channel,
        state: impl Into<ControllerState>,
    ) -> PifResult<()> {
        self.controllers
            .get_mut(channel as usize)
            .ok_or(PifError::Unexpected { channel })
            .and_then(|c| {
                if c.is_some() {
                    Err(PifError::AlreadyAttached { channel })
                } else {
                    *c = Some(state.into());
                    Ok(())
                }
            })
    }

    pub fn detach_controller(&mut self, channel: Channel) -> PifResult<()> {
        self.controllers
            .get_mut(channel as usize)
            .ok_or(PifError::Unexpected { channel })
            .and_then(|c: &mut Option<ControllerState>| {
                if c.is_none() {
                    Err(PifError::NotAttached { channel })
                } else {
                    *c = None;
                    Ok(())
                }
            })
    }

    pub fn update_controller(
        &mut self,
        channel: Channel,
        state: impl Into<ControllerState>,
    ) -> PifResult<()> {
        self.controllers
            .get_mut(channel as usize)
            .ok_or(PifError::Unexpected { channel })
            .and_then(|c| {
                if c.is_none() {
                    Err(PifError::NotAttached { channel })
                } else {
                    *c = Some(state.into());
                    Ok(())
                }
            })
    }
}
