use self::joybus::Channels;
use n64_common::{
    log::warn,
    utils::{boxed_array, tartan_bitfield::bitfield, thiserror},
};
use std::{fmt, ops::Range};

mod joybus;

pub use joybus::{controller, Channel};

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Region {
    Ram,
    Rom,
}

impl Region {
    #[allow(clippy::len_without_is_empty)] // Makes no sense on enums
    pub const fn len(&self) -> usize {
        match self {
            Region::Ram => 0x40,
            Region::Rom => 0x7C0,
        }
    }
}

#[derive(thiserror::Error, Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum PifError {
    #[error("Offset {offset} is out of bounds in {region:?}")]
    OffsetOutOfBounds { region: Region, offset: usize },
    #[error("Joybus parse error: {0}")]
    JoybusParseError(joybus::ParseError),
    #[error("Channel {channel:?} is already attached")]
    AlreadyAttached { channel: Channel },
    #[error("Channel {channel:?} is not attached")]
    NotAttached { channel: Channel },
    #[error("Channel {channel:?} is not attached")]
    Unexpected { channel: Channel },
    #[error("Unimplemented joybus request: {request:?}")]
    UnimplementedJoybusRequest { request: joybus::Request },
}

impl From<joybus::ParseError> for PifError {
    fn from(error: joybus::ParseError) -> Self {
        Self::JoybusParseError(error)
    }
}

pub type PifResult<T> = Result<T, PifError>;

bitfield! {
    /// See [n64brew](https://n64brew.dev/wiki/PIF-NUS#RAM-based_communication_protocol)
    pub struct Command(u8) {
        [0] pub initialise_joybus,
        [1] pub challenge_checksum,
        [3] pub terminate_boot,
        [4] pub rom_lockout,
        [5] pub acquire_checksum,
        [6] pub run_checksum,
        [7] pub acknowledge,
    }
}

/// See [n64brew](https://n64brew.dev/wiki/PIF-NUS)
pub struct Pif {
    ram: Box<[u8; Region::Ram.len()]>,
    rom: Box<[u8; Region::Rom.len()]>,
    pub channels: Channels,
}

impl Pif {
    const COMMAND_OFFSET: usize = 0x3F;
    const CIC_SEED_RANGE: Range<usize> = 0x24..0x28;

    pub fn new(cic_seed: u32) -> Self {
        let mut ram = boxed_array();
        ram[Self::CIC_SEED_RANGE].copy_from_slice(&cic_seed.to_be_bytes());
        Self {
            ram,
            rom: boxed_array(),
            channels: Channels::new(),
        }
    }

    pub fn read<const SIZE: usize>(&self, region: Region, offset: usize) -> PifResult<&[u8; SIZE]> {
        match region {
            Region::Ram => self.ram.get(offset..offset + SIZE),
            Region::Rom => self.rom.get(offset..offset + SIZE),
        }
        // SAFETY: We always fetch SIZE bytes.
        .map(|bytes| unsafe { bytes.try_into().unwrap_unchecked() })
        .ok_or(PifError::OffsetOutOfBounds { region, offset })
    }

    pub fn write_ram<const SIZE: usize>(
        &mut self,
        offset: usize,
        value: &[u8; SIZE],
    ) -> PifResult<()> {
        // Writing less than 4 bytes still writes 4 bytes, by zero-extending the value.
        self.ram
            .get_mut(offset..offset + SIZE.max(4))
            .map(|bytes| {
                bytes[..SIZE].copy_from_slice(value);
                if SIZE < 4 {
                    bytes[SIZE..].fill(0);
                }
            })
            .ok_or(PifError::OffsetOutOfBounds {
                region: Region::Ram,
                offset,
            })
    }

    fn process_command_read(&mut self) -> PifResult<()> {
        if self.read_command().challenge_checksum() {
            todo!("PIF-NUS challenge checksum");
        }

        // TODO: handle cartridge
        for port in self.channels.connected_devices() {
            let mut msg = match joybus::Message::new(port.address(), self.ram.as_mut_slice())? {
                joybus::Status::Message(msg) => msg,
                joybus::Status::ResetChannel | joybus::Status::SkipChannel => continue,
            };

            match msg.request {
                joybus::Request::Info | joybus::Request::ResetInfo => {
                    if let Some(state) = port.controller() {
                        msg.reply(match state {
                            joybus::response::ControllerState::Standard(_) => {
                                joybus::response::Info::Controller {
                                    pak_installed: false,
                                    checksum_error: false,
                                }
                            }
                            joybus::response::ControllerState::Mouse(_) => {
                                joybus::response::Info::Mouse
                            }
                        });
                    } else {
                        msg.reply_invalid();
                    }
                }

                joybus::Request::ControllerState => {
                    if let Some(state) = port.controller() {
                        msg.reply(*state)
                    } else {
                        msg.reply_invalid();
                    }
                }

                joybus::Request::WriteControllerAccessory => {
                    // Namco Museum for some reason requests this, even though we never report `pak_installed`.
                    warn!("stub: joybus::Command::WriteControllerAccessory");
                    msg.reply_invalid();
                }

                request => Err(PifError::UnimplementedJoybusRequest { request })?,
            }
        }
        Ok(())
    }

    fn process_command_write(&mut self) -> PifResult<()> {
        let mut cmd = self.read_command();
        if cmd.initialise_joybus() {
            // TODO: keep track of the current state. Read DMA should not work without this.
            self.write_command(cmd.with_initialise_joybus(false));
        } else {
            return Ok(());
        }

        // Reset the channels, then initialise them with the contents of RAM.
        self.channels.parse(self.ram.as_slice());
        Ok(())
    }

    pub fn read_dma(&mut self, _pif_address: u32, rdram: &mut [u8]) -> Result<(), PifError> {
        self.process_command_read()?;
        rdram.copy_from_slice(self.ram.as_slice());
        Ok(())
    }

    pub fn write_dma(&mut self, _data: u32, rdram: &[u8]) -> Result<(), PifError> {
        self.ram.copy_from_slice(rdram);
        self.process_command_write()
    }

    fn read_command(&self) -> Command {
        self.ram[Self::COMMAND_OFFSET].into()
    }

    fn write_command(&mut self, command: Command) {
        self.ram[Self::COMMAND_OFFSET] = command.into();
    }
}

impl fmt::Debug for Pif {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Pif")
            .field("channels", &self.channels)
            .field("rom", &"<omitted>")
            .field("ram", &"<omitted>")
            .finish()
    }
}
