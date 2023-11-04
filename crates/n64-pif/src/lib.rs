use self::joybus::Channels;
use std::fmt;
use tartan_bitfield::bitfield;

mod joybus;

// TODO: deduplicate, this is stolen from delusion64::bus.
/// Allocates a fixed-sized boxed array of a given length.
fn boxed_array<T: Default + Clone, const LEN: usize>() -> Box<[T; LEN]> {
    // Use a Vec to allocate directly onto the heap. Using an array will allocate on the stack,
    // which can cause a stack overflow. SAFETY: We're sure the input size matches the output size.
    let result = vec![Default::default(); LEN].into_boxed_slice();
    unsafe { result.try_into().unwrap_unchecked() }
}

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

#[derive(Debug)]
enum Direction {
    Read,
    Write,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum PifError {
    OffsetOutOfBounds { region: Region, offset: usize },
    JoybusParseError(joybus::ParseError),
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
    channels: Channels,
}

impl Pif {
    const COMMAND_OFFSET: usize = 0x3F;

    pub fn new() -> Self {
        Self {
            ram: boxed_array(),
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

    fn process_command(&mut self, direction: Direction) -> PifResult<()> {
        let mut cmd = self.read_command();
        println!("delusion64: si {direction:?} dma");
        match direction {
            Direction::Read => {
                if cmd.challenge_checksum() {
                    todo!("PIF-NUS challenge checksum");
                }

                // TODO: handle cartridge
                for device in self.channels.controllers() {
                    let msg = joybus::Message::new(device.address, self.ram.as_mut_slice())?;
                    match msg.request {
                        joybus::Request::Info | joybus::Request::ResetInfo => {
                            msg.reply(joybus::response::Info::Controller {
                                pak_installed: false,
                                checksum_error: false,
                            });
                        }

                        joybus::Request::WriteControllerAccessory => {
                            // Namco Museum for some reason requests this, even though we never report `pak_installed`.
                            println!("stub: joybus::Command::WriteControllerAccessory");
                        }

                        req => todo!("PIF-NUS joybus request {req:#?}"),
                    }
                }
            }

            Direction::Write => {
                if !cmd.initialise_joybus() {
                    return Ok(());
                } else {
                    // TODO: keep track of the current state. Read DMA should not work without this.
                    self.write_command(cmd.with_initialise_joybus(false));
                }

                // Reset the channels, then initialise them with the contents of RAM.
                self.channels = Channels::new_initialised(self.ram.as_slice());
            }
        }
        Ok(())
    }

    pub fn read_dma(&mut self, _pif_address: u32, rdram: &mut [u8]) -> Result<(), PifError> {
        self.process_command(Direction::Read)?;
        rdram.copy_from_slice(self.ram.as_slice());
        Ok(())
    }

    pub fn write_dma(&mut self, _data: u32, rdram: &[u8]) -> Result<(), PifError> {
        self.ram.copy_from_slice(rdram);
        self.process_command(Direction::Write)
    }

    fn read_command(&self) -> Command {
        self.ram[Self::COMMAND_OFFSET].into()
    }

    fn write_command(&mut self, command: Command) {
        self.ram[Self::COMMAND_OFFSET] = command.into();
    }
}

impl Default for Pif {
    fn default() -> Self {
        Self::new()
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
