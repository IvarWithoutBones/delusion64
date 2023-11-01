use std::fmt;

use command::Command;
use device::Devices;

use crate::device::DEVICES_LEN;

mod command;
mod device;

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

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum PifError {
    OffsetOutOfBounds { region: Region, offset: usize },
    CommandParseError(command::ParseError),
}

impl From<command::ParseError> for PifError {
    fn from(error: command::ParseError) -> Self {
        Self::CommandParseError(error)
    }
}

pub type PifResult<T> = Result<T, PifError>;

#[derive(Debug)]
enum Direction {
    Read,
    Write,
}

// #[derive(Debug)] // TODO: better impl, this dumps too much
pub struct Pif {
    ram: Box<[u8; Region::Ram.len()]>,
    rom: Box<[u8; Region::Rom.len()]>,
    devices: Devices,
}

impl Pif {
    const COMMAND_OFFSET: usize = 0x3F;

    pub fn new() -> Self {
        Self {
            ram: boxed_array(),
            rom: boxed_array(),
            devices: Default::default(),
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
        self.rom
            .get_mut(offset..offset + SIZE.max(4))
            .map(|bytes| {
                bytes.copy_from_slice(value);
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
        match direction {
            Direction::Read => {
                // Skip the last device, which is the cartridge port
                for device in self
                    .devices
                    .iter()
                    .take(DEVICES_LEN - 1)
                    .filter(|device| !device.skip)
                {
                    let mut offset = device.address as usize;

                    let mut send = self.ram[offset];
                    offset += 1;

                    println!("send: {send:#04x}");

                    let recv_offset = offset;
                    let mut recv = self.ram[offset];
                    println!("recv: {recv:#04x}");
                    offset += 1;

                    send &= 0x3F;
                    recv &= 0x3F;

                    let mut input = [0_u8; 64];
                    for input_byte in input.iter_mut().take(send as usize) {
                        *input_byte = self.ram[offset];
                        offset += 1;
                    }

                    {
                        for (i, input_byte) in input.iter().enumerate() {
                            if i % 8 == 0 {
                                println!()
                            }
                            print!("{input_byte:02x} ");
                        }
                        println!();
                    }

                    let mut valid = false;

                    let mut output = [0_u8; 64];
                    if input[0] == 0 || input[0] == 0xff {
                        output[0] = 0x05; //0x05 = gamepad; 0x02 = mouse
                        output[1] = 0x00;
                        output[2] = 0x02; //0x02 = nothing present in controller slot
                        valid = true;
                    }

                    if valid {
                        for out in output.iter().take(recv as usize) {
                            self.ram[offset] = *out;
                            offset += 1;
                        }
                    } else {
                        self.ram[recv_offset] = recv | 0x80;
                    }
                }
            }
            Direction::Write => {
                // Initialise the devices
                assert!(self.read_command()? == Command::ControllerState);
                for device in &mut self.devices {
                    device.skip = true;
                    device.reset = false;
                }

                // Parse the devices
                let mut offset = 0;
                let mut channel = 0;
                while channel < 5 && offset < 64 {
                    let mut send = self.ram[offset];
                    offset += 1;

                    match send {
                        0xFE => break,    // End of packet
                        0xFF => continue, // Padding
                        0x00 => {
                            // Channel skip
                            channel += 1;
                            continue;
                        }
                        0xFD => {
                            // Channel reset
                            self.devices[channel].reset = true;
                            channel += 1;
                            continue;
                        }
                        _ => {}
                    }

                    let send_offset = offset - 1;
                    let mut recv = self.ram[offset];
                    offset += 1;

                    send &= 0x3F;
                    recv &= 0x3F;
                    offset += send as usize + recv as usize;

                    if offset < 64 {
                        self.devices[channel].address = send_offset as u8;
                        self.devices[channel].skip = false;
                        channel += 1;
                    }
                }
            }
        }

        Ok(())
    }

    pub fn read_dma(&mut self, _pif_address: u32, rdram: &mut [u8]) -> Result<(), PifError> {
        let cmd = self.read_command()?;
        println!("delusion64: PIF read DMA with command {cmd:#?}");
        self.process_command(Direction::Read)?;
        rdram.copy_from_slice(self.ram.as_slice());
        Ok(())
    }

    pub fn write_dma(&mut self, _data: u32, rdram: &[u8]) -> Result<(), PifError> {
        self.ram.copy_from_slice(rdram);
        let cmd = self.read_command()?;
        println!("delusion64: PIF write DMA submitted command {cmd:#?}");
        self.process_command(Direction::Write)
    }

    fn read_command(&self) -> Result<Command, PifError> {
        Ok(self.ram[Self::COMMAND_OFFSET].try_into()?)
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
            .field("devices", &self.devices)
            .field("rom", &"<omitted>")
            .field("ram", &{
                let mut output = String::new();
                for (i, byte) in self.ram.iter().enumerate().take(0x40) {
                    if i % 8 == 0 {
                        output.push('\n');
                    }
                    output.push_str(&format!("{byte:02x} "));
                }
                output
            })
            .finish()
    }
}
