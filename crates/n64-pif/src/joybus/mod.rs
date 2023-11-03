use self::response::Response;
use strum::FromRepr;

pub use self::device::Channels;

mod device;
pub mod response;

pub const fn parse_len(byte: u8) -> usize {
    // Unsure what the upper two bits are used for?
    (byte as usize) & 0b0011_1111
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum ParseError {
    UnknownCommand(u8),
    ReservedCommand(u8),
}

/// Assigns a second identifier to an enum variant, since these all need to be unique,
/// but some commands have the same value. Hardware uses an 8-bit value, so this is accurate when truncated.
const fn ambiguous_repr(repr: u8, id: u8) -> u16 {
    (repr as u16) | ((id as u16) << 8)
}

/// A request from the N64 to the PIF-NUS's Joybus, referred to as a "command" in the documentation.
/// To avoid confusion with a PIF-NUS command, we call it a request instead.
/// See [n64brew](https://n64brew.dev/wiki/Joybus_Protocol#Command_List)
#[derive(FromRepr, Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
#[repr(u16)]
pub enum Request {
    Info = 0x00,
    ControllerState = 0x01,
    ReadControllerAccessory = 0x02,
    WriteControllerAccessory = 0x03,
    ReadEEPROM = 0x04,
    WriteEEPROM = 0x05,
    RealtimeClockInfo = 0x06,
    ReadRealtimeClockBlock = 0x07,
    WriteRealtimeClockBlock = 0x08,
    VoiceRecognition = 0x09,                // 0x09..0x0D
    ReadKeypress = ambiguous_repr(0x13, 1), // Overlaps with ReadGameBoy
    ReadGameBoy = ambiguous_repr(0x13, 2),  // Overlaps with ReadKeypress
    WriteGameBoy = 0x14,
    ResetInfo = 0xFF,
}

impl Request {
    pub fn new(value: u8, input_len: usize) -> Result<Self, ParseError> {
        match Request::from_repr(value as u16) {
            Some(cmd) => Ok(cmd),
            None => match value {
                // Manually fix up commands hard to model nicely with the derive macro.
                0x09..=0x0D => Ok(Request::VoiceRecognition),
                0x13 => {
                    if input_len == 2 {
                        Ok(Request::ReadKeypress)
                    } else {
                        Ok(Request::ReadGameBoy)
                    }
                }

                0x0E | 0x10 | 0x11 | 0x12 => Err(ParseError::ReservedCommand(value)),
                _ => Err(ParseError::UnknownCommand(value)),
            },
        }
    }
}

impl From<Request> for u8 {
    fn from(cmd: Request) -> Self {
        // The ambiguous identifier bits get truncated off here.
        cmd as u8
    }
}

pub struct Message<'a> {
    pub request: Request,
    pub input: &'a [u8],
    pub output: &'a mut [u8],
}

impl<'a> Message<'a> {
    pub fn new(mut addr: usize, ram: &'a mut [u8]) -> Result<Self, ParseError> {
        let input_len = parse_len(ram[addr]);
        let output_len = parse_len(ram[addr + 1]);
        addr += 2;

        let request = if input_len != 0 {
            Request::new(ram[addr], input_len)?
        } else {
            Request::Info
        };

        let (input, output) = ram[addr..addr + input_len + output_len].split_at_mut(input_len);
        Ok(Self {
            request,
            input,
            output,
        })
    }

    pub fn reply(self, response: impl Response) {
        let bytes = response.into_bytes();
        let len = self.output.len().min(bytes.as_ref().len());
        self.output[..len].copy_from_slice(&bytes.as_ref()[..len]);
        if len < self.output.len() {
            self.output[len..].fill(0);
        }
    }
}
