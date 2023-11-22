use self::response::Response;
use strum::FromRepr;
use tartan_bitfield::bitfield;

pub use self::channel::{Channel, Channels};

mod channel;
pub mod controller;
pub mod response;

bitfield! {
    pub struct PacketMeta(u8) {
        [0..=5] length: u8,
        // For input
        [6] pub reset,
        [7] pub skip,
        // For output
        [6] pub over,
        [7] pub invalid,
    }
}

impl PacketMeta {
    pub fn len(&self) -> usize {
        self.length() as usize
    }

    pub const fn raw(&self) -> u8 {
        self.0
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct PacketLen {
    pub input: u8,
    pub output: u8,
}

impl PacketLen {
    const fn new(input: u8, output: u8) -> Self {
        Self { input, output }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum ParseError {
    UnknownRequest(u8),
    InvalidRequestLen {
        request: Request,
        expected: PacketLen,
        got: PacketLen,
    },
    RequestInputEmpty,
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
    pub fn new(value: u8, input: PacketMeta, output: PacketMeta) -> Result<Self, ParseError> {
        let len = PacketLen::new(input.length(), output.length());
        let request = match Request::from_repr(value as u16) {
            Some(cmd) => Ok(cmd),
            None => match value {
                // Manually fix up commands hard to model nicely with the derive macro.
                0x09..=0x0D => Ok(Request::VoiceRecognition),
                0x13 => {
                    if len == Request::ReadKeypress.len().unwrap() {
                        Ok(Request::ReadKeypress)
                    } else {
                        Ok(Request::ReadGameBoy)
                    }
                }

                _ => Err(ParseError::UnknownRequest(value)),
            },
        }?;

        let Some(expected) = request.len() else {
            // Unknown packet size, we assume its fine.
            return Ok(request);
        };

        if expected == len {
            Ok(request)
        } else {
            Err(ParseError::InvalidRequestLen {
                request,
                expected,
                got: len,
            })
        }
    }

    const fn len(&self) -> Option<PacketLen> {
        let (input, output) = match self {
            Request::Info => (1, 3),
            Request::ControllerState => (1, 4),
            Request::ReadControllerAccessory => (3, 33),
            Request::WriteControllerAccessory => (35, 1),
            Request::ReadEEPROM => (2, 8),
            Request::WriteEEPROM => (10, 1),
            Request::RealtimeClockInfo => (1, 3),
            Request::ReadRealtimeClockBlock => (2, 9),
            Request::WriteRealtimeClockBlock => (10, 1),
            Request::VoiceRecognition => return None,
            Request::ReadKeypress => (2, 7),
            Request::ReadGameBoy => (3, 33),
            Request::WriteGameBoy => (35, 1),
            Request::ResetInfo => (1, 3),
        };
        Some(PacketLen::new(input, output))
    }
}

impl From<Request> for u8 {
    fn from(cmd: Request) -> Self {
        // The ambiguous identifier bits get truncated off here.
        cmd as u8
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Status<T> {
    Message(T),
    ResetChannel,
    SkipChannel,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Message<'a> {
    pub request: Request,
    input: &'a [u8],
    output: &'a mut [u8],
    output_meta: &'a mut u8,
}

impl<'a> Message<'a> {
    pub fn new(addr: usize, ram: &'a mut [u8]) -> Result<Status<Self>, ParseError> {
        let (meta, data) = ram[addr..].split_at_mut(2);
        let input_meta = PacketMeta(meta[0]);
        let output_meta = PacketMeta(meta[1]);

        if input_meta.reset() {
            return Ok(Status::ResetChannel);
        } else if input_meta.skip() {
            return Ok(Status::SkipChannel);
        }

        let request = if input_meta.len() != 0 {
            Request::new(data[0], input_meta, output_meta)?
        } else {
            Err(ParseError::RequestInputEmpty)?
        };

        let len = input_meta.len() + output_meta.len();
        let (input, output) = data[..len].split_at_mut(input_meta.len());

        Ok(Status::Message(Self {
            request,
            input,
            output,
            output_meta: &mut meta[1],
        }))
    }

    pub fn reply(self, response: impl Response) {
        let bytes = response.into_bytes();
        let len = self.output.len().min(bytes.as_ref().len());
        self.output[..len].copy_from_slice(&bytes.as_ref()[..len]);
        debug_assert_eq!(self.output.len(), bytes.as_ref().len());
    }

    pub fn reply_invalid(&mut self) {
        *self.output_meta |= PacketMeta::default().with_invalid(true).raw()
    }
}
