#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum ParseError {
    UnknownCommand(u8),
    ReservedCommand(u8),
}

/// See [n64brew](https://n64brew.dev/wiki/Joybus_Protocol#Command_List)
#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Command {
    Info,
    ControllerState,
    ReadControllerAccessory,
    WriteControllerAccessory,
    ReadEEPROM,
    WriteEEPROM,
    RealtimeClockInfo,
    ReadRealtimeClockBlock,
    WriteRealtimeClockBlock,
    VoiceRecognition(u8),
    ReadKeypress,
    #[allow(dead_code)] // TODO: fix, see comment below
    ReadGameBoy,
    #[allow(dead_code)] // TODO: fix, see comment below
    WriteGameBoy,
    ReadGameBoyAdvance,
    WriteGameBoyAdvance,
    SteeringWheelForceFeedback,
    ControllerShortPoll,
    ControllerReadOrigin,
    ControllerCalibrate,
    ControllerLongPoll,
    KeyboardPoll,
    ResetInfo,
}

impl TryFrom<u8> for Command {
    type Error = ParseError;

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        // TODO: Also match the amount of Rx/Tx bytes. This also disambiguates overlaps.
        Ok(match value {
            0x00 => Self::Info,
            0x01 => Self::ControllerState,
            0x02 => Self::ReadControllerAccessory,
            0x03 => Self::WriteControllerAccessory,
            0x04 => Self::ReadEEPROM,
            0x05 => Self::WriteEEPROM,
            0x06 => Self::RealtimeClockInfo,
            0x07 => Self::ReadRealtimeClockBlock,
            0x08 => Self::WriteRealtimeClockBlock,
            0x09..=0x0D => Self::VoiceRecognition(value),
            0x13 => Self::ReadKeypress,       // Overlaps with ReadGameBoy
            0x14 => Self::ReadGameBoyAdvance, // Overlaps with WriteGameBoy
            0x15 => Self::WriteGameBoyAdvance,
            0x30 => Self::SteeringWheelForceFeedback,
            0x40 => Self::ControllerShortPoll,
            0x41 => Self::ControllerReadOrigin,
            0x42 => Self::ControllerCalibrate,
            0x43 => Self::ControllerLongPoll,
            0x54 => Self::KeyboardPoll,
            0xFF => Self::ResetInfo,
            0x0E | 0x10 | 0x11 | 0x12 => Err(ParseError::ReservedCommand(value))?,
            other => Err(ParseError::UnknownCommand(other))?,
        })
    }
}
