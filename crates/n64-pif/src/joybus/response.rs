use super::controller;

pub trait Response {
    const INPUT_LEN: usize;
    const OUTPUT_LEN: usize;

    // Unfortunately we cannot specify this to be [u8; Self::OUTPUT_LEN] until `generic_const_exprs` is stable.
    // See https://github.com/rust-lang/rust/issues/60551.
    type Output: AsRef<[u8]>;

    fn into_bytes(self) -> Self::Output;
}

/// See [n64brew](https://n64brew.dev/wiki/Joybus_Protocol#0x00_-_Info).
#[allow(dead_code)]
#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Info {
    Controller {
        pak_installed: bool,
        checksum_error: bool,
    },
    DancePad,
    VoiceRecognitionUnit,
    Mouse,
    RandnetKeyboard,
    GameboyAccessory,
    EepRom4K {
        write_in_progress: bool,
    },
    EepRom16K {
        write_in_progress: bool,
    },
}

impl Response for Info {
    const INPUT_LEN: usize = 1;
    const OUTPUT_LEN: usize = 3;

    type Output = [u8; Self::OUTPUT_LEN];

    fn into_bytes(self) -> Self::Output {
        match self {
            Self::Controller {
                pak_installed,
                checksum_error,
            } => [
                0x05,
                0x00,
                // 0b001: Pak installed
                // 0b010: No Pak installed
                // 0b100: Checksum error in the previous command
                (1 << (!pak_installed as u8)) | ((checksum_error as u8) << 2),
            ],
            Self::DancePad => [0x05, 0x00, 0x00],
            Self::VoiceRecognitionUnit => [0x00, 0x01, 0x00],
            Self::Mouse => [0x02, 0x00, 0x00],
            Self::RandnetKeyboard => [0x00, 0x02, 0x00],
            Self::GameboyAccessory => [0x00, 0x03, 0x00],
            // 0b1000_0000: A write to the EEPROM is currently in progress
            Self::EepRom4K { write_in_progress } => [0x00, 0x80, (write_in_progress as u8) << 7],
            Self::EepRom16K { write_in_progress } => [0x00, 0xC0, (write_in_progress as u8) << 7],
        }
    }
}

/// See [n64brew](https://n64brew.dev/wiki/Joybus_Protocol#0x01_-_Controller_State).
#[allow(dead_code)]
#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum ControllerState {
    Standard(controller::StandardController),
    Mouse(controller::Mouse),
}

impl Response for ControllerState {
    const INPUT_LEN: usize = 1;
    const OUTPUT_LEN: usize = 4;

    type Output = [u8; Self::OUTPUT_LEN];

    fn into_bytes(self) -> Self::Output {
        match self {
            ControllerState::Standard(mut controller) => controller.as_bytes(),
            ControllerState::Mouse(mouse) => mouse.as_bytes(),
        }
    }
}

impl From<controller::StandardController> for ControllerState {
    fn from(controller: controller::StandardController) -> Self {
        Self::Standard(controller)
    }
}

impl From<controller::Mouse> for ControllerState {
    fn from(mouse: controller::Mouse) -> Self {
        Self::Mouse(mouse)
    }
}
