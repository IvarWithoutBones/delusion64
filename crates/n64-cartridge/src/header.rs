use binrw::BinRead;
use std::fmt;

pub const LEN: usize = 0x3F;

/// The version of the libultra SDK the ROM was compiled with.
pub struct LibultraVersion {
    pub major: u8,
    pub minor: u8,
    pub revision: char,
}

impl LibultraVersion {
    fn new(from: [u8; 0x4]) -> Self {
        Self {
            major: from[2] / 10,
            minor: from[2] % 10,
            revision: from[3] as char,
        }
    }
}

impl fmt::Debug for LibultraVersion {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}.{}{}", self.major, self.minor, self.revision)
    }
}

#[derive(BinRead, Debug)]
#[br(big, repr = u8)]
#[repr(u8)]
pub enum CategoryCode {
    GamePak = b'N',
    DiskDrive = b'D',
    ExpandableGamePak = b'C',
    ExpandableDiskDrive = b'E',
    Aleck64GamePak = b'Z',
}

#[derive(BinRead, Debug)]
#[br(big, repr = u8)]
#[repr(u8)]
pub enum DestinationCode {
    All = b'A',
    Brazil = b'B',
    China = b'C',
    Germany = b'D',
    NorthAmerica = b'E',
    France = b'F',
    Gateway64NTSC = b'G',
    Netherlands = b'H',
    Italy = b'I',
    Japan = b'J',
    Korea = b'K',
    Gateway64PAL = b'L',
    Canada = b'N',
    Europe0 = b'P',
    Spain = b'S',
    Australia = b'U',
    Scandinavia = b'W',
    Europe1 = b'X',
    Europe2 = b'Y',
    Europe3 = b'Z',
}

#[derive(BinRead, Debug)]
pub struct GameCode {
    pub category: CategoryCode,
    #[br(map = |x: [u8; 2]| [x[0] as char, x[1] as char])]
    pub unique: [char; 2],
    pub destination: DestinationCode,
}

/// https://n64brew.dev/wiki/ROM_Header
#[derive(BinRead)]
#[br(big)]
pub struct Header {
    pub pi_bsd_dom1_flags: u32,
    #[br(map = Header::read_clock_rate)]
    pub clock_rate: u32,
    pub boot_address: u32,
    #[br(map = LibultraVersion::new)]
    pub libultra_version: LibultraVersion,
    pub ipl3_check_code: u64,
    _reserved_1: u64,
    #[br(map = Header::read_title)]
    pub title: String,
    _reserved_2: [u8; 0x7],
    pub game_code: GameCode,
    pub version: u8,
}

impl Header {
    fn read_clock_rate(input: u32) -> u32 {
        // Masked by 0xFFFFFFF0 to obtain the proper clock rate value,
        // A value of 0 appears to use the default clock rate of 62.5MHz.
        let result = input & 0xFFFFFFF0;
        if result == 0 {
            62500000
        } else {
            result
        }
    }

    fn read_title(bytes: [u8; 0x14]) -> String {
        String::from_utf8_lossy(&bytes)
            .to_string()
            .trim()
            .to_string()
    }
}

impl fmt::Debug for Header {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Header")
            .field(
                "pi_bsd_dom1_flags",
                &format_args!("{:#08X}", self.pi_bsd_dom1_flags),
            )
            .field("clock_rate", &self.clock_rate)
            .field("boot_address", &format_args!("{:#08X}", self.boot_address))
            .field("libultra_version", &self.libultra_version)
            .field(
                "ipl3_check_code",
                &format_args!("{:#016X}", self.ipl3_check_code),
            )
            .field("title", &self.title)
            .field("game_code", &self.game_code)
            .field("version", &self.version)
            .finish()
    }
}
