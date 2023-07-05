use binrw::binrw;
use std::{fmt, io::SeekFrom, mem::size_of};

/// The version of the libultra SDK the ROM was compiled with.
#[binrw]
#[derive(PartialEq, Eq, PartialOrd, Ord)]
pub struct LibultraVersion {
    _reserved: [u8; 0x2],

    // The major/minor versions are manipulated when parsing, keep the original so that we can write it back.
    _major_minor: u8,
    #[br(map(Self::read_major), seek_before(SeekFrom::Current(-(size_of::<u8>() as i64))))]
    #[bw(ignore)]
    pub major: u8,
    #[br(map(Self::read_minor), seek_before(SeekFrom::Current(-(size_of::<u8>() as i64))))]
    #[bw(ignore)]
    pub minor: u8,

    #[br(map(|x: u8| x as char))]
    #[bw(map(|x: &char| *x as u8))]
    pub revision: char,
}

impl LibultraVersion {
    const fn read_major(input: u8) -> u8 {
        input / 10
    }

    const fn read_minor(input: u8) -> u8 {
        input % 10
    }
}

impl fmt::Debug for LibultraVersion {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}.{}{}", self.major, self.minor, self.revision)
    }
}

#[binrw]
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
#[brw(big, repr(u8))]
#[repr(u8)]
pub enum CategoryCode {
    GamePak = b'N',
    DiskDrive = b'D',
    ExpandableGamePak = b'C',
    ExpandableDiskDrive = b'E',
    Aleck64GamePak = b'Z',
    Unknown = 0, // Used by some homebrew ROMs
}

#[binrw]
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
#[brw(big, repr(u8))]
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
    Unknown = 0, // Used by some homebrew ROMs
}

#[binrw]
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct GameCode {
    pub category: CategoryCode,
    #[br(map(|x: [u8; 2]| x.map(|b| b as char)))]
    #[bw(map(|x: &[char; 2]| x.map(|c| c as u8)))]
    pub unique: [char; 2],
    pub destination: DestinationCode,
}

/// https://n64brew.dev/wiki/ROM_Header
#[binrw]
#[derive(PartialEq, Eq, PartialOrd, Ord)]
pub struct Header {
    pub pi_bsd_dom1_flags: u32,

    // The clock rate is manipulated when parsing, keep the original so that we can write it back.
    _raw_clock_rate: u32,
    #[br(map(Self::read_clock_rate), seek_before(SeekFrom::Current(-(size_of::<u32>() as i64))))]
    #[bw(ignore)]
    pub clock_rate: u32,

    pub boot_address: u32,
    pub libultra_version: LibultraVersion,
    pub ipl3_checksum: u64,
    _reserved_1: u64,
    #[br(map(Header::read_title))]
    #[bw(map(Header::write_title))]
    pub title: String,
    _reserved_2: [u8; 0x7],
    pub game_code: GameCode,
    pub version: u8,
}

impl Header {
    const TITLE_LEN: usize = 0x14;

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

    fn read_title(bytes: [u8; Self::TITLE_LEN]) -> String {
        String::from_utf8_lossy(&bytes)
            .to_string()
            .trim()
            .to_string()
    }

    fn write_title(title: &String) -> [u8; Self::TITLE_LEN] {
        let mut bytes = [0; Self::TITLE_LEN];
        bytes[..title.len()].copy_from_slice(title.as_bytes());
        bytes
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
                "ipl3_checksum",
                &format_args!("{:#016X}", self.ipl3_checksum),
            )
            .field("title", &self.title)
            .field("game_code", &self.game_code)
            .field("version", &self.version)
            .finish()
    }
}
