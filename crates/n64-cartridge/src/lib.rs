use crate::header::Header;
use binrw::{binrw, io::Cursor, BinRead, BinResult, BinWrite};
use std::{fmt, io::SeekFrom};

mod header;

pub const HEADER_SIZE: usize = 64;
pub const IPL3_SIZE: usize = 0x1000 - HEADER_SIZE;

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum CartridgeError {
    InvalidHeader(String),
    UnknownCic { crc: u32 },
}

impl fmt::Display for CartridgeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::InvalidHeader(s) => write!(f, "invalid header: {s}"),
            Self::UnknownCic { crc } => write!(f, "unknown CIC: {crc:#x}"),
        }
    }
}

pub type CartridgeResult<T> = Result<T, CartridgeError>;

/// See [n64brew](https://n64brew.dev/wiki/CIC-NUS#Variants).
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub enum Cic {
    Cic6101,
    Cic6102,
    Cic6103,
    Cic6105,
    Cic6106,
    Cic7102,
    Cic8303,
}

impl Cic {
    pub fn new(bin: &[u8; IPL3_SIZE]) -> CartridgeResult<Self> {
        const CRC: crc::Crc<u32> = crc::Crc::<u32>::new(&crc::CRC_32_ISO_HDLC);
        match CRC.checksum(bin) {
            0x6170_A4A1 => Ok(Self::Cic6101),
            0x90BB_6CB5 => Ok(Self::Cic6102),
            0x0B05_0EE0 => Ok(Self::Cic6103),
            0x98BC_2C86 => Ok(Self::Cic6105),
            0xACC8_580A => Ok(Self::Cic6106),
            0x009E_9EA3 => Ok(Self::Cic7102),
            0x0E01_8159 => Ok(Self::Cic8303),
            crc => Err(CartridgeError::UnknownCic { crc }),
        }
    }

    pub const fn seed(&self) -> u32 {
        match self {
            Cic::Cic6101 | Cic::Cic7102 => 0x0004_3F3F,
            Cic::Cic6102 => 0x0000_3F3F,
            Cic::Cic6103 => 0x0000_783F,
            Cic::Cic6105 => 0x0000_913F,
            Cic::Cic6106 => 0x0000_853F,
            Cic::Cic8303 => 0x0000_DD00,
        }
    }

    /// The offset in RDRAM where the size of RDRAM is stored during IPL.
    pub const fn rdram_len_offset(&self) -> Option<usize> {
        match self {
            Cic::Cic6105 => Some(0x3F0),
            Cic::Cic6106 => None,
            _ => Some(0x318),
        }
    }
}

#[binrw]
#[derive(PartialEq, Eq, PartialOrd, Ord)]
pub struct Cartridge {
    pub header: Header,
    #[bw(ignore)]
    #[br(map = |b| Cic::new(&b), seek_before(SeekFrom::Current(2)), restore_position)]
    pub cic: CartridgeResult<Cic>,
    #[br(parse_with = Self::read_remainder)]
    pub data: Box<[u8]>,
}

impl Cartridge {
    pub fn new(bin: &[u8]) -> CartridgeResult<Self> {
        let mut cursor = Cursor::new(bin);
        Self::read_be(&mut cursor).map_err(|e| CartridgeError::InvalidHeader(e.to_string()))
    }

    pub fn read(&self) -> Option<Box<[u8]>> {
        let mut buf = Vec::new();
        let mut cursor = Cursor::new(&mut buf);
        self.write_be(&mut cursor).ok()?;
        Some(buf.into_boxed_slice())
    }

    #[binrw::parser(reader, endian)]
    fn read_remainder() -> BinResult<Box<[u8]>> {
        let _ = endian; // Silence warning about this being unused, we cannot add an underscore because of the macro
        let mut buf = Vec::new();
        reader.read_to_end(&mut buf)?;
        Ok(buf.into_boxed_slice())
    }
}

impl fmt::Debug for Cartridge {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Cartridge")
            .field("header", &self.header)
            .field("cic", &self.cic)
            .field("data", &format_args!("[u8; {:#x}]", self.data.len()))
            .finish()
    }
}
