use crate::header::Header;
use binrw::{binrw, io::Cursor, BinRead, BinResult, BinWrite};
use std::fmt;

mod header;

pub const IPL3_BOOT_CODE_LEN: usize = 0xFC0;

#[binrw]
#[derive(PartialEq, Eq, PartialOrd, Ord)]
pub struct Cartridge {
    pub header: Header,
    pub ipl3_boot_code: Box<[u8; IPL3_BOOT_CODE_LEN]>,
    #[br(parse_with = Self::read_remainder)]
    pub data: Box<[u8]>,
}

impl Cartridge {
    pub fn new(bin: &[u8]) -> Result<Self, String> {
        let mut cursor = Cursor::new(bin);
        Self::read_be(&mut cursor).map_err(|e| e.to_string())
    }

    pub fn read(&self) -> Option<Vec<u8>> {
        let mut buf = Vec::new();
        let mut cursor = Cursor::new(&mut buf);
        self.write_be(&mut cursor).ok()?;
        Some(buf)
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
            .field(
                "ipl3_boot_code",
                &format_args!("[u8; {IPL3_BOOT_CODE_LEN:#x}]"),
            )
            .field("data", &format_args!("[u8; {:#x}]", self.data.len()))
            .finish()
    }
}
