use crate::header::Header;
use binrw::{io::Cursor, BinRead};

mod header;

pub const IPL3_BOOT_CODE_LEN: usize = 0xFC0;

pub struct Cartridge {
    pub header: Header,
    pub ipl3_boot_code: Box<[u8; IPL3_BOOT_CODE_LEN]>,
    pub data: Box<[u8]>,
}

impl Cartridge {
    pub fn new(bin: &[u8]) -> Option<Self> {
        let header = {
            let mut cursor = Cursor::new(bin);
            Header::read(&mut cursor).ok()?
        };

        let ipl3_boot_code = bin[header::LEN..header::LEN + IPL3_BOOT_CODE_LEN]
            .to_vec()
            .into_boxed_slice()
            .try_into()
            .ok()?;
        let data = bin[header::LEN + IPL3_BOOT_CODE_LEN..]
            .to_vec()
            .into_boxed_slice();

        Some(Self {
            header,
            ipl3_boot_code,
            data,
        })
    }
}

impl std::fmt::Debug for Cartridge {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
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
