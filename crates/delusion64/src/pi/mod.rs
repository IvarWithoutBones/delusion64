//! The Peripheral Interface (PI), used for communication with the cartridge and disk drive.
//! See https://n64brew.dev/wiki/Peripheral_Interface, and https://github.com/Dillonb/n64-resources/blob/master/pi_dma.org.

use self::register::{
    CartAddress, DramAddress, Latch, PageSize, PulseWidth, ReadLength, Register, Release, Status,
    WriteLength,
};

mod register;

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum DmaType {
    FromMemory,
    ToMemory,
}

/// See https://n64brew.dev/wiki/Peripheral_Interface#Domains.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Domain {
    One,
    Two,
}

/// See https://n64brew.dev/wiki/Peripheral_Interface#Domains.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Region {
    /// Writes to this region will be ignored, while reads will return open-bus data.
    /// See https://n64brew.dev/wiki/Peripheral_Interface#Open_bus_behavior.
    Unknown,
    DiskDriveRegisters,
    DiskDriveRom,
    SRam,
    CartridgeRom,
}

impl Region {
    pub const fn new(addr: u32) -> Self {
        match addr {
            0x0000_0000..=0x04FF_FFFF => Region::Unknown,
            0x0500_0000..=0x05FF_FFFF => Region::DiskDriveRegisters,
            0x0600_0000..=0x07FF_FFFF => Region::DiskDriveRom,
            0x0800_0000..=0x0FFF_FFFF => Region::SRam,
            0x1000_0000..=0xFFFF_FFFF => Region::CartridgeRom,
        }
    }

    // TODO: do we need this?
    pub const fn domain(&self) -> Domain {
        match self {
            // This is hardcoded in the PI itself, according to n64brew.
            Region::Unknown => Domain::One,
            Region::DiskDriveRegisters => Domain::Two,
            Region::DiskDriveRom => Domain::One,
            Region::SRam => Domain::Two,
            Region::CartridgeRom => Domain::One,
        }
    }
}

/// The Peripheral Interface (PI), used for communication with the cartridge and disk drive.
/// See https://n64brew.dev/wiki/Peripheral_Interface, and https://github.com/Dillonb/n64-resources/blob/master/pi_dma.org.
#[derive(Default, Debug, PartialEq, Eq)]
pub struct PeripheralInterface {
    pub dram_address: DramAddress,
    pub cart_address: CartAddress,
    pub read_length: ReadLength,
    pub write_length: WriteLength,
    pub status: Status,

    pub domain_1_latch: Latch,
    pub domain_2_latch: Latch,

    pub domain_1_pulse_width: PulseWidth,
    pub domain_2_pulse_width: PulseWidth,

    pub domain_1_page_size: PageSize,
    pub domain_2_page_size: PageSize,

    pub domain_1_release: Release,
    pub domain_2_release: Release,
}

impl PeripheralInterface {
    pub fn new() -> Self {
        // TODO: configure the BSD registers based on the cartridge header.
        Default::default()
    }

    pub fn read(&self, offset: usize) -> Option<u32> {
        match Register::new(offset)? {
            Register::DramAddress => Some(self.dram_address.into()),
            Register::CartAddress => Some(self.cart_address.into()),
            Register::Status => Some(self.status.into()),

            // Reading these registers appears to always return 0x7F, according to n64brew.
            Register::ReadLength => Some(0x7F),
            Register::WriteLength => Some(0x7F),

            Register::Latch(dom) => Some(match dom {
                Domain::One => self.domain_1_latch.into(),
                Domain::Two => self.domain_2_latch.into(),
            }),

            Register::PulseWidth(dom) => Some(match dom {
                Domain::One => self.domain_1_pulse_width.into(),
                Domain::Two => self.domain_2_pulse_width.into(),
            }),

            Register::PageSize(dom) => Some(match dom {
                Domain::One => self.domain_1_page_size.into(),
                Domain::Two => self.domain_2_page_size.into(),
            }),

            Register::Release(dom) => Some(match dom {
                Domain::One => self.domain_1_release.into(),
                Domain::Two => self.domain_2_release.into(),
            }),
        }
    }

    pub fn write(
        &mut self,
        offset: usize,
        value: u32,
        rdram: &mut [u8],
        cart: &mut [u8],
    ) -> Option<()> {
        match Register::new(offset)? {
            Register::DramAddress => self.dram_address = DramAddress::from(value),
            Register::CartAddress => self.cart_address = CartAddress::from(value),

            Register::ReadLength => {
                self.read_length = ReadLength::from(value);
                self.start_dma(DmaType::FromMemory, rdram, cart);
                panic!("PI: started read DMA {self:#x?}");
            }

            Register::WriteLength => {
                self.write_length = WriteLength::from(value);
                self.start_dma(DmaType::ToMemory, rdram, cart);
            }

            Register::Status => self.status.write(value),

            Register::Latch(dom) => match dom {
                Domain::One => self.domain_1_latch = Latch::from(value),
                Domain::Two => self.domain_2_latch = Latch::from(value),
            },

            Register::PulseWidth(dom) => match dom {
                Domain::One => self.domain_1_pulse_width = PulseWidth::from(value),
                Domain::Two => self.domain_2_pulse_width = PulseWidth::from(value),
            },

            Register::PageSize(dom) => match dom {
                Domain::One => self.domain_1_page_size = PageSize::from(value),
                Domain::Two => self.domain_2_page_size = PageSize::from(value),
            },

            Register::Release(dom) => match dom {
                Domain::One => self.domain_1_release = Release::from(value),
                Domain::Two => self.domain_2_release = Release::from(value),
            },
        };

        Some(())
    }

    pub fn start_dma(&mut self, ty: DmaType, rdram: &mut [u8], cart: &mut [u8]) {
        assert!(ty == DmaType::ToMemory); // TODO: implement DMA to cartridge.
        let cart_address = self.cart_address.address() as usize;
        let region = Region::new(cart_address as _);
        let domain = region.domain();
        match region {
            Region::CartridgeRom => {
                // TODO: use the configured `PageSize` for the given domain, prettify
                let cart_offset = cart_address - 0x1000_0000;
                let rdram_offset = self.dram_address.address() as usize;
                let length = self.write_length.length() as usize;
                rdram[rdram_offset..=rdram_offset + length]
                    .copy_from_slice(&cart[cart_offset..=cart_offset + length]);
                println!("PI: DMA from cart {cart_offset:#x} to rdram {rdram_offset:#x} (length {length:#x})");
            }

            _ => panic!(
                "PI: DMA to unknown address {cart_address:#x} (region {region:?}, domain {domain:?})"
            ),
        }
    }
}
