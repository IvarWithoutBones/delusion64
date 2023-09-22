//! The Peripheral Interface (PI), used for communication with the cartridge and disk drive.
//! See https://n64brew.dev/wiki/Peripheral_Interface, and https://github.com/Dillonb/n64-resources/blob/master/pi_dma.org.

use self::register::{
    CartAddress, DramAddress, Latch, PageSize, PulseWidth, ReadLength, Register, Release, Status,
    WriteLength,
};
use std::ops::Range;

mod register;

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum PiError {
    InvalidRegisterOffset(usize),
    UnimplementedDma { region: Region, domain: Domain },
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

    pub const fn offset(&self, addr: u32) -> u32 {
        match self {
            Region::Unknown => addr,
            Region::DiskDriveRegisters => addr - 0x0500_0000,
            Region::DiskDriveRom => addr - 0x0600_0000,
            Region::SRam => addr - 0x0800_0000,
            Region::CartridgeRom => addr - 0x1000_0000,
        }
    }

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

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct Mutated {
    pub rdram: Option<Range<u32>>,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum DmaStatus {
    Idle,
    Busy,
    Finished,
}

/// The Peripheral Interface (PI), used for communication with the cartridge and disk drive.
/// See https://n64brew.dev/wiki/Peripheral_Interface, and https://github.com/Dillonb/n64-resources/blob/master/pi_dma.org.
#[derive(Default, Debug, PartialEq, Eq)]
pub struct PeripheralInterface {
    dma_cycles_remaining: Option<u32>,

    // Registers
    dram_address: DramAddress,
    cart_address: CartAddress,
    read_length: ReadLength,
    write_length: WriteLength,
    // NOTE: Differentiates when reading or writing to it.
    status_read: Status,
    status_write: Status,
    domain_1_latch: Latch,
    domain_2_latch: Latch,
    domain_1_pulse_width: PulseWidth,
    domain_2_pulse_width: PulseWidth,
    domain_1_page_size: PageSize,
    domain_2_page_size: PageSize,
    domain_1_release: Release,
    domain_2_release: Release,
}

impl PeripheralInterface {
    pub fn new(bsd_domain_1_flags: u32) -> Self {
        // Initialize the PI registers based on the flags from the cartridge header.
        let mut pi = Self::default();
        for (i, byte) in bsd_domain_1_flags
            .to_be_bytes()
            .iter()
            .enumerate()
            .skip(1) // Unsure what the first byte is used for? Matches amount of bytes for unaligned DMA, and ReadLength/WriteLength read values.
            .map(|(i, b)| (i, *b as u32))
        {
            match i {
                1 => {
                    pi.domain_1_page_size = PageSize::from(byte);
                    pi.domain_1_release = Release::from(byte);
                }
                2 => pi.domain_1_pulse_width = PulseWidth::from(byte),
                3 => pi.domain_1_latch = Latch::from(byte),
                _ => unreachable!(),
            }
        }
        pi
    }

    pub fn read(&self, offset: usize) -> Result<u32, PiError> {
        match Register::new(offset)? {
            Register::DramAddress => Ok(self.dram_address.into()),
            Register::CartAddress => Ok(self.cart_address.into()),
            Register::Status => Ok(self.status_read.into()),
            Register::ReadLength => Ok(ReadLength::READ_VALUE),
            Register::WriteLength => Ok(WriteLength::READ_VALUE),

            Register::Latch(dom) => Ok(match dom {
                Domain::One => self.domain_1_latch.into(),
                Domain::Two => self.domain_2_latch.into(),
            }),

            Register::PulseWidth(dom) => Ok(match dom {
                Domain::One => self.domain_1_pulse_width.into(),
                Domain::Two => self.domain_2_pulse_width.into(),
            }),

            Register::PageSize(dom) => Ok(match dom {
                Domain::One => self.domain_1_page_size.into(),
                Domain::Two => self.domain_2_page_size.into(),
            }),

            Register::Release(dom) => Ok(match dom {
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
    ) -> Result<Mutated, PiError> {
        let mut mutated = None;
        match Register::new(offset)? {
            Register::DramAddress => self.dram_address = DramAddress::from(value),
            Register::CartAddress => self.cart_address = CartAddress::from(value),

            Register::ReadLength => {
                self.read_length = ReadLength::from(value);
                todo!("PI: read DMA {self:#x?}");
            }

            Register::WriteLength => {
                self.write_length = WriteLength::from(value);
                mutated = Some(self.dma(rdram, cart)?);
            }

            Register::Status => {
                let mut status = Status::from(value);

                if status.reset_dma() {
                    self.status_read.set_dma_busy(false);
                    self.status_read.set_io_busy(false);
                    self.reset_dma();
                }

                if status.clear_interrupt() {
                    self.status_read.set_interrupt(false);
                }

                self.status_write = status.with_reset_dma(false).with_clear_interrupt(false);
            }

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

        Ok(mutated.unwrap_or_default())
    }

    pub fn tick(&mut self) -> DmaStatus {
        if let Some(cycles) = &mut self.dma_cycles_remaining {
            if *cycles == 0 {
                self.reset_dma();
                DmaStatus::Finished
            } else {
                *cycles -= 1;
                DmaStatus::Busy
            }
        } else {
            DmaStatus::Idle
        }
    }

    fn reset_dma(&mut self) {
        self.dma_cycles_remaining.take();
        self.status_read.set_dma_busy(false);
        self.status_read.set_io_busy(false);
    }

    const fn dma_cycles(&self) -> u32 {
        100000 // TODO: implement this properly
    }

    fn dma(&mut self, rdram: &mut [u8], cart: &[u8]) -> Result<Mutated, PiError> {
        self.dma_cycles_remaining = Some(self.dma_cycles());
        self.status_read.set_dma_busy(true);
        self.status_read.set_io_busy(true);

        let cart_address = self.cart_address.address() as usize;
        match Region::new(cart_address as u32) {
            Region::CartridgeRom => {
                // TODO: use the configured `PageSize`
                let length = self.write_length.length() as usize;

                let rdram_offset = self.dram_address.address() as usize;
                let rdram_slice = &mut rdram[rdram_offset..=rdram_offset + length];

                let cart_offset = Region::CartridgeRom.offset(cart_address as u32) as usize;
                let cart_slice = &cart[cart_offset..=cart_offset + length];

                rdram_slice.copy_from_slice(cart_slice);
                println!("PI: DMA from cart {cart_offset:#x} to rdram {rdram_offset:#x} (length {length:#x})");

                Ok(Mutated {
                    rdram: Some(rdram_offset as u32..(rdram_offset + length + 1) as u32),
                })
            }

            region => Err(PiError::UnimplementedDma {
                region,
                domain: region.domain(),
            }),
        }
    }
}
