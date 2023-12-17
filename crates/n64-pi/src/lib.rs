//! The Peripheral Interface (PI), used for communication with the cartridge and disk drive.
//! See [n64brew](https://n64brew.dev/wiki/Peripheral_Interface), and [Dillonb's DMA documentation](https://github.com/Dillonb/n64-resources/blob/master/pi_dma.org).

use self::register::{
    CartAddress, DramAddress, Latch, PageSize, PulseWidth, ReadLength, Register, Release, Status,
    WriteLength,
};
use n64_common::{memory::Section, utils::thiserror, InterruptDevice, SideEffects};
use std::ops::Range;

mod register;

#[derive(thiserror::Error, Debug, Clone, PartialEq, Eq)]
pub enum PiError {
    #[error("Offset {0:#x} is out of bounds")]
    InvalidRegisterOffset(usize),
    #[error("Unimplemented DMA: {region:?} (domain {domain:?})")]
    UnimplementedDma { region: Region, domain: Domain },
    #[error("RDRAM address {range:?} is out of bounds (RDRAM length: {rdram_len})")]
    RdramAddressOutOfBounds {
        range: Range<usize>,
        rdram_len: usize,
    },
    #[error("Cartridge address {range:?} is out of bounds (cartridge length: {cartridge_len})")]
    CartridgeAddressOutOfBounds {
        range: Range<usize>,
        cartridge_len: usize,
    },
}

pub type PiResult<T> = Result<T, PiError>;

/// See [n64brew](https://n64brew.dev/wiki/Peripheral_Interface#Domains).
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Domain {
    One,
    Two,
}

/// See [n64brew](https://n64brew.dev/wiki/Peripheral_Interface#Domains).
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Region {
    /// Writes to this region will be ignored, while reads will return open-bus data.
    /// See [n64brew](https://n64brew.dev/wiki/Peripheral_Interface#Open_bus_behavior).
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

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum BusDevice {
    CartridgeRom,
    CartridgeSram,
}

impl BusDevice {
    const fn cycles(&self) -> u32 {
        // TODO: implement this properly, these values are just guesses
        match self {
            BusDevice::CartridgeRom | BusDevice::CartridgeSram => 150,
        }
    }
}

/// The Peripheral Interface (PI), used for communication with the cartridge and disk drive.
/// See [n64brew](https://n64brew.dev/wiki/Peripheral_Interface).
#[derive(Default, Debug, PartialEq, Eq)]
pub struct PeripheralInterface {
    cartridge: Box<[u8]>,
    latch: Option<u32>,
    latch_cycles_remaining: Option<u32>,
    dma_cycles_remaining: Option<u32>,

    // Registers
    dram_address: DramAddress,
    cart_address: CartAddress,
    read_length: ReadLength,
    write_length: WriteLength,
    status: Status,
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
    pub fn new(cartridge: Box<[u8]>, bsd_domain_1_flags: u32) -> Self {
        // Initialize the PI registers based on the flags from the cartridge header.
        // Unsure what the first byte is used for? Matches amount of bytes for unaligned DMA, and ReadLength/WriteLength read values.
        let bsd_flags = bsd_domain_1_flags.to_be_bytes();
        let domain_1_page_size = PageSize::from(bsd_flags[1] as u32);
        let domain_1_release = Release::from(bsd_flags[1] as u32);
        let domain_1_pulse_width = PulseWidth::from(bsd_flags[2] as u32);
        let domain_1_latch = Latch::from(bsd_flags[3] as u32);

        Self {
            cartridge,
            domain_1_page_size,
            domain_1_release,
            domain_1_pulse_width,
            domain_1_latch,
            ..Default::default()
        }
    }

    pub fn read_bus<const SIZE: usize>(
        &mut self,
        device: BusDevice,
        offset: usize,
    ) -> PiResult<[u8; SIZE]> {
        // Reading from the latch will make it disappear, so in the case of a write -> read -> read, only the first read will return the latch value.
        if let Some(latch) = self.latch.take() {
            self.status.set_io_busy(false);
            let mut result = [0_u8; SIZE];
            match SIZE {
                1 => result[..SIZE].copy_from_slice(&[(latch >> 24) as u8]),
                2 => result[..SIZE].copy_from_slice(&((latch >> 16) as u16).to_be_bytes()),
                4 => result[..SIZE].copy_from_slice(&latch.to_be_bytes()),
                8 => result[..SIZE].copy_from_slice(&((latch as u64) << 32).to_be_bytes()),
                _ => unreachable!(),
            };
            return Ok(result);
        }

        // Align the offset to 4 bytes. The check should get monomorphized away.
        let offset = if (SIZE % 4) != 0 {
            (offset + 2) & !(SIZE + 1)
        } else {
            offset
        };

        match device {
            BusDevice::CartridgeSram => todo!("PI: read cartridge SRAM"),

            BusDevice::CartridgeRom => self
                .cartridge
                .get(offset..offset + SIZE)
                .ok_or(PiError::CartridgeAddressOutOfBounds {
                    range: offset..offset + SIZE,
                    cartridge_len: self.cartridge.len(),
                })
                .map(|slice| {
                    // SAFETY: We just checked that the slice is SIZE bytes long.
                    unsafe { slice.try_into().unwrap_unchecked() }
                }),
        }
    }

    pub fn write_bus<const SIZE: usize>(
        &mut self,
        device: BusDevice,
        offset: usize,
        value: &[u8; SIZE],
    ) -> PiResult<()> {
        if self.status.io_busy() {
            // The latch is still active, ignore the write
            return Ok(());
        }

        // SAFETY: we always check the size is correct before using it.
        self.latch = Some(match SIZE {
            1 => {
                let shift = 24 - ((offset & 1) * 8);
                (unsafe { *value.get_unchecked(0) } as u32) << shift
            }
            2 => {
                (u16::from_be_bytes(unsafe {
                    value.get_unchecked(..).try_into().unwrap_unchecked()
                }) as u32)
                    << 16
            }
            4 => {
                u32::from_be_bytes(unsafe { value.get_unchecked(..).try_into().unwrap_unchecked() })
            }
            8 => u32::from_be_bytes(unsafe {
                value.get_unchecked(..4).try_into().unwrap_unchecked()
            }),
            _ => unreachable!(),
        });

        self.status.set_io_busy(true);
        self.latch_cycles_remaining = Some(device.cycles());
        Ok(())
    }

    pub fn read_register(&self, offset: usize) -> PiResult<u32> {
        match Register::new(offset)? {
            Register::DramAddress => Ok(self.dram_address.into()),
            Register::CartAddress => Ok(self.cart_address.into()),
            Register::Status => Ok(self.status.into()),
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

    pub fn write_register(
        &mut self,
        offset: usize,
        value: u32,
        rdram: &mut [u8],
    ) -> PiResult<SideEffects> {
        let register = Register::new(offset)?;
        let mut side_effects = SideEffects::default();

        // Only Status can be written while we're busy
        if register != Register::Status && (self.status.io_busy() || self.status.dma_busy()) {
            self.status.set_error(true);
            return Ok(side_effects);
        }

        match register {
            Register::DramAddress => self.dram_address = DramAddress::from(value),
            Register::CartAddress => self.cart_address = CartAddress::from(value),

            Register::ReadLength => {
                self.read_length = ReadLength::from(value);
                todo!("PI: read DMA {self:#x?}");
            }

            Register::WriteLength => {
                self.write_length = WriteLength::from(value);
                side_effects = self.dma(rdram)?;
            }

            Register::Status => {
                let status = Status::from(value);
                if status.reset_dma() {
                    self.status.set_error(false);
                    self.reset_dma();
                }

                if status.clear_interrupt() {
                    self.status.set_interrupt(false);
                    side_effects.lower_interrupt(InterruptDevice::PeripheralInterface);
                }
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

        Ok(side_effects)
    }

    pub fn tick(&mut self, cycles: usize) -> SideEffects {
        let mut side_effects = SideEffects::new();
        if let Some(latch_cycles) = &mut self.latch_cycles_remaining {
            *latch_cycles = latch_cycles.saturating_sub(cycles as u32);
            if *latch_cycles == 0 {
                self.latch_cycles_remaining.take();
                self.latch.take();
                self.status.set_io_busy(false);
            }
        }

        if let Some(dma_cycles) = &mut self.dma_cycles_remaining {
            *dma_cycles = dma_cycles.saturating_sub(cycles as u32);
            if *dma_cycles == 0 {
                self.reset_dma();
                side_effects.raise_interrupt(InterruptDevice::PeripheralInterface);
            }
        }

        side_effects
    }

    fn reset_dma(&mut self) {
        self.dma_cycles_remaining.take();
        self.status.set_dma_busy(false);
    }

    const fn dma_cycles(&self) -> u32 {
        1 // TODO: implement this properly
    }

    fn dma(&mut self, rdram: &mut [u8]) -> PiResult<SideEffects> {
        // TODO: use the configured `PageSize`
        self.dma_cycles_remaining = Some(self.dma_cycles());
        self.status.set_dma_busy(true);

        let cart_address = self.cart_address.address() as usize;
        match Region::new(cart_address as u32) {
            Region::CartridgeRom => {
                let len = self.write_length.length() as usize + 1;

                let cart_offset = Region::CartridgeRom.offset(cart_address as u32) as usize;
                let cart_slice = self.cartridge.get(cart_offset..(cart_offset + len)).ok_or(
                    PiError::CartridgeAddressOutOfBounds {
                        range: cart_offset..(cart_offset + len),
                        cartridge_len: self.cartridge.len(),
                    },
                )?;

                let rdram_offset = self.dram_address.address() as usize;
                let rdram_range = rdram_offset..(rdram_offset + len);
                let rdram_slice = {
                    let rdram_len = rdram.len();
                    rdram
                        .get_mut(rdram_range.clone())
                        .ok_or(PiError::RdramAddressOutOfBounds {
                            range: rdram_range.clone(),
                            rdram_len,
                        })?
                };

                rdram_slice[..len].copy_from_slice(cart_slice);
                let mut side_effects = SideEffects::new();
                side_effects.set_dirty_section(Section::RdramMemory, rdram_range);
                Ok(side_effects)
            }

            region => Err(PiError::UnimplementedDma {
                region,
                domain: region.domain(),
            }),
        }
    }
}
