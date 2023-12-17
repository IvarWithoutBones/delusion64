use self::register::{
    DramAddress, PifAddressRead4, PifAddressRead64, PifAddressWrite4, PifAddressWrite64, Status,
};
use n64_common::{utils::thiserror, InterruptDevice, SideEffects};
use n64_pif::Pif;
use std::ops::Range;

pub use n64_pif::{controller, Channel, PifError};

mod register;

#[derive(thiserror::Error, Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum SiError {
    #[error("Offset {offset} is out of bounds")]
    OffsetOutOfBounds { offset: usize },
    #[error("Pif error: {0}")]
    PifError(#[from] PifError),
}
pub type SiResult<T> = Result<T, SiError>;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum DmaType {
    Read,
    Write,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
struct Dma {
    pub ty: DmaType,
    pub length: u32,
    pub cycles_remaining: usize,
    pub pif_address: u32,
    pub ram_address: u32,
}

impl Dma {
    #[must_use]
    pub fn tick(&mut self, cycles: usize) -> bool {
        self.cycles_remaining = self.cycles_remaining.saturating_sub(cycles);
        self.cycles_remaining == 0
    }
}

#[derive(Debug)]
pub struct SerialInterface {
    dram_address: DramAddress,
    pif_address_read_64: PifAddressRead64,
    pif_address_write_4: PifAddressWrite4,
    pif_address_write_64: PifAddressWrite64,
    pif_address_read_4: PifAddressRead4,
    status: Status,

    dma: Option<Dma>,
    pub pif: Pif,
}

impl SerialInterface {
    pub fn new(cic_seed: u32) -> Self {
        Self {
            dram_address: DramAddress::default(),
            pif_address_read_64: PifAddressRead64::default(),
            pif_address_write_4: PifAddressWrite4::default(),
            pif_address_write_64: PifAddressWrite64::default(),
            pif_address_read_4: PifAddressRead4::default(),
            status: Status::default(),
            dma: None,
            pif: Pif::new(cic_seed),
        }
    }

    pub fn read_pif_rom<const SIZE: usize>(&self, offset: usize) -> SiResult<&[u8; SIZE]> {
        Ok(self.pif.read(n64_pif::Region::Rom, offset)?)
    }

    pub fn read_pif_ram<const SIZE: usize>(&self, offset: usize) -> SiResult<&[u8; SIZE]> {
        Ok(self.pif.read(n64_pif::Region::Ram, offset)?)
    }

    pub fn write_pif_ram<const SIZE: usize>(
        &mut self,
        offset: usize,
        value: &[u8; SIZE],
    ) -> SiResult<()> {
        Ok(self.pif.write_ram(offset, value)?)
    }

    pub fn read(&mut self, offset: usize) -> SiResult<u32> {
        let result = match offset {
            DramAddress::OFFSET => self.dram_address.address(),
            PifAddressRead64::OFFSET => self.pif_address_read_64.offset(),
            PifAddressWrite4::OFFSET => self.pif_address_write_4.into(),
            PifAddressWrite64::OFFSET => self.pif_address_write_64.address(),
            PifAddressRead4::OFFSET => self.pif_address_read_4.into(),
            Status::OFFSET => self.status.into(),
            _ => Err(SiError::OffsetOutOfBounds { offset })?,
        };
        Ok(result)
    }

    pub fn write(&mut self, offset: usize, value: u32, rdram: &mut [u8]) -> SiResult<SideEffects> {
        let mut side_effects = SideEffects::default();
        match offset {
            DramAddress::OFFSET => self.dram_address = value.into(),
            PifAddressRead64::OFFSET => {
                self.pif_address_read_64 = value.into();
                self.status.set_dma_busy(true);
                self.dma = Some(Dma {
                    ty: DmaType::Read,
                    length: 64,
                    cycles_remaining: 1, // TODO: inaccurate
                    pif_address: self.pif_address_read_64.offset(),
                    ram_address: self.dram_address.address(),
                });

                self.pif.read_dma(
                    self.pif_address_read_64.offset(),
                    &mut rdram[self.rdram_range()],
                )?;
            }
            PifAddressWrite4::OFFSET => self.pif_address_write_4 = value.into(),
            PifAddressWrite64::OFFSET => {
                self.pif_address_write_64 = value.into();
                self.status.set_dma_busy(true);
                self.dma = Some(Dma {
                    ty: DmaType::Write,
                    length: 64,
                    cycles_remaining: 4065 * 3,
                    pif_address: self.pif_address_write_64.address(),
                    ram_address: self.dram_address.address(),
                });

                self.pif.write_dma(
                    self.pif_address_write_64.address(),
                    &rdram[self.rdram_range()],
                )?;
            }
            PifAddressRead4::OFFSET => self.pif_address_read_4 = value.into(),
            Status::OFFSET => {
                // Writing to the status register clears the interrupt (if any).
                self.status.set_interrupt(false);
                side_effects.lower_interrupt(InterruptDevice::SerialInterface);
            }
            _ => Err(SiError::OffsetOutOfBounds { offset })?,
        }
        Ok(side_effects)
    }

    pub fn tick(&mut self, cycles: usize) -> SideEffects {
        let mut side_effects = SideEffects::new();
        if let Some(dma) = &mut self.dma {
            if dma.tick(cycles) {
                self.status.set_dma_busy(false);
                self.status.set_interrupt(true);
                self.dma = None;
                side_effects.raise_interrupt(InterruptDevice::SerialInterface);
            }
        }
        side_effects
    }

    fn rdram_range(&self) -> Range<usize> {
        let rdram_addr = self.dram_address.address() as usize;
        rdram_addr..rdram_addr + n64_pif::Region::Ram.len()
    }
}
