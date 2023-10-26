use self::register::{
    DramAddress, PifAddressRead4, PifAddressRead64, PifAddressWrite4, PifAddressWrite64, Status,
};

mod register;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum SiError {
    OffsetOutOfBounds { offset: usize },
}

pub type SiResult<T> = Result<T, SiError>;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum DmaStatus {
    Idle,
    Completed,
    Busy,
}

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
    pub fn tick(&mut self) -> bool {
        if self.cycles_remaining == 0 {
            true
        } else {
            self.cycles_remaining -= 1;
            false
        }
    }
}

#[derive(Debug, Default)]
pub struct SideEffects {
    pub lower_interrupt: bool,
}

#[derive(Debug, Default)]
pub struct SerialInterface {
    dram_address: DramAddress,
    pif_address_read_64: PifAddressRead64,
    pif_address_write_4: PifAddressWrite4,
    pif_address_write_64: PifAddressWrite64,
    pif_address_read_4: PifAddressRead4,
    status: Status,
    dma: Option<Dma>,
}

impl SerialInterface {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn read(&mut self, offset: usize) -> SiResult<u32> {
        let result = match offset {
            DramAddress::OFFSET => self.dram_address.address(),
            PifAddressRead64::OFFSET => self.pif_address_read_64.pif_address().into(),
            PifAddressWrite4::OFFSET => self.pif_address_write_4.into(),
            PifAddressWrite64::OFFSET => self.pif_address_write_64.data(),
            PifAddressRead4::OFFSET => self.pif_address_read_4.into(),
            Status::OFFSET => self.status.into(),
            _ => Err(SiError::OffsetOutOfBounds { offset })?,
        };
        Ok(result)
    }

    pub fn write(&mut self, offset: usize, value: u32) -> SiResult<SideEffects> {
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
                    pif_address: self.pif_address_read_64.pif_address() as u32,
                    ram_address: self.dram_address.address(),
                })
            }
            PifAddressWrite4::OFFSET => self.pif_address_write_4 = value.into(),
            PifAddressWrite64::OFFSET => {
                self.pif_address_write_64 = value.into();
                self.status.set_dma_busy(true);
                self.dma = Some(Dma {
                    ty: DmaType::Write,
                    length: 64,
                    cycles_remaining: 4065 * 3,
                    pif_address: self.pif_address_write_64.data(),
                    ram_address: self.dram_address.address(),
                })
            }
            PifAddressRead4::OFFSET => self.pif_address_read_4 = value.into(),
            Status::OFFSET => {
                // Writing to the status register clears the interrupt (if any).
                self.status.set_interrupt(false);
                side_effects.lower_interrupt = true;
            }
            _ => Err(SiError::OffsetOutOfBounds { offset })?,
        }
        Ok(side_effects)
    }

    pub fn tick(&mut self) -> DmaStatus {
        if let Some(dma) = &mut self.dma {
            if dma.tick() {
                self.status.set_dma_busy(false);
                self.status.set_interrupt(true);
                self.dma = None;
                DmaStatus::Completed
            } else {
                DmaStatus::Busy
            }
        } else {
            DmaStatus::Idle
        }
    }
}
