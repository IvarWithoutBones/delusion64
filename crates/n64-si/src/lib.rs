use self::register::{
    DramAddress, PifAddressRead4, PifAddressRead64, PifAddressWrite4, PifAddressWrite64, Status,
};

mod register;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum SiError {
    OffsetOutOfBounds { offset: usize },
}

pub type SiResult<T> = Result<T, SiError>;

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
        println!(
            "delusion64: si register write of {:#x} to {:#x}",
            value, offset
        );
        match offset {
            DramAddress::OFFSET => self.dram_address = value.into(),
            PifAddressRead64::OFFSET => self.pif_address_read_64 = value.into(),
            PifAddressWrite4::OFFSET => self.pif_address_write_4 = value.into(),
            PifAddressWrite64::OFFSET => {
                self.status.set_dma_busy(true);
                self.pif_address_write_64 = value.into();
            }
            PifAddressRead4::OFFSET => self.pif_address_read_4 = value.into(),
            Status::OFFSET => {
                // Writing to the status register clears the interrupt (if any).
                self.status.set_interrupt(false);
                side_effects.lower_interrupt = true;
            }
            _ => Err(SiError::OffsetOutOfBounds { offset })?,
        }
        println!("{self:#x?}");
        Ok(side_effects)
    }
}
