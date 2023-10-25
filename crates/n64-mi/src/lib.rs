use std::mem::size_of;
use tartan_bitfield::bitfield;

bitfield! {
    /// https://n64brew.dev/wiki/MIPS_Interface#0x0430_0000_-_MI_MODE
    pub struct Mode(u32) {
        [0..=6] init_length: u8,

        // When reading
        [7] init_mode,
        [8] ebus_test_mode,
        [9] rdram_register_mode,

        // When writing
        [7] clear_init_mode,
        [8] write_init_mode,
        [9] clear_ebus_test_mode,
        [10] write_ebus_test_mode,
        [11] clear_dp_interrupt,
        [12] clear_rdram_register_mode,
        [13] write_rdram_register_mode,
    }
}

impl Mode {
    const INDEX: usize = 0;
}

bitfield! {
    /// https://n64brew.dev/wiki/MIPS_Interface#0x0430_0004_-_MI_VERSION
    pub struct Version(u32) {
        [0..=7] io_version: u8,
        [8..=15] rac_version: u8,
        [16..=23] rdp_version: u8,
        [24..=31] rsp_version: u8,
    }
}

impl Version {
    const INDEX: usize = 1;

    const fn new() -> Self {
        // Most consoles report this, according to n64brew.
        Self(0x0202_0102)
    }
}

bitfield! {
    /// https://n64brew.dev/wiki/MIPS_Interface#0x0430_0008_-_MI_INTERRUPT
    pub struct Interrupt(u32) {
        /// Set when the RSP executes a BREAK opcode while SP_STATUS has been configured with the INTERRUPT_ON_BREAK bit
        [0] sp,
        /// Set when a SI DMA to/from PIF RAM finishes
        [1] si,
        /// Set when the AI begins playing back a new audio buffer
        [2] ai,
        /// Set when the VI starts processing a specific half-line of the screen
        [3] vi,
        /// Set when a PI DMA transfer finishes
        [4] pi,
        /// Set when the RDP finishes a full sync
        [5] dp,
    }
}

impl Interrupt {
    const INDEX: usize = 2;
}

bitfield! {
    /// https://n64brew.dev/wiki/MIPS_Interface#0x0430_000C_-_MI_MASK
    pub struct Mask(u32) {
        // When reading
        [0] sp_mask,
        [1] si_mask,
        [2] ai_mask,
        [3] vi_mask,
        [4] pi_mask,
        [5] dp_mask,

        // When writing
        [0] clear_sp_mask,
        [1] write_sp_mask,
        [2] clear_si_mask,
        [3] write_si_mask,
        [4] clear_ai_mask,
        [5] write_ai_mask,
        [6] clear_vi_mask,
        [7] write_vi_mask,
        [8] clear_pi_mask,
        [9] write_pi_mask,
        [10] clear_dp_mask,
        [11] write_dp_mask,
    }
}

impl Mask {
    const INDEX: usize = 3;
}

#[derive(Debug)]
pub enum Register {
    Mode(Mode),
    Version(Version),
    Interrupt(Interrupt),
    Mask(Mask),
}

impl From<Mode> for Register {
    fn from(value: Mode) -> Self {
        Self::Mode(value)
    }
}

impl From<Version> for Register {
    fn from(value: Version) -> Self {
        Self::Version(value)
    }
}

impl From<Interrupt> for Register {
    fn from(value: Interrupt) -> Self {
        Self::Interrupt(value)
    }
}

impl From<Mask> for Register {
    fn from(value: Mask) -> Self {
        Self::Mask(value)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum InterruptType {
    RspBreak = 0,
    SerialInterface = 1,
    AudioInterface = 2,
    VideoInterface = 3,
    PeripheralInterface = 4,
    RdpSync = 5,
}

#[derive(Debug)]
pub enum MiError {
    RegisterNotFound(usize),
    ReadOnlyRegisterWrite(Register),
}

#[derive(Debug)]
pub struct MipsInterface {
    mode: Mode,
    version: Version,
    interrupt: Interrupt,
    mask: Mask,
}

impl MipsInterface {
    /// A mask that covers the interrupt bits in the IP field of the CP0 Cause register.
    pub const INTERRUPT_PENDING_MASK: u8 = 0b100;

    pub fn new() -> Self {
        Self {
            mode: Mode::default(),
            version: Version::new(),
            interrupt: Interrupt::default(),
            mask: Mask::default(),
        }
    }

    const fn offset_to_index(offset: usize) -> usize {
        // The MI registers are mirrored every 16 bytes.
        (offset & 0b1111) / size_of::<u32>()
    }

    pub fn write(&mut self, offset: usize, value: u32) -> Result<(), MiError> {
        // Note that setting both clear and write bits at the same time has an unspecified result, we let the write take precedence.
        let index = Self::offset_to_index(offset);
        // println!("Write to MI register {index:#x?} = {self:#x?}");
        match index {
            Mode::INDEX => {
                let mode = Mode(value);
                if mode.clear_dp_interrupt() {
                    self.interrupt.set_dp(false);
                }

                if mode.write_init_mode() {
                    self.mode.set_init_mode(true);
                } else if mode.clear_init_mode() {
                    self.mode.set_init_mode(false);
                }

                if mode.write_rdram_register_mode() {
                    self.mode.set_ebus_test_mode(true);
                } else if mode.clear_ebus_test_mode() {
                    self.mode.set_ebus_test_mode(false);
                }

                if mode.write_rdram_register_mode() {
                    self.mode.set_rdram_register_mode(true);
                } else if mode.clear_rdram_register_mode() {
                    self.mode.set_rdram_register_mode(false);
                }

                Ok(())
            }
            Version::INDEX => Err(MiError::ReadOnlyRegisterWrite(Version(value).into())),
            Interrupt::INDEX => Err(MiError::ReadOnlyRegisterWrite(Interrupt(value).into())),
            Mask::INDEX => {
                let mask = Mask(value);
                if mask.write_sp_mask() {
                    self.mask.set_sp_mask(true);
                } else if mask.clear_sp_mask() {
                    self.mask.set_sp_mask(false);
                }

                if mask.write_si_mask() {
                    self.mask.set_si_mask(true);
                } else if mask.clear_si_mask() {
                    self.mask.set_si_mask(false);
                }

                if mask.write_ai_mask() {
                    self.mask.set_ai_mask(true);
                } else if mask.clear_ai_mask() {
                    self.mask.set_ai_mask(false);
                }

                if mask.write_vi_mask() {
                    self.mask.set_vi_mask(true);
                } else if mask.clear_vi_mask() {
                    self.mask.set_vi_mask(false);
                }

                if mask.write_pi_mask() {
                    self.mask.set_pi_mask(true);
                } else if mask.clear_pi_mask() {
                    self.mask.set_pi_mask(false);
                }

                if mask.write_dp_mask() {
                    self.mask.set_dp_mask(true);
                } else if mask.clear_dp_mask() {
                    self.mask.set_dp_mask(false);
                }

                Ok(())
            }
            _ => Err(MiError::RegisterNotFound(index)),
        }
    }

    pub fn read(&self, offset: usize) -> Result<u32, MiError> {
        let index = Self::offset_to_index(offset);
        let value = match index {
            Mode::INDEX => self.mode.into(),
            Version::INDEX => self.version.into(),
            Interrupt::INDEX => self.interrupt.into(),
            Mask::INDEX => self.mask.into(),
            _ => return Err(MiError::RegisterNotFound(index)),
        };
        // println!("Read from MI register {index:#x?} = {self:#x?}");
        Ok(value)
    }

    pub fn should_interrupt(&self) -> bool {
        (self.interrupt.sp() && self.mask.sp_mask())
            || (self.interrupt.si() && self.mask.si_mask())
            || (self.interrupt.ai() && self.mask.ai_mask())
            || (self.interrupt.vi() && self.mask.vi_mask())
            || (self.interrupt.pi() && self.mask.pi_mask())
            || (self.interrupt.dp() && self.mask.dp_mask())
    }

    /// Sets the interrupt bit for the given interrupt type, returning whether to raise an interrupt or not by masking.
    fn set_interrupt(&mut self, interrupt: InterruptType, value: bool) -> bool {
        match interrupt {
            InterruptType::RspBreak => self.interrupt.set_sp(value),
            InterruptType::SerialInterface => self.interrupt.set_si(value),
            InterruptType::AudioInterface => self.interrupt.set_ai(value),
            InterruptType::VideoInterface => self.interrupt.set_vi(value),
            InterruptType::PeripheralInterface => self.interrupt.set_pi(value),
            InterruptType::RdpSync => self.interrupt.set_dp(value),
        }
        self.should_interrupt()
    }

    pub fn raise_interrupt(&mut self, interrupt: InterruptType) -> bool {
        self.set_interrupt(interrupt, true)
    }

    pub fn lower_interrupt(&mut self, interrupt: InterruptType) -> bool {
        self.set_interrupt(interrupt, false)
    }
}

impl Default for MipsInterface {
    fn default() -> Self {
        Self::new()
    }
}
