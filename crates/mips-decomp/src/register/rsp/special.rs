use tartan_bitfield::bitfield;

bitfield! {
    /// The memory-mapped RSP program counter.
    /// See [n64brew](https://n64brew.dev/wiki/Reality_Signal_Processor/Interface#RSP_PC_register) for more information.
    pub struct ProgramCounter(u32) {
        [0..=11] pc: u32,
    }
}

impl ProgramCounter {
    pub const OFFSET: usize = 0x40000; // 0x0408_0000

    /// Creates a new program counter register with the given value.
    #[must_use]
    pub fn from_raw(value: u32) -> Self {
        let mut res = Self::default();
        res.write(value);
        res
    }

    /// Writes the given value into the program counter register.
    pub fn write(&mut self, value: u32) {
        // The least significant two bits are always zero (i.e. aligned to 4 bytes)
        self.set_pc(value & !0b11);
    }

    /// Returns the program counter register.
    #[must_use]
    pub fn read(&self) -> u32 {
        self.pc()
    }

    pub fn increment(&mut self, amount: u32) {
        self.write(self.read() + amount);
    }
}
