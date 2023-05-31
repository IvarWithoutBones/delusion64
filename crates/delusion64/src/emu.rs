const MEMORY_SIZE: usize = 0x100000000;

pub struct Emulator {
    pub memory: Box<[u8; MEMORY_SIZE]>,
}

impl Emulator {
    pub fn new() -> Self {
        Self {
            // Allocate memory directly on the heap using a vec to avoid stack overflows
            memory: vec![0; MEMORY_SIZE].into_boxed_slice().try_into().unwrap(),
        }
    }
}

impl mips_lifter::env::Memory for Emulator {
    #[inline]
    fn read_u8(&self, addr: u64) -> u8 {
        self.memory[addr as usize]
    }

    #[inline]
    fn write_u8(&mut self, addr: u64, value: u8) {
        self.memory[addr as usize] = value;
    }
}
