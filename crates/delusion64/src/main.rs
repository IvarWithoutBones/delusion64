use core::fmt;
use n64_cartridge::Cartridge;
use std::mem::size_of;

const MEMORY_SIZE: usize = 0xa5000000;

struct Emulator {
    memory: Box<[u8; MEMORY_SIZE]>,
}

impl Emulator {
    fn new() -> Self {
        Self {
            // Allocate memory directly on the heap using a vec to avoid stack overflows
            memory: vec![0; MEMORY_SIZE].into_boxed_slice().try_into().unwrap(),
        }
    }

    #[inline]
    unsafe fn read<T>(&self, addr: u64) -> T
    where
        T: Copy + Sized + fmt::LowerHex,
    {
        let ty = std::any::type_name::<T>();
        print!("read_{ty}: {addr:#x}");

        let addr = addr as usize;
        let values = self
            .memory
            .get(addr..addr + size_of::<T>())
            .unwrap_or_else(|| panic!("read memory index out of bounds: {addr:#x}"));

        let value = unsafe { std::ptr::read_unaligned(values.as_ptr() as _) };
        println!(" = {value:#x}");
        value
    }

    #[inline]
    unsafe fn write<T>(&mut self, addr: u64, value: T)
    where
        T: Copy + Sized + fmt::LowerHex,
    {
        let ty = std::any::type_name::<T>();
        println!("write_{ty}: {addr:#x} = {value:#x}");

        let addr = addr as usize;
        let values = self
            .memory
            .get_mut(addr..addr + size_of::<T>())
            .unwrap_or_else(|| panic!("write memory index out of bounds: {addr:#x}"));
        unsafe { std::ptr::write_unaligned(values.as_mut_ptr() as _, value) }
    }
}

impl mips_lifter::env::Memory for Emulator {
    #[inline]
    fn read_u8(&self, addr: u64) -> u8 {
        unsafe { self.read(addr) }
    }

    #[inline]
    fn read_u16(&self, addr: u64) -> u16 {
        unsafe { self.read(addr) }
    }

    #[inline]
    fn read_u32(&self, addr: u64) -> u32 {
        unsafe { self.read(addr) }
    }

    #[inline]
    fn read_u64(&self, addr: u64) -> u64 {
        unsafe { self.read(addr) }
    }

    #[inline]
    fn write_u8(&mut self, addr: u64, value: u8) {
        unsafe { self.write(addr, value) }
    }

    #[inline]
    fn write_u16(&mut self, addr: u64, value: u16) {
        unsafe { self.write(addr, value) }
    }

    #[inline]
    fn write_u32(&mut self, addr: u64, value: u32) {
        unsafe { self.write(addr, value) }
    }

    #[inline]
    fn write_u64(&mut self, addr: u64, value: u64) {
        unsafe { self.write(addr, value) }
    }
}

fn main() {
    let mut args = std::env::args().skip(1);

    let bin = {
        let path = args.next().expect("no file provided");
        std::fs::read(path).expect("failed to read file")
    };

    let cart = Cartridge::new(&bin).unwrap_or_else(|e| {
        eprintln!("failed to parse cartridge: {e}");
        std::process::exit(1);
    });
    println!("{cart:#?}");

    let maybe_output_path = args.next();
    let entry_point = 0;
    mips_lifter::lift(
        Emulator::new(),
        &cart.ipl3_boot_code.as_slice()[..0xb2c],
        maybe_output_path.as_deref(),
        entry_point,
    );
}
