use std::ops::Range;

pub const RDRAM_MEM: Range<u64> = 0x00000000..0x03F00000;
pub const RDRAM_REGISTERS: Range<u64> = 0x03F00000..0x03F80000;
pub const RDRAM_REGISTERS_WRITE_ONLY: Range<u64> = 0x03F80000..0x04000000;

pub const RSP_DMEM: Range<u64> = 0x04000000..0x04001000;
pub const RSP_IMEM: Range<u64> = 0x04001000..0x04002000;
pub const RSP_DRAM_IRAM_MIRRORS: Range<u64> = 0x04002000..0x04040000;
pub const RSP_REGISTERS: Range<u64> = 0x04040000..0x040C0000;
pub const RSP_COMMAND_REGISTERS: Range<u64> = 0x04100000..0x041FFFFF;
pub const RSP_SPAN_REGISTERS: Range<u64> = 0x04200000..0x04300000;

pub const MIPS_INTERFACE: Range<u64> = 0x04300000..0x04400000;
pub const VIDEO_INTERFACE: Range<u64> = 0x04400000..0x04500000;
pub const AUDIO_INTERFACE: Range<u64> = 0x04500000..0x04600000;
pub const PERIPHERAL_INTERFACE: Range<u64> = 0x04600000..0x04700000;
pub const RDRAM_INTERFACE: Range<u64> = 0x04700000..0x04800000;
pub const SERIAL_INTERFACE: Range<u64> = 0x04800000..0x048FFFFF;

pub const DISK_DRIVE_REGISTERS: Range<u64> = 0x05000000..0x06000000;
pub const DISK_DRIVE_IPL4_ROM: Range<u64> = 0x06000000..0x08000000;

pub const CARTRIDGE_SRAM: Range<u64> = 0x08000000..0x10000000;
pub const CARTRIDGE_ROM: Range<u64> = 0x10000000..0x1FC00000;

pub const PIF_ROM: Range<u64> = 0x1FC00000..0x1FC007C0;
pub const PIF_RAM: Range<u64> = 0x1FC007C0..0x1FC00800;

const fn range_len(range: Range<u64>) -> usize {
    (range.end - range.start) as usize
}

const fn subtract_range(range: Range<u64>, addr: u64) -> usize {
    (addr - range.start) as usize
}

/// Allocates a boxed slice of a given length on the heap.
fn boxed_slice<const LEN: usize>() -> Box<[u8; LEN]> {
    // Use `vec!` to avoid stack allocation, it may overflow.
    vec![0; LEN].into_boxed_slice().try_into().unwrap()
}

pub struct Emulator {
    pub rdram: Box<[u8; range_len(RDRAM_MEM)]>,
    pub rsp_dmem: Box<[u8; range_len(RSP_DMEM)]>,
    pub rsp_imem: Box<[u8; range_len(RSP_IMEM)]>,
    pub cartridge_rom: Box<[u8]>,
}

impl Emulator {
    pub fn new(cartridge_rom: Box<[u8]>) -> Self {
        Self {
            cartridge_rom,
            rdram: boxed_slice(),
            rsp_dmem: boxed_slice(),
            rsp_imem: boxed_slice(),
        }
    }
}

impl mips_lifter::runtime::Memory for Emulator {
    #[inline]
    fn read_u8(&self, addr: u64) -> u8 {
        match addr {
            _ if RDRAM_MEM.contains(&addr) => self.rdram[subtract_range(RDRAM_MEM, addr)],
            _ if RSP_DMEM.contains(&addr) => self.rsp_dmem[subtract_range(RSP_DMEM, addr)],
            _ if RSP_IMEM.contains(&addr) => self.rsp_imem[subtract_range(RSP_IMEM, addr)],
            _ if RSP_DRAM_IRAM_MIRRORS.contains(&addr) => {
                let normalised = addr % (range_len(RSP_DMEM) + range_len(RSP_IMEM)) as u64;
                self.read_u8(normalised + RSP_DMEM.start)
            }

            _ if RDRAM_REGISTERS.contains(&addr) => {
                println!("stub: RDRAM register read at {addr:#x}");
                0
            }

            _ if RDRAM_REGISTERS_WRITE_ONLY.contains(&addr) => {
                panic!("RDRAM write-only register read at {addr:#x}")
            }

            _ if RDRAM_INTERFACE.contains(&addr) => {
                println!("stub: RI read at {addr:#x}");
                0
            }

            _ if RSP_REGISTERS.contains(&addr) => {
                println!("stub: RSP register read at {addr:#x}");
                0
            }

            _ if RSP_COMMAND_REGISTERS.contains(&addr) => {
                println!("stub: RSP command register read at {addr:#x}");
                0
            }

            _ if RSP_SPAN_REGISTERS.contains(&addr) => {
                println!("stub: RSP span register read at {addr:#x}");
                0
            }

            _ if MIPS_INTERFACE.contains(&addr) => {
                println!("stub: MI read at {addr:#x}");
                0
            }

            _ if VIDEO_INTERFACE.contains(&addr) => {
                println!("stub: VI read at {addr:#x}");
                0
            }

            _ if AUDIO_INTERFACE.contains(&addr) => {
                println!("stub: AI read at {addr:#x}");
                0
            }

            _ if PERIPHERAL_INTERFACE.contains(&addr) => {
                println!("stub: PI read at {addr:#x}");
                0
            }

            _ if SERIAL_INTERFACE.contains(&addr) => {
                println!("stub: SI read at {addr:#x}");
                0
            }

            _ if DISK_DRIVE_REGISTERS.contains(&addr) => {
                println!("stub: DD register read at {addr:#x}");
                0
            }

            _ if DISK_DRIVE_IPL4_ROM.contains(&addr) => {
                println!("stub: DD IPL4 ROM read at {addr:#x}");
                0
            }

            _ if CARTRIDGE_SRAM.contains(&addr) => {
                println!("stub: cartridge SRAM read at {addr:#x}");
                0
            }

            _ if CARTRIDGE_ROM.contains(&addr) => {
                let offset = subtract_range(CARTRIDGE_ROM, addr);
                if offset > self.cartridge_rom.len() {
                    0
                } else {
                    self.cartridge_rom[offset]
                }
            }

            _ if PIF_ROM.contains(&addr) => {
                println!("stub: PIF ROM read at {addr:#x}");
                0
            }

            _ if PIF_RAM.contains(&addr) => {
                println!("stub: PIF RAM read at {addr:#x}");
                0
            }

            _ => panic!("unimplemented read_u8 at {addr:#x}"),
        }
    }

    #[inline]
    fn write_u8(&mut self, addr: u64, value: u8) {
        match addr {
            _ if RDRAM_MEM.contains(&addr) => self.rdram[subtract_range(RDRAM_MEM, addr)] = value,
            _ if RSP_DMEM.contains(&addr) => self.rsp_dmem[subtract_range(RSP_DMEM, addr)] = value,
            _ if RSP_IMEM.contains(&addr) => self.rsp_imem[subtract_range(RSP_IMEM, addr)] = value,
            _ if RSP_DRAM_IRAM_MIRRORS.contains(&addr) => {
                let normalised = addr % (range_len(RSP_DMEM) + range_len(RSP_IMEM)) as u64;
                self.write_u8(normalised + RSP_DMEM.start, value)
            }

            _ if RDRAM_REGISTERS.contains(&addr) => {
                println!("stub: RDRAM register write at {addr:#x} = {value:#x}");
            }

            _ if RDRAM_REGISTERS_WRITE_ONLY.contains(&addr) => {
                println!("stub: RDRAM write-only register write at {addr:#x} = {value:#x}");
            }

            _ if RDRAM_INTERFACE.contains(&addr) => {
                println!("stub: RI write at {addr:#x} = {value:#x}");
            }

            _ if RSP_REGISTERS.contains(&addr) => {
                println!("stub: RSP register write at {addr:#x} = {value:#x}");
            }

            _ if RSP_COMMAND_REGISTERS.contains(&addr) => {
                println!("stub: RSP command register write at {addr:#x} = {value:#x}");
            }

            _ if RSP_SPAN_REGISTERS.contains(&addr) => {
                println!("stub: RSP span register write at {addr:#x} = {value:#x}");
            }

            _ if MIPS_INTERFACE.contains(&addr) => {
                println!("stub: MI write at {addr:#x} = {value:#x}");
            }

            _ if VIDEO_INTERFACE.contains(&addr) => {
                println!("stub: VI write at {addr:#x} = {value:#x}");
            }

            _ if AUDIO_INTERFACE.contains(&addr) => {
                println!("stub: AI write at {addr:#x} = {value:#x}");
            }

            _ if PERIPHERAL_INTERFACE.contains(&addr) => {
                println!("stub: PI write at {addr:#x} = {value:#x}");
            }

            _ if SERIAL_INTERFACE.contains(&addr) => {
                println!("stub: SI write at {addr:#x} = {value:#x}");
            }

            _ if DISK_DRIVE_REGISTERS.contains(&addr) => {
                println!("stub: DD register write at {addr:#x} = {value:#x}");
            }

            _ if DISK_DRIVE_IPL4_ROM.contains(&addr) => {
                panic!("DD IPL4 ROM write at {addr:#x} = {value:#x}");
            }

            _ if CARTRIDGE_SRAM.contains(&addr) => {
                println!("stub: cartridge SRAM write at {addr:#x} = {value:#x}");
            }

            _ if CARTRIDGE_ROM.contains(&addr) => {
                panic!("cartridge ROM write at {addr:#x} = {value:#x}");
            }

            _ if PIF_ROM.contains(&addr) => {
                println!("stub: PIF ROM write at {addr:#x} = {value:#x}");
            }

            _ if PIF_RAM.contains(&addr) => {
                println!("stub: PIF RAM write at {addr:#x} = {value:#x}");
            }

            _ => panic!("unimplemented write_u8 at {addr:#x} = {value:#x}"),
        }
    }
}
