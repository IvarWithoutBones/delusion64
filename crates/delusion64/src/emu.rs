use std::ops::Range;

struct ConstRange<const START: u64, const END: u64>;

impl<const START: u64, const END: u64> ConstRange<START, END> {
    const fn new() -> Self {
        Self
    }

    const fn len(&self) -> usize {
        (END - START) as usize
    }

    const fn contains(&self, addr: u64) -> bool {
        addr >= START && addr < END
    }

    const fn index(&self, addr: u64) -> usize {
        (addr - START) as usize
    }
}

const RDRAM_MEM: ConstRange<0x00000000, 0x03F00000> = ConstRange::new();
const RSP_DMEM: ConstRange<0x04000000, 0x04001000> = ConstRange::new();
const RSP_IMEM: ConstRange<0x04001000, 0x04002000> = ConstRange::new();

/// Allocates a boxed slice of a given length on the heap, initialized to 0.
fn boxed_slice<const LEN: usize>() -> Box<[u8; LEN]> {
    vec![0; LEN].into_boxed_slice().try_into().unwrap()
}

pub struct Emulator {
    pub rdram: Box<[u8; RDRAM_MEM.len()]>,
    pub rsp_dmem: Box<[u8; RSP_DMEM.len()]>,
    pub rsp_imem: Box<[u8; RSP_IMEM.len()]>,
}

impl Emulator {
    pub fn new() -> Self {
        Self {
            rdram: boxed_slice(),
            rsp_dmem: boxed_slice(),
            rsp_imem: boxed_slice(),
        }
    }
}

impl mips_lifter::env::Memory for Emulator {
    #[inline]
    fn read_u8(&self, addr: u64) -> u8 {
        // TODO: virtual memory mapping, see: http://en64.shoutwiki.com/wiki/N64_TLB
        {
            /// Extract a range of bits from the given number.
            const fn bit_range(num: u64, range: Range<u64>) -> u64 {
                assert!(range.start <= u64::BITS as _);
                assert!(range.end <= u64::BITS as _);
                let mask = 2_usize.pow((range.end - range.start) as _) - 1;
                (num >> range.start) & mask as u64
            }

            let offset = bit_range(addr, 0..12);
            let vpn = bit_range(addr, 12..29);
            let access_type = bit_range(addr, 29..31);
            let asid = bit_range(addr, 32..39);
            println!("read_u8: addr={addr:#x} offset={offset:#x} vpn={vpn:#x} access_type={access_type:#x} asid={asid:#x}");
        }

        match addr {
            _ if RDRAM_MEM.contains(addr) => self.rdram[RDRAM_MEM.index(addr)],
            _ if RSP_DMEM.contains(addr) => self.rsp_dmem[RSP_DMEM.index(addr)],
            _ if RSP_IMEM.contains(addr) => self.rsp_imem[RSP_IMEM.index(addr)],
            _ => panic!("unimplemented read_u8 for address: {addr:#x}"),
        }
    }

    #[inline]
    fn write_u8(&mut self, addr: u64, value: u8) {
        match addr {
            _ if RDRAM_MEM.contains(addr) => self.rdram[RDRAM_MEM.index(addr)] = value,
            _ if RSP_DMEM.contains(addr) => self.rsp_dmem[RSP_DMEM.index(addr)] = value,
            _ if RSP_IMEM.contains(addr) => self.rsp_imem[RSP_IMEM.index(addr)] = value,
            _ => panic!("unimplemented read_u8 for address: {addr:#x} = {value:#x}"),
        }
    }
}
