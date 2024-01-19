#![allow(dead_code, clippy::cast_possible_truncation)]

use crate::{MemoryBankHandle, RspError};
use mips_lifter::{
    runtime::bus::{Bus as BusInterface, BusResult, Int, PhysicalAddress},
    target::rsp::register::control::MemoryBank,
    JitBuilder, RegisterBank,
};
use std::{
    sync::{
        atomic::{self, AtomicU32, AtomicU64, AtomicUsize},
        Arc,
    },
    thread::JoinHandle,
};

pub struct Handle {
    thread: JoinHandle<Bus>,
    cycle_budget: Arc<AtomicUsize>,
    cp0_registers: RegisterBank<u32, 8>,
    pc: RegisterBank<u64, 1>,
}

impl Handle {
    #[allow(clippy::similar_names)]
    pub fn new(dmem: MemoryBankHandle, imem: MemoryBankHandle) -> Self {
        let default_regs = Box::new(std::array::from_fn(|_| AtomicU32::default()));
        let cp0_registers = RegisterBank::new_shared(default_regs);
        let registers_for_bus = cp0_registers.share().unwrap();
        cp0_registers.write_relaxed(4, 1).unwrap(); // sp status halted

        let pc = Box::new([AtomicU64::default()]);
        let pc = RegisterBank::new_shared(pc);
        let pc_for_bus = pc.share().unwrap();

        let cycles: Arc<_> = AtomicUsize::default().into();
        let cycles_for_bus = cycles.clone();
        let thread = std::thread::spawn(move || {
            Bus::new(registers_for_bus, pc_for_bus, cycles_for_bus, dmem, imem)
        });

        Self {
            cp0_registers,
            thread,
            cycle_budget: cycles,
            pc,
        }
    }

    pub fn sp_status(&self) -> u32 {
        self.cp0_registers.read_relaxed(4).unwrap()
    }

    pub fn set_sp_status(&self, value: u32) {
        self.cp0_registers.write_relaxed(4, value).unwrap();
    }

    pub fn pc(&self) -> u64 {
        self.pc.read_relaxed(0).unwrap()
    }

    pub fn set_pc(&self, value: u64) {
        let value = (value & !0b11) & 0b1111_1111_1111;
        self.pc.write_relaxed(0, value).unwrap();
    }

    pub fn tick(&self, cycles: usize) {
        self.cycle_budget
            .fetch_add(cycles, atomic::Ordering::Relaxed);
    }
}

struct Bus {
    dmem: MemoryBankHandle,
    imem: MemoryBankHandle,
    cycle_budget: Arc<AtomicUsize>,
}

impl Bus {
    #[allow(clippy::similar_names)]
    pub fn new(
        cp0_regs: RegisterBank<u32, 8>,
        pc: RegisterBank<u64, 1>,
        cycle_budget: Arc<AtomicUsize>,
        dmem: MemoryBankHandle,
        imem: MemoryBankHandle,
    ) -> Self {
        while cycle_budget.load(atomic::Ordering::Relaxed) == 0 {
            // TODO crude way to wait for !SP_STATUS.halted
            std::thread::sleep(std::time::Duration::from_millis(100));
        }

        let res = Self {
            dmem,
            imem,
            cycle_budget,
        };

        let regs = mips_lifter::target::rsp::Registers {
            control: cp0_regs,
            special: pc,
            ..Default::default()
        };

        JitBuilder::new_rsp(res)
            .with_trace(false)
            .with_rsp_registers(regs)
            .run()
    }

    fn read<const SIZE: usize>(&mut self, bank: MemoryBank, address: u32) -> [u8; SIZE] {
        let mem = match bank {
            MemoryBank::IMem => &self.imem,
            MemoryBank::DMem => &self.dmem,
        }
        .read()
        .expect("failed to acquire read lock");

        std::array::from_fn(|i| mem[((address as usize) + i) % MemoryBank::LEN])
    }

    fn write<const SIZE: usize>(&mut self, bank: MemoryBank, address: u32, slice: &[u8; SIZE]) {
        let mut mem = match bank {
            MemoryBank::IMem => &self.imem,
            MemoryBank::DMem => &self.dmem,
        }
        .write()
        .expect("failed to acquire write lock");

        for (i, value) in slice.iter().enumerate() {
            mem[((address as usize) + i) % MemoryBank::LEN] = *value;
        }
    }
}

impl BusInterface for Bus {
    type Error = RspError;

    fn read_instruction_memory<const SIZE: usize>(
        &mut self,
        address: PhysicalAddress,
    ) -> BusResult<Int<SIZE>, Self::Error> {
        Ok(Int::from_array(self.read(MemoryBank::IMem, address)).into())
    }

    fn read_memory<const SIZE: usize>(
        &mut self,
        address: PhysicalAddress,
    ) -> BusResult<Int<SIZE>, Self::Error> {
        Ok(Int::from_array(self.read(MemoryBank::DMem, address)).into())
    }

    fn write_memory<const SIZE: usize>(
        &mut self,
        address: PhysicalAddress,
        value: Int<SIZE>,
    ) -> BusResult<(), Self::Error> {
        self.write(MemoryBank::DMem, address, value.as_slice());
        Ok(().into())
    }

    fn tick(&mut self, cycles: usize) -> BusResult<(), Self::Error> {
        // Yield until our budget is big enough (i.e. the CPU has ran enough cycles for us to catch up to it)
        while self.cycle_budget.load(atomic::Ordering::Relaxed) < cycles {
            std::thread::sleep(std::time::Duration::from_millis(1));
        }

        self.cycle_budget
            .fetch_sub(cycles, atomic::Ordering::Relaxed);
        Ok(().into())
    }
}
