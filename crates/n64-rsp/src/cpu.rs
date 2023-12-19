#![allow(dead_code, clippy::cast_possible_truncation)]

use crate::{MemoryBank, RspError};
use mips_lifter::{
    runtime::bus::{Bus as BusInterface, BusResult, Int, PhysicalAddress},
    RegisterBank,
};
use std::{
    sync::{
        atomic::{self, AtomicU64, AtomicUsize},
        Arc, RwLock,
    },
    thread::JoinHandle,
};

pub struct Handle {
    handle: JoinHandle<Bus>,
    cycle_budget: Arc<AtomicUsize>,
    cp0_registers: RegisterBank<u64, 32>,
}

impl Handle {
    #[allow(clippy::similar_names)]
    pub fn new(
        dmem: Arc<RwLock<Box<[u8; MemoryBank::DMem.len()]>>>,
        imem: Arc<RwLock<Box<[u8; MemoryBank::IMem.len()]>>>,
    ) -> Self {
        let default_regs = Box::new(std::array::from_fn(|_| AtomicU64::default()));
        let (cp0_registers, registers_for_bus) = RegisterBank::new_shared(default_regs);
        cp0_registers.write_relaxed(0, 1).unwrap(); // sp status halted

        let cycles: Arc<_> = AtomicUsize::default().into();
        let cycles_for_bus = cycles.clone();
        let handle =
            std::thread::spawn(move || Bus::new(registers_for_bus, cycles_for_bus, dmem, imem));

        Self {
            cp0_registers,
            handle,
            cycle_budget: cycles,
        }
    }

    pub fn sp_status(&self) -> u32 {
        self.cp0_registers.read_relaxed(0).unwrap() as u32
    }

    pub fn set_sp_status(&self, value: u32) {
        self.cp0_registers
            .write_relaxed(0, u64::from(value))
            .unwrap();
    }

    pub fn tick(&self, cycles: usize) {
        // Just to see if we can communicate with the JIT
        let status = self.cp0_registers.read_relaxed(0).unwrap() | 0b100;
        self.cp0_registers.write_relaxed(0, status).unwrap();

        self.cycle_budget
            .fetch_add(cycles, atomic::Ordering::Relaxed);
    }
}

struct Bus {
    // TODO: add registers
    dmem: Arc<RwLock<Box<[u8; MemoryBank::DMem.len()]>>>,
    imem: Arc<RwLock<Box<[u8; MemoryBank::IMem.len()]>>>,
    cycle_budget: Arc<AtomicUsize>,
    cycles_ran: usize,
}

impl Bus {
    #[allow(clippy::similar_names)]
    pub fn new(
        cp0_regs: RegisterBank<u64, 32>,
        cycle_budget: Arc<AtomicUsize>,
        dmem: Arc<RwLock<Box<[u8; MemoryBank::DMem.len()]>>>,
        imem: Arc<RwLock<Box<[u8; MemoryBank::IMem.len()]>>>,
    ) -> Self {
        while cycle_budget.load(atomic::Ordering::Relaxed) == 0 {
            // TODO crude way to wait for !SP_STATUS.halted
            std::thread::sleep(std::time::Duration::from_millis(100));
        }

        let _res = Self {
            dmem,
            imem,
            cycle_budget,
            cycles_ran: 0,
        };

        let _regs = mips_lifter::Registers {
            cp0: cp0_regs,
            ..Default::default()
        };
        // JitBuilder::new(Target::Rsp, res)
        //     .with_trace(true)
        //     .with_registers(regs)
        //     .run()
        todo!("rsp jit")
    }
}

impl BusInterface for Bus {
    type Error = RspError;

    fn read_memory<const SIZE: usize>(
        &mut self,
        address: PhysicalAddress,
    ) -> BusResult<Int<SIZE>, Self::Error> {
        // TODO: should not assume banks
        println!("read_memory: {address:x}");
        let imem = self.imem.read().unwrap();
        let slice = &imem[address as usize..][..SIZE];
        Ok(Int::from_slice(slice).unwrap().into())
    }

    fn write_memory<const SIZE: usize>(
        &mut self,
        address: PhysicalAddress,
        value: Int<SIZE>,
    ) -> BusResult<(), Self::Error> {
        // TODO: should not assume banks
        println!("write_memory: {address:x} = {value:#x?}");
        let mut dmem = self.dmem.write().unwrap();
        let slice = &mut dmem[address as usize..][..SIZE];
        slice.copy_from_slice(value.as_slice());
        Ok(().into())
    }

    fn tick(&mut self, cycles: usize) -> BusResult<(), Self::Error> {
        println!("tick: {cycles}");
        self.cycles_ran += cycles;

        let budget = self.cycle_budget.load(atomic::Ordering::Relaxed);
        if budget < self.cycles_ran {
            // Yield until our budget is big enough
            println!("waiting till budget is big enough");
            while self.cycle_budget.load(atomic::Ordering::Relaxed) < self.cycles_ran {
                std::thread::yield_now();
            }
        }

        println!("continuing");
        // Continue running until we've used up our budget
        self.cycles_ran -= budget;
        self.cycle_budget
            .fetch_sub(budget, atomic::Ordering::Relaxed);
        Ok(().into())
    }
}
