use crate::{register::Registers, Memory, RspError, RspResult};
use mips_lifter::{
    runtime::bus::{Bus as BusInterface, BusResult, Int, PhysicalAddress},
    target::rsp::{
        register::control::{MemoryBank, Status},
        ControlRegisterBank, SpecialRegisterBank,
    },
    JitBuilder, RegisterBank,
};
use std::{
    sync::{
        atomic::{self, AtomicUsize},
        Arc,
    },
    thread::JoinHandle,
};

#[derive(Debug)]
pub struct Handle {
    _thread: JoinHandle<Bus>,
    cycle_budget: Arc<AtomicUsize>,
}

impl Handle {
    pub fn new(memory: Memory) -> (Self, Registers) {
        let cycle_budget = Arc::from(AtomicUsize::default());
        let special = SpecialRegisterBank::from(RegisterBank::zeroed_shared());
        let mut control = ControlRegisterBank::from(RegisterBank::zeroed_shared());

        // The RSP is halted by default, until the CPU tells it to start running
        control.write_parsed(Status::default().with_halted(true));

        let this = Self {
            _thread: {
                let cycle_budget = cycle_budget.clone();
                let special_registers = special.share().expect("special regs are shared");
                let control_registers = control.share().expect("control regs are shared");
                std::thread::spawn(|| {
                    Bus::new(control_registers, special_registers, cycle_budget, memory)
                })
            },
            cycle_budget,
        };
        let regs = Registers::new(control, special);
        (this, regs)
    }

    pub fn tick(&self, cycles: usize) {
        self.cycle_budget
            .fetch_add(cycles, atomic::Ordering::Relaxed);
    }
}

struct Bus {
    memory: Memory,
    cycle_budget: Arc<AtomicUsize>,
}

impl Bus {
    pub fn new(
        control: ControlRegisterBank,
        special: SpecialRegisterBank,
        cycle_budget: Arc<AtomicUsize>,
        memory: Memory,
    ) -> Self {
        while control.read_parsed::<Status>().halted() {
            std::thread::sleep(std::time::Duration::from_millis(100));
        }

        let this = Self {
            memory,
            cycle_budget,
        };

        let regs = mips_lifter::target::rsp::Registers {
            control,
            special,
            ..Default::default()
        };

        JitBuilder::new_rsp(this)
            .with_rsp_registers(regs)
            .with_trace(true)
            .run()
    }

    fn read<const SIZE: usize>(&mut self, bank: MemoryBank, address: u32) -> RspResult<[u8; SIZE]> {
        let mem = self.memory.read(bank)?;
        Ok(std::array::from_fn(|i| {
            mem[((address as usize) + i) % MemoryBank::LEN]
        }))
    }

    fn write<const SIZE: usize>(
        &mut self,
        bank: MemoryBank,
        address: u32,
        slice: &[u8; SIZE],
    ) -> RspResult<()> {
        let mut mem = self.memory.write(bank)?;
        for (i, value) in slice.iter().enumerate() {
            mem[((address as usize) + i) % MemoryBank::LEN] = *value;
        }
        Ok(())
    }
}

impl BusInterface for Bus {
    type Error = RspError;

    fn read_instruction_memory<const SIZE: usize>(
        &mut self,
        address: PhysicalAddress,
    ) -> BusResult<Int<SIZE>, Self::Error> {
        let value = self.read(MemoryBank::IMem, address)?;
        Ok(Int::from_array(value).into())
    }

    fn read_memory<const SIZE: usize>(
        &mut self,
        address: PhysicalAddress,
    ) -> BusResult<Int<SIZE>, Self::Error> {
        let value = self.read(MemoryBank::DMem, address)?;
        Ok(Int::from_array(value).into())
    }

    fn write_memory<const SIZE: usize>(
        &mut self,
        address: PhysicalAddress,
        value: Int<SIZE>,
    ) -> BusResult<(), Self::Error> {
        self.write(MemoryBank::DMem, address, value.as_slice())?;
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
