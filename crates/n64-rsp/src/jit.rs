use crate::{dma, register::Registers, Memory, RspError, RspResult};
use mips_lifter::{
    gdb,
    runtime::bus::{
        Bus as BusInterface, BusResult, BusValue, DmaDirection, DmaInfo, Int, PhysicalAddress,
    },
    target::rsp::{
        register::control::{MemoryBank, Status},
        ControlRegisterBank, SpecialRegisterBank,
    },
    JitBuilder, RegisterBank,
};
use std::{
    net::TcpStream,
    ops::Range,
    sync::{
        atomic::{self, AtomicUsize},
        mpsc::{self, Receiver, Sender},
        Arc,
    },
    thread::JoinHandle,
};

pub(crate) type IMemRange = Range<u32>;

#[derive(Debug)]
pub struct Handle {
    _thread: JoinHandle<Bus>,
    cycle_budget: Arc<AtomicUsize>,
    imem_mutation_sender: Sender<IMemRange>,
    dma_request_receiver: Receiver<DmaInfo>,
}

impl Handle {
    pub fn new(memory: Memory, gdb: Option<TcpStream>) -> (Self, Registers) {
        let cycle_budget = Arc::from(AtomicUsize::default());
        let special = SpecialRegisterBank::from(RegisterBank::zeroed_shared());
        let mut control = ControlRegisterBank::from(RegisterBank::zeroed_shared());

        // The RSP is halted by default, until the CPU tells it to start running
        control.write_parsed(Status::default().with_halted(true));

        let (imem_mutation_sender, imem_mutation_receiver) = mpsc::channel();
        let (dma_request_sender, dma_request_receiver) = mpsc::channel();

        let this = Self {
            _thread: {
                let cycle_budget = cycle_budget.clone();
                let special_registers = special.share().expect("special regs are shared");
                let control_registers = control.share().expect("control regs are shared");
                std::thread::spawn(|| {
                    Bus::new(
                        control_registers,
                        special_registers,
                        cycle_budget,
                        imem_mutation_receiver,
                        dma_request_sender,
                        memory,
                        gdb,
                    )
                })
            },
            imem_mutation_sender,
            cycle_budget,
            dma_request_receiver,
        };
        let regs = Registers::new(control, special);
        (this, regs)
    }

    pub fn tick(&self, cycles: usize) {
        self.cycle_budget
            .fetch_add(cycles, atomic::Ordering::Relaxed);
    }

    pub(crate) fn pause(&self) {
        self.cycle_budget.store(0, atomic::Ordering::Relaxed);
    }

    pub fn invalidate_imem(&self, range: Range<usize>) {
        let range = range.start.try_into().expect("address too large")
            ..range.end.try_into().expect("address too large");
        self.imem_mutation_sender
            .send(range)
            .expect("failed to send IMem mutation");
    }

    pub(crate) fn poll_dma_request(&self) -> Option<dma::Direction> {
        self.dma_request_receiver
            .try_recv()
            .ok()
            .map(|info| match info.direction {
                // TODO: use `DmaDirection` everywhere
                DmaDirection::ToRdram => dma::Direction::ToRdram,
                DmaDirection::FromRdram => dma::Direction::ToSpMemory,
            })
    }
}

struct Bus {
    memory: Memory,
    cycle_budget: Arc<AtomicUsize>,
    imem_mutation_receiver: Receiver<IMemRange>,
    dma_request_sender: Sender<DmaInfo>,
}

impl Bus {
    pub fn new(
        control: ControlRegisterBank,
        special: SpecialRegisterBank,
        cycle_budget: Arc<AtomicUsize>,
        imem_mutation_receiver: Receiver<IMemRange>,
        dma_request_sender: Sender<DmaInfo>,
        memory: Memory,
        gdb: Option<TcpStream>,
    ) -> Self {
        while control.read_parsed::<Status>().halted() {
            std::thread::sleep(std::time::Duration::from_millis(100));
        }

        let this = Self {
            memory,
            cycle_budget,
            imem_mutation_receiver,
            dma_request_sender,
        };

        let regs = mips_lifter::target::rsp::Registers {
            control,
            special,
            ..Default::default()
        };

        let gdb = gdb.map(|stream| {
            gdb::Connection::new_rsp(stream, None).expect("failed to create GDB connection")
        });

        JitBuilder::new_rsp(this)
            .with_rsp_registers(regs)
            .maybe_with_gdb(gdb)
            .with_trace(true)
            .run()
    }

    fn read<const SIZE: usize>(&mut self, bank: MemoryBank, address: u32) -> RspResult<[u8; SIZE]> {
        let mem = self.memory.read(bank)?;
        Ok(std::array::from_fn(|i| {
            mem[((address as usize) + i) % MemoryBank::LEN]
        }))
    }

    fn write<const SIZE: usize>(&mut self, address: u32, slice: &[u8; SIZE]) -> RspResult<()> {
        // Note: we only ever write to DMEM, no need to invalidate IMEM
        let mut mem = self.memory.write(MemoryBank::DMem)?;
        for (i, value) in slice.iter().enumerate() {
            mem[((address as usize) + i) % MemoryBank::LEN] = *value;
        }
        Ok(())
    }

    fn check_imem_mutations<T>(&self, v: &mut BusValue<T>) {
        while let Ok(range) = self.imem_mutation_receiver.try_recv() {
            v.mutated(range);
        }
    }
}

impl BusInterface for Bus {
    type Error = RspError;

    fn read_instruction_memory<const SIZE: usize>(
        &mut self,
        address: PhysicalAddress,
    ) -> BusResult<Int<SIZE>, Self::Error> {
        let value = self.read(MemoryBank::IMem, address)?;
        let mut result = Int::from_array(value).into();
        self.check_imem_mutations(&mut result);
        Ok(result)
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
        self.write(address, value.as_slice())?;
        Ok(BusValue::default())
    }

    fn tick(&mut self, cycles: usize) -> BusResult<(), Self::Error> {
        let mut result = BusValue::default();

        // Yield until our budget is big enough (i.e. the CPU has ran enough cycles for us to catch up to it)
        while self.cycle_budget.load(atomic::Ordering::Relaxed) < cycles {
            std::thread::sleep(std::time::Duration::from_millis(1));
        }

        let _ = self.cycle_budget.fetch_update(
            atomic::Ordering::Relaxed,
            atomic::Ordering::Relaxed,
            |budget| Some(budget.saturating_sub(cycles)),
        );

        self.check_imem_mutations(&mut result);
        Ok(result)
    }

    fn ranges_to_invalidate(&mut self) -> Vec<Range<PhysicalAddress>> {
        self.imem_mutation_receiver.try_iter().collect()
    }

    fn request_dma(&mut self, info: DmaInfo) -> BusResult<(), Self::Error> {
        self.dma_request_sender
            .send(info)
            .expect("failed to send DMA request");

        let mut result = BusValue::default();
        if info.direction == DmaDirection::FromRdram {
            // Instruction memory will be mutated, let the JIT know
            let range = info.other_address..(info.other_address + info.length);
            result.mutated(range);
        }

        Ok(result)
    }
}
