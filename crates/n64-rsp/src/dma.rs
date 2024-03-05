use crate::{jit, memory::Memory};

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub(crate) enum Direction {
    ToRdram,
    ToSpMemory,
}

#[derive(Debug)]
pub(crate) struct State {
    pub direction: Direction,
    pub cycles: usize,
}

impl State {
    pub fn tick_is_ready(&mut self, ctx: &TickContext) -> bool {
        self.cycles = self.cycles.saturating_sub(ctx.cycles);
        self.cycles == 0
    }
}

#[derive(Debug)]
pub(crate) struct TickContext<'a> {
    pub cycles: usize,
    pub rdram: &'a mut [u8],
    pub memory: &'a mut Memory,
    pub cpu: &'a jit::Handle,
}
