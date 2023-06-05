use inkwell::{basic_block::BasicBlock, context::Context, module::Module};
use mips_decomp::{BlockList, MaybeInstruction, INSTRUCTION_SIZE};
use std::{collections::HashMap, ops::Range};

pub type Labels<'ctx> = HashMap<u64, Label<'ctx>>;

#[derive(Debug, Clone)]
pub struct Label<'ctx> {
    pub start_address: u64,
    pub instructions: Vec<MaybeInstruction>,
    pub basic_block: BasicBlock<'ctx>,
    pub fall_through: Option<BasicBlock<'ctx>>,
    pub id: u64,
}

impl Label<'_> {
    pub fn end_address(&self) -> u64 {
        self.start_address + (self.instructions.len() as u64 * INSTRUCTION_SIZE as u64)
    }

    pub fn range(&self) -> Range<u64> {
        self.start_address..self.end_address()
    }

    pub fn name(address: u64) -> String {
        format!("label_{:06x}", address)
    }
}

pub struct LabelPass<'ctx> {
    context: &'ctx Context,
    module: Module<'ctx>,
    labels: Labels<'ctx>,
    raw_blocks: &'ctx BlockList,
}

impl<'ctx> LabelPass<'ctx> {
    pub fn new(context: &'ctx Context, module: Module<'ctx>, raw_blocks: &'ctx BlockList) -> Self {
        Self {
            context,
            module,
            raw_blocks,
            labels: HashMap::new(),
        }
    }

    /// Generate labels for every block.
    pub fn run(&mut self) {
        let current_func = self.module.get_last_function().unwrap();

        let mut id = 0;
        let mut pos = 0;
        for raw_block in self.raw_blocks.iter() {
            self.labels.entry(pos).or_insert_with(|| {
                let name = Label::name(pos);
                let basic_block = self.context.append_basic_block(current_func, &name);
                id += 1;
                Label {
                    id: id as _,
                    start_address: pos,
                    instructions: raw_block.clone(),
                    fall_through: None,
                    basic_block,
                }
            });

            for instr in raw_block {
                if instr.ends_block() {
                    // Generate labels for static jumps.
                    if let Some(target) = instr.try_resolve_static_jump(pos as _) {
                        self.labels.entry(target as _).or_insert_with(|| {
                            let name = Label::name(target as _);
                            let basic_block = self.context.append_basic_block(current_func, &name);

                            let mut found = false;
                            let instructions = self
                                .raw_blocks
                                .iter()
                                .flatten()
                                .skip(target as usize / INSTRUCTION_SIZE)
                                .take_while(|instr| {
                                    // Take the very last instruction as well.
                                    if instr.ends_block() {
                                        found = true;
                                        true
                                    } else {
                                        !found
                                    }
                                })
                                .cloned()
                                .collect();

                            id += 1;
                            Label {
                                start_address: target as _,
                                fall_through: None,
                                instructions,
                                basic_block,
                                id: id as _,
                            }
                        });

                        // Shrink the previous block containing the jump instruction, if it exists.
                        if let Some(prev_block) = self.labels.values_mut().find(|l| {
                            target
                                .checked_sub(INSTRUCTION_SIZE as _)
                                .map_or(false, |a| l.range().contains(&(a as _)))
                        }) {
                            prev_block.instructions.truncate(
                                (target as usize - prev_block.start_address as usize)
                                    / INSTRUCTION_SIZE,
                            );
                        }
                    }

                    // Generate labels for instructions that discard delay slots.
                    if instr.discards_delay_slot() {
                        let start_address = pos + (INSTRUCTION_SIZE * 2) as u64;
                        self.labels.entry(start_address).or_insert_with(|| {
                            let name = Label::name(start_address);
                            let basic_block = self.context.append_basic_block(current_func, &name);

                            let instructions = self
                                .raw_blocks
                                .iter()
                                .flatten()
                                .skip(start_address as usize / INSTRUCTION_SIZE)
                                .take_while(|instr| !instr.ends_block())
                                .cloned()
                                .collect();

                            id += 1;
                            Label {
                                id: id as _,
                                start_address,
                                instructions,
                                fall_through: None,
                                basic_block,
                            }
                        });

                        // Shrink the previous block if it exists.
                        if let Some(prev_block) = self.labels.values_mut().find(|l| {
                            start_address
                                .checked_sub(INSTRUCTION_SIZE as _)
                                .map_or(false, |a| l.range().contains(&(a as _)))
                        }) {
                            prev_block.instructions.truncate(
                                (start_address as usize - prev_block.start_address as usize)
                                    / INSTRUCTION_SIZE,
                            );
                        }
                    }
                }

                pos += INSTRUCTION_SIZE as u64;
            }
        }

        // Generate fall-through attributes for every label.
        let search_labels = self.labels.clone();
        for curr in self.labels.values_mut() {
            let fall_through = search_labels
                .values()
                .find(|next| next.id != curr.id && next.start_address == curr.end_address())
                .map(|l| l.basic_block);

            curr.fall_through = fall_through;
        }
    }

    /// Dump the labels and the generated LLVM IR to stderr.
    #[allow(dead_code)]
    pub fn dump(&self) {
        println!("\nlabels:");
        for v in self.labels.values() {
            println!(
                "{:06x}..={:06x}:",
                v.start_address,
                v.end_address() - INSTRUCTION_SIZE as u64
            );

            for instr in &v.instructions {
                println!("  {instr}");
            }

            if let Some(fall_through) = v.fall_through {
                println!(
                    "fall through: {}",
                    fall_through.get_name().to_str().unwrap()
                );
            }
        }

        println!("\nGenerated LLVM IR:\n```");
        self.module.print_to_stderr();
        println!("```\n");
    }

    pub fn consume(self) -> (&'ctx Context, Module<'ctx>, Labels<'ctx>) {
        (self.context, self.module, self.labels)
    }
}
