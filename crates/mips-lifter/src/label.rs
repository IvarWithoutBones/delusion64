use crate::{
    codegen::{function_attributes, CodeGen, FUNCTION_PREFIX},
    recompiler::{compile_instruction, compile_instruction_with_delay_slot},
    runtime::RuntimeFunction,
    LLVM_CALLING_CONVENTION_FAST,
};
use inkwell::{attributes::AttributeLoc, context::Context, module::Module, values::FunctionValue};
use mips_decomp::{instruction::ParsedInstruction, register, Label, INSTRUCTION_SIZE};
use std::sync::atomic::{AtomicUsize, Ordering};

pub type JitFunctionPointer = usize;

// Ensure we dont define the same function twice, for example when TLB mappings change.
static ID: AtomicUsize = AtomicUsize::new(0);

#[derive(Debug)]
pub struct LabelWithContext<'ctx> {
    /// The label containing the instructions that will be compiled into this function.
    pub label: Label,
    /// The function that will be called to execute the instructions in this label.
    pub function: FunctionValue<'ctx>,
    /// The function that will be called to fall through to the next block.
    pub fallthrough_fn: Option<FunctionValue<'ctx>>,
    /// The instruction that the next block contains, if the current block ends with a delay slot.
    pub fallthrough_instr: Option<ParsedInstruction>,
    /// A raw pointer to the JIT compiled function, from LLVM's execution engine.
    pub pointer: Option<JitFunctionPointer>,
}

impl<'ctx> LabelWithContext<'ctx> {
    fn new(
        label: &Label,
        fallthrough_fn: Option<FunctionValue<'ctx>>,
        fallthrough_instr: Option<ParsedInstruction>,
        module: &Module<'ctx>,
        context: &'ctx Context,
    ) -> LabelWithContext<'ctx> {
        let name = {
            let start = label.start() * INSTRUCTION_SIZE;
            let id = ID.fetch_add(1, Ordering::Relaxed);
            format!("{FUNCTION_PREFIX}{start:06x}_{id}")
        };

        let void_fn_type = context.void_type().fn_type(&[], false);
        let function = module.add_function(&name, void_fn_type, None);
        function.set_call_conventions(LLVM_CALLING_CONVENTION_FAST);
        for attr in function_attributes(context) {
            function.add_attribute(AttributeLoc::Function, attr);
        }

        LabelWithContext {
            label: label.to_owned(),
            function,
            fallthrough_fn,
            fallthrough_instr,
            pointer: None,
        }
    }

    pub fn compile(&self, codegen: &CodeGen<'ctx>) {
        let name = format!("block_{:06x}", self.label.start() * INSTRUCTION_SIZE);
        let basic_block = codegen.context.append_basic_block(self.function, &name);
        codegen.builder.position_at_end(basic_block);

        let len = self.label.instructions.len();
        let mut i = 0;
        while i < len {
            let instr = &self.label.instructions[i];
            if !instr.has_delay_slot() {
                if self.compile_instruction(i, instr, codegen).is_none() {
                    // Stubbed instruction encountered in the middle of the basic block, stop now so our runtime panic can take care of it.
                    // Note that we do not panic here because the runtime environment has the ability to update the debugger connection post panic.
                    break;
                }
                i += 1;
            } else {
                let next_instr = if i == (len - 1) {
                    // If the delay slot is the last instruction in the block, we execute the singular fallthrough instruction
                    // from the next block. The `fallthrough_fn` will skip over the next slot, so we only run it once.
                    self.fallthrough_instr
                        .as_ref()
                        .expect("missing fallthrough instruction")
                } else {
                    // If the current instruction and the delay slot instruction are in the same block, we can swap them.
                    // In some cases we may see two delay slots in a row, so we count the number of delay slots to skip.
                    let mut offset = 1;
                    while let Some(next) = self.label.instructions.get(i + offset) {
                        if next.has_delay_slot() {
                            offset += 1;
                            if offset == 3 {
                                panic!("three delay slots in a row!")
                            }
                        } else {
                            break;
                        }
                    }
                    &self.label.instructions[i + offset]
                };

                let addr = self.index_to_virtual_address(i);
                compile_instruction_with_delay_slot(
                    codegen,
                    addr,
                    instr,
                    next_instr,
                    |pc, poll| self.on_instruction(pc, poll, codegen),
                );
                break;
            }
        }

        if codegen.get_insert_block().get_terminator().is_none() {
            if let Some(fallthrough_fn) = self.fallthrough_fn {
                codegen.call_function(fallthrough_fn);
            } else {
                let str = format!("ERROR: label {:#x} attempted to execute fallthrough block without one existing!\n", self.label.start() * INSTRUCTION_SIZE);
                codegen.build_panic(&str, "error_no_fallthrough");
            }
        }
    }

    fn compile_instruction(
        &self,
        index: usize,
        instr: &ParsedInstruction,
        codegen: &CodeGen<'ctx>,
    ) -> Option<()> {
        self.on_instruction(self.index_to_virtual_address(index), true, codegen);
        compile_instruction(codegen, instr)
    }

    fn on_instruction(&self, addr: u64, poll_interrupts: bool, codegen: &CodeGen<'ctx>) {
        let addr = codegen.context.i64_type().const_int(addr, false);
        let poll_interrupts = codegen
            .context
            .bool_type()
            .const_int(poll_interrupts as u64, false);
        codegen.write_special_register(register::Special::Pc, addr);
        // Call the `on_instruction` callback from the environment, used for the debugger.
        env_call!(codegen, RuntimeFunction::OnInstruction, [poll_interrupts]);
    }

    fn index_to_virtual_address(&self, index: usize) -> u64 {
        // Assumes the label start corresponds to a virtual address.
        debug_assert!((self.label.len() / 4) > index);
        (self.label.start() + (index * INSTRUCTION_SIZE)) as u64
    }
}

pub fn generate_label_functions<'ctx>(
    labels: mips_decomp::LabelList,
    context: &'ctx Context,
    module: &Module<'ctx>,
) -> Vec<LabelWithContext<'ctx>> {
    let mut result: Vec<LabelWithContext<'_>> = Vec::new();

    for label in labels.iter() {
        let existing_label = result
            .iter()
            .position(|existing| existing.label.start() == label.start());

        let (fallthrough_fn, fallthrough_instr) = {
            let mut label = label;
            let mut instr = None;

            if label.instructions.last().unwrap().has_delay_slot() {
                if let Some(next_label) = labels.get(label.start() + INSTRUCTION_SIZE) {
                    // If the last instruction has a delay slot, the instruction to execute prior is split into the next label.
                    // We do want the next label to be a valid jump target, so we cannot simply merge the two.
                    // The solution is to copy the singular instruction into the current label, and skip the label that contains the delay slot.
                    // Note that two delay slots in a row are undefined behaviour, so we don't need to handle that case.
                    label = next_label;
                    instr = Some(label.instructions[0].clone());
                } else {
                    eprintln!(
                        "WARNING: label {:#x} has a delay slot but no next label!",
                        label.start()
                    );
                }
            }

            if let Some(fallthrough) = label.fallthrough_offset.and_then(|o| labels.get(o)) {
                if let Some(existing) = result
                    .iter()
                    .find(|existing| existing.label.start() == fallthrough.start())
                {
                    (Some(existing.function), instr)
                } else {
                    result.push(LabelWithContext::new(
                        fallthrough,
                        None,
                        None,
                        module,
                        context,
                    ));

                    let last = result.last().unwrap();
                    (Some(last.function), instr)
                }
            } else {
                (None, None)
            }
        };

        if let Some(existing_idx) = existing_label {
            let existing = &mut result[existing_idx];
            existing.fallthrough_fn = fallthrough_fn;
            existing.fallthrough_instr = fallthrough_instr;
        } else {
            result.push(LabelWithContext::new(
                label,
                fallthrough_fn,
                fallthrough_instr,
                module,
                context,
            ));
        }
    }

    result
}
