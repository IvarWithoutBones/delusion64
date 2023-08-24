use crate::{
    codegen::{function_attributes, CodeGen},
    recompiler::recompile_instruction,
    runtime::RuntimeFunction,
    LLVM_CALLING_CONVENTION_FAST,
};
use inkwell::{
    attributes::AttributeLoc, basic_block::BasicBlock, context::Context, module::Module,
    values::FunctionValue,
};
use mips_decomp::{instruction::ParsedInstruction, register, Label, INSTRUCTION_SIZE};

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct LabelWithContext<'ctx> {
    /// The label containing the instructions that will be compiled into this function.
    pub label: Label,
    /// The function that will be called to execute the instructions in this label.
    pub function: FunctionValue<'ctx>,
    /// The function that will be called to fall through to the next block.
    pub fallthrough_fn: Option<FunctionValue<'ctx>>,
    /// The instruction that the next block contains, if the current block ends with a delay slot.
    pub fallthrough_instr: Option<ParsedInstruction>,
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
            format!("function_{start:06x}")
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
        }
    }

    pub fn compile(&self, codegen: &CodeGen<'ctx>) {
        let name = format!("block_{:06x}", self.label.start() * INSTRUCTION_SIZE);
        let basic_block = codegen.context.append_basic_block(self.function, &name);
        codegen.builder.position_at_end(basic_block);

        // Ensure we never overflow the stack with return addresses of recursive calls.
        // In theory we should be able to avoid storing the return address in the first place, but this works for now.
        unsafe { codegen.restore_host_stack() };

        let len = self.label.instructions.len();
        let second_last = len.saturating_sub(2);
        let mut i = 0;
        while i < len {
            let instr = &self.label.instructions[i];

            // A delay slot effectively swaps the order of two instructions.
            if instr.has_delay_slot() {
                let next_instr = if i == (len - 1) {
                    // If the delay slot is the last instruction in the block, we execute the singular fallthrough instruction
                    // from the next block. The `fallthrough_fn` will skip over the next slot, so we only run it once.
                    if let Some(next) = self.fallthrough_instr.as_ref() {
                        next
                    } else {
                        // Because of undefined behavior, we can't really do anything here.
                        break;
                    }
                } else if i == second_last {
                    // if the two instructions are inside of the same block, we can just swap them
                    &self.label.instructions[i + 1]
                } else {
                    panic!("unable to resolve delay slot for {instr:#?}");
                };

                // `recompile_instruction` positions the builder at the block ran when the branch was not taken, so that we can generate a fallthrough.
                // This is problematic for an Likely Branch instruction, as should *only* run the delay slot instruction if the branch is taken.
                // In this case we cannot simply swap the instructions, but instead inject the delay slot instruction prior to the jump to the branch target.
                if !instr.mnemonic().discards_delay_slot() {
                    self.compile_instruction(i + 1, false, next_instr, codegen);
                    self.compile_instruction(i, true, instr, codegen);
                } else {
                    // The block taken when the branch was successful.
                    let then_block = self.compile_instruction(i, false, instr, codegen).unwrap();

                    // Two instructions that end a block in a row are undefined behavior, just ignore it.
                    if !next_instr.ends_block() {
                        let current_block = codegen.builder.get_insert_block().unwrap();
                        codegen
                            .builder
                            .position_before(&then_block.get_first_instruction().unwrap());
                        self.compile_instruction(i + 1, false, next_instr, codegen);
                        codegen.builder.position_at_end(current_block);
                    }
                }

                break;
            } else {
                self.compile_instruction(i, false, instr, codegen);
                i += 1;
            }
        }

        if codegen
            .builder
            .get_insert_block()
            .unwrap()
            .get_terminator()
            .is_none()
        {
            if let Some(fallthrough_fn) = self.fallthrough_fn {
                codegen.call_function(fallthrough_fn);
            } else {
                let str = format!("ERROR: label {:#x} attempted to execute fallthrough block without one existing!\n", self.label.start() * 4);
                codegen.print_string(&str, "error_no_fallthrough");
                env_call!(codegen, RuntimeFunction::Panic, []);
                codegen.builder.build_unreachable();
            }
        }
    }

    fn compile_instruction(
        &self,
        index: usize,
        executed_delay_slot: bool,
        instr: &ParsedInstruction,
        codegen: &CodeGen<'ctx>,
    ) -> Option<BasicBlock<'ctx>> {
        // Set the program counter to the current instruction, assumes the labels start corresponds to a virtual address.
        let addr = ((self.label.start() + index) * INSTRUCTION_SIZE) as u64;
        let addr_const = codegen.context.i64_type().const_int(addr, false);
        codegen.write_special_register(register::Special::Pc, addr_const);

        // Call the `on_instruction` callback from the environment, used for the debugger.
        env_call!(codegen, RuntimeFunction::OnInstruction, []);

        // Finally recompile the disassembled instruction.
        recompile_instruction(codegen, instr, addr, executed_delay_slot)
    }
}

pub fn generate_label_functions<'ctx>(
    labels: mips_decomp::LabelList,
    context: &'ctx Context,
    module: &Module<'ctx>,
) -> Vec<LabelWithContext<'ctx>> {
    let mut result: Vec<LabelWithContext<'_>> = Vec::new();

    for (i, label) in labels.iter().enumerate() {
        println!("generating function for label {}/{}", i + 1, labels.len());

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
                    panic!("delay slot into non-existent label");
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
