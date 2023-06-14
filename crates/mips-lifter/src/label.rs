use crate::{
    codegen::CodeGen, recompiler::recompile_instruction, runtime::RuntimeFunction,
    LLVM_CALLING_CONVENTION_FAST,
};
use inkwell::{context::Context, module::Module, values::FunctionValue};
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
        let name = format!("function_{:06x}", label.start() * INSTRUCTION_SIZE);
        let void_fn_type = context.void_type().fn_type(&[], false);
        let function = module.add_function(&name, void_fn_type, None);
        function.set_call_conventions(LLVM_CALLING_CONVENTION_FAST);

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

                self.compile_instruction(i + 1, next_instr, codegen);
                if !next_instr.ends_block() {
                    // Any other case would be undefined behavior, so we can't really do anything here.
                    self.compile_instruction(i, instr, codegen);
                }

                break;
            } else {
                self.compile_instruction(i, instr, codegen);
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
                codegen.call_label(fallthrough_fn);
            } else {
                codegen.builder.build_return(None);
            }
        }
    }

    fn compile_instruction(
        &self,
        index: usize,
        instr: &ParsedInstruction,
        codegen: &CodeGen<'ctx>,
    ) {
        let addr = ((self.label.start() + index) * INSTRUCTION_SIZE) as u64;

        // Set the program counter to the current instruction.
        // TODO: this is inaccurate, we should set PC based on the TLB.
        let pc = codegen
            .build_i64(addr + 0x0000_0000_A400_0040)
            .into_int_value();
        codegen.write_special_reg(register::Special::Pc, pc.into());

        // Call the `on_instruction` callback from the environment, used for the debugger.
        env_call!(codegen, RuntimeFunction::OnInstruction, []);

        // Finally recompile the disassembled instruction.
        recompile_instruction(codegen, instr, addr);
    }
}

pub fn generate_label_functions<'ctx>(
    labels: mips_decomp::LabelList,
    context: &'ctx Context,
    module: &Module<'ctx>,
) -> Vec<LabelWithContext<'ctx>> {
    let mut result: Vec<LabelWithContext<'_>> = Vec::new();

    for (i, label) in labels.iter().enumerate() {
        println!(
            "generating function for label {}/{}",
            i + 1,
            labels.len() + 1
        );

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
                    println!("warning: delay slot into invalid label");
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
