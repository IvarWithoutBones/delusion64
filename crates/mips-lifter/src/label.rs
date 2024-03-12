use crate::{
    codegen::{function_attributes, CodeGen, CompilationResult, FUNCTION_PREFIX},
    macros::{cmp, env_call},
    runtime::RuntimeFunction,
    target::{Instruction, Label, LabelList, Target},
    LLVM_CALLING_CONVENTION_TAILCC,
};
use inkwell::{attributes::AttributeLoc, context::Context, module::Module, values::FunctionValue};
use mips_decomp::INSTRUCTION_SIZE;
use std::sync::atomic::{AtomicUsize, Ordering};

pub type JitFunctionPointer = usize;

// Ensure we dont define the same function twice, for example when TLB mappings change.
static ID: AtomicUsize = AtomicUsize::new(0);

#[derive(Debug)]
pub struct LabelWithContext<'ctx, T: Target> {
    /// The container for the instructions that will be compiled into this function.
    pub label: T::Label,
    /// The native LLVM function that will be called to emulate the instructions.
    pub function: FunctionValue<'ctx>,
    /// A raw pointer to the JIT compiled function, from LLVM's execution engine. Cached to avoid hashing the function name to find it.
    pub pointer: Option<JitFunctionPointer>,
    /// The module this function is defined in. This will be set once code generation is complete.
    pub module: Option<Module<'ctx>>,

    /// The function that will be called to fall through to the next block.
    pub fallthrough_fn: Option<FunctionValue<'ctx>>,
    /// The instruction that the next block contains, if the current block ends with a delay slot.
    pub fallthrough_instr: Option<T::Instruction>,
}

impl<'ctx, T: Target> LabelWithContext<'ctx, T> {
    fn new(
        label: T::Label,
        fallthrough_fn: Option<FunctionValue<'ctx>>,
        fallthrough_instr: Option<T::Instruction>,
        module: &Module<'ctx>,
        context: &'ctx Context,
    ) -> Self {
        let name = {
            let start = label.start() as usize;
            let id = ID.fetch_add(1, Ordering::Relaxed);
            format!("{FUNCTION_PREFIX}{start:06x}_{id}")
        };

        let void_fn_type = context.void_type().fn_type(&[], false);
        let function = module.add_function(&name, void_fn_type, None);
        function.set_call_conventions(LLVM_CALLING_CONVENTION_TAILCC);
        for attr in function_attributes(context) {
            function.add_attribute(AttributeLoc::Function, attr);
        }

        LabelWithContext {
            label,
            function,
            fallthrough_fn,
            fallthrough_instr,
            module: None,
            pointer: None,
        }
    }

    fn on_block_entered(&self, codegen: &CodeGen<'ctx, T>) -> CompilationResult<()> {
        // Write out the PC corresponding to this blocks starting address.
        codegen.write_program_counter(self.label.start())?;

        // Trap into the runtime environment to poll interrupts, etc.
        let i64_type = codegen.context.i64_type();
        let instrs_len = i64_type.const_int(self.label.len() as u64, false);
        let maybe_exception_vec =
            env_call!(codegen, RuntimeFunction::OnBlockEntered, [instrs_len])?
                .try_as_basic_value()
                .left()
                .unwrap()
                .into_int_value();

        // If an exception was raised, jump to the vector returned by the runtime environment. This is indicated by a non-null pointer.
        // TODO: only do this for the CPU, Target-dependant
        let exception_occured = cmp!(codegen, maybe_exception_vec != 0)?;
        codegen.build_if("exception_occured", exception_occured, || {
            codegen.build_jump_to_host_ptr(maybe_exception_vec, "exception_vector_jmp")?;
            Ok(codegen.builder.build_return(None)?)
        })?;
        Ok(())
    }

    pub fn compile(
        &self,
        codegen: &CodeGen<'ctx, T>,
        instr_callback: bool,
    ) -> CompilationResult<()> {
        let name = format!("block_{:06x}", self.index_to_virtual_address(0));
        let basic_block = codegen.context.append_basic_block(self.function, &name);
        codegen.builder.position_at_end(basic_block);
        self.on_block_entered(codegen)?;

        let len = self.label.len();
        for (i, instr) in self.label.instructions().enumerate() {
            if !instr.has_delay_slot() {
                if let Err(err) = self.compile_instruction(i, instr_callback, &instr, codegen) {
                    // We do not propagate the error here because the runtime environment can update the debugger.
                    // Instead, jump to the panic handler when the faulty instruction would otherwise be executed.
                    codegen.build_panic(&err.to_string(), "error_while_compiling_instruction")?;
                    break;
                }
            } else {
                let next_instr = if i == (len - 1) {
                    // If the delay slot is the last instruction in the block, we execute the singular fallthrough instruction
                    // from the next block. The `fallthrough_fn` will skip over the next slot, so we only run it once.
                    self.fallthrough_instr
                        .as_ref()
                        .expect("missing fallthrough instruction")
                        .clone()
                } else {
                    // If the current instruction and the delay slot instruction are in the same block, we can swap them.
                    // In some cases we may see two delay slots in a row, so we count the number of delay slots to skip.
                    let mut offset = 1;
                    while let Some(next) = self.label.instructions().nth(i + offset) {
                        if next.has_delay_slot() {
                            offset += 1;
                        } else {
                            break;
                        }
                    }
                    self.label
                        .instructions()
                        .nth(i + offset)
                        .as_ref()
                        .unwrap()
                        .clone()
                };

                let addr = self.index_to_virtual_address(i);
                if let Err(err) = T::compile_instruction_with_delay_slot(
                    codegen,
                    addr,
                    &instr,
                    &next_instr,
                    |pc| self.on_instruction(pc, instr_callback, codegen),
                ) {
                    // Same as above
                    codegen.build_panic(&err.to_string(), "error_while_compiling_instruction")?;
                }
                break;
            }
        }

        if codegen.get_insert_block().get_terminator().is_none() {
            if let Some(fallthrough_fn) = self.fallthrough_fn {
                codegen.call_function(fallthrough_fn)?;
            } else {
                let addr = self.index_to_virtual_address(0);
                let str = format!("ERROR: label {addr:#x} attempted to execute fallthrough block without one existing!\n");
                codegen.build_panic(&str, "error_no_fallthrough")?;
            }
        }
        Ok(())
    }

    fn compile_instruction(
        &self,
        index: usize,
        instr_callback: bool,
        instr: &T::Instruction,
        codegen: &CodeGen<'ctx, T>,
    ) -> CompilationResult<()> {
        self.on_instruction(
            self.index_to_virtual_address(index),
            instr_callback,
            codegen,
        )?;
        T::compile_instruction(codegen, instr)
    }

    fn on_instruction(
        &self,
        addr: u64,
        instr_callback: bool,
        codegen: &CodeGen<'ctx, T>,
    ) -> CompilationResult<()> {
        // TODO: only write this when we actually need to, for example when checking for exceptions, or the debugger is attached.
        codegen.write_program_counter(addr)?;
        // Call the `on_instruction` callback from the environment, used for the debugger/tracing.
        if instr_callback {
            env_call!(codegen, RuntimeFunction::OnInstruction, [])?;
        }
        Ok(())
    }

    fn index_to_virtual_address(&self, index: usize) -> u64 {
        // Assumes the label start corresponds to a virtual address.
        debug_assert!((self.label.len() / INSTRUCTION_SIZE) > index);
        (self.label.start() as usize + (index * INSTRUCTION_SIZE)) as u64
    }
}

pub fn generate_labels<'ctx, T: Target>(
    labels: T::LabelList,
    context: &'ctx Context,
    module: &Module<'ctx>,
) -> Vec<LabelWithContext<'ctx, T>> {
    let mut result: Vec<LabelWithContext<'_, T>> = Vec::new();

    for label in labels.iter() {
        let existing_label = result
            .iter()
            .position(|existing| existing.label.start() == label.start());

        let (fallthrough_fn, fallthrough_instr) = {
            let mut label = label.clone();
            let mut instr = None;

            if label.instructions().last().unwrap().has_delay_slot() {
                if let Some(next_label) =
                    labels.iter().nth(label.start() as usize + INSTRUCTION_SIZE)
                {
                    // If the last instruction has a delay slot, the instruction to execute prior is split into the next label.
                    // We do want the next label to be a valid jump target, so we cannot simply merge the two.
                    // The solution is to copy the singular instruction into the current label, and skip the label that contains the delay slot.
                    // Note that two delay slots in a row are undefined behaviour, so we don't need to handle that case.
                    label = next_label;
                    instr = Some(label.instructions().next().unwrap().clone());
                } else {
                    eprintln!(
                        "WARNING: label {:#x} has a delay slot but no next label!",
                        label.start()
                    );
                }
            }

            if let Some(fallthrough) = label
                .fallthrough_offset()
                .and_then(|o| labels.get_label(o as u64))
            {
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
