use crate::{
    codegen::CodeGen,
    target::{Cpu, Target},
};
use inkwell::{values::IntValue, IntPredicate};
use mips_decomp::{
    instruction::{Mnenomic, ParsedInstruction},
    register, Exception, INSTRUCTION_SIZE,
};

pub mod cpu;
pub mod rsp;

fn stub<T: Target>(codegen: &CodeGen<T>, name: &str) {
    let output = format!("{name} instruction not implemented");
    let storage_name = format!("{name}_stub");
    codegen.build_panic(&output, &storage_name);
}

/// A helper to calculate the target for jump instructions (JAL, J).
/// The 26-bit target is shifted left two bits and combined with the high-order four bits of the address of the delay slot.
fn jump_address(delay_slot_pc: u64, target: u32) -> u64 {
    let masked_delay_slot = delay_slot_pc & 0xFFFF_FFFF_F000_0000;
    let shifted_target = (target as u64) << 2;
    shifted_target | masked_delay_slot
}

enum JumpTarget<'ctx> {
    Constant(u64),
    Dynamic(IntValue<'ctx>),
}

pub fn compile_instruction_with_delay_slot(
    codegen: &CodeGen<Cpu>,
    pc: u64,
    instr: &ParsedInstruction,
    delay_slot_instr: &ParsedInstruction,
    on_instruction: impl Fn(u64),
) {
    debug_assert!(instr.mnemonic().has_delay_slot());
    let delay_slot_pc = pc + INSTRUCTION_SIZE as u64;

    let compile_delay_slot_instr = || {
        // Update runtime metadata about delay slots so we can properly handle traps and exceptions
        codegen.set_inside_delay_slot(true);
        on_instruction(delay_slot_pc);
        cpu::compile_instruction(codegen, delay_slot_instr);
        codegen.set_inside_delay_slot(false);
    };

    if instr.mnemonic().is_branch() || instr.mnemonic().is_trap() {
        // Evaluate the branch condition prior to running the delay slot instruction,
        // as the delay slot instruction can influence the branch condition.
        let name = &format!("{}_cmp", instr.mnemonic().name());
        let comparison = {
            let (pred, lhs, rhs) = if instr.mnemonic().is_branch() {
                evaluate_branch(codegen, instr, delay_slot_pc)
            } else {
                debug_assert!(instr.mnemonic().is_trap());
                cpu::evaluate_trap(codegen, instr)
            };
            codegen.builder.build_int_compare(pred, lhs, rhs, name)
        };

        if !instr.mnemonic().is_likely_branch() {
            // If the delay slot instruction is not discarded, it gets executed regardless of the branch condition.
            compile_delay_slot_instr();
        }

        on_instruction(pc);
        codegen.build_if(name, comparison, || {
            if instr.mnemonic().is_likely_branch() {
                // If the delay slot is discarded, the delay slot instruction only gets executed when the branch is taken.
                compile_delay_slot_instr();
            }

            if instr.mnemonic().is_branch() {
                let target_pc = instr
                    .try_resolve_constant_jump(pc)
                    .expect("target address for branch instruction could not be resolved");
                codegen.build_constant_jump(target_pc)
            } else {
                // This must be a trap instruction, checked above.
                codegen.throw_exception(Exception::Trap, Some(0), None);
            }
        });
    } else {
        // Evaluate the jump target, and set the link register if needed, prior to executing the delay slot instruction.
        let target = evaluate_jump(codegen, instr, delay_slot_pc);

        // Compile the delay slot, writes to the jump target register will be ignored.
        compile_delay_slot_instr();

        // Execute the jump.
        on_instruction(pc);
        match target {
            JumpTarget::Constant(vaddr) => codegen.build_constant_jump(vaddr),
            JumpTarget::Dynamic(vaddr) => codegen.build_dynamic_jump(vaddr),
        }
    }
}

fn evaluate_jump<'ctx>(
    codegen: &CodeGen<'ctx, Cpu>,
    instr: &ParsedInstruction,
    delay_slot_pc: u64,
) -> JumpTarget<'ctx> {
    let i64_type = codegen.context.i64_type();
    let return_address = i64_type.const_int(delay_slot_pc + INSTRUCTION_SIZE as u64, false);
    match instr.mnemonic() {
        Mnenomic::J => {
            // Jump to target address
            JumpTarget::Constant(jump_address(delay_slot_pc, instr.immediate()))
        }

        Mnenomic::Jal => {
            // Jump to target address, stores return address in r31 (ra)
            codegen.write_register(register::GeneralPurpose::Ra, return_address);
            JumpTarget::Constant(jump_address(delay_slot_pc, instr.immediate()))
        }

        Mnenomic::Jr => {
            // Jump to address stored in rs. If the address is not aligned to a 4-byte boundary, an address error exception occurs.
            let source = codegen.read_general_register(i64_type, instr.rs());
            codegen.build_if(
                "jr_addr_misaligned",
                {
                    let low_bits = codegen.build_mask(source, 0b11, "jr_addr_low_two_bits");
                    cmp!(codegen, low_bits != 0)
                },
                || {
                    // The exception occurs during the instruction fetch stage after finishing the jump instruction.
                    // We set PC to the address of the instruction that caused the exception, so EPC is set accordingly.
                    codegen.write_register(register::Special::Pc, source);
                    codegen.throw_exception(Exception::AddressLoad, None, Some(source))
                },
            );
            JumpTarget::Dynamic(source)
        }

        Mnenomic::Jalr => {
            // Jump to address stored in rs, stores return address in rd
            codegen.write_general_register(instr.rd(), return_address);
            JumpTarget::Dynamic(
                codegen.read_general_register(codegen.context.i64_type(), instr.rs()),
            )
        }

        _ => unreachable!("evaluate_jump: {}", instr.mnemonic().name()),
    }
}

fn evaluate_branch<'ctx>(
    codegen: &CodeGen<'ctx, Cpu>,
    instr: &ParsedInstruction,
    delay_slot_pc: u64,
) -> (IntPredicate, IntValue<'ctx>, IntValue<'ctx>) {
    // NOTE: For the VR43000, the difference between regular and likely branches is taken care of by the callee.
    let i64_type = codegen.context.i64_type();
    match instr.mnemonic() {
        Mnenomic::Beq | Mnenomic::Beql => {
            // If rs equals rt, branch to address.
            let source = codegen.read_general_register(i64_type, instr.rs());
            let target = codegen.read_general_register(i64_type, instr.rt());
            (IntPredicate::EQ, source, target)
        }

        Mnenomic::Bne | Mnenomic::Bnel => {
            // If rs is not equal to rt, branch to address.
            let source = codegen.read_general_register(i64_type, instr.rs());
            let target = codegen.read_general_register(i64_type, instr.rt());
            (IntPredicate::NE, source, target)
        }

        Mnenomic::Blez | Mnenomic::Blezl => {
            // If rs is less than or equal to zero, branch to address.
            let source = codegen.read_general_register(i64_type, instr.rs());
            let zero = i64_type.const_zero();
            (IntPredicate::SLE, source, zero)
        }

        Mnenomic::Bgez | Mnenomic::Bgezl => {
            // If rs is greater than or equal to zero, branch to address.
            let source = codegen.read_general_register(i64_type, instr.rs());
            (IntPredicate::SGE, source, i64_type.const_zero())
        }

        Mnenomic::Bgezal | Mnenomic::Bgezall => {
            // If rs is greater than or equal to zero, branch to address. Unconditionally stores return address to r31 (ra).
            let return_address = i64_type.const_int(delay_slot_pc + INSTRUCTION_SIZE as u64, false);
            codegen.write_register(register::GeneralPurpose::Ra, return_address);
            let source = codegen.read_general_register(i64_type, instr.rs());
            (IntPredicate::SGE, source, i64_type.const_zero())
        }

        Mnenomic::Bltz | Mnenomic::Bltzl => {
            // If rs is less than zero, branch to address.
            let source = codegen.read_general_register(i64_type, instr.rs());
            (IntPredicate::SLT, source, i64_type.const_zero())
        }

        Mnenomic::Bltzal | Mnenomic::Bltzall => {
            // If rs is less than zero, branch to address. Unconditionally stores return address to r31 (ra).
            let return_address = i64_type.const_int(delay_slot_pc + INSTRUCTION_SIZE as u64, false);
            codegen.write_register(register::GeneralPurpose::Ra, return_address);
            let source = codegen.read_general_register(i64_type, instr.rs());
            (IntPredicate::SLT, source, i64_type.const_zero())
        }

        Mnenomic::Bgtz | Mnenomic::Bgtzl => {
            // If rs is greater than zero, branch to address.
            let source = codegen.read_general_register(i64_type, instr.rs());
            (IntPredicate::SGT, source, i64_type.const_zero())
        }

        Mnenomic::Bc1t | Mnenomic::Bc1tl => {
            // If the last floating-point compare is true, branch to address.
            let source = codegen.read_register(i64_type, register::Fpu::F31);
            let mask = i64_type.const_int(1 << register::fpu::ControlStatus::CONDITION_BIT, false);
            let masked = codegen.builder.build_and(source, mask, "bc1t_mask");
            (IntPredicate::NE, masked, i64_type.const_zero())
        }

        Mnenomic::Bc1f | Mnenomic::Bc1fl => {
            // If the last floating-point compare is false, branch to address.
            let source = codegen.read_register(i64_type, register::Fpu::F31);
            let mask = i64_type.const_int(1 << register::fpu::ControlStatus::CONDITION_BIT, false);
            let masked = codegen.builder.build_and(source, mask, "bc1t_mask");
            (IntPredicate::EQ, masked, i64_type.const_zero())
        }

        _ => todo!("evaluate_branch: {}", instr.mnemonic().name()),
    }
}

// TODO: add common instructions here
