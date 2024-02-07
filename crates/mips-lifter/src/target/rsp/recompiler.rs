use super::{register, Rsp};
use crate::codegen::{CodeGen, CompilationError, CompilationResult};
use inkwell::{values::IntValue, IntPredicate};
use mips_decomp::{
    instruction::{Mnenomic, ParsedInstruction},
    register::rsp::control::MemoryBank,
    Exception, INSTRUCTION_SIZE,
};

/// A helper to calculate the target for jump instructions (JAL, J).
/// The 26-bit target is shifted left two bits and combined with the high-order four bits of the address of the delay slot.
fn jump_address(delay_slot_pc: u64, target: u32) -> u64 {
    let masked_delay_slot = delay_slot_pc & 0xFFFF_FFFF_F000_0000;
    let shifted_target = u64::from(target) << 2;
    shifted_target | masked_delay_slot
}

enum JumpTarget<'ctx> {
    Constant(u64),
    Dynamic(IntValue<'ctx>),
}

pub(crate) fn compile_instruction_with_delay_slot(
    codegen: &CodeGen<Rsp>,
    pc: u64,
    instr: &ParsedInstruction,
    delay_slot_instr: &ParsedInstruction,
    on_instruction: impl Fn(u64) -> CompilationResult<()>,
) -> CompilationResult<()> {
    debug_assert!(instr.mnemonic().is_branch() || instr.mnemonic().is_jump());
    let delay_slot_pc = (pc + INSTRUCTION_SIZE as u64) % MemoryBank::LEN as u64;
    let pc = pc % MemoryBank::LEN as u64;

    let compile_delay_slot_instr = || -> CompilationResult<()> {
        // Update runtime metadata about delay slots so we can properly handle traps and exceptions
        codegen.set_inside_delay_slot(true)?;
        on_instruction(delay_slot_pc)?;
        compile_instruction(codegen, delay_slot_instr)?;
        codegen.set_inside_delay_slot(false)?;
        Ok(())
    };

    if instr.mnemonic().is_branch() {
        // Evaluate the branch condition prior to running the delay slot instruction,
        // as the delay slot instruction can influence the branch condition.
        let name = &format!("{}_cmp", instr.mnemonic().name());
        let comparison = {
            let (pred, lhs, rhs) = evaluate_branch(codegen, instr, delay_slot_pc)?;
            codegen.builder.build_int_compare(pred, lhs, rhs, name)?
        };

        // The delay slot instruction gets executed regardless of the branch condition.
        compile_delay_slot_instr()?;

        on_instruction(pc)?;
        codegen.build_if(name, comparison, || {
            let target_pc = instr
                .try_resolve_constant_jump(pc)
                .expect("target address for branch instruction could not be resolved");
            codegen.build_constant_jump(target_pc)?;
            Ok(())
        })?;
        Ok(())
    } else {
        // Evaluate the jump target, and set the link register if needed, prior to executing the delay slot instruction.
        let target = evaluate_jump(codegen, instr, delay_slot_pc)?;

        // Compile the delay slot, writes to the jump target register will be ignored.
        compile_delay_slot_instr()?;

        // Execute the jump.
        on_instruction(pc)?;
        match target {
            JumpTarget::Constant(vaddr) => codegen.build_constant_jump(vaddr),
            JumpTarget::Dynamic(vaddr) => codegen.build_dynamic_jump(vaddr),
        }
    }
}

fn evaluate_jump<'ctx>(
    codegen: &CodeGen<'ctx, Rsp>,
    instr: &ParsedInstruction,
    delay_slot_pc: u64,
) -> CompilationResult<JumpTarget<'ctx>> {
    const MASK: u64 = (MemoryBank::LEN as u64 - 1) & !0b11;
    let i32_type = codegen.context.i32_type();
    let return_address = {
        let value = (delay_slot_pc + INSTRUCTION_SIZE as u64) & MASK;
        i32_type.const_int(value, false)
    };

    Ok(match instr.mnemonic() {
        Mnenomic::J => {
            // Jump to target address
            JumpTarget::Constant(jump_address(delay_slot_pc, instr.immediate()))
        }

        Mnenomic::Jal => {
            // Jump to target address, stores return address in r31 (ra)
            codegen.write_general_register(register::GeneralPurpose::Ra, return_address)?;
            JumpTarget::Constant(jump_address(delay_slot_pc, instr.immediate()))
        }

        Mnenomic::Jr => {
            // Jump to address stored in rs.
            let source = codegen.read_general_register(i32_type, instr.rs())?;
            let jump_target = codegen.build_mask(source, MASK, "jr_mask")?;
            JumpTarget::Dynamic(jump_target)
        }

        Mnenomic::Jalr => {
            // Jump to address stored in rs, stores return address in rd
            // NOTE: Register read/write is swapped compared to the CPU
            let source = codegen.read_general_register(i32_type, instr.rs())?;
            let jump_target = codegen.build_mask(source, MASK, "jalr_mask")?;
            codegen.write_general_register(instr.rd(), return_address)?;
            JumpTarget::Dynamic(jump_target)
        }

        _ => unreachable!("RSP evaluate_jump: {}", instr.mnemonic().name()),
    })
}

fn evaluate_branch<'ctx>(
    codegen: &CodeGen<'ctx, Rsp>,
    instr: &ParsedInstruction,
    delay_slot_pc: u64,
) -> CompilationResult<(IntPredicate, IntValue<'ctx>, IntValue<'ctx>)> {
    assert!(
        !instr.mnemonic().is_likely_branch(),
        "RSP does not support likely branches"
    );

    let i32_type = codegen.context.i32_type();
    Ok(match instr.mnemonic() {
        Mnenomic::Beq => {
            // If rs equals rt, branch to address.
            let source = codegen.read_general_register(i32_type, instr.rs())?;
            let target = codegen.read_general_register(i32_type, instr.rt())?;
            (IntPredicate::EQ, source, target)
        }

        Mnenomic::Bne => {
            // If rs is not equal to rt, branch to address.
            let source = codegen.read_general_register(i32_type, instr.rs())?;
            let target = codegen.read_general_register(i32_type, instr.rt())?;
            (IntPredicate::NE, source, target)
        }

        Mnenomic::Blez => {
            // If rs is less than or equal to zero, branch to address.
            let source = codegen.read_general_register(i32_type, instr.rs())?;
            let zero = i32_type.const_zero();
            (IntPredicate::SLE, source, zero)
        }

        Mnenomic::Bgez => {
            // If rs is greater than or equal to zero, branch to address.
            let source = codegen.read_general_register(i32_type, instr.rs())?;
            (IntPredicate::SGE, source, i32_type.const_zero())
        }

        Mnenomic::Bltz => {
            // If rs is less than zero, branch to address.
            let source = codegen.read_general_register(i32_type, instr.rs())?;
            (IntPredicate::SLT, source, i32_type.const_zero())
        }

        Mnenomic::Bltzal => {
            // If rs is less than zero, branch to address. Unconditionally stores return address to r31 (ra).
            let source = codegen.read_general_register(i32_type, instr.rs())?;
            let return_address = i32_type.const_int(delay_slot_pc + INSTRUCTION_SIZE as u64, false);
            codegen.write_general_register(register::GeneralPurpose::Ra, return_address)?;
            (IntPredicate::SLT, source, i32_type.const_zero())
        }

        Mnenomic::Bgezal => {
            // If rs is greater than or equal to zero, branch to address. Unconditionally stores return address to r31 (ra).
            let source = codegen.read_general_register(i32_type, instr.rs())?;
            let return_address = i32_type.const_int(delay_slot_pc + INSTRUCTION_SIZE as u64, false);
            codegen.write_general_register(register::GeneralPurpose::Ra, return_address)?;
            (IntPredicate::SGE, source, i32_type.const_zero())
        }

        Mnenomic::Bgtz => {
            // If rs is greater than zero, branch to address.
            let source = codegen.read_general_register(i32_type, instr.rs())?;
            (IntPredicate::SGT, source, i32_type.const_zero())
        }

        _ => todo!("RSP evaluate_branch: {}", instr.mnemonic().name()),
    })
}

#[allow(clippy::too_many_lines)]
pub(crate) fn compile_instruction(
    codegen: &CodeGen<Rsp>,
    instr: &ParsedInstruction,
) -> CompilationResult<()> {
    let i8_type = codegen.context.i8_type();
    let i16_type = codegen.context.i16_type();
    let i32_type = codegen.context.i32_type();

    match instr.mnemonic() {
        Mnenomic::Break => {
            codegen.throw_exception(Exception::Breakpoint, None, None)?;
        }

        Mnenomic::Slt => {
            // If signed rs is less than signed rt, store one in rd, otherwise store zero
            let source = codegen.read_general_register(i32_type, instr.rs())?;
            let target = codegen.read_general_register(i32_type, instr.rt())?;
            let cmp = codegen.zero_extend_to(i32_type, cmps!(codegen, source < target)?)?;
            codegen.write_general_register(instr.rd(), cmp)?;
        }

        Mnenomic::Slti => {
            // If signed rs is less than sign-extended 16-bit immediate, store one in rd, otherwise store zero
            let immediate = codegen.sign_extend_to(
                i32_type,
                i16_type.const_int(u64::from(instr.immediate()), true),
            )?;
            let source = codegen.read_general_register(i32_type, instr.rs())?;
            let cmp = codegen.zero_extend_to(i32_type, cmps!(codegen, source < immediate)?)?;
            codegen.write_general_register(instr.rt(), cmp)?;
        }

        Mnenomic::Sltu => {
            // If signed rs is less than unsigned rt, store one in rd, otherwise store zero
            let source = codegen.read_general_register(i32_type, instr.rs())?;
            let target = codegen.read_general_register(i32_type, instr.rt())?;
            let cmp = codegen.zero_extend_to(i32_type, cmpu!(codegen, source < target)?)?;
            codegen.write_general_register(instr.rd(), cmp)?;
        }

        Mnenomic::Sltiu => {
            // If unsigned rs is less than sign-extended 16-bit immediate, store one in rt, otherwise store zero
            let immediate = codegen.sign_extend_to(
                i32_type,
                i16_type.const_int(u64::from(instr.immediate()), true),
            )?;
            let source = codegen.read_general_register(i32_type, instr.rs())?;
            let cmp = codegen.zero_extend_to(i32_type, cmpu!(codegen, source < immediate)?)?;
            codegen.write_general_register(instr.rt(), cmp)?;
        }

        Mnenomic::Lui => {
            // 16-bit immediate is shifted left 16 bits using trailing zeros, result placed in rt
            let result = i32_type.const_int(u64::from(instr.immediate()) << 16, false);
            codegen.write_general_register(instr.rt(), result)?;
        }

        Mnenomic::Sll => {
            // Shift rt left by sa bits, store result in rd
            let shift = i32_type.const_int(u64::from(instr.sa()), false);
            let target = codegen.read_general_register(i32_type, instr.rt())?;
            let result = codegen
                .builder
                .build_left_shift(target, shift, "sll_shift")?;
            codegen.write_general_register(instr.rd(), result)?;
        }

        Mnenomic::Sllv => {
            // shift rt left by the low-order five bits of rs, store result in rd
            let target = codegen.read_general_register(i32_type, instr.rt())?;
            let source = {
                let source = codegen.read_general_register(i32_type, instr.rs())?;
                codegen.build_mask(source, 0b1_1111, "sllv_rs_mask")?
            };

            let result = codegen
                .builder
                .build_left_shift(target, source, "sllv_shift")?;
            codegen.write_general_register(instr.rd(), result)?;
        }

        Mnenomic::Srl => {
            // Shift rt right by sa bits, store result in rd
            let target = codegen.read_general_register(i32_type, instr.rt())?;
            let shift = i32_type.const_int(u64::from(instr.sa()), false);
            let result = codegen
                .builder
                .build_right_shift(target, shift, false, "srl_shift")?;
            codegen.write_general_register(instr.rd(), result)?;
        }

        Mnenomic::Srlv => {
            // Shift rt right by the low-order five bits of rs, store result in rd
            let target = codegen.read_general_register(i32_type, instr.rt())?;
            let source = {
                let source = codegen.read_general_register(i32_type, instr.rs())?;
                codegen.build_mask(source, 0b1_1111, "srlv_rs_mask")?
            };

            let result = codegen
                .builder
                .build_right_shift(target, source, false, "srlv_shift")?;
            codegen.write_general_register(instr.rd(), result)?;
        }

        Mnenomic::Sra => {
            // Shift rt right by sa bits, store result in rd
            let target = codegen.read_general_register(i32_type, instr.rt())?;
            let shift = i32_type.const_int(u64::from(instr.sa()), false);
            let result = codegen
                .builder
                .build_right_shift(target, shift, true, "sra_shift")?;
            codegen.write_general_register(instr.rd(), result)?;
        }

        Mnenomic::Srav => {
            // Shift rt right by the low-order five bits of rs, store result in rd
            let target = codegen.read_general_register(i32_type, instr.rt())?;
            let source = {
                let source = codegen.read_general_register(i32_type, instr.rs())?;
                codegen.build_mask(source, 0b1_1111, "srav_rs_mask")?
            };

            let result = codegen
                .builder
                .build_right_shift(target, source, true, "srav_shift")?;
            codegen.write_general_register(instr.rd(), result)?;
        }

        Mnenomic::Or => {
            // OR rs and rt, store result in rd
            let source = codegen.read_general_register(i32_type, instr.rs())?;
            let target = codegen.read_general_register(i32_type, instr.rt())?;
            let result = codegen.builder.build_or(source, target, "or_res")?;
            codegen.write_general_register(instr.rd(), result)?;
        }

        Mnenomic::Ori => {
            // OR rs and zero-extended 16-bit immediate, store result in rt
            let immediate = i32_type.const_int(u64::from(instr.immediate()), false);
            let source = codegen.read_general_register(i32_type, instr.rs())?;
            let result = codegen.builder.build_or(source, immediate, "ori_res")?;
            codegen.write_general_register(instr.rt(), result)?;
        }

        Mnenomic::Nor => {
            // NOR rs and rt, store result in rd
            let source = codegen.read_general_register(i32_type, instr.rs())?;
            let target = codegen.read_general_register(i32_type, instr.rt())?;
            let or = codegen.builder.build_or(source, target, "nor_or")?;
            let result = codegen.builder.build_not(or, "nor_res")?;
            codegen.write_general_register(instr.rd(), result)?;
        }

        Mnenomic::Xor => {
            // XOR rs with rt, store result in rd
            let source = codegen.read_general_register(i32_type, instr.rs())?;
            let target = codegen.read_general_register(i32_type, instr.rt())?;
            let result = codegen.builder.build_xor(source, target, "xor_res")?;
            codegen.write_general_register(instr.rd(), result)?;
        }

        Mnenomic::Xori => {
            // XOR rs with zero-extended immediate, store result in rd
            let immediate = i32_type.const_int(u64::from(instr.immediate()), false);
            let source = codegen.read_general_register(i32_type, instr.rs())?;
            let result = codegen.builder.build_xor(source, immediate, "xori_res")?;
            codegen.write_general_register(instr.rt(), result)?;
        }

        Mnenomic::And => {
            // AND rs with rt, store result in rd
            let source = codegen.read_general_register(i32_type, instr.rs())?;
            let target = codegen.read_general_register(i32_type, instr.rt())?;
            let result = codegen.builder.build_and(source, target, "and_res")?;
            codegen.write_general_register(instr.rd(), result)?;
        }

        Mnenomic::Andi => {
            // AND rs with zero-extended 16-bit immediate, store result in rt
            let immediate = i32_type.const_int(u64::from(instr.immediate()), false);
            let source = codegen.read_general_register(i32_type, instr.rs())?;
            let result = codegen.builder.build_and(source, immediate, "andi_res")?;
            codegen.write_general_register(instr.rt(), result)?;
        }

        Mnenomic::Add | Mnenomic::Addu => {
            // Add rs and rt, store result in rd
            let source = codegen.read_general_register(i32_type, instr.rs())?;
            let target = codegen.read_general_register(i32_type, instr.rt())?;
            let result = codegen.builder.build_int_add(source, target, "add_res")?;
            codegen.write_general_register(instr.rd(), result)?;
        }

        Mnenomic::Addi | Mnenomic::Addiu => {
            // Add sign-extended 16-bit immediate and rs, store result in rt
            let immediate = codegen.sign_extend_to(
                i32_type,
                i16_type.const_int(u64::from(instr.immediate()), true),
            )?;
            let source = codegen.read_general_register(i32_type, instr.rs())?;
            let result = codegen
                .builder
                .build_int_add(source, immediate, "addi_res")?;
            codegen.write_general_register(instr.rt(), result)?;
        }

        Mnenomic::Sub | Mnenomic::Subu => {
            // Subtract rt from rs, store result in rd
            let source = codegen.read_general_register(i32_type, instr.rs())?;
            let target = codegen.read_general_register(i32_type, instr.rt())?;
            let result = codegen.builder.build_int_sub(source, target, "sub_res")?;
            codegen.write_general_register(instr.rd(), result)?;
        }

        Mnenomic::Lb => {
            // Loads byte stored at memory address (base + offset), stores sign-extended byte in rt
            let address = codegen.base_plus_offset(i32_type, instr, "lb_addr")?;
            let value = codegen.sign_extend_to(i32_type, codegen.read_memory(i8_type, address)?)?;
            codegen.write_general_register(instr.rt(), value)?;
        }

        Mnenomic::Lbu => {
            // Loads byte stored at memory address (base + offset), stores zero-extended byte in rt
            let address = codegen.base_plus_offset(i32_type, instr, "lbu_addr")?;
            let value = codegen.zero_extend_to(i32_type, codegen.read_memory(i8_type, address)?)?;
            codegen.write_general_register(instr.rt(), value)?;
        }

        Mnenomic::Lh => {
            // Loads halfword stored at memory address (base + offset), stores sign-extended halfword in rt
            let address = codegen.base_plus_offset(i32_type, instr, "lh_addr")?;
            let value =
                codegen.sign_extend_to(i32_type, codegen.read_memory(i16_type, address)?)?;
            codegen.write_general_register(instr.rt(), value)?;
        }

        Mnenomic::Lhu => {
            // Loads halfword stored at memory address (base + offset), stores zero-extended halfword in rt
            let address = codegen.base_plus_offset(i32_type, instr, "lhu_addr")?;
            let value =
                codegen.zero_extend_to(i32_type, codegen.read_memory(i16_type, address)?)?;
            codegen.write_general_register(instr.rt(), value)?;
        }

        Mnenomic::Lw | Mnenomic::Lwu => {
            // Loads word stored at memory address (base + offset), stores word in rt
            let address = codegen.base_plus_offset(i32_type, instr, "lw_addr")?;
            let value = codegen.read_memory(i32_type, address)?;
            codegen.write_general_register(instr.rt(), value)?;
        }

        Mnenomic::Sb => {
            // Stores least-significant byte from rt, to memory address (base + offset)
            let target = codegen.read_general_register(i8_type, instr.rt())?;
            let address = codegen.base_plus_offset(i32_type, instr, "sb_addr")?;
            codegen.write_memory(address, target)?;
        }

        Mnenomic::Sh => {
            // Stores halfword from rt, to memory address (base + offset)
            let address = codegen.base_plus_offset(i32_type, instr, "sh_addr")?;
            let target = codegen.read_general_register(i16_type, instr.rt())?;
            codegen.write_memory(address, target)?;
        }

        Mnenomic::Sw => {
            // Stores word from rt, to memory address (base + offset)
            let address = codegen.base_plus_offset(i32_type, instr, "sw_addr")?;
            let target = codegen.read_general_register(i32_type, instr.rt())?;
            codegen.write_memory(address, target)?;
        }

        _ => {
            codegen.build_stub(&format!("RSP instruction: {}", instr.mnemonic().name()))?;
            return Err(CompilationError::UnimplementedInstruction(
                instr.mnemonic().full_name(),
            ));
        }
    }
    Ok(())
}
