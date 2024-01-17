use super::Rsp;
use crate::codegen::CodeGen;
use mips_decomp::{
    instruction::{Mnenomic, ParsedInstruction},
    Exception,
};

pub(crate) fn compile_instruction_with_delay_slot(
    _codegen: &CodeGen<Rsp>,
    _pc: u64,
    _instr: &ParsedInstruction,
    _delay_slot_instr: &ParsedInstruction,
    _on_instruction: impl Fn(u64),
) {
    todo!("RSP compile_instruction_with_delay_slot")
}

#[allow(clippy::too_many_lines)]
pub(crate) fn compile_instruction(codegen: &CodeGen<Rsp>, instr: &ParsedInstruction) -> Option<()> {
    let i8_type = codegen.context.i8_type();
    let i16_type = codegen.context.i16_type();
    let i32_type = codegen.context.i32_type();

    match instr.mnemonic() {
        Mnenomic::Break => {
            codegen.throw_exception(Exception::Breakpoint, None, None);
        }

        Mnenomic::Slt => {
            // If signed rs is less than signed rt, store one in rd, otherwise store zero
            let source = codegen.read_general_register(i32_type, instr.rs());
            let target = codegen.read_general_register(i32_type, instr.rt());
            let cmp = codegen.zero_extend_to(i32_type, cmps!(codegen, source < target));
            codegen.write_general_register(instr.rd(), cmp);
        }

        Mnenomic::Slti => {
            // If signed rs is less than sign-extended 16-bit immediate, store one in rd, otherwise store zero
            let immediate = codegen.sign_extend_to(
                i32_type,
                i16_type.const_int(u64::from(instr.immediate()), true),
            );
            let source = codegen.read_general_register(i32_type, instr.rs());
            let cmp = codegen.zero_extend_to(i32_type, cmps!(codegen, source < immediate));
            codegen.write_general_register(instr.rt(), cmp);
        }

        Mnenomic::Sltu => {
            // If signed rs is less than unsigned rt, store one in rd, otherwise store zero
            let source = codegen.read_general_register(i32_type, instr.rs());
            let target = codegen.read_general_register(i32_type, instr.rt());
            let cmp = codegen.zero_extend_to(i32_type, cmpu!(codegen, source < target));
            codegen.write_general_register(instr.rd(), cmp);
        }

        Mnenomic::Sltiu => {
            // If unsigned rs is less than sign-extended 16-bit immediate, store one in rt, otherwise store zero
            let immediate = codegen.sign_extend_to(
                i32_type,
                i16_type.const_int(u64::from(instr.immediate()), true),
            );
            let source = codegen.read_general_register(i32_type, instr.rs());
            let cmp = codegen.zero_extend_to(i32_type, cmpu!(codegen, source < immediate));
            codegen.write_general_register(instr.rt(), cmp);
        }

        Mnenomic::Lui => {
            // 16-bit immediate is shifted left 16 bits using trailing zeros, result placed in rt
            let result = i32_type.const_int(u64::from(instr.immediate()) << 16, false);
            codegen.write_general_register(instr.rt(), result);
        }

        Mnenomic::Sll => {
            // Shift rt left by sa bits, store result in rd
            let shift = i32_type.const_int(u64::from(instr.sa()), false);
            let target = codegen.read_general_register(i32_type, instr.rt());
            let result = codegen.builder.build_left_shift(target, shift, "sll_shift");
            codegen.write_general_register(instr.rd(), result);
        }

        Mnenomic::Or => {
            // OR rs and rt, store result in rd
            let source = codegen.read_general_register(i32_type, instr.rs());
            let target = codegen.read_general_register(i32_type, instr.rt());
            let result = codegen.builder.build_or(source, target, "or_res");
            codegen.write_general_register(instr.rd(), result);
        }

        Mnenomic::Ori => {
            // OR rs and zero-extended 16-bit immediate, store result in rt
            let immediate = i32_type.const_int(u64::from(instr.immediate()), false);
            let source = codegen.read_general_register(i32_type, instr.rs());
            let result = codegen.builder.build_or(source, immediate, "ori_res");
            codegen.write_general_register(instr.rt(), result);
        }

        Mnenomic::Nor => {
            // NOR rs and rt, store result in rd
            let source = codegen.read_general_register(i32_type, instr.rs());
            let target = codegen.read_general_register(i32_type, instr.rt());
            let or = codegen.builder.build_or(source, target, "nor_or");
            let result = codegen.builder.build_not(or, "nor_res");
            codegen.write_general_register(instr.rd(), result);
        }

        Mnenomic::Xor => {
            // XOR rs with rt, store result in rd
            let source = codegen.read_general_register(i32_type, instr.rs());
            let target = codegen.read_general_register(i32_type, instr.rt());
            let result = codegen.builder.build_xor(source, target, "xor_res");
            codegen.write_general_register(instr.rd(), result);
        }

        Mnenomic::Xori => {
            // XOR rs with zero-extended immediate, store result in rd
            let immediate = i32_type.const_int(u64::from(instr.immediate()), false);
            let source = codegen.read_general_register(i32_type, instr.rs());
            let result = codegen.builder.build_xor(source, immediate, "xori_res");
            codegen.write_general_register(instr.rt(), result);
        }

        Mnenomic::And => {
            // AND rs with rt, store result in rd
            let source = codegen.read_general_register(i32_type, instr.rs());
            let target = codegen.read_general_register(i32_type, instr.rt());
            let result = codegen.builder.build_and(source, target, "and_res");
            codegen.write_general_register(instr.rd(), result);
        }

        Mnenomic::Andi => {
            // AND rs with zero-extended 16-bit immediate, store result in rt
            let immediate = i32_type.const_int(u64::from(instr.immediate()), false);
            let source = codegen.read_general_register(i32_type, instr.rs());
            let result = codegen.builder.build_and(source, immediate, "andi_res");
            codegen.write_general_register(instr.rt(), result);
        }

        Mnenomic::Add | Mnenomic::Addu => {
            // Add rs and rt, store result in rd
            let source = codegen.read_general_register(i32_type, instr.rs());
            let target = codegen.read_general_register(i32_type, instr.rt());
            let result = codegen.builder.build_int_add(source, target, "add_res");
            codegen.write_general_register(instr.rd(), result);
        }

        Mnenomic::Addi | Mnenomic::Addiu => {
            // Add sign-extended 16-bit immediate and rs, store result in rt
            let immediate = codegen.sign_extend_to(
                i32_type,
                i16_type.const_int(u64::from(instr.immediate()), true),
            );
            let source = codegen.read_general_register(i32_type, instr.rs());
            let result = codegen.builder.build_int_add(source, immediate, "addi_res");
            codegen.write_general_register(instr.rt(), result);
        }

        Mnenomic::Sub | Mnenomic::Subu => {
            // Subtract rt from rs, store result in rd
            let source = codegen.read_general_register(i32_type, instr.rs());
            let target = codegen.read_general_register(i32_type, instr.rt());
            let result = codegen.builder.build_int_sub(source, target, "sub_res");
            codegen.write_general_register(instr.rd(), result);
        }

        Mnenomic::Lb => {
            // Loads byte stored at memory address (base + offset), stores sign-extended byte in rt
            let address = codegen.base_plus_offset(i32_type, instr, "lb_addr");
            let value = codegen.sign_extend_to(i32_type, codegen.read_memory(i8_type, address));
            codegen.write_general_register(instr.rt(), value);
        }

        Mnenomic::Lbu => {
            // Loads byte stored at memory address (base + offset), stores zero-extended byte in rt
            let address = codegen.base_plus_offset(i32_type, instr, "lbu_addr");
            let value = codegen.zero_extend_to(i32_type, codegen.read_memory(i8_type, address));
            codegen.write_general_register(instr.rt(), value);
        }

        Mnenomic::Lh => {
            // Loads halfword stored at memory address (base + offset), stores sign-extended halfword in rt
            let address = codegen.base_plus_offset(i32_type, instr, "lh_addr");
            let value = codegen.sign_extend_to(i32_type, codegen.read_memory(i16_type, address));
            codegen.write_general_register(instr.rt(), value);
        }

        Mnenomic::Lhu => {
            // Loads halfword stored at memory address (base + offset), stores zero-extended halfword in rt
            let address = codegen.base_plus_offset(i32_type, instr, "lhu_addr");
            let value = codegen.zero_extend_to(i32_type, codegen.read_memory(i16_type, address));
            codegen.write_general_register(instr.rt(), value);
        }

        Mnenomic::Lw | Mnenomic::Lwu => {
            // Loads word stored at memory address (base + offset), stores word in rt
            let address = codegen.base_plus_offset(i32_type, instr, "lw_addr");
            let value = codegen.read_memory(i32_type, address);
            codegen.write_general_register(instr.rt(), value);
        }

        Mnenomic::Sb => {
            // Stores least-significant byte from rt, to memory address (base + offset)
            let target = codegen.read_general_register(i8_type, instr.rt());
            let address = codegen.base_plus_offset(i32_type, instr, "sb_addr");
            codegen.write_memory(address, target);
        }

        Mnenomic::Sh => {
            // Stores halfword from rt, to memory address (base + offset)
            let address = codegen.base_plus_offset(i32_type, instr, "sh_addr");
            let target = codegen.read_general_register(i16_type, instr.rt());
            codegen.write_memory(address, target);
        }

        Mnenomic::Sw => {
            // Stores word from rt, to memory address (base + offset)
            let address = codegen.base_plus_offset(i32_type, instr, "sw_addr");
            let target = codegen.read_general_register(i32_type, instr.rt());
            codegen.write_memory(address, target);
        }

        _ => {
            codegen.build_stub(&format!("RSP instruction: {}", instr.mnemonic().name()));
            return None;
        }
    }
    Some(())
}
