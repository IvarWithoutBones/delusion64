use super::{register, Rsp};
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

#[allow(clippy::cast_possible_truncation, clippy::unnecessary_wraps)]
pub(crate) fn compile_instruction(codegen: &CodeGen<Rsp>, instr: &ParsedInstruction) -> Option<()> {
    let i32_type = codegen.context.i32_type();

    match instr.mnemonic() {
        Mnenomic::Break => {
            codegen.throw_exception(Exception::Breakpoint, None, None);
        }

        Mnenomic::Sll => {
            // Shift rt left by sa bits, store result in rd
            let shift = i32_type.const_int(u64::from(instr.sa()), false);
            let target = codegen.read_register_raw(
                i32_type,
                register::GeneralPurpose::from_repr(instr.rt() as u8).unwrap(),
            );
            let result = codegen.builder.build_left_shift(target, shift, "sll_shift");
            codegen.write_register_raw(
                register::GeneralPurpose::from_repr(instr.rd() as u8).unwrap(),
                result,
            );
        }

        Mnenomic::Ori => {
            // OR rs and zero-extended 16-bit immediate, store result in rt
            let immediate = i32_type.const_int(u64::from(instr.immediate()), false);
            let source = codegen.read_register_raw(
                i32_type,
                register::GeneralPurpose::from_repr(instr.rs() as u8).unwrap(),
            );

            let result = codegen.builder.build_or(source, immediate, "ori_res");
            codegen.write_register_raw(
                register::GeneralPurpose::from_repr(instr.rt() as u8).unwrap(),
                result,
            );
        }

        _ => todo!("RSP compile_instruction {instr}"),
    }
    Some(())
}
