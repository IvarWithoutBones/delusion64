#![allow(dead_code)]

use super::stub;
use crate::{codegen::CodeGen, target::Cpu};
use inkwell::values::BasicValue;
use mips_decomp::instruction::{Mnenomic, ParsedInstruction};

// TODO: CodeGen generics are incorrect!
pub fn compile_instruction(codegen: &CodeGen<Cpu>, instr: &ParsedInstruction) -> Option<()> {
    let i32_type = codegen.context.i32_type();
    let mnemonic = instr.mnemonic();

    match mnemonic {
        Mnenomic::Sll => {
            // Shift rt left by sa bits, store result in rd
            let shift = i32_type.const_int(instr.sa() as u64, false);
            let target = codegen.read_general_register(i32_type, instr.rt());
            let result = codegen.builder.build_left_shift(target, shift, "sll_shift");
            codegen.write_general_register(instr.rd(), result);
        }

        Mnenomic::Break => {
            // TODO: this is the base pointer, which is not accurate
            let ptr = codegen
                .globals
                .as_ref()
                .unwrap()
                .registers
                .cp0
                .pointer_value();

            let halted_and_broke = i32_type.const_int(0b11, false);

            let status = codegen.builder.build_load(i32_type, ptr, "status");
            status
                .as_instruction_value()
                .unwrap()
                .set_atomic_ordering(inkwell::AtomicOrdering::Acquire)
                .unwrap();

            let new_status =
                codegen
                    .builder
                    .build_or(status.into_int_value(), halted_and_broke, "new_status");

            let store = codegen.builder.build_store(ptr, new_status);
            store
                .set_atomic_ordering(inkwell::AtomicOrdering::Release)
                .unwrap();

            codegen.build_panic("see cp0 reg #1", "sp_status_test");
        }

        _ => stub(codegen, mnemonic.name()),
    }

    Some(())
}
