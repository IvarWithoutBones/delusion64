use crate::{codegen::CodeGen, label::Labels};
use inkwell::IntPredicate;
use mips_decomp::{
    instruction::{Instruction, Mnemonic, Operand},
    INSTRUCTION_SIZE,
};

pub fn recompile_instruction(
    codegen: &CodeGen,
    labels: &Labels,
    instr: &Instruction,
    address: u64,
) {
    let i16_type = codegen.context.i16_type();
    let i32_type = codegen.context.i32_type();
    let i64_type = codegen.context.i64_type();

    match (&instr.mnemonic, &instr.operand) {
        (Mnemonic::Nop, ..) => {
            // Do nothing
        }

        (
            Mnemonic::Daddiu,
            Operand::Immediate {
                source,
                target,
                immediate,
            },
        ) => {
            // Add sign-extended 16bit immediate and rs, store result in rt
            let immediate = i16_type.const_int(*immediate as _, true);
            let source = codegen.load_register(*source).into_int_value();
            let added = codegen
                .builder
                .build_int_add(source, immediate, "daddiu_result");

            // Truncate the result to 16-bits
            let result = codegen
                .builder
                .build_int_truncate(added, i16_type, "daddiu_result_trunc");
            codegen.store_register(*target, result.into());
        }

        (
            Mnemonic::Sltiu,
            Operand::Immediate {
                source,
                target,
                immediate,
            },
        ) => {
            // If unsigned rs is less than sign-extended immediate, store one in rt, otherwise store zero
            let immediate = i16_type.const_int(*immediate as _, true);
            let source = codegen.load_register(*source).into_int_value();
            let cmp = codegen.builder.build_int_compare(
                IntPredicate::ULT,
                source,
                immediate,
                "sltiu_cmp",
            );

            // Convert the comparison to a numeric value.
            let result = codegen
                .builder
                .build_int_z_extend(cmp, i64_type, "sltiu_result");
            codegen.store_register(*target, result.into());
        }

        (
            Mnemonic::Daddu,
            Operand::Register {
                destination,
                source,
                target,
                ..
            },
        ) => {
            // Add rs and rt, store result in rd
            let source = codegen.load_register(*source).into_int_value();
            let target = codegen.load_register(*target).into_int_value();
            let result = codegen
                .builder
                .build_int_add(source, target, "daddu_result");
            codegen.store_register(*destination, result.into());
        }

        (
            Mnemonic::Dsll,
            Operand::Register {
                destination,
                target,
                shift,
                ..
            },
        ) => {
            // Shift rt left by sa bits, store result in rd (64-bits)
            let shift = i64_type.const_int(*shift as _, false);
            let target = codegen.load_register(*target).into_int_value();
            let result = codegen
                .builder
                .build_left_shift(target, shift, "dsll_result");
            codegen.store_register(*destination, result.into());
        }

        (
            Mnemonic::Sltu,
            Operand::Register {
                destination,
                target,
                source,
                ..
            },
        ) => {
            // If unsigned rs is less than unsigned rt, store one in rd, otherwise store zero.
            let source = codegen.load_register(*source).into_int_value();
            let target = codegen.load_register(*target).into_int_value();

            let cmp =
                codegen
                    .builder
                    .build_int_compare(IntPredicate::ULT, source, target, "sltu_cmp");

            // Convert the comparison to a numeric value.
            let result = codegen
                .builder
                .build_int_z_extend(cmp, i64_type, "sltu_result");
            codegen.store_register(*destination, result.into());
        }

        (
            Mnemonic::Sll,
            Operand::Register {
                destination,
                target,
                shift,
                ..
            },
        ) => {
            // Shift rt left by sa bits, store result in rd (32-bits)
            let shift = i64_type.const_int(*shift as _, false);
            let target = codegen.load_register(*target).into_int_value();
            let result = codegen.builder.build_left_shift(target, shift, "sll_shift");

            // Truncate the result to 32-bits.
            let result = codegen
                .builder
                .build_int_truncate(result, i32_type, "sll_truncate");

            codegen.store_register(*destination, result.into());
        }

        (Mnemonic::Beq, Operand::Immediate { source, target, .. }) => {
            // If rs equals rt, branch to address
            let source = codegen.load_register(*source).into_int_value();
            let target = codegen.load_register(*target).into_int_value();

            let cmp =
                codegen
                    .builder
                    .build_int_compare(IntPredicate::EQ, source, target, "beq_cmp");

            let then_block = labels
                .get(&(instr.try_resolve_static_jump(address as _).unwrap() as _))
                .unwrap()
                .basic_block;

            let else_block = labels
                .get(&(address + INSTRUCTION_SIZE as u64))
                .unwrap()
                .basic_block;

            codegen
                .builder
                .build_conditional_branch(cmp, then_block, else_block);
        }

        (Mnemonic::Bne, Operand::Immediate { source, target, .. }) => {
            // If rs is not equal to rt, branch to address.
            let source = codegen.load_register(*source).into_int_value();
            let target = codegen.load_register(*target).into_int_value();

            let cmp =
                codegen
                    .builder
                    .build_int_compare(IntPredicate::NE, source, target, "bne_cmp");

            let then_block = labels
                .get(&(instr.try_resolve_static_jump(address as _).unwrap() as _))
                .unwrap()
                .basic_block;

            let else_block = labels
                .get(&(address + INSTRUCTION_SIZE as u64))
                .unwrap()
                .basic_block;

            codegen
                .builder
                .build_conditional_branch(cmp, then_block, else_block);
        }

        (Mnemonic::Jr, _) => {
            // Jump to address stored in rs
            codegen.call_exit_block(); // Inaccurate, just for testing
        }

        _ => todo!("unimplemented instruction: {instr}"),
    };
}
