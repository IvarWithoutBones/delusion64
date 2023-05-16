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
    match instr.operand {
        Operand::Immediate {
            source,
            target,
            immediate,
        } => {
            recompile_immediate_instruction(
                codegen,
                labels,
                instr,
                address,
                (source, target, immediate),
            );
        }

        Operand::Register {
            source,
            target,
            destination,
            shift,
        } => {
            recompile_register_instruction(
                codegen,
                labels,
                instr,
                address,
                (source, target, destination, shift),
            );
        }

        _ => todo!(),
    }
}

fn recompile_register_instruction(
    codegen: &CodeGen,
    _labels: &Labels,
    instr: &Instruction,
    _address: u64,
    operands: (u8, u8, u8, u8),
) {
    let (source, target, destination, shift) = operands;
    let i32_type = codegen.context.i32_type();
    let i64_type = codegen.context.i64_type();

    match instr.mnemonic {
        Mnemonic::Nop => {
            // Do nothing.
        }

        Mnemonic::Or => {
            // OR rs and rt, store result in rd
            let source = codegen.load_register(source).into_int_value();
            let target = codegen.load_register(target).into_int_value();
            let result = codegen.builder.build_or(source, target, "or_result");
            codegen.store_register(destination, result.into());
        }

        Mnemonic::Daddu => {
            // Add rs and rt, store result in rd
            let source = codegen.load_register(source).into_int_value();
            let target = codegen.load_register(target).into_int_value();
            let result = codegen
                .builder
                .build_int_add(source, target, "daddu_result");
            codegen.store_register(destination, result.into());
        }

        Mnemonic::Dsll => {
            // Shift rt left by sa bits, store result in rd (64-bits)
            let shift = i64_type.const_int(shift as _, false);
            let target = codegen.load_register(target).into_int_value();
            let result = codegen
                .builder
                .build_left_shift(target, shift, "dsll_result");
            codegen.store_register(destination, result.into());
        }

        Mnemonic::Sltu => {
            // If unsigned rs is less than unsigned rt, store one in rd, otherwise store zero.
            let source = codegen.load_register(source).into_int_value();
            let target = codegen.load_register(target).into_int_value();

            let cmp =
                codegen
                    .builder
                    .build_int_compare(IntPredicate::ULT, source, target, "sltu_cmp");

            // Convert the comparison to a numeric value.
            let result = codegen
                .builder
                .build_int_z_extend(cmp, i64_type, "sltu_result");
            codegen.store_register(destination, result.into());
        }

        Mnemonic::Sll => {
            // Shift rt left by sa bits, store result in rd (32-bits)
            let shift = i64_type.const_int(shift as _, false);
            let target = codegen.load_register(target).into_int_value();
            let result = codegen.builder.build_left_shift(target, shift, "sll_shift");

            // Truncate the result to 32-bits.
            let result = codegen
                .builder
                .build_int_truncate(result, i32_type, "sll_truncate");

            codegen.store_register(destination, result.into());
        }

        Mnemonic::Jr => {
            // Jump to address stored in rs
            // TODO: actually implement this
        }

        _ => todo!("unimplemented register instruction: {instr}"),
    }
}

fn recompile_immediate_instruction(
    codegen: &CodeGen,
    labels: &Labels,
    instr: &Instruction,
    address: u64,
    operands: (u8, u8, u16),
) {
    let (source, target, immediate) = operands;
    let i16_type = codegen.context.i16_type();
    let i64_type = codegen.context.i64_type();

    match instr.mnemonic {
        Mnemonic::Sd => {
            // Stores doubleword from rt, to memory address (base + offset)
            let source = codegen.load_register(source).into_int_value();
            let offset = i16_type.const_int(immediate as _, false);
            let address = codegen.builder.build_int_add(source, offset, "sd_address");
            codegen.store_memory(address, codegen.load_register(target));
        }

        Mnemonic::Ld => {
            // Loads doubleword stored at memory address (base + offset), stores doubleword in rt
            let source = codegen.load_register(source).into_int_value();
            let offset = i16_type.const_int(immediate as _, false);
            let address = codegen.builder.build_int_add(source, offset, "ld_address");
            codegen.store_register(target, codegen.load_memory(i64_type, address));
        }

        Mnemonic::Daddiu => {
            // Add sign-extended 16bit immediate and rs, store result in rt
            let immediate = codegen.sign_extend_to_i64(i16_type.const_int(immediate as _, true));
            let source = codegen.load_register(source).into_int_value();
            let result = codegen
                .builder
                .build_int_add(source, immediate, "daddiu_result");

            codegen.store_register(target, result.into());
        }

        Mnemonic::Sltiu => {
            // If unsigned rs is less than sign-extended immediate, store one in rt, otherwise store zero
            let immediate = codegen.sign_extend_to_i64(i16_type.const_int(immediate as _, true));
            let source = codegen.load_register(source).into_int_value();
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

            codegen.store_register(target, result.into());
        }

        Mnemonic::Beq => {
            // If rs equals rt, branch to address
            let source = codegen.load_register(source).into_int_value();
            let target = codegen.load_register(target).into_int_value();

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

        Mnemonic::Bne => {
            // If rs is not equal to rt, branch to address.
            let source = codegen.load_register(source).into_int_value();
            let target = codegen.load_register(target).into_int_value();

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

        _ => todo!("unimplemented immediate instruction: {instr}"),
    }
}
