use crate::{codegen::CodeGen, env::RuntimeFunction, label::Labels};
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

        Operand::Jump { target } => {
            recompile_jump_instruction(codegen, labels, instr, address, target);
        }
    }
}

fn recompile_jump_instruction(
    codegen: &CodeGen,
    labels: &Labels,
    instr: &Instruction,
    address: u64,
    _target: u32,
) {
    match instr.mnemonic {
        Mnemonic::Jal => {
            // Jump to target address, stores return address in r31 (ra)
            let target = instr.try_resolve_static_jump(address as _).unwrap();
            let target_block = labels.get(&(target as _)).unwrap().basic_block;

            // Store the return address
            let return_address = codegen
                .build_i64(address + INSTRUCTION_SIZE as u64)
                .into_int_value();
            codegen.store_register(31u32, return_address.into());

            // Jump to the target
            println!("jumping from {address:#x} to {:x}", target);
            codegen.builder.build_unconditional_branch(target_block);
        }

        _ => todo!("unimplemented jump instruction: {instr}"),
    }
}

fn recompile_register_instruction(
    codegen: &CodeGen,
    labels: &Labels,
    instr: &Instruction,
    address: u64,
    operands: (u8, u8, u8, u8),
) {
    let (source, target, destination, shift) = operands;
    let i32_type = codegen.context.i32_type();
    let i64_type = codegen.context.i64_type();

    match instr.mnemonic {
        Mnemonic::Nop => {
            // Do nothing.
        }

        Mnemonic::And => {
            // AND rs with rt, store result in rd
            let source = codegen.load_register(source).into_int_value();
            let target = codegen.load_register(target).into_int_value();
            let result = codegen.builder.build_and(source, target, "and_result");
            codegen.store_register(destination, result.into());
        }

        Mnemonic::Xor => {
            // XOR rs with rt, store result in rd
            let source = codegen.load_register(source).into_int_value();
            let target = codegen.load_register(target).into_int_value();
            let result = codegen.builder.build_xor(source, target, "xor_result");
            codegen.store_register(destination, result.into());
        }

        Mnemonic::Addu => {
            // Add rs and rt, store result in rd
            let source = codegen.load_register(source).into_int_value();
            let target = codegen.load_register(target).into_int_value();
            let result = codegen.builder.build_int_add(source, target, "addu_result");
            codegen.store_register(destination, result.into());
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

        Mnemonic::Dsll32 => {
            // Shift rt left by (32 + sa) bits, store result in rd
            let target = codegen.load_register(target).into_int_value();
            let shift = i64_type.const_int((shift + 32) as _, false);
            let result = codegen
                .builder
                .build_left_shift(target, shift, "dsll32_shift");
            codegen.store_register(destination, result.into());
        }

        Mnemonic::Dsrl32 => {
            // Shift rt right by (32 + sa) bits, store sign-extended result in rd
            let target = codegen.load_register(target).into_int_value();
            let shift = i64_type.const_int((shift + 32) as _, false);
            let result = codegen
                .builder
                .build_right_shift(target, shift, true, "dsrl32_shift");
            codegen.store_register(destination, result.into());
        }

        Mnemonic::Jr => {
            // Jump to address stored in rs
            let source = codegen.load_register(source).into_int_value();

            let block_id = {
                let result = codegen.build_env_call(RuntimeFunction::GetBlockId, &[source.into()]);
                result.try_as_basic_value().left().unwrap().into_int_value()
            };

            let current_block = codegen.builder.get_insert_block().unwrap();
            let not_found_block = codegen.build_basic_block("jr_id_not_found").unwrap();
            let cases = labels
                .values()
                .map(|label| {
                    let id = i64_type.const_int(label.id, false);
                    (id, label.basic_block)
                })
                .collect::<Vec<_>>();

            // Loop over all blocks and build a switch statement.
            codegen
                .builder
                .build_switch(block_id, not_found_block, &cases);

            // Build the else block.
            codegen.builder.position_at_end(not_found_block);
            codegen.print_constant_string("ERROR: unable to fetch basic block\n", "jr_error");
            codegen.builder.build_return(None);
            codegen.builder.position_at_end(current_block);
        }

        Mnemonic::Jalr => {
            // Jump to address stored in rs, stores return address in rd
            let source = codegen.load_register(source).into_int_value();

            // Store the return address
            let return_addr = i64_type.const_int(address + INSTRUCTION_SIZE as u64, false);
            codegen.store_register(destination, return_addr.into());

            let block_id = {
                let result = codegen.build_env_call(RuntimeFunction::GetBlockId, &[source.into()]);
                result.try_as_basic_value().left().unwrap().into_int_value()
            };

            let current_block = codegen.builder.get_insert_block().unwrap();
            let not_found_block = codegen.build_basic_block("jalr_id_not_found").unwrap();
            let cases = labels
                .values()
                .map(|label| {
                    let id = i64_type.const_int(label.id, false);
                    (id, label.basic_block)
                })
                .collect::<Vec<_>>();

            // Build a switch statement to find the target block.
            codegen
                .builder
                .build_switch(block_id, not_found_block, &cases);

            // Build the else block.
            codegen.builder.position_at_end(not_found_block);
            codegen.print_constant_string("ERROR: unable to fetch basic block\n", "jarl_error");
            codegen.builder.build_return(None);
            codegen.builder.position_at_end(current_block);
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
    let i8_type = codegen.context.i8_type();
    let i16_type = codegen.context.i16_type();
    let i32_type = codegen.context.i32_type();
    let i64_type = codegen.context.i64_type();

    match instr.mnemonic {
        Mnemonic::Sh => {
            // Stores halfword from rt, to memory address (base + offset)
            let source = codegen.load_register(source).into_int_value();
            let offset = codegen.truncate_to_i64(i16_type.const_int(immediate as _, true));
            let address = codegen.builder.build_int_add(source, offset, "sh_address");

            let target = codegen.load_register(target).into_int_value();
            let target = codegen
                .builder
                .build_int_truncate(target, i32_type, "sh_truncate");

            codegen.store_memory(address, target.into());
        }

        Mnemonic::Sdl => {
            // Loads a portion of rt, stores 1-8 bytes in high-order portion of memory address (base + offset)
            let source = codegen.load_register(source).into_int_value();
            let offset = codegen.truncate_to_i64(i16_type.const_int(immediate as _, true));
            let address = codegen.builder.build_int_add(source, offset, "sdl_address");

            let data = {
                let addr = codegen.builder.build_and(
                    address,
                    i64_type.const_int(!7, false),
                    "sdl_data_and",
                );
                codegen.load_memory(i64_type, addr).into_int_value()
            };

            let shift = {
                let zero = i64_type.const_zero();
                let seven = i64_type.const_int(7, false);
                let eight = i64_type.const_int(8, false);

                let xor = codegen.builder.build_xor(address, zero, "sdl_shift_xor");
                let and = codegen.builder.build_and(xor, seven, "sdl_shift_and");
                codegen.builder.build_int_mul(eight, and, "sdl_shift_mul")
            };

            let old_register = {
                let reg = codegen.load_register(target).into_int_value();
                codegen
                    .builder
                    .build_right_shift(reg, shift, false, "sdl_reg_shift")
            };

            let mask = {
                let max = i64_type.const_all_ones();
                let mask = codegen
                    .builder
                    .build_right_shift(max, shift, false, "sdl_mask");
                codegen.builder.build_not(mask, "sdl_mask_not")
            };

            let result = {
                let and = codegen.builder.build_and(data, mask, "sdl_result_and");
                codegen.builder.build_or(and, old_register, "sdl_result_or")
            };

            codegen.store_register(target, result.into());
        }

        Mnemonic::Sdr => {
            // Loads a portion of rt, stores 1-8 bytes in low-order portion of memory address (base + offset)
            let source = codegen.load_register(source).into_int_value();
            let offset = codegen.truncate_to_i64(i16_type.const_int(immediate as _, true));
            let address = codegen.builder.build_int_add(source, offset, "sdl_address");

            let data = {
                let addr = codegen.builder.build_and(
                    address,
                    i64_type.const_int(!7, false),
                    "sdr_data_and",
                );
                codegen.load_memory(i64_type, addr).into_int_value()
            };

            let shift = {
                let seven = i64_type.const_int(7, false);
                let eight = i64_type.const_int(8, false);

                let xor = codegen.builder.build_xor(address, seven, "sdr_shift_xor");
                let and = codegen.builder.build_and(xor, seven, "sdr_shift_and");
                codegen.builder.build_int_mul(eight, and, "sdr_shift_mul")
            };

            let old_register = {
                let reg = codegen.load_register(target).into_int_value();
                codegen
                    .builder
                    .build_left_shift(reg, shift, "sdr_reg_shift")
            };

            let mask = {
                let max = i64_type.const_all_ones();
                let mask = codegen.builder.build_left_shift(max, shift, "sdr_mask");
                codegen.builder.build_not(mask, "sdr_mask_not")
            };

            let result = {
                let and = codegen.builder.build_and(data, mask, "sdr_result_and");
                codegen.builder.build_or(and, old_register, "sdr_result_or")
            };

            codegen.store_register(target, result.into());
        }

        Mnemonic::Sb => {
            // Stores least-significant byte from rt, to memory address (base + offset)
            let target = codegen.load_register(target).into_int_value();

            // TODO: is this the least significant byte?
            let target = codegen
                .builder
                .build_int_truncate(target, i8_type, "sb_truncate");
            let target = codegen.zero_extend_to_i64(target);

            let offset = codegen.truncate_to_i64(i16_type.const_int(immediate as _, true));
            let address = codegen.builder.build_int_add(target, offset, "sb_address");
            codegen.store_memory(address, codegen.load_register(source));
        }

        Mnemonic::Sd => {
            // Stores doubleword from rt, to memory address (base + offset)
            let source = codegen.load_register(source).into_int_value();
            let offset = codegen.truncate_to_i64(i16_type.const_int(immediate as _, true));
            let address = codegen.builder.build_int_add(source, offset, "sd_address");
            codegen.store_memory(address, codegen.load_register(target));
        }

        Mnemonic::Ld => {
            // Loads doubleword stored at memory address (base + offset), stores doubleword in rt
            let source = codegen.load_register(source).into_int_value();
            let offset = codegen.truncate_to_i64(i16_type.const_int(immediate as _, false));
            let address = codegen.builder.build_int_add(source, offset, "ld_address");
            codegen.store_register(target, codegen.load_memory(i64_type, address));
        }

        Mnemonic::Lbu => {
            // Loads byte stored at memory address (base + offset), stores zero-extended byte in rt
            let source = codegen.load_register(source).into_int_value();
            let offset = codegen.truncate_to_i64(i16_type.const_int(immediate as _, false));
            let address = codegen.builder.build_int_add(source, offset, "lbu_address");

            let value = {
                let value = codegen.load_memory(i8_type, address);
                codegen.zero_extend_to_i64(value.into_int_value())
            };
            codegen.store_register(target, value.into());
        }

        Mnemonic::Lb => {
            // Loads byte stored at memory address (base + offset), stores sign-extended byte in rt
            let source = codegen.load_register(source).into_int_value();
            let offset = codegen.truncate_to_i64(i16_type.const_int(immediate as _, false));
            let address = codegen.builder.build_int_add(source, offset, "lb_address");

            let value = {
                let value = codegen.load_memory(i8_type, address);
                codegen.sign_extend_to_i64(value.into_int_value())
            };
            codegen.store_register(target, value.into());
        }

        Mnemonic::Lui => {
            // 16-bit immediate is shifted left 16 bits using trailing zeros, result placed in rt
            let immediate = codegen.zero_extend_to_i64(i16_type.const_int(immediate as _, false));
            let shift_amount = i64_type.const_int(16, false);
            let result = codegen
                .builder
                .build_left_shift(immediate, shift_amount, "lui_result");
            codegen.store_register(target, result.into());
        }

        Mnemonic::Addiu => {
            // Add sign-extended 16bit immediate and rs, store result in rt
            let immediate = codegen.sign_extend_to_i64(i16_type.const_int(immediate as _, true));
            let source = codegen.load_register(source).into_int_value();
            let result = codegen
                .builder
                .build_int_add(source, immediate, "addiu_result");
            codegen.store_register(target, result.into());
        }

        Mnemonic::Andi => {
            // AND rs with zero-extended immediate, store result in rt
            let source = codegen.load_register(source).into_int_value();
            let immediate = codegen.zero_extend_to_i64(i16_type.const_int(immediate as _, false));
            let result = codegen.builder.build_and(source, immediate, "andi_result");
            codegen.store_register(target, result.into());
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
