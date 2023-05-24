use crate::{codegen::CodeGen, env::RuntimeFunction, label::Labels};
use inkwell::IntPredicate;
use mips_decomp::{
    instruction::{Mnenomic, ParsedInstruction},
    INSTRUCTION_SIZE,
};

pub fn recompile_instruction(
    codegen: &CodeGen,
    labels: &Labels,
    instr: &ParsedInstruction,
    address: u64,
) {
    let mnemonic = instr.mnemonic();
    let i8_type = codegen.context.i8_type();
    let i16_type = codegen.context.i16_type();
    let i32_type = codegen.context.i32_type();
    let i64_type = codegen.context.i64_type();

    match mnemonic {
        Mnenomic::Jal => {
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

        Mnenomic::And => {
            // AND rs with rt, store result in rd
            let source = codegen.load_register(instr.rs()).into_int_value();
            let target = codegen.load_register(instr.rt()).into_int_value();
            let result = codegen.builder.build_and(source, target, "and_result");
            codegen.store_register(instr.rd(), result.into());
        }

        Mnenomic::Xor => {
            // XOR rs with rt, store result in rd
            let source = codegen.load_register(instr.rs()).into_int_value();
            let target = codegen.load_register(instr.rt()).into_int_value();
            let result = codegen.builder.build_xor(source, target, "xor_result");
            codegen.store_register(instr.rd(), result.into());
        }

        Mnenomic::Addu => {
            // Add rs and rt, store result in rd
            let source = codegen.load_register(instr.rs()).into_int_value();
            let target = codegen.load_register(instr.rt()).into_int_value();
            let result = codegen.builder.build_int_add(source, target, "addu_result");
            codegen.store_register(instr.rd(), result.into());
        }

        Mnenomic::Or => {
            // OR rs and rt, store result in rd
            let source = codegen.load_register(instr.rs()).into_int_value();
            let target = codegen.load_register(instr.rt()).into_int_value();
            let result = codegen.builder.build_or(source, target, "or_result");
            codegen.store_register(instr.rd(), result.into());
        }

        Mnenomic::Daddu => {
            // Add rs and rt, store result in rd
            let source = codegen.load_register(instr.rs()).into_int_value();
            let target = codegen.load_register(instr.rt()).into_int_value();
            let result = codegen
                .builder
                .build_int_add(source, target, "daddu_result");
            codegen.store_register(instr.rd(), result.into());
        }

        Mnenomic::Dsll => {
            // Shift rt left by sa bits, store result in rd (64-bits)
            let shift = i64_type.const_int(instr.sa() as _, false);
            let target = codegen.load_register(instr.rt()).into_int_value();
            let result = codegen
                .builder
                .build_left_shift(target, shift, "dsll_result");
            codegen.store_register(instr.rd(), result.into());
        }

        Mnenomic::Sltu => {
            // If unsigned rs is less than unsigned rt, store one in rd, otherwise store zero.
            let source = codegen.load_register(instr.rs()).into_int_value();
            let target = codegen.load_register(instr.rt()).into_int_value();

            let cmp =
                codegen
                    .builder
                    .build_int_compare(IntPredicate::ULT, source, target, "sltu_cmp");

            // Convert the comparison to a numeric value.
            let result = codegen
                .builder
                .build_int_z_extend(cmp, i64_type, "sltu_result");
            codegen.store_register(instr.rd(), result.into());
        }

        Mnenomic::Sll => {
            // Shift rt left by sa bits, store result in rd (32-bits)
            let shift = i64_type.const_int(instr.sa() as _, false);
            let target = codegen.load_register(instr.rt()).into_int_value();
            let result = codegen.builder.build_left_shift(target, shift, "sll_shift");

            // Truncate the result to 32-bits.
            let result = codegen
                .builder
                .build_int_truncate(result, i32_type, "sll_truncate");

            codegen.store_register(instr.rd(), result.into());
        }

        Mnenomic::Dsll32 => {
            // Shift rt left by (32 + sa) bits, store result in rd
            let target = codegen.load_register(instr.rt()).into_int_value();
            let shift = i64_type.const_int((instr.sa() + 32) as _, false);
            let result = codegen
                .builder
                .build_left_shift(target, shift, "dsll32_shift");
            codegen.store_register(instr.rd(), result.into());
        }

        Mnenomic::Dsrl32 => {
            // Shift rt right by (32 + sa) bits, store sign-extended result in rd
            let target = codegen.load_register(instr.rt()).into_int_value();
            let shift = i64_type.const_int((instr.sa() + 32) as _, false);
            let result = codegen
                .builder
                .build_right_shift(target, shift, true, "dsrl32_shift");
            codegen.store_register(instr.rd(), result.into());
        }

        Mnenomic::Jr => {
            // Jump to address stored in rs
            let source = codegen.load_register(instr.rs()).into_int_value();

            let block_id = {
                let result = codegen.build_env_call(RuntimeFunction::GetBlockId, &[source.into()]);
                result.try_as_basic_value().left().unwrap().into_int_value()
            };

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
                .build_switch(block_id, codegen.label_not_found, &cases);
        }

        Mnenomic::Jalr => {
            // Jump to address stored in rs, stores return address in rd
            let source = codegen.load_register(instr.rs()).into_int_value();

            // Store the return address
            let return_addr = i64_type.const_int(address + INSTRUCTION_SIZE as u64, false);
            codegen.store_register(instr.rd(), return_addr.into());

            let block_id = {
                let result = codegen.build_env_call(RuntimeFunction::GetBlockId, &[source.into()]);
                result.try_as_basic_value().left().unwrap().into_int_value()
            };

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
                .build_switch(block_id, codegen.label_not_found, &cases);
        }

        Mnenomic::Sh => {
            // Stores halfword from rt, to memory address (base + offset)
            let address = codegen.base_plus_offset(instr, "sh_address");
            let target = codegen.load_register(instr.rt()).into_int_value();
            let target = codegen
                .builder
                .build_int_truncate(target, i16_type, "sh_truncate");
            codegen.store_memory(address, target.into());
        }

        Mnenomic::Sdl => {
            // Loads a portion of rt, stores 1-8 bytes in high-order portion of memory address (base + offset)
            let address = codegen.base_plus_offset(instr, "sdl_address");
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
                let reg = codegen.load_register(instr.rt()).into_int_value();
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

            codegen.store_register(instr.rt(), result.into());
        }

        Mnenomic::Sdr => {
            // Loads a portion of rt, stores 1-8 bytes in low-order portion of memory address (base + offset)
            let address = codegen.base_plus_offset(instr, "sdr_address");
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
                let reg = codegen.load_register(instr.rt()).into_int_value();
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

            codegen.store_register(instr.rt(), result.into());
        }

        Mnenomic::Sb => {
            // Stores least-significant byte from rt, to memory address (base + offset)
            let target = codegen.load_register(instr.rt()).into_int_value();
            // TODO: is this the least significant byte?
            let target = codegen
                .builder
                .build_int_truncate(target, i8_type, "sb_truncate");

            let address = codegen.base_plus_offset(instr, "sb_address");
            codegen.store_memory(address, target.into());
        }

        Mnenomic::Sd => {
            // Stores doubleword from rt, to memory address (base + offset)
            let target = codegen.load_register(instr.rt()).into_int_value();
            let address = codegen.base_plus_offset(instr, "sd_address");
            codegen.store_memory(address, target.into());
        }

        Mnenomic::Ld => {
            // Loads doubleword stored at memory address (base + offset), stores doubleword in rt
            let address = codegen.base_plus_offset(instr, "ld_address");
            let value = codegen.load_memory(i64_type, address);
            codegen.store_register(instr.rt(), value);
        }

        Mnenomic::Lbu => {
            // Loads byte stored at memory address (base + offset), stores zero-extended byte in rt
            let address = codegen.base_plus_offset(instr, "lbu_address");
            let value = {
                let value = codegen.load_memory(i8_type, address);
                codegen.zero_extend_to_i64(value.into_int_value())
            };

            codegen.store_register(instr.rt(), value.into());
        }

        Mnenomic::Lb => {
            // Loads byte stored at memory address (base + offset), stores sign-extended byte in rt
            let address = codegen.base_plus_offset(instr, "lb_address");
            let value = {
                let value = codegen.load_memory(i8_type, address);
                codegen.sign_extend_to_i64(value.into_int_value())
            };
            codegen.store_register(instr.rt(), value.into());
        }

        Mnenomic::Lw => {
            // Loads word stored at memory address (base + offset), stores sign-extended word in rt
            let address = codegen.base_plus_offset(instr, "lw_address");
            let value = {
                let value = codegen.load_memory(i32_type, address);
                codegen.sign_extend_to_i64(value.into_int_value())
            };
            codegen.store_register(instr.rt(), value.into());
        }

        Mnenomic::Lui => {
            // 16-bit immediate is shifted left 16 bits using trailing zeros, result placed in rt
            let immediate =
                codegen.zero_extend_to_i64(i16_type.const_int(instr.immediate() as _, false));
            let shift_amount = i64_type.const_int(16, false);
            let result = codegen
                .builder
                .build_left_shift(immediate, shift_amount, "lui_result");
            codegen.store_register(instr.rt(), result.into());
        }

        Mnenomic::Addiu => {
            // Add sign-extended 16bit immediate and rs, store result in rt
            let immediate =
                codegen.sign_extend_to_i64(i16_type.const_int(instr.immediate() as _, true));
            let source = codegen.load_register(instr.rs()).into_int_value();
            let result = codegen
                .builder
                .build_int_add(source, immediate, "addiu_result");
            codegen.store_register(instr.rt(), result.into());
        }

        Mnenomic::Andi => {
            // AND rs with zero-extended immediate, store result in rt
            let source = codegen.load_register(instr.rs()).into_int_value();
            let immediate =
                codegen.zero_extend_to_i64(i16_type.const_int(instr.immediate() as _, false));
            let result = codegen.builder.build_and(source, immediate, "andi_result");
            codegen.store_register(instr.rt(), result.into());
        }

        Mnenomic::Addi => {
            // Add sign-extended 16bit immediate and rs, store result in rt
            let immediate =
                codegen.sign_extend_to_i64(i16_type.const_int(instr.immediate() as _, true));
            let source = codegen.load_register(instr.rs()).into_int_value();
            let result = codegen
                .builder
                .build_int_add(source, immediate, "addi_result");
            codegen.store_register(instr.rt(), result.into());
        }

        Mnenomic::Ori => {
            // OR rs and zero-extended immediate, store result in rt
            let source = codegen.load_register(instr.rs()).into_int_value();
            let immediate =
                codegen.zero_extend_to_i64(i16_type.const_int(instr.immediate() as _, false));
            let result = codegen.builder.build_or(source, immediate, "ori_result");
            codegen.store_register(instr.rt(), result.into());
        }

        Mnenomic::Daddiu => {
            // Add sign-extended 16bit immediate and rs, store result in rt
            let immediate =
                codegen.sign_extend_to_i64(i16_type.const_int(instr.immediate() as _, true));
            let source = codegen.load_register(instr.rs()).into_int_value();
            let result = codegen
                .builder
                .build_int_add(source, immediate, "daddiu_result");

            codegen.store_register(instr.rt(), result.into());
        }

        Mnenomic::Sltiu => {
            // If unsigned rs is less than sign-extended immediate, store one in rt, otherwise store zero
            let immediate =
                codegen.sign_extend_to_i64(i16_type.const_int(instr.immediate() as _, true));
            let source = codegen.load_register(instr.rs()).into_int_value();
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

            codegen.store_register(instr.rt(), result.into());
        }

        Mnenomic::Beq => {
            // If rs equals rt, branch to address
            let source = codegen.load_register(instr.rs()).into_int_value();
            let target = codegen.load_register(instr.rt()).into_int_value();

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

        Mnenomic::Bne => {
            // If rs is not equal to rt, branch to address.
            let source = codegen.load_register(instr.rs()).into_int_value();
            let target = codegen.load_register(instr.rt()).into_int_value();

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

        _ => todo!("instruction {} at {address:#x}", instr.mnemonic().name()),
    }
}
