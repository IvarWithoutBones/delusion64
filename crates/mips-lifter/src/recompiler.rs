use crate::{codegen::CodeGen, env_call, label::Labels, runtime::RuntimeFunction};
use inkwell::IntPredicate;
use mips_decomp::{
    instruction::{Mnenomic, ParsedInstruction},
    register, INSTRUCTION_SIZE,
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
        Mnenomic::Mtc0 => {
            // Copy contents of GPR rt, to CP0's coprocessor register rd
            let target = codegen.read_general_reg(instr.rt());
            codegen.write_cp0_reg(instr.rd(), target);
        }

        Mnenomic::Mfc0 => {
            // Copy contents of CP0's coprocessor register rd, to GPR rt
            let destination = codegen.read_cp0_reg(instr.rd());
            codegen.write_general_reg(instr.rt(), destination);
        }

        Mnenomic::Lwcz => {
            // Copies word stored at memory address (base + offset), to CPz register rt
            assert_ne!(instr.coprocessor(), 1); // TODO: remove once the FPU is implemented
            let address = codegen.base_plus_offset(instr, "lwcz_addr");
            let value = codegen.read_memory(i32_type, address);
            codegen.write_cp0_reg(instr.rt(), value);
        }

        Mnenomic::Copz => {
            // Perform coprocessor operation
            // TODO: Implement this
        }

        Mnenomic::Break => {
            // Causes breakpoint exception
            // TODO: Implement this
        }

        Mnenomic::Jal => {
            // Jump to target address, stores return address in r31 (ra)
            let target = instr.try_resolve_static_jump(address as _).unwrap();
            let target_block = labels.get(&(target as _)).unwrap().basic_block;

            // Store the return address
            let return_address = codegen
                .build_i64(address + INSTRUCTION_SIZE as u64)
                .into_int_value();
            codegen.write_general_reg(register::GeneralPurpose::Ra, return_address.into());

            // Jump to the target
            println!("jumping from {address:#x} to {target:#x}");
            codegen.builder.build_unconditional_branch(target_block);
        }

        Mnenomic::J => {
            // Jump to target address
            let target = instr.try_resolve_static_jump(address as _).unwrap();
            let target_block = labels.get(&(target as _)).unwrap().basic_block;
            println!("jumping from {address:#x} to {target:#x}");
            codegen.builder.build_unconditional_branch(target_block);
        }

        Mnenomic::And => {
            // AND rs with rt, store result in rd
            let source = codegen.read_general_reg(instr.rs()).into_int_value();
            let target = codegen.read_general_reg(instr.rt()).into_int_value();
            let result = codegen.builder.build_and(source, target, "and_res");
            codegen.write_general_reg(instr.rd(), result.into());
        }

        Mnenomic::Xor => {
            // XOR rs with rt, store result in rd
            let source = codegen.read_general_reg(instr.rs()).into_int_value();
            let target = codegen.read_general_reg(instr.rt()).into_int_value();
            let result = codegen.builder.build_xor(source, target, "xor_res");
            codegen.write_general_reg(instr.rd(), result.into());
        }

        Mnenomic::Xori => {
            // XOR rs with zero-extended immediate, store result in rd
            let source = codegen.read_general_reg(instr.rs()).into_int_value();
            let immediate =
                codegen.zero_extend_to_i64(i16_type.const_int(instr.immediate() as _, false));

            let result = codegen.builder.build_xor(source, immediate, "xori_res");
            codegen.write_general_reg(instr.rt(), result.into());
        }

        Mnenomic::Addu => {
            // Add rs and rt, store result in rd
            let source = codegen.read_general_reg(instr.rs()).into_int_value();
            let target = codegen.read_general_reg(instr.rt()).into_int_value();
            let result = codegen.builder.build_int_add(source, target, "addu_res");
            codegen.write_general_reg(instr.rd(), result.into());
        }

        Mnenomic::Or => {
            // OR rs and rt, store result in rd
            let source = codegen.read_general_reg(instr.rs()).into_int_value();
            let target = codegen.read_general_reg(instr.rt()).into_int_value();
            let result = codegen.builder.build_or(source, target, "or_res");
            codegen.write_general_reg(instr.rd(), result.into());
        }

        Mnenomic::Daddu => {
            // Add rs and rt, store result in rd
            let source = codegen.read_general_reg(instr.rs()).into_int_value();
            let target = codegen.read_general_reg(instr.rt()).into_int_value();
            let result = codegen.builder.build_int_add(source, target, "daddu_res");
            codegen.write_general_reg(instr.rd(), result.into());
        }

        Mnenomic::Dsll => {
            // Shift rt left by sa bits, store result in rd (64-bits)
            let shift = i64_type.const_int(instr.sa() as _, false);
            let target = codegen.read_general_reg(instr.rt()).into_int_value();
            let result = codegen.builder.build_left_shift(target, shift, "dsll_res");
            codegen.write_general_reg(instr.rd(), result.into());
        }

        Mnenomic::Sltu => {
            // If unsigned rs is less than unsigned rt, store one in rd, otherwise store zero.
            let source = codegen.read_general_reg(instr.rs()).into_int_value();
            let target = codegen.read_general_reg(instr.rt()).into_int_value();

            let cmp =
                codegen
                    .builder
                    .build_int_compare(IntPredicate::ULT, source, target, "sltu_cmp");

            // Convert the comparison to a numeric value.
            let result = codegen
                .builder
                .build_int_z_extend(cmp, i64_type, "sltu_res");
            codegen.write_general_reg(instr.rd(), result.into());
        }

        Mnenomic::Sll => {
            // Shift rt left by sa bits, store result in rd (32-bits)
            let shift = i64_type.const_int(instr.sa() as _, false);
            let target = codegen.read_general_reg(instr.rt()).into_int_value();
            let result = codegen.builder.build_left_shift(target, shift, "sll_shift");

            // Truncate the result to 32-bits.
            let result = codegen
                .builder
                .build_int_truncate(result, i32_type, "sll_trunc");

            codegen.write_general_reg(instr.rd(), result.into());
        }

        Mnenomic::Sllv => {
            // Shift rt left by rs (limited to 31), store result in rd
            let target = codegen.read_general_reg(instr.rt()).into_int_value();
            let source = codegen.read_general_reg(instr.rs()).into_int_value();

            // Limit the shift to 31 bits.
            let shift = codegen
                .builder
                .build_int_truncate(source, i32_type, "sllv_trunc");
            let shift = codegen
                .builder
                .build_int_z_extend(shift, i64_type, "sllv_extend");

            let result = codegen
                .builder
                .build_left_shift(target, shift, "sllv_shift");
            codegen.write_general_reg(instr.rd(), result.into());
        }

        Mnenomic::Dsll32 => {
            // Shift rt left by (32 + sa) bits, store result in rd
            let target = codegen.read_general_reg(instr.rt()).into_int_value();
            let shift = i64_type.const_int((instr.sa() + 32) as _, false);
            let result = codegen
                .builder
                .build_left_shift(target, shift, "dsll32_shift");
            codegen.write_general_reg(instr.rd(), result.into());
        }

        Mnenomic::Dsrl32 => {
            // Shift rt right by (32 + sa) bits, store sign-extended result in rd
            let target = codegen.read_general_reg(instr.rt()).into_int_value();
            let shift = i64_type.const_int((instr.sa() + 32) as _, false);
            let result = codegen
                .builder
                .build_right_shift(target, shift, true, "dsrl32_shift");
            codegen.write_general_reg(instr.rd(), result.into());
        }

        Mnenomic::Srl => {
            // Shift rt right by sa bits, store sign-extended result in rd
            let target = codegen.read_general_reg(instr.rt()).into_int_value();
            let shift = i64_type.const_int(instr.sa() as _, false);
            let result = codegen
                .builder
                .build_right_shift(target, shift, true, "srl_shift");
            codegen.write_general_reg(instr.rd(), result.into());
        }

        Mnenomic::Srlv => {
            // Shift rt right by rs (limited to 31), store sign-extended result in rd
            let target = codegen.read_general_reg(instr.rt()).into_int_value();
            let source = codegen.read_general_reg(instr.rs()).into_int_value();

            // Limit the shift to 31 bits.
            let shift = codegen
                .builder
                .build_int_truncate(source, i32_type, "srlv_trunc");
            let shift = codegen
                .builder
                .build_int_z_extend(shift, i64_type, "srlv_extend");

            let result = codegen
                .builder
                .build_right_shift(target, shift, true, "srlv_shift");
            codegen.write_general_reg(instr.rd(), result.into());
        }

        Mnenomic::Multu => {
            // Multiply unsigned rs by unsigned rt, store low-order word of result in register LO and high-order word in HI
            let source = codegen.read_general_reg(instr.rs()).into_int_value();
            let target = codegen.read_general_reg(instr.rt()).into_int_value();

            let result = codegen.builder.build_int_mul(source, target, "multu_res");

            // Store the low-order word in LO.
            let lo = codegen
                .builder
                .build_int_truncate(result, i32_type, "multu_lo");
            codegen.write_special_reg(register::Special::Lo, lo.into());

            // Store the high-order word in HI.
            let hi = codegen.builder.build_right_shift(
                result,
                i64_type.const_int(u64::BITS as u64 / 2, false),
                false,
                "multu_hi",
            );
            codegen.write_special_reg(register::Special::Hi, hi.into());
        }

        Mnenomic::Mflo => {
            // Copy contents of register LO to rd
            // TODO: Should produce incorrect results if any of the two following instructions modify the HI and LO registers
            let lo = codegen
                .read_special_reg(register::Special::Lo)
                .into_int_value();
            codegen.write_general_reg(instr.rd(), lo.into());
        }

        Mnenomic::Jr => {
            // Jump to address stored in rs
            let source = codegen.read_general_reg(instr.rs()).into_int_value();

            let block_id = {
                let result = env_call!(codegen, RuntimeFunction::GetBlockId, [source]);
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
            let source = codegen.read_general_reg(instr.rs()).into_int_value();

            // Store the return address
            let return_addr = i64_type.const_int(address + INSTRUCTION_SIZE as u64, false);
            codegen.write_general_reg(instr.rd(), return_addr.into());

            let block_id = {
                let result = env_call!(codegen, RuntimeFunction::GetBlockId, [source]);
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
            let address = codegen.base_plus_offset(instr, "sh_addr");
            let target = codegen.read_general_reg(instr.rt()).into_int_value();
            let target = codegen
                .builder
                .build_int_truncate(target, i16_type, "sh_trunc");
            codegen.write_memory(address, target.into());
        }

        Mnenomic::Lwr => {
            // Loads a portion of a word beginning at memory address (base + offset), stores 1-4 bytes in low-order portion of rt
            let address = codegen.base_plus_offset(instr, "lwr_addr");

            let shift = {
                let three = i64_type.const_int(3, false);
                let eight = i64_type.const_int(8, false);

                let xor = codegen.builder.build_xor(address, three, "lwr_shift_xor");
                let and = codegen.builder.build_and(xor, three, "lwr_shift_and");
                codegen.builder.build_int_mul(eight, and, "lwr_shift_mul")
            };

            let mask = {
                let max = i64_type.const_all_ones();
                let mask = codegen
                    .builder
                    .build_right_shift(max, shift, false, "lwr_mask");
                codegen.builder.build_not(mask, "lwr_mask_not")
            };

            let data = {
                let not_three = i64_type.const_int(!3, false);
                let addr = codegen
                    .builder
                    .build_and(address, not_three, "lwr_data_and");
                let data = codegen.read_memory(i64_type, addr).into_int_value();

                codegen
                    .builder
                    .build_right_shift(data, shift, false, "lwr_data_shift")
            };

            let result = {
                let reg = codegen.read_general_reg(instr.rt()).into_int_value();
                let anded = codegen.builder.build_and(reg, mask, "lwr_result_and");
                codegen.builder.build_or(anded, data, "lwr_result_or")
            };

            codegen.write_general_reg(instr.rt(), result.into());
        }

        Mnenomic::Sdl => {
            // Loads a portion of rt, stores 1-8 bytes in high-order portion of memory address (base + offset)
            let address = codegen.base_plus_offset(instr, "sdl_addr");
            let data = {
                let addr = codegen.builder.build_and(
                    address,
                    i64_type.const_int(!7, false),
                    "sdl_data_and",
                );
                codegen.read_memory(i64_type, addr).into_int_value()
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
                let reg = codegen.read_general_reg(instr.rt()).into_int_value();
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

            codegen.write_general_reg(instr.rt(), result.into());
        }

        Mnenomic::Sdr => {
            // Loads a portion of rt, stores 1-8 bytes in low-order portion of memory address (base + offset)
            let address = codegen.base_plus_offset(instr, "sdr_addr");
            let data = {
                let addr = codegen.builder.build_and(
                    address,
                    i64_type.const_int(!7, false),
                    "sdr_data_and",
                );
                codegen.read_memory(i64_type, addr).into_int_value()
            };

            let shift = {
                let seven = i64_type.const_int(7, false);
                let eight = i64_type.const_int(8, false);

                let xor = codegen.builder.build_xor(address, seven, "sdr_shift_xor");
                let and = codegen.builder.build_and(xor, seven, "sdr_shift_and");
                codegen.builder.build_int_mul(eight, and, "sdr_shift_mul")
            };

            let old_register = {
                let reg = codegen.read_general_reg(instr.rt()).into_int_value();
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

            codegen.write_general_reg(instr.rt(), result.into());
        }

        Mnenomic::Sw => {
            // Stores word from rt, to memory address (base + offset)
            let address = codegen.base_plus_offset(instr, "sw_addr");
            let target = {
                let target = codegen.read_general_reg(instr.rt()).into_int_value();
                codegen
                    .builder
                    .build_int_truncate(target, i32_type, "sw_trunc")
            };
            codegen.write_memory(address, target.into());
        }

        Mnenomic::Sb => {
            // Stores least-significant byte from rt, to memory address (base + offset)
            let target = codegen.read_general_reg(instr.rt()).into_int_value();
            // TODO: is this the least significant byte?
            let target = codegen
                .builder
                .build_int_truncate(target, i8_type, "sb_trunc");

            let address = codegen.base_plus_offset(instr, "sb_addr");
            codegen.write_memory(address, target.into());
        }

        Mnenomic::Sd => {
            // Stores doubleword from rt, to memory address (base + offset)
            let target = codegen.read_general_reg(instr.rt()).into_int_value();
            let address = codegen.base_plus_offset(instr, "sd_addr");
            codegen.write_memory(address, target.into());
        }

        Mnenomic::Ld => {
            // Loads doubleword stored at memory address (base + offset), stores doubleword in rt
            let address = codegen.base_plus_offset(instr, "ld_addr");
            let value = codegen.read_memory(i64_type, address);
            codegen.write_general_reg(instr.rt(), value);
        }

        Mnenomic::Lbu => {
            // Loads byte stored at memory address (base + offset), stores zero-extended byte in rt
            let address = codegen.base_plus_offset(instr, "lbu_addr");
            let value = {
                let value = codegen.read_memory(i8_type, address);
                codegen.zero_extend_to_i64(value.into_int_value())
            };

            codegen.write_general_reg(instr.rt(), value.into());
        }

        Mnenomic::Lb => {
            // Loads byte stored at memory address (base + offset), stores sign-extended byte in rt
            let address = codegen.base_plus_offset(instr, "lb_addr");
            let value = {
                let value = codegen.read_memory(i8_type, address);
                codegen.sign_extend_to_i64(value.into_int_value())
            };
            codegen.write_general_reg(instr.rt(), value.into());
        }

        Mnenomic::Lw => {
            // Loads word stored at memory address (base + offset), stores sign-extended word in rt
            let address = codegen.base_plus_offset(instr, "lw_addr");
            let value = {
                let value = codegen.read_memory(i32_type, address);
                codegen.sign_extend_to_i64(value.into_int_value())
            };
            codegen.write_general_reg(instr.rt(), value.into());
        }

        Mnenomic::Lui => {
            // 16-bit immediate is shifted left 16 bits using trailing zeros, result placed in rt
            let immediate =
                codegen.zero_extend_to_i64(i16_type.const_int(instr.immediate() as _, false));
            let shift_amount = i64_type.const_int(16, false);
            let result = codegen
                .builder
                .build_left_shift(immediate, shift_amount, "lui_res");
            codegen.write_general_reg(instr.rt(), result.into());
        }

        Mnenomic::Addiu => {
            // Add sign-extended 16bit immediate and rs, store result in rt
            let immediate =
                codegen.sign_extend_to_i64(i16_type.const_int(instr.immediate() as _, true));
            let source = codegen.read_general_reg(instr.rs()).into_int_value();
            let result = codegen
                .builder
                .build_int_add(source, immediate, "addiu_res");
            codegen.write_general_reg(instr.rt(), result.into());
        }

        Mnenomic::Addi => {
            // Add sign-extended 16bit immediate and rs, store result in rt
            let immediate =
                codegen.sign_extend_to_i64(i16_type.const_int(instr.immediate() as _, true));
            let source = codegen.read_general_reg(instr.rs()).into_int_value();
            let result = codegen.builder.build_int_add(source, immediate, "addi_res");
            codegen.write_general_reg(instr.rt(), result.into());
        }

        Mnenomic::Add => {
            // Add rs and rt, store result in rd
            let source = codegen.read_general_reg(instr.rs()).into_int_value();
            let target = codegen.read_general_reg(instr.rt()).into_int_value();
            let result = codegen.builder.build_int_add(source, target, "add_res");
            codegen.write_general_reg(instr.rd(), result.into());
        }

        Mnenomic::Andi => {
            // AND rs with zero-extended immediate, store result in rt
            let source = codegen.read_general_reg(instr.rs()).into_int_value();
            let immediate =
                codegen.zero_extend_to_i64(i16_type.const_int(instr.immediate() as _, false));
            let result = codegen.builder.build_and(source, immediate, "andi_res");
            codegen.write_general_reg(instr.rt(), result.into());
        }

        Mnenomic::Ori => {
            // OR rs and zero-extended immediate, store result in rt
            let source = codegen.read_general_reg(instr.rs()).into_int_value();
            let immediate =
                codegen.zero_extend_to_i64(i16_type.const_int(instr.immediate() as _, false));
            let result = codegen.builder.build_or(source, immediate, "ori_res");
            codegen.write_general_reg(instr.rt(), result.into());
        }

        Mnenomic::Daddiu => {
            // Add sign-extended 16bit immediate and rs, store result in rt
            let immediate =
                codegen.sign_extend_to_i64(i16_type.const_int(instr.immediate() as _, true));
            let source = codegen.read_general_reg(instr.rs()).into_int_value();
            let result = codegen
                .builder
                .build_int_add(source, immediate, "daddiu_res");

            codegen.write_general_reg(instr.rt(), result.into());
        }

        Mnenomic::Slt => {
            // If signed rs is less than signed rt, store one in rd, otherwise store zero
            let source = codegen.read_general_reg(instr.rs()).into_int_value();
            let target = codegen.read_general_reg(instr.rt()).into_int_value();

            let result = {
                let cmp =
                    codegen
                        .builder
                        .build_int_compare(IntPredicate::SLT, source, target, "slt_cmp");

                codegen.builder.build_int_z_extend(cmp, i64_type, "slt_res")
            };

            codegen.write_general_reg(instr.rd(), result.into());
        }

        Mnenomic::Slti => {
            // If signed rs is less than sign-extended immediate, store one in rd, otherwise store zero
            let immediate =
                codegen.sign_extend_to_i64(i16_type.const_int(instr.immediate() as _, true));
            let source = codegen.read_general_reg(instr.rs()).into_int_value();

            let result = {
                let cmp = codegen.builder.build_int_compare(
                    IntPredicate::SLT,
                    source,
                    immediate,
                    "slti_cmp",
                );

                codegen
                    .builder
                    .build_int_z_extend(cmp, i64_type, "slti_res")
            };

            codegen.write_general_reg(instr.rt(), result.into());
        }

        Mnenomic::Sltiu => {
            // If unsigned rs is less than sign-extended immediate, store one in rt, otherwise store zero
            let immediate =
                codegen.sign_extend_to_i64(i16_type.const_int(instr.immediate() as _, true));
            let source = codegen.read_general_reg(instr.rs()).into_int_value();
            let cmp = codegen.builder.build_int_compare(
                IntPredicate::ULT,
                source,
                immediate,
                "sltiu_cmp",
            );

            // Convert the comparison to a numeric value.
            let result = codegen
                .builder
                .build_int_z_extend(cmp, i64_type, "sltiu_res");

            codegen.write_general_reg(instr.rt(), result.into());
        }

        Mnenomic::Subu => {
            // Subtract rt from rs, store result in rd
            let source = codegen.read_general_reg(instr.rs()).into_int_value();
            let target = codegen.read_general_reg(instr.rt()).into_int_value();
            let result = codegen.builder.build_int_sub(target, source, "subu_res");
            codegen.write_general_reg(instr.rd(), result.into());
        }

        Mnenomic::Cache => {
            // Flush instruction or data cache at address (base + offset) to RAM
            // TODO: Implement
        }

        Mnenomic::Beq => {
            // If rs equals rt, branch to address
            let source = codegen.read_general_reg(instr.rs()).into_int_value();
            let target = codegen.read_general_reg(instr.rt()).into_int_value();

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
            let source = codegen.read_general_reg(instr.rs()).into_int_value();
            let target = codegen.read_general_reg(instr.rt()).into_int_value();

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

        Mnenomic::Bltz => {
            // If rs is less than zero, branch to address (delay slot + offset)
            let source = codegen.read_general_reg(instr.rs()).into_int_value();
            let zero = i64_type.const_int(0, false);

            let cmp =
                codegen
                    .builder
                    .build_int_compare(IntPredicate::SLT, source, zero, "bltz_cmp");

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

        Mnenomic::Beql => {
            // If rs equals rt, branch to address (delay slot + offset), otherwise discard delay slot instruction
            let source = codegen.read_general_reg(instr.rs()).into_int_value();
            let target = codegen.read_general_reg(instr.rt()).into_int_value();

            let cmp =
                codegen
                    .builder
                    .build_int_compare(IntPredicate::EQ, source, target, "beql_cmp");

            let then_block = labels
                .get(&(instr.try_resolve_static_jump(address as _).unwrap() as _))
                .unwrap()
                .basic_block;
            let else_block = labels
                .get(&(address + (2 * INSTRUCTION_SIZE) as u64))
                .unwrap()
                .basic_block;

            codegen
                .builder
                .build_conditional_branch(cmp, then_block, else_block);
        }

        Mnenomic::Bgezal => {
            // If rs is greater than or equal to zero, branch to address (delay slot + offset) and store next address to r31 (ra)
            let source = codegen.read_general_reg(instr.rs()).into_int_value();
            let zero = i64_type.const_int(0, false);
            let next_addr = address + INSTRUCTION_SIZE as u64;

            let cmp =
                codegen
                    .builder
                    .build_int_compare(IntPredicate::SGE, source, zero, "bgezal_cmp");

            let else_block = labels.get(&(next_addr)).unwrap().basic_block;

            let then_real_block = labels
                .get(&(instr.try_resolve_static_jump(address as _).unwrap() as _))
                .unwrap()
                .basic_block;

            // Create a new block that sets r31 and branches to the real block
            let curr_block = codegen.builder.get_insert_block().unwrap();
            let then_block = codegen
                .context
                .append_basic_block(curr_block.get_parent().unwrap(), "bgezal_then");

            codegen.builder.position_at_end(then_block);
            codegen.write_general_reg(
                register::GeneralPurpose::Ra,
                i64_type.const_int(next_addr as _, false).into(),
            );
            codegen.builder.build_unconditional_branch(then_real_block);
            codegen.builder.position_at_end(curr_block);

            codegen
                .builder
                .build_conditional_branch(cmp, then_real_block, else_block);
        }

        Mnenomic::Bnel => {
            // If rs is not equal to rt, branch to address (delay slot + offset), otherwise discard delay slot instruction
            let source = codegen.read_general_reg(instr.rs()).into_int_value();
            let target = codegen.read_general_reg(instr.rt()).into_int_value();

            let cmp =
                codegen
                    .builder
                    .build_int_compare(IntPredicate::NE, source, target, "bnel_cmp");

            let then_block = labels
                .get(&(instr.try_resolve_static_jump(address as _).unwrap() as _))
                .unwrap()
                .basic_block;
            let else_block = labels
                .get(&(address + (2 * INSTRUCTION_SIZE) as u64))
                .unwrap()
                .basic_block;

            codegen
                .builder
                .build_conditional_branch(cmp, then_block, else_block);
        }

        Mnenomic::Blezl => {
            // If rs is less than or equal to zero, branch to address (delay slot + offset), otherwise discard delay slot instruction
            let source = codegen.read_general_reg(instr.rs()).into_int_value();
            let zero = i64_type.const_int(0, false);

            let cmp =
                codegen
                    .builder
                    .build_int_compare(IntPredicate::SLE, source, zero, "blezl_cmp");

            let then_block = labels
                .get(&(instr.try_resolve_static_jump(address as _).unwrap() as _))
                .unwrap()
                .basic_block;
            let else_block = labels
                .get(&(address + (2 * INSTRUCTION_SIZE) as u64))
                .unwrap()
                .basic_block;

            codegen
                .builder
                .build_conditional_branch(cmp, then_block, else_block);
        }

        Mnenomic::Bgezl => {
            // If rs is greater than or equal to zero, branch to address (delay slot + offset), otherwise discard delay slot instruction
            let source = codegen.read_general_reg(instr.rs()).into_int_value();
            let zero = i64_type.const_int(0, false);

            let cmp =
                codegen
                    .builder
                    .build_int_compare(IntPredicate::SGE, source, zero, "blezl_cmp");

            let then_block = labels
                .get(&(instr.try_resolve_static_jump(address as _).unwrap() as _))
                .unwrap()
                .basic_block;
            let else_block = labels
                .get(&(address + (2 * INSTRUCTION_SIZE) as u64))
                .unwrap()
                .basic_block;

            codegen
                .builder
                .build_conditional_branch(cmp, then_block, else_block);
        }

        Mnenomic::Bgez => {
            // If rs is greater than or equal to zero, branch to address (delay slot + offset)
            let source = codegen.read_general_reg(instr.rs()).into_int_value();
            let zero = i64_type.const_int(0, false);

            let cmp =
                codegen
                    .builder
                    .build_int_compare(IntPredicate::SGE, source, zero, "bgez_cmp");

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
