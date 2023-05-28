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
        Mnenomic::Mtc0 => {
            // Copy contents of GPR rt, to CP0's coprocessor register rd
            let target = codegen.load_gpr(instr.rt()).into_int_value();
            codegen.store_cp0_reg(instr.rd(), target.into());
        }

        Mnenomic::Jal => {
            // Jump to target address, stores return address in r31 (ra)
            let target = instr.try_resolve_static_jump(address as _).unwrap();
            let target_block = labels.get(&(target as _)).unwrap().basic_block;

            // Store the return address
            let return_address = codegen
                .build_i64(address + INSTRUCTION_SIZE as u64)
                .into_int_value();
            codegen.store_gpr(31u32, return_address.into());

            // Jump to the target
            println!("jumping from {address:#x} to {:x}", target);
            codegen.builder.build_unconditional_branch(target_block);
        }

        Mnenomic::And => {
            // AND rs with rt, store result in rd
            let source = codegen.load_gpr(instr.rs()).into_int_value();
            let target = codegen.load_gpr(instr.rt()).into_int_value();
            let result = codegen.builder.build_and(source, target, "and_result");
            codegen.store_gpr(instr.rd(), result.into());
        }

        Mnenomic::Xor => {
            // XOR rs with rt, store result in rd
            let source = codegen.load_gpr(instr.rs()).into_int_value();
            let target = codegen.load_gpr(instr.rt()).into_int_value();
            let result = codegen.builder.build_xor(source, target, "xor_result");
            codegen.store_gpr(instr.rd(), result.into());
        }

        Mnenomic::Addu => {
            // Add rs and rt, store result in rd
            let source = codegen.load_gpr(instr.rs()).into_int_value();
            let target = codegen.load_gpr(instr.rt()).into_int_value();
            let result = codegen.builder.build_int_add(source, target, "addu_result");
            codegen.store_gpr(instr.rd(), result.into());
        }

        Mnenomic::Or => {
            // OR rs and rt, store result in rd
            let source = codegen.load_gpr(instr.rs()).into_int_value();
            let target = codegen.load_gpr(instr.rt()).into_int_value();
            let result = codegen.builder.build_or(source, target, "or_result");
            codegen.store_gpr(instr.rd(), result.into());
        }

        Mnenomic::Daddu => {
            // Add rs and rt, store result in rd
            let source = codegen.load_gpr(instr.rs()).into_int_value();
            let target = codegen.load_gpr(instr.rt()).into_int_value();
            let result = codegen
                .builder
                .build_int_add(source, target, "daddu_result");
            codegen.store_gpr(instr.rd(), result.into());
        }

        Mnenomic::Dsll => {
            // Shift rt left by sa bits, store result in rd (64-bits)
            let shift = i64_type.const_int(instr.sa() as _, false);
            let target = codegen.load_gpr(instr.rt()).into_int_value();
            let result = codegen
                .builder
                .build_left_shift(target, shift, "dsll_result");
            codegen.store_gpr(instr.rd(), result.into());
        }

        Mnenomic::Sltu => {
            // If unsigned rs is less than unsigned rt, store one in rd, otherwise store zero.
            let source = codegen.load_gpr(instr.rs()).into_int_value();
            let target = codegen.load_gpr(instr.rt()).into_int_value();

            let cmp =
                codegen
                    .builder
                    .build_int_compare(IntPredicate::ULT, source, target, "sltu_cmp");

            // Convert the comparison to a numeric value.
            let result = codegen
                .builder
                .build_int_z_extend(cmp, i64_type, "sltu_result");
            codegen.store_gpr(instr.rd(), result.into());
        }

        Mnenomic::Sll => {
            // Shift rt left by sa bits, store result in rd (32-bits)
            let shift = i64_type.const_int(instr.sa() as _, false);
            let target = codegen.load_gpr(instr.rt()).into_int_value();
            let result = codegen.builder.build_left_shift(target, shift, "sll_shift");

            // Truncate the result to 32-bits.
            let result = codegen
                .builder
                .build_int_truncate(result, i32_type, "sll_truncate");

            codegen.store_gpr(instr.rd(), result.into());
        }

        Mnenomic::Sllv => {
            // Shift rt left by rs (limited to 31), store result in rd
            let target = codegen.load_gpr(instr.rt()).into_int_value();
            let source = codegen.load_gpr(instr.rs()).into_int_value();

            // Limit the shift to 31 bits.
            let shift = codegen
                .builder
                .build_int_truncate(source, i32_type, "sllv_truncate");
            let shift = codegen
                .builder
                .build_int_z_extend(shift, i64_type, "sllv_extend");

            let result = codegen
                .builder
                .build_left_shift(target, shift, "sllv_shift");
            codegen.store_gpr(instr.rd(), result.into());
        }

        Mnenomic::Dsll32 => {
            // Shift rt left by (32 + sa) bits, store result in rd
            let target = codegen.load_gpr(instr.rt()).into_int_value();
            let shift = i64_type.const_int((instr.sa() + 32) as _, false);
            let result = codegen
                .builder
                .build_left_shift(target, shift, "dsll32_shift");
            codegen.store_gpr(instr.rd(), result.into());
        }

        Mnenomic::Dsrl32 => {
            // Shift rt right by (32 + sa) bits, store sign-extended result in rd
            let target = codegen.load_gpr(instr.rt()).into_int_value();
            let shift = i64_type.const_int((instr.sa() + 32) as _, false);
            let result = codegen
                .builder
                .build_right_shift(target, shift, true, "dsrl32_shift");
            codegen.store_gpr(instr.rd(), result.into());
        }

        Mnenomic::Srl => {
            // Shift rt right by sa bits, store sign-extended result in rd
            let target = codegen.load_gpr(instr.rt()).into_int_value();
            let shift = i64_type.const_int(instr.sa() as _, false);
            let result = codegen
                .builder
                .build_right_shift(target, shift, true, "srl_shift");
            codegen.store_gpr(instr.rd(), result.into());
        }

        Mnenomic::Srlv => {
            // Shift rt right by rs (limited to 31), store sign-extended result in rd
            let target = codegen.load_gpr(instr.rt()).into_int_value();
            let source = codegen.load_gpr(instr.rs()).into_int_value();

            // Limit the shift to 31 bits.
            let shift = codegen
                .builder
                .build_int_truncate(source, i32_type, "srlv_truncate");
            let shift = codegen
                .builder
                .build_int_z_extend(shift, i64_type, "srlv_extend");

            let result = codegen
                .builder
                .build_right_shift(target, shift, true, "srlv_shift");
            codegen.store_gpr(instr.rd(), result.into());
        }

        Mnenomic::Multu => {
            // Multiply unsigned rs by unsigned rt, store low-order word of result in register LO and high-order word in HI
            // TODO: Implement
        }

        Mnenomic::Jr => {
            // Jump to address stored in rs
            let source = codegen.load_gpr(instr.rs()).into_int_value();

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
            let source = codegen.load_gpr(instr.rs()).into_int_value();

            // Store the return address
            let return_addr = i64_type.const_int(address + INSTRUCTION_SIZE as u64, false);
            codegen.store_gpr(instr.rd(), return_addr.into());

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
            let target = codegen.load_gpr(instr.rt()).into_int_value();
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
                let reg = codegen.load_gpr(instr.rt()).into_int_value();
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

            codegen.store_gpr(instr.rt(), result.into());
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
                let reg = codegen.load_gpr(instr.rt()).into_int_value();
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

            codegen.store_gpr(instr.rt(), result.into());
        }

        Mnenomic::Sw => {
            // Stores word from rt, to memory address (base + offset)
            let address = codegen.base_plus_offset(instr, "sw_address");
            let target = {
                let target = codegen.load_gpr(instr.rt()).into_int_value();
                codegen
                    .builder
                    .build_int_truncate(target, i32_type, "sb_truncate")
            };
            codegen.store_memory(address, target.into());
        }

        Mnenomic::Sb => {
            // Stores least-significant byte from rt, to memory address (base + offset)
            let target = codegen.load_gpr(instr.rt()).into_int_value();
            // TODO: is this the least significant byte?
            let target = codegen
                .builder
                .build_int_truncate(target, i8_type, "sb_truncate");

            let address = codegen.base_plus_offset(instr, "sb_address");
            codegen.store_memory(address, target.into());
        }

        Mnenomic::Sd => {
            // Stores doubleword from rt, to memory address (base + offset)
            let target = codegen.load_gpr(instr.rt()).into_int_value();
            let address = codegen.base_plus_offset(instr, "sd_address");
            codegen.store_memory(address, target.into());
        }

        Mnenomic::Ld => {
            // Loads doubleword stored at memory address (base + offset), stores doubleword in rt
            let address = codegen.base_plus_offset(instr, "ld_address");
            let value = codegen.load_memory(i64_type, address);
            codegen.store_gpr(instr.rt(), value);
        }

        Mnenomic::Lbu => {
            // Loads byte stored at memory address (base + offset), stores zero-extended byte in rt
            let address = codegen.base_plus_offset(instr, "lbu_address");
            let value = {
                let value = codegen.load_memory(i8_type, address);
                codegen.zero_extend_to_i64(value.into_int_value())
            };

            codegen.store_gpr(instr.rt(), value.into());
        }

        Mnenomic::Lb => {
            // Loads byte stored at memory address (base + offset), stores sign-extended byte in rt
            let address = codegen.base_plus_offset(instr, "lb_address");
            let value = {
                let value = codegen.load_memory(i8_type, address);
                codegen.sign_extend_to_i64(value.into_int_value())
            };
            codegen.store_gpr(instr.rt(), value.into());
        }

        Mnenomic::Lw => {
            // Loads word stored at memory address (base + offset), stores sign-extended word in rt
            let address = codegen.base_plus_offset(instr, "lw_address");
            let value = {
                let value = codegen.load_memory(i32_type, address);
                codegen.sign_extend_to_i64(value.into_int_value())
            };
            codegen.store_gpr(instr.rt(), value.into());
        }

        Mnenomic::Lui => {
            // 16-bit immediate is shifted left 16 bits using trailing zeros, result placed in rt
            let immediate =
                codegen.zero_extend_to_i64(i16_type.const_int(instr.immediate() as _, false));
            let shift_amount = i64_type.const_int(16, false);
            let result = codegen
                .builder
                .build_left_shift(immediate, shift_amount, "lui_result");
            codegen.store_gpr(instr.rt(), result.into());
        }

        Mnenomic::Addiu => {
            // Add sign-extended 16bit immediate and rs, store result in rt
            let immediate =
                codegen.sign_extend_to_i64(i16_type.const_int(instr.immediate() as _, true));
            let source = codegen.load_gpr(instr.rs()).into_int_value();
            let result = codegen
                .builder
                .build_int_add(source, immediate, "addiu_result");
            codegen.store_gpr(instr.rt(), result.into());
        }

        Mnenomic::Addi => {
            // Add sign-extended 16bit immediate and rs, store result in rt
            let immediate =
                codegen.sign_extend_to_i64(i16_type.const_int(instr.immediate() as _, true));
            let source = codegen.load_gpr(instr.rs()).into_int_value();
            let result = codegen
                .builder
                .build_int_add(source, immediate, "addi_result");
            codegen.store_gpr(instr.rt(), result.into());
        }

        Mnenomic::Add => {
            // Add rs and rt, store result in rd
            let source = codegen.load_gpr(instr.rs()).into_int_value();
            let target = codegen.load_gpr(instr.rt()).into_int_value();
            let result = codegen.builder.build_int_add(source, target, "add_result");
            codegen.store_gpr(instr.rd(), result.into());
        }

        Mnenomic::Andi => {
            // AND rs with zero-extended immediate, store result in rt
            let source = codegen.load_gpr(instr.rs()).into_int_value();
            let immediate =
                codegen.zero_extend_to_i64(i16_type.const_int(instr.immediate() as _, false));
            let result = codegen.builder.build_and(source, immediate, "andi_result");
            codegen.store_gpr(instr.rt(), result.into());
        }

        Mnenomic::Ori => {
            // OR rs and zero-extended immediate, store result in rt
            let source = codegen.load_gpr(instr.rs()).into_int_value();
            let immediate =
                codegen.zero_extend_to_i64(i16_type.const_int(instr.immediate() as _, false));
            let result = codegen.builder.build_or(source, immediate, "ori_result");
            codegen.store_gpr(instr.rt(), result.into());
        }

        Mnenomic::Daddiu => {
            // Add sign-extended 16bit immediate and rs, store result in rt
            let immediate =
                codegen.sign_extend_to_i64(i16_type.const_int(instr.immediate() as _, true));
            let source = codegen.load_gpr(instr.rs()).into_int_value();
            let result = codegen
                .builder
                .build_int_add(source, immediate, "daddiu_result");

            codegen.store_gpr(instr.rt(), result.into());
        }

        Mnenomic::Slt => {
            // If signed rs is less than signed rt, store one in rd, otherwise store zero
            let source = codegen.load_gpr(instr.rs()).into_int_value();
            let target = codegen.load_gpr(instr.rt()).into_int_value();

            let result = {
                let cmp =
                    codegen
                        .builder
                        .build_int_compare(IntPredicate::SLT, source, target, "slt_cmp");

                codegen
                    .builder
                    .build_int_z_extend(cmp, i64_type, "slt_result")
            };

            codegen.store_gpr(instr.rd(), result.into());
        }

        Mnenomic::Slti => {
            // If signed rs is less than sign-extended immediate, store one in rd, otherwise store zero
            let immediate =
                codegen.sign_extend_to_i64(i16_type.const_int(instr.immediate() as _, true));
            let source = codegen.load_gpr(instr.rs()).into_int_value();

            let result = {
                let cmp = codegen.builder.build_int_compare(
                    IntPredicate::SLT,
                    source,
                    immediate,
                    "slti_cmp",
                );

                codegen
                    .builder
                    .build_int_z_extend(cmp, i64_type, "slti_result")
            };

            codegen.store_gpr(instr.rt(), result.into());
        }

        Mnenomic::Sltiu => {
            // If unsigned rs is less than sign-extended immediate, store one in rt, otherwise store zero
            let immediate =
                codegen.sign_extend_to_i64(i16_type.const_int(instr.immediate() as _, true));
            let source = codegen.load_gpr(instr.rs()).into_int_value();
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

            codegen.store_gpr(instr.rt(), result.into());
        }

        Mnenomic::Subu => {
            // Subtract rt from rs, store result in rd
            let source = codegen.load_gpr(instr.rs()).into_int_value();
            let target = codegen.load_gpr(instr.rt()).into_int_value();
            let result = codegen.builder.build_int_sub(target, source, "subu_result");
            codegen.store_gpr(instr.rd(), result.into());
        }

        Mnenomic::Cache => {
            // Flush instruction or data cache at address (base + offset) to RAM
            // TODO: Implement
        }

        Mnenomic::Beq => {
            // If rs equals rt, branch to address
            let source = codegen.load_gpr(instr.rs()).into_int_value();
            let target = codegen.load_gpr(instr.rt()).into_int_value();

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
            let source = codegen.load_gpr(instr.rs()).into_int_value();
            let target = codegen.load_gpr(instr.rt()).into_int_value();

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
            let source = codegen.load_gpr(instr.rs()).into_int_value();
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
            let source = codegen.load_gpr(instr.rs()).into_int_value();
            let target = codegen.load_gpr(instr.rt()).into_int_value();

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
            let source = codegen.load_gpr(instr.rs()).into_int_value();
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
            codegen.store_gpr(31_u8, i64_type.const_int(next_addr as _, false).into());
            codegen.builder.build_unconditional_branch(then_real_block);
            codegen.builder.position_at_end(curr_block);

            codegen
                .builder
                .build_conditional_branch(cmp, then_real_block, else_block);
        }

        Mnenomic::Bnel => {
            // If rs is not equal to rt, branch to address (delay slot + offset), otherwise discard delay slot instruction
            let source = codegen.load_gpr(instr.rs()).into_int_value();
            let target = codegen.load_gpr(instr.rt()).into_int_value();

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

        _ => todo!("instruction {} at {address:#x}", instr.mnemonic().name()),
    }
}
