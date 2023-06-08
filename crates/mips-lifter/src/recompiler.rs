use crate::codegen::CodeGen;
use inkwell::IntPredicate;
use mips_decomp::{
    instruction::{Mnenomic, ParsedInstruction},
    register, INSTRUCTION_SIZE,
};

fn stub(codegen: &CodeGen, name: &str) {
    let output = format!("STUB: {name} instruction not executed");
    let storage_name = format!("{name}_stub");
    codegen.print_constant_string(&output, &storage_name);
}

pub fn recompile_instruction(codegen: &CodeGen, instr: &ParsedInstruction, pc: u64) {
    let mnemonic = instr.mnemonic();
    let next_pc =
        pc + ((if instr.discards_delay_slot() { 2 } else { 1 }) * INSTRUCTION_SIZE) as u64;

    let i8_type = codegen.context.i8_type();
    let i16_type = codegen.context.i16_type();
    let i32_type = codegen.context.i32_type();
    let i64_type = codegen.context.i64_type();
    let i128_type = codegen.context.i128_type();

    match mnemonic {
        Mnenomic::Tltiu => {
            // If unsigned rs is less than sign-extended immediate, cause a trap exception
            stub(codegen, "tltiu");
        }

        Mnenomic::Tlti => {
            // If signed rs is less than sign-extended immediate, cause a trap exception
            stub(codegen, "tlti");
        }

        Mnenomic::Teqi => {
            // If rs equals sign-extended immediate, cause a trap exception
            stub(codegen, "teqi");
        }

        Mnenomic::Tgei => {
            // If signed rs is greater than or equal to sign-extended immediate, cause a trap exception
            stub(codegen, "tgei");
        }

        Mnenomic::Tnei => {
            // If rs does not equal sign-extended immediate, cause a trap exception
            stub(codegen, "tnei");
        }

        Mnenomic::Tge => {
            // If signed rs is greater than or equal to signed rt, cause a trap exception
            stub(codegen, "tge");
        }

        Mnenomic::Tgeu => {
            // If unsigned rs is greater than or equals to unsigned rt, cause a trap exception
            stub(codegen, "tgeu");
        }

        Mnenomic::Tgeiu => {
            // If unsigned rs is greater than or equal to sign-extended immediate, cause a trap exception
            stub(codegen, "tgeiu");
        }

        Mnenomic::Teq => {
            // If rs equals rt, cause a trap exception
            stub(codegen, "teq");
        }

        Mnenomic::Tlt => {
            // If signed rs is less than signed rt, cause a trap exception
            stub(codegen, "tlt");
        }

        Mnenomic::Tltu => {
            // If unsigned rs is less than unsigned rt, cause a trap exception
            stub(codegen, "tltu");
        }

        Mnenomic::Tne => {
            // If rs does not equal rt, cause a trap exception
            stub(codegen, "tne");
        }

        Mnenomic::AddFmt => {
            stub(codegen, "addfmt");
        }

        Mnenomic::Syscall => {
            // Causes system call exception
            stub(codegen, "syscall");
        }

        Mnenomic::Break => {
            // Causes breakpoint exception
            stub(codegen, "break");
        }

        Mnenomic::Copz => {
            // Perform coprocessor operation
            stub(codegen, "copz");
        }

        Mnenomic::Bczfl => {
            // If CPz's CpCond is false, branch to address (delay slot + offset), otherwise discard delay slot instruction
            stub(codegen, "bczfl");
        }

        Mnenomic::Bczf => {
            // If CPz's CpCond is false, branch to address (delay slot + offset)
            stub(codegen, "bczf");
        }

        Mnenomic::Bczt => {
            // If CPz's CpCond is true, branch to address (delay slot + offset)
            stub(codegen, "bczt");
        }

        Mnenomic::Bcztl => {
            // If CPz's CpCond is true, branch to address (delay slot + offset), otherwise discard delay slot instruction
            stub(codegen, "bcztl");
        }

        Mnenomic::Swcz => {
            // Copies word from CPz, to memory address (base + offset)
            stub(codegen, "swcz");
        }

        Mnenomic::Sdcz => {
            // Copies doubleword from CPz, to memory address (base + offset)
            stub(codegen, "sdcz");
        }

        Mnenomic::Ldcz => {
            // Copies doubleword stored at memory address (base + offset), to CPz
            stub(codegen, "ldcz");
        }

        Mnenomic::Cache => {
            // Flush instruction or data cache at address (base + offset) to RAM
            stub(codegen, "cache");
        }

        Mnenomic::Ldl => {
            // Loads a portion of a doubleword beginning at memory address (base + offset), stores 1-8 bytes in high-order portion of rt
            stub(codegen, "ldl");
        }

        Mnenomic::Ldr => {
            // Loads a portion of a doubleword beginning at memory address (base + offset), stores 1-8 bytes in low-order portion of rt
            stub(codegen, "ldr");
        }

        Mnenomic::Swl => {
            // Loads a portion of rt, stores 1-4 bytes in high-order portion of memory address (base + offset)
            stub(codegen, "swl");
        }

        Mnenomic::Swr => {
            // Loads a portion of rt, stores 1-4 bytes in low-order portion of memory address (base + offset)
            stub(codegen, "swr");
        }

        Mnenomic::Lwl => {
            // Loads a portion of a word beginning at memory address (base + offset), stores 1-4 bytes in high-order portion of rt
            stub(codegen, "lwl");
        }

        Mnenomic::Sync => {
            // Executed as NOP on the VR4300
        }

        Mnenomic::Mtlo => {
            // Copy contents of rs to register LO
            let target = codegen.read_general_reg(instr.rs());
            codegen.write_special_reg(register::Special::Lo, target);
        }

        Mnenomic::Mthi => {
            // Copy contents of rs to register HI
            let target = codegen.read_general_reg(instr.rs());
            codegen.write_special_reg(register::Special::Hi, target);
        }

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

        Mnenomic::Dmtc0 => {
            // Copy doubleword contents of GPR rt, to CPz coprocessor register rd
            // TODO: dont assume cp0? Kinda weird zero is in the name though...
            let target = codegen.read_general_reg(instr.rt());
            codegen.write_cp0_reg(instr.rd(), target);
        }

        Mnenomic::Dmfc0 => {
            // Copy doubleword contents of CPz coprocessor register rd, to GPR rt
            // TODO: dont assume cp0? Kinda weird zero is in the name though...
            let destination = codegen.read_cp0_reg(instr.rd());
            codegen.write_general_reg(instr.rt(), destination);
        }

        Mnenomic::Cfcz => {
            // Copy contents of CPz control register rd, to GPR rt
            // TODO: dont assume cp0
            let value = codegen.read_cp0_reg(instr.rd());
            codegen.write_general_reg(instr.rt(), value);
        }

        Mnenomic::Ctcz => {
            // Copy contents of GPR rt, to CPz control register rd
            // TODO: dont assume cp0
            let value = codegen.read_general_reg(instr.rt());
            codegen.write_cp0_reg(instr.rd(), value);
        }

        Mnenomic::Mfcz => {
            // Copy contents of CPz's coprocessor register rd, to GPR rt
            // TODO: dont assume cp0
            let value = codegen.read_cp0_reg(instr.rd());
            codegen.write_general_reg(instr.rt(), value);
        }

        Mnenomic::Mtcz => {
            // Copy contents of GPR rt, to CPz's coprocessor register rd
            // TODO: dont assume cp0
            let value = codegen.read_general_reg(instr.rt());
            codegen.write_cp0_reg(instr.rd(), value);
        }

        Mnenomic::Lwcz => {
            // Copies word stored at memory address (base + offset), to CPz register rt
            // TODO: dont assume cp0
            let address = codegen.base_plus_offset(instr, "lwcz_addr");
            let value = codegen.read_memory(i32_type, address);
            codegen.write_cp0_reg(instr.rt(), value);
        }

        Mnenomic::Jal => {
            // Jump to target address, stores return address in r31 (ra)
            let return_address = codegen.build_i64(next_pc).into_int_value();
            codegen.write_general_reg(register::GeneralPurpose::Ra, return_address.into());

            let target = instr.try_resolve_static_jump(pc as _).unwrap();
            codegen.call_label(codegen.get_label(target));
        }

        Mnenomic::J => {
            // Jump to target address
            let target_pc = instr.try_resolve_static_jump(pc as _).unwrap();
            codegen.call_label(codegen.get_label(target_pc));
        }

        // Mnenomic::Sc => {
        //     // If LL bit is set, stores contents of rt, to memory address (base + offset)
        //     let ll_bit = codegen.read_special_reg(register::Special::LoadLink);
        //     let cond = codegen.builder.build_int_compare(
        //         IntPredicate::EQ,
        //         ll_bit.into_int_value(),
        //         i64_type.const_int(1, false),
        //         "sc_cond",
        //     );
        //
        //     let next_block = codegen.get_label(next_pc);
        //     let name = format!("label_{pc:06x}_write");
        //     let then_block = codegen.build_fall_through_block(&name, next_block, || {
        //         let addr = codegen.base_plus_offset(instr, "sc_addr");
        //         let value = codegen.read_general_reg(instr.rt());
        //         codegen.write_memory(addr, value);
        //     });
        //
        //     codegen
        //         .builder
        //         .build_conditional_branch(cond, then_block, next_block);
        // }
        //
        // Mnenomic::Scd => {
        //     // If LL bit is set, stores contents of rt, to memory address (base + offset)
        //     let ll_bit = codegen.read_special_reg(register::Special::LoadLink);
        //     let cond = codegen.builder.build_int_compare(
        //         IntPredicate::EQ,
        //         ll_bit.into_int_value(),
        //         i64_type.const_int(1, false),
        //         "scd_cond",
        //     );
        //
        //     let next_block = codegen.get_label(next_pc);
        //     let name = format!("label_{pc:6x}_write");
        //     let then_block = codegen.build_fall_through_block(&name, next_block, || {
        //         let addr = codegen.base_plus_offset(instr, "scd_addr");
        //         let value = codegen.read_general_reg(instr.rt());
        //         codegen.write_memory(addr, value);
        //     });
        //
        //     codegen
        //         .builder
        //         .build_conditional_branch(cond, then_block, next_block);
        // }
        Mnenomic::Ll => {
            // Loads word stored at memory address (base + offset), stores sign-extended word in rt, and sets the LL bit to 1
            let addr = codegen.base_plus_offset(instr, "ll_addr");
            let value = codegen.read_memory(i32_type, addr).into_int_value();
            let sign_extended = codegen.sign_extend_to_i64(value);
            codegen.write_general_reg(instr.rt(), sign_extended.into());

            let one = i64_type.const_int(1, false).into();
            codegen.write_special_reg(register::Special::LoadLink, one);
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

        Mnenomic::Daddi => {
            // Add sign-extended 16bit immediate and rs, store result in rt
            let source = codegen.read_general_reg(instr.rs()).into_int_value();
            let immediate =
                codegen.sign_extend_to_i64(i16_type.const_int(instr.immediate() as _, false));
            let result = codegen
                .builder
                .build_int_add(source, immediate, "daddi_res");
            codegen.write_general_reg(instr.rt(), result.into());
        }

        Mnenomic::Daddu => {
            // Add rs and rt, store result in rd
            let source = codegen.read_general_reg(instr.rs()).into_int_value();
            let target = codegen.read_general_reg(instr.rt()).into_int_value();
            let result = codegen.builder.build_int_add(source, target, "daddu_res");
            codegen.write_general_reg(instr.rd(), result.into());
        }

        Mnenomic::Nor => {
            // NOR rs and rt, store result in rd
            let source = codegen.read_general_reg(instr.rs()).into_int_value();
            let target = codegen.read_general_reg(instr.rt()).into_int_value();
            let or = codegen.builder.build_or(source, target, "nor_or");
            let result = codegen.builder.build_not(or, "nor_res");
            codegen.write_general_reg(instr.rd(), result.into());
        }

        Mnenomic::Div => {
            // Divide signed rs by signed rt, store quotient in register LO and remainder in HI
            let source = codegen.read_general_reg(instr.rs()).into_int_value();
            let target = codegen.read_general_reg(instr.rt()).into_int_value();

            let quotient = codegen
                .builder
                .build_int_signed_div(source, target, "divu_quot");
            let remainder = codegen
                .builder
                .build_int_signed_rem(source, target, "divu_rem");

            codegen.write_special_reg(register::Special::Lo, quotient.into());
            codegen.write_special_reg(register::Special::Hi, remainder.into());
        }

        Mnenomic::Ddiv => {
            // Divide signed rs by signed rt, store quotient in register LO and remainder in HI
            let source = codegen.read_general_reg(instr.rs()).into_int_value();
            let target = codegen.read_general_reg(instr.rt()).into_int_value();

            let quotient = codegen
                .builder
                .build_int_signed_div(source, target, "ddiv_quot");
            let remainder = codegen
                .builder
                .build_int_signed_rem(source, target, "ddiv_rem");

            codegen.write_special_reg(register::Special::Lo, quotient.into());
            codegen.write_special_reg(register::Special::Hi, remainder.into());
        }

        Mnenomic::Ddivu => {
            // Divide unsigned rs by unsigned rt, store quotient in register LO and remainder in HI
            let source = codegen.read_general_reg(instr.rs()).into_int_value();
            let target = codegen.read_general_reg(instr.rt()).into_int_value();

            let quotient = codegen
                .builder
                .build_int_unsigned_div(source, target, "ddivu_quot");
            let remainder = codegen
                .builder
                .build_int_unsigned_rem(source, target, "ddivu_rem");

            codegen.write_special_reg(register::Special::Lo, quotient.into());
            codegen.write_special_reg(register::Special::Hi, remainder.into());
        }

        Mnenomic::Divu => {
            // Divide unsigned rs by unsigned rt, store quotient in register LO and remainder in HI
            let source = codegen.read_general_reg(instr.rs()).into_int_value();
            let target = codegen.read_general_reg(instr.rt()).into_int_value();

            let quotient = codegen
                .builder
                .build_int_unsigned_div(source, target, "divu_quot");
            let remainder = codegen
                .builder
                .build_int_unsigned_rem(source, target, "divu_rem");

            codegen.write_special_reg(register::Special::Lo, quotient.into());
            codegen.write_special_reg(register::Special::Hi, remainder.into());
        }

        Mnenomic::Dsllv => {
            // Shift rt left by rs (limited to 63), store result in rd
            let target = codegen.read_general_reg(instr.rt()).into_int_value();
            let source = codegen.read_general_reg(instr.rs()).into_int_value();

            let result = codegen
                .builder
                .build_right_shift(target, source, false, "dsllv_shift");
            codegen.write_general_reg(instr.rd(), result.into());
        }

        Mnenomic::Or => {
            // OR rs and rt, store result in rd
            let source = codegen.read_general_reg(instr.rs()).into_int_value();
            let target = codegen.read_general_reg(instr.rt()).into_int_value();
            let result = codegen.builder.build_or(source, target, "or_res");
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

            let result = codegen
                .builder
                .build_left_shift(target, source, "sllv_shift");
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

        Mnenomic::Dsrl => {
            // Shift rt right by sa bits, store sign-extended result in rd
            let target = codegen.read_general_reg(instr.rt()).into_int_value();
            let shift = i64_type.const_int(instr.sa() as _, false);
            let result = codegen
                .builder
                .build_right_shift(target, shift, true, "dsrl32_shift");
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

        Mnenomic::Dsrlv => {
            // Shift rt right by rs (limited to 63), store sign-extended result in rd
            let target = codegen.read_general_reg(instr.rt()).into_int_value();
            let source = codegen.read_general_reg(instr.rs()).into_int_value();

            let result = codegen
                .builder
                .build_right_shift(target, source, true, "dsrlv_shift");
            codegen.write_general_reg(instr.rd(), result.into());
        }

        Mnenomic::Dsra => {
            // Shift rt right by sa bits, store sign-extended result in rd
            let target = codegen.read_general_reg(instr.rt()).into_int_value();
            let shift = i64_type.const_int(instr.sa() as _, false);
            let result = codegen
                .builder
                .build_right_shift(target, shift, true, "dsra32_shift");
            codegen.write_general_reg(instr.rd(), result.into());
        }

        Mnenomic::Dsrav => {
            // Shift rt right by rs (limited to 63), store sign-extended result in rd
            let target = codegen.read_general_reg(instr.rt()).into_int_value();
            let source = codegen.read_general_reg(instr.rs()).into_int_value();

            let result = codegen
                .builder
                .build_right_shift(target, source, true, "dsrav_shift");
            codegen.write_general_reg(instr.rd(), result.into());
        }

        Mnenomic::Dsra32 => {
            // Shift rt right by (32 + sa) bits, store sign-extended result in rd
            let target = codegen.read_general_reg(instr.rt()).into_int_value();
            let shift = i64_type.const_int((instr.sa() + 32) as _, false);
            let result = codegen
                .builder
                .build_right_shift(target, shift, true, "dsra32_shift");
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

            let result = codegen
                .builder
                .build_right_shift(target, source, false, "srlv_shift");
            codegen.write_general_reg(instr.rd(), result.into());
        }

        Mnenomic::Sra => {
            // Shift rt right by sa bits, store sign-extended result in rd
            let target = codegen.read_general_reg(instr.rt()).into_int_value();
            let shift = i64_type.const_int(instr.sa() as _, false);
            let result = codegen
                .builder
                .build_right_shift(target, shift, true, "sra_shift");
            codegen.write_general_reg(instr.rd(), result.into());
        }

        Mnenomic::Srav => {
            // Shift rt right by rs (limited to 31), store sign-extended result in rd
            let target = codegen.read_general_reg(instr.rt()).into_int_value();
            let source = codegen.read_general_reg(instr.rs()).into_int_value();

            let result = codegen
                .builder
                .build_right_shift(target, source, true, "srav_shift");
            codegen.write_general_reg(instr.rd(), result.into());
        }

        Mnenomic::Dmult => {
            // Multiply signed rs with signed rt, store low-order doubleword in LO, and high-order doubleword to HI
            let source = {
                let source = codegen.read_general_reg(instr.rs()).into_int_value();
                codegen
                    .builder
                    .build_int_s_extend(source, i128_type, "dmult_source")
            };
            let target = {
                let target = codegen.read_general_reg(instr.rt()).into_int_value();
                codegen
                    .builder
                    .build_int_s_extend(target, i128_type, "dmult_target")
            };

            let result = codegen
                .builder
                .build_int_mul(source, target, "dmult_result");

            // Store the low-order doubleword in LO.
            let lo = codegen
                .builder
                .build_int_truncate(result, i64_type, "dmult_lo_trunc");
            codegen.write_special_reg(register::Special::Lo, lo.into());

            // Store the high-order doubleword in HI.
            let hi = {
                let shift = i128_type.const_int(64, false);
                let hi = codegen
                    .builder
                    .build_right_shift(result, shift, false, "dmult_hi");
                codegen
                    .builder
                    .build_int_truncate(hi, i64_type, "dmult_hi_trunc")
            };
            codegen.write_special_reg(register::Special::Hi, hi.into());
        }

        Mnenomic::Dmultu => {
            // Multiply unsigned rs with unsigned rt, store low-order doubleword in LO, and high-order doubleword to HI
            let source = {
                let source = codegen.read_general_reg(instr.rs()).into_int_value();
                codegen
                    .builder
                    .build_int_z_extend(source, i128_type, "dmultu_source")
            };
            let target = {
                let target = codegen.read_general_reg(instr.rt()).into_int_value();
                codegen
                    .builder
                    .build_int_z_extend(target, i128_type, "dmultu_target")
            };

            let result = codegen.builder.build_int_mul(source, target, "dmultu_res");

            // Store the low-order doubleword in LO.
            let lo = codegen
                .builder
                .build_int_truncate(result, i64_type, "dmultu_lo");
            codegen.write_special_reg(register::Special::Lo, lo.into());

            // Store the high-order doubleword in HI.
            let hi = {
                let shift = i128_type.const_int(64, false);
                let hi = codegen
                    .builder
                    .build_right_shift(result, shift, false, "dmultu_hi");
                codegen
                    .builder
                    .build_int_truncate(hi, i64_type, "dmultu_hi_trunc")
            };
            codegen.write_special_reg(register::Special::Hi, hi.into());
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
            let shift = i64_type.const_int(32, false);
            let hi = codegen
                .builder
                .build_right_shift(result, shift, false, "multu_hi");
            codegen.write_special_reg(register::Special::Hi, hi.into());
        }

        Mnenomic::Mflo => {
            // Copy contents of register LO to rd
            // TODO: Should produce incorrect results if any of the two following instructions modify LO register
            let lo = codegen.read_special_reg(register::Special::Lo);
            codegen.write_general_reg(instr.rd(), lo);
        }

        Mnenomic::Mfhi => {
            // Copy contents of register HI to rd
            // TODO: Should produce incorrect results if any of the two following instructions modify the HI register
            let hi = codegen.read_special_reg(register::Special::Hi);
            codegen.write_general_reg(instr.rd(), hi);
        }

        Mnenomic::Jr => {
            // Jump to address stored in rs
            let source = codegen.read_general_reg(instr.rs()).into_int_value();
            codegen.build_dynamic_jump(source);
        }

        Mnenomic::Jalr => {
            // Jump to address stored in rs, stores return address in rd
            let source = codegen.read_general_reg(instr.rs()).into_int_value();
            let return_addr = i64_type.const_int(pc + INSTRUCTION_SIZE as u64, false);
            codegen.write_general_reg(instr.rd(), return_addr.into());
            codegen.build_dynamic_jump(source);
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

        Mnenomic::Lwu => {
            // Loads word stored at memory address (base + offset), stores zero-extended word in rt
            let address = codegen.base_plus_offset(instr, "lwu_addr");
            let value = {
                let value = codegen.read_memory(i32_type, address);
                codegen.zero_extend_to_i64(value.into_int_value())
            };
            codegen.write_general_reg(instr.rt(), value.into());
        }

        Mnenomic::Lh => {
            // Loads halfword stored at memory address (base + offset), stores sign-extended halfword in rt
            let address = codegen.base_plus_offset(instr, "lh_addr");
            let value = {
                let value = codegen.read_memory(i16_type, address);
                codegen.sign_extend_to_i64(value.into_int_value())
            };
            codegen.write_general_reg(instr.rt(), value.into());
        }

        Mnenomic::Lhu => {
            // Loads halfword stored at memory address (base + offset), stores zero-extended halfword in rt
            let address = codegen.base_plus_offset(instr, "lhu_addr");
            let value = {
                let value = codegen.read_memory(i32_type, address);
                codegen.zero_extend_to_i64(value.into_int_value())
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

        Mnenomic::Dadd => {
            // Add rs and rt, store result in rd
            let source = codegen.read_general_reg(instr.rs()).into_int_value();
            let target = codegen.read_general_reg(instr.rt()).into_int_value();
            let result = codegen.builder.build_int_add(source, target, "dadd_res");
            codegen.write_general_reg(instr.rd(), result.into());
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

            let result = codegen.build_int_compare_as_i64(
                IntPredicate::SLT,
                source,
                target,
                "slt_cmp",
                "slt_res",
            );

            codegen.write_general_reg(instr.rd(), result.into());
        }

        Mnenomic::Slti => {
            // If signed rs is less than sign-extended immediate, store one in rd, otherwise store zero
            let immediate =
                codegen.sign_extend_to_i64(i16_type.const_int(instr.immediate() as _, true));
            let source = codegen.read_general_reg(instr.rs()).into_int_value();

            let result = codegen.build_int_compare_as_i64(
                IntPredicate::SLT,
                source,
                immediate,
                "slti_cmp",
                "slti_res",
            );

            codegen.write_general_reg(instr.rt(), result.into());
        }

        Mnenomic::Sltiu => {
            // If unsigned rs is less than sign-extended immediate, store one in rt, otherwise store zero
            let immediate =
                codegen.sign_extend_to_i64(i16_type.const_int(instr.immediate() as _, true));
            let source = codegen.read_general_reg(instr.rs()).into_int_value();

            let result = codegen.build_int_compare_as_i64(
                IntPredicate::ULT,
                source,
                immediate,
                "sltiu_cmp",
                "sltiu_res",
            );

            codegen.write_general_reg(instr.rt(), result.into());
        }

        Mnenomic::Sub => {
            // Subtract rt from rs, store result in rd
            let source = codegen.read_general_reg(instr.rs()).into_int_value();
            let target = codegen.read_general_reg(instr.rt()).into_int_value();
            let result = codegen.builder.build_int_sub(source, target, "sub_res");
            codegen.write_general_reg(instr.rd(), result.into());
        }

        Mnenomic::Subu => {
            // Subtract rt from rs, store result in rd
            let source = codegen.read_general_reg(instr.rs()).into_int_value();
            let target = codegen.read_general_reg(instr.rt()).into_int_value();
            let result = codegen.builder.build_int_sub(target, source, "subu_res");
            codegen.write_general_reg(instr.rd(), result.into());
        }

        Mnenomic::Beq => {
            // If rs equals rt, branch to address
            let source = codegen.read_general_reg(instr.rs()).into_int_value();
            let target = codegen.read_general_reg(instr.rt()).into_int_value();
            let cmp =
                codegen
                    .builder
                    .build_int_compare(IntPredicate::EQ, source, target, "beq_cmp");

            let target_pc = instr.try_resolve_static_jump(pc).unwrap();
            codegen.build_conditional_branch(cmp, target_pc);
        }

        Mnenomic::Bne => {
            // If rs is not equal to rt, branch to address.
            let source = codegen.read_general_reg(instr.rs()).into_int_value();
            let target = codegen.read_general_reg(instr.rt()).into_int_value();
            let cmp =
                codegen
                    .builder
                    .build_int_compare(IntPredicate::NE, source, target, "bne_cmp");

            let target_pc = instr.try_resolve_static_jump(pc).unwrap();
            codegen.build_conditional_branch(cmp, target_pc);
        }

        Mnenomic::Bltz => {
            // If rs is less than zero, branch to address (delay slot + offset)
            let source = codegen.read_general_reg(instr.rs()).into_int_value();
            let zero = i64_type.const_zero();
            let cmp =
                codegen
                    .builder
                    .build_int_compare(IntPredicate::SLT, source, zero, "bltz_cmp");

            let target_pc = instr.try_resolve_static_jump(pc).unwrap();
            codegen.build_conditional_branch(cmp, target_pc);
        }

        Mnenomic::Bltzl => {
            // If rs is less than zero, branch to address (delay slot + offset), otherwise discard delay slot instruction
            let source = codegen.read_general_reg(instr.rs()).into_int_value();
            let zero = i64_type.const_zero();
            let cmp =
                codegen
                    .builder
                    .build_int_compare(IntPredicate::SLT, source, zero, "bltz_cmp");

            let target_pc = instr.try_resolve_static_jump(pc).unwrap();
            codegen.build_conditional_branch(cmp, target_pc);
        }

        Mnenomic::Beql => {
            // If rs equals rt, branch to address (delay slot + offset), otherwise discard delay slot instruction
            let source = codegen.read_general_reg(instr.rs()).into_int_value();
            let target = codegen.read_general_reg(instr.rt()).into_int_value();
            let cmp =
                codegen
                    .builder
                    .build_int_compare(IntPredicate::EQ, source, target, "beql_cmp");

            let target_pc = instr.try_resolve_static_jump(pc).unwrap();
            codegen.build_conditional_branch(cmp, target_pc);
        }

        Mnenomic::Bgezal => {
            // If rs is greater than or equal to zero, branch to address (delay slot + offset) and store next address to r31 (ra)
            let source = codegen.read_general_reg(instr.rs()).into_int_value();
            let zero = i64_type.const_zero();
            let cmp =
                codegen
                    .builder
                    .build_int_compare(IntPredicate::SGE, source, zero, "bgezal_cmp");

            let target_pc = instr.try_resolve_static_jump(pc).unwrap();
            codegen.build_conditional_branch_set_ra(cmp, target_pc, next_pc);
        }

        Mnenomic::Bgezall => {
            // If rs is greater than or equal to zero, branch to address (delay slot + offset) and store next address to r31, otherwise discard delay slot instruction
            let source = codegen.read_general_reg(instr.rs()).into_int_value();
            let zero = i64_type.const_zero();
            let cmp =
                codegen
                    .builder
                    .build_int_compare(IntPredicate::SGE, source, zero, "bgezall_cmp");

            let target_pc = instr.try_resolve_static_jump(pc).unwrap();
            codegen.build_conditional_branch_set_ra(cmp, target_pc, next_pc);
        }

        Mnenomic::Bltzal => {
            // If rs is less than zero, branch to address (delay slot + offset) and store next address to r31 (ra)
            let source = codegen.read_general_reg(instr.rs()).into_int_value();
            let zero = i64_type.const_zero();
            let cmp =
                codegen
                    .builder
                    .build_int_compare(IntPredicate::SLT, source, zero, "bltzal_cmp");

            let target_pc = instr.try_resolve_static_jump(pc).unwrap();
            codegen.build_conditional_branch_set_ra(cmp, target_pc, next_pc);
        }

        Mnenomic::Bltzall => {
            // If rs is less than zero, branch to address (delay slot + offset) and store next address to r31, otherwise discard delay slot instruction
            let source = codegen.read_general_reg(instr.rs()).into_int_value();
            let zero = i64_type.const_zero();
            let cmp =
                codegen
                    .builder
                    .build_int_compare(IntPredicate::SLT, source, zero, "bltzal_cmp");

            let target_pc = instr.try_resolve_static_jump(pc).unwrap();
            codegen.build_conditional_branch_set_ra(cmp, target_pc, next_pc);
        }

        Mnenomic::Bnel => {
            // If rs is not equal to rt, branch to address (delay slot + offset), otherwise discard delay slot instruction
            let source = codegen.read_general_reg(instr.rs()).into_int_value();
            let target = codegen.read_general_reg(instr.rt()).into_int_value();
            let cmp =
                codegen
                    .builder
                    .build_int_compare(IntPredicate::NE, source, target, "bnel_cmp");

            let target_pc = instr.try_resolve_static_jump(pc).unwrap();
            codegen.build_conditional_branch(cmp, target_pc);
        }

        Mnenomic::Blez => {
            // If rs is less than or equal to zero, branch to address (delay slot + offset)
            let source = codegen.read_general_reg(instr.rs()).into_int_value();
            let zero = i64_type.const_zero();
            let cmp =
                codegen
                    .builder
                    .build_int_compare(IntPredicate::SLE, source, zero, "blez_cmp");

            let target_pc = instr.try_resolve_static_jump(pc).unwrap();
            codegen.build_conditional_branch(cmp, target_pc);
        }

        Mnenomic::Blezl => {
            // If rs is less than or equal to zero, branch to address (delay slot + offset), otherwise discard delay slot instruction
            let source = codegen.read_general_reg(instr.rs()).into_int_value();
            let zero = i64_type.const_zero();
            let cmp =
                codegen
                    .builder
                    .build_int_compare(IntPredicate::SLE, source, zero, "blezl_cmp");

            let target_pc = instr.try_resolve_static_jump(pc).unwrap();
            codegen.build_conditional_branch(cmp, target_pc);
        }

        Mnenomic::Bgezl => {
            // If rs is greater than or equal to zero, branch to address (delay slot + offset), otherwise discard delay slot instruction
            let source = codegen.read_general_reg(instr.rs()).into_int_value();
            let zero = i64_type.const_zero();
            let cmp =
                codegen
                    .builder
                    .build_int_compare(IntPredicate::SGE, source, zero, "blezl_cmp");

            let target_pc = instr.try_resolve_static_jump(pc).unwrap();
            codegen.build_conditional_branch(cmp, target_pc);
        }

        Mnenomic::Bgez => {
            // If rs is greater than or equal to zero, branch to address (delay slot + offset)
            let source = codegen.read_general_reg(instr.rs()).into_int_value();
            let zero = i64_type.const_zero();
            let cmp =
                codegen
                    .builder
                    .build_int_compare(IntPredicate::SGE, source, zero, "bgez_cmp");

            let target_pc = instr.try_resolve_static_jump(pc).unwrap();
            codegen.build_conditional_branch(cmp, target_pc);
        }

        Mnenomic::Bgtz => {
            // If rs is greater than zero, branch to address (delay slot + offset)
            let source = codegen.read_general_reg(instr.rs()).into_int_value();
            let zero = i64_type.const_zero();
            let cmp =
                codegen
                    .builder
                    .build_int_compare(IntPredicate::SGT, source, zero, "bgtz_cmp");

            let target_pc = instr.try_resolve_static_jump(pc).unwrap();
            codegen.build_conditional_branch(cmp, target_pc);
        }

        Mnenomic::Bgtzl => {
            // If rs is greater than zero, branch to address (delay slot + offset), otherwise discard delay slot instruction
            let source = codegen.read_general_reg(instr.rs()).into_int_value();
            let zero = i64_type.const_zero();
            let cmp =
                codegen
                    .builder
                    .build_int_compare(IntPredicate::SGT, source, zero, "bgtz_cmp");

            let target_pc = instr.try_resolve_static_jump(pc).unwrap();
            codegen.build_conditional_branch(cmp, target_pc);
        }

        _ => todo!("instruction {} at {pc:#x}", instr.mnemonic().name()),
    }
}
