use crate::{codegen::CodeGen, runtime::RuntimeFunction};
use inkwell::{basic_block::BasicBlock, values::IntValue, IntPredicate};
use mips_decomp::{
    instruction::{Mnenomic, ParsedInstruction},
    register,
};

fn stub(codegen: &CodeGen, name: &str) {
    let output = format!("STUB: {name} instruction not executed");
    let storage_name = format!("{name}_stub");
    codegen.print_constant_string(&output, &storage_name);
    env_call!(codegen, RuntimeFunction::Panic, []);
}

/// A helper for various jump instructions (JAL, J) which calculates the target address at runtime.
/// The 26-bit target is shifted left two bits and combined with the high-order four bits of the address of the delay slot.
fn jump_address<'ctx>(codegen: &CodeGen<'ctx>, target: u32) -> IntValue<'ctx> {
    let i64_type = codegen.context.i64_type();
    let delay_slot = {
        let address = codegen.next_instruction_address();
        let mask = i64_type.const_int(0xFFFF_FFFF_F000_0000, false);
        codegen.builder.build_and(address, mask, "delay_slot")
    };

    let target = i64_type.const_int((target as u64) << 2, false);
    codegen.builder.build_or(target, delay_slot, "jal_addr")
}

pub fn recompile_instruction<'ctx>(
    codegen: &CodeGen<'ctx>,
    instr: &ParsedInstruction,
    pc: u64,
) -> Option<BasicBlock<'ctx>> {
    let mnemonic = instr.mnemonic();
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

        Mnenomic::CCondFmtFs => {
            // Compares fs and ft using cond
            stub(codegen, "ccondfmtfs");
        }

        Mnenomic::Syscall => {
            // Causes system call exception
            stub(codegen, "syscall");
        }

        Mnenomic::Break => {
            // Causes breakpoint exception
            stub(codegen, "break");
        }

        Mnenomic::Cop2 => {
            // Perform a CP2 operation
            stub(codegen, "cop2");
        }

        Mnenomic::Bczf => {
            // If CPz's CpCond is false, branch to address (delay slot + offset)
            stub(codegen, "bczf");
        }

        Mnenomic::Bczt => {
            // If CPz's CpCond is true, branch to address (delay slot + offset)
            stub(codegen, "bczt");
        }

        Mnenomic::Swc1 => {
            // Copies word from CP1, to memory address (base + offset)
            stub(codegen, "swc1");
        }

        Mnenomic::Swc2 => {
            // Copies word from CP1, to memory address (base + offset)
            stub(codegen, "swc2");
        }

        Mnenomic::Sdc1 => {
            // Copies doubleword from CP1, to memory address (base + offset)
            stub(codegen, "sdc1");
        }

        Mnenomic::Ldc1 => {
            // Copies doubleword stored at memory address (base + offset), to CP1
            stub(codegen, "ldc1");
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

        Mnenomic::Ctc2 => {
            // Copy contents of GPR rt, to CP2's control register rd
            stub(codegen, "ctc2");
        }

        Mnenomic::Mfc2 => {
            // Copy contents of CP2's coprocessor register rd, to GPR rt
            stub(codegen, "mfc2");
        }

        Mnenomic::Mtc2 => {
            // Copy contents of GPR rt, to CP2's coprocessor register rd
            stub(codegen, "mtc2");
        }

        Mnenomic::Lwc1 => {
            // Copies word stored at memory address (base + offset), to CP1's register rt
            stub(codegen, "lwc1");
        }

        Mnenomic::Lwc2 => {
            // Copies word stored at memory address (base + offset), to CP2's register rt
            stub(codegen, "lwc2");
        }

        Mnenomic::Bczfl => {
            // If CPz's CpCond is false, branch to address (delay slot + offset), otherwise discard delay slot instruction
            stub(codegen, "bczfl");
            // Stub needed because this is a likely branch
            let curr_block = codegen.builder.get_insert_block().unwrap();
            return Some(curr_block);
        }

        Mnenomic::Bcztl => {
            // If CPz's CpCond is true, branch to address (delay slot + offset), otherwise discard delay slot instruction
            stub(codegen, "bcztl");
            // Stub needed because this is a likely branch
            let curr_block = codegen.builder.get_insert_block().unwrap();
            return Some(curr_block);
        }

        Mnenomic::AddFmt => {
            // Add fs and ft, store result in fd
            stub(codegen, "addfmt");
        }

        Mnenomic::Sync => {
            // Executed as NOP on the VR4300
        }

        Mnenomic::Cache => {
            // Flush instruction or data cache at address (base + offset) to RAM
            // There is no need to emulate the instruction/data cache, so we'll ignore it for now.
        }

        Mnenomic::Tlbwi => {
            // Stores the contents of EntryHi and EntryLo registers into the TLB entry pointed at by the Index register
            let index = codegen.read_register(i64_type, register::Cp0::Index);
            env_call!(codegen, RuntimeFunction::WriteTlbEntry, [index]);
        }

        Mnenomic::Ctc1 => {
            // Copy contents of GPR rt, to CP1's control register fs. Only valid if fs equals 0 or 31
            assert!(instr.fs() == 0 || instr.fs() == 31);
            let target = codegen.read_general_register(i64_type, instr.rt());
            codegen.write_fpu_register(instr.fs(), target);
        }

        Mnenomic::Cfc1 => {
            // Copy contents of CP1's control register fs, to GPR rt. Only valid if fs equals 0 or 31
            assert!(instr.fs() == 0 || instr.fs() == 31);
            let source = codegen.read_fpu_register(i64_type, instr.fs());
            codegen.write_general_register(instr.rt(), source);
        }

        Mnenomic::Mtlo => {
            // Copy contents of rs to register LO
            let target = codegen.read_general_register(i64_type, instr.rs());
            codegen.write_register(register::Special::Lo, target);
        }

        Mnenomic::Mthi => {
            // Copy contents of rs to register HI
            let target = codegen.read_general_register(i64_type, instr.rs());
            codegen.write_register(register::Special::Hi, target);
        }

        Mnenomic::Mtc0 => {
            // Copy contents of GPR rt, to CP0's coprocessor register rd
            let target = codegen.read_general_register(i32_type, instr.rt());
            codegen.write_cp0_register(instr.rd(), target);
        }

        Mnenomic::Mfc0 => {
            // Copy contents of CP0's coprocessor register rd, to GPR rt
            let destination = codegen.read_cp0_register(i32_type, instr.rd());
            codegen.write_general_register(instr.rt(), destination);
        }

        Mnenomic::Dmtc0 => {
            // Copy doubleword contents of GPR rt, to CP0 coprocessor register rd
            let target = codegen.read_general_register(i64_type, instr.rt());
            codegen.write_cp0_register(instr.rd(), target);
        }

        Mnenomic::Dmfc0 => {
            // Copy doubleword contents of CP0's coprocessor register rd, to GPR rt
            let destination = codegen.read_cp0_register(i64_type, instr.rd());
            codegen.write_general_register(instr.rt(), destination);
        }

        Mnenomic::Jalr => {
            // Jump to address stored in rs, stores return address in rd
            let source = codegen.read_general_register(i64_type, instr.rs());
            let return_addr = codegen.next_instruction_address();
            codegen.write_general_register(instr.rd(), return_addr);
            codegen.build_dynamic_jump(source);
        }

        Mnenomic::Jal => {
            // Jump to target address, stores return address in r31 (ra)
            let return_address = codegen.next_instruction_address();
            codegen.write_general_register(register::GeneralPurpose::Ra, return_address);
            codegen.call_label_dynamic(jump_address(codegen, instr.immediate()));
        }

        Mnenomic::J => {
            // Jump to target address
            codegen.call_label_dynamic(jump_address(codegen, instr.immediate()));
        }

        Mnenomic::Sc => {
            // If LL bit is set, stores contents of rt, to memory address (base + offset), truncated to 32-bits
            let ll_bit = codegen.read_register(i64_type, register::Special::LoadLink);
            let cond = codegen.builder.build_int_compare(
                IntPredicate::EQ,
                ll_bit,
                i32_type.const_int(1, false),
                "sc_cond",
            );

            let current_fn = codegen
                .builder
                .get_insert_block()
                .unwrap()
                .get_parent()
                .unwrap();
            let then_block = codegen.context.append_basic_block(current_fn, "sc_write");
            let next_block = codegen.context.append_basic_block(current_fn, "sc_done");

            codegen
                .builder
                .build_conditional_branch(cond, then_block, next_block);

            codegen.builder.position_at_end(then_block);
            {
                let addr = codegen.base_plus_offset(instr, "sc_addr");
                let value = codegen.read_general_register(i32_type, instr.rt());
                codegen.write_memory(addr, value);
                codegen.builder.build_unconditional_branch(next_block);
            }

            codegen.builder.position_at_end(next_block);
        }

        Mnenomic::Scd => {
            // If LL bit is set, stores contents of rt, to memory address (base + offset)
            let ll_bit = codegen.read_register(i64_type, register::Special::LoadLink);
            let cond = codegen.builder.build_int_compare(
                IntPredicate::EQ,
                ll_bit,
                i32_type.const_int(1, false),
                "scd_cond",
            );

            let current_fn = codegen
                .builder
                .get_insert_block()
                .unwrap()
                .get_parent()
                .unwrap();
            let then_block = codegen.context.append_basic_block(current_fn, "scd_write");
            let next_block = codegen.context.append_basic_block(current_fn, "scd_done");

            codegen
                .builder
                .build_conditional_branch(cond, then_block, next_block);

            codegen.builder.position_at_end(then_block);
            {
                let addr = codegen.base_plus_offset(instr, "scd_addr");
                let value = codegen.read_general_register(i64_type, instr.rt());
                codegen.write_memory(addr, value);
                codegen.builder.build_unconditional_branch(next_block);
            }

            codegen.builder.position_at_end(next_block);
            // Fall through
        }

        Mnenomic::Ll => {
            // Loads word stored at memory address (base + offset), stores sign-extended word in rt, and sets the LL bit to 1
            let addr = codegen.base_plus_offset(instr, "ll_addr");
            let value = codegen.read_memory(i32_type, addr);
            let sign_extended = codegen.sign_extend_to(i64_type, value);
            codegen.write_general_register(instr.rt(), sign_extended);

            let one = i64_type.const_int(1, false);
            codegen.write_register(register::Special::LoadLink, one);
        }

        Mnenomic::And => {
            // AND rs with rt, store result in rd
            let source = codegen.read_general_register(i64_type, instr.rs());
            let target = codegen.read_general_register(i64_type, instr.rt());
            let result = codegen.builder.build_and(source, target, "and_res");
            codegen.write_general_register(instr.rd(), result);
        }

        Mnenomic::Xor => {
            // XOR rs with rt, store result in rd
            let source = codegen.read_general_register(i64_type, instr.rs());
            let target = codegen.read_general_register(i64_type, instr.rt());
            let result = codegen.builder.build_xor(source, target, "xor_res");
            codegen.write_general_register(instr.rd(), result);
        }

        Mnenomic::Xori => {
            // XOR rs with zero-extended immediate, store result in rd
            let source = codegen.read_general_register(i64_type, instr.rs());
            let immediate =
                codegen.zero_extend_to(i64_type, i16_type.const_int(instr.immediate() as _, false));

            let result = codegen.builder.build_xor(source, immediate, "xori_res");
            codegen.write_general_register(instr.rt(), result);
        }

        Mnenomic::Addu => {
            // Add rs and rt, store result in rd
            let source = codegen.read_general_register(i32_type, instr.rs());
            let target = codegen.read_general_register(i32_type, instr.rt());

            let result = codegen.sign_extend_to(
                i64_type,
                codegen.builder.build_int_add(source, target, "addu_res"),
            );

            codegen.write_general_register(instr.rd(), result);
        }

        Mnenomic::Daddi => {
            // Add sign-extended 16bit immediate and rs, store result in rt
            let source = codegen.read_general_register(i64_type, instr.rs());
            let immediate =
                codegen.sign_extend_to(i64_type, i16_type.const_int(instr.immediate() as _, true));
            let result = codegen
                .builder
                .build_int_add(source, immediate, "daddi_res");
            codegen.write_general_register(instr.rt(), result);
        }

        Mnenomic::Daddu => {
            // Add rs and rt, store result in rd
            let source = codegen.read_general_register(i64_type, instr.rs());
            let target = codegen.read_general_register(i64_type, instr.rt());
            let result = codegen.builder.build_int_add(source, target, "daddu_res");
            codegen.write_general_register(instr.rd(), result);
        }

        Mnenomic::Nor => {
            // NOR rs and rt, store result in rd
            let source = codegen.read_general_register(i64_type, instr.rs());
            let target = codegen.read_general_register(i64_type, instr.rt());
            let or = codegen.builder.build_or(source, target, "nor_or");
            let result = codegen.builder.build_not(or, "nor_res");
            codegen.write_general_register(instr.rd(), result);
        }

        Mnenomic::Div => {
            // Divide signed rs by signed rt, store quotient in register LO and remainder in HI
            let source = codegen.read_general_register(i32_type, instr.rs());
            let target = codegen.read_general_register(i32_type, instr.rt());

            let quotient = codegen
                .builder
                .build_int_signed_div(source, target, "divu_quot");
            let remainder = codegen
                .builder
                .build_int_signed_rem(source, target, "divu_rem");

            codegen.write_register(register::Special::Lo, quotient);
            codegen.write_register(register::Special::Hi, remainder);
        }

        Mnenomic::Ddiv => {
            // Divide signed rs by signed rt, store quotient in register LO and remainder in HI
            let source = codegen.read_general_register(i64_type, instr.rs());
            let target = codegen.read_general_register(i64_type, instr.rt());

            let quotient = codegen
                .builder
                .build_int_signed_div(source, target, "ddiv_quot");
            let remainder = codegen
                .builder
                .build_int_signed_rem(source, target, "ddiv_rem");

            codegen.write_register(register::Special::Lo, quotient);
            codegen.write_register(register::Special::Hi, remainder);
        }

        Mnenomic::Ddivu => {
            // Divide unsigned rs by unsigned rt, store quotient in register LO and remainder in HI
            let source = codegen.read_general_register(i64_type, instr.rs());
            let target = codegen.read_general_register(i64_type, instr.rt());

            let quotient = codegen
                .builder
                .build_int_unsigned_div(source, target, "ddivu_quot");
            let remainder = codegen
                .builder
                .build_int_unsigned_rem(source, target, "ddivu_rem");

            codegen.write_register(register::Special::Lo, quotient);
            codegen.write_register(register::Special::Hi, remainder);
        }

        Mnenomic::Divu => {
            // Divide unsigned rs by unsigned rt, store quotient in register LO and remainder in HI
            let source = codegen.read_general_register(i32_type, instr.rs());
            let target = codegen.read_general_register(i32_type, instr.rt());

            let quotient = codegen
                .builder
                .build_int_unsigned_div(source, target, "divu_quot");
            let remainder = codegen
                .builder
                .build_int_unsigned_rem(source, target, "divu_rem");

            codegen.write_register(register::Special::Lo, quotient);
            codegen.write_register(register::Special::Hi, remainder);
        }

        Mnenomic::Or => {
            // OR rs and rt, store result in rd
            let source = codegen.read_general_register(i64_type, instr.rs());
            let target = codegen.read_general_register(i64_type, instr.rt());
            let result = codegen.builder.build_or(source, target, "or_res");
            codegen.write_general_register(instr.rd(), result);
        }

        Mnenomic::Dsll => {
            // Shift rt left by sa bits, store result in rd (64-bits)
            let shift = i64_type.const_int(instr.sa() as _, false);
            let target = codegen.read_general_register(i64_type, instr.rt());
            let result = codegen.builder.build_left_shift(target, shift, "dsll_res");
            codegen.write_general_register(instr.rd(), result);
        }

        Mnenomic::Dsllv => {
            // Shift rt left by rs (limited to 63), store result in rd
            let target = codegen.read_general_register(i64_type, instr.rt());
            let source = {
                let raw = codegen.read_general_register(i64_type, instr.rs());
                let mask = i64_type.const_int(63, false);
                codegen.builder.build_and(raw, mask, "dsllv_and")
            };

            let result = codegen
                .builder
                .build_left_shift(target, source, "dsllv_shift");
            codegen.write_general_register(instr.rd(), result);
        }

        Mnenomic::Sll => {
            // Shift rt left by sa bits, store result in rd (32-bits)
            let shift = i32_type.const_int(instr.sa() as _, false);
            let target = codegen.read_general_register(i32_type, instr.rt());
            let result = codegen.builder.build_left_shift(target, shift, "sll_shift");
            codegen.write_general_register(instr.rd(), result);
        }

        Mnenomic::Sllv => {
            // Shift rt left by the low-order five bits of rs (limited to 31), store result in rd
            let target = codegen.read_general_register(i32_type, instr.rt());
            let source = {
                let raw = codegen.read_general_register(i32_type, instr.rs());
                let mask = i32_type.const_int(0b11111, false);
                codegen.builder.build_and(raw, mask, "sllv_rs_mask")
            };

            let result = codegen.sign_extend_to(
                i64_type,
                codegen
                    .builder
                    .build_left_shift(target, source, "sllv_shift"),
            );

            codegen.write_general_register(instr.rd(), result);
        }

        Mnenomic::Dsll32 => {
            // Shift rt left by (32 + sa) bits, store result in rd
            let target = codegen.read_general_register(i64_type, instr.rt());
            let shift = i64_type.const_int((instr.sa() + 32) as _, false);
            let result = codegen
                .builder
                .build_left_shift(target, shift, "dsll32_shift");
            codegen.write_general_register(instr.rd(), result);
        }

        Mnenomic::Dsrl => {
            // Shift rt right by sa bits, store sign-extended result in rd
            let target = codegen.read_general_register(i64_type, instr.rt());
            let shift = i64_type.const_int(instr.sa() as _, false);
            let result = codegen
                .builder
                .build_right_shift(target, shift, true, "dsrl32_shift");
            codegen.write_general_register(instr.rd(), result);
        }

        Mnenomic::Dsrl32 => {
            // Shift rt right by (32 + sa) bits, store sign-extended result in rd
            let target = codegen.read_general_register(i64_type, instr.rt());
            let shift = i64_type.const_int((instr.sa() + 32) as _, false);
            let result = codegen
                .builder
                .build_right_shift(target, shift, true, "dsrl32_shift");
            codegen.write_general_register(instr.rd(), result);
        }

        Mnenomic::Dsrlv => {
            // Shift rt right by rs (limited to 63), store sign-extended result in rd
            let target = codegen.read_general_register(i64_type, instr.rt());
            let source = {
                let raw = codegen.read_general_register(i64_type, instr.rs());
                let mask = i64_type.const_int(63, false);
                codegen.builder.build_and(raw, mask, "dsrlv_and")
            };

            let result = codegen
                .builder
                .build_right_shift(target, source, true, "dsrlv_shift");
            codegen.write_general_register(instr.rd(), result);
        }

        Mnenomic::Dsra => {
            // Shift rt right by sa bits, store sign-extended result in rd
            let target = codegen.read_general_register(i64_type, instr.rt());
            let shift = i64_type.const_int(instr.sa() as _, false);
            let result = codegen
                .builder
                .build_right_shift(target, shift, true, "dsra32_shift");
            codegen.write_general_register(instr.rd(), result);
        }

        Mnenomic::Dsrav => {
            // Shift rt right by rs (limited to 63), store sign-extended result in rd
            let target = codegen.read_general_register(i64_type, instr.rt());
            let source = {
                let raw = codegen.read_general_register(i64_type, instr.rs());
                let mask = i64_type.const_int(63, false);
                codegen.builder.build_and(raw, mask, "dsrav_and")
            };

            let result = codegen
                .builder
                .build_right_shift(target, source, true, "dsrav_shift");
            codegen.write_general_register(instr.rd(), result);
        }

        Mnenomic::Dsra32 => {
            // Shift rt right by (32 + sa) bits, store sign-extended result in rd
            let target = codegen.read_general_register(i64_type, instr.rt());
            let shift = i64_type.const_int((instr.sa() + 32) as _, false);
            let result = codegen
                .builder
                .build_right_shift(target, shift, true, "dsra32_shift");
            codegen.write_general_register(instr.rd(), result);
        }

        Mnenomic::Srl => {
            // Shift rt right by sa bits, store sign-extended result in rd
            let target = codegen.read_general_register(i32_type, instr.rt());
            let shift = i32_type.const_int(instr.sa() as _, false);
            let result = codegen
                .builder
                .build_right_shift(target, shift, false, "srl_shift");
            codegen.write_general_register(instr.rd(), codegen.sign_extend_to(i64_type, result));
        }

        Mnenomic::Srlv => {
            // Shift rt right by the low-order five bits of rs, store sign-extended result in rd
            let target = codegen.read_general_register(i32_type, instr.rt());
            let source = {
                let value = codegen.read_general_register(i32_type, instr.rs());
                let mask = i32_type.const_int(0b11111, false);
                codegen.builder.build_and(value, mask, "srlv_rs_mask")
            };

            let result = codegen
                .builder
                .build_right_shift(target, source, false, "srlv_shift");
            codegen.write_general_register(instr.rd(), codegen.sign_extend_to(i64_type, result));
        }

        Mnenomic::Sra => {
            // Shift rt right by sa bits, store sign-extended result in rd
            let target = codegen.read_general_register(i32_type, instr.rt());
            let shift = i32_type.const_int(instr.sa() as _, false);

            let result = codegen.sign_extend_to(
                i64_type,
                codegen
                    .builder
                    .build_right_shift(target, shift, true, "sra_shift"),
            );
            codegen.write_general_register(instr.rd(), result);
        }

        Mnenomic::Srav => {
            // Shift rt right by the low-order five bits of rs, store sign-extended result in rd
            let target = codegen.read_general_register(i64_type, instr.rt());
            let source = {
                let value = codegen.read_general_register(i64_type, instr.rs());
                let mask = i64_type.const_int(0b11111, false);
                codegen.builder.build_and(value, mask, "srav_rs_mask")
            };

            let result = {
                let shifted = codegen.truncate_to(
                    i32_type,
                    codegen
                        .builder
                        .build_right_shift(target, source, true, "srav_shift"),
                );

                // Zero out the upper 32 bits
                codegen.sign_extend_to(i64_type, shifted)
            };

            codegen.write_general_register(instr.rd(), result);
        }

        Mnenomic::Dmult => {
            // Multiply signed rs with signed rt, store low-order doubleword in LO, and high-order doubleword to HI
            let source = codegen.zero_extend_to(
                i128_type,
                codegen.read_general_register(i64_type, instr.rs()),
            );
            let target = codegen.zero_extend_to(
                i128_type,
                codegen.read_general_register(i64_type, instr.rt()),
            );
            let result = codegen
                .builder
                .build_int_mul(source, target, "dmult_result");

            // Store the low-order doubleword in LO.
            let lo = codegen
                .builder
                .build_int_truncate(result, i64_type, "dmult_lo_trunc");
            codegen.write_register(register::Special::Lo, lo);

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
            codegen.write_register(register::Special::Hi, hi);
        }

        Mnenomic::Dmultu => {
            // Multiply unsigned rs with unsigned rt, store low-order doubleword in LO, and high-order doubleword to HI
            let source = codegen.zero_extend_to(
                i128_type,
                codegen.read_general_register(i64_type, instr.rs()),
            );
            let target = codegen.zero_extend_to(
                i128_type,
                codegen.read_general_register(i64_type, instr.rt()),
            );
            let result = codegen.builder.build_int_mul(source, target, "dmultu_res");

            // Store the low-order doubleword in LO.
            let lo = codegen
                .builder
                .build_int_truncate(result, i64_type, "dmultu_lo");
            codegen.write_register(register::Special::Lo, lo);

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
            codegen.write_register(register::Special::Hi, hi);
        }

        Mnenomic::Multu => {
            // Multiply unsigned rs by unsigned rt, store low-order word of result in register LO and high-order word in HI
            let source = codegen.zero_extend_to(
                i64_type,
                codegen.read_general_register(i32_type, instr.rs()),
            );
            let target = codegen.zero_extend_to(
                i64_type,
                codegen.read_general_register(i32_type, instr.rt()),
            );
            let result = codegen.builder.build_int_mul(source, target, "multu_res");

            let lo = codegen.truncate_to(i32_type, result);
            codegen.write_register(register::Special::Lo, lo);

            // Store the high-order word in HI.
            let shift = i64_type.const_int(32, false);
            let hi = codegen
                .builder
                .build_right_shift(result, shift, false, "multu_hi");
            codegen.write_register(register::Special::Hi, hi);
        }

        Mnenomic::Mflo => {
            // Copy contents of register LO to rd
            // This should produce incorrect results if any of the two following instructions modify LO register, probably fine to ignore.
            let lo = codegen.read_register(i64_type, register::Special::Lo);
            codegen.write_general_register(instr.rd(), lo);
        }

        Mnenomic::Mfhi => {
            // Copy contents of register HI to rd
            // This should produce incorrect results if any of the two following instructions modify LO register, probably fine to ignore.
            let hi = codegen.read_register(i64_type, register::Special::Hi);
            codegen.write_general_register(instr.rd(), hi);
        }

        Mnenomic::Jr => {
            // Jump to address stored in rs
            let source = codegen.read_general_register(i64_type, instr.rs());
            codegen.build_dynamic_jump(source);
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
                let data = codegen.read_memory(i64_type, addr);

                codegen
                    .builder
                    .build_right_shift(data, shift, false, "lwr_data_shift")
            };

            let result = {
                let reg = codegen.zero_extend_to(
                    i64_type,
                    codegen.read_general_register(i32_type, instr.rt()),
                );
                let anded = codegen.builder.build_and(reg, mask, "lwr_result_and");
                codegen.builder.build_or(anded, data, "lwr_result_or")
            };

            codegen.write_general_register(instr.rt(), result);
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
                codegen.read_memory(i64_type, addr)
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
                let reg = codegen.zero_extend_to(
                    i64_type,
                    codegen.read_general_register(i32_type, instr.rt()),
                );
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

            codegen.write_general_register(instr.rt(), result);
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
                codegen.read_memory(i64_type, addr)
            };

            let shift = {
                let seven = i64_type.const_int(7, false);
                let eight = i64_type.const_int(8, false);

                let xor = codegen.builder.build_xor(address, seven, "sdr_shift_xor");
                let and = codegen.builder.build_and(xor, seven, "sdr_shift_and");
                codegen.builder.build_int_mul(eight, and, "sdr_shift_mul")
            };

            let old_register = {
                let reg = codegen.zero_extend_to(
                    i64_type,
                    codegen.read_general_register(i32_type, instr.rt()),
                );
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

            codegen.write_general_register(instr.rt(), result);
        }

        Mnenomic::Sw => {
            // Stores word from rt, to memory address (base + offset)
            let target = codegen.read_general_register(i32_type, instr.rt());
            let address = codegen.base_plus_offset(instr, "sw_addr");
            codegen.write_memory(address, target);
        }

        Mnenomic::Sh => {
            // Stores halfword from rt, to memory address (base + offset)
            let address = codegen.base_plus_offset(instr, "sh_addr");
            let target = codegen.read_general_register(i16_type, instr.rt());
            codegen.write_memory(address, target);
        }

        Mnenomic::Sb => {
            // Stores least-significant byte from rt, to memory address (base + offset)
            let target = codegen.read_general_register(i8_type, instr.rt());
            let address = codegen.base_plus_offset(instr, "sb_addr");
            codegen.write_memory(address, target);
        }

        Mnenomic::Sd => {
            // Stores doubleword from rt, to memory address (base + offset)
            let target = codegen.read_general_register(i64_type, instr.rt());
            let address = codegen.base_plus_offset(instr, "sd_addr");
            codegen.write_memory(address, target);
        }

        Mnenomic::Ld => {
            // Loads doubleword stored at memory address (base + offset), stores doubleword in rt
            let address = codegen.base_plus_offset(instr, "ld_addr");
            let value = codegen.read_memory(i64_type, address);
            codegen.write_general_register(instr.rt(), value);
        }

        Mnenomic::Lb => {
            // Loads byte stored at memory address (base + offset), stores sign-extended byte in rt
            let address = codegen.base_plus_offset(instr, "lb_addr");
            let value = codegen.sign_extend_to(i64_type, codegen.read_memory(i8_type, address));
            codegen.write_general_register(instr.rt(), value);
        }

        Mnenomic::Lbu => {
            // Loads byte stored at memory address (base + offset), stores zero-extended byte in rt
            let address = codegen.base_plus_offset(instr, "lbu_addr");
            let value = codegen.zero_extend_to(i64_type, codegen.read_memory(i8_type, address));
            codegen.write_general_register(instr.rt(), value);
        }

        Mnenomic::Lw => {
            // Loads word stored at memory address (base + offset), stores sign-extended word in rt
            let address = codegen.base_plus_offset(instr, "lw_addr");
            let value = codegen.sign_extend_to(i64_type, codegen.read_memory(i32_type, address));
            codegen.write_general_register(instr.rt(), value);
        }

        Mnenomic::Lwu => {
            // Loads word stored at memory address (base + offset), stores zero-extended word in rt
            let address = codegen.base_plus_offset(instr, "lwu_addr");
            let value = codegen.zero_extend_to(i64_type, codegen.read_memory(i32_type, address));
            codegen.write_general_register(instr.rt(), value);
        }

        Mnenomic::Lh => {
            // Loads halfword stored at memory address (base + offset), stores sign-extended halfword in rt
            let address = codegen.base_plus_offset(instr, "lh_addr");
            let value = codegen.sign_extend_to(i64_type, codegen.read_memory(i16_type, address));
            codegen.write_general_register(instr.rt(), value);
        }

        Mnenomic::Lhu => {
            // Loads halfword stored at memory address (base + offset), stores zero-extended halfword in rt
            let address = codegen.base_plus_offset(instr, "lhu_addr");
            let value = codegen.zero_extend_to(i64_type, codegen.read_memory(i16_type, address));
            codegen.write_general_register(instr.rt(), value);
        }

        Mnenomic::Lui => {
            // 16-bit immediate is shifted left 16 bits using trailing zeros, result placed in rt
            let immediate = (instr.immediate() << 16) as u64;
            let result = codegen.sign_extend_to(i64_type, i32_type.const_int(immediate, false));
            codegen.write_general_register(instr.rt(), result);
        }

        Mnenomic::Addiu => {
            // Add sign-extended 16-bit immediate and rs, store result in rt
            let immediate =
                codegen.sign_extend_to(i32_type, i16_type.const_int(instr.immediate() as _, true));
            let source = codegen.read_general_register(i32_type, instr.rs());

            let result = codegen.sign_extend_to(
                i64_type,
                codegen
                    .builder
                    .build_int_add(source, immediate, "addiu_res"),
            );

            codegen.write_general_register(instr.rt(), result);
        }

        Mnenomic::Addi => {
            // Add sign-extended 16-bit immediate and rs, store result in rt
            let immediate =
                codegen.sign_extend_to(i32_type, i16_type.const_int(instr.immediate() as _, true));
            let source = codegen.read_general_register(i32_type, instr.rs());
            let result = codegen.builder.build_int_add(source, immediate, "addi_res");
            codegen.write_general_register(instr.rt(), result);
        }

        Mnenomic::Add => {
            // Add rs and rt, store result in rd
            let source = codegen.read_general_register(i32_type, instr.rs());
            let target = codegen.read_general_register(i32_type, instr.rt());

            let result = codegen.sign_extend_to(
                i64_type,
                codegen.builder.build_int_add(source, target, "add_res"),
            );

            codegen.write_general_register(instr.rd(), result);
        }

        Mnenomic::Andi => {
            // AND rs with zero-extended 16-bit immediate, store result in rt
            let immediate =
                codegen.zero_extend_to(i64_type, i16_type.const_int(instr.immediate() as _, false));
            let source = codegen.read_general_register(i64_type, instr.rs());
            let result = codegen.builder.build_and(source, immediate, "andi_res");
            codegen.write_general_register(instr.rt(), result);
        }

        Mnenomic::Ori => {
            // OR rs and zero-extended 16-bit immediate, store result in rt
            let immediate =
                codegen.zero_extend_to(i64_type, i16_type.const_int(instr.immediate() as _, false));
            let source = codegen.read_general_register(i64_type, instr.rs());
            let result = codegen.builder.build_or(source, immediate, "ori_res");
            codegen.write_general_register(instr.rt(), result);
        }

        Mnenomic::Dadd => {
            // Add rs and rt, store result in rd
            let source = codegen.read_general_register(i64_type, instr.rs());
            let target = codegen.read_general_register(i64_type, instr.rt());
            let result = codegen.builder.build_int_add(source, target, "dadd_res");
            codegen.write_general_register(instr.rd(), result);
        }

        Mnenomic::Daddiu => {
            // Add sign-extended 16bit immediate and rs, store result in rt
            let immediate =
                codegen.sign_extend_to(i64_type, i16_type.const_int(instr.immediate() as _, true));
            let source = codegen.read_general_register(i64_type, instr.rs());
            let result = codegen
                .builder
                .build_int_add(source, immediate, "daddiu_res");
            codegen.write_general_register(instr.rt(), result);
        }

        Mnenomic::Slt => {
            // If signed rs is less than signed rt, store one in rd, otherwise store zero
            let source = codegen.read_general_register(i64_type, instr.rs());
            let target = codegen.read_general_register(i64_type, instr.rt());
            let cmp = codegen.build_compare_as_i32(IntPredicate::SLT, source, target, "slt_cmp");
            codegen.write_general_register(instr.rd(), cmp);
        }

        Mnenomic::Slti => {
            // If signed rs is less than sign-extended 16-bit immediate, store one in rd, otherwise store zero
            let imm =
                codegen.sign_extend_to(i32_type, i16_type.const_int(instr.immediate() as _, true));
            let source = codegen.read_general_register(i32_type, instr.rs());
            let cmp = codegen.build_compare_as_i32(IntPredicate::SLT, source, imm, "slti_cmp");
            codegen.write_general_register(instr.rt(), cmp);
        }

        Mnenomic::Sltu => {
            // If unsigned rs is less than unsigned rt, store one in rd, otherwise store zero.
            let source = codegen.read_general_register(i64_type, instr.rs());
            let target = codegen.read_general_register(i64_type, instr.rt());

            let cmp = codegen.zero_extend_to(
                i64_type,
                codegen
                    .builder
                    .build_int_compare(IntPredicate::ULT, source, target, "sltu_cmp"),
            );

            codegen.write_general_register(instr.rd(), cmp);
        }

        Mnenomic::Sltiu => {
            // If unsigned rs is less than sign-extended 16-bit immediate, store one in rt, otherwise store zero
            let imm =
                codegen.sign_extend_to(i64_type, i16_type.const_int(instr.immediate() as _, true));
            let source = codegen.read_general_register(i64_type, instr.rs());
            let cmp = codegen.build_compare_as_i32(IntPredicate::ULT, source, imm, "sltiu_cmp");
            codegen.write_general_register(instr.rt(), cmp);
        }

        Mnenomic::Sub => {
            // Subtract rt from rs, store result in rd
            let source = codegen.read_general_register(i32_type, instr.rs());
            let target = codegen.read_general_register(i32_type, instr.rt());
            let result = codegen.builder.build_int_sub(source, target, "sub_res");
            codegen.write_general_register(instr.rd(), result);
        }

        Mnenomic::Subu => {
            // Subtract rt from rs, store result in rd
            let source = codegen.read_general_register(i32_type, instr.rs());
            let target = codegen.read_general_register(i32_type, instr.rt());

            let result = codegen.sign_extend_to(
                i64_type,
                codegen.builder.build_int_sub(source, target, "subu_res"),
            );

            codegen.write_general_register(instr.rd(), result);
        }

        Mnenomic::Beq => {
            // If rs equals rt, branch to address
            let source = codegen.read_general_register(i64_type, instr.rs());
            let target = codegen.read_general_register(i64_type, instr.rt());
            let cmp =
                codegen
                    .builder
                    .build_int_compare(IntPredicate::EQ, source, target, "beq_cmp");

            let target_pc = instr.try_resolve_static_jump(pc).unwrap();
            codegen.build_conditional_branch(cmp, target_pc);
        }

        Mnenomic::Bne => {
            // If rs is not equal to rt, branch to address.
            let source = codegen.read_general_register(i64_type, instr.rs());
            let target = codegen.read_general_register(i64_type, instr.rt());
            let cmp =
                codegen
                    .builder
                    .build_int_compare(IntPredicate::NE, source, target, "bne_cmp");

            let target_pc = instr.try_resolve_static_jump(pc).unwrap();
            codegen.build_conditional_branch(cmp, target_pc);
        }

        Mnenomic::Bltz => {
            // If rs is less than zero, branch to address (delay slot + offset)
            let source = codegen.read_general_register(i64_type, instr.rs());
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
            let source = codegen.read_general_register(i64_type, instr.rs());
            let zero = i64_type.const_zero();
            let cmp =
                codegen
                    .builder
                    .build_int_compare(IntPredicate::SLT, source, zero, "bltz_cmp");

            let target_pc = instr.try_resolve_static_jump(pc).unwrap();
            let then_block = codegen.build_conditional_branch(cmp, target_pc);
            return Some(then_block);
        }

        Mnenomic::Beql => {
            // If rs equals rt, branch to address (delay slot + offset), otherwise discard delay slot instruction
            let source = codegen.read_general_register(i64_type, instr.rs());
            let target = codegen.read_general_register(i64_type, instr.rt());
            let cmp =
                codegen
                    .builder
                    .build_int_compare(IntPredicate::EQ, source, target, "beql_cmp");

            let target_pc = instr.try_resolve_static_jump(pc).unwrap();
            let then_block = codegen.build_conditional_branch(cmp, target_pc);
            return Some(then_block);
        }

        Mnenomic::Bgezal => {
            // If rs is greater than or equal to zero, branch to address (delay slot + offset) and store next address to r31 (ra)
            let source = codegen.read_general_register(i64_type, instr.rs());
            let zero = i64_type.const_zero();
            let cmp =
                codegen
                    .builder
                    .build_int_compare(IntPredicate::SGE, source, zero, "bgezal_cmp");

            let target_pc = instr.try_resolve_static_jump(pc).unwrap();
            let then_block = codegen.build_conditional_branch(cmp, target_pc);
            return Some(then_block);
        }

        Mnenomic::Bgezall => {
            // If rs is greater than or equal to zero, branch to address (delay slot + offset) and store next address to r31, otherwise discard delay slot instruction
            let source = codegen.read_general_register(i64_type, instr.rs());
            let zero = i64_type.const_zero();
            let cmp =
                codegen
                    .builder
                    .build_int_compare(IntPredicate::SGE, source, zero, "bgezall_cmp");

            let target_pc = instr.try_resolve_static_jump(pc).unwrap();
            let then_block = codegen.build_conditional_branch(cmp, target_pc);
            return Some(then_block);
        }

        Mnenomic::Bltzal => {
            // If rs is less than zero, branch to address (delay slot + offset) and store next address to r31 (ra)
            let source = codegen.read_general_register(i64_type, instr.rs());
            let zero = i64_type.const_zero();
            let cmp =
                codegen
                    .builder
                    .build_int_compare(IntPredicate::SLT, source, zero, "bltzal_cmp");

            let target_pc = instr.try_resolve_static_jump(pc).unwrap();
            let then_block = codegen.build_conditional_branch(cmp, target_pc);
            return Some(then_block);
        }

        Mnenomic::Bltzall => {
            // If rs is less than zero, branch to address (delay slot + offset) and store next address to r31, otherwise discard delay slot instruction
            let source = codegen.read_general_register(i64_type, instr.rs());
            let zero = i64_type.const_zero();
            let cmp =
                codegen
                    .builder
                    .build_int_compare(IntPredicate::SLT, source, zero, "bltzal_cmp");

            let target_pc = instr.try_resolve_static_jump(pc).unwrap();
            let then_block = codegen.build_conditional_branch(cmp, target_pc);
            return Some(then_block);
        }

        Mnenomic::Bnel => {
            // If rs is not equal to rt, branch to address (delay slot + offset), otherwise discard delay slot instruction
            let source = codegen.read_general_register(i64_type, instr.rs());
            let target = codegen.read_general_register(i64_type, instr.rt());
            let cmp =
                codegen
                    .builder
                    .build_int_compare(IntPredicate::NE, source, target, "bnel_cmp");

            let target_pc = instr.try_resolve_static_jump(pc).unwrap();
            let then_block = codegen.build_conditional_branch(cmp, target_pc);
            return Some(then_block);
        }

        Mnenomic::Blez => {
            // If rs is less than or equal to zero, branch to address (delay slot + offset)
            let source = codegen.read_general_register(i64_type, instr.rs());
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
            let source = codegen.read_general_register(i64_type, instr.rs());
            let zero = i64_type.const_zero();
            let cmp =
                codegen
                    .builder
                    .build_int_compare(IntPredicate::SLE, source, zero, "blezl_cmp");

            let target_pc = instr.try_resolve_static_jump(pc).unwrap();
            let then_block = codegen.build_conditional_branch(cmp, target_pc);
            return Some(then_block);
        }

        Mnenomic::Bgezl => {
            // If rs is greater than or equal to zero, branch to address (delay slot + offset), otherwise discard delay slot instruction
            let source = codegen.read_general_register(i64_type, instr.rs());
            let zero = i64_type.const_zero();
            let cmp =
                codegen
                    .builder
                    .build_int_compare(IntPredicate::SGE, source, zero, "blezl_cmp");

            let target_pc = instr.try_resolve_static_jump(pc).unwrap();
            let then_block = codegen.build_conditional_branch(cmp, target_pc);
            return Some(then_block);
        }

        Mnenomic::Bgez => {
            // If rs is greater than or equal to zero, branch to address (delay slot + offset)
            let source = codegen.read_general_register(i64_type, instr.rs());
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
            let source = codegen.read_general_register(i64_type, instr.rs());
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
            let source = codegen.read_general_register(i64_type, instr.rs());
            let zero = i64_type.const_zero();
            let cmp =
                codegen
                    .builder
                    .build_int_compare(IntPredicate::SGT, source, zero, "bgtz_cmp");

            let target_pc = instr.try_resolve_static_jump(pc).unwrap();
            let then_block = codegen.build_conditional_branch(cmp, target_pc);
            return Some(then_block);
        }

        _ => todo!("instruction {} at {pc:#x}", instr.mnemonic().name()),
    };

    None
}
