use crate::{codegen::CodeGen, runtime::RuntimeFunction};
use inkwell::{values::IntValue, FloatPredicate, IntPredicate};
use mips_decomp::{
    instruction::{FloatCondition, FloatFormat, Mnenomic, ParsedInstruction},
    register, Exception, INSTRUCTION_SIZE,
};

fn stub(codegen: &CodeGen, name: &str) {
    let output = format!("{name} instruction not implemented");
    let storage_name = format!("{name}_stub");
    codegen.build_panic(&output, &storage_name);
}

/// A helper to calculate the target for jump instructions (JAL, J).
/// The 26-bit target is shifted left two bits and combined with the high-order four bits of the address of the delay slot.
fn jump_address(delay_slot_pc: u64, target: u32) -> u64 {
    let masked_delay_slot = delay_slot_pc & 0xFFFF_FFFF_F000_0000;
    let shifted_target = (target as u64) << 2;
    shifted_target | masked_delay_slot
}

pub fn compile_instruction_with_delay_slot(
    codegen: &CodeGen,
    pc: u64,
    instr: &ParsedInstruction,
    delay_slot_instr: &ParsedInstruction,
    on_instruction: impl Fn(u64),
) {
    debug_assert!(instr.mnemonic().has_delay_slot());
    let delay_slot_pc = pc + INSTRUCTION_SIZE as u64;

    let compile_delay_slot_instr = || {
        // Update runtime metadata about delay slots so we can properly handle traps and exceptions
        codegen.set_inside_delay_slot(true);
        on_instruction(delay_slot_pc);
        compile_instruction(codegen, delay_slot_instr);
        codegen.set_inside_delay_slot(false);
    };

    if instr.mnemonic().is_branch() {
        // Evaluate the branch condition prior to running the delay slot instruction,
        // as the delay slot instruction can influence the branch condition. TODO: Is this correct?
        let name = &format!("{}_cmp", instr.mnemonic().name());
        let comparison = {
            let (pred, lhs, rhs) = evaluate_conditional_branch(codegen, instr, delay_slot_pc);
            codegen.builder.build_int_compare(pred, lhs, rhs, name)
        };

        if !instr.mnemonic().is_likely_branch() {
            // If the delay slot instruction is not discarded, it gets executed regardless of the branch condition.
            compile_delay_slot_instr();
        }

        on_instruction(pc);
        codegen.build_if(name, comparison, || {
            if instr.mnemonic().is_likely_branch() {
                // If the delay slot is discarded, the delay slot instruction only gets executed when the branch is taken.
                compile_delay_slot_instr();
            }

            let target_pc = instr
                .try_resolve_constant_jump(pc)
                .expect("target address for branch instruction could not be resolved");
            codegen.build_constant_jump(target_pc)
        });
    } else {
        // Recompile the delay slot instruction first, then the unconditional branch
        compile_delay_slot_instr();
        on_instruction(pc);
        compile_unconditional_branch(codegen, instr, delay_slot_pc);
    }
}

fn compile_unconditional_branch(codegen: &CodeGen, instr: &ParsedInstruction, delay_slot_pc: u64) {
    let i64_type = codegen.context.i64_type();
    match instr.mnemonic() {
        Mnenomic::J => {
            // Jump to target address
            let addr = jump_address(delay_slot_pc, instr.immediate());
            codegen.build_constant_jump(addr);
        }

        Mnenomic::Jr => {
            // Jump to address stored in rs. If the address is not aligned to a 4-byte boundary, an address error exception occurs.
            let source = codegen.read_general_register(i64_type, instr.rs());
            codegen.build_if(
                "jr_addr_misaligned",
                {
                    let low_bits = codegen.build_mask(source, 0b11, "jr_addr_low_two_bits");
                    cmp!(codegen, low_bits != 0)
                },
                || {
                    // The exception occurs during the instruction fetch stage after finishing this instruction.
                    // Setting PC here makes the runtime set EPC correctly, since that's how it works for other instructions.
                    codegen.write_register(register::Special::Pc, source);
                    codegen.throw_exception(Exception::AddressLoad, Some(source))
                },
            );

            codegen.build_dynamic_jump(source);
        }

        Mnenomic::Jal => {
            // Jump to target address, stores return address in r31 (ra)
            let return_address = i64_type.const_int(delay_slot_pc + INSTRUCTION_SIZE as u64, false);
            codegen.write_register(register::GeneralPurpose::Ra, return_address);
            let addr = jump_address(delay_slot_pc, instr.immediate());
            codegen.build_constant_jump(addr);
        }

        Mnenomic::Jalr => {
            // Jump to address stored in rs, stores return address in rd
            let return_address = i64_type.const_int(delay_slot_pc + INSTRUCTION_SIZE as u64, false);
            codegen.write_general_register(instr.rd(), return_address);
            let source = codegen.read_general_register(i64_type, instr.rs());
            codegen.build_dynamic_jump(source);
        }

        _ => unreachable!(),
    }
}

fn evaluate_conditional_branch<'ctx>(
    codegen: &CodeGen<'ctx>,
    instr: &ParsedInstruction,
    delay_slot_pc: u64,
) -> (IntPredicate, IntValue<'ctx>, IntValue<'ctx>) {
    let i64_type = codegen.context.i64_type();
    // NOTE: The difference between regular and likely branches is taken care of in `compile_instruction_with_delay_slot`.
    match instr.mnemonic() {
        Mnenomic::Beq | Mnenomic::Beql => {
            // If rs equals rt, branch to address.
            let source = codegen.read_general_register(i64_type, instr.rs());
            let target = codegen.read_general_register(i64_type, instr.rt());
            (IntPredicate::EQ, source, target)
        }

        Mnenomic::Bne | Mnenomic::Bnel => {
            // If rs is not equal to rt, branch to address.
            let source = codegen.read_general_register(i64_type, instr.rs());
            let target = codegen.read_general_register(i64_type, instr.rt());
            (IntPredicate::NE, source, target)
        }

        Mnenomic::Blez | Mnenomic::Blezl => {
            // If rs is less than or equal to zero, branch to address.
            let source = codegen.read_general_register(i64_type, instr.rs());
            let zero = i64_type.const_zero();
            (IntPredicate::SLE, source, zero)
        }

        Mnenomic::Bgez | Mnenomic::Bgezl => {
            // If rs is greater than or equal to zero, branch to address.
            let source = codegen.read_general_register(i64_type, instr.rs());
            (IntPredicate::SGE, source, i64_type.const_zero())
        }

        Mnenomic::Bgezal | Mnenomic::Bgezall => {
            // If rs is greater than or equal to zero, branch to address. Unconditionally stores return address to r31 (ra).
            let return_address = i64_type.const_int(delay_slot_pc + INSTRUCTION_SIZE as u64, false);
            codegen.write_register(register::GeneralPurpose::Ra, return_address);
            let source = codegen.read_general_register(i64_type, instr.rs());
            (IntPredicate::SGE, source, i64_type.const_zero())
        }

        Mnenomic::Bltz | Mnenomic::Bltzl => {
            // If rs is less than zero, branch to address.
            let source = codegen.read_general_register(i64_type, instr.rs());
            (IntPredicate::SLT, source, i64_type.const_zero())
        }

        Mnenomic::Bltzal | Mnenomic::Bltzall => {
            // If rs is less than zero, branch to address. Unconditionally stores return address to r31 (ra).
            let return_address = i64_type.const_int(delay_slot_pc + INSTRUCTION_SIZE as u64, false);
            codegen.write_register(register::GeneralPurpose::Ra, return_address);
            let source = codegen.read_general_register(i64_type, instr.rs());
            (IntPredicate::SLT, source, i64_type.const_zero())
        }

        Mnenomic::Bgtz | Mnenomic::Bgtzl => {
            // If rs is greater than zero, branch to address.
            let source = codegen.read_general_register(i64_type, instr.rs());
            (IntPredicate::SGT, source, i64_type.const_zero())
        }

        Mnenomic::Bc1t | Mnenomic::Bc1tl => {
            // If the last floating-point compare is true, branch to address.
            let source = codegen.read_register(i64_type, register::Fpu::F31);
            let mask = i64_type.const_int(1 << register::fpu::ControlStatus::CONDITION_BIT, false);
            let masked = codegen.builder.build_and(source, mask, "bc1t_mask");
            (IntPredicate::NE, masked, i64_type.const_zero())
        }

        Mnenomic::Bc1f | Mnenomic::Bc1fl => {
            // If the last floating-point compare is false, branch to address.
            let source = codegen.read_register(i64_type, register::Fpu::F31);
            let mask = i64_type.const_int(1 << register::fpu::ControlStatus::CONDITION_BIT, false);
            let masked = codegen.builder.build_and(source, mask, "bc1t_mask");
            (IntPredicate::EQ, masked, i64_type.const_zero())
        }

        _ => todo!("evaluate_conditional_branch: {}", instr.mnemonic().name()),
    }
}

pub fn compile_instruction(codegen: &CodeGen, instr: &ParsedInstruction) -> Option<()> {
    let mnemonic = instr.mnemonic();
    let bool_type = codegen.context.bool_type();
    let i8_type = codegen.context.i8_type();
    let i16_type = codegen.context.i16_type();
    let i32_type = codegen.context.i32_type();
    let i64_type = codegen.context.i64_type();
    let i128_type = codegen.context.i128_type();
    let f32_type = codegen.context.f32_type();
    let f64_type = codegen.context.f64_type();

    match mnemonic {
        Mnenomic::Sync => {
            // Executed as NOP on the VR4300
        }

        Mnenomic::Cache => {
            // Flush instruction or data cache at address (base + offset) to RAM
            // There is no need to emulate the instruction/data cache, so we'll ignore it for now.
        }

        Mnenomic::Eret => {
            // Return from interrupt, exception, or error exception. Unsets the LL bit.
            codegen.write_register(register::Special::LoadLink, bool_type.const_zero());

            let status = codegen.read_register(i64_type, register::Cp0::Status);
            let erl_mask = i64_type.const_int(0b100, false);
            let erl_set = {
                let status = codegen.builder.build_and(status, erl_mask, "eret_erl_mask");
                cmp!(codegen, status == erl_mask)
            };

            codegen.build_if_else(
                "eret_erl_set",
                erl_set,
                || {
                    // Clear the ERL bit of the CP0 register Status to zero.
                    let not_erl = erl_mask.const_not();
                    let new_status = codegen.builder.build_and(status, not_erl, "eret_clear_erl");
                    codegen.write_register(register::Cp0::Status, new_status);
                    // Load the contents of the CP0 register ErrorEPC to the PC
                    let error_epc = codegen.read_register(i64_type, register::Cp0::ErrorEPC);
                    codegen.build_dynamic_jump(error_epc);
                },
                || {
                    // Clear the EXL bit of the CP0 register Status to zero.
                    let not_exl = i64_type.const_int(0b10, false).const_not();
                    let new_status = codegen.builder.build_and(status, not_exl, "eret_clear_exl");
                    codegen.write_register(register::Cp0::Status, new_status);
                    // Load the contents of the CP0 register EPC to the PC
                    let epc = codegen.read_register(i64_type, register::Cp0::EPC);
                    codegen.build_dynamic_jump(epc);
                },
            );
            codegen.builder.build_unreachable();
        }

        Mnenomic::Tlbp => {
            // Searches for a TLB entry that matches the EntryHi register, and sets the Index register to the index of the matching entry.
            env_call!(codegen, RuntimeFunction::ProbeTlbEntry, []);
        }

        Mnenomic::Tlbwi => {
            // Stores the contents of EntryHi and EntryLo registers into the TLB entry pointed at by the Index register
            let index = codegen.read_register(i64_type, register::Cp0::Index);
            env_call!(codegen, RuntimeFunction::WriteTlbEntry, [index]);
        }

        Mnenomic::Tlbr => {
            // Loads EntryHi and EntryLo registers with the TLB entry pointed at by the Index register.
            let index = codegen.read_register(i64_type, register::Cp0::Index);
            env_call!(codegen, RuntimeFunction::ReadTlbEntry, [index]);
        }

        Mnenomic::Ctc1 => {
            // Copy contents of GPR rt, to CP1's control register fcr
            let target = codegen.read_general_register(i64_type, instr.rt());
            codegen.write_fpu_control_register(instr.fcr(), target);
        }

        Mnenomic::Cfc1 => {
            // Copy contents of CP1's control register frc, to GPR rt
            let source = codegen.read_fpu_control_register(i64_type, instr.fcr());
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
            // Copy contents of GPR rt, to CP0 register rd
            let target = codegen.read_general_register(i32_type, instr.rt());
            codegen.write_cp0_register(instr.rd(), target);
        }

        Mnenomic::Mtc1 => {
            // Copy contents of GPR rt, to CP1 register fs
            let target = codegen.read_general_register(i32_type, instr.rt());
            codegen.write_fpu_register(instr.fs(), target);
        }

        Mnenomic::Mfc0 => {
            // Copy contents of CP0 register rd, to GPR rt
            let destination = codegen.read_cp0_register(i32_type, instr.rd());
            codegen
                .write_general_register(instr.rt(), codegen.sign_extend_to(i64_type, destination));
        }

        Mnenomic::Mfc1 => {
            // Copy contents of CP1 register fs, to GPR rt
            let source = codegen.read_fpu_register(i32_type, instr.fs());
            codegen.write_general_register(instr.rt(), codegen.sign_extend_to(i64_type, source));
        }

        Mnenomic::Dmtc0 => {
            // Copy doubleword contents of GPR rt, to CP0 register rd
            let target = codegen.read_general_register(i64_type, instr.rt());
            codegen.write_cp0_register(instr.rd(), target);
        }

        Mnenomic::Dmtc1 => {
            // Copy doubleword contents of GPR rt, to CP1 register fs
            let target = codegen.read_general_register(i64_type, instr.rt());
            codegen.write_fpu_register(instr.fs(), target);
        }

        Mnenomic::Dmfc0 => {
            // Copy doubleword contents of CP0 register rd, to GPR rt
            let destination = codegen.read_cp0_register(i64_type, instr.rd());
            codegen.write_general_register(instr.rt(), destination);
        }

        Mnenomic::Dmfc1 => {
            // Copy doubleword contents of CP1 register fs, to GPR rt
            let source = codegen.read_fpu_register(i64_type, instr.fs());
            codegen.write_general_register(instr.rt(), source);
        }

        Mnenomic::Sc => {
            // If LL bit is set, stores contents of rt, to memory address (base + offset), truncated to 32-bits
            let ll_bit = codegen.read_register(bool_type, register::Special::LoadLink);
            codegen.build_if("sc_ll_set", cmp!(codegen, ll_bit == 1), || {
                let addr = codegen.base_plus_offset(instr, "sc_addr");
                let value = codegen.read_general_register(i32_type, instr.rt());
                codegen.write_memory(addr, value);
            });
        }

        Mnenomic::Scd => {
            // If LL bit is set, stores contents of rt, to memory address (base + offset)
            let ll_bit = codegen.read_register(bool_type, register::Special::LoadLink);
            codegen.build_if("scd_ll_set", cmp!(codegen, ll_bit == 1), || {
                let addr = codegen.base_plus_offset(instr, "scd_addr");
                let value = codegen.read_general_register(i64_type, instr.rt());
                codegen.write_memory(addr, value);
            });
        }

        Mnenomic::Ll => {
            // Loads word stored at memory address (base + offset), stores sign-extended word in rt, and sets the LL bit to 1
            let addr = codegen.base_plus_offset(instr, "ll_addr");
            let value = codegen.read_memory(i32_type, addr);
            let sign_extended = codegen.sign_extend_to(i64_type, value);
            codegen.write_general_register(instr.rt(), sign_extended);
            codegen.write_register(register::Special::LoadLink, bool_type.const_int(1, false));
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

        Mnenomic::Daddi => {
            // Add sign-extended 16bit immediate and rs, store result in rt. If the addition overflows, an integer overflow exception occurs.
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
            let source = codegen.sign_extend_to(
                i64_type,
                codegen.read_general_register(i32_type, instr.rs()),
            );

            let target = codegen.sign_extend_to(
                i64_type,
                codegen.read_general_register(i32_type, instr.rt()),
            );

            codegen.build_if_else(
                "div_zero_divisor",
                cmp!(codegen, target == 0),
                || {
                    codegen.write_register(register::Special::Hi, source);
                    codegen.build_if_else(
                        "div_dividend_positive",
                        cmps!(codegen, source >= 0),
                        || codegen.write_register(register::Special::Lo, i64_type.const_all_ones()),
                        || {
                            codegen
                                .write_register(register::Special::Lo, i64_type.const_int(1, false))
                        },
                    );
                },
                || {
                    let quotient = codegen.sign_extend_to(
                        i64_type,
                        codegen.truncate_to(
                            i32_type,
                            codegen
                                .builder
                                .build_int_signed_div(source, target, "div_quotient"),
                        ),
                    );

                    let remainder = codegen.sign_extend_to(
                        i64_type,
                        codegen.truncate_to(
                            i32_type,
                            codegen
                                .builder
                                .build_int_signed_rem(source, target, "div_remainder"),
                        ),
                    );

                    codegen.write_register(register::Special::Lo, quotient);
                    codegen.write_register(register::Special::Hi, remainder);
                },
            );
        }

        Mnenomic::Divu => {
            // Divide unsigned rs by unsigned rt, store quotient in register LO and remainder in HI
            let source = codegen.read_general_register(i32_type, instr.rs());
            let target = codegen.read_general_register(i32_type, instr.rt());

            codegen.build_if_else(
                "divu_zero_divisor",
                cmp!(codegen, target == 0),
                || {
                    codegen.write_register(register::Special::Lo, i64_type.const_all_ones());
                    codegen.write_register(
                        register::Special::Hi,
                        codegen.sign_extend_to(i64_type, source),
                    );
                },
                || {
                    let quotient = codegen.sign_extend_to(
                        i64_type,
                        codegen.truncate_to(
                            i32_type,
                            codegen
                                .builder
                                .build_int_signed_div(source, target, "divu_quotient"),
                        ),
                    );

                    let remainder = codegen.sign_extend_to(
                        i64_type,
                        codegen.truncate_to(
                            i32_type,
                            codegen
                                .builder
                                .build_int_signed_rem(source, target, "divu_remainder"),
                        ),
                    );

                    codegen.write_register(register::Special::Lo, quotient);
                    codegen.write_register(register::Special::Hi, remainder);
                },
            );
        }

        Mnenomic::Ddiv => {
            // Divide signed rs by signed rt, store quotient in register LO and remainder in HI
            let source = codegen.read_general_register(i64_type, instr.rs());
            let target = codegen.read_general_register(i64_type, instr.rt());

            codegen.build_if_else(
                "ddiv_valid_divisor",
                cmp!(codegen, target == 0),
                || {
                    codegen.write_register(register::Special::Hi, source);
                    codegen.build_if_else(
                        "ddiv_dividend_positive",
                        cmps!(codegen, source >= 0),
                        || codegen.write_register(register::Special::Lo, i64_type.const_all_ones()),
                        || {
                            codegen
                                .write_register(register::Special::Lo, i64_type.const_int(1, false))
                        },
                    );
                },
                || {
                    codegen.build_if_else(
                        "ddiv_dividend_min_int",
                        cmp!(
                            codegen,
                            source == i64_type.const_int(i64::MIN as u64, false)
                        ),
                        || {
                            codegen.write_register(register::Special::Hi, i64_type.const_zero());
                            codegen.write_register(register::Special::Lo, source);
                        },
                        || {
                            let quotient = codegen.builder.build_int_signed_div(
                                source,
                                target,
                                "ddiv_quotient",
                            );
                            let remainder = codegen.builder.build_int_signed_rem(
                                source,
                                target,
                                "ddiv_remainder",
                            );

                            codegen.write_register(register::Special::Lo, quotient);
                            codegen.write_register(register::Special::Hi, remainder);
                        },
                    );
                },
            );
        }

        Mnenomic::Ddivu => {
            // Divide unsigned rs by unsigned rt, store quotient in register LO and remainder in HI
            let source = codegen.read_general_register(i64_type, instr.rs());
            let target = codegen.read_general_register(i64_type, instr.rt());

            codegen.build_if_else(
                "ddivu_zero_divisor",
                cmp!(codegen, target == 0),
                || {
                    codegen.write_register(register::Special::Hi, source);
                    codegen.write_register(register::Special::Lo, i64_type.const_all_ones());
                },
                || {
                    let quotient =
                        codegen
                            .builder
                            .build_int_unsigned_div(source, target, "ddivu_quotient");
                    let remainder =
                        codegen
                            .builder
                            .build_int_unsigned_rem(source, target, "ddivu_remainder");

                    codegen.write_register(register::Special::Lo, quotient);
                    codegen.write_register(register::Special::Hi, remainder);
                },
            );
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
            let result = codegen.sign_extend_to(
                i64_type,
                codegen.builder.build_left_shift(target, shift, "sll_shift"),
            );
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
            let shift = i64_type.const_int((instr.sa() + 32) as u64, false);
            let result = codegen
                .builder
                .build_left_shift(target, shift, "dsll32_shift");
            codegen.write_general_register(instr.rd(), result);
        }

        Mnenomic::Dsrl => {
            // Shift rt right by sa bits, store sign-extended result in rd
            let target = codegen.read_general_register(i64_type, instr.rt());
            let shift = i64_type.const_int(instr.sa() as u64, false);
            let result = codegen
                .builder
                .build_right_shift(target, shift, false, "dsrl_shift");
            codegen.write_general_register(instr.rd(), result);
        }

        Mnenomic::Dsrl32 => {
            // Shift rt right by (32 + sa) bits, store sign-extended result in rd
            let target = codegen.read_general_register(i64_type, instr.rt());
            let shift = i64_type.const_int((instr.sa() + 32) as u64, false);
            let result = codegen
                .builder
                .build_right_shift(target, shift, false, "dsrl32_shift");
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
                .build_right_shift(target, source, false, "dsrlv_shift");
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
            let target = codegen.read_general_register(i64_type, instr.rt());
            let shift = i64_type.const_int(instr.sa() as _, false);

            // Sign-extend the upper 32 bits by truncating to an u32, then sign-extending to an i64
            let result = codegen.truncate_to(
                i32_type,
                codegen
                    .builder
                    .build_right_shift(target, shift, true, "sra_shift"),
            );
            codegen.write_general_register(instr.rd(), codegen.sign_extend_to(i64_type, result));
        }

        Mnenomic::Srav => {
            // Shift rt right by the low-order five bits of rs, store sign-extended result in rd
            let target = codegen.read_general_register(i64_type, instr.rt());
            let source = {
                let value = codegen.read_general_register(i64_type, instr.rs());
                let mask = i64_type.const_int(0b11111, false);
                codegen.builder.build_and(value, mask, "srav_rs_mask")
            };

            // Sign-extend the upper 32 bits by truncating to an u32, then sign-extending to an i64
            let result = codegen.truncate_to(
                i32_type,
                codegen
                    .builder
                    .build_right_shift(target, source, true, "srav_shift"),
            );
            codegen.write_general_register(instr.rd(), codegen.sign_extend_to(i64_type, result));
        }

        Mnenomic::Dmult => {
            // Multiply signed rs with signed rt, store low-order doubleword in LO, and high-order doubleword to HI
            let source = codegen.sign_extend_to(
                i128_type,
                codegen.read_general_register(i64_type, instr.rs()),
            );
            let target = codegen.sign_extend_to(
                i128_type,
                codegen.read_general_register(i64_type, instr.rt()),
            );

            let (hi, lo) =
                codegen.split(codegen.builder.build_int_mul(source, target, "dmult_res"));
            codegen.write_register(register::Special::Hi, codegen.sign_extend_to(i64_type, hi));
            codegen.write_register(register::Special::Lo, codegen.sign_extend_to(i64_type, lo));
        }

        Mnenomic::Dmultu => {
            // Multiply unsigned rs with unsigned rt, store low-order doubleword in LO, and high-order doubleword to HI
            let source = codegen.sign_extend_to(
                i128_type,
                codegen.read_general_register(i64_type, instr.rs()),
            );
            let target = codegen.sign_extend_to(
                i128_type,
                codegen.read_general_register(i64_type, instr.rt()),
            );

            let (hi, lo) =
                codegen.split(codegen.builder.build_int_mul(source, target, "dmultu_res"));
            codegen.write_register(register::Special::Hi, codegen.sign_extend_to(i64_type, hi));
            codegen.write_register(register::Special::Lo, codegen.sign_extend_to(i64_type, lo));
        }

        Mnenomic::Mult => {
            // Multiply signed rs by signed rt, store low-order word of result in register LO and high-order word in HI
            let source = codegen.sign_extend_to(
                i64_type,
                codegen.read_general_register(i32_type, instr.rs()),
            );
            let target = codegen.sign_extend_to(
                i64_type,
                codegen.read_general_register(i32_type, instr.rt()),
            );

            let (hi, lo) = codegen.split(codegen.builder.build_int_mul(source, target, "mult_res"));
            codegen.write_register(register::Special::Hi, codegen.sign_extend_to(i64_type, hi));
            codegen.write_register(register::Special::Lo, codegen.sign_extend_to(i64_type, lo));
        }

        Mnenomic::Multu => {
            // Multiply unsigned rs by unsigned rt, store low-order word of result in register LO and high-order word in HI
            let source = codegen.sign_extend_to(
                i64_type,
                codegen.read_general_register(i32_type, instr.rs()),
            );
            let target = codegen.sign_extend_to(
                i64_type,
                codegen.read_general_register(i32_type, instr.rt()),
            );

            let (hi, lo) =
                codegen.split(codegen.builder.build_int_mul(source, target, "multu_res"));
            codegen.write_register(register::Special::Hi, codegen.sign_extend_to(i64_type, hi));
            codegen.write_register(register::Special::Lo, codegen.sign_extend_to(i64_type, lo));
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

        Mnenomic::Lwl => {
            // Loads a portion of a word beginning at memory address (base + offset), stores 1-4 bytes in high-order portion of rt
            let address = codegen.base_plus_offset(instr, "lwl_addr");

            let shift = {
                let zero = i32_type.const_zero();
                let three = i32_type.const_int(3, false);
                let eight = i32_type.const_int(8, false);
                let xor = codegen.builder.build_xor(address, zero, "lwl_shift_xor");
                let and = codegen.builder.build_and(xor, three, "lwl_shift_and");
                codegen.builder.build_int_mul(eight, and, "lwl_shift_mul")
            };

            let result = {
                let data = {
                    let addr = codegen.builder.build_and(
                        address,
                        i32_type.const_int(!3, false),
                        "lwl_data_and",
                    );
                    let data = codegen.read_memory(i32_type, addr);
                    codegen
                        .builder
                        .build_left_shift(data, shift, "lwl_data_shift")
                };

                let target = {
                    let mask = {
                        let initial = i32_type.const_int(0xFFFF_FFFF, false);
                        let mask = codegen.builder.build_left_shift(initial, shift, "lwl_mask");
                        codegen.builder.build_not(mask, "lwl_mask_not")
                    };

                    let target = codegen.read_general_register(i32_type, instr.rt());
                    codegen.builder.build_and(target, mask, "lwl_result_and")
                };

                codegen.sign_extend_to(
                    i64_type,
                    codegen.builder.build_or(target, data, "lwl_result_or"),
                )
            };

            codegen.write_general_register(instr.rt(), result);
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
            let vaddr = codegen.base_plus_offset(instr, "sdl_addr");
            let paddr = {
                let paddr = codegen.get_physical_address(vaddr);
                codegen.builder.build_and(
                    paddr,
                    i32_type.const_int(7, false).const_not(),
                    "sdl_paddr_masked",
                )
            };

            let shift = {
                let zero = i64_type.const_zero();
                let seven = i64_type.const_int(7, false);
                let eight = i64_type.const_int(8, false);
                let xor = codegen.builder.build_xor(vaddr, zero, "sdl_shift_xor");
                let and = codegen.builder.build_and(xor, seven, "sdl_shift_and");
                codegen.builder.build_int_mul(eight, and, "sdl_shift_mul")
            };

            let result = {
                let data = {
                    let mask = {
                        let raw = codegen.builder.build_right_shift(
                            i64_type.const_all_ones(),
                            shift,
                            false,
                            "sdl_data_mask_shift",
                        );
                        codegen.builder.build_not(raw, "sdl_data_mask_not")
                    };
                    let data = codegen.read_physical_memory(i64_type, paddr);
                    codegen.builder.build_and(data, mask, "sdl_data_and")
                };

                let target = {
                    let rt = codegen.read_general_register(i64_type, instr.rt());
                    codegen
                        .builder
                        .build_right_shift(rt, shift, false, "sdl_rt_shift")
                };

                codegen.builder.build_or(data, target, "sdl_result_or")
            };

            codegen.write_physical_memory(paddr, result);
        }

        Mnenomic::Sdr => {
            // Loads a portion of rt, stores 1-8 bytes in low-order portion of memory address (base + offset)
            let vaddr = codegen.base_plus_offset(instr, "sdr_addr");
            let paddr = {
                let paddr = codegen.get_physical_address(vaddr);
                codegen.builder.build_and(
                    paddr,
                    i32_type.const_int(7, false).const_not(),
                    "sdr_paddr_masked",
                )
            };

            let shift = {
                let seven = i64_type.const_int(7, false);
                let eight = i64_type.const_int(8, false);
                let xor = codegen.builder.build_xor(vaddr, seven, "sdr_shift_xor");
                let and = codegen.builder.build_and(xor, seven, "sdr_shift_and");
                codegen.builder.build_int_mul(eight, and, "sdr_shift_mul")
            };

            let result = {
                let data = {
                    let mask = {
                        let raw = codegen.builder.build_left_shift(
                            i64_type.const_all_ones(),
                            shift,
                            "sdr_data_mask_shift",
                        );
                        codegen.builder.build_not(raw, "sdr_data_mask_not")
                    };

                    let data = codegen.read_physical_memory(i64_type, paddr);
                    codegen.builder.build_and(data, mask, "sdr_data_and")
                };

                let target = {
                    let rt = codegen.read_general_register(i64_type, instr.rt());
                    codegen.builder.build_left_shift(rt, shift, "sdr_rt_shift")
                };

                codegen.builder.build_or(data, target, "sdr_result_or")
            };

            codegen.write_physical_memory(paddr, result);
        }

        Mnenomic::Ldl => {
            // Loads a portion of a doubleword beginning at memory address (base + offset), stores 1-8 bytes in high-order portion of rt.
            let address = codegen.base_plus_offset(instr, "ldl_addr");

            let shift = {
                let zero = i64_type.const_zero();
                let seven = i64_type.const_int(7, false);
                let eight = i64_type.const_int(8, false);
                let xored = codegen.builder.build_xor(address, zero, "ldl_shift_xor");
                let anded = codegen.builder.build_and(xored, seven, "ldl_shift_and");
                codegen.builder.build_int_mul(eight, anded, "ldl_shift_mul")
            };

            let data = {
                let data = {
                    let addr = codegen.builder.build_and(
                        address,
                        i64_type.const_int(!7, false),
                        "ldl_addr_masked",
                    );
                    codegen.read_memory(i64_type, addr)
                };
                codegen
                    .builder
                    .build_left_shift(data, shift, "ldl_data_shifted")
            };

            let old_target = {
                let mask = {
                    let mask = codegen.builder.build_left_shift(
                        i64_type.const_all_ones(),
                        shift,
                        "ldl_rt_mask",
                    );
                    codegen.builder.build_not(mask, "ldl_rt_mask_not")
                };
                let target = codegen.read_general_register(i64_type, instr.rt());
                codegen.builder.build_and(target, mask, "ldl_rt_masked")
            };

            let result = codegen.builder.build_or(old_target, data, "ldl_result");
            codegen.write_general_register(instr.rt(), result);
        }

        Mnenomic::Ldr => {
            // Loads a portion of a doubleword beginning at memory address (base + offset), stores 1-8 bytes in low-order portion of rt.
            let address = codegen.base_plus_offset(instr, "ldr_addr");

            let shift = {
                let seven = i64_type.const_int(7, false);
                let eight = i64_type.const_int(8, false);
                let xored = codegen.builder.build_xor(address, seven, "ldr_shift_xor");
                let anded = codegen.builder.build_and(xored, seven, "ldr_shift_and");
                codegen.builder.build_int_mul(eight, anded, "ldr_shift_mul")
            };

            let data = {
                let data = {
                    let addr = codegen.builder.build_and(
                        address,
                        i64_type.const_int(!7, false),
                        "ldr_addr_masked",
                    );
                    codegen.read_memory(i64_type, addr)
                };
                codegen
                    .builder
                    .build_right_shift(data, shift, false, "ldr_data_shifted")
            };

            let old_target = {
                let mask = {
                    let mask = codegen.builder.build_right_shift(
                        i64_type.const_all_ones(),
                        shift,
                        false,
                        "ldr_rt_mask",
                    );
                    codegen.builder.build_not(mask, "ldr_rt_mask_not")
                };
                let target = codegen.read_general_register(i64_type, instr.rt());
                codegen.builder.build_and(target, mask, "ldr_rt_masked")
            };

            let result = codegen.builder.build_or(old_target, data, "ldr_result");
            codegen.write_general_register(instr.rt(), result);
        }

        Mnenomic::Swl => {
            // Loads a portion of rt, stores 1-4 bytes in high-order portion of memory address (base + offset)
            let address = codegen.base_plus_offset(instr, "swl_addr");

            let shift = {
                let zero = i64_type.const_zero();
                let three = i64_type.const_int(3, false);
                let eight = i32_type.const_int(8, false);
                let xor = codegen.builder.build_xor(address, zero, "swl_shift_xor");
                let and = codegen.truncate_to(
                    i32_type,
                    codegen.builder.build_and(xor, three, "swl_shift_and"),
                );
                codegen.builder.build_int_mul(eight, and, "swl_shift_mul")
            };

            let address_masked = codegen.builder.build_and(
                address,
                i64_type.const_int(!3, false),
                "swl_addr_masked",
            );

            let result = {
                let data = {
                    let mask = {
                        let mask = codegen.builder.build_right_shift(
                            i32_type.const_all_ones(),
                            shift,
                            false,
                            "swl_mask",
                        );
                        codegen.builder.build_not(mask, "swl_mask_not")
                    };

                    let data = codegen.read_memory(i32_type, address_masked);
                    codegen.builder.build_and(data, mask, "swl_data_and")
                };

                let target = {
                    let reg = codegen.read_general_register(i32_type, instr.rt());
                    codegen
                        .builder
                        .build_right_shift(reg, shift, false, "swl_rt_shift")
                };

                codegen.builder.build_or(data, target, "swl_result_or")
            };

            codegen.write_memory(address_masked, result);
        }

        Mnenomic::Swr => {
            // Loads a portion of rt, stores 1-4 bytes in low-order portion of memory address (base + offset)
            let address = codegen.base_plus_offset(instr, "swr_addr");

            let shift = {
                let three = i64_type.const_int(3, false);
                let eight = i32_type.const_int(8, false);
                let xor = codegen.builder.build_xor(address, three, "swr_shift_xor");
                let and = codegen.truncate_to(
                    i32_type,
                    codegen.builder.build_and(xor, three, "swr_shift_and"),
                );
                codegen.builder.build_int_mul(eight, and, "swr_shift_mul")
            };

            let address_masked = codegen.builder.build_and(
                address,
                i64_type.const_int(!3, false),
                "swr_addr_masked",
            );

            let result = {
                let data = {
                    let mask = {
                        let mask = codegen.builder.build_left_shift(
                            i32_type.const_all_ones(),
                            shift,
                            "swr_mask",
                        );
                        codegen.builder.build_not(mask, "swr_mask_not")
                    };

                    let data = codegen.read_memory(i32_type, address_masked);
                    codegen.builder.build_and(data, mask, "swr_data_and")
                };

                let target = {
                    let reg = codegen.read_general_register(i32_type, instr.rt());
                    codegen.builder.build_left_shift(reg, shift, "swr_rt_shift")
                };

                codegen.builder.build_or(data, target, "swr_result_or")
            };

            codegen.write_memory(address_masked, result);
        }

        Mnenomic::Sw => {
            // Stores word from rt, to memory address (base + offset). If the address is not word-aligned, an address error exception occurs.
            let address = codegen.base_plus_offset(instr, "sw_addr");
            codegen.build_if(
                "sw_addr_invalid",
                {
                    let align = {
                        let low = codegen.build_mask(address, 0b11, "sw_addr_low_bits");
                        cmp!(codegen, low != 0)
                    };

                    let upper = {
                        let high = codegen.build_mask(address, u64::MAX << 32, "sw_addr_high_bits");
                        cmp!(codegen, high == 0)
                    };

                    codegen.builder.build_or(align, upper, "sw_addr_invalid")
                },
                || codegen.throw_exception(Exception::AddressStore, Some(address)),
            );

            let target = codegen.read_general_register(i32_type, instr.rt());
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
            // Loads word stored at memory address (base + offset), stores sign-extended word in rt.
            // If the address is not aligned to 4 bytes, or the upper 32 bits of the address are not zero, an address error exception occurs.
            let address = codegen.base_plus_offset(instr, "lw_addr");
            codegen.build_if(
                "lw_addr_invalid",
                {
                    let align = {
                        let low = codegen.build_mask(address, 0b11, "lw_addr_low_bits");
                        cmp!(codegen, low != 0)
                    };

                    let upper = {
                        let high = codegen.build_mask(address, u64::MAX << 32, "lw_addr_high_bits");
                        cmp!(codegen, high == 0)
                    };

                    codegen.builder.build_or(align, upper, "lw_addr_invalid")
                },
                || codegen.throw_exception(Exception::AddressLoad, Some(address)),
            );

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
            // Add sign-extended 16-bit immediate and rs, store sign-extended result in rt
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
            // Add sign-extended 16-bit immediate and rs, store sign-extended result in rt. If the addition overflows, an integer overflow exception occurs.
            // TODO: handle exception
            let immediate =
                codegen.sign_extend_to(i32_type, i16_type.const_int(instr.immediate() as _, true));
            let source = codegen.read_general_register(i32_type, instr.rs());

            let result = codegen.sign_extend_to(
                i64_type,
                codegen.builder.build_int_add(source, immediate, "addi_res"),
            );
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

        Mnenomic::Add => {
            // Add rs and rt, store sign-extended result in rd. If the addition overflows, an integer overflow exception occurs.
            // TODO: handle exception
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
            // Add rs and rt, store result in rd. If the addition overflows, an integer overflow exception occurs.
            // TODO: handle exception
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
            let cmp = codegen.zero_extend_to(i64_type, cmps!(codegen, source < target));
            codegen.write_general_register(instr.rd(), cmp);
        }

        Mnenomic::Slti => {
            // If signed rs is less than sign-extended 16-bit immediate, store one in rd, otherwise store zero
            let immediate =
                codegen.sign_extend_to(i64_type, i16_type.const_int(instr.immediate() as _, true));
            let source = codegen.read_general_register(i64_type, instr.rs());
            let cmp = codegen.zero_extend_to(i64_type, cmps!(codegen, source < immediate));
            codegen.write_general_register(instr.rt(), cmp);
        }

        Mnenomic::Sltu => {
            // If unsigned rs is less than unsigned rt, store one in rd, otherwise store zero.
            let source = codegen.read_general_register(i64_type, instr.rs());
            let target = codegen.read_general_register(i64_type, instr.rt());
            let cmp = codegen.zero_extend_to(i64_type, cmpu!(codegen, source < target));
            codegen.write_general_register(instr.rd(), cmp);
        }

        Mnenomic::Sltiu => {
            // If unsigned rs is less than sign-extended 16-bit immediate, store one in rt, otherwise store zero
            let immediate =
                codegen.sign_extend_to(i64_type, i16_type.const_int(instr.immediate() as _, true));
            let source = codegen.read_general_register(i64_type, instr.rs());
            let cmp = codegen.zero_extend_to(i64_type, cmpu!(codegen, source < immediate));
            codegen.write_general_register(instr.rt(), cmp);
        }

        Mnenomic::Sub => {
            // Subtract rt from rs, store result in rd. If the subtraction overflows, an integer overflow exception occurs.
            // TODO: handle exception
            let source = codegen.read_general_register(i32_type, instr.rs());
            let target = codegen.read_general_register(i32_type, instr.rt());
            let result = codegen.builder.build_int_sub(source, target, "sub_res");
            codegen.write_general_register(instr.rd(), codegen.sign_extend_to(i64_type, result));
        }

        Mnenomic::Subu => {
            // Subtract rt from rs, store result in rd
            let source = codegen.read_general_register(i32_type, instr.rs());
            let target = codegen.read_general_register(i32_type, instr.rt());
            let result = codegen.builder.build_int_sub(source, target, "subu_res");
            codegen.write_general_register(instr.rd(), codegen.sign_extend_to(i64_type, result));
        }

        Mnenomic::Dsub => {
            // Subtract rt from rs, store result in rd. If the subtraction overflows, an integer overflow exception occurs.
            // TODO: handle exception
            let source = codegen.read_general_register(i64_type, instr.rs());
            let target = codegen.read_general_register(i64_type, instr.rt());
            let result = codegen.builder.build_int_sub(source, target, "dsubu_res");
            codegen.write_general_register(instr.rd(), result);
        }

        Mnenomic::Dsubu => {
            // Subtract rt from rs, store result in rd
            let source = codegen.read_general_register(i64_type, instr.rs());
            let target = codegen.read_general_register(i64_type, instr.rt());
            let result = codegen.builder.build_int_sub(source, target, "dsubu_res");
            codegen.write_general_register(instr.rd(), result);
        }

        Mnenomic::Ldc1 => {
            // Copies double-word stored at memory address (base + offset), to CP1 register ft.
            let addr = codegen.base_plus_offset(instr, "lwc1_addr");
            let value = codegen.read_memory(i64_type, addr);
            codegen.write_fpu_register(instr.ft(), value);
        }

        Mnenomic::Lwc1 => {
            // Copies word stored at memory address (base + offset), to CP1 register ft.
            let addr = codegen.base_plus_offset(instr, "lwc1_addr");
            let value = codegen.read_memory(i32_type, addr);
            // TODO: This should go in the upper/lower 32 bits depending on the FR field in CP0 Status
            codegen.write_fpu_register(instr.ft(), value);
        }

        Mnenomic::Swc1 => {
            // Stores word at CP1 register ft, to memory address (base + offset)
            let value = codegen.read_fpu_register(i32_type, instr.ft());
            let addr = codegen.base_plus_offset(instr, "swc1_addr");
            codegen.write_memory(addr, value);
        }

        Mnenomic::Sdc1 => {
            // Stores double-word at CP1 register ft, to memory address (base + offset)
            let value = codegen.read_fpu_register(i64_type, instr.ft());
            let addr = codegen.base_plus_offset(instr, "sdc1_addr");
            codegen.write_memory(addr, value);
        }

        Mnenomic::AddFmt => {
            // CP1 registers fs and ft are added together. The result is stored to CP1 register fd.
            // TODO: is this correct?
            let float_ty = match instr.float_format() {
                FloatFormat::Single | FloatFormat::Word => f32_type,
                FloatFormat::Double | FloatFormat::Long => f64_type,
            };

            let source = codegen.read_fpu_register_float(float_ty, instr.fs());
            let target = codegen.read_fpu_register_float(float_ty, instr.ft());
            let result = codegen.builder.build_float_add(source, target, "add.fmt");
            codegen.write_fpu_register_float(instr.fd(), result);
        }

        Mnenomic::DivFmt => {
            // CP1 register fs is divided by CP1 register ft. The result is stored to CP1 register rd.
            // TODO: is this correct?
            let float_ty = match instr.float_format() {
                FloatFormat::Single | FloatFormat::Word => f32_type,
                FloatFormat::Double | FloatFormat::Long => f64_type,
            };

            let source = codegen.read_fpu_register_float(float_ty, instr.fs());
            let target = codegen.read_fpu_register_float(float_ty, instr.ft());
            let result = codegen
                .builder
                .build_float_div(source, target, "div_fmt_res");
            codegen.write_fpu_register_float(instr.fd(), result);
        }

        Mnenomic::CvtDFmt => {
            // CP1 register fs is converted into a double-precision floating-point format. The result is stored to CP1 register fd.
            // TODO: is this correct?
            let float_ty = match instr.float_format() {
                FloatFormat::Single | FloatFormat::Word => f32_type,
                FloatFormat::Double | FloatFormat::Long => f64_type,
            };

            let source = codegen.read_fpu_register_float(float_ty, instr.fs());
            let result = codegen
                .builder
                .build_float_cast(source, f64_type, "cvt_d_res");
            codegen.write_fpu_register_float(instr.fd(), result);
        }

        Mnenomic::CvtSFmt => {
            // CP1 register fs is converted into a single-precision floating-point format. The result is stored to CP1 register fd.
            // TODO: is this correct?
            let float_ty = match instr.float_format() {
                FloatFormat::Single | FloatFormat::Word => f32_type,
                FloatFormat::Double | FloatFormat::Long => f64_type,
            };

            let source = codegen.read_fpu_register_float(float_ty, instr.fs());
            let result = codegen
                .builder
                .build_float_cast(source, f32_type, "cvt_d_res");
            codegen.write_fpu_register_float(instr.fd(), result);
        }

        Mnenomic::TruncWFmt => {
            // CP1 register fs is arithmetically converted to a 32-bit fixed-point single format. The result is stored to CP1 register fd.
            // TODO: is this correct?
            let float_ty = match instr.float_format() {
                FloatFormat::Single | FloatFormat::Word => f32_type,
                FloatFormat::Double | FloatFormat::Long => f64_type,
            };

            let source = codegen.read_fpu_register_float(float_ty, instr.fs());
            let result = codegen
                .builder
                .build_float_trunc(source, f32_type, "trunc_w_res");
            codegen.write_fpu_register_float(instr.fd(), result);
        }

        Mnenomic::CCondFmt => {
            // Compares CP1 register fs and CP1 register ft using cond. The result is stored to the C bit of FCR31.
            // TODO: is this correct?
            let float_ty = match instr.float_format() {
                FloatFormat::Single | FloatFormat::Word => f32_type,
                FloatFormat::Double | FloatFormat::Long => f64_type,
            };
            let cond = match instr.float_condition() {
                FloatCondition::LessThanOrEqual => FloatPredicate::ULE,
                _ => todo!("float condition {:?}", instr.float_condition()),
            };

            let source = codegen.read_fpu_register_float(float_ty, instr.fs());
            let target = codegen.read_fpu_register_float(float_ty, instr.ft());
            let cmp = codegen
                .builder
                .build_float_compare(cond, source, target, "c_cond_res");

            let shift = i32_type.const_int(register::fpu::ControlStatus::CONDITION_BIT, false);
            let mask = codegen.builder.build_left_shift(cmp, shift, "c_cond_shift");

            let fcr31 = codegen.read_register(i32_type, register::FpuControl::ControlStatus);
            let fcr_with_cmp = codegen.builder.build_and(fcr31, mask, "c_cond_mask");
            codegen.write_register(register::FpuControl::ControlStatus, fcr_with_cmp);
        }

        _ => {
            stub(codegen, &instr.mnemonic().full_name());
            return None;
        }
    };

    Some(())
}
