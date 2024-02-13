//! The RSP Vector Unit, also known as the VU. Documentation can be found in a few places:
//! * [n64brew](https://n64brew.dev/wiki/Reality_Signal_Processor/CPU_Core#Instructions_overview)
//! * [rasky's r64emu](https://github.com/rasky/r64emu/blob/master/doc/rsp.md)

use crate::{
    codegen::{CodeGen, CompilationError, CompilationResult},
    macros::cmpu,
    target::rsp::Rsp,
};
use inkwell::values::IntValue;
use mips_decomp::instruction::{Mnenomic, ParsedInstruction};

const ELEMENTS_MASK: u64 = 0b1111;
const ELEMENTS: u64 = 16;

#[allow(clippy::too_many_lines)]
pub(super) fn compile_instruction(
    codegen: &CodeGen<Rsp>,
    instr: &ParsedInstruction,
) -> CompilationResult<()> {
    let i8_type = codegen.context.i8_type();
    let i32_type = codegen.context.i32_type();
    let vec_type = i8_type.vec_type(16);

    let base_plus_offset = |access_size: u32, name: &str| -> CompilationResult<IntValue> {
        let base = codegen.read_general_register(i32_type, instr.base())?;
        let offset = i32_type.const_int(u64::from(instr.offset() * access_size), false);
        Ok(codegen.builder.build_int_add(base, offset, name)?)
    };

    match instr.mnemonic() {
        Mnenomic::Lqv => {
            // Load (up to) 16 bytes into VPR vt, left-aligned
            let elem = i32_type.const_int(u64::from(instr.element()), false);
            let addr = base_plus_offset(16, "lqv_addr")?;
            let len = {
                // Load up to the nearest boundary aligned to the total amount of elements, starting from the instructions element specifier.
                let align = codegen.builder.build_int_sub(
                    i32_type.const_int(ELEMENTS, false),
                    codegen.build_mask(addr, ELEMENTS_MASK, "lqv_offset")?,
                    "lqv_elem_offset",
                )?;
                let max = i32_type.const_int(ELEMENTS - u64::from(instr.element()), false);
                codegen.build_umin(align, max, "lqv_len")?
            };

            let initial_target = codegen.read_vector_register(instr.vt())?;
            let header_block = codegen.get_insert_block();
            let target = codegen.build_for_loop(
                "lqv_loop",
                i32_type.const_zero(),
                |i| cmpu!(codegen, i <= len),
                |i| codegen.increment(i, 1, "lqv_next_index"),
                |i| {
                    let target_phi = codegen.builder.build_phi(vec_type, "lqv_target")?;
                    let elem = codegen.builder.build_int_add(elem, i, "lqv_elem")?;
                    let addr = codegen.builder.build_int_add(addr, i, "lqv_addr")?;
                    let value = codegen.read_memory(i8_type, addr)?;

                    let target = target_phi.as_basic_value().into_vector_value();
                    let new_target = codegen
                        .builder
                        .build_insert_element(target, value, elem, "lqv_res")?;
                    target_phi.add_incoming(&[
                        (&initial_target, header_block),
                        (&new_target, codegen.get_insert_block()),
                    ]);
                    Ok(target)
                },
            )?;

            codegen.write_vector_register(instr.vt(), target)?;
        }

        Mnenomic::Sqv => {
            // Store (up to) 16 bytes from a VPR, left-aligned
            let elem = i32_type.const_int(u64::from(instr.element()), false);
            let addr = base_plus_offset(16, "sqv_addr")?;
            let size = codegen.builder.build_int_sub(
                i32_type.const_int(ELEMENTS, false),
                codegen.build_mask(addr, ELEMENTS_MASK, "sqv_offset")?,
                "sqv_size",
            )?;

            let target = codegen.read_vector_register(instr.vt())?;
            codegen.build_for_loop(
                "sqv_loop",
                i32_type.const_zero(),
                |i| cmpu!(codegen, i < size),
                |i| codegen.increment(i, 1, "sqv_next_index"),
                |i| {
                    let addr = codegen.builder.build_int_add(addr, i, "sqv_addr")?;
                    let elem = codegen.builder.build_int_add(elem, i, "sqv_elem")?;
                    let value = codegen
                        .builder
                        .build_extract_element(target, elem, "sqv_value")?
                        .into_int_value();
                    codegen.write_memory(addr, value)
                },
            )?;
        }

        Mnenomic::Srv => {
            // Store (up to) 16 bytes from a VPR, right-aligned
            let addr = base_plus_offset(16, "srv_addr")?;
            let addr_base = codegen.build_mask(addr, !ELEMENTS_MASK, "srv_addr_base")?;
            let size = codegen.build_mask(addr, ELEMENTS_MASK, "srv_size")?;
            let elem = {
                let value = i32_type.const_int(ELEMENTS + u64::from(instr.element()), false);
                codegen.builder.build_int_sub(value, size, "srv_elem")?
            };

            let target = codegen.read_vector_register(instr.vt())?;
            codegen.build_for_loop(
                "srv_loop",
                i32_type.const_zero(),
                |i| cmpu!(codegen, i < size),
                |i| codegen.increment(i, 1, "srv_next_index"),
                |i| {
                    let addr = codegen.builder.build_int_add(addr_base, i, "srv_addr")?;
                    let elem = codegen.build_mask(
                        codegen.builder.build_int_add(elem, i, "srv_elem")?,
                        ELEMENTS_MASK,
                        "srv_elem",
                    )?;
                    let value = codegen
                        .builder
                        .build_extract_element(target, elem, "srv_value")?
                        .into_int_value();
                    codegen.write_memory(addr, value)
                },
            )?;
        }

        Mnenomic::Swv => {
            // Store 16 bytes from VPR vt, rotating the vector if the target address is misaligned
            let addr = base_plus_offset(16, "swv_addr")?;
            let addr_base = codegen.build_mask(addr, !0b111, "swv_addr_base")?;
            let addr_offset = codegen.build_mask(addr, 0b111, "swv_offset")?;
            let target = codegen.read_vector_register(instr.vt())?;
            for i in 0..16 {
                let addr = codegen.builder.build_int_add(
                    addr_base,
                    codegen.build_mask(
                        codegen.builder.build_int_add(
                            addr_offset,
                            i32_type.const_int(i, false),
                            "swv_base",
                        )?,
                        ELEMENTS_MASK,
                        "swv_base_mask",
                    )?,
                    "swv_addr",
                )?;

                let elem = (u64::from(instr.element()) + i) & ELEMENTS_MASK;
                let value = codegen.build_extract_element(target, elem, "swv_value")?;
                codegen.write_memory(addr, value)?;
            }
        }

        Mnenomic::Shv => {
            // Store 8 8-bit unsigned values into VPR vt, accessing every other byte in memory
            // TODO: merge this with Spv/Suv?
            let addr = base_plus_offset(16, "shv_addr")?;
            let addr_base = codegen.build_mask(addr, !0b111, "shv_addr_base")?;
            let addr_offset = codegen.build_mask(addr, 0b111, "shv_offset")?;
            let target = codegen.read_vector_register(instr.vt())?;
            for i in (0..8).map(|i| i * 2) {
                let addr = codegen.builder.build_int_add(
                    addr_base,
                    codegen.build_mask(
                        codegen.builder.build_int_add(
                            addr_offset,
                            i32_type.const_int(i, false),
                            "shv_base",
                        )?,
                        ELEMENTS_MASK,
                        "shv_base_mask",
                    )?,
                    "shv_addr",
                )?;

                let value = {
                    let elem = u64::from(instr.element()) + i;
                    let value = codegen.builder.build_left_shift(
                        codegen.build_extract_element(target, elem, "shv_value")?,
                        i32_type.const_int(1, false),
                        "shv_value_shift",
                    )?;
                    let sign = codegen.builder.build_right_shift(
                        codegen.build_extract_element(target, elem + 1, "shv_sign")?,
                        i32_type.const_int(7, false),
                        false,
                        "shv_sign_shift",
                    )?;
                    codegen
                        .builder
                        .build_or(value, sign, "shv_combine_sign_value")?
                };

                codegen.write_memory(addr, value)?;
            }
        }

        Mnenomic::Sfv => {
            // Store 4 8-bit unsigned values from VPR vt, accessing every fourth byte in memory
            let addr = base_plus_offset(16, "sfv_addr")?;
            let addr_base = codegen.build_mask(addr, !0b111, "sfv_addr_base")?;
            let addr_offset = codegen.build_mask(addr, 0b111, "sfv_addr_offset")?;
            let target = codegen.read_vector_register(instr.vt())?;

            // The starting element is hardcoded depending on E. The three next ones can be determined by adding 1,2,3, but staying within the vector half
            let elem_base = match instr.element() {
                0 | 15 => Some(0),
                1 => Some(6),
                4 => Some(1),
                5 => Some(7),
                8 => Some(4),
                11 => Some(3),
                12 => Some(5),
                _ => None,
            };

            for i in 0..4 {
                let addr = codegen.builder.build_int_add(
                    addr_base,
                    codegen.build_mask(
                        codegen.builder.build_int_add(
                            addr_offset,
                            i32_type.const_int(i << 2, false),
                            "sfv_base",
                        )?,
                        ELEMENTS_MASK,
                        "sfv_base_mask",
                    )?,
                    "sfv_addr",
                )?;

                let value = if let Some(elem_base) = elem_base {
                    let elem = ((elem_base & 0b100) | ((elem_base + i) & 0b11)) << 1;
                    let data = codegen.builder.build_left_shift(
                        codegen.build_extract_element(target, elem, "sfv_data")?,
                        i32_type.const_int(1, false),
                        "sfv_data_shift",
                    )?;
                    let sign = codegen.builder.build_right_shift(
                        codegen.build_extract_element(target, elem + 1, "sfv_sign")?,
                        i32_type.const_int(7, false),
                        false,
                        "sfv_sign_shift",
                    )?;
                    codegen.builder.build_or(data, sign, "sfv_value")?
                } else {
                    // Invalid element specifier
                    i8_type.const_zero()
                };

                codegen.write_memory(addr, value)?;
            }
        }

        Mnenomic::Stv => {
            // Store 16 bytes from 8 different VPRs, starting at VPR vt
            let addr = base_plus_offset(16, "stv_addr")?;
            let addr_base = codegen.build_mask(addr, !0b111, "stv_addr_base")?;
            let addr_offset = codegen.builder.build_int_sub(
                codegen.build_mask(addr, 0b111, "stv_addr_offset")?,
                i32_type.const_int(u64::from(instr.element()) & !0b1, false),
                "stv_offset",
            )?;

            for i in 0..16 {
                let addr = codegen.builder.build_int_add(
                    addr_base,
                    codegen.build_mask(
                        codegen.builder.build_int_add(
                            addr_offset,
                            i32_type.const_int(i, false),
                            "stv_base",
                        )?,
                        ELEMENTS_MASK,
                        "stv_base_mask",
                    )?,
                    "stv_addr",
                )?;

                let value = {
                    let elem = ((16 - (u64::from(instr.element()) & !0b1)) + i) & ELEMENTS_MASK;
                    let reg_index = u64::from(instr.vt() & !0b111) + (i / 2);
                    let reg = codegen.read_vector_register(reg_index)?;
                    codegen.build_extract_element(reg, elem, "stv_value")?
                };

                codegen.write_memory(addr, value)?;
            }
        }

        Mnenomic::Spv | Mnenomic::Suv => {
            // store 8 signed/unsigned 8-bit values from 8 lanes of VPR vt, to memory at (base + (offset * 8))
            compile_packed_store(codegen, instr)?;
        }

        Mnenomic::Sbv | Mnenomic::Ssv | Mnenomic::Slv | Mnenomic::Sdv => {
            // Store a certain amount of bytes from a VPR, into memory at (base + (offset * bytes))
            compile_vector_store(codegen, instr)?;
        }

        _ => {
            let name = instr.mnemonic().full_name();
            codegen.build_stub(&format!("vector instruction: {name}"))?;
            return Err(CompilationError::UnimplementedInstruction(name));
        }
    }

    Ok(())
}

fn compile_packed_store(
    codegen: &CodeGen<Rsp>,
    instr: &ParsedInstruction,
) -> CompilationResult<()> {
    let name = instr.mnemonic().name();
    let shift_sign = match instr.mnemonic() {
        Mnenomic::Spv => 0b1000,
        Mnenomic::Suv => 0,
        _ => unimplemented!("RSP compile_packed_store: {name}"),
    };

    let i8_type = codegen.context.i8_type();
    let i32_type = codegen.context.i32_type();
    let i16_vec_type = codegen.context.i16_type().vec_type(8);
    let base = codegen.read_general_register(i32_type, instr.base())?;
    let target = {
        // TODO: implement a way to request the vector type from `read_vector_register`
        let i8_vec = codegen.read_vector_register(instr.vt())?;
        codegen
            .builder
            .build_bitcast(i8_vec, i16_vec_type, &format!("{name}_target"))?
            .into_vector_value()
    };

    for i in 0..8 {
        let addr = {
            let offset = i32_type.const_int(u64::from((instr.offset() * 8) + i), false);
            let name = &format!("{name}_addr");
            codegen.builder.build_int_add(base, offset, name)?
        };

        let data = {
            let elem_index = instr.element() + i;
            let lane_name = &format!("{name}_lane");
            let lane =
                codegen.build_extract_element(target, u64::from(elem_index & 0b111), lane_name)?;

            let mut data = codegen.truncate_to(i8_type, lane)?;
            if (elem_index & 0b1000) == shift_sign {
                // Take the most significant bit of the lane and shift it to the least significant of the data
                let sign = codegen.truncate_to(
                    i8_type,
                    codegen.build_mask(
                        codegen.builder.build_right_shift(
                            lane,
                            i32_type.const_int(15, false),
                            false,
                            &format!("{name}_lane_sign"),
                        )?,
                        1,
                        &format!("{name}_lane_sign_mask"),
                    )?,
                )?;

                data = codegen.builder.build_or(
                    codegen.builder.build_left_shift(
                        data,
                        i32_type.const_int(1, false),
                        &format!("{name}_data_pos"),
                    )?,
                    sign,
                    &format!("{name}_combine_sign"),
                )?;
            }
            data
        };
        codegen.write_memory(addr, data)?;
    }
    Ok(())
}

fn compile_vector_store(
    codegen: &CodeGen<Rsp>,
    instr: &ParsedInstruction,
) -> CompilationResult<()> {
    let name = instr.mnemonic().name();
    let bytes = match instr.mnemonic() {
        Mnenomic::Sbv => 1,
        Mnenomic::Ssv => 2,
        Mnenomic::Slv => 4,
        Mnenomic::Sdv => 8,
        _ => unimplemented!("RSP compile_vector_store: {name}"),
    };

    let i32_type = codegen.context.i32_type();
    let target = codegen.read_vector_register(instr.vt())?;
    let base = codegen.read_general_register(i32_type, instr.base())?;
    for i in 0..bytes {
        let addr = {
            let name = &format!("{name}_addr");
            let offset = i32_type.const_int(u64::from((instr.offset() * bytes) + i), false);
            codegen.builder.build_int_add(base, offset, name)?
        };
        let data = {
            let name = &format!("{name}_data");
            codegen.build_extract_element(target, u64::from(instr.element() + i), name)?
        };
        codegen.write_memory(addr, data)?;
    }
    Ok(())
}
