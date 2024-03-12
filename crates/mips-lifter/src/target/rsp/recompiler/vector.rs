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
use std::cmp::min;

const ELEMENTS_MASK: u64 = 0b1111;
/// The amount of elements, in bytes, in a vector register
const ELEMENTS: u64 = 16;
/// The amount of lanes, in u16's, in a vector register
const LANES: u64 = 8;

#[allow(clippy::too_many_lines)]
pub(super) fn compile_instruction(
    codegen: &CodeGen<Rsp>,
    instr: &ParsedInstruction,
) -> CompilationResult<()> {
    let i8_type = codegen.context.i8_type();
    let i16_type = codegen.context.i16_type();
    let i32_type = codegen.context.i32_type();

    let base_plus_offset = |access_size: u32, name: &str| -> CompilationResult<IntValue> {
        let base = codegen.read_general_register(i32_type, instr.base())?;
        let offset = i32_type.const_int(u64::from(instr.offset() * access_size), false);
        Ok(codegen.builder.build_int_add(base, offset, name)?)
    };

    match instr.mnemonic() {
        Mnenomic::Mtc2 => {
            // Copy the lower 16 bits of GPR rt into vector register vd, at byte offset `element`
            let elem = u64::from(instr.element());
            let target = codegen.read_general_register(i16_type, instr.rt())?;
            let mut value = codegen.build_bswap(target, "mtc2_value")?;
            if elem == (ELEMENTS - 1) {
                // If the u16 would not fit, the value gets truncated, unlike mfc2 which wraps around
                value = codegen.truncate_to(i8_type, value)?;
            }

            let offset = i32_type.const_int(elem, false);
            codegen.write_vector_register_byte_offset(instr.vd(), value, offset)?;
        }

        Mnenomic::Mfc2 => {
            // Copy the lower 16 bits of vector register vd, at byte offset `element`, into GPR rd
            let elem = u64::from(instr.element());
            let target = codegen.read_vector_register::<u8>(instr.vt())?;

            // Note that `build_extract_element` performs wrapping, which is desirable here unlike mtc2
            let highest = codegen.build_extract_element(target, elem, "mfc2_value_msb")?;
            let lowest = codegen.build_extract_element(target, elem + 1, "mfc2_value_lsb")?;

            // Combine the two u8's into a single u16, then sign extend it to a u32
            let value = codegen.sign_extend_to(
                i32_type,
                codegen.builder.build_or(
                    codegen.builder.build_left_shift(
                        codegen.zero_extend_to(i16_type, highest)?,
                        i16_type.const_int(8, false),
                        "mfc2_value",
                    )?,
                    codegen.zero_extend_to(i16_type, lowest)?,
                    "mfc2_value",
                )?,
            )?;
            codegen.write_general_register(instr.rd(), value)?;
        }

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

            codegen.build_for_loop(
                "lqv_loop",
                i32_type.const_zero(),
                |i| cmpu!(codegen, i < len),
                |i| codegen.increment(i, 1, "lqv_next_index"),
                |i| {
                    let elem = codegen.builder.build_int_add(elem, i, "lqv_elem")?;
                    let addr = codegen.builder.build_int_add(addr, i, "lqv_addr")?;
                    let value = codegen.read_memory(i8_type, addr)?;
                    codegen.write_vector_register_byte_offset(instr.vt(), value, elem)
                },
            )?;
        }

        Mnenomic::Lrv => {
            // Load (up to) 16 bytes into a VPR, right-aligned
            let addr = base_plus_offset(16, "lrv_addr")?;
            let addr_base = codegen.build_mask(addr, !ELEMENTS_MASK, "lrv_addr_base")?;
            // This might underflow, in which case the loop should not run
            let len = codegen.build_unsigned_saturating_sub(
                codegen.build_mask(addr, ELEMENTS_MASK, "lrv_offset")?,
                i32_type.const_int(u64::from(instr.element()), false),
                "lrv_len",
            )?;

            let elem = {
                let last = i32_type.const_int(ELEMENTS, false);
                codegen.builder.build_int_sub(last, len, "lrv_elem")?
            };

            codegen.build_for_loop(
                "lrv_loop",
                i32_type.const_zero(),
                |i| cmpu!(codegen, i < len),
                |i| codegen.increment(i, 1, "lrv_next_index"),
                |i| {
                    let addr = codegen.builder.build_int_add(addr_base, i, "lrv_addr")?;
                    let value = codegen.read_memory(i8_type, addr)?;
                    let elem = codegen.builder.build_int_add(elem, i, "lrv_elem")?;
                    codegen.write_vector_register_byte_offset(instr.vt(), value, elem)
                },
            )?;
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

            let target = codegen.read_vector_register::<u8>(instr.vt())?;
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

            let target = codegen.read_vector_register::<u8>(instr.vt())?;
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
            let target = codegen.read_vector_register::<u8>(instr.vt())?;
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
            let target = codegen.read_vector_register::<u8>(instr.vt())?;
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
                        i8_type.const_int(1, false),
                        "shv_value_shift",
                    )?;
                    let sign = codegen.builder.build_right_shift(
                        codegen.build_extract_element(target, elem + 1, "shv_sign")?,
                        i8_type.const_int(7, false),
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
            let target = codegen.read_vector_register::<u16>(instr.vt())?;

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
                    let elem = (elem_base & 0b100) | ((elem_base + i) & 0b11);
                    codegen.truncate_to(
                        i8_type,
                        codegen.build_rotate_left(
                            codegen.build_extract_element(target, elem, "sfv_value")?,
                            i16_type.const_int(1, false),
                            "sfv_rotate_value",
                        )?,
                    )?
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
                    let reg = codegen.read_vector_register::<u8>(reg_index)?;
                    codegen.build_extract_element(reg, elem, "stv_value")?
                };

                codegen.write_memory(addr, value)?;
            }
        }

        Mnenomic::Ltv => {
            // Load 16 bytes into 8 different VPRs, starting at VPR vt
            let elem = u64::from(instr.element());
            let addr = base_plus_offset(16, "ltv_addr")?;
            let addr_base = codegen.build_mask(addr, !0b111, "ltv_addr_base")?;
            let addr_offset = codegen.build_mask(addr, 0b1000, "ltv_offset")?;

            for i in 0..16 {
                let addr = codegen.builder.build_int_add(
                    addr_base,
                    codegen.build_mask(
                        codegen.builder.build_int_add(
                            addr_offset,
                            i32_type.const_int(i + elem, false),
                            "ltv_base",
                        )?,
                        ELEMENTS_MASK,
                        "ltv_base_mask",
                    )?,
                    "ltv_addr",
                )?;

                let value = codegen.read_memory(i8_type, addr)?;
                let reg = u64::from(instr.vt() & !0b111) + (((elem >> 1) + (i / 2)) & 0b111);
                codegen.write_vector_register_byte_offset(
                    reg,
                    value,
                    i32_type.const_int(i, false),
                )?;
            }
        }

        Mnenomic::Lpv => {
            // Load 8 8-bit signed values into VPR vt
            compile_packed_load(codegen, instr, "lpv", |i, addr| {
                // Zero-extend the value so that every other byte is zero'd out, overwriting the entire register
                let elem = i32_type.const_int(i << 1, false);
                let value = {
                    let mem = codegen.read_memory(i8_type, addr)?;
                    codegen.zero_extend_to(i16_type, mem)?
                };
                codegen.write_vector_register_byte_offset(instr.vt(), value, elem)
            })?;
        }

        Mnenomic::Luv => {
            // Load 8 8-bit unsigned values into VPR vt
            compile_packed_load(codegen, instr, "luv", |i, addr| {
                let value = codegen.build_rotate_right(
                    codegen.zero_extend_to(i16_type, codegen.read_memory(i8_type, addr)?)?,
                    i16_type.const_int(1, false),
                    "luv_rotate_value",
                )?;

                let elem = i32_type.const_int(i << 1, false);
                codegen.write_vector_register_byte_offset(instr.vt(), value, elem)
            })?;
        }

        Mnenomic::Lhv => {
            // Load 8 8-bit unsigned values into VPR vt, accessing every other byte in memory
            compile_packed_load(codegen, instr, "lhv", |i, addr| {
                let value = codegen.build_rotate_right(
                    codegen.zero_extend_to(i16_type, codegen.read_memory(i8_type, addr)?)?,
                    i16_type.const_int(1, false),
                    "lhv_rotate_value",
                )?;

                let elem = i32_type.const_int(i, false);
                codegen.write_vector_register_byte_offset(instr.vt(), value, elem)
            })?;
        }

        Mnenomic::Lfv => {
            // Load 4 8-bit unsigned values into VPR vt, accessing every fourth byte in memory
            let elem = u64::from(instr.element());
            let addr = base_plus_offset(16, "lfv_addr")?;
            let addr_base = codegen.build_mask(addr, !0b111, "lfv_addr_base")?;
            let addr_offset = {
                let offset = codegen.build_mask(addr, 0b111, "lfv_addr_offset")?;
                codegen.builder.build_int_sub(
                    offset,
                    i32_type.const_int(elem, false),
                    "lfv_addr_offset_with_elem",
                )?
            };

            let mut res = i16_type.vec_type(LANES.try_into().unwrap()).const_zero();
            for i in 0..4 {
                for offset in [0, 8] {
                    let addr = codegen.builder.build_int_add(
                        addr_base,
                        codegen.build_mask(
                            codegen.builder.build_int_add(
                                addr_offset,
                                i32_type.const_int((i << 2) + offset, false),
                                "lfv_base",
                            )?,
                            ELEMENTS_MASK,
                            "lfv_base_mask",
                        )?,
                        "lfv_addr",
                    )?;

                    let value = codegen.build_rotate_right(
                        codegen.zero_extend_to(i16_type, codegen.read_memory(i8_type, addr)?)?,
                        i16_type.const_int(1, false),
                        "lfv_rotate_value",
                    )?;

                    res = codegen.builder.build_insert_element(
                        res,
                        value,
                        i32_type.const_int(i + (offset / 2), false),
                        "lfv_insert_value",
                    )?;
                }
            }

            let res = codegen
                .builder
                .build_bitcast(
                    res,
                    i8_type.vec_type(ELEMENTS.try_into().unwrap()),
                    "lfv_result_as_i8_vec",
                )?
                .into_vector_value();
            for i in elem..min(elem + 8, ELEMENTS) {
                codegen.write_vector_register_byte_offset(
                    instr.vt(),
                    codegen.build_extract_element(res, i, "lfv_extract_result")?,
                    i32_type.const_int(i, false),
                )?;
            }
        }

        Mnenomic::Lwv => {
            // This instruction does nothing, unlike `swv`: no registers are read from or written to
        }

        Mnenomic::Spv | Mnenomic::Suv => {
            // store 8 signed/unsigned 8-bit values from 8 lanes of VPR vt, to memory at (base + (offset * 8))
            compile_packed_store(codegen, instr)?;
        }

        Mnenomic::Sbv | Mnenomic::Ssv | Mnenomic::Slv | Mnenomic::Sdv => {
            // Store `size` bytes from VPR vt, to memory at `base + (offset * size)`
            compile_store(codegen, instr)?;
        }

        Mnenomic::Lsv | Mnenomic::Llv | Mnenomic::Ldv | Mnenomic::Lbv => {
            // Load `size` bytes into VPR vt, from memory at `base + (offset * size)`
            compile_load(codegen, instr)?;
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
        _ => unimplemented!("Vector Unit compile_packed_store: {name}"),
    };

    let i8_type = codegen.context.i8_type();
    let i32_type = codegen.context.i32_type();
    let i16_type = codegen.context.i16_type();
    let base = codegen.read_general_register(i32_type, instr.base())?;
    let target = codegen.read_vector_register::<u16>(instr.vt())?;

    for i in 0..8 {
        let addr = {
            let offset = i32_type.const_int(u64::from((instr.offset() * 8) + i), false);
            let name = &format!("{name}_addr");
            codegen.builder.build_int_add(base, offset, name)?
        };

        let data = {
            let elem_index = instr.element() + i;
            let elem = u64::from(elem_index & 0b111);
            let lane_name = &format!("{name}_lane");
            let lane = codegen.build_extract_element(target, elem, lane_name)?;

            if (elem_index & 0b1000) == shift_sign {
                codegen.truncate_to(
                    i8_type,
                    codegen.build_rotate_left(
                        lane,
                        i16_type.const_int(1, false),
                        &format!("{name}_rotate"),
                    )?,
                )?
            } else {
                codegen.truncate_to(i8_type, lane)?
            }
        };
        codegen.write_memory(addr, data)?;
    }
    Ok(())
}

fn compile_store(codegen: &CodeGen<Rsp>, instr: &ParsedInstruction) -> CompilationResult<()> {
    let name = instr.mnemonic().name();
    let bytes = match instr.mnemonic() {
        Mnenomic::Sbv => 1,
        Mnenomic::Ssv => 2,
        Mnenomic::Slv => 4,
        Mnenomic::Sdv => 8,
        _ => unimplemented!("Vector Unit compile_store: {name}"),
    };

    let i32_type = codegen.context.i32_type();
    let target = codegen.read_vector_register::<u8>(instr.vt())?;
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

fn compile_load(codegen: &CodeGen<Rsp>, instr: &ParsedInstruction) -> CompilationResult<()> {
    let i32_type = codegen.context.i32_type();
    let base = codegen.read_general_register(i32_type, instr.base())?;
    let bytes = match instr.mnemonic() {
        Mnenomic::Lbv => 1,
        Mnenomic::Lsv => 2,
        Mnenomic::Llv => 4,
        Mnenomic::Ldv => 8,
        other => unimplemented!("Vector Unit compile_load: {other:?}"),
    };

    // Read while saturating if the element specifier overflows, unlike store instructions which wrap around
    for i in 0..min(bytes, ELEMENTS - u64::from(instr.element())) {
        let value = {
            let addr_name = &format!("{}_addr", instr.mnemonic().name());
            let offset = i32_type.const_int((u64::from(instr.offset()) * bytes) + i, false);
            let addr = codegen.builder.build_int_add(base, offset, addr_name)?;
            codegen.read_memory(codegen.context.i8_type(), addr)?
        };

        let elem = i32_type.const_int(u64::from(instr.element()) + i, false);
        codegen.write_vector_register_byte_offset(instr.vt(), value, elem)?;
    }
    Ok(())
}

fn compile_packed_load<'ctx>(
    codegen: &CodeGen<'ctx, Rsp>,
    instr: &ParsedInstruction,
    name: &str,
    write: impl Fn(u64, IntValue<'ctx>) -> CompilationResult<()>,
) -> CompilationResult<()> {
    let (access_size, addr_shift) = match instr.mnemonic() {
        Mnenomic::Luv | Mnenomic::Lpv => (8, 0),
        Mnenomic::Lhv => (16, 1),
        other => unimplemented!("Vector Unit compile_packed_load: {other:?}"),
    };

    let i32_type = codegen.context.i32_type();
    let addr = {
        let base = codegen.read_general_register(i32_type, instr.base())?;
        let offset = i32_type.const_int(u64::from(instr.offset()) * access_size, false);
        let name = &format!("{name}_addr");
        codegen.builder.build_int_add(base, offset, name)?
    };

    let addr_base = codegen.build_mask(addr, !0b111, &format!("{name}_addr_base"))?;
    let addr_offset = codegen.builder.build_int_sub(
        codegen.build_mask(addr, 0b111, &format!("{name}_addr_offset"))?,
        i32_type.const_int(u64::from(instr.element()), false),
        &format!("{name}_addr_offset_with_elem"),
    )?;

    for i in (0..8).map(|i| i << addr_shift) {
        let addr = codegen.builder.build_int_add(
            addr_base,
            codegen.build_mask(
                codegen.builder.build_int_add(
                    addr_offset,
                    i32_type.const_int(i, false),
                    &format!("{name}_base_plus_index"),
                )?,
                ELEMENTS_MASK,
                &format!("{name}_base_plus_index_norm"),
            )?,
            &format!("{name}_addr_index"),
        )?;
        write(i, addr)?;
    }
    Ok(())
}
