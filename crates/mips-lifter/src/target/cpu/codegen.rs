//! Code generation helpers

use super::Cpu;
use crate::codegen::{BitWidth, CodeGen, NumericValue};
use inkwell::{
    types::{BasicType, IntType, PointerType},
    values::{IntValue, PointerValue},
    AddressSpace,
};
use mips_decomp::{
    instruction::ParsedInstruction,
    register::{self, Register},
    Exception,
};

/// There are a few coprocessor 0 registers which are reserved, and therefore not properly implemented in hardware.
/// Writing to any of them will fill a latch with the value, which can then be read back from any other reserved register.
/// Because the latch is global across all reserved registers, we arbitrarily pick one to store its contents.
pub(crate) const RESERVED_CP0_REGISTER_LATCH: register::Cp0 = register::Cp0::Reserved7;

/// Coprocessor 2 registers are not implemented, the value is instead stored into one global latch.
/// Since we only ever write to one of the reserved CP0 registers, we can reuse one to store the CP2 register value.
pub(crate) const CP2_REGISTER_LATCH: register::Cp0 = register::Cp0::Reserved21;

impl<'ctx> CodeGen<'ctx, Cpu> {
    /*
       Register accessors
    */

    fn fpu_general_register_pointer(
        &self,
        bit_width: &impl BitWidth,
        ptr_ty: PointerType<'ctx>,
        index: u8,
    ) -> PointerValue<'ctx> {
        let i32_type = self.context.i32_type();
        let i64_type = self.context.i64_type();
        let name = |index: u8| -> String {
            format!("{}_", register::Fpu::from_repr(index).unwrap().name())
        };

        // We cannot call register_pointer here since that'll recurse right back to us.
        let pointer_at_index = |index: u8| -> PointerValue<'ctx> {
            unsafe {
                self.builder.build_in_bounds_gep(
                    i64_type,
                    self.globals().registers.fpu.pointer_value(),
                    &[i64_type.const_int(u64::from(index), false)],
                    &name(index), // This'll bounds check
                )
            }
        };

        // If the FR bit of the Status register is disabled, we have 16 32-bit registers instead of 32 64-bit registers.
        // In the case FR is disabled, the even registers are the upper 32 bits, and the odd registers are the lower 32 bits.
        let fr_set = {
            let status = self.read_register(i64_type, register::Cp0::Status);
            let fr_bit = self.build_mask(status, register::cp0::Status::FR_MASK, "fr_in_status");
            cmp!(self, fr_bit != 0)
        };

        let mut then_ptr = None;
        let mut else_ptr = None;
        let (then_bb, else_bb) = self.build_if_else(
            "fr_in_status",
            fr_set,
            || {
                then_ptr = Some(pointer_at_index(index));
            },
            || {
                else_ptr = Some(match bit_width.bit_width() {
                    32 => {
                        let alignment = index & 1;
                        let base_register = index & !alignment;
                        let name = name(base_register);
                        let ptr = self.builder.build_pointer_cast(
                            pointer_at_index(base_register),
                            i32_type.ptr_type(AddressSpace::default()),
                            &format!("{name}_lo_ptr_"),
                        );

                        // If the index is odd, we need to add 1 to the pointer to get the high-order 32 bits.
                        unsafe {
                            self.builder.build_in_bounds_gep(
                                i32_type,
                                ptr,
                                &[i32_type.const_int(u64::from(alignment), false)],
                                &name,
                            )
                        }
                    }

                    64 => pointer_at_index(index & !1), // Round down to the nearest even register.
                    bit_width => todo!("write_fpu_register: bit_width={bit_width}"),
                });
            },
        );

        let result = self.builder.build_phi(ptr_ty, "fpr_ptr");
        result.add_incoming(&[(&then_ptr.unwrap(), then_bb), (&else_ptr.unwrap(), else_bb)]);
        result.as_basic_value().into_pointer_value()
    }

    /// Read the general-purpose register (GPR) at the given index.
    /// If the `ty` is less than 64 bits the value will be truncated, and the lower bits returned.
    pub fn read_general_register(
        &self,
        ty: IntType<'ctx>,
        index: impl Into<u64>,
    ) -> IntValue<'ctx> {
        let index = u8::try_from(index.into()).expect("index must be less than 32");
        let reg = register::GeneralPurpose::from_repr(index).unwrap();
        if reg == register::GeneralPurpose::Zero {
            // Register zero is hardwired to zero.
            ty.const_zero()
        } else {
            self.read_register_raw(ty, reg)
        }
    }

    /// Read the CP0 register at the given index.
    /// If the `ty` is less than 64 bits the value will be truncated, and the lower bits returned.
    pub fn read_cp0_register(&self, ty: IntType<'ctx>, index: impl Into<u64>) -> IntValue<'ctx> {
        let index = u8::try_from(index.into()).expect("index must be less than 32");
        let mut reg = register::Cp0::from_repr(index).unwrap();
        if reg.is_reserved() {
            // Reserved registers use a global latch, redirect to the place we store it.
            reg = RESERVED_CP0_REGISTER_LATCH;
        }

        self.read_register_raw(ty, reg)
    }

    /// Read the floating-point unit (FPU) register at the given index.
    /// If the `bit_width` is less than 64 bits the value will be truncated, and the lower bits returned.
    pub fn read_fpu_register<T>(&self, bit_width: impl BitWidth, index: impl Into<u64>) -> T
    where
        T: NumericValue<'ctx>,
    {
        let index = u8::try_from(index.into()).expect("index must be less than 32");
        let reg = register::Fpu::from_repr(index).unwrap();
        let tag = T::tag(self.context, &bit_width);
        let ptr = {
            let ptr_type = tag.ptr_type(AddressSpace::default());
            self.fpu_general_register_pointer(&bit_width, ptr_type, index)
        };

        let value = T::try_from(self.read_register_pointer(ptr, tag, reg.into()));
        unsafe { value.unwrap_unchecked() } // SAFETY: We loaded the value with the correct type.
    }

    /// Read the floating-point unit (FPU) control register at the given index.
    /// If the `ty` is less than 64 bits the value will be truncated, and the lower bits returned.
    pub fn read_fpu_control_register(
        &self,
        ty: IntType<'ctx>,
        index: impl Into<u64>,
    ) -> IntValue<'ctx> {
        let index = u8::try_from(index.into()).expect("index must be less than 32");
        let reg = register::FpuControl::from_repr(index).unwrap();
        self.read_register_raw(ty, reg)
    }

    /// Read the specified miscellaneous "special" register.
    /// If the `ty` is less than 64 bits the value will be truncated, and the lower bits returned.
    pub fn read_special_register(
        &self,
        ty: IntType<'ctx>,
        reg: register::Special,
    ) -> IntValue<'ctx> {
        self.read_register_raw(ty, reg)
    }

    /// Read the coprocessor 2 (CP2) register latch.
    /// If the `ty` is less than 64 bits the value will be truncated, and the lower bits returned.
    pub fn read_cp2_register(&self, ty: IntType<'ctx>) -> IntValue<'ctx> {
        self.read_register_raw(ty, CP2_REGISTER_LATCH)
    }

    /// Read the specified register.
    /// If the `ty` is less than 64 bits the value will be truncated, and the lower bits returned.
    pub fn read_register(&self, ty: IntType<'ctx>, reg: impl Into<Register>) -> IntValue<'ctx> {
        match reg.into() {
            Register::GeneralPurpose(reg) => self.read_general_register(ty, reg),
            Register::Special(reg) => self.read_special_register(ty, reg),
            Register::Cp0(reg) => self.read_cp0_register(ty, reg),
            Register::Fpu(reg) => self.read_fpu_register(ty, reg),
            Register::FpuControl(reg) => self.read_fpu_control_register(ty, reg),
        }
    }

    pub fn write_general_register(&self, index: impl Into<u64>, value: IntValue<'ctx>) {
        let index = u8::try_from(index.into()).expect("index must be less than 32");
        let reg = register::GeneralPurpose::from_repr(index).unwrap();
        if reg != register::GeneralPurpose::Zero {
            // Register zero is hardwired to zero.
            self.write_register_raw(reg.into(), value);
        }
    }

    pub fn write_cp0_register(&self, index: impl Into<u64>, mut value: IntValue<'ctx>) {
        use register::cp0::{
            Cause, Compare, Config, Context, Index, LLAddr, PErr, Status, Wired, XContext,
        };
        let i32_type = self.context.i32_type();
        let i64_type = self.context.i64_type();

        let index = u8::try_from(index.into()).expect("index must be less than 32");
        let mut reg = register::Cp0::from_repr(index).unwrap();
        match reg {
            register::Cp0::BadVAddr | register::Cp0::PRId | register::Cp0::CacheErr => {
                // Read-only
                return;
            }

            register::Cp0::Compare => {
                value = self.build_mask(value, Compare::WRITE_MASK, "cp0_compare_masked");

                // Writes also clear the Timer bit of Interrupt Pending in the Cause register.
                let cause = self.read_register(i32_type, register::Cp0::Cause);
                let mask = i32_type.const_int(!Cause::INTERRUPT_PENDING_TIMER_MASK, false);
                let masked_cause = self.builder.build_and(cause, mask, "cp0_cause_masked");
                self.write_register(register::Cp0::Cause, masked_cause);
            }

            register::Cp0::Config => {
                // The writable area gets zeroed out prior to writing, while the rest of the register is preserved.
                let mask = i32_type.const_int(Config::WRITE_MASK, false);
                let new = self.builder.build_and(value, mask, "cp0_config_masked");
                let prev = {
                    let prev = self.read_register(i32_type, register::Cp0::Config);
                    let mask = i32_type.const_int(!Config::WRITE_MASK, false);
                    self.builder.build_and(prev, mask, "cp0_config_prev_masked")
                };
                value = self.builder.build_or(prev, new, "cop0_config_value");
            }

            register::Cp0::Context => {
                // Combine the existent BadVPN2, together with PTEBase from the new value.
                let new = self.build_mask(
                    self.sign_extend_to(i64_type, value),
                    Context::PAGE_TABLE_ENTRY_BASE_MASK,
                    "cp0_context_ptebase_masked",
                );
                let prev = {
                    let context = self.read_register(i64_type, register::Cp0::Context);
                    self.build_mask(context, Context::READ_ONLY_MASK, "cp0_context_prev_masked")
                };
                value = self.builder.build_or(prev, new, "cp0_context_combined");
            }

            register::Cp0::XContext => {
                // Combine the existent BadVPN2 and ASID, together with PTEBase from the new value.
                let new = self.build_mask(
                    self.sign_extend_to(i64_type, value),
                    XContext::PAGE_TABLE_ENTRY_BASE_MASK,
                    "cp0_xcontext_ptebase_masked",
                );
                let prev = self.build_mask(
                    self.read_register(i64_type, register::Cp0::XContext),
                    XContext::READ_ONLY_MASK,
                    "cp0_xcontext_prev_masked",
                );
                value = self.builder.build_or(prev, new, "cp0_xcontext_combined");
            }

            register::Cp0::Status => {
                value = self.build_mask(value, Status::WRITE_MASK, "cp0_status_masked");
            }

            register::Cp0::PErr => {
                value = self.build_mask(value, PErr::WRITE_MASK, "cp0_perr_masked");
            }

            register::Cp0::LLAddr => {
                value = self.build_mask(value, LLAddr::WRITE_MASK, "cp0_lladdr_masked");
            }

            register::Cp0::Index => {
                value = self.build_mask(value, Index::WRITE_MASK, "cp0_index_masked");
            }

            register::Cp0::Wired => {
                value = self.build_mask(value, Wired::WRITE_MASK, "cp0_wired_masked");
            }

            _ => {
                if reg.is_reserved() {
                    // Reserved registers store the value in a global latch, redirect to that.
                    reg = RESERVED_CP0_REGISTER_LATCH;
                }
            }
        }

        self.write_register_raw(reg.into(), value);
        if reg != RESERVED_CP0_REGISTER_LATCH {
            // Any CP0 register write sets the reserved latch as well as the target register.
            self.write_register_raw(RESERVED_CP0_REGISTER_LATCH.into(), value);
        }
    }

    pub fn write_fpu_register<T>(&self, bit_width: impl BitWidth, index: impl Into<u64>, value: T)
    where
        T: NumericValue<'ctx>,
    {
        let index = u8::try_from(index.into()).expect("index must be less than 32");
        let reg = register::Fpu::from_repr(index).unwrap();
        let ptr_type = T::tag(self.context, &bit_width).ptr_type(AddressSpace::default());
        let ptr = self.fpu_general_register_pointer(&bit_width, ptr_type, index);
        self.write_register_pointer(value.as_basic_value_enum(), ptr, reg.into());
    }

    pub fn write_fpu_control_register(&self, index: impl Into<u64>, mut value: IntValue<'ctx>) {
        let index = u8::try_from(index.into()).expect("index must be less than 32");
        let reg = register::FpuControl::from_repr(index).unwrap();
        match reg {
            register::FpuControl::ControlStatus => {
                value = self.build_mask(
                    value,
                    register::fpu::ControlStatus::WRITE_MASK,
                    "fpu_control_masked",
                );
                self.write_register_raw(reg.into(), value);
            }

            // Read-only
            register::FpuControl::ImplementationRevision => {}

            // These are all reserved
            _ => unimplemented!("write_fpu_control_register: {reg:?}"),
        }
    }

    pub fn write_special_register(&self, reg: register::Special, value: IntValue<'ctx>) {
        self.write_register_raw(reg.into(), value);
    }

    /// Write the coprocessor 2 (CP2) register latch.
    /// If the `ty` is less than 64 bits the value will be truncated, and the lower bits returned.
    pub fn write_cp2_register(&self, value: IntValue<'ctx>) {
        self.write_register_raw(CP2_REGISTER_LATCH.into(), value);
    }

    pub fn write_register(&self, reg: impl Into<Register>, value: IntValue<'ctx>) {
        match reg.into() {
            Register::GeneralPurpose(reg) => self.write_general_register(reg, value),
            Register::Cp0(reg) => self.write_cp0_register(reg, value),
            Register::Fpu(reg) => self.write_fpu_register(value.get_type(), reg, value),
            Register::FpuControl(reg) => self.write_fpu_control_register(reg, value),
            Register::Special(reg) => self.write_special_register(reg, value),
        }
    }

    /*
       General helpers
    */

    pub fn base_plus_offset(&self, instr: &ParsedInstruction, add_name: &str) -> IntValue<'ctx> {
        let i16_type = self.context.i16_type();
        let i64_type = self.context.i64_type();

        let base = self.read_general_register(i64_type, instr.base());
        let offset = self.sign_extend_to(
            i64_type,
            i16_type.const_int(u64::from(instr.offset()), true),
        );
        self.builder.build_int_add(base, offset, add_name)
    }

    pub fn assert_coprocessor_usable(&self, coprocessor: u8) {
        use mips_decomp::register::{cp0::Status, Cp0};
        assert!(
            coprocessor <= 3,
            "invalid coprocessor in assert_coprocessor_usable: {coprocessor}"
        );

        let mask = match coprocessor {
            0 => Status::COPROCESSOR_0_ENABLED_MASK,
            1 => Status::COPROCESSOR_1_ENABLED_MASK,
            2 => Status::COPROCESSOR_2_ENABLED_MASK,
            3 => Status::COPROCESSOR_3_ENABLED_MASK,
            _ => unreachable!(),
        };

        let enabled = self.build_mask(
            self.read_register(self.context.i32_type(), Cp0::Status),
            mask,
            &format!("cop{coprocessor}_usable"),
        );

        self.build_if(
            &format!("cop{coprocessor}_unusable"),
            cmp!(self, enabled == 0),
            || {
                self.throw_exception(Exception::CoprocessorUnusable, Some(coprocessor), None);
            },
        );
    }

    /// Builds a check for arithmetic overflow exception, and throws if it should occur.
    pub fn check_overflow_exception(
        &self,
        ty: Overflow,
        lhs: IntValue<'ctx>,
        rhs: IntValue<'ctx>,
        result: IntValue<'ctx>,
    ) {
        // Swap arguments for subtraction so that this function can be used the same way for both.
        let (lhs, rhs) = match ty {
            Overflow::Subtract => (rhs, lhs),
            Overflow::Add => (lhs, rhs),
        };

        // Compare the twos complement sign bits of the operands and the result.
        let overflow = {
            let lhs = {
                let xor = self.builder.build_xor(lhs, rhs, "signed_overflow_lhs_xor");
                match ty {
                    Overflow::Add => self.builder.build_not(xor, "signed_overflow_lhs"),
                    Overflow::Subtract => xor,
                }
            };

            let rhs = self
                .builder
                .build_xor(rhs, result, "signed_overflow_rhs_xor");

            let combined = self.builder.build_and(lhs, rhs, "signed_overflow_and");
            let ty = combined.get_type();
            let shift = ty.const_int((ty.bit_width() as u64 * 8) - 1, false);
            self.builder
                .build_right_shift(combined, shift, false, "signed_overflow_shift")
        };

        self.build_if("signed_overflow", overflow, || {
            self.throw_exception(Exception::ArithmeticOverflow, Some(0), None);
        });
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Overflow {
    Subtract,
    Add,
}
