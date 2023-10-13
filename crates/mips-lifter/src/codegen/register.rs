use super::CodeGen;
use inkwell::{
    types::{FloatType, IntType},
    values::{FloatValue, IntValue, PointerValue},
};
use mips_decomp::register::{self, Register};

/// There are a few coprocessor 0 registers which are reserved, and therefore not properly implemented in hardware.
/// Writing to any of them will fill a latch with the value, which can then be read back from any other reserved register.
/// Because the latch is global across all reserved registers, we arbitrarily pick one to store its contents.
pub(crate) const RESERVED_CP0_REGISTER_LATCH: register::Cp0 = register::Cp0::Reserved7;

/// Coprocessor 2 registers are not implemented, the value is instead stored into one global latch.
/// Since we only ever write to one of the reserved CP0 registers, we can reuse one to store the CP2 register value.
pub(crate) const CP2_REGISTER_LATCH: register::Cp0 = register::Cp0::Reserved21;

impl<'ctx> CodeGen<'ctx> {
    fn fpu_general_register_pointer<T>(&self, ty: IntType<'ctx>, index: T) -> PointerValue<'ctx>
    where
        T: Into<u64>,
    {
        let index = index.into() as usize;
        let i32_type = self.context.i32_type();
        let i64_type = self.context.i64_type();
        let name = |index: usize| -> String {
            format!("{}_", register::Fpu::from_repr(index).unwrap().name())
        };

        // We cannot call register_pointer here since that'll recurse right back to us.
        let pointer_at_index = |index: usize| -> PointerValue<'ctx> {
            unsafe {
                self.builder.build_in_bounds_gep(
                    i64_type,
                    self.globals.registers.fpu.as_pointer_value(),
                    &[i64_type.const_int(index as u64, false)],
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
                else_ptr = Some(match ty.get_bit_width() {
                    32 => {
                        let alignment = index & 1;
                        let base_register = index & !alignment;
                        let name = name(base_register);
                        let ptr = self.builder.build_pointer_cast(
                            pointer_at_index(base_register),
                            i32_type.ptr_type(Default::default()),
                            &format!("{name}lo_ptr_"),
                        );

                        // If the index is odd, we need to add 1 to the pointer to get the high-order 32 bits.
                        unsafe {
                            self.builder.build_in_bounds_gep(
                                i32_type,
                                ptr,
                                &[i32_type.const_int(alignment as u64, false)],
                                &name,
                            )
                        }
                    }

                    64 => pointer_at_index(index & !1), // Round down to the nearest even register.
                    bit_width => todo!("write_fpu_register: bit_width={bit_width}"),
                });
            },
        );

        let result = self
            .builder
            .build_phi(ty.ptr_type(Default::default()), "fpr_ptr");
        result.add_incoming(&[(&then_ptr.unwrap(), then_bb), (&else_ptr.unwrap(), else_bb)]);
        result.as_basic_value().into_pointer_value()
    }

    fn register_pointer<T>(&self, reg: T) -> PointerValue<'ctx>
    where
        T: Into<Register>,
    {
        let reg = reg.into();
        let i64_type = self.context.i64_type();

        let base_ptr = match reg {
            Register::GeneralPurpose(_) => {
                self.globals.registers.general_purpose.as_pointer_value()
            }
            Register::Special(_) => self.globals.registers.special.as_pointer_value(),
            Register::Cp0(_) => self.globals.registers.cp0.as_pointer_value(),
            Register::Fpu(_) => todo!("use fpu_general_register_pointer in register_pointer"),
            Register::FpuControl(_) => self.globals.registers.fpu_control.as_pointer_value(),
        };

        unsafe {
            self.builder.build_in_bounds_gep(
                i64_type,
                base_ptr,
                &[i64_type.const_int(reg.to_repr() as _, false)],
                &format!("{}_ptr", reg.name()),
            )
        }
    }

    /// Read the general-purpose register (GPR) at the given index.
    /// If the `ty` is less than 64 bits the value will be truncated, and the lower bits returned.
    pub fn read_general_register<T>(&self, ty: IntType<'ctx>, index: T) -> IntValue<'ctx>
    where
        T: Into<u64>,
    {
        let reg = register::GeneralPurpose::from_repr(index.into() as _).unwrap();
        if reg == register::GeneralPurpose::Zero {
            // Register zero is hardwired to zero.
            ty.const_zero()
        } else {
            let name = format!("{}_", reg.name());
            let reg_ptr = self.register_pointer(reg);
            self.builder.build_load(ty, reg_ptr, &name).into_int_value()
        }
    }

    /// Read the CP0 register at the given index.
    /// If the `ty` is less than 64 bits the value will be truncated, and the lower bits returned.
    pub fn read_cp0_register<T>(&self, ty: IntType<'ctx>, index: T) -> IntValue<'ctx>
    where
        T: Into<u64>,
    {
        let mut reg = register::Cp0::from_repr(index.into() as usize).unwrap();
        if reg.is_reserved() {
            // Reserved registers use a global latch, redirect to the place we store it.
            reg = RESERVED_CP0_REGISTER_LATCH;
        }

        let name = &format!("{}_", reg.name());
        let register = self.register_pointer(reg);
        self.builder.build_load(ty, register, name).into_int_value()
    }

    /// Read the floating-point unit (FPU) register at the given index.
    /// If the `ty` is less than 64 bits the value will be truncated, and the lower bits returned.
    pub fn read_fpu_register<T>(&self, ty: IntType<'ctx>, index: T) -> IntValue<'ctx>
    where
        T: Into<u64>,
    {
        let index = index.into();
        let name = register::Fpu::from_repr(index as usize).unwrap().name();
        let ptr = self.fpu_general_register_pointer(ty, index);

        self.builder
            .build_load(ty, ptr, &format!("{name}_"))
            .into_int_value()
    }

    pub fn read_fpu_register_float<T>(&self, ty: FloatType<'ctx>, index: T) -> FloatValue<'ctx>
    where
        T: Into<u64>,
    {
        let reg = register::Fpu::from_repr(index.into() as usize).unwrap();
        let name = format!("{}_", reg.name());
        let reg_ptr = self.register_pointer(reg);
        self.builder
            .build_load(ty, reg_ptr, &name)
            .into_float_value()
    }

    /// Read the floating-point unit (FPU) control register at the given index.
    /// If the `ty` is less than 64 bits the value will be truncated, and the lower bits returned.
    pub fn read_fpu_control_register<T>(&self, ty: IntType<'ctx>, index: T) -> IntValue<'ctx>
    where
        T: Into<u64>,
    {
        let reg = register::FpuControl::from_repr(index.into() as usize).unwrap();
        let name = format!("{}_", reg.name());
        let reg_ptr = self.register_pointer(reg);
        self.builder.build_load(ty, reg_ptr, &name).into_int_value()
    }

    /// Read the specified miscellaneous "special" register.
    /// If the `ty` is less than 64 bits the value will be truncated, and the lower bits returned.
    pub fn read_special_register(
        &self,
        ty: IntType<'ctx>,
        reg: register::Special,
    ) -> IntValue<'ctx> {
        let name = &format!("{}_", reg.name());
        let register = self.register_pointer(reg);
        self.builder.build_load(ty, register, name).into_int_value()
    }

    /// Read the coprocessor 2 (CP2) register latch.
    /// If the `ty` is less than 64 bits the value will be truncated, and the lower bits returned.
    pub fn read_cp2_register(&self, ty: IntType<'ctx>) -> IntValue<'ctx> {
        let register = self.register_pointer(CP2_REGISTER_LATCH);
        self.builder
            .build_load(ty, register, "cp2_register_latch_")
            .into_int_value()
    }

    /// Read the specified register.
    /// If the `ty` is less than 64 bits the value will be truncated, and the lower bits returned.
    pub fn read_register<T>(&self, ty: IntType<'ctx>, reg: T) -> IntValue<'ctx>
    where
        T: Into<Register>,
    {
        match reg.into() {
            Register::GeneralPurpose(reg) => self.read_general_register(ty, reg),
            Register::Special(reg) => self.read_special_register(ty, reg),
            Register::Cp0(reg) => self.read_cp0_register(ty, reg),
            Register::Fpu(reg) => self.read_fpu_register(ty, reg),
            Register::FpuControl(reg) => self.read_fpu_control_register(ty, reg),
        }
    }

    pub fn write_general_register<T>(&self, index: T, value: IntValue<'ctx>)
    where
        T: Into<u64>,
    {
        let reg = register::GeneralPurpose::from_repr(index.into() as _).unwrap();
        if reg != register::GeneralPurpose::Zero {
            // Register zero is hardwired to zero.
            let register = self.register_pointer(reg);
            self.builder.build_store(register, value);
        }
    }

    pub fn write_cp0_register<T>(&self, index: T, mut value: IntValue<'ctx>)
    where
        T: Into<u64>,
    {
        let mut reg = register::Cp0::from_repr(index.into() as usize).unwrap();
        match reg {
            register::Cp0::BadVAddr | register::Cp0::PRId | register::Cp0::CacheErr => {
                // Read-only
                return;
            }

            register::Cp0::Config => {
                // All writable get set zero prior to writing.
                let mask = value
                    .get_type()
                    .const_int(register::cp0::Config::WRITE_MASK, false);
                let masked_value = self
                    .builder
                    .build_and(value, mask, "cop0_config_value_mask");
                let masked_config = {
                    let config = self.read_register(value.get_type(), register::Cp0::Config);
                    self.builder
                        .build_and(config, mask.const_not(), "cop0_config_reg_mask")
                };

                value = self
                    .builder
                    .build_or(masked_config, masked_value, "cop0_config_value");
            }

            register::Cp0::Status => {
                // Bit 19 of the Status register is writable, and neither are the upper 32 bits. Mask them out.
                let mask = value
                    .get_type()
                    .const_int(register::cp0::Status::WRITE_MASK, false);
                value = self.builder.build_and(value, mask, "cop0_status_masked");
            }

            register::Cp0::PErr => {
                // Only the lower 8 bits are writable.
                let mask = value
                    .get_type()
                    .const_int(register::cp0::PErr::WRITE_MASK, false);
                value = self.builder.build_and(value, mask, "cop0_perr_masked");
            }

            register::Cp0::LLAddr => {
                // 32-bit register, mask out the upper bits.
                let mask = value
                    .get_type()
                    .const_int(register::cp0::LLAddr::WRITE_MASK, false);
                value = self.builder.build_and(value, mask, "cop0_lladdr_masked");
            }

            register::Cp0::Index => {
                // Not all bits of the Index register are writable, mask them out.
                let mask = value
                    .get_type()
                    .const_int(register::cp0::Index::WRITE_MASK, false);
                value = self.builder.build_and(value, mask, "cop0_index_masked");
            }

            register::Cp0::Wired => {
                // Not all bits of the Wired register are writable, mask them out.
                let mask = value
                    .get_type()
                    .const_int(register::cp0::Wired::WRITE_MASK, false);
                value = self.builder.build_and(value, mask, "cop0_index_masked");
            }

            register::Cp0::Context => {
                // Combine the existent BadVPN2, together with PTEBase from the value.
                let i64_type = self.context.i64_type();
                let masked_value = {
                    let mask = i64_type
                        .const_int(register::cp0::Context::PAGE_TABLE_ENTRY_BASE_MASK, false);
                    self.builder.build_and(
                        self.sign_extend_to(i64_type, value),
                        mask,
                        "cop0_context_value_mask",
                    )
                };

                let masked_context = {
                    let context = self.read_register(i64_type, register::Cp0::Context);
                    let mask = i64_type.const_int(register::cp0::Context::READ_ONLY_MASK, false);
                    self.builder
                        .build_and(context, mask, "cop0_context_reg_mask")
                };

                value = self
                    .builder
                    .build_or(masked_context, masked_value, "cp0_context_value");
            }

            register::Cp0::XContext => {
                // Combine the existent BadVPN2 and ASID, together with PTEBase from the value.
                let i64_type = self.context.i64_type();
                let masked_value = {
                    let mask = i64_type
                        .const_int(register::cp0::XContext::PAGE_TABLE_ENTRY_BASE_MASK, false);
                    self.builder.build_and(
                        self.sign_extend_to(i64_type, value),
                        mask,
                        "cop0_xcontext_value_mask",
                    )
                };

                let masked_context = {
                    let context = self.read_register(i64_type, register::Cp0::XContext);
                    let mask = i64_type.const_int(register::cp0::XContext::READ_ONLY_MASK, false);
                    self.builder
                        .build_and(context, mask, "cop0_xcontext_reg_mask")
                };

                value = self
                    .builder
                    .build_or(masked_context, masked_value, "cp0_xcontext_value");
            }

            _ => {
                if reg.is_reserved() {
                    // Reserved registers store the value in a global latch, redirect to that.
                    reg = RESERVED_CP0_REGISTER_LATCH;
                }
            }
        }

        let reg_ptr = self.register_pointer(reg);
        self.builder.build_store(reg_ptr, value);

        if reg != RESERVED_CP0_REGISTER_LATCH {
            // Any CP0 register write sets the reserved latch as well as the target register.
            let reserved_latch = self.register_pointer(RESERVED_CP0_REGISTER_LATCH);
            self.builder.build_store(reserved_latch, value);
        }
    }

    pub fn write_fpu_register<T>(&self, index: T, value: IntValue<'ctx>)
    where
        T: Into<u64>,
    {
        let ptr = self.fpu_general_register_pointer(value.get_type(), index);
        self.builder.build_store(ptr, value);
    }

    pub fn write_fpu_register_float<T>(&self, index: T, value: FloatValue<'ctx>)
    where
        T: Into<u64>,
    {
        let reg = register::Fpu::from_repr(index.into() as _).unwrap();
        let reg_ptr = self.register_pointer(reg);
        self.builder.build_store(reg_ptr, value);
    }

    pub fn write_fpu_control_register<T>(&self, index: T, mut value: IntValue<'ctx>)
    where
        T: Into<u64>,
    {
        let reg = register::FpuControl::from_repr(index.into() as usize).unwrap();
        match reg {
            register::FpuControl::ControlStatus => {
                value = self.build_mask(
                    value,
                    register::fpu::ControlStatus::WRITE_MASK,
                    "fpu_control_masked",
                );
                self.builder.build_store(self.register_pointer(reg), value);
            }

            // Read-only
            register::FpuControl::ImplementationRevision => {}

            // These are all reserved, should handle this properly at some point.
            _ => todo!("write_fpu_control_register: {reg:?}"),
        }
    }

    pub fn write_special_register(&self, reg: register::Special, value: IntValue<'ctx>) {
        let register = self.register_pointer(reg);
        self.builder.build_store(register, value);
    }

    /// Write the coprocessor 2 (CP2) register latch.
    /// If the `ty` is less than 64 bits the value will be truncated, and the lower bits returned.
    pub fn write_cp2_register(&self, value: IntValue<'ctx>) {
        let register = self.register_pointer(CP2_REGISTER_LATCH);
        self.builder.build_store(register, value);
    }

    pub fn write_register<T>(&self, reg: T, value: IntValue<'ctx>)
    where
        T: Into<Register>,
    {
        match reg.into() {
            Register::GeneralPurpose(reg) => self.write_general_register(reg, value),
            Register::Cp0(reg) => self.write_cp0_register(reg, value),
            Register::Fpu(reg) => self.write_fpu_register(reg, value),
            Register::FpuControl(reg) => self.write_fpu_control_register(reg, value),
            Register::Special(reg) => self.write_special_register(reg, value),
        }
    }
}
