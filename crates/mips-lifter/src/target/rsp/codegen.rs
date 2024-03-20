use super::{register, Rsp};
use crate::{
    codegen::{CodeGen, CompilationResult},
    macros::env_call,
    runtime::RuntimeFunction,
    target::Globals,
};
use inkwell::{
    context::Context,
    types::{IntType, VectorType},
    values::{IntValue, PointerValue, VectorValue},
};

impl<'ctx> CodeGen<'ctx, Rsp> {
    /// Get the pointer to the DMA register for the current buffer.
    fn dma_register_pointer(
        &self,
        reg: register::Control,
    ) -> CompilationResult<PointerValue<'ctx>> {
        debug_assert!(reg.is_dma_register());
        let i32_type = self.context.i32_type();
        let reg = reg.to_lower_buffer();
        let ptr = self.globals().registers.pointer_value(self, &reg.into());
        let offset = {
            // Either zero or DMA_BUFFER_OFFSET, depending on the busy bit
            let busy = self.read_control_register(i32_type, register::Control::DmaBusy)?;
            let offset = register::Control::DMA_BUFFER_OFFSET as u64;
            let mul = i32_type.const_int(offset, false);
            self.builder.build_int_mul(busy, mul, "dma_buffer_offset")?
        };

        // SAFETY: Adding zero or DMA_BUFFER_OFFSET to the lower register pointer is always in-bounds, it refers to the upper/lower buffer.
        Ok(unsafe {
            self.builder.build_in_bounds_gep(
                i32_type,
                ptr,
                &[offset],
                &format!("{}_dma_active_buf", reg.name()),
            )?
        })
    }

    /// Extracts the element at the given index from the given vector, returning it as an integer.
    pub fn build_extract_element(
        &self,
        vec: VectorValue<'ctx>,
        elem: u64,
        name: &str,
    ) -> CompilationResult<IntValue<'ctx>> {
        let elem = {
            // The element field is limited to 15, and wraps around.
            let value = elem & 0b1111;
            self.context.i32_type().const_int(value, false)
        };
        Ok(self
            .builder
            .build_extract_element(vec, elem, name)?
            .into_int_value())
    }

    /// Read the control (CP0) register at the given index.
    pub fn read_control_register(
        &self,
        ty: IntType<'ctx>,
        index: impl Into<u64>,
    ) -> CompilationResult<IntValue<'ctx>> {
        debug_assert!(ty.get_bit_width() <= 32);
        #[allow(clippy::cast_possible_truncation)]
        let reg = register::Control::from_repr(index.into() as u8).unwrap();
        match reg {
            register::Control::DmaBusy | register::Control::DmaFull => {
                // These are mirrors of certain bits of the status register
                let status = {
                    let reg = register::Control::Status;
                    self.read_register_raw(ty, reg)?.into_int_value()
                };

                let shifted = {
                    // Shift the appropriate bit from the status register into the LSB.
                    let amount = match reg {
                        register::Control::DmaBusy => register::control::Status::DMA_BUSY_SHIFT,
                        register::Control::DmaFull => register::control::Status::DMA_FULL_SHIFT,
                        _ => unreachable!(),
                    };
                    let shift = ty.const_int(amount, false);
                    let name = &format!("{}_shift_to_lsb", reg.name());
                    self.builder.build_right_shift(status, shift, false, name)?
                };

                // Mask off the irrelevant bits
                let name = &format!("{}_mask_status_bit", reg.name());
                self.build_mask(shifted, 0b1, name)
            }

            register::Control::Semaphore => {
                // After reading the semaphore it is set to 1, regardless of its previous state
                let value = self.read_register_raw(ty, reg)?.into_int_value();
                self.write_register_raw(reg, ty.const_int(1, false))?;
                Ok(value)
            }

            _ if reg.is_dma_register() => {
                let ptr = self.dma_register_pointer(reg)?;
                self.read_register_pointer(ptr, ty, reg.into())
                    .map(inkwell::values::BasicValueEnum::into_int_value)
            }

            _ => Ok(self.read_register_raw(ty, reg)?.into_int_value()),
        }
    }

    /// Read the vector (CP2) register at the given index.
    pub fn read_vector_register<T: VectorRegisterElement<'ctx>>(
        &self,
        index: impl Into<u64>,
    ) -> CompilationResult<VectorValue<'ctx>> {
        #[allow(clippy::cast_possible_truncation)]
        let reg = register::Vector::from_repr(index.into() as u8).unwrap();
        let ty = T::vector_type(self.context);
        Ok(self.read_register_raw(ty, reg)?.into_vector_value())
    }

    pub fn read_flags_register(
        &self,
        ty: IntType<'ctx>,
        index: impl Into<u64>,
    ) -> CompilationResult<IntValue<'ctx>> {
        #[allow(clippy::cast_possible_truncation)]
        let reg = register::Flags::new(index.into() as u8);
        Ok(self.read_register_raw(ty, reg)?.into_int_value())
    }

    /// Write the control (CP0) register at the given index.
    pub fn write_control_register(
        &self,
        index: impl Into<u64>,
        value: IntValue<'ctx>,
    ) -> CompilationResult<()> {
        #[allow(clippy::cast_possible_truncation)]
        let reg = register::Control::from_repr(index.into() as u8).unwrap();
        let i32_type = self.context.i32_type();
        match reg {
            register::Control::DmaBusy | register::Control::DmaFull => {
                // Read-only mirrors of the status register
                Ok(())
            }

            register::Control::Semaphore => {
                // When writing to the semaphore it is set to 0, regardless of the given value
                let zero = i32_type.const_zero();
                self.write_register_raw(reg, zero)
            }

            register::Control::Status => {
                // Writing the status register may set the halted bit, so we let the runtime handle it.
                env_call!(&self, RuntimeFunction::RspWriteStatus, [value])?;
                Ok(())
            }

            _ if reg.is_dma_register() => {
                let ptr = self.dma_register_pointer(reg)?;
                self.write_register_pointer(value.into(), ptr, reg.into())?;
                match reg.to_lower_buffer() {
                    register::Control::DmaReadLength1 => {
                        // Request a DMA from RDRAM
                        let to_rdram = self.context.bool_type().const_zero();
                        env_call!(&self, RuntimeFunction::RequestDma, [to_rdram])?;
                    }
                    register::Control::DmaWriteLength1 => {
                        // Request a DMA to RDRAM
                        let to_rdram = self.context.bool_type().const_all_ones();
                        env_call!(&self, RuntimeFunction::RequestDma, [to_rdram])?;
                    }
                    _ => {}
                }
                Ok(())
            }

            _ => self.write_register_raw(reg, value),
        }
    }

    /// Write the vector (CP2) register at the given index.
    #[allow(dead_code)]
    pub fn write_vector_register(
        &self,
        index: impl Into<u64>,
        value: VectorValue<'ctx>,
    ) -> CompilationResult<()> {
        #[allow(clippy::cast_possible_truncation)]
        let reg = register::Vector::from_repr(index.into() as u8).unwrap();
        self.write_register_raw(reg, value)
    }

    pub fn write_flags_register(
        &self,
        index: impl Into<u64>,
        mut value: IntValue<'ctx>,
    ) -> CompilationResult<()> {
        #[allow(clippy::cast_possible_truncation)]
        let reg = register::Flags::new(index.into() as u8);
        if matches!(reg, register::Flags::VCE) && value.get_type().get_bit_width() > 8 {
            // VCE is only 8 bits
            value = self.truncate_to(self.context.i8_type(), value)?;
        }
        self.write_register_raw(reg, value)
    }

    /// Write the given 8-bit `value`, into vector register `index` at `offset`.
    pub fn write_vector_register_byte_offset(
        &self,
        index: impl Into<u64>,
        value: IntValue<'ctx>,
        offset: IntValue<'ctx>,
    ) -> CompilationResult<()> {
        let i8_type = self.context.i8_type();
        #[allow(clippy::cast_possible_truncation)]
        let reg = register::Vector::from_repr(index.into() as u8)
            .unwrap()
            .into();

        // TODO: Validate the offset in relation to the value's type, this can read out of bounds.
        let ptr = unsafe {
            self.builder.build_in_bounds_gep(
                i8_type,
                self.globals().registers.pointer_value(self, &reg),
                &[offset],
                "vector_register_byte_offset",
            )?
        };

        self.write_register_pointer(value.into(), ptr, reg)
    }
}

pub trait VectorRegisterElement<'ctx> {
    fn vector_type(context: &'ctx Context) -> VectorType<'ctx>;
}

impl<'ctx> VectorRegisterElement<'ctx> for u8 {
    fn vector_type(context: &'ctx Context) -> VectorType<'ctx> {
        context.i8_type().vec_type(16)
    }
}

impl<'ctx> VectorRegisterElement<'ctx> for u16 {
    fn vector_type(context: &'ctx Context) -> VectorType<'ctx> {
        context.i16_type().vec_type(8)
    }
}
