#![allow(dead_code)]

use super::{register, Rsp};
use crate::{
    codegen::{CodeGen, CompilationResult},
    target::Globals,
};
use inkwell::{
    types::IntType,
    values::{IntValue, VectorValue},
};

impl<'ctx> CodeGen<'ctx, Rsp> {
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
        #[allow(clippy::cast_possible_truncation)]
        let reg = register::Control::from_repr(index.into() as u8).unwrap();
        Ok(self.read_register_raw(ty, reg)?.into_int_value())
    }

    /// Read the vector (CP2) register at the given index.
    pub fn read_vector_register(
        &self,
        index: impl Into<u64>,
    ) -> CompilationResult<VectorValue<'ctx>> {
        #[allow(clippy::cast_possible_truncation)]
        let reg = register::Vector::from_repr(index.into() as u8).unwrap();
        let ty = self.context.i8_type().vec_type(16);
        Ok(self.read_register_raw(ty, reg)?.into_vector_value())
    }

    /// Read the miscellaneous register at the given index.
    pub fn read_special_register(
        &self,
        ty: IntType<'ctx>,
        index: impl Into<u64>,
    ) -> CompilationResult<IntValue<'ctx>> {
        #[allow(clippy::cast_possible_truncation)]
        let reg = register::Special::from_repr(index.into() as u8).unwrap();
        Ok(self.read_register_raw(ty, reg)?.into_int_value())
    }

    /// Read the given register
    pub fn read_rsp_register(
        &self,
        ty: IntType<'ctx>,
        reg: register::Register,
    ) -> CompilationResult<IntValue<'ctx>> {
        match reg {
            register::Register::GeneralPurpose(r) => self.read_general_register(ty, r),
            register::Register::Control(r) => self.read_control_register(ty, r),
            register::Register::Vector(_r) => todo!("read_vector_register"),
            // register::Register::Vector(r) => todo!("read_vector_register"),
            register::Register::Special(r) => self.read_special_register(ty, r),
        }
    }

    /// Write the control (CP0) register at the given index.
    #[allow(clippy::needless_pass_by_value)]
    pub fn write_control_register(
        &self,
        _index: impl Into<u64>,
        _value: IntValue<'ctx>,
    ) -> CompilationResult<()> {
        todo!("codegen to update RSP control registers")
    }

    /// Write the vector (CP2) register at the given index.
    pub fn write_vector_register(
        &self,
        index: impl Into<u64>,
        value: VectorValue<'ctx>,
    ) -> CompilationResult<()> {
        #[allow(clippy::cast_possible_truncation)]
        let reg = register::Vector::from_repr(index.into() as u8).unwrap();
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
        debug_assert_eq!(value.get_type(), i8_type);
        #[allow(clippy::cast_possible_truncation)]
        let reg = register::Vector::from_repr(index.into() as u8)
            .unwrap()
            .into();

        // TODO: Validate the offset, this will read out of bounds when given an offset bigger than 15.
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

    /// Write the miscellaneous register at the given index.
    pub fn write_special_register(
        &self,
        index: impl Into<u64>,
        value: IntValue<'ctx>,
    ) -> CompilationResult<()> {
        #[allow(clippy::cast_possible_truncation)]
        let reg = register::Special::from_repr(index.into() as u8).unwrap();
        self.write_register_raw(reg, value)
    }

    /// Write the given register
    pub fn write_rsp_register(
        &self,
        reg: register::Register,
        value: IntValue<'ctx>,
    ) -> CompilationResult<()> {
        match reg {
            register::Register::GeneralPurpose(r) => self.write_general_register(r, value),
            register::Register::Control(r) => self.write_control_register(r, value),
            register::Register::Vector(_r) => todo!("write_vector_register"),
            register::Register::Special(r) => self.write_special_register(r, value),
        }
    }
}
