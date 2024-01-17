#![allow(dead_code)]

use super::{register, Rsp};
use crate::codegen::CodeGen;
use inkwell::{types::IntType, values::IntValue};

impl<'ctx> CodeGen<'ctx, Rsp> {
    /// Read the control (CP0) register at the given index.
    pub fn read_control_register(
        &self,
        ty: IntType<'ctx>,
        index: impl Into<u64>,
    ) -> IntValue<'ctx> {
        #[allow(clippy::cast_possible_truncation)]
        let reg = register::Control::from_repr(index.into() as u8).unwrap();
        self.read_register_raw(ty, reg)
    }

    /// Read the vector (CP2) register at the given index.
    pub fn read_vector_register(&self, ty: IntType<'ctx>, index: impl Into<u64>) -> IntValue<'ctx> {
        #[allow(clippy::cast_possible_truncation)]
        let reg = register::Vector::from_repr(index.into() as u8).unwrap();
        self.read_register_raw(ty, reg)
    }

    /// Read the miscellaneous register at the given index.
    pub fn read_special_register(
        &self,
        ty: IntType<'ctx>,
        index: impl Into<u64>,
    ) -> IntValue<'ctx> {
        #[allow(clippy::cast_possible_truncation)]
        let reg = register::Special::from_repr(index.into() as u8).unwrap();
        self.read_register_raw(ty, reg)
    }

    /// Read the given register
    pub fn read_rsp_register(&self, ty: IntType<'ctx>, reg: register::Register) -> IntValue<'ctx> {
        match reg {
            register::Register::GeneralPurpose(r) => self.read_general_register(ty, r),
            register::Register::Control(r) => self.read_control_register(ty, r),
            register::Register::Vector(r) => self.read_vector_register(ty, r),
            register::Register::Special(r) => self.read_special_register(ty, r),
        }
    }

    /// Write the control (CP0) register at the given index.
    #[allow(clippy::needless_pass_by_value)]
    pub fn write_control_register(&self, _index: impl Into<u64>, _value: IntValue<'ctx>) {
        todo!("codegen to update RSP control registers")
    }

    /// Write the vector (CP2) register at the given index.
    pub fn write_vector_register(&self, index: impl Into<u64>, value: IntValue<'ctx>) {
        #[allow(clippy::cast_possible_truncation)]
        let reg = register::Vector::from_repr(index.into() as u8).unwrap();
        self.write_register_raw(reg, value);
    }

    /// Write the miscellaneous register at the given index.
    pub fn write_special_register(&self, index: impl Into<u64>, value: IntValue<'ctx>) {
        #[allow(clippy::cast_possible_truncation)]
        let reg = register::Special::from_repr(index.into() as u8).unwrap();
        self.write_register_raw(reg, value);
    }

    /// Write the given register
    pub fn write_rsp_register(&self, reg: register::Register, value: IntValue<'ctx>) {
        match reg {
            register::Register::GeneralPurpose(r) => self.write_general_register(r, value),
            register::Register::Control(r) => self.write_control_register(r, value),
            register::Register::Vector(r) => self.write_vector_register(r, value),
            register::Register::Special(r) => self.write_special_register(r, value),
        }
    }
}
