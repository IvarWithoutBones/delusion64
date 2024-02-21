//! Helpers for building arithmetic operations.

use super::{CodeGen, CompilationError, CompilationResult};
use crate::target::Target;
use inkwell::{
    context::Context,
    types::{BasicType, FloatType, IntType},
    values::{BasicValue, BasicValueEnum, FloatValue, IntValue},
};

impl<'ctx, T: Target> CodeGen<'ctx, T> {
    /// Sign-extends the given value to the given type.
    pub fn sign_extend_to(
        &self,
        ty: IntType<'ctx>,
        value: IntValue<'ctx>,
    ) -> CompilationResult<IntValue<'ctx>> {
        let name = format!("sign_ext_to_i{}_", ty.get_bit_width());
        Ok(self.builder.build_int_s_extend(value, ty, &name)?)
    }

    /// Zero-extends the given value to the given type.
    pub fn zero_extend_to(
        &self,
        ty: IntType<'ctx>,
        value: IntValue<'ctx>,
    ) -> CompilationResult<IntValue<'ctx>> {
        let name = format!("zero_ext_to_i{}_", ty.get_bit_width());
        Ok(self.builder.build_int_z_extend(value, ty, &name)?)
    }

    /// Truncates the given value to the given type.
    pub fn truncate_to(
        &self,
        ty: IntType<'ctx>,
        value: IntValue<'ctx>,
    ) -> CompilationResult<IntValue<'ctx>> {
        let name = format!("trunc_to_i{}_", ty.get_bit_width());
        Ok(self.builder.build_int_truncate(value, ty, &name)?)
    }

    /// Returns the argument with the smallest value, when treated as unsigned integers.
    pub fn build_umin(
        &self,
        lhs: IntValue<'ctx>,
        rhs: IntValue<'ctx>,
        name: &str,
    ) -> CompilationResult<IntValue<'ctx>> {
        let func = self.intrinsic_declaration("llvm.umin", &[lhs.get_type().into()])?;
        Ok(self
            .builder
            .build_call(func, &[lhs.into(), rhs.into()], name)?
            .try_as_basic_value()
            .left()
            .ok_or(CompilationError::NoReturnValue)?
            .into_int_value())
    }

    /// Splits the given integer in two, returning the high and low order bits, in that order.
    /// The type of integer that is returned is half the bit width of the input integer.
    /// For example, if the input integer is 64 bits wide, the high and low order bits will both. be 32 bits
    pub fn split(
        &self,
        value: IntValue<'ctx>,
    ) -> CompilationResult<(IntValue<'ctx>, IntValue<'ctx>)> {
        let ty = value.get_type();
        let half_ty = IntValue::tag(self.context, &(ty.get_bit_width() / 2));

        let hi = {
            let shift = ty.const_int(half_ty.get_bit_width() as u64, false);
            let shifted = self
                .builder
                .build_right_shift(value, shift, false, "split_hi")?;
            self.truncate_to(half_ty, shifted)?
        };
        let lo = self.truncate_to(half_ty, value)?;
        Ok((hi, lo))
    }

    /// Generates a logical AND operation between the two given values. This is a shorthand for
    /// `builder.build_and(lhs, lhs.get_type().const_int(rhs, false), name)`.
    pub fn build_mask(
        &self,
        to_mask: IntValue<'ctx>,
        mask: u64,
        name: &str,
    ) -> CompilationResult<IntValue<'ctx>> {
        let mask = to_mask.get_type().const_int(mask, false);
        Ok(self.builder.build_and(to_mask, mask, name)?)
    }

    /// Increments the given `value` by `num` and returns the result. This is a shorthand for
    /// `builder.build_int_add(value, value.get_type().const_int(num, false), name)`.
    pub fn increment(
        &self,
        value: IntValue<'ctx>,
        num: u64,
        name: &str,
    ) -> CompilationResult<IntValue<'ctx>> {
        let one = value.get_type().const_int(num, false);
        Ok(self.builder.build_int_add(value, one, name)?)
    }
}

/// A type that can be used to specify the bit width of a [`NumericValue`].
pub(crate) trait BitWidth: std::fmt::Display + Copy {
    /// The number of bits this type occupies.
    fn bit_width(&self) -> usize;
}

impl BitWidth for IntType<'_> {
    fn bit_width(&self) -> usize {
        self.get_bit_width() as usize
    }
}

impl BitWidth for usize {
    fn bit_width(&self) -> usize {
        *self
    }
}

impl BitWidth for u32 {
    fn bit_width(&self) -> usize {
        *self as usize
    }
}

/// A value that can be used in arithmetic operations, such as integers and floats.
pub(crate) trait NumericValue<'ctx>:
    BasicValue<'ctx> + TryFrom<BasicValueEnum<'ctx>> + Copy
{
    /// The type of this value.
    type Tag: BasicType<'ctx> + Clone;

    /// The type of this value, using the given bit width.
    fn tag(context: &'ctx Context, bit_width: &impl BitWidth) -> Self::Tag;
}

impl<'ctx> NumericValue<'ctx> for IntValue<'ctx> {
    type Tag = IntType<'ctx>;

    fn tag(context: &'ctx Context, bit_width: &impl BitWidth) -> Self::Tag {
        match bit_width.bit_width() {
            1 => context.bool_type(),
            8 => context.i8_type(),
            16 => context.i16_type(),
            32 => context.i32_type(),
            64 => context.i64_type(),
            128 => context.i128_type(),
            _ => unimplemented!("<IntValue as NumericValue>::tag(): bit_width={bit_width}"),
        }
    }
}

impl<'ctx> NumericValue<'ctx> for FloatValue<'ctx> {
    type Tag = FloatType<'ctx>;

    fn tag(context: &'ctx Context, bit_width: &impl BitWidth) -> Self::Tag {
        match bit_width.bit_width() {
            16 => context.f16_type(),
            32 => context.f32_type(),
            64 => context.f64_type(),
            128 => context.f128_type(),
            _ => unimplemented!("<FloatValue as NumericValue>::tag(): bit_width={bit_width}"),
        }
    }
}
