#![allow(dead_code)]

use self::a::{AsBasicType, AsIntType, AsPointerType};

use super::{bus::Bus, Environment, ValidRuntime};
use crate::target::Target;
use inkwell::{
    context::Context,
    types::{BasicMetadataTypeEnum, FunctionType},
};

mod a {
    // TODO move to arithmatic
    use inkwell::{
        context::Context,
        types::{BasicMetadataTypeEnum, IntType, PointerType},
    };

    pub trait AsIntType {
        fn as_int_type(context: &Context) -> IntType;
    }

    pub trait AsPointerType {
        fn as_pointer_type(context: &Context) -> PointerType;
    }

    pub trait AsBasicType {
        fn as_basic_type(context: &Context) -> BasicMetadataTypeEnum;
    }

    impl AsIntType for usize {
        fn as_int_type(context: &Context) -> IntType {
            // It should be fine to rely on Rust's compile time checks since LLVM will target the same ISA.
            #[cfg(target_pointer_width = "64")]
            {
                context.i64_type()
            }
            #[cfg(target_pointer_width = "32")]
            {
                context.i32_type()
            }
            #[cfg(not(any(target_pointer_width = "64", target_pointer_width = "32")))]
            {
                compile_error!("unsupported target_pointer_width")
            }
        }
    }

    impl AsPointerType for usize {
        fn as_pointer_type(context: &Context) -> PointerType {
            Self::as_int_type(context).ptr_type(Default::default())
        }
    }

    impl AsBasicType for usize {
        fn as_basic_type(context: &Context) -> BasicMetadataTypeEnum {
            Self::as_int_type(context).into()
        }
    }

    macro_rules! impl_as_basic_type {
        ($(($ty: ty, $method: ident)),* $(,)?) => {
            $(
                impl AsIntType for $ty {
                    fn as_int_type(context: &Context) -> IntType {
                        context.$method()
                    }
                }

                impl AsPointerType for $ty {
                    fn as_pointer_type(context: &Context) -> PointerType {
                        context.$method().ptr_type(Default::default())
                    }
                }

                impl AsBasicType for $ty {
                    fn as_basic_type(context: &Context) -> BasicMetadataTypeEnum {
                        context.$method().into()
                    }
                }

                impl AsBasicType for *const $ty {
                    fn as_basic_type(context: &Context) -> BasicMetadataTypeEnum {
                        <$ty as AsPointerType>::as_pointer_type(context).into()
                    }
                }

                impl AsBasicType for *mut $ty {
                    fn as_basic_type(context: &Context) -> BasicMetadataTypeEnum {
                        <$ty as AsPointerType>::as_pointer_type(context).into()
                    }
                }
            )*
        };
    }

    impl_as_basic_type!(
        (u64, i64_type),
        (u32, i32_type),
        (u16, i16_type),
        (u8, i8_type),
        (bool, bool_type),
    );
}

type ArgTypes<'ctx> = [BasicMetadataTypeEnum<'ctx>];

pub trait ReturnType {
    fn fn_type<'ctx>(context: &'ctx Context, arg_types: &ArgTypes<'ctx>) -> FunctionType<'ctx>;
}

pub trait Arguments {
    fn fn_args<'ctx, R>(context: &'ctx Context, with_args: impl FnOnce(&ArgTypes<'ctx>) -> R) -> R;
}

pub trait Callback {
    type Args: Arguments;
    type Ret: ReturnType;

    fn signature<'ctx>(&self, context: &'ctx Context) -> FunctionType<'ctx> {
        Self::Args::fn_args(context, |args| Self::Ret::fn_type(context, args))
    }

    fn as_ptr(&self) -> RawPointer;
}

impl ReturnType for () {
    fn fn_type<'ctx>(context: &'ctx Context, arg_types: &ArgTypes<'ctx>) -> FunctionType<'ctx> {
        context.void_type().fn_type(arg_types, false)
    }
}

impl<T: AsIntType> ReturnType for T {
    fn fn_type<'ctx>(context: &'ctx Context, arg_types: &ArgTypes<'ctx>) -> FunctionType<'ctx> {
        T::as_int_type(context).fn_type(arg_types, false)
    }
}

macro_rules! impl_callback_signature {
    ($($($generic: ident),+)?) => {
        // We implement `Arguments` for a unit, `()`, tuples, and a single type. The latter will throw an unused parens warning.
        #[allow(unused_parens)]
        impl $(<$($generic),+>)? Arguments for ($($($generic),+)?)
        where
            $($($generic: AsBasicType),+)?
        {
            fn fn_args<'ctx, R>(context: &'ctx Context, with_args: impl FnOnce(&ArgTypes<'ctx>) -> R) -> R {
                let env_ptr = usize::as_pointer_type(context);
                let args = &[
                    env_ptr.into(),
                    $($($generic::as_basic_type(context)),+)?
                ];
                with_args(args)
            }
        }

        #[allow(unused_parens)]
        impl<T, B, R, $($($generic),+)?> Callback
            for extern "C" fn(&mut Environment<'_, T, B>, $($($generic),*)?) -> R
        where
            for<'a> Environment<'a, T, B>: ValidRuntime<T>,
            T: Target,
            B: Bus,
            R: ReturnType,
            $(($($generic),+): Arguments,)?
        {
            type Ret = R;
            type Args = ($($($generic),+)?);

            fn as_ptr(&self) -> RawPointer {
                *self as RawPointer
            }
        }
    };
}

impl_callback_signature!();
impl_callback_signature!(Arg1);
impl_callback_signature!(Arg1, Arg2);
impl_callback_signature!(Arg1, Arg2, Arg3);
impl_callback_signature!(Arg1, Arg2, Arg3, Arg4);
impl_callback_signature!(Arg1, Arg2, Arg3, Arg4, Arg5);
impl_callback_signature!(Arg1, Arg2, Arg3, Arg4, Arg5, Arg6);
impl_callback_signature!(Arg1, Arg2, Arg3, Arg4, Arg5, Arg6, Arg7);

macro_rules! callback {
    (|$env: ident $(, $arg: ident : $arg_ty: ty)* $(,)?| $(-> $ret: tt)? $body: expr) => {{
        extern "C" fn callback<T: Target, B: Bus>(
            $env: &mut Environment<'_, T, B>,
            $($arg: $arg_ty),*
        ) $(-> $ret)?
        where
            for<'a> Environment<'a, T, B>: ValidRuntime<T>,
        {
            $body
        }
        // We need to explicitly cast this to avoid coercing it into an `Fn` pointer, for which `Callback` is not implemented.
        callback as extern "C" fn(&mut Environment<'_, T, B>, $($arg_ty),*) $(-> $ret)?
    }};
}

pub type RawPointer = usize;

pub mod signatures {
    use crate::runtime::Environment;

    pub type Panic<T, B> = extern "C" fn(&mut Environment<'_, T, B>, *const u8, usize);
    pub type OnBlockEntered<T, B> = extern "C" fn(&mut Environment<'_, T, B>, u64) -> usize;
    pub type OnInstruction<T, B> = extern "C" fn(&mut Environment<'_, T, B>);
    pub type HandleException<T, B> =
        extern "C" fn(&mut Environment<'_, T, B>, u64, bool, u8, bool, u64) -> usize;
}

pub trait Callbacks<T: Target, B: Bus>
where
    for<'a> Environment<'a, T, B>: ValidRuntime<T>,
{
    #[inline]
    fn handle_exception(&self) -> signatures::HandleException<T, B> {
        callback!(|env,
                   code: u64,
                   has_coprocessor: bool,
                   coprocessor: u8,
                   has_bad_vaddr: bool,
                   bad_vaddr: u64|
         -> usize {
            let msg = &format!(
                "unimplemented call to `handle_exception`:\n\
                code={code:#x},\n\
                has_coprocessor={has_coprocessor}, coprocessor={coprocessor},\n\
                has_bad_vaddr={has_bad_vaddr}, bad_vaddr={bad_vaddr:#x}"
            );
            env.panic_update_debugger(msg)
        })
    }

    #[inline]
    fn panic(&self) -> signatures::Panic<T, B> {
        callback!(|env, string_ptr: *const u8, len: usize| {
            let slice = unsafe { std::slice::from_raw_parts(string_ptr, len) };
            match std::str::from_utf8(slice) {
                Ok(str) => env.panic_update_debugger(&format!("Environment::panic called: {str}")),
                Err(err) => {
                    let msg = &format!("Environment::panic called with an invalid string: {err}");
                    env.panic_update_debugger(msg)
                }
            }
        })
    }

    #[inline]
    fn on_block_entered(&self) -> signatures::OnBlockEntered<T, B> {
        callback!(|env, addr: u64| -> usize {
            let msg = &format!("unimplemented call to `on_block_entered` with addr={addr:#x}");
            env.panic_update_debugger(msg)
        })
    }

    #[inline]
    fn on_instruction_callback(&self) -> signatures::OnInstruction<T, B> {
        callback!(|env| {
            let msg = "unimplemented call to `on_instruction_callback`";
            env.panic_update_debugger(msg)
        })
    }
}

impl<T: Target, B: Bus> Callbacks<T, B> for Environment<'_, T, B> where
    for<'a> Environment<'a, T, B>: ValidRuntime<T>
{
}

impl<'ctx, T: Target, B: Bus> Environment<'ctx, T, B>
where
    for<'a> Environment<'a, T, B>: ValidRuntime<T>,
{
    fn map_(&mut self) {
        let a = self.on_instruction_callback();
        self.on_instruction_callback()(self);
        let _b = a.signature(self.codegen.context);
        let c: extern "C" fn(&mut Environment<'_, T, B>) = self.on_instruction_callback();
        c(self);
    }
}
