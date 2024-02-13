/// Builds a call to the specified runtime function, passing the given arguments (if any).
/// This is a macro so that we can concatenate the environment pointer and arguments without allocations.
///
/// # Example
///
/// ```ignore
/// let ptr = env_call!(&codegen, RuntimeFunction::GetFunctionPtr, [address])
///     .unwrap()
///     .try_as_basic_value()
///     .left()
///     .unwrap()
///     .into_int_value();
/// ```
macro_rules! env_call {
    ($codegen:expr, $func:expr, [$($args:expr),*]) => {{
        // Type-check the arguments.
        let codegen: &$crate::codegen::CodeGen<_> = $codegen;
        let func: &$crate::runtime::RuntimeFunction = &$func;

        let globals = codegen.globals();
        let args = &[globals.env_ptr.as_pointer_value().into(), $($args.into()),*];
        let func = *globals.functions.get(func).expect("runtime functions not initialised");
        codegen.builder.build_call(func, args, concat!("env_call_", stringify!($func)))
    }};
}

/// Generates the macros for integer comparisons. This is an internal macro and should not be used directly.
#[rustfmt::skip]
macro_rules! cmp_macro {
    ($name:ident, $ty:tt, $doc:expr) => {
        #[doc = $doc]
        macro_rules! $name {
            (impl !=, equality, name) => ("ne");
            (impl !=, equality, pred) => (inkwell::IntPredicate::NE);

            (impl ==, equality, name) => ("eq");
            (impl ==, equality, pred) => (inkwell::IntPredicate::EQ);

            (impl >=, unsigned, name) => ("uge");
            (impl >=, unsigned, pred) => (inkwell::IntPredicate::UGE);

            (impl >=, signed, name) => ("sge");
            (impl >=, signed, pred) => (inkwell::IntPredicate::SGE);

            (impl <=, unsigned, name) => ("ule");
            (impl <=, unsigned, pred) => (inkwell::IntPredicate::ULE);

            (impl <=, signed, name) => ("sle");
            (impl <=, signed, pred) => (inkwell::IntPredicate::SLE);

            (impl >, unsigned, name) => ("ugt");
            (impl >, unsigned, pred) => (inkwell::IntPredicate::UGT);

            (impl >, signed, name) => ("sgt");
            (impl >, signed, pred) => (inkwell::IntPredicate::SGT);

            (impl <, unsigned, name) => ("ult");
            (impl <, unsigned, pred) => (inkwell::IntPredicate::ULT);

            (impl <, signed, name) => ("slt");
            (impl <, signed, pred) => (inkwell::IntPredicate::SLT);

            (impl $cond:tt, $ty_inner:tt, name) => (
                compile_error!(concat!("invalid ", stringify!($ty), " comparison: ", stringify!($cond)))
            );

            (impl $cond:tt, $ty_inner:tt, pred) => (
                compile_error!(concat!("invalid ", stringify!($ty), " comparison: ", stringify!($cond)))
            );

            (impl $codegen:ident, ($left:expr) $cond:tt ($right:expr)) => {{
                // Type check the arguments.
                let codegen: &$crate::codegen::CodeGen<_> = &$codegen;
                let left: inkwell::values::IntValue = $left;
                let right: inkwell::values::IntValue = $right;

                codegen.builder.build_int_compare(
                    $name!(impl $cond, $ty, pred),
                    left,
                    right,
                    concat!(
                        stringify!($left),
                        "_",
                        $name!(impl $cond, $ty, name),
                        "_",
                        stringify!($right)
                    ),
                ).map_err($crate::codegen::CompilationError::from)
            }};

            ($codegen:ident, $left:ident $cond:tt $right:literal) => {{
                let const_int = {
                    let numeric: u64 = $right as u64;
                    let left: &inkwell::values::IntValue = &$left;
                    left.get_type().const_int(numeric, false)
                };
                $name!(impl $codegen, ($left) $cond (const_int))
            }};

            ($codegen:ident, $left:literal $cond:tt $right:expr) => {{
                let const_int = {
                    let numeric: u64 = $left as u64;
                    let right: &inkwell::values::IntValue = &$right;
                    right.get_type().const_int(numeric, false)
                };
                $name!(impl $codegen, (const_int) $cond ($right))
            }};

            ($codegen:ident, ($left:expr) $cond:tt ($right:expr)) => {{
                $name!(impl $codegen, ($left) $cond ($right))
            }};

            ($codegen:ident, $left:ident $cond:tt $right:expr) => {{
                $name!(impl $codegen, ($left) $cond ($right))
            }};
        }
    };
}

cmp_macro!(
    cmp,
    equality,
    r#"
    Compares the given two integers for equality, returning an runtime boolean.

    # Example

    ```ignore
    cmp!(codegen, left == right);
    cmp!(codegen, left != right);
    ```
    "#
);

cmp_macro!(
    cmpu,
    unsigned,
    r#"
    Compares the given two unsigned integers for equality, returning an runtime boolean.

    # Example

    ```ignore
    cmpu!(codegen, left < right);
    cmpu!(codegen, left > right);
    cmpu!(codegen, left => right);
    cmpu!(codegen, left <= right);
    ```
    "#
);

cmp_macro!(
    cmps,
    signed,
    r#"
    Compares the given two signed integers for equality, returning an runtime boolean.

    # Example

    ```ignore
    cmps!(codegen, left < right);
    cmps!(codegen, left > right);
    cmps!(codegen, left => right);
    cmps!(codegen, left <= right);
    ```
    "#
);

// This is required for the macros to be visible within the crate, without being visible to the crates consumer (like with `macro_export`).
#[allow(clippy::single_component_path_imports)]
pub(crate) use {cmp, cmps, cmpu, env_call};
