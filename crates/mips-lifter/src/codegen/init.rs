use super::{CodeGen, Globals};
use crate::{env_call, runtime::RuntimeFunction};
use inkwell::{execution_engine::JitFunction, values::FunctionValue};
use mips_decomp::{register, INSTRUCTION_SIZE};

#[derive(Default, Debug)]
pub struct Helpers<'ctx> {
    main: Option<FunctionValue<'ctx>>,
    jump: Option<FunctionValue<'ctx>>,
    fallthrough_one: Option<FunctionValue<'ctx>>,
    fallthrough_two: Option<FunctionValue<'ctx>>,
}

impl<'ctx> Helpers<'ctx> {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn jump(&self) -> FunctionValue<'ctx> {
        self.jump.expect("jump helper function not initialized")
    }

    pub fn fallthrough_one(&self) -> FunctionValue<'ctx> {
        self.fallthrough_one
            .expect("fallthrough_one helper function not initialized")
    }

    pub fn fallthrough_two(&self) -> FunctionValue<'ctx> {
        self.fallthrough_two
            .expect("fallthrough_two helper function not initialized")
    }

    pub fn map_into(&mut self, module: &inkwell::module::Module<'ctx>) {
        let declare = |func: Option<FunctionValue<'ctx>>| {
            let func = func.expect("helper function not initialized");
            let name = func.get_name().to_str().expect("invalid function name");
            module.add_function(name, func.get_type(), None)
        };

        self.main = Some(declare(self.main));
        self.jump = Some(declare(self.jump));
        self.fallthrough_one = Some(declare(self.fallthrough_one));
        self.fallthrough_two = Some(declare(self.fallthrough_two));
    }
}

impl<'ctx> CodeGen<'ctx> {
    pub fn main(&self) -> JitFunction<unsafe extern "C" fn()> {
        let func_val = self.helpers.main.expect("main function not initialized");
        let func_name = func_val.get_name().to_str().expect("invalid function name");
        let func = unsafe { self.execution_engine.get_function(func_name) };
        func.expect("LLVM target is not initialized")
    }

    pub fn initialise(&mut self, globals: Globals<'ctx>) {
        debug_assert!(
            self.helpers.main.is_none(),
            "codegen should only be initialised once"
        );

        self.globals = Some(globals);
        self.build_jump_helper();
        self.build_fallthrough_helpers();
        self.build_main();

        if let Err(err) = self.verify() {
            eprintln!("{err}");
            panic!("generated code for helpers failed to verify");
        }
    }

    fn build_main(&mut self) {
        let i64_type = self.context.i64_type();
        let fn_type = self.context.void_type().fn_type(&[], false);
        let main_fn = self.module.add_function("main", fn_type, None);

        let entry_block = self.context.append_basic_block(main_fn, "entry");
        self.builder.position_at_end(entry_block);
        {
            let pc = self.read_register(i64_type, register::Special::Pc);
            self.build_dynamic_jump(pc);
        }

        self.helpers.main = Some(main_fn);
    }

    fn build_jump_helper(&mut self) {
        let i64_type = self.context.i64_type();
        let void_type = self.context.void_type();

        // Generate the function signature
        let func = self.set_func_attrs(self.module.add_function(
            "jump_to_vaddr",
            void_type.fn_type(&[i64_type.into()], false),
            None,
        ));

        // Generate the function body.
        let block = self.context.append_basic_block(func, "entry");
        self.builder.position_at_end(block);
        {
            let address = func.get_first_param().unwrap().into_int_value();
            let ptr = env_call!(&self, RuntimeFunction::GetFunctionPtr, [address])
                .try_as_basic_value()
                .left()
                .unwrap()
                .into_int_value();
            self.build_jump_to_jit_func_ptr(ptr, "jump_fn_call")
        }

        self.helpers.jump = Some(func);
    }

    fn build_fallthrough_helpers(&mut self) {
        let make_func = |codegen: &Self, amount: u64, name: &str| -> FunctionValue<'ctx> {
            // Generate the function declaration.
            let func = codegen.set_func_attrs(codegen.module.add_function(
                name,
                codegen.context.void_type().fn_type(&[], false),
                None,
            ));

            // Generate the function body.
            let block = codegen.context.append_basic_block(func, "entry");
            codegen.builder.position_at_end(block);
            {
                let i64_type = codegen.context.i64_type();
                let pc = codegen.read_register(i64_type, register::Special::Pc);
                let offset = i64_type.const_int(amount * INSTRUCTION_SIZE as u64, false);
                let new_pc = codegen.builder.build_int_add(pc, offset, "new_pc");
                codegen.build_dynamic_jump(new_pc);
            }

            func
        };

        self.helpers.fallthrough_one = Some(make_func(self, 1, "fallthrough_one_instruction"));
        self.helpers.fallthrough_two = Some(make_func(self, 2, "fallthrough_two_instructions"));
    }
}