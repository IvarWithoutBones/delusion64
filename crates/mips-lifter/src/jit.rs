use inkwell::{
    builder::Builder,
    context::Context,
    execution_engine::{ExecutionEngine, JitFunction},
    module::Module,
    IntPredicate,
};

type SumFunc = unsafe extern "C" fn(u64, u64, u64) -> u64;
type LoopFunc = unsafe extern "C" fn(u64) -> u64;
type AddFunc = unsafe extern "C" fn(u64) -> u64;

extern "C" fn test() {
    println!("less than 10");
}

pub struct CodeGen<'ctx> {
    pub context: &'ctx Context,
    pub module: Module<'ctx>,
    pub builder: Builder<'ctx>,
    pub execution_engine: ExecutionEngine<'ctx>,
}

impl<'ctx> CodeGen<'ctx> {
    pub fn new(
        context: &'ctx Context,
        module: Module<'ctx>,
        execution_engine: ExecutionEngine<'ctx>,
    ) -> Self {
        Self {
            context,
            module,
            builder: context.create_builder(),
            execution_engine,
        }
    }

    pub fn jit_compile_add(&self) -> Option<JitFunction<AddFunc>> {
        // Declare the function signature
        let i64_type = self.context.i64_type();
        let fn_type = i64_type.fn_type(&[i64_type.into()], false);
        let function = self.module.add_function("add", fn_type, None);

        // Define the function body
        let entry_block = self.context.append_basic_block(function, "entry");

        // Entry block
        self.builder.position_at_end(entry_block);
        let param = function.get_nth_param(0)?.into_int_value();
        let one = i64_type.const_int(10, false);
        let result = self.builder.build_int_add(param, one, "result");
        self.builder.build_return(Some(&result));

        unsafe { self.execution_engine.get_function("add").ok() }
    }

    pub fn jit_compile_loop(&self) -> Option<JitFunction<LoopFunc>> {
        // Declare the function signature: fn loop(amount: u64) -> u64
        let i64_type = self.context.i64_type();
        let fn_type = i64_type.fn_type(&[i64_type.into()], false);
        let function = self.module.add_function("loop", fn_type, None);

        // Define the function body
        let entry_block = self.context.append_basic_block(function, "entry");
        let loop_block = self.context.append_basic_block(function, "loop");
        let exit_block = self.context.append_basic_block(function, "exit");

        // Entry block
        self.builder.position_at_end(entry_block);
        let amount_param = function.get_nth_param(0)?.into_int_value();
        let zero = i64_type.const_int(0, false);
        self.builder.build_unconditional_branch(loop_block);

        // Loop block
        self.builder.position_at_end(loop_block);

        // Keep track of the index, initialize it to 0 and increment it every iteration
        let index_phi = self.builder.build_phi(i64_type, "index");
        index_phi.add_incoming(&[(&zero, entry_block)]);
        let next_index = self.builder.build_int_add(
            index_phi.as_basic_value().into_int_value(),
            i64_type.const_int(1, false),
            "next_index",
        );
        index_phi.add_incoming(&[(&next_index, loop_block)]);

        // Check if we should continue looping
        let cond = self.builder.build_int_compare(
            IntPredicate::ULT,
            index_phi.as_basic_value().into_int_value(),
            amount_param,
            "continue_cmp",
        );
        self.builder
            .build_conditional_branch(cond, loop_block, exit_block);

        // Exit block
        self.builder.position_at_end(exit_block);
        self.builder.build_return(Some(&index_phi.as_basic_value()));

        unsafe { self.execution_engine.get_function("loop").ok() }
    }

    pub fn jit_compile_sum(&self) -> Option<JitFunction<SumFunc>> {
        let i64_type = self.context.i64_type();
        let fn_type = i64_type.fn_type(&[i64_type.into(), i64_type.into(), i64_type.into()], false);
        let function = self.module.add_function("sum", fn_type, None);

        let main_block = self.context.append_basic_block(function, "entry");
        let true_block = self.context.append_basic_block(function, "true");
        let false_block = self.context.append_basic_block(function, "false");
        let merge_block = self.context.append_basic_block(function, "merge");

        self.builder.position_at_end(main_block);

        let x = function.get_nth_param(0)?.into_int_value();
        let y = function.get_nth_param(1)?.into_int_value();

        let sum = self.builder.build_int_add(x, y, "sum");
        let cond = self.builder.build_int_compare(
            IntPredicate::ULT,
            sum,
            i64_type.const_int(10, false),
            "cond",
        );

        self.builder
            .build_conditional_branch(cond, true_block, false_block);

        // True block
        self.builder.position_at_end(true_block);
        let true_sum = self
            .builder
            .build_int_add(sum, i64_type.const_int(10, false), "true_sum");

        // Build a call to the `test` function
        let void_type = self.context.void_type();
        let fn_type = void_type.fn_type(&[], false);
        let test_fn = self.module.add_function("test", fn_type, None);

        self.execution_engine
            .add_global_mapping(&test_fn, test as usize);
        self.builder.build_call(test_fn, &[], "test");

        self.builder.build_unconditional_branch(merge_block);

        // False block
        self.builder.position_at_end(false_block);
        let false_sum = self
            .builder
            .build_int_sub(sum, i64_type.const_int(10, false), "false_sum");
        self.builder.build_unconditional_branch(merge_block);

        // Merge block
        self.builder.position_at_end(merge_block);
        let phi = self.builder.build_phi(i64_type, "phi");
        phi.add_incoming(&[(&true_sum, true_block), (&false_sum, false_block)]);
        self.builder.build_return(Some(&phi.as_basic_value()));

        unsafe { self.execution_engine.get_function("sum").ok() }
    }
}
