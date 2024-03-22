//! Helpers for managing control flow generation.

use super::{CodeGen, CompilationResult};
use crate::{codegen::CompilationError, label::JitFunctionPointer, target::Target};
use inkwell::values::IntValue;

impl<'ctx, T: Target> CodeGen<'ctx, T> {
    /// Generate a jump to the given guest virtual address, ending the current basic block.
    /// If possible, it is recommended to use `build_constant_jump` instead so that cached functions can be used directly.
    pub fn build_dynamic_jump(&self, address: IntValue<'ctx>) -> CompilationResult<()> {
        let address = self.zero_extend_to(self.context.i64_type(), address)?;
        self.set_call_attrs(self.builder.build_call(
            self.helpers.jump(),
            &[address.into()],
            "build_dynamic_jump",
        )?);
        self.builder.build_return(None)?;
        Ok(())
    }

    /// Generate a jump to the given constant virtual address, ending the current basic block.
    pub fn build_constant_jump(&self, address: u64) -> CompilationResult<()> {
        if let Ok(lab) = self.labels.get(address) {
            // If we have previously compiled this function we can insert a call directly.
            let (func, _ptr) = self.register_function(self.module(), lab.function)?;
            let call = self.builder.build_call(func, &[], "constant_jump")?;
            self.set_call_attrs(call);
            self.builder.build_return(None)?;
        } else {
            // Let the runtime environment JIT it, then jump to it.
            self.build_dynamic_jump(self.context.i64_type().const_int(address, false))?;
        }
        Ok(())
    }

    /// Generate a jump to the given host function pointer, ending the current basic block.
    /// This assumes the calling convention used for JIT blocks.
    pub fn build_jump_to_host_ptr(
        &self,
        host_ptr: IntValue<'ctx>,
        name: &str,
    ) -> CompilationResult<()> {
        let func_type = self.context.void_type().fn_type(&[], false);
        let ptr_type = func_type.ptr_type(Default::default());
        let ptr = self.builder.build_int_to_ptr(host_ptr, ptr_type, name)?;
        self.set_call_attrs(
            self.builder
                .build_indirect_call(func_type, ptr, &[], name)?,
        );
        Ok(())
    }

    /// Generate a host function pointer which simply returns to the caller. Calling this inside of a
    /// JIT'ed block will propagate the return up to the consumer of this crate, thanks to tail call optimization.
    pub fn return_from_jit_ptr(&self) -> CompilationResult<JitFunctionPointer> {
        const NAME: &str = "return_from_jit_helper";
        let module = self.context.create_module("return_from_jit_helper_module");
        let func = self.set_func_attrs(module.add_function(
            NAME,
            self.context.void_type().fn_type(&[], false),
            None,
        ));

        let block = self.context.append_basic_block(func, "entry");
        self.builder.position_at_end(block);
        {
            // Since every function we call uses tail calls, this should immediately return to the caller.
            self.builder.build_return(None)?;
        }

        self.execution_engine
            .add_module(&module)
            .map_err(|_| CompilationError::AddModuleFailed)?;
        let func_ptr = self
            .execution_engine
            .get_function_address(NAME)
            .map_err(|error| CompilationError::FunctionLookup {
                name: NAME.to_string(),
                error,
            })?;
        Ok(func_ptr as JitFunctionPointer)
    }

    /// Builds a loop using the following arguments, in order:
    /// * `name`       - The name of the loop in the IR.
    /// * `index_init` - The initial index, prior to starting the loop.
    /// * `compare`    - A function that returns true if the loop should continue.
    /// * `next_index` - A function that modifies the index, returning the new value.
    /// * `body`       - A function that builds the loop body. The return value is propagated.
    pub fn build_for_loop<R>(
        &self,
        name: &str,
        index_init: IntValue<'ctx>,
        mut compare: impl FnMut(IntValue<'ctx>) -> CompilationResult<IntValue<'ctx>>,
        mut next_index: impl FnMut(IntValue<'ctx>) -> CompilationResult<IntValue<'ctx>>,
        mut body: impl FnMut(IntValue<'ctx>) -> CompilationResult<R>,
    ) -> CompilationResult<R> {
        let header_block = self.get_insert_block();
        let (loop_block, done_block) = (
            self.append_basic_block(&format!("{name}_body")),
            self.append_basic_block(&format!("{name}_done")),
        );

        self.builder
            .build_conditional_branch(compare(index_init)?, loop_block, done_block)?;

        self.builder.position_at_end(loop_block);
        let data = {
            let index_phi = self
                .builder
                .build_phi(index_init.get_type(), &format!("{name}_phi"))?;
            let index = index_phi.as_basic_value().into_int_value();

            let data = body(index)?;
            let next_index = next_index(index)?;
            self.builder
                .build_conditional_branch(compare(next_index)?, loop_block, done_block)?;

            index_phi.add_incoming(&[(&index_init, header_block), (&next_index, loop_block)]);
            data
        };

        self.builder.position_at_end(done_block);
        Ok(data)
    }

    // If the comparison is true, execute the function generated within the given closure, otherwise skip it.
    // When the true block does not already have a terminator, a jump to the false block is generated.
    pub fn build_if<R>(
        &self,
        name: &str,
        cmp: IntValue<'ctx>,
        then: impl FnOnce() -> CompilationResult<R>,
    ) -> CompilationResult<R> {
        let (then_block, else_block) = (
            self.append_basic_block(&format!("{name}_then")),
            self.append_basic_block(&format!("{name}_else")),
        );
        self.builder
            .build_conditional_branch(cmp, then_block, else_block)?;

        self.builder.position_at_end(then_block);
        let extra_data = then()?;
        if !self.current_block_terminated() {
            self.builder.build_unconditional_branch(else_block)?;
        }

        self.builder.position_at_end(else_block);
        Ok(extra_data)
    }

    /// If the comparison is true, execute the first function generated within the given closure,
    /// otherwise the second. If either block does not already have a terminator, a jump to the merge block is generated.
    pub fn build_if_else<R1, R2>(
        &self,
        name: &str,
        cmp: IntValue<'ctx>,
        then: impl FnOnce() -> CompilationResult<R1>,
        otherwise: impl FnOnce() -> CompilationResult<R2>,
    ) -> CompilationResult<(R1, R2)> {
        let merge_block = self.append_basic_block(&format!("{name}_merge"));
        let then_data = self.build_if(name, cmp, || {
            let then_data = then()?;
            if !self.current_block_terminated() {
                self.builder.build_unconditional_branch(merge_block)?;
            }
            Ok(then_data)
        })?;

        // `build_if` leaves us positioned at the false block, so we can just call `otherwise`.
        let else_data = otherwise()?;
        if !self.current_block_terminated() {
            self.builder.build_unconditional_branch(merge_block)?;
        }

        self.builder.position_at_end(merge_block);
        Ok((then_data, else_data))
    }
}
