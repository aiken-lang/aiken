use crate::ast::{Name, Program};

pub mod interner;
pub mod shrinker;

/// Full optimization for Program<Name> (without span context).
/// This includes DeBruijn interning and all optimizations.
pub fn aiken_optimize_and_intern(program: Program<Name>) -> Program<Name> {
    let mut prog = program.run_once_pass();

    let mut prev_count = 0;

    loop {
        let (current_program, context) = prog.multi_pass();

        if context.node_count == prev_count {
            prog = current_program;
            break;
        } else {
            prog = current_program;
            prev_count = context.node_count;
        }
    }

    prog = prog
        .builtin_curry_reducer()
        .multi_pass()
        .0
        .builtin_curry_reducer();

    loop {
        let (current_program, context) = prog.multi_pass();

        if context.node_count == prev_count {
            prog = current_program;
            break;
        } else {
            prog = current_program;
            prev_count = context.node_count;
        }
    }

    prog.clean_up_no_inlines().afterwards()
}

/// Full optimization for Program<Name, C> that preserves span/context.
/// All optimizations are now generic over the context type.
pub fn aiken_optimize_with_context<C: Clone + Default + PartialEq>(
    program: Program<Name, C>,
) -> Program<Name, C> {
    // NOTE: We use the same optimization passes as aiken_optimize_and_intern
    // to ensure identical semantics. The DeBruijn cleanup preserves context.
    let mut prog = program.run_once_pass();

    let mut prev_count = 0;

    loop {
        let (current_program, context) = prog.multi_pass();

        if context.node_count == prev_count {
            prog = current_program;
            break;
        } else {
            prog = current_program;
            prev_count = context.node_count;
        }
    }

    prog = prog
        .builtin_curry_reducer()
        .multi_pass()
        .0
        .builtin_curry_reducer();

    loop {
        let (current_program, context) = prog.multi_pass();

        if context.node_count == prev_count {
            prog = current_program;
            break;
        } else {
            prog = current_program;
            prev_count = context.node_count;
        }
    }

    prog.clean_up_no_inlines().afterwards()
}

