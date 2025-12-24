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

/// Minimal optimization for Program<Name> (without span context).
/// Only performs passes necessary for correctness, skipping performance optimizations.
pub fn aiken_optimize_minimal(program: Program<Name>) -> Program<Name> {
    let prog = program.run_once_pass();
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

/// Minimal optimization for Program<Name, C> that preserves span/context.
/// Only performs passes necessary for correctness, skipping performance optimizations
/// like inlining, lambda reduction, etc. This produces larger but more readable code
/// that maps more directly to the source.
pub fn aiken_optimize_minimal_with_context<C: Clone + Default + PartialEq>(
    program: Program<Name, C>,
) -> Program<Name, C> {
    // Only run the once-pass which handles builtins and necessary transformations
    // Skip multi_pass (inlining, lambda reduction) and builtin_curry_reducer
    let prog = program.run_once_pass();
    prog.clean_up_no_inlines().afterwards()
}

