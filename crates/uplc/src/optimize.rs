use crate::ast::{Name, Program};

pub mod interner;
pub mod shrinker;

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

    prog.clean_up()
}
