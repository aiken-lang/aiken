use crate::{
    ast::{Name, Program},
    optimize::shrinker::Context,
};

pub mod interner;
pub mod shrinker;

fn optimize_repeatedly(prev_count: &mut usize, mut program: Program<Name>) -> Program<Name> {
    loop {
        let (new_program, Context { node_count, .. }) = program.multi_pass();

        program = new_program;

        if node_count == *prev_count {
            break;
        } else {
            *prev_count = node_count;
        }
    }

    program
}

pub fn aiken_optimize_and_intern(program: Program<Name>) -> Program<Name> {
    let mut node_count = 0;

    let program = optimize_repeatedly(&mut node_count, program.run_once_pass())
        .builtin_curry_reducer()
        .multi_pass()
        .0
        .builtin_curry_reducer();

    optimize_repeatedly(&mut node_count, program)
        .clean_up_no_inlines()
        .afterwards()
}
