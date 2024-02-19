use crate::ast::{Name, Program};

pub mod shrinker;

pub fn aiken_optimize_and_intern(program: Program<Name>) -> Program<Name> {
    let (program, mut current_unique) = program.builtin_force_reducer();
    current_unique.large_offset();

    program
        .lambda_reducer()
        .inline_reducer()
        .lambda_reducer()
        .inline_reducer()
        .force_delay_reducer()
        .cast_data_reducer()
        .convert_arithmetic_ops()
        .builtin_curry_reducer(&mut current_unique)
        .lambda_reducer()
        .inline_reducer()
        .builtin_curry_reducer(&mut current_unique)
        .lambda_reducer()
        .inline_reducer()
}
