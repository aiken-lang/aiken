use crate::ast::{Name, Program};

mod interner;
pub mod shrinker;

pub fn aiken_optimize_and_intern(program: Program<Name>) -> Program<Name> {
    program
        .builtin_force_reducer()
        .lambda_reducer()
        .inline_reducer()
        .lambda_reducer()
        .inline_reducer()
        .force_delay_reducer()
        .cast_data_reducer()
        .convert_arithmetic_ops()
        .builtin_curry_reducer()
        .lambda_reducer()
        .inline_reducer()
        .builtin_curry_reducer()
        .lambda_reducer()
        .inline_reducer()
}
