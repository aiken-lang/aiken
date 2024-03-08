use crate::ast::{Name, Program};

pub mod interner;
pub mod shrinker;

pub fn aiken_optimize_and_intern(program: Program<Name>) -> Program<Name> {
    program
        .inline_constr_ops()
        .builtin_force_reducer()
        .lambda_reducer()
        .inline_reducer()
        .identity_reducer()
        .lambda_reducer()
        .inline_reducer()
        .force_delay_reducer()
        .cast_data_reducer()
        .convert_arithmetic_ops()
        .builtin_curry_reducer()
        .lambda_reducer()
        .inline_reducer()
        .identity_reducer()
        .builtin_curry_reducer()
        .lambda_reducer()
        .inline_reducer()
        .remove_no_inlines()
}
