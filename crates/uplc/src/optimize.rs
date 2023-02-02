use crate::ast::{Name, Program};

pub mod shrinker;

pub fn aiken_optimize(term: Program<Name>) -> Program<Name> {
    term.builtin_force_reduce().lambda_reduce().inline_reduce()
}
