use crate::{
    ast::{Name, Program},
    parser::interner::Interner,
};

pub mod shrinker;

pub fn aiken_optimize_and_intern(program: Program<Name>) -> Program<Name> {
    let mut program = program.builtin_force_reduce();

    let mut interner = Interner::new();

    interner.program(&mut program);

    program.lambda_reduce().inline_reduce()
}
