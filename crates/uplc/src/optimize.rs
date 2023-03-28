use crate::{
    ast::{Name, NamedDeBruijn, Program},
    parser::interner::Interner,
};

pub mod shrinker;

pub fn aiken_optimize_and_intern(program: Program<Name>) -> Program<Name> {
    let mut program = program.builtin_force_reduce();

    let mut interner = Interner::new();

    interner.program(&mut program);

    // Use conversion to Debruijn to prevent optimizations from affecting shadowing
    let program_named: Program<NamedDeBruijn> = program.try_into().unwrap();

    let program: Program<Name> = program_named.try_into().unwrap();

    program
        .lambda_reduce()
        .inline_reduce()
        .lambda_reduce()
        .inline_reduce()
}
