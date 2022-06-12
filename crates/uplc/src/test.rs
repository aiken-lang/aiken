// e2e encoding/decoding tests
use crate::{
    ast::{DeBruijn, Program},
    parser,
};

#[test]
fn integer() {
    let bytes = include_bytes!("../test_data/basic/integer/integer.flat");
    let code = include_str!("../test_data/basic/integer/integer.uplc");

    let parsed_program = parser::program(code).unwrap();

    let debruijn_program: Program<DeBruijn> = parsed_program.try_into().unwrap();

    let decoded_program: Program<DeBruijn> = Program::from_flat(bytes).unwrap();

    assert_eq!(debruijn_program, decoded_program);

    let encoded_program = debruijn_program.to_flat().unwrap();

    assert_eq!(encoded_program, bytes);
}

#[test]
fn jpg() {
    let bytes = include_bytes!("../test_data/jpg/jpg.flat");
    let code = include_str!("../test_data/jpg/jpg.uplc");

    let parsed_program = parser::program(code).unwrap();

    let debruijn_program: Program<DeBruijn> = parsed_program.try_into().unwrap();

    let decoded_program: Program<DeBruijn> = Program::from_flat(bytes).unwrap();

    assert_eq!(debruijn_program, decoded_program);

    let encoded_program = debruijn_program.to_flat().unwrap();

    assert_eq!(encoded_program, bytes);
}
