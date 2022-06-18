/// e2e encoding/decoding tests
use crate::{
    ast::{DeBruijn, Name, Program},
    parser,
};

#[test]
fn integer() {
    let bytes = include_bytes!("../test_data/basic/integer/integer.flat");
    let code = include_str!("../test_data/basic/integer/integer.uplc");

    let parsed_program = parser::program(code).unwrap();

    let debruijn_program: Program<DeBruijn> = parsed_program.clone().try_into().unwrap();

    let decoded_program: Program<DeBruijn> = Program::from_flat(bytes).unwrap();

    assert_eq!(debruijn_program, decoded_program);

    let encoded_program = debruijn_program.to_flat().unwrap();

    assert_eq!(encoded_program, bytes);

    let name_program: Program<Name> = decoded_program.try_into().unwrap();

    assert_eq!(parsed_program, name_program);
}

#[test]
fn jpg() {
    let bytes = include_bytes!("../test_data/jpg/jpg.flat");
    let code = include_str!("../test_data/jpg/jpg.uplc");

    let parsed_program = parser::program(code).unwrap();

    let debruijn_program: Program<DeBruijn> = parsed_program.clone().try_into().unwrap();

    let decoded_program: Program<DeBruijn> = Program::from_flat(bytes).unwrap();

    assert_eq!(debruijn_program, decoded_program);

    let encoded_program = debruijn_program.to_flat().unwrap();

    assert_eq!(encoded_program, bytes);

    let name_program: Program<Name> = decoded_program.try_into().unwrap();

    assert_eq!(parsed_program, name_program);

    let pretty = name_program.to_pretty();

    assert_eq!(pretty, code);
}
