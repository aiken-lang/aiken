/// e2e encoding/decoding tests
use uplc::{
    ast::{DeBruijn, Name, Program},
    parser,
};

fn round_trip_test(bytes: &[u8], code: &str) {
    parsed_program_matches_decoded_bytes(bytes, code);
    encoded_program_matches_bytes(bytes, code);
    can_convert_between_de_bruijn_and_name(bytes, code);
    decoded_bytes_can_convert_into_code(bytes, code);
}

fn parsed_program_matches_decoded_bytes(bytes: &[u8], code: &str) {
    let parsed_program = parser::program(code).unwrap();

    let debruijn_program: Program<DeBruijn> = parsed_program.try_into().unwrap();

    let decoded_program: Program<DeBruijn> = Program::from_flat(bytes).unwrap();

    assert_eq!(debruijn_program, decoded_program);
}

fn encoded_program_matches_bytes(bytes: &[u8], code: &str) {
    let parsed_program = parser::program(code).unwrap();

    let debruijn_program: Program<DeBruijn> = parsed_program.try_into().unwrap();

    let encoded_program = debruijn_program.to_flat().unwrap();

    assert_eq!(encoded_program, bytes);
}
fn can_convert_between_de_bruijn_and_name(bytes: &[u8], code: &str) {
    let parsed_program = parser::program(code).unwrap();

    let decoded_program: Program<DeBruijn> = Program::from_flat(bytes).unwrap();

    let name_program: Program<Name> = decoded_program.try_into().unwrap();

    assert_eq!(parsed_program, name_program);
}

fn decoded_bytes_can_convert_into_code(bytes: &[u8], code: &str) {
    let decoded_program: Program<DeBruijn> = Program::from_flat(bytes).unwrap();

    let name_program: Program<Name> = decoded_program.try_into().unwrap();

    let pretty = name_program.to_pretty();

    assert_eq!(pretty, code);
}

#[test]
fn integer() {
    let bytes = include_bytes!("../test_data/basic/integer/integer.flat");
    let code = include_str!("../test_data/basic/integer/integer.uplc");

    round_trip_test(bytes, code);
}

#[test]
fn jpg() {
    let bytes = include_bytes!("../test_data/jpg/jpg.flat");
    let code = include_str!("../test_data/jpg/jpg.uplc");

    round_trip_test(bytes, code);
}

#[test]
fn fibonacci() {
    let bytes = include_bytes!("../test_data/fibonacci/fibonacci.flat");
    let code = include_str!("../test_data/fibonacci/fibonacci.uplc");

    round_trip_test(bytes, code);
}

#[test]
fn case_constr() {
    let bytes = include_bytes!("../test_data/case_constr/case_constr.flat");
    let code = include_str!("../test_data/case_constr/case_constr.uplc");

    round_trip_test(bytes, code);
}

#[test]
fn one_way_fibonacci() {
    let bytes = include_bytes!("../test_data/fibonacci/fibonacci.flat");
    // This code doesn't match the expected `i_unique` naming scheme, so it can't be round-tripped.
    // We still want to test these "unsanitary" cases because we can't control the naming pattern
    // the consumer uses. We just can't guarantee that the decoded Flat bytes will match their
    // names.
    let code = include_str!("../test_data/fibonacci/unsanitary_fibonacci.uplc");

    parsed_program_matches_decoded_bytes(bytes, code);
    encoded_program_matches_bytes(bytes, code);
}
