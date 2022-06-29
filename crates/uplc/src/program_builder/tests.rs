use super::*;
use crate::parser;
use crate::program_builder::constant::WithConstant;

#[test]
fn build_named__with_const_different_version() {
    let code = r"(program
                       44.55.66
                       (con integer 11)
                     )";
    let expected = parser::program(code).unwrap();
    let actual = Builder::new(44, 55, 66).with_constant_int(11).build_named();
    assert_eq!(expected, actual);
}

#[test]
fn build_named__with_lam() {
    let code = r"(program
                       1.2.3
                       (lam i_0 (con integer 1))
                     )";
    let expected = parser::program(code).unwrap();
    let actual = Builder::new(1, 2, 3)
        .with_lambda("i_0")
        .with_constant_int(1)
        .build_named();
    assert_eq!(expected, actual);
}

#[test]
fn build_named__with_nested_lam() {
    let code = r"(program
                       1.2.3
                       (lam i_0 (lam i_1 (con integer 1)))
                     )";
    let expected = parser::program(code).unwrap();
    let actual = Builder::new(1, 2, 3)
        .with_lambda("i_0")
        .with_lambda("i_1")
        .with_constant_int(1)
        .build_named();
    assert_eq!(expected, actual);
}
