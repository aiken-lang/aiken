use crate::Builder;
use uplc::parser;

#[test]
fn build_named__with_const() {
    let code = r"(program
                       11.22.33
                       (con integer 11)
                     )";
    let expected = parser::program(code).unwrap();
    let actual = Builder::default()
        .with_version(11, 22, 33)
        .with_constant_int(11)
        .build_named();
    assert_eq!(expected, actual);
}

#[test]
fn build_named__with_different_const() {
    let code = r"(program
                       11.22.33
                       (con integer 22)
                     )";
    let expected = parser::program(code).unwrap();
    let actual = Builder::default()
        .with_version(11, 22, 33)
        .with_constant_int(22)
        .build_named();
    assert_eq!(expected, actual);
}

#[test]
fn build_named__with_const_different_version() {
    let code = r"(program
                       44.55.66
                       (con integer 11)
                     )";
    let expected = parser::program(code).unwrap();
    let actual = Builder::default()
        .with_version(44, 55, 66)
        .with_constant_int(11)
        .build_named();
    assert_eq!(expected, actual);
}

#[test]
fn build_named__with_lam() {
    let code = r"(program
                       1.2.3
                       (lam i_0 (con integer 1))
                     )";
    let expected = parser::program(code).unwrap();
    let actual = Builder::default()
        .with_version(1, 2, 3)
        .with_lambda(1)
        .build_named();
    assert_eq!(expected, actual);
}
