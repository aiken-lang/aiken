use super::*;
use crate::parser;
use crate::program_builder::constant::WithConstant;
use proptest::prelude::*;

proptest! {
    #[test]
    fn build_named__with_version(
        maj: isize,
        min: isize,
        patch: isize,
    ) {
        let maj = maj.abs() as usize;
        let min = min.abs() as usize;
        let patch = patch.abs() as usize;
        let code = format!(r"(program
                           {}.{}.{}
                           (con integer 11)
                         )", maj, min, patch);
        let expected = parser::program(&code).unwrap();
        let actual = Builder::new(maj, min, patch).with_constant_int(11).build_named();
        assert_eq!(expected, actual);
    }
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
