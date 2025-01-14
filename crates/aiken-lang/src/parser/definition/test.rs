use crate::{
    ast,
    parser::{error::ParseError, token::Token},
};
use chumsky::prelude::*;

pub fn parser() -> impl Parser<Token, ast::UntypedDefinition, Error = ParseError> {
    crate::parser::definition::test_like::parser(Token::Test)
}

#[cfg(test)]
mod tests {
    use crate::assert_definition;

    #[test]
    fn def_test() {
        assert_definition!(
            r#"
            test foo() {
                True
            }
            "#
        );
    }

    #[test]
    fn def_test_fail() {
        assert_definition!(
            r#"
            test invalid_inputs() fail {
              expect True = False

              False
            }
            "#
        );
    }

    #[test]
    fn def_property_test() {
        assert_definition!(
            r#"
            test foo(x via fuzz.any_int) {
                True
            }
            "#
        );
    }

    #[test]
    fn def_invalid_property_test() {
        assert_definition!(
            r#"
            test foo(x via f, y via g) {
                True
            }
            "#
        );
    }

    #[test]
    fn def_property_test_annotated_fuzzer() {
        assert_definition!(
            r#"
            test foo(x: Int via foo()) {
                True
            }
            "#
        );
    }
}
