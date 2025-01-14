use crate::{
    ast,
    parser::{error::ParseError, token::Token},
};
use chumsky::prelude::*;

pub fn parser() -> impl Parser<Token, ast::UntypedDefinition, Error = ParseError> {
    crate::parser::definition::test_like::parser(Token::Benchmark)
}

#[cfg(test)]
mod tests {
    use crate::assert_definition;

    #[test]
    fn def_benchmark() {
        assert_definition!(
            r#"
            bench foo(x via fuzz.any_int) {
                True
            }
            "#
        );
    }

    #[test]
    fn def_invalid_benchmark() {
        assert_definition!(
            r#"
            bench foo(x via f, y via g) {
                True
            }
            "#
        );
    }

    #[test]
    fn def_benchmark_annotated_fuzzer() {
        assert_definition!(
            r#"
            bench foo(x: Int via foo()) {
                True
            }
            "#
        );
    }
}
