use chumsky::prelude::*;

use crate::{
    ast::UntypedPattern,
    parser::{error::ParseError, literal, token::Token},
};

pub fn parser() -> impl Parser<Token, UntypedPattern, Error = ParseError> {
    literal::int().map_with_span(|(value, base), location| UntypedPattern::Int {
        location,
        value,
        base,
    })
}

#[cfg(test)]
mod tests {
    use crate::assert_expr;

    #[test]
    fn pattern_negative_int() {
        assert_expr!(
            r#"
            when foo is {
              -1 -> True
            }
            "#
        );
    }

    #[test]
    fn pattern_negative_int_not_first_case() {
        assert_expr!(
            r#"
            when bar is {
              42 -> -14
              -42 -> 14
            }
            "#
        );
    }
}
