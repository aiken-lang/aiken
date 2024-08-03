use crate::{
    ast::{ByteArrayFormatPreference, UntypedPattern},
    parser::{error::ParseError, literal, token::Token},
};
use chumsky::prelude::*;

pub fn parser() -> impl Parser<Token, UntypedPattern, Error = ParseError> {
    literal::string().validate(|_, location, emit| {
        emit(ParseError::match_string(location));

        UntypedPattern::ByteArray {
            location,
            value: Vec::new(),
            preferred_format: ByteArrayFormatPreference::Utf8String,
        }
    })
}

#[cfg(test)]
mod tests {
    use crate::assert_expr;

    #[test]
    fn pattern_string() {
        assert_expr!(
            r#"
            when foo is {
              @"foo" -> True
            }
            "#
        );
    }
}
