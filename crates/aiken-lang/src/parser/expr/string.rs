use chumsky::prelude::*;

use crate::{
    expr::UntypedExpr,
    parser::{error::ParseError, literal::string, literal::utf8_string, token::Token},
};

pub fn parser() -> impl Parser<Token, UntypedExpr, Error = ParseError> {
    string().map_with_span(|value, span| UntypedExpr::String {
        location: span,
        value,
    })
}

pub fn hybrid() -> impl Parser<Token, UntypedExpr, Error = ParseError> {
    choice((
        string(),
        utf8_string().map(|(_, bytes)| String::from_utf8(bytes).unwrap()),
    ))
    .map_with_span(|value, span| UntypedExpr::String {
        location: span,
        value,
    })
}

#[cfg(test)]
mod tests {
    use crate::assert_expr;

    #[test]
    fn string_basic() {
        assert_expr!("@\"aiken\"");
    }
}
