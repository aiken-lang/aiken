use chumsky::prelude::*;

use crate::{
    ast,
    expr::UntypedExpr,
    parser::{error::ParseError, expr, token::Token},
};

pub fn parser() -> impl Parser<Token, ast::UntypedDefinition, Error = ParseError> {
    just(Token::Bang)
        .ignored()
        .or_not()
        .then_ignore(just(Token::Test))
        .then(select! {Token::Name {name} => name})
        .then_ignore(just(Token::LeftParen))
        .then_ignore(just(Token::RightParen))
        .map_with_span(|name, span| (name, span))
        .then(
            expr::sequence()
                .or_not()
                .delimited_by(just(Token::LeftBrace), just(Token::RightBrace)),
        )
        .map_with_span(|(((fail, name), span_end), body), span| {
            ast::UntypedDefinition::Test(ast::Function {
                arguments: vec![],
                body: body.unwrap_or_else(|| UntypedExpr::todo(span, None)),
                doc: None,
                location: span_end,
                end_position: span.end - 1,
                name,
                public: false,
                return_annotation: None,
                return_type: (),
                can_error: fail.is_some(),
            })
        })
}

#[cfg(test)]
mod tests {
    use crate::assert_definition;

    #[test]
    fn test_fail() {
        assert_definition!(
            r#"
            !test invalid_inputs() {
              expect True = False

              False
            }
            "#
        );
    }
}
