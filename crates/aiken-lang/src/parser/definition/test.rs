use chumsky::prelude::*;

use crate::{
    ast,
    expr::UntypedExpr,
    parser::{error::ParseError, expr, token::Token},
};

pub fn parser() -> impl Parser<Token, ast::UntypedDefinition, Error = ParseError> {
    // TODO: can remove Token::Bang after a few releases (curr v1.0.11)
    just(Token::Bang)
        .ignored()
        .or_not()
        .then_ignore(just(Token::Test))
        .then(select! {Token::Name {name} => name})
        .then_ignore(just(Token::LeftParen))
        .then_ignore(just(Token::RightParen))
        .then(just(Token::Fail).ignored().or_not())
        .map_with_span(|name, span| (name, span))
        .then(
            expr::sequence()
                .or_not()
                .delimited_by(just(Token::LeftBrace), just(Token::RightBrace)),
        )
        .map_with_span(|((((old_fail, name), fail), span_end), body), span| {
            ast::UntypedDefinition::Test(ast::Function {
                arguments: vec![],
                body: body.unwrap_or_else(|| UntypedExpr::todo(None, span)),
                doc: None,
                location: span_end,
                end_position: span.end - 1,
                name,
                public: false,
                return_annotation: None,
                return_type: (),
                can_error: fail.is_some() || old_fail.is_some(),
            })
        })
}

#[cfg(test)]
mod tests {
    use crate::assert_definition;

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
}
