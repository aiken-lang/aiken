use chumsky::prelude::*;

use crate::{
    ast,
    expr::{FnStyle, UntypedExpr},
    parser::{annotation, error::ParseError, token::Token},
};

pub fn parser(
    sequence: Recursive<'_, Token, UntypedExpr, ParseError>,
) -> impl Parser<Token, UntypedExpr, Error = ParseError> + '_ {
    just(Token::Fn)
        .ignore_then(
            params()
                .separated_by(just(Token::Comma))
                .allow_trailing()
                .delimited_by(just(Token::LeftParen), just(Token::RightParen)),
        )
        .then(just(Token::Pure).ignored().or_not())
        .then(just(Token::RArrow).ignore_then(annotation()).or_not())
        .then(sequence.delimited_by(just(Token::LeftBrace), just(Token::RightBrace)))
        .map_with_span(
            |(((arguments, pure_opt), return_annotation), body), span| UntypedExpr::Fn {
                arguments,
                body: Box::new(body),
                location: span,
                fn_style: FnStyle::Plain,
                return_annotation,
                is_pure: pure_opt.is_some(),
            },
        )
}

pub fn params() -> impl Parser<Token, ast::UntypedArg, Error = ParseError> {
    // TODO: return a better error when a label is provided `UnexpectedLabel`
    choice((
        select! {Token::DiscardName {name} => name}.map_with_span(|name, span| {
            ast::ArgName::Discarded {
                label: name.clone(),
                name,
                location: span,
            }
        }),
        select! {Token::Name {name} => name}.map_with_span(|name, span| ast::ArgName::Named {
            label: name.clone(),
            name,
            location: span,
            is_validator_param: false,
        }),
    ))
    .then(just(Token::Colon).ignore_then(annotation()).or_not())
    .map_with_span(|(arg_name, annotation), span| ast::Arg {
        location: span,
        annotation,
        tipo: (),
        arg_name,
    })
}

#[cfg(test)]
mod tests {
    use crate::assert_expr;

    #[test]
    fn anonymous_function_basic() {
        assert_expr!(r#"fn (a: Int) -> Int { a + 1 }"#);
    }
}
