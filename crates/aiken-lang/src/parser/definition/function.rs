use chumsky::prelude::*;

use crate::{
    ast,
    expr::UntypedExpr,
    parser::{annotation, error::ParseError, expr, token::Token, utils},
};

pub fn parser() -> impl Parser<Token, ast::UntypedDefinition, Error = ParseError> {
    utils::optional_flag(Token::Pub)
        .then_ignore(just(Token::Fn))
        .then(select! {Token::Name {name} => name})
        .then(
            param(false)
                .separated_by(just(Token::Comma))
                .allow_trailing()
                .delimited_by(just(Token::LeftParen), just(Token::RightParen))
                .map_with_span(|arguments, span| (arguments, span)),
        )
        .then(just(Token::RArrow).ignore_then(annotation()).or_not())
        .then(
            expr::sequence()
                .or_not()
                .delimited_by(just(Token::LeftBrace), just(Token::RightBrace)),
        )
        .map_with_span(
            |((((public, name), (arguments, args_span)), return_annotation), body), span| {
                ast::UntypedDefinition::Fn(ast::Function {
                    arguments,
                    body: body.unwrap_or_else(|| UntypedExpr::todo(None, span)),
                    doc: None,
                    location: ast::Span {
                        start: span.start,
                        end: return_annotation
                            .as_ref()
                            .map(|l| l.location().end)
                            .unwrap_or_else(|| args_span.end),
                    },
                    end_position: span.end - 1,
                    name,
                    public,
                    return_annotation,
                    return_type: (),
                    can_error: true,
                })
            },
        )
}

pub fn param(is_validator_param: bool) -> impl Parser<Token, ast::UntypedArg, Error = ParseError> {
    choice((
        select! {Token::Name {name} => name}
            .then(select! {Token::DiscardName {name} => name})
            .map_with_span(|(label, name), span| ast::ArgName::Discarded {
                label,
                name,
                location: span,
            }),
        select! {Token::DiscardName {name} => name}.map_with_span(|name, span| {
            ast::ArgName::Discarded {
                label: name.clone(),
                name,
                location: span,
            }
        }),
        select! {Token::Name {name} => name}
            .then(select! {Token::Name {name} => name})
            .map_with_span(move |(label, name), span| ast::ArgName::Named {
                label,
                name,
                location: span,
                is_validator_param,
            }),
        select! {Token::Name {name} => name}.map_with_span(move |name, span| ast::ArgName::Named {
            label: name.clone(),
            name,
            location: span,
            is_validator_param,
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
    use crate::assert_definition;

    #[test]
    fn function_empty() {
        assert_definition!(
            r#"
            pub fn run() {}
            "#
        );
    }

    #[test]
    fn function_non_public() {
        assert_definition!(
            r#"
            fn run() {}
            "#
        );
    }
}
