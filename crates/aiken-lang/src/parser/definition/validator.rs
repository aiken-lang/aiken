use chumsky::prelude::*;

use crate::{
    ast::{self, ArgBy, ArgName},
    expr::UntypedExpr,
    parser::{annotation, error::ParseError, expr, token::Token},
};

use super::function::param;

pub fn parser() -> impl Parser<Token, ast::UntypedDefinition, Error = ParseError> {
    just(Token::Validator)
        .ignore_then(select! {Token::Name {name} => name})
        .then(
            param(true)
                .separated_by(just(Token::Comma))
                .allow_trailing()
                .delimited_by(just(Token::LeftParen), just(Token::RightParen))
                .map_with_span(|arguments, span| (arguments, span))
                .or_not(),
        )
        // so far: validator my_validator(arg1: Whatever)
        .then(
            select! {Token::Name {name} => name}
                .then(args_and_body())
                .map(|(name, mut function)| {
                    function.name = name;

                    function
                })
                .repeated()
                .then(
                    just(Token::Else)
                        .ignore_then(args_and_body().map(|mut function| {
                            function.name = "else".to_string();

                            function
                        }))
                        .or_not(),
                )
                .delimited_by(just(Token::LeftBrace), just(Token::RightBrace)),
        )
        .map_with_span(
            |((name, opt_extra_params), (handlers, opt_catch_all)), span| {
                let (params, params_span) = opt_extra_params.unwrap_or((
                    vec![],
                    ast::Span {
                        start: 0,
                        end: span.start + "validator".len(),
                    },
                ));

                ast::UntypedDefinition::Validator(ast::Validator {
                    doc: None,
                    name,
                    handlers,
                    location: ast::Span {
                        start: span.start,
                        // capture the span from the optional params
                        end: params_span.end,
                    },
                    params,
                    end_position: span.end - 1,
                    fallback: opt_catch_all.unwrap_or(ast::Function {
                        arguments: vec![ast::UntypedArg {
                            by: ArgBy::ByName(ArgName::Discarded {
                                name: "_ctx".to_string(),
                                label: "_ctx".to_string(),
                                location: ast::Span::empty(),
                            }),
                            location: ast::Span::empty(),
                            annotation: None,
                            doc: None,
                            is_validator_param: false,
                        }],
                        body: UntypedExpr::fail(None, ast::Span::empty()),
                        doc: None,
                        location: ast::Span::empty(),
                        end_position: span.end - 1,
                        name: "else".to_string(),
                        public: true,
                        return_annotation: None,
                        return_type: (),
                        on_test_failure: ast::OnTestFailure::FailImmediately,
                    }),
                })
            },
        )
}

pub fn args_and_body() -> impl Parser<Token, ast::UntypedFunction, Error = ParseError> {
    param(false)
        .separated_by(just(Token::Comma))
        .allow_trailing()
        .delimited_by(just(Token::LeftParen), just(Token::RightParen))
        .map_with_span(|arguments, span| (arguments, span))
        .then(just(Token::RArrow).ignore_then(annotation()).or_not())
        .then(
            expr::sequence()
                .or_not()
                .delimited_by(just(Token::LeftBrace), just(Token::RightBrace)),
        )
        .map_with_span(
            |(((arguments, args_span), return_annotation), body), span| ast::Function {
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
                name: "temp".to_string(),
                public: true,
                return_annotation,
                return_type: (),
                on_test_failure: ast::OnTestFailure::FailImmediately,
            },
        )
}

#[cfg(test)]
mod tests {
    use crate::assert_definition;

    #[test]
    fn validator() {
        assert_definition!(
            r#"
            validator hello {
              spend (datum, rdmr, ctx) {
                True
              }
            }
            "#
        );
    }

    #[test]
    fn double_validator() {
        assert_definition!(
            r#"
            validator thing {
              spend (datum, rdmr, ctx) {
                True
              }

              mint (rdmr, ctx) {
                True
              }
            }
            "#
        );
    }

    #[test]
    fn fallback() {
        assert_definition!(
            r#"
            validator thing {
              spend (datum, rdmr, ctx) {
                True
              }

              mint (rdmr, ctx) {
                True
              }

              else (_) {
                fail
              }
            }
            "#
        );
    }
}
