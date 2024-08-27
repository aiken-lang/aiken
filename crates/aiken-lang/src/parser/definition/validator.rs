use super::function::param;
use crate::{
    ast::{self, well_known},
    expr::UntypedExpr,
    parser::{annotation, error::ParseError, expr, token::Token},
};
use chumsky::prelude::*;

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
                .map_with_span(|(name, mut function), span| {
                    function.name = name;
                    function.location.start = span.start;

                    function
                })
                .repeated()
                .then(
                    just(Token::Else)
                        .ignore_then(args_and_body().map_with_span(|mut function, span| {
                            function.name = well_known::VALIDATOR_ELSE.to_string();
                            function.location.start = span.start;

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

                let location = ast::Span {
                    start: span.start,
                    // capture the span from the optional params
                    end: params_span.end,
                };

                ast::UntypedDefinition::Validator(ast::Validator {
                    doc: None,
                    name,
                    handlers,
                    location,
                    params,
                    end_position: span.end - 1,
                    fallback: opt_catch_all
                        .unwrap_or(ast::UntypedValidator::default_fallback(location)),
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
            |(((arguments, args_span), return_annotation), body), span| {
                let location = ast::Span {
                    start: span.start,
                    end: return_annotation
                        .as_ref()
                        .map(|l| l.location().end)
                        .unwrap_or_else(|| args_span.end),
                };

                ast::Function {
                    arguments,
                    body: body.unwrap_or_else(|| UntypedExpr::todo(None, span)),
                    doc: None,
                    location,
                    end_position: span.end - 1,
                    name: "temp".to_string(),
                    public: true,
                    return_annotation: return_annotation
                        .or(Some(ast::Annotation::boolean(location))),
                    return_type: (),
                    on_test_failure: ast::OnTestFailure::FailImmediately,
                }
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
