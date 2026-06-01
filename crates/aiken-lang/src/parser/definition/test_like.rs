use crate::{
    ast,
    ast::OnTestFailure,
    expr::UntypedExpr,
    parser::{annotation, error::ParseError, expr, pattern, token::Token},
};
use chumsky::prelude::*;

pub fn parser(keyword: Token) -> impl Parser<Token, ast::UntypedDefinition, Error = ParseError> {
    just(keyword.clone())
        .ignore_then(select! {Token::Name {name} => name})
        .then(
            via()
                .separated_by(just(Token::Comma))
                .allow_trailing()
                .delimited_by(just(Token::LeftParen), just(Token::RightParen)),
        )
        .then(
            just(Token::Fail)
                .ignore_then(just(Token::Once).ignored().or_not().map(|once| {
                    once.map(|_| OnTestFailure::SucceedImmediately)
                        .unwrap_or(OnTestFailure::SucceedEventually)
                }))
                .or_not(),
        )
        .map_with_span(|name, span| (name, span))
        .then(
            expr::sequence()
                .or_not()
                .delimited_by(just(Token::LeftBrace), just(Token::RightBrace)),
        )
        .map_with_span(
            move |((((name, arguments), fail), span_end), body), span| match keyword {
                Token::Test => ast::UntypedDefinition::Test(ast::Function {
                    arguments,
                    body: body.unwrap_or_else(|| UntypedExpr::todo(None, span)),
                    doc: None,
                    location: span_end,
                    end_position: span.end - 1,
                    name,
                    public: false,
                    return_annotation: None,
                    return_type: (),
                    on_test_failure: fail.unwrap_or(OnTestFailure::FailImmediately),
                }),
                Token::Benchmark => ast::UntypedDefinition::Benchmark(ast::Function {
                    arguments,
                    body: body.unwrap_or_else(|| UntypedExpr::todo(None, span)),
                    doc: None,
                    location: span_end,
                    end_position: span.end - 1,
                    name,
                    public: false,
                    return_annotation: None,
                    return_type: (),
                    on_test_failure: fail.unwrap_or(OnTestFailure::FailImmediately),
                }),
                _ => unreachable!("Only Test and Benchmark tokens are supported"),
            },
        )
}

pub fn via() -> impl Parser<Token, ast::UntypedArgVia, Error = ParseError> {
    choice((
        select! {Token::DiscardName {name} => name}.map_with_span(|name, span| {
            ast::ArgBy::ByName(ast::ArgName::Discarded {
                label: name.clone(),
                name,
                location: span,
            })
        }),
        select! {Token::Name {name} => name}.map_with_span(|name, location| {
            ast::ArgBy::ByName(ast::ArgName::Named {
                label: name.clone(),
                name,
                location,
            })
        }),
        pattern().map(ast::ArgBy::ByPattern),
    ))
    .then(just(Token::Colon).ignore_then(annotation()).or_not())
    .map_with_span(|(arg_name, annotation), location| (arg_name, annotation, location))
    .then_ignore(just(Token::Via))
    .then(fuzzer())
    .map(|((by, annotation, location), via)| ast::ArgVia {
        arg: ast::UntypedArg {
            by,
            annotation,
            location,
            doc: None,
            is_validator_param: false,
        },
        via,
    })
}

pub fn fuzzer<'a>() -> impl Parser<Token, UntypedExpr, Error = ParseError> + 'a {
    expr::sequence()
}

#[cfg(test)]
mod tests {
    use crate::assert_definition;

    #[test]
    fn test_pair_in_via() {
        assert_definition!(
            r#"
            test foo(x via some_generator(Pair(14, 42))) {
              x == 56
            }
            "#
        );
    }

    #[test]
    fn test_parse_via_negative_int() {
        assert_definition!(
            r#"
            test foo(n via int_between(-10, 10)) {
              x * x <= 100
            }
            "#
        );
    }

    #[test]
    fn test_parse_via_negative_int_newline() {
        assert_definition!(
            r#"
            test foo(
              n via int_between(
                -10,
                10,
              )
            ) {
              x * x <= 100
            }
            "#
        );
    }

    #[test]
    fn test_parse_via_anonymous_function_argument() {
        assert_definition!(
            r#"
            test foo(
              xs via fuzz.map2(
                fuzz.int(),
                fuzz.int(),
                fn(a, b) { [a, b] },
              )
            ) {
              True
            }
            "#
        );
    }
}
