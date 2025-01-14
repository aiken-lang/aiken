use crate::{
    ast,
    ast::OnTestFailure,
    expr::UntypedExpr,
    parser::{
        annotation,
        chain::{call::parser as call, field_access, tuple_index::parser as tuple_index, Chain},
        error::ParseError,
        expr::{self, bytearray, int as uint, list, string, tuple, var},
        pattern,
        token::Token,
    },
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
    recursive(|expression| {
        let chain = choice((
            tuple_index(),
            field_access::parser(),
            call(expression.clone()),
        ));

        let int = || {
            just(Token::Minus)
                .to(ast::UnOp::Negate)
                .map_with_span(|op, span| (op, span))
                .or_not()
                .then(uint())
                .map(|(op, value)| match op {
                    None => value,
                    Some((op, location)) => UntypedExpr::UnOp {
                        op,
                        location,
                        value: Box::new(value),
                    },
                })
        };

        choice((
            int(),
            string(),
            bytearray(),
            tuple(expression.clone()),
            list(expression.clone()),
            var(),
        ))
        .then(chain.repeated())
        .foldl(|expr, chain| match chain {
            Chain::Call(args, span) => expr.call(args, span),
            Chain::FieldAccess(label, span) => expr.field_access(label, span),
            Chain::TupleIndex(index, span) => expr.tuple_index(index, span),
        })
    })
}
