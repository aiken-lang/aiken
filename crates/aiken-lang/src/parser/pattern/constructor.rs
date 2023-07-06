use chumsky::prelude::*;

use crate::{
    ast::{CallArg, UntypedPattern},
    parser::{error::ParseError, token::Token},
};

pub fn parser(
    expression: Recursive<'_, Token, UntypedPattern, ParseError>,
) -> impl Parser<Token, UntypedPattern, Error = ParseError> + '_ {
    select! {Token::UpName { name } => name}
        .then(args(expression))
        .map_with_span(|(name, (arguments, with_spread, is_record)), location| {
            UntypedPattern::Constructor {
                is_record,
                location,
                name,
                arguments,
                module: None,
                constructor: (),
                with_spread,
                tipo: (),
            }
        })
}

pub(crate) fn args(
    expression: Recursive<'_, Token, UntypedPattern, ParseError>,
) -> impl Parser<Token, (Vec<CallArg<UntypedPattern>>, bool, bool), Error = ParseError> + '_ {
    let record_constructor_pattern_arg_parser = choice((
        select! {Token::Name {name} => name}
            .then_ignore(just(Token::Colon))
            .then(expression.clone())
            .map_with_span(|(name, pattern), span| CallArg {
                location: span,
                label: Some(name),
                value: pattern,
            }),
        select! {Token::Name{name} => name}.map_with_span(|name, span| CallArg {
            location: span,
            value: UntypedPattern::Var {
                name: name.clone(),
                location: span,
            },
            label: Some(name),
        }),
    ))
    .separated_by(just(Token::Comma))
    .allow_trailing()
    .then(
        just(Token::DotDot)
            .then_ignore(just(Token::Comma).or_not())
            .ignored()
            .or_not(),
    )
    .delimited_by(just(Token::LeftBrace), just(Token::RightBrace));

    let tuple_constructor_pattern_arg_parser = expression
        .clone()
        .map(|pattern| CallArg {
            location: pattern.location(),
            value: pattern,
            label: None,
        })
        .separated_by(just(Token::Comma))
        .allow_trailing()
        .then(
            just(Token::DotDot)
                .then_ignore(just(Token::Comma).or_not())
                .ignored()
                .or_not(),
        )
        .delimited_by(just(Token::LeftParen), just(Token::RightParen));

    choice((
        record_constructor_pattern_arg_parser.map(|a| (a, true)),
        tuple_constructor_pattern_arg_parser.map(|a| (a, false)),
    ))
    .or_not()
    .map(|opt_args| {
        opt_args
            .map(|((a, b), c)| (a, b.is_some(), c))
            .unwrap_or_else(|| (vec![], false, false))
    })
}
