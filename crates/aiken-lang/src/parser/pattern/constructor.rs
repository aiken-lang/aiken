use chumsky::prelude::*;

use crate::{
    ast::{CallArg, Span, UntypedPattern},
    parser::{error::ParseError, token::Token},
};

pub fn parser(
    pattern: Recursive<'_, Token, UntypedPattern, ParseError>,
) -> impl Parser<Token, UntypedPattern, Error = ParseError> + '_ {
    select! {Token::UpName { name } => name}
        .then(args(pattern))
        .map_with_span(
            |(name, (arguments, spread_location, is_record)), location| {
                UntypedPattern::Constructor {
                    is_record,
                    location,
                    name,
                    arguments,
                    module: None,
                    constructor: (),
                    spread_location,
                    tipo: (),
                }
            },
        )
}

pub(crate) fn args(
    pattern: Recursive<'_, Token, UntypedPattern, ParseError>,
) -> impl Parser<Token, (Vec<CallArg<UntypedPattern>>, Option<Span>, bool), Error = ParseError> + '_
{
    let record_constructor_pattern_arg_parser = choice((
        select! {Token::Name {name} => name}
            .then_ignore(just(Token::Colon))
            .then(pattern.clone())
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
            .ignored()
            .map_with_span(|_spread, span| span)
            .then_ignore(just(Token::Comma).or_not())
            .or_not(),
    )
    .delimited_by(just(Token::LeftBrace), just(Token::RightBrace));

    let tuple_constructor_pattern_arg_parser = pattern
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
                .ignored()
                .map_with_span(|_spread, span| span)
                .then_ignore(just(Token::Comma).or_not())
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
            .map(|((a, b), c)| (a, b, c))
            .unwrap_or_else(|| (vec![], None, false))
    })
}

#[cfg(test)]
mod tests {
    use crate::assert_pattern;

    #[test]
    fn constructor_basic() {
        assert_pattern!("True");
    }

    #[test]
    fn constructor_module_select() {
        assert_pattern!("module.Foo");
    }
}
