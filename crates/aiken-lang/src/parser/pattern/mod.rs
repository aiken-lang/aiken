use chumsky::prelude::*;

use crate::ast;

use super::{
    error::{self, ParseError},
    token::Token,
};

pub fn parser() -> impl Parser<Token, ast::UntypedPattern, Error = ParseError> {
    recursive(|expression| {
        let record_constructor_pattern_arg_parser = choice((
            select! {Token::Name {name} => name}
                .then_ignore(just(Token::Colon))
                .then(expression.clone())
                .map_with_span(|(name, pattern), span| ast::CallArg {
                    location: span,
                    label: Some(name),
                    value: pattern,
                }),
            select! {Token::Name{name} => name}.map_with_span(|name, span| ast::CallArg {
                location: span,
                value: ast::UntypedPattern::Var {
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
            .map(|pattern| ast::CallArg {
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

        let constructor_pattern_args_parser = choice((
            record_constructor_pattern_arg_parser.map(|a| (a, true)),
            tuple_constructor_pattern_arg_parser.map(|a| (a, false)),
        ))
        .or_not()
        .map(|opt_args| {
            opt_args
                .map(|((a, b), c)| (a, b.is_some(), c))
                .unwrap_or_else(|| (vec![], false, false))
        });

        let constructor_pattern_parser =
            select! {Token::UpName { name } => name}.then(constructor_pattern_args_parser);

        choice((
            select! { Token::Name {name} => name }
                .then(
                    just(Token::Dot)
                        .ignore_then(constructor_pattern_parser.clone())
                        .or_not(),
                )
                .map_with_span(|(name, opt_pattern), span| {
                    if let Some((c_name, (arguments, with_spread, is_record))) = opt_pattern {
                        ast::UntypedPattern::Constructor {
                            is_record,
                            location: span,
                            name: c_name,
                            arguments,
                            module: Some(name),
                            constructor: (),
                            with_spread,
                            tipo: (),
                        }
                    } else {
                        ast::UntypedPattern::Var {
                            location: span,
                            name,
                        }
                    }
                }),
            constructor_pattern_parser.map_with_span(
                |(name, (arguments, with_spread, is_record)), span| {
                    ast::UntypedPattern::Constructor {
                        is_record,
                        location: span,
                        name,
                        arguments,
                        module: None,
                        constructor: (),
                        with_spread,
                        tipo: (),
                    }
                },
            ),
            select! {Token::DiscardName {name} => name}.map_with_span(|name, span| {
                ast::UntypedPattern::Discard {
                    name,
                    location: span,
                }
            }),
            select! {Token::Int {value, base} => (value, base)}.map_with_span(
                |(value, base), span| ast::UntypedPattern::Int {
                    location: span,
                    value,
                    base,
                },
            ),
            expression
                .clone()
                .separated_by(just(Token::Comma))
                .allow_trailing()
                .delimited_by(
                    choice((just(Token::LeftParen), just(Token::NewLineLeftParen))),
                    just(Token::RightParen),
                )
                .map_with_span(|elems, span| ast::UntypedPattern::Tuple {
                    location: span,
                    elems,
                }),
            just(Token::LeftSquare)
                .ignore_then(expression.clone().separated_by(just(Token::Comma)))
                .then(choice((
                    just(Token::Comma).ignore_then(
                        just(Token::DotDot)
                            .ignore_then(expression.clone().or_not())
                            .or_not(),
                    ),
                    just(Token::Comma).ignored().or_not().map(|_| None),
                )))
                .then_ignore(just(Token::RightSquare))
                .validate(|(elements, tail), span: ast::Span, emit| {
                    let tail = match tail {
                        // There is a tail and it has a Pattern::Var or Pattern::Discard
                        Some(Some(
                            pat @ (ast::UntypedPattern::Var { .. }
                            | ast::UntypedPattern::Discard { .. }),
                        )) => Some(pat),
                        Some(Some(pat)) => {
                            emit(ParseError::expected_input_found(
                                pat.location(),
                                None,
                                Some(error::Pattern::Match),
                            ));

                            Some(pat)
                        }
                        // There is a tail but it has no content, implicit discard
                        Some(None) => Some(ast::UntypedPattern::Discard {
                            location: ast::Span {
                                start: span.end - 1,
                                end: span.end,
                            },
                            name: "_".to_string(),
                        }),
                        // No tail specified
                        None => None,
                    };

                    ast::UntypedPattern::List {
                        location: span,
                        elements,
                        tail: tail.map(Box::new),
                    }
                }),
        ))
        .then(
            just(Token::As)
                .ignore_then(select! { Token::Name {name} => name})
                .or_not(),
        )
        .map_with_span(|(pattern, opt_as), span| {
            if let Some(name) = opt_as {
                ast::UntypedPattern::Assign {
                    name,
                    location: span,
                    pattern: Box::new(pattern),
                }
            } else {
                pattern
            }
        })
    })
}
