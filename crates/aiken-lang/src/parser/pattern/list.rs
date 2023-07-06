use chumsky::prelude::*;

use crate::{
    ast::{self, UntypedPattern},
    parser::{
        error::{self, ParseError},
        token::Token,
    },
};

pub fn parser(
    expression: Recursive<'_, Token, UntypedPattern, ParseError>,
) -> impl Parser<Token, UntypedPattern, Error = ParseError> + '_ {
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
                Some(Some(pat @ (UntypedPattern::Var { .. } | UntypedPattern::Discard { .. }))) => {
                    Some(pat)
                }
                Some(Some(pat)) => {
                    emit(ParseError::expected_input_found(
                        pat.location(),
                        None,
                        Some(error::Pattern::Match),
                    ));

                    Some(pat)
                }
                // There is a tail but it has no content, implicit discard
                Some(None) => Some(UntypedPattern::Discard {
                    location: ast::Span {
                        start: span.end - 1,
                        end: span.end,
                    },
                    name: "_".to_string(),
                }),
                // No tail specified
                None => None,
            };

            UntypedPattern::List {
                location: span,
                elements,
                tail: tail.map(Box::new),
            }
        })
}
