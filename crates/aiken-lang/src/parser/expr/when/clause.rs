use chumsky::prelude::*;
use vec1::vec1;

use crate::{
    ast,
    expr::UntypedExpr,
    parser::{error::ParseError, expr::string::flexible, pattern, token::Token},
};

use super::guard;

pub fn parser(
    r: Recursive<'_, Token, UntypedExpr, ParseError>,
) -> impl Parser<Token, ast::UntypedClause, Error = ParseError> + '_ {
    pattern()
        .then(just(Token::Vbar).ignore_then(pattern()).repeated().or_not())
        .then(choice((
            just(Token::If)
                .ignore_then(guard())
                .or_not()
                .then_ignore(just(Token::RArrow)),
            just(Token::If)
                .ignore_then(take_until(just(Token::RArrow)))
                .validate(|_value, span, emit| {
                    emit(ParseError::invalid_when_clause_guard(span));
                    None
                }),
        )))
        // TODO: add hint "Did you mean to wrap a multi line clause in curly braces?"
        .then(choice((
            r.clone(),
            just(Token::Todo)
                .ignore_then(
                    r.clone()
                        .then_ignore(one_of(Token::RArrow).not().rewind())
                        .or_not(),
                )
                .map_with_span(|reason, span| UntypedExpr::todo(span, reason.map(flexible))),
            just(Token::ErrorTerm)
                .ignore_then(
                    r.clone()
                        .then_ignore(just(Token::RArrow).not().rewind())
                        .or_not(),
                )
                .map_with_span(|reason, span| UntypedExpr::error(span, reason.map(flexible))),
        )))
        .map_with_span(
            |(((pattern, alternative_patterns_opt), guard), then), span| {
                let mut patterns = vec1![pattern];
                patterns.append(&mut alternative_patterns_opt.unwrap_or_default());
                ast::UntypedClause {
                    location: span,
                    patterns,
                    guard,
                    then,
                }
            },
        )
}
