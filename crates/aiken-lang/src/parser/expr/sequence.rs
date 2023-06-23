use chumsky::prelude::*;

use crate::{
    ast::TraceKind,
    expr::UntypedExpr,
    parser::{error::ParseError, token::Token},
};

pub fn parser() -> impl Parser<Token, UntypedExpr, Error = ParseError> {
    recursive(|r| {
        choice((
            just(Token::Trace)
                .ignore_then(super::parser(r.clone()))
                .then(r.clone())
                .map_with_span(|(text, then_), span| UntypedExpr::Trace {
                    kind: TraceKind::Trace,
                    location: span,
                    then: Box::new(then_),
                    text: Box::new(super::string::flexible(text)),
                }),
            just(Token::ErrorTerm)
                .ignore_then(super::parser(r.clone()).or_not())
                .map_with_span(|reason, span| {
                    UntypedExpr::error(span, reason.map(super::string::flexible))
                }),
            just(Token::Todo)
                .ignore_then(super::parser(r.clone()).or_not())
                .map_with_span(|reason, span| {
                    UntypedExpr::todo(span, reason.map(super::string::flexible))
                }),
            super::parser(r.clone())
                .then(r.repeated())
                .foldl(|current, next| current.append_in_sequence(next)),
        ))
    })
}
