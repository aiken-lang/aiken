use chumsky::prelude::*;

use crate::{
    ast::TraceKind,
    expr::UntypedExpr,
    parser::{
        error::ParseError,
        expr::{block::parser as block, string},
        token::Token,
    },
};

pub fn parser() -> impl Parser<Token, UntypedExpr, Error = ParseError> {
    recursive(|sequence| {
        choice((
            // just(Token::Trace)
            //     .ignore_then(choice((
            //         string::hybrid(),
            //         block(sequence.clone()),
            //         sequence.clone(),
            //     )))
            //     .then(sequence.clone())
            //     .map_with_span(|(text, then_), span| UntypedExpr::Trace {
            //         kind: TraceKind::Trace,
            //         location: span,
            //         then: Box::new(then_),
            //         text: Box::new(text),
            //     }),
            super::parser(sequence.clone())
                .then(sequence.repeated())
                .foldl(|current, next| current.append_in_sequence(next)),
        ))
    })
}
