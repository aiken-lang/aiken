use crate::{
    expr::UntypedExpr,
    parser::{error::ParseError, token::Token},
};
use chumsky::prelude::*;

pub fn parser() -> impl Parser<Token, UntypedExpr, Error = ParseError> {
    recursive(|sequence| {
        super::parser(sequence.clone())
            .then(sequence.repeated())
            .foldl(|current, next| current.append_in_sequence(next))
    })
}
