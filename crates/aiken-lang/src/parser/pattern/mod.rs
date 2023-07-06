use chumsky::prelude::*;

mod constructor;
mod discard;
mod int;
mod list;
mod tuple;
mod var;

pub use constructor::parser as constructor;
pub use discard::parser as discard;
pub use int::parser as int;
pub use list::parser as list;
pub use tuple::parser as tuple;
pub use var::parser as var;

use crate::{
    ast::UntypedPattern,
    parser::{error::ParseError, token::Token},
};

pub fn parser() -> impl Parser<Token, UntypedPattern, Error = ParseError> {
    recursive(|expression| {
        choice((
            var(expression.clone()),
            constructor(expression.clone()),
            discard(),
            int(),
            tuple(expression.clone()),
            list(expression),
        ))
        .then(
            just(Token::As)
                .ignore_then(select! { Token::Name {name} => name})
                .or_not(),
        )
        .map_with_span(|(pattern, opt_as), span| {
            if let Some(name) = opt_as {
                UntypedPattern::Assign {
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
