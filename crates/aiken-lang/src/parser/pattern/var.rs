use chumsky::prelude::*;

use super::constructor;
use crate::{
    ast::UntypedPattern,
    parser::{error::ParseError, token::Token},
};

pub fn parser(
    expression: Recursive<'_, Token, UntypedPattern, ParseError>,
) -> impl Parser<Token, UntypedPattern, Error = ParseError> + '_ {
    select! { Token::Name {name} => name }
        .then(
            just(Token::Dot)
                .ignore_then(
                    select! {Token::UpName { name } => name}.then(constructor::args(expression)),
                )
                .or_not(),
        )
        .map_with_span(|(name, opt_pattern), span| {
            if let Some((c_name, (arguments, spread_location, is_record))) = opt_pattern {
                UntypedPattern::Constructor {
                    is_record,
                    location: span,
                    name: c_name,
                    arguments,
                    module: Some(name),
                    constructor: (),
                    spread_location,
                    tipo: (),
                }
            } else {
                UntypedPattern::Var {
                    location: span,
                    name,
                }
            }
        })
}
