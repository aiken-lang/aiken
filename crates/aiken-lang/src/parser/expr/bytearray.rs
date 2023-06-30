use chumsky::prelude::*;

use crate::{
    expr::UntypedExpr,
    parser::{error::ParseError, token::Token, utils},
};

pub fn parser() -> impl Parser<Token, UntypedExpr, Error = ParseError> {
    utils::bytearray().map_with_span(|(preferred_format, bytes), span| UntypedExpr::ByteArray {
        location: span,
        bytes,
        preferred_format,
    })
}
