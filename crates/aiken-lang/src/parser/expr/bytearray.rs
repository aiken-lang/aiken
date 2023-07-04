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

#[cfg(test)]
mod tests {
    use crate::assert_expr;

    #[test]
    fn bytearray_basic() {
        assert_expr!("#[0, 170, 255]");
    }

    #[test]
    fn bytearray_base16() {
        assert_expr!("#\"00aaff\"");
    }

    #[test]
    fn bytearray_utf8_encoded() {
        assert_expr!("\"aiken\"");
    }
}
