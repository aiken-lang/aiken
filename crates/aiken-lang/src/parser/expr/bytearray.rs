use chumsky::prelude::*;

use crate::parser::{
    error::ParseError, expr::UntypedExpr, literal::bytearray::parser as bytearray, token::Token,
};

pub fn parser() -> impl Parser<Token, UntypedExpr, Error = ParseError> {
    bytearray(|bytes, preferred_format, location| UntypedExpr::ByteArray {
        location,
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
