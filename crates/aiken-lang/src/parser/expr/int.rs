use chumsky::prelude::*;

use crate::{
    expr::UntypedExpr,
    parser::{error::ParseError, literal::uint, token::Token},
};

pub fn parser() -> impl Parser<Token, UntypedExpr, Error = ParseError> {
    uint().map_with_span(|(value, base), span| UntypedExpr::UInt {
        location: span,
        value,
        base,
    })
}

#[cfg(test)]
mod tests {
    use crate::assert_expr;

    #[test]
    fn int_literal() {
        assert_expr!("1");
    }

    #[test]
    fn int_negative() {
        assert_expr!("-1");
    }

    #[test]
    fn int_numeric_underscore() {
        assert_expr!(
            r#"
            {
              let i = 1_234_567
              let j = 1_000_000
              let k = -10_000
            }
            "#
        );
    }

    #[test]
    fn int_hex_bytes() {
        assert_expr!(r#"0x01"#);
    }
}
