use crate::{
    ast::UntypedPattern,
    parser::{error::ParseError, literal, token::Token},
};
use chumsky::prelude::*;

pub fn parser() -> impl Parser<Token, UntypedPattern, Error = ParseError> {
    literal::bytearray(|value, preferred_format, curve, location, emit| {
        if curve.is_some() {
            emit(ParseError::match_on_curve(location));
        }

        UntypedPattern::ByteArray {
            location,
            value,
            preferred_format,
        }
    })
}

#[cfg(test)]
mod tests {
    use crate::assert_expr;

    #[test]
    fn pattern_bytearray() {
        assert_expr!(
            r#"
            when foo is {
                #"00abcd" -> True
                "Aiken, rocks!" -> True
                #[1, 2, 3, 4] -> True
                #[0x00, 0xab, 0xcd] -> True
                _ -> False
            }
        "#
        );
    }

    #[test]
    fn pattern_bytearray_g1_element() {
        assert_expr!(
            r#"
            when foo is {
                #<Bls12_381, G1>"950dfd33da2682260c76038dfb8bad6e84ae9d599a3c151815945ac1e6ef6b1027cd917f3907479d20d636ce437a41f5" -> False
                _ -> True
            }
        "#
        );
    }

    #[test]
    fn pattern_bytearray_g2_element() {
        assert_expr!(
            r#"
            when foo is {
                #<Bls12_381, G2>"b0629fa1158c2d23a10413fe91d381a84d25e31d041cd0377d25828498fd02011b35893938ced97535395e4815201e67108bcd4665e0db25d602d76fa791fab706c54abf5e1a9e44b4ac1e6badf3d2ac0328f5e30be341677c8bac5dda7682f1" -> False
                _ -> True
            }
        "#
        );
    }
}
