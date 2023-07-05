use chumsky::prelude::*;

use crate::{
    ast,
    expr::UntypedExpr,
    parser::{
        error::{self, ParseError},
        token::{Base, Token},
    },
};

pub fn parser() -> impl Parser<Token, UntypedExpr, Error = ParseError> {
    bytearray().map_with_span(|(preferred_format, bytes), span| UntypedExpr::ByteArray {
        location: span,
        bytes,
        preferred_format,
    })
}

pub fn bytearray(
) -> impl Parser<Token, (ast::ByteArrayFormatPreference, Vec<u8>), Error = ParseError> {
    choice((array_of_bytes(), hex_string(), utf8_string()))
}

pub fn array_of_bytes(
) -> impl Parser<Token, (ast::ByteArrayFormatPreference, Vec<u8>), Error = ParseError> {
    just(Token::Hash)
        .ignore_then(
            select! {Token::Int {value, base, ..} => (value, base)}
                .validate(|(value, base), span, emit| {
                    let byte: u8 = match value.parse() {
                        Ok(b) => b,
                        Err(_) => {
                            emit(ParseError::expected_input_found(
                                span,
                                None,
                                Some(error::Pattern::Byte),
                            ));
                            0
                        }
                    };
                    (byte, base)
                })
                .separated_by(just(Token::Comma))
                .allow_trailing()
                .delimited_by(just(Token::LeftSquare), just(Token::RightSquare)),
        )
        .validate(|bytes, span, emit| {
            let base = bytes.iter().fold(Ok(None), |acc, (_, base)| match acc {
                Ok(None) => Ok(Some(base)),
                Ok(Some(previous_base)) if previous_base == base => Ok(Some(base)),
                _ => Err(()),
            });

            let base = match base {
                Err(()) => {
                    emit(ParseError::hybrid_notation_in_bytearray(span));
                    Base::Decimal {
                        numeric_underscore: false,
                    }
                }
                Ok(None) => Base::Decimal {
                    numeric_underscore: false,
                },
                Ok(Some(base)) => *base,
            };

            (bytes.into_iter().map(|(b, _)| b).collect::<Vec<u8>>(), base)
        })
        .map(|(bytes, base)| (ast::ByteArrayFormatPreference::ArrayOfBytes(base), bytes))
}

pub fn hex_string(
) -> impl Parser<Token, (ast::ByteArrayFormatPreference, Vec<u8>), Error = ParseError> {
    just(Token::Hash)
        .ignore_then(
            select! {Token::ByteString {value} => value}.validate(|value, span, emit| {
                match hex::decode(value) {
                    Ok(bytes) => bytes,
                    Err(_) => {
                        emit(ParseError::malformed_base16_string_literal(span));
                        vec![]
                    }
                }
            }),
        )
        .map(|token| (ast::ByteArrayFormatPreference::HexadecimalString, token))
}

pub fn utf8_string(
) -> impl Parser<Token, (ast::ByteArrayFormatPreference, Vec<u8>), Error = ParseError> {
    select! {Token::ByteString {value} => value.into_bytes() }
        .map(|token| (ast::ByteArrayFormatPreference::Utf8String, token))
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
