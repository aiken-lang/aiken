use chumsky::prelude::*;

use crate::{
    ast,
    parser::{
        error::{self, ParseError},
        token::{Base, Token},
    },
};

pub fn parser<A>(
    into: impl Fn(Vec<u8>, ast::ByteArrayFormatPreference, ast::Span) -> A,
) -> impl Parser<Token, A, Error = ParseError> {
    choice((array_of_bytes(), hex_string(), utf8_string()))
        .map_with_span(move |(preferred_format, bytes), span| into(bytes, preferred_format, span))
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
