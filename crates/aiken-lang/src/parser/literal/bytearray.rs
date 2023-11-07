use chumsky::prelude::*;

use crate::{
    ast,
    parser::{
        error::{self, ParseError},
        token::{Base, Token},
    },
};

pub fn parser<A>(
    into: impl Fn(
        Vec<u8>,
        ast::ByteArrayFormatPreference,
        Option<ast::CurveType>,
        ast::Span,
        &mut dyn FnMut(ParseError),
    ) -> A,
) -> impl Parser<Token, A, Error = ParseError> {
    choice((
        array_of_bytes(),
        hex_string(),
        utf8_string().map(|(p, b)| (None, p, b)),
    ))
    .validate(move |(curve, preferred_format, bytes), span, emit| {
        into(bytes, preferred_format, curve, span, emit)
    })
}

fn curve_point() -> impl Parser<Token, ast::CurveType, Error = ParseError> {
    just(Token::Less)
        .ignore_then(select! {Token::UpName {name} => name})
        .then_ignore(just(Token::Comma))
        .then(select! {Token::UpName {name} => name})
        .then_ignore(just(Token::Greater))
        .validate(
            |(curve_type, point_type), span, emit| match curve_type.as_str() {
                "Bls12_381" => {
                    let point = match point_type.as_str() {
                        "G1" => ast::Bls12_381PointType::G1,
                        "G2" => ast::Bls12_381PointType::G2,
                        _ => {
                            emit(ParseError::unknown_point_curve(
                                curve_type,
                                Some(point_type),
                                span,
                            ));

                            ast::Bls12_381PointType::G1
                        }
                    };

                    ast::CurveType::Bls12_381(point)
                }
                _ => {
                    emit(ParseError::unknown_point_curve(curve_type, None, span));

                    ast::CurveType::Bls12_381(ast::Bls12_381PointType::G1)
                }
            },
        )
}

pub fn array_of_bytes() -> impl Parser<
    Token,
    (
        Option<ast::CurveType>,
        ast::ByteArrayFormatPreference,
        Vec<u8>,
    ),
    Error = ParseError,
> {
    just(Token::Hash)
        .ignore_then(curve_point().or_not())
        .then(
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
        .validate(|(curve, bytes), span, emit| {
            let base = bytes.iter().try_fold(None, |acc, (_, base)| match acc {
                None => Ok(Some(base)),
                Some(previous_base) if previous_base == base => Ok(Some(base)),
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

            (
                curve,
                bytes.into_iter().map(|(b, _)| b).collect::<Vec<u8>>(),
                base,
            )
        })
        .map(|(curve, bytes, base)| {
            (
                curve,
                ast::ByteArrayFormatPreference::ArrayOfBytes(base),
                bytes,
            )
        })
}

pub fn hex_string() -> impl Parser<
    Token,
    (
        Option<ast::CurveType>,
        ast::ByteArrayFormatPreference,
        Vec<u8>,
    ),
    Error = ParseError,
> {
    just(Token::Hash)
        .ignore_then(curve_point().or_not())
        .then(
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
        .map(|(curve, token)| {
            (
                curve,
                ast::ByteArrayFormatPreference::HexadecimalString,
                token,
            )
        })
}

pub fn utf8_string(
) -> impl Parser<Token, (ast::ByteArrayFormatPreference, Vec<u8>), Error = ParseError> {
    select! {Token::ByteString {value} => value.into_bytes() }
        .map(|token| (ast::ByteArrayFormatPreference::Utf8String, token))
}
