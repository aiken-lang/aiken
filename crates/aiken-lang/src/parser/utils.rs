use chumsky::prelude::*;

use crate::ast;

use super::{
    error::{self, ParseError},
    token::{Base, Token},
};

pub fn public() -> impl Parser<Token, (), Error = ParseError> {
    just(Token::Pub).ignored()
}

pub fn bytearray(
) -> impl Parser<Token, (ast::ByteArrayFormatPreference, Vec<u8>), Error = ParseError> {
    let bytearray_list_parser = just(Token::Hash)
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
        .map(|(bytes, base)| (ast::ByteArrayFormatPreference::ArrayOfBytes(base), bytes));

    let bytearray_hexstring_parser =
        just(Token::Hash)
            .ignore_then(select! {Token::ByteString {value} => value}.validate(
                |value, span, emit| match hex::decode(value) {
                    Ok(bytes) => bytes,
                    Err(_) => {
                        emit(ParseError::malformed_base16_string_literal(span));
                        vec![]
                    }
                },
            ))
            .map(|token| (ast::ByteArrayFormatPreference::HexadecimalString, token));

    let bytearray_utf8_parser = select! {Token::ByteString {value} => value.into_bytes() }
        .map(|token| (ast::ByteArrayFormatPreference::Utf8String, token));

    choice((
        bytearray_list_parser,
        bytearray_hexstring_parser,
        bytearray_utf8_parser,
    ))
}

pub fn type_name_with_args() -> impl Parser<Token, (String, Option<Vec<String>>), Error = ParseError>
{
    just(Token::Type).ignore_then(
        select! {Token::UpName { name } => name}.then(
            select! {Token::Name { name } => name}
                .separated_by(just(Token::Comma))
                .allow_trailing()
                .delimited_by(just(Token::Less), just(Token::Greater))
                .or_not(),
        ),
    )
}

#[cfg(test)]
#[macro_use]
mod macros {
    #[macro_export]
    macro_rules! assert_expr {
        ($code:expr) => {
            let $crate::parser::lexer::LexInfo { tokens, .. } = $crate::parser::lexer::run($code).unwrap();

            let stream = chumsky::Stream::from_iter($crate::ast::Span::create(tokens.len()), tokens.into_iter());

            let result = $crate::parser::expr::sequence().parse(stream).unwrap();

            insta::with_settings!({
                description => concat!("Code:\n\n", $code),
                prepend_module_to_snapshot => false,
                omit_expression => true
            }, {
                insta::assert_debug_snapshot!(result);
            });
        };
    }
}
