use chumsky::prelude::*;
use uplc::machine::runtime::Compressable;

use crate::{
    ast,
    parser::{annotation, error::ParseError, literal, token::Token, utils},
};

pub fn parser() -> impl Parser<Token, ast::UntypedDefinition, Error = ParseError> {
    utils::optional_flag(Token::Pub)
        .then_ignore(just(Token::Const))
        .then(select! {Token::Name{name} => name})
        .then(
            just(Token::Colon)
                .ignore_then(annotation::parser())
                .or_not(),
        )
        .then_ignore(just(Token::Equal))
        .then(value())
        .map_with_span(|(((public, name), annotation), value), span| {
            ast::UntypedDefinition::ModuleConstant(ast::ModuleConstant {
                doc: None,
                location: span,
                public,
                name,
                annotation,
                value: Box::new(value),
                tipo: (),
            })
        })
}

pub fn value() -> impl Parser<Token, ast::Constant, Error = ParseError> {
    let constant_string_parser =
        select! {Token::String {value} => value}.map_with_span(|value, span| {
            ast::Constant::String {
                location: span,
                value,
            }
        });

    let constant_int_parser =
        literal::int().map_with_span(|(value, base), location| ast::Constant::Int {
            location,
            value,
            base,
        });

    let constant_bytearray_parser = literal::bytearray(
        |bytes, preferred_format, curve, location, emit| match curve {
            Some(curve @ ast::CurveType::Bls12_381(point)) => {
                let point = match point {
                    ast::Bls12_381PointType::G1 => {
                        blst::blst_p1::uncompress(&bytes).map(ast::Bls12_381Point::G1)
                    }
                    ast::Bls12_381PointType::G2 => {
                        blst::blst_p2::uncompress(&bytes).map(ast::Bls12_381Point::G2)
                    }
                };

                let point = point.unwrap_or_else(|_err| {
                    emit(ParseError::point_not_on_curve(curve, location));

                    ast::Bls12_381Point::default()
                });

                ast::Constant::CurvePoint {
                    location,
                    point: ast::Curve::Bls12_381(point).into(),
                    preferred_format,
                }
            }
            None => ast::Constant::ByteArray {
                location,
                bytes,
                preferred_format,
            },
        },
    );

    choice((
        constant_string_parser,
        constant_int_parser,
        constant_bytearray_parser,
    ))
}
