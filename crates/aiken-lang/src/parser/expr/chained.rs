use chumsky::prelude::*;

use super::anonymous_binop::parser as anonymous_binop;
use super::anonymous_function::parser as anonymous_function;
use super::assignment;
use super::block::parser as block;
use super::bytearray::parser as bytearray;
use super::if_else::parser as if_else;
use super::int::parser as int;
use super::list::parser as list;
use super::record::parser as record;
use super::record_update::parser as record_update;
use super::string::parser as string;
use super::tuple::parser as tuple;
use super::var::parser as var;
use super::when::parser as when;

use crate::{
    expr::UntypedExpr,
    parser::{
        chain::{call::parser as call, field_access, tuple_index::parser as tuple_index, Chain},
        error::ParseError,
        token::Token,
    },
};

pub fn parser<'a>(
    sequence: Recursive<'a, Token, UntypedExpr, ParseError>,
    expression: Recursive<'a, Token, UntypedExpr, ParseError>,
) -> impl Parser<Token, UntypedExpr, Error = ParseError> + 'a {
    let chain = choice((
        tuple_index(),
        field_access::parser(),
        call(expression.clone()),
    ));
    chain_start(sequence, expression)
        .then(chain.repeated())
        .foldl(|expr, chain| match chain {
            Chain::Call(args, span) => expr.call(args, span),
            Chain::FieldAccess(label, span) => expr.field_access(label, span),
            Chain::TupleIndex(index, span) => expr.tuple_index(index, span),
        })
        .then(just(Token::Question).or_not())
        .map_with_span(|(value, token), location| match token {
            Some(_) => UntypedExpr::TraceIfFalse {
                value: Box::new(value),
                location,
            },
            None => value,
        })
}

pub fn chain_start<'a>(
    sequence: Recursive<'a, Token, UntypedExpr, ParseError>,
    expression: Recursive<'a, Token, UntypedExpr, ParseError>,
) -> impl Parser<Token, UntypedExpr, Error = ParseError> + 'a {
    choice((
        string(),
        int(),
        record_update(expression.clone()),
        record(expression.clone()),
        field_access::constructor(),
        var(),
        tuple(expression.clone()),
        bytearray(),
        list(expression.clone()),
        anonymous_function(sequence.clone()),
        anonymous_binop(),
        block(sequence.clone()),
        when(expression.clone()),
        assignment::let_(expression.clone()),
        assignment::expect(expression.clone()),
        if_else(sequence, expression.clone()),
    ))
}
