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
    ast::{self, Span},
    expr::{FnStyle, UntypedExpr},
    parser::{
        chain::{
            call::parser as call, field_access, tuple_index::parser as tuple_index, Chain,
            ParserArg,
        },
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
            Chain::Call(args, span) => {
                let mut holes = Vec::new();

                let args = args
                    .into_iter()
                    .enumerate()
                    .map(|(index, a)| match a {
                        ParserArg::Arg(arg) => *arg,
                        ParserArg::Hole { location, label } => {
                            let name = format!("{}__{index}", ast::CAPTURE_VARIABLE);

                            holes.push(ast::Arg {
                                location: Span::empty(),
                                annotation: None,
                                arg_name: ast::ArgName::Named {
                                    label: name.clone(),
                                    name,
                                    location: Span::empty(),
                                    is_validator_param: false,
                                },
                                tipo: (),
                            });

                            ast::CallArg {
                                label,
                                location,
                                value: UntypedExpr::Var {
                                    location,
                                    name: format!("{}__{index}", ast::CAPTURE_VARIABLE),
                                },
                            }
                        }
                    })
                    .collect();

                let call = UntypedExpr::Call {
                    location: expr.location().union(span),
                    fun: Box::new(expr),
                    arguments: args,
                };

                if holes.is_empty() {
                    call
                } else {
                    UntypedExpr::Fn {
                        location: call.location(),
                        fn_style: FnStyle::Capture,
                        arguments: holes,
                        body: Box::new(call),
                        return_annotation: None,
                    }
                }
            }

            Chain::FieldAccess(label, span) => UntypedExpr::FieldAccess {
                location: expr.location().union(span),
                label,
                container: Box::new(expr),
            },

            Chain::TupleIndex(index, span) => UntypedExpr::TupleIndex {
                location: expr.location().union(span),
                index,
                tuple: Box::new(expr),
            },
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
