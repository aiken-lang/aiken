use crate::ast::{self, Span};
use crate::expr::UntypedExpr;

pub(crate) mod call;
pub(crate) mod field_access;
pub(crate) mod tuple_index;

pub(crate) enum Chain {
    Call(Vec<ParserArg>, Span),
    FieldAccess(String, Span),
    TupleIndex(usize, Span),
}

// Parsing a function call into the appropriate structure
#[derive(Debug)]
pub(crate) enum ParserArg {
    Arg(Box<ast::CallArg<UntypedExpr>>),
    Hole {
        location: Span,
        label: Option<String>,
    },
}
