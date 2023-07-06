use crate::ast::{ParsedCallArg, Span};

pub(crate) mod call;
pub(crate) mod field_access;
pub(crate) mod tuple_index;

pub(crate) enum Chain {
    Call(Vec<ParsedCallArg>, Span),
    FieldAccess(String, Span),
    TupleIndex(usize, Span),
}
