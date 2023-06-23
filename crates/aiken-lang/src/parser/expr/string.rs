use crate::{ast, expr::UntypedExpr};

/// Interpret bytearray string literals written as utf-8 strings, as strings.
///
/// This is mostly convenient so that todo & error works with either @"..." or plain "...".
/// In this particular context, there's actually no ambiguity about the right-hand-side, so
/// we can provide this syntactic sugar.
pub fn flexible(expr: UntypedExpr) -> UntypedExpr {
    match expr {
        UntypedExpr::ByteArray {
            preferred_format: ast::ByteArrayFormatPreference::Utf8String,
            bytes,
            location,
        } => UntypedExpr::String {
            location,
            value: String::from_utf8(bytes).unwrap(),
        },
        _ => expr,
    }
}
