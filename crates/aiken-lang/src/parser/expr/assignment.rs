use chumsky::prelude::*;

use crate::{
    ast,
    expr::UntypedExpr,
    parser::{annotation, error::ParseError, pattern, token::Token},
};

pub fn let_(
    r: Recursive<'_, Token, UntypedExpr, ParseError>,
) -> impl Parser<Token, UntypedExpr, Error = ParseError> + '_ {
    just(Token::Let)
        .ignore_then(pattern())
        .then(just(Token::Colon).ignore_then(annotation()).or_not())
        .then_ignore(just(Token::Equal))
        .then(r.clone())
        .map_with_span(
            move |((pattern, annotation), value), span| UntypedExpr::Assignment {
                location: span,
                value: Box::new(value),
                pattern,
                kind: ast::AssignmentKind::Let,
                annotation,
            },
        )
}

pub fn expect(
    r: Recursive<'_, Token, UntypedExpr, ParseError>,
) -> impl Parser<Token, UntypedExpr, Error = ParseError> + '_ {
    just(Token::Expect)
        .ignore_then(
            pattern()
                .then(just(Token::Colon).ignore_then(annotation()).or_not())
                .then_ignore(just(Token::Equal))
                .or_not(),
        )
        .then(r.clone())
        .map_with_span(move |(opt_pattern, value), span| {
            let (pattern, annotation) = opt_pattern.unwrap_or_else(|| {
                (
                    ast::UntypedPattern::Constructor {
                        is_record: false,
                        location: span,
                        name: "True".to_string(),
                        arguments: vec![],
                        module: None,
                        constructor: (),
                        with_spread: false,
                        tipo: (),
                    },
                    None,
                )
            });

            UntypedExpr::Assignment {
                location: span,
                value: Box::new(value),
                pattern,
                kind: ast::AssignmentKind::Expect,
                annotation,
            }
        })
}

#[cfg(test)]
mod tests {
    use crate::assert_expr;

    #[test]
    fn let_bindings() {
        assert_expr!("let thing = [ 1, 2, a ]");
    }

    #[test]
    fn expect() {
        assert_expr!("expect Some(x) = something.field");
    }

    #[test]
    fn expect_bool_sugar() {
        assert_expr!("expect something.field == wow");
    }
}
