use chumsky::prelude::*;

use crate::{
    ast::TraceKind,
    expr::UntypedExpr,
    parser::{
        error::ParseError,
        expr::{string, when::clause},
        token::Token,
    },
};

pub fn parser<'a>(
    expression: Recursive<'a, Token, UntypedExpr, ParseError>,
    sequence: Recursive<'a, Token, UntypedExpr, ParseError>,
) -> impl Parser<Token, UntypedExpr, Error = ParseError> + 'a {
    choice((
        just(Token::Todo).ignore_then(choice((
            clause(expression.clone())
                .ignored()
                .rewind()
                .map_with_span(|_, span| UntypedExpr::todo(None, span)),
            choice((string::hybrid(), expression.clone()))
                .or_not()
                .map_with_span(UntypedExpr::todo),
        ))),
        just(Token::Fail).ignore_then(choice((
            clause(expression.clone())
                .ignored()
                .rewind()
                .map_with_span(|_, span| UntypedExpr::fail(None, span)),
            choice((string::hybrid(), expression.clone()))
                .or_not()
                .map_with_span(UntypedExpr::fail),
        ))),
        just(Token::Trace)
            .ignore_then(clause(expression.clone()).or_not().ignored().rewind())
            .ignore_then(choice((string::hybrid(), expression.clone())))
            .then(sequence.clone())
            .map_with_span(|(text, then_), span| UntypedExpr::Trace {
                kind: TraceKind::Trace,
                location: span,
                then: Box::new(then_),
                text: Box::new(text),
            }),
    ))
}

#[cfg(test)]
mod tests {
    use crate::assert_expr;

    #[test]
    fn error_basic() {
        assert_expr!(
            r#"
            fail @"foo"
            "#
        );
    }

    #[test]
    fn error_sugar() {
        assert_expr!(
            r#"
            fail "foo"
            "#
        );
    }

    #[test]
    fn todo_basic() {
        assert_expr!(
            r#"
            todo @"foo"
            "#
        );
    }

    #[test]
    fn todo_sugar() {
        assert_expr!(
            r#"
            todo "foo"
            "#
        );
    }

    #[test]
    fn fail_expr() {
        assert_expr!(
            r#"
            fail str.join([@"Some string ", some_params, @" some string"], @"")
            "#
        );
    }

    #[test]
    fn fail_empty() {
        assert_expr!(
            r#"
            fail
            "#
        );
    }

    #[test]
    fn trace_expr() {
        assert_expr!(
            r#"
            trace some_var 
            "#
        );
    }
}
