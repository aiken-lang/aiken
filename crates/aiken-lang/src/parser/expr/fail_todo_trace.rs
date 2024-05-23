use crate::{
    ast::TraceKind,
    expr::UntypedExpr,
    parser::{
        error::ParseError,
        expr::{string, when::clause},
        token::Token,
    },
};
use chumsky::prelude::*;

pub fn parser<'a>(
    expression: Recursive<'a, Token, UntypedExpr, ParseError>,
    sequence: Recursive<'a, Token, UntypedExpr, ParseError>,
) -> impl Parser<Token, UntypedExpr, Error = ParseError> + 'a {
    let message = choice((
        clause(expression.clone()).ignored().rewind().to(None),
        choice((string::hybrid(), expression.clone())).or_not(),
    ))
    .boxed();

    choice((
        just(Token::Todo)
            .ignore_then(message.clone())
            .map_with_span(UntypedExpr::todo),
        just(Token::Fail)
            .ignore_then(message)
            .map_with_span(UntypedExpr::fail),
        just(Token::Trace)
            .ignore_then(choice((string::hybrid(), expression.clone())))
            .then(sequence.clone().or_not())
            .map_with_span(|(text, then_), span| UntypedExpr::Trace {
                kind: TraceKind::Trace,
                location: span,
                then: Box::new(then_.unwrap_or_else(|| UntypedExpr::todo(None, span))),
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
    fn todo_empty() {
        assert_expr!(
            r#"
            todo
            "#
        );
    }

    #[test]
    fn todo_expr() {
        assert_expr!(
            r#"
            todo string.join(["foo", "bar"])
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
            trace string.join(["foo", "bar"])
            a
            "#
        );
    }

    #[test]
    fn trace_expr_todo() {
        assert_expr!(
            r#"
            trace some_var
            "#
        );
    }
}
