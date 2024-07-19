use crate::{
    ast::TraceKind,
    expr::UntypedExpr,
    parser::{
        error::{ParseError, Pattern},
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
            .then(
                choice((just(Token::Colon), just(Token::Comma)))
                    .then(
                        choice((string::hybrid(), expression.clone()))
                            .separated_by(just(Token::Comma)),
                    )
                    .validate(|(token, arguments), span, emit| {
                        if token != Token::Colon {
                            emit(ParseError::expected_but_got(
                                Pattern::Token(Token::Colon),
                                Pattern::Token(token),
                                span.map(|start, _end| (start, start + 1)),
                            ))
                        }

                        arguments
                    })
                    .or_not()
                    .map(|opt| opt.unwrap_or_default()),
            )
            .then(sequence.clone().or_not())
            .map_with_span(
                |((label, arguments), continuation), span| UntypedExpr::Trace {
                    kind: TraceKind::Trace,
                    location: span,
                    then: Box::new(continuation.unwrap_or_else(|| UntypedExpr::todo(None, span))),
                    label: Box::new(label),
                    arguments,
                },
            ),
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
    fn trace_string() {
        assert_expr!(
            r#"
            trace @"foo"
            a
            "#
        );
    }

    #[test]
    fn trace_bytearray() {
        assert_expr!(
            r#"
            trace "foo"
            a
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

    #[test]
    fn trace_labelled() {
        assert_expr!(
            r#"
            trace foo: "bar"
            "#
        );
    }

    #[test]
    fn trace_variadic() {
        assert_expr!(
            r#"
            trace "foo": @"bar", baz
            "#
        );
    }
}
