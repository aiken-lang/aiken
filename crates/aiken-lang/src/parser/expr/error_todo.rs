use chumsky::prelude::*;

use crate::{
    expr::UntypedExpr,
    parser::{
        error::ParseError,
        expr::{block::parser as block, string},
        token::Token,
    },
};

pub fn parser(
    sequence: Recursive<'_, Token, UntypedExpr, ParseError>,
) -> impl Parser<Token, UntypedExpr, Error = ParseError> + '_ {
    let message = || choice((string::hybrid(), block(sequence.clone())));
    choice((
        just(Token::Todo)
            .ignore_then(message().or_not())
            .map_with_span(UntypedExpr::todo),
        just(Token::ErrorTerm)
            .ignore_then(message().or_not())
            .map_with_span(UntypedExpr::error),
    ))
}

#[cfg(test)]
mod tests {
    use crate::assert_expr;

    #[test]
    fn error_basic() {
        assert_expr!(
            r#"
            error @"foo"
            "#
        );
    }

    #[test]
    fn error_sugar() {
        assert_expr!(
            r#"
            error "foo"
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
}
