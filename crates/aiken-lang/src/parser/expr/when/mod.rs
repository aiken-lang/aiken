use chumsky::prelude::*;

mod clause;
mod guard;

use crate::{
    expr::UntypedExpr,
    parser::{error::ParseError, token::Token},
};
pub use clause::parser as clause;
pub use guard::parser as guard;

pub fn parser(
    expression: Recursive<'_, Token, UntypedExpr, ParseError>,
) -> impl Parser<Token, UntypedExpr, Error = ParseError> + '_ {
    just(Token::When)
        // TODO: If subject is empty we should return ParseErrorType::ExpectedExpr,
        .ignore_then(expression.clone().map(Box::new))
        .then_ignore(just(Token::Is))
        .then_ignore(just(Token::LeftBrace))
        // TODO: If clauses are empty we should return ParseErrorType::NoCaseClause
        .then(clause(expression).repeated())
        .then_ignore(just(Token::RightBrace))
        .map_with_span(|(subject, clauses), span| UntypedExpr::When {
            location: span,
            subject,
            clauses,
        })
}

#[cfg(test)]
mod tests {
    use crate::assert_expr;

    #[test]
    fn when_basic() {
        assert_expr!(
            r#"
            when a is {
              1 | 4 | 5 -> {
                let amazing = 5
                amazing
              }
              3 -> 9
              _ -> 4
            }
            "#
        );
    }

    #[test]
    fn when_guard_deprecation() {
        assert_expr!(
            r#"
            when a is {
              2 if x > 1 -> 3
              _ -> 1
            }
            "#
        );
    }
}
