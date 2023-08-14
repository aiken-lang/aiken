use chumsky::prelude::*;

use crate::{
    ast::LogicalOpChainKind,
    expr::UntypedExpr,
    parser::{error::ParseError, token::Token},
};

pub fn parser(
    expression: Recursive<'_, Token, UntypedExpr, ParseError>,
) -> impl Parser<Token, UntypedExpr, Error = ParseError> + '_ {
    choice((
        just(Token::And).to(LogicalOpChainKind::And),
        just(Token::Or).to(LogicalOpChainKind::Or),
    ))
    .then(
        expression
            .separated_by(just(Token::Comma))
            .allow_trailing()
            .delimited_by(just(Token::LeftBrace), just(Token::RightBrace)),
    )
    .map_with_span(|(kind, exprs), span| UntypedExpr::LogicalOpChain {
        kind,
        expressions: exprs,
        location: span,
    })
}

#[cfg(test)]
mod tests {
    use crate::assert_expr;

    #[test]
    fn and_chain() {
        assert_expr!(
            r#"
            and {
              1 == 2,
              something,
            }
            "#
        );
    }

    #[test]
    fn or_chain() {
        assert_expr!(
            r#"
            or {
              1 == 2,
              something,
            }
            "#
        );
    }

    #[test]
    fn and_or_chain() {
        assert_expr!(
            r#"
            or {
              1 == 2,
              something,
              and {
                1 == 2,
                something,
              },
            }
            "#
        );
    }
}
