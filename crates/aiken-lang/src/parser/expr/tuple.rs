use chumsky::prelude::*;

use crate::{
    expr::UntypedExpr,
    parser::{error::ParseError, token::Token},
};

pub fn parser(
    r: Recursive<'_, Token, UntypedExpr, ParseError>,
) -> impl Parser<Token, UntypedExpr, Error = ParseError> + '_ {
    r.clone()
        .separated_by(just(Token::Comma))
        .at_least(2)
        .allow_trailing()
        .delimited_by(
            choice((just(Token::LeftParen), just(Token::NewLineLeftParen))),
            just(Token::RightParen),
        )
        .map_with_span(|elems, span| UntypedExpr::Tuple {
            location: span,
            elems,
        })
}

#[cfg(test)]
mod tests {
    use crate::assert_expr;

    #[test]
    fn parse_tuple() {
        assert_expr!(
            r#"
            let tuple = (1, 2, 3, 4)
            tuple.1st + tuple.2nd + tuple.3rd + tuple.4th
            "#
        );
    }

    #[test]
    fn parse_tuple2() {
        assert_expr!(
            r#"
            let a = foo(14)
            (a, 42)
            "#
        );
    }
}
