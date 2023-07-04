use chumsky::prelude::*;

use crate::{
    expr::UntypedExpr,
    parser::{error::ParseError, token::Token},
};

pub fn parser(
    seq_r: Recursive<'_, Token, UntypedExpr, ParseError>,
) -> impl Parser<Token, UntypedExpr, Error = ParseError> + '_ {
    choice((
        seq_r
            .clone()
            .delimited_by(just(Token::LeftBrace), just(Token::RightBrace)),
        seq_r.clone().delimited_by(
            choice((just(Token::LeftParen), just(Token::NewLineLeftParen))),
            just(Token::RightParen),
        ),
    ))
}

#[cfg(test)]
mod tests {
    use crate::assert_expr;

    #[test]
    fn block_basic() {
        assert_expr!(
            r#"
            let b = {
              let x = 4

              x + 5
            }
            "#
        );
    }
}
