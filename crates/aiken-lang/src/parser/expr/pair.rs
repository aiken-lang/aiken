use crate::{
    ast::well_known,
    builtins::PRELUDE,
    expr::UntypedExpr,
    parser::{error::ParseError, token::Token},
};
use chumsky::prelude::*;

pub fn parser(
    r: Recursive<'_, Token, UntypedExpr, ParseError>,
) -> impl Parser<Token, UntypedExpr, Error = ParseError> + '_ {
    select! {Token::Name { name } if name == PRELUDE => name}
        .then_ignore(just(Token::Dot))
        .or_not()
        .then_ignore(select! {Token::UpName { name } if name == well_known::PAIR => name})
        .ignore_then(
            r.clone()
                .separated_by(just(Token::Comma))
                .exactly(2)
                .allow_trailing()
                .delimited_by(
                    choice((just(Token::LeftParen), just(Token::NewLineLeftParen))),
                    just(Token::RightParen),
                )
                .map_with_span(|elems, location| UntypedExpr::Pair {
                    location,
                    fst: elems
                        .first()
                        .expect("Pair should have exactly 2 elements")
                        .to_owned()
                        .into(),
                    snd: elems
                        .last()
                        .expect("Pair should have exactly 2 elements")
                        .to_owned()
                        .into(),
                }),
        )
}

#[cfg(test)]
mod tests {
    use crate::assert_expr;

    #[test]
    fn basic_pair() {
        assert_expr!(r#"Pair(1, 2)"#);
    }

    #[test]
    fn pair_from_prelude() {
        assert_expr!(r#"aiken.Pair(1, 2)"#);
    }
}
