use chumsky::prelude::*;

use crate::{
    expr::UntypedExpr,
    parser::{error::ParseError, token::Token},
};

pub fn parser(
    expression: Recursive<'_, Token, UntypedExpr, ParseError>,
) -> impl Parser<Token, UntypedExpr, Error = ParseError> + '_ {
    just(Token::LeftSquare)
        .ignore_then(expression.clone().separated_by(just(Token::Comma)))
        .then(choice((
            just(Token::Comma).ignore_then(
                just(Token::DotDot)
                    .ignore_then(expression)
                    .map(Box::new)
                    .or_not(),
            ),
            just(Token::Comma).ignored().or_not().map(|_| None),
        )))
        .then_ignore(just(Token::RightSquare))
        // TODO: check if tail.is_some and elements.is_empty then return ListSpreadWithoutElements error
        .map_with_span(|(elements, tail), span| UntypedExpr::List {
            location: span,
            elements,
            tail,
        })
}

pub fn array(
    expression: Recursive<'_, Token, UntypedExpr, ParseError>,
) -> impl Parser<Token, UntypedExpr, Error = ParseError> + '_ {
    select! { Token::Name { name } if name == "array" => () }
        .ignore_then(just(Token::LeftSquare))
        .ignore_then(
            expression
                .separated_by(just(Token::Comma))
                .then_ignore(just(Token::Comma).or_not()),
        )
        .then_ignore(just(Token::RightSquare))
        .map_with_span(|elements, span| UntypedExpr::Array {
            location: span,
            elements,
        })
}

#[cfg(test)]
mod tests {
    use crate::assert_expr;

    #[test]
    fn empty_list() {
        assert_expr!("[]");
    }

    #[test]
    fn int_list() {
        assert_expr!("[1, 2, 3]");
    }

    #[test]
    fn list_spread() {
        assert_expr!("[1, 2, ..[]]");
    }

    #[test]
    fn empty_array() {
        assert_expr!("array[]");
    }

    #[test]
    fn int_array() {
        assert_expr!("array[1, 2, 3]");
    }
}
