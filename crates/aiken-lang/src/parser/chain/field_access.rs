use super::Chain;
use crate::{
    ast::well_known,
    parser::{ParseError, token::Token},
};
use chumsky::prelude::*;

pub(crate) fn parser() -> impl Parser<Token, Chain, Error = ParseError> {
    just(Token::Dot)
        .ignore_then(choice((
            select! { Token::Else => well_known::VALIDATOR_ELSE.to_string() },
            select! { Token::Name { name } => name, },
            select! { Token::UpName { name } => name, },
        )))
        .map_with_span(Chain::FieldAccess)
}

#[cfg(test)]
mod tests {
    use crate::assert_expr;

    #[test]
    fn field_access_else_1() {
        assert_expr!("foo.else");
    }

    #[test]
    fn field_access_else_2() {
        assert_expr!("foo.bar.else");
    }
}
