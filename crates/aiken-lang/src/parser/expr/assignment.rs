use chumsky::prelude::*;

use crate::{
    ast,
    expr::UntypedExpr,
    parser::{annotation, error::ParseError, pattern, token::Token},
};

pub fn let_(
    r: Recursive<'_, Token, UntypedExpr, ParseError>,
) -> impl Parser<Token, UntypedExpr, Error = ParseError> + '_ {
    assignment(r, ast::AssignmentKind::Let)
}

pub fn expect(
    r: Recursive<'_, Token, UntypedExpr, ParseError>,
) -> impl Parser<Token, UntypedExpr, Error = ParseError> + '_ {
    assignment(r, ast::AssignmentKind::Expect)
}

fn assignment(
    r: Recursive<'_, Token, UntypedExpr, ParseError>,
    kind: ast::AssignmentKind,
) -> impl Parser<Token, UntypedExpr, Error = ParseError> + '_ {
    let keyword = match kind {
        ast::AssignmentKind::Let => Token::Let,
        ast::AssignmentKind::Expect => Token::Expect,
    };

    just(keyword)
        .ignore_then(pattern())
        .then(just(Token::Colon).ignore_then(annotation()).or_not())
        .then_ignore(just(Token::Equal))
        .then(r.clone())
        .map_with_span(
            move |((pattern, annotation), value), span| UntypedExpr::Assignment {
                location: span,
                value: Box::new(value),
                pattern,
                kind,
                annotation,
            },
        )
}

#[cfg(test)]
mod tests {
    use crate::assert_expr;

    #[test]
    fn let_bindings() {
        assert_expr!("let thing = [ 1, 2, a ]");
    }

    #[test]
    fn expect() {
        assert_expr!("expect Some(x) = something.field");
    }
}
