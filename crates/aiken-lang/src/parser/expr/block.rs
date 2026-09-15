use chumsky::prelude::*;

use crate::{
    ast::Span,
    expr::UntypedExpr,
    parser::{error::ParseError, token::Token},
};

pub fn parser(
    sequence: Recursive<'_, Token, UntypedExpr, ParseError>,
) -> impl Parser<Token, UntypedExpr, Error = ParseError> + '_ {
    sequence
        .delimited_by(just(Token::LeftBrace), just(Token::RightBrace))
        .map_with_span(assignment_into_sequence)
}

pub(super) fn assignment_into_sequence(expression: UntypedExpr, location: Span) -> UntypedExpr {
    if matches!(expression, UntypedExpr::Assignment { .. }) {
        UntypedExpr::Sequence {
            location,
            expressions: vec![expression],
        }
    } else {
        expression
    }
}

#[cfg(test)]
mod tests {
    use crate::{assert_definition, assert_expr};

    #[test]
    fn block_let() {
        assert_expr!(
            r#"
            let b = {
              let x = 4
              x + 5
            }
            "#
        );
    }

    #[test]
    fn block_single() {
        assert_expr!(
            r#"{
            foo
            }
            "#
        );
    }

    #[test]
    fn sequence_then_expr() {
        assert_definition!(
            r#"
            test foo() {
              {
                let a = Void
                a
              }
              True
            }
            "#
        );
    }

    #[test]
    fn sequence_then_sequence() {
        assert_definition!(
            r#"
            test foo() {
              {
                let a = Void
                a
              }
              let _ = True
              True
            }
            "#
        );
    }

    #[test]
    fn parse_simple_grouping() {
        assert_expr!("(x)");
    }

    #[test]
    fn parse_nested_grouping() {
        assert_expr!("((x))");
    }

    #[test]
    fn parse_parenthesized_assignment() {
        assert_expr!("(let x = 1)");
    }

    #[test]
    fn parse_bare_parenthesized_assignment() {
        assert_expr!("(x = 1)");
    }

    #[test]
    fn parse_parenthesized_sequence() {
        assert_expr!(
            r#"(
              let x = 1
              x
            )"#
        );
    }

    #[test]
    fn parse_newline_left_paren() {
        assert_expr!(
            r#"x
            (x)"#
        );
    }
}
