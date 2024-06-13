use super::block;
use crate::{
    ast,
    expr::UntypedExpr,
    parser::{annotation, error::ParseError, pattern, token::Token},
};
use chumsky::prelude::*;

pub fn parser<'a>(
    sequence: Recursive<'a, Token, UntypedExpr, ParseError>,
    expression: Recursive<'a, Token, UntypedExpr, ParseError>,
) -> impl Parser<Token, UntypedExpr, Error = ParseError> + 'a {
    just(Token::If)
        .ignore_then(if_branch(sequence.clone(), expression.clone()))
        .then(
            just(Token::Else)
                .ignore_then(just(Token::If))
                .ignore_then(if_branch(sequence.clone(), expression))
                .repeated(),
        )
        .then_ignore(just(Token::Else))
        .then(block(sequence))
        .map_with_span(|((first, alternative_branches), final_else), span| {
            let mut branches = vec1::vec1![first];

            branches.extend(alternative_branches);

            UntypedExpr::If {
                location: span,
                branches,
                final_else: Box::new(final_else),
            }
        })
}

fn if_branch<'a>(
    sequence: Recursive<'a, Token, UntypedExpr, ParseError>,
    expression: Recursive<'a, Token, UntypedExpr, ParseError>,
) -> impl Parser<Token, ast::UntypedIfBranch, Error = ParseError> + 'a {
    expression
        .then(
            just(Token::Is)
                .ignore_then(
                    pattern()
                        .then_ignore(just(Token::Colon))
                        .or_not()
                        .then(annotation())
                        .map_with_span(|(pattern, annotation), span| (pattern, annotation, span)),
                )
                .or_not(),
        )
        .then(block(sequence))
        .map_with_span(|((condition, is), body), span| {
            let is = is.map(|(pattern, annotation, is_span)| {
                let pattern = pattern.unwrap_or_else(|| match &condition {
                    UntypedExpr::Var { name, location } => ast::Pattern::Var {
                        name: name.clone(),
                        location: *location,
                    },
                    _ => ast::Pattern::Discard {
                        location: is_span,
                        name: "_".to_string(),
                    },
                });

                ast::AssignmentPattern {
                    pattern,
                    annotation: Some(annotation),
                    location: is_span,
                }
            });

            ast::IfBranch {
                condition,
                body,
                is,
                location: span,
            }
        })
}

#[cfg(test)]
mod tests {
    use crate::assert_expr;

    #[test]
    fn if_else_basic() {
        assert_expr!(
            r#"
            if True {
              1 + 1
            } else if a < 1 {
              3
            } else {
              4
            }
            "#
        );
    }

    #[test]
    fn if_else_ambiguous_record() {
        assert_expr!(
            r#"
            if ec1 == Infinity {
              ec2
            } else if ec1 == Foo { foo } {
              ec1
            } else {
              Infinity
            }
            "#
        );
    }

    #[test]
    fn if_else_with_soft_cast() {
        assert_expr!(
            r#"
            if ec1 is Some(x): Option<Int> {
              ec2
            } else if ec1 is Foo { foo }: Foo {
              ec1
            } else if ec1 is Option<Int> {
              let Some(x) = ec1

              x
            } else {
              Infinity
            }
            "#
        );
    }

    #[test]
    fn if_soft_cast_discard_assign() {
        assert_expr!(
            r#"
            if foo() is Foo {
              todo
            } else {
              todo
            }
            "#
        );
    }

    #[test]
    fn if_soft_cast_not_var_condition() {
        assert_expr!(
            r#"
            if foo() is Foo { a }: Foo {
              todo
            } else {
              todo
            }
            "#
        );
    }
}
