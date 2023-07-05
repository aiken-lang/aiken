use chumsky::prelude::*;
use vec1::Vec1;

mod anonymous_binop;
pub mod anonymous_function;
pub mod assignment;
mod block;
pub(crate) mod bytearray;
mod chained;
mod if_else;
mod int;
mod list;
mod record;
mod record_update;
mod sequence;
pub mod string;
mod tuple;
mod var;
pub mod when;

pub use anonymous_function::parser as anonymous_function;
pub use block::parser as block;
pub use bytearray::parser as bytearray;
pub use chained::parser as chained;
pub use if_else::parser as if_else;
pub use int::parser as int;
pub use list::parser as list;
pub use record::parser as record;
pub use record_update::parser as record_update;
pub use sequence::parser as sequence;
pub use string::parser as string;
pub use tuple::parser as tuple;
pub use var::parser as var;
pub use when::parser as when;

use super::{error::ParseError, token::Token};
use crate::{ast, expr::UntypedExpr};

pub fn parser(
    sequence: Recursive<'_, Token, UntypedExpr, ParseError>,
) -> impl Parser<Token, UntypedExpr, Error = ParseError> + '_ {
    recursive(|expression| {
        let chained_debugged = chained(sequence, expression)
            .then(just(Token::Question).or_not())
            .map_with_span(|(value, token), location| match token {
                Some(_) => UntypedExpr::TraceIfFalse {
                    value: Box::new(value),
                    location,
                },
                None => value,
            });

        // Negate
        let op = choice((
            just(Token::Bang).to(ast::UnOp::Not),
            just(Token::Minus)
                // NOTE: Prevent conflict with usage for '-' as a standalone binary op.
                // This will make '-' parse when used as standalone binop in a function call.
                // For example:
                //
                //    foo(a, -, b)
                //
                // but it'll fail in a let-binding:
                //
                //    let foo = -
                //
                // which seems acceptable.
                .then_ignore(just(Token::Comma).not().rewind())
                .to(ast::UnOp::Negate),
        ));

        let unary = op
            .map_with_span(|op, span| (op, span))
            .repeated()
            .then(chained_debugged)
            .foldr(|(un_op, span), value| UntypedExpr::UnOp {
                op: un_op,
                location: span.union(value.location()),
                value: Box::new(value),
            })
            .boxed();

        // Product
        let op = choice((
            just(Token::Star).to(ast::BinOp::MultInt),
            just(Token::Slash).to(ast::BinOp::DivInt),
            just(Token::Percent).to(ast::BinOp::ModInt),
        ));

        let product = unary
            .clone()
            .then(op.then(unary).repeated())
            .foldl(|a, (op, b)| UntypedExpr::BinOp {
                location: a.location().union(b.location()),
                name: op,
                left: Box::new(a),
                right: Box::new(b),
            })
            .boxed();

        // Sum
        let op = choice((
            just(Token::Plus).to(ast::BinOp::AddInt),
            just(Token::Minus).to(ast::BinOp::SubInt),
        ));

        let sum = product
            .clone()
            .then(op.then(product).repeated())
            .foldl(|a, (op, b)| UntypedExpr::BinOp {
                location: a.location().union(b.location()),
                name: op,
                left: Box::new(a),
                right: Box::new(b),
            })
            .boxed();

        // Comparison
        let op = choice((
            just(Token::EqualEqual).to(ast::BinOp::Eq),
            just(Token::NotEqual).to(ast::BinOp::NotEq),
            just(Token::Less).to(ast::BinOp::LtInt),
            just(Token::Greater).to(ast::BinOp::GtInt),
            just(Token::LessEqual).to(ast::BinOp::LtEqInt),
            just(Token::GreaterEqual).to(ast::BinOp::GtEqInt),
        ));

        let comparison = sum
            .clone()
            .then(op.then(sum).repeated())
            .foldl(|a, (op, b)| UntypedExpr::BinOp {
                location: a.location().union(b.location()),
                name: op,
                left: Box::new(a),
                right: Box::new(b),
            })
            .boxed();

        // Conjunction
        let op = just(Token::AmperAmper).to(ast::BinOp::And);
        let conjunction = comparison
            .clone()
            .then(op.then(comparison).repeated())
            .foldl(|a, (op, b)| UntypedExpr::BinOp {
                location: a.location().union(b.location()),
                name: op,
                left: Box::new(a),
                right: Box::new(b),
            })
            .boxed();

        // Disjunction
        let op = just(Token::VbarVbar).to(ast::BinOp::Or);
        let disjunction = conjunction
            .clone()
            .then(op.then(conjunction).repeated())
            .foldl(|a, (op, b)| UntypedExpr::BinOp {
                location: a.location().union(b.location()),
                name: op,
                left: Box::new(a),
                right: Box::new(b),
            })
            .boxed();

        // Pipeline
        disjunction
            .clone()
            .then(
                choice((just(Token::Pipe), just(Token::NewLinePipe)))
                    .then(disjunction)
                    .repeated(),
            )
            .foldl(|l, (pipe, r)| {
                if let UntypedExpr::PipeLine {
                    mut expressions,
                    one_liner,
                } = l
                {
                    expressions.push(r);
                    return UntypedExpr::PipeLine {
                        expressions,
                        one_liner,
                    };
                }

                let mut expressions = Vec1::new(l);
                expressions.push(r);
                UntypedExpr::PipeLine {
                    expressions,
                    one_liner: pipe != Token::NewLinePipe,
                }
            })
    })
}

#[cfg(test)]
mod tests {
    use crate::assert_expr;

    #[test]
    fn plus_binop() {
        assert_expr!("a + 1");
    }

    #[test]
    fn pipeline() {
        assert_expr!(
            r#"
            a + 2
            |> add_one
            |> add_one
            "#
        );
    }

    #[test]
    fn field_access() {
        assert_expr!("user.name");
    }

    #[test]
    fn function_invoke() {
        assert_expr!(
            r#"
            let x = add_one(3)

            let map_add_x = list.map(_, fn (y) { x + y })

            map_add_x([ 1, 2, 3 ])
            "#
        );
    }
}
