use crate::{
    ast,
    expr::{FnStyle, UntypedExpr},
    parser::{error::ParseError, token::Token},
};
use chumsky::prelude::*;

pub fn parser() -> impl Parser<Token, UntypedExpr, Error = ParseError> {
    select! {
        Token::EqualEqual => ast::BinOp::Eq,
        Token::NotEqual => ast::BinOp::NotEq,
        Token::Less => ast::BinOp::LtInt,
        Token::LessEqual => ast::BinOp::LtEqInt,
        Token::Greater => ast::BinOp::GtInt,
        Token::GreaterEqual => ast::BinOp::GtEqInt,
        Token::VbarVbar => ast::BinOp::Or,
        Token::AmperAmper => ast::BinOp::And,
        Token::Plus => ast::BinOp::AddInt,
        Token::Minus => ast::BinOp::SubInt,
        Token::Slash => ast::BinOp::DivInt,
        Token::Star => ast::BinOp::MultInt,
        Token::Percent => ast::BinOp::ModInt,
    }
    .map_with_span(|name, location| {
        use ast::BinOp::*;

        let arg_annotation = match name {
            Or | And => Some(ast::Annotation::boolean(location)),
            Eq | NotEq => None,
            LtInt | LtEqInt | GtInt | GtEqInt | AddInt | SubInt | MultInt | DivInt | ModInt => {
                Some(ast::Annotation::int(location))
            }
        };

        let return_annotation = match name {
            Or | And | Eq | NotEq | LtInt | LtEqInt | GtInt | GtEqInt => {
                Some(ast::Annotation::boolean(location))
            }
            AddInt | SubInt | MultInt | DivInt | ModInt => Some(ast::Annotation::int(location)),
        };

        let arguments = vec![
            ast::UntypedArg {
                by: ast::ArgBy::ByName(ast::ArgName::Named {
                    name: "left".to_string(),
                    label: "left".to_string(),
                    location,
                }),
                is_validator_param: false,
                annotation: arg_annotation.clone(),
                doc: None,
                location,
            },
            ast::UntypedArg {
                by: ast::ArgBy::ByName(ast::ArgName::Named {
                    name: "right".to_string(),
                    label: "right".to_string(),
                    location,
                }),
                is_validator_param: false,
                annotation: arg_annotation,
                doc: None,
                location,
            },
        ];

        let body = UntypedExpr::BinOp {
            location,
            name,
            left: Box::new(UntypedExpr::Var {
                location,
                name: "left".to_string(),
            }),
            right: Box::new(UntypedExpr::Var {
                location,
                name: "right".to_string(),
            }),
        };

        UntypedExpr::Fn {
            arguments,
            body: Box::new(body),
            return_annotation,
            fn_style: FnStyle::BinOp(name),
            location,
        }
    })
}

#[cfg(test)]
mod tests {
    use crate::assert_expr;

    #[test]
    fn first_class_binop() {
        assert_expr!(
            r#"
            compare_with(a, >, b)
            compare_with(a, >=, b)
            compare_with(a, <, b)
            compare_with(a, <=, b)
            compare_with(a, ==, b)
            compare_with(a, !=, b)
            combine_with(a, &&, b)
            combine_with(a, ||, b)
            compute_with(a, +, b)
            compute_with(a, -, b)
            compute_with(a, /, b)
            compute_with(a, *, b)
            compute_with(a, %, b)"#
        );
    }
}
