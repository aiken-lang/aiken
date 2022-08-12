use chumsky::prelude::*;

use crate::{ast, error::ParseError, token::Token};

pub fn module_parser() -> impl Parser<Token, ast::UntypedModule, Error = ParseError> {
    let imports = just(Token::Use).ignore_then();
}
