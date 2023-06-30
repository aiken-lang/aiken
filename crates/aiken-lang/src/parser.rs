mod annotation;
pub mod definitions;
pub mod error;
pub mod expr;
pub mod extra;
pub mod lexer;
pub mod pattern;
pub mod token;
mod utils;

pub use annotation::parser as annotation;
pub use definitions::parser as definitions;
pub use expr::parser as expression;
pub use pattern::parser as pattern;

use crate::ast;
use chumsky::prelude::*;
use error::ParseError;
use extra::ModuleExtra;

pub fn module(
    src: &str,
    kind: ast::ModuleKind,
) -> Result<(ast::UntypedModule, ModuleExtra), Vec<ParseError>> {
    let lexer::LexInfo { tokens, extra } = lexer::run(src)?;

    let stream = chumsky::Stream::from_iter(ast::Span::create(tokens.len()), tokens.into_iter());

    let definitions = definitions().parse(stream)?;

    let module = ast::UntypedModule {
        kind,
        definitions,
        docs: vec![],
        name: "".to_string(),
        type_info: (),
    };

    Ok((module, extra))
}
