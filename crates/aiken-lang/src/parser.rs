mod annotation;
pub mod definition;
pub mod error;
pub mod expr;
pub mod extra;
pub mod lexer;
pub mod pattern;
pub mod token;
mod utils;

pub use annotation::parser as annotation;
pub use definition::parser as definition;
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

    let definitions = definition().repeated().then_ignore(end()).parse(stream)?;

    let module = ast::UntypedModule {
        kind,
        definitions,
        docs: vec![],
        name: "".to_string(),
        type_info: (),
    };

    Ok((module, extra))
}

#[cfg(test)]
mod tests {
    use crate::assert_module;

    #[test]
    fn windows_newline() {
        assert_module!("use aiken/list\r\n");
    }

    #[test]
    fn can_handle_comments_at_end_of_file() {
        assert_module!(
            r#"
            use aiken

            // some comment
            // more comments"#
        );
    }
}
