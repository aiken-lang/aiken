mod annotation;
pub mod chain;
pub mod definition;
pub mod error;
pub mod expr;
pub mod extra;
pub mod lexer;
pub mod literal;
pub mod pattern;
pub mod token;
mod utils;

use crate::{ast, line_numbers::LineNumbers};
pub use annotation::parser as annotation;
use chumsky::prelude::*;
pub use definition::{import::parser as import, parser as definition};
use error::ParseError;
pub use expr::parser as expression;
use extra::ModuleExtra;
use indexmap::IndexMap;
pub use pattern::parser as pattern;

pub fn module(
    src: &str,
    kind: ast::ModuleKind,
) -> Result<(ast::UntypedModule, ModuleExtra), Vec<ParseError>> {
    let lexer::LexInfo { tokens, extra } = lexer::run(src)?;

    let stream = chumsky::Stream::from_iter(ast::Span::create(tokens.len(), 1), tokens.into_iter());

    let definitions = import()
        .repeated()
        .map(|imports| {
            let mut store = IndexMap::new();

            for import in imports.into_iter() {
                let key = (import.module, import.as_name);
                match store.remove(&key) {
                    None => {
                        store.insert(key, (import.location, import.unqualified));
                    }
                    Some((location, unqualified)) => {
                        let mut merged_unqualified = Vec::new();
                        merged_unqualified.extend(unqualified);
                        merged_unqualified.extend(import.unqualified);
                        store.insert(key, (location, merged_unqualified));
                    }
                }
            }

            store
                .into_iter()
                .map(|((module, as_name), (location, unqualified))| {
                    ast::Definition::Use(ast::Use {
                        module,
                        as_name,
                        location,
                        unqualified,
                        package: (),
                    })
                })
                .collect::<Vec<ast::UntypedDefinition>>()
        })
        .then(definition().repeated())
        .map(|(imports, others)| {
            let mut defs = Vec::new();
            defs.extend(imports);
            defs.extend(others);
            defs
        })
        .then_ignore(end())
        .parse(stream)?;

    let lines = LineNumbers::new(src);

    let module = ast::UntypedModule {
        kind,
        lines,
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
    fn merge_imports() {
        assert_module!(
            r#"
            use aiken/list.{bar, foo}
            use aiken/list.{baz}
            "#
        );
    }

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

    #[test]
    fn function_ambiguous_sequence() {
        assert_module!(
            r#"
            fn foo_1() {
              let a = bar
              (40)
            }

            fn foo_2() {
              let a = bar
              {40}
            }

            fn foo_3() {
              let a = (40+2)
            }

            fn foo_4() {
              let a = bar(42)
              (a + 14) * 42
            }
            "#
        );
    }

    #[test]
    fn parse_unicode_offset_1() {
        assert_module!(
            r#"
            fn foo() {
              let x = "★"
              x
            }
            "#
        );
    }

    #[test]
    fn parse_unicode_offset_2() {
        assert_module!(
            r#"
            fn foo() {
              let x = "*"
              x
            }
            "#
        );
    }
}
