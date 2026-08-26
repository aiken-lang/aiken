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
                        merged_unqualified.extend(unqualified.1);
                        merged_unqualified.extend(import.unqualified.1);
                        store.insert(key, (location, (unqualified.0, merged_unqualified)));
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
    use crate::{assert_module, ast::ModuleKind};
    use std::{
        process::Command,
        thread,
        time::{Duration, Instant},
    };

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

    fn issue_1377_source(depth: usize) -> String {
        let expression = (0..depth).fold("3".to_string(), |expression, index| {
            let operator = if index % 3 == 1 { "*" } else { "+" };
            format!("({expression} {operator} {})", index + 4)
        });

        format!("pub fn probe(y: Int) -> Int {{\n  let value = {expression}\n  value * y\n}}\n")
    }

    #[test]
    fn issue_1377_parenthesized_operator_chain_parses_at_safe_depth() {
        super::module(&issue_1377_source(12), ModuleKind::Lib)
            .expect("parenthesized operator chain should parse");
    }

    #[test]
    #[ignore = "issue #1377: depth 18 exceeds the bounded parser deadline"]
    fn issue_1377_parenthesized_operator_chain_deadline_reproducer() {
        const CHILD_ENV: &str = "AIKEN_ISSUE_1377_CHILD";
        const DEPTH: usize = 18;
        const DEADLINE: Duration = Duration::from_secs(10);

        if std::env::var_os(CHILD_ENV).is_some() {
            super::module(&issue_1377_source(DEPTH), ModuleKind::Lib)
                .expect("parenthesized operator chain should parse");
            return;
        }

        // Calibration: on an Apple M3 Max at 5bcde6d, depth 18 took 99.72s.
        // Ten seconds separates that nonlinear behavior while keeping the forced
        // reproducer bounded well below five minutes on macOS and Linux.
        let mut child = Command::new(std::env::current_exe().expect("test binary should exist"))
            .args([
                "--ignored",
                "--exact",
                "parser::tests::issue_1377_parenthesized_operator_chain_deadline_reproducer",
                "--nocapture",
            ])
            .env(CHILD_ENV, "1")
            .spawn()
            .expect("parser subprocess should start");
        let deadline = Instant::now() + DEADLINE;

        loop {
            if let Some(status) = child.try_wait().expect("parser subprocess should be waitable") {
                assert!(
                    status.success(),
                    "issue #1377 parser subprocess exited unsuccessfully: {status}"
                );
                return;
            }

            if Instant::now() >= deadline {
                let _ = child.kill();
                let _ = child.wait();
                panic!(
                    "issue #1377: depth {DEPTH} parse exceeded the {}s wall-clock deadline",
                    DEADLINE.as_secs()
                );
            }

            thread::sleep(Duration::from_millis(25));
        }
    }
}
