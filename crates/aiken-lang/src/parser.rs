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

    let stream = chumsky::Stream::from_iter(ast::Span::create(src.len(), 1), tokens.into_iter());

    let (definitions, definition_decorators) = definition::decorated_import()
        .repeated()
        .map(|imports| {
            let mut store = IndexMap::new();

            for (index, (import, decorators)) in imports.into_iter().enumerate() {
                // Decorated imports stay distinct so each invalid target is diagnosed
                // at its own source location rather than disappearing during merging.
                let decorated = (!decorators.is_empty()).then_some(index);
                let key = (import.module, import.as_name, decorated);
                match store.remove(&key) {
                    None => {
                        store.insert(key, (import.location, import.unqualified, decorators));
                    }
                    Some((location, unqualified, decorators)) => {
                        let mut merged_unqualified = Vec::new();
                        merged_unqualified.extend(unqualified.1);
                        merged_unqualified.extend(import.unqualified.1);
                        store.insert(
                            key,
                            (location, (unqualified.0, merged_unqualified), decorators),
                        );
                    }
                }
            }

            store
                .into_iter()
                .map(
                    |((module, as_name, _), (location, unqualified, decorators))| {
                        (
                            ast::Definition::Use(ast::Use {
                                module,
                                as_name,
                                location,
                                unqualified,
                                package: (),
                            }),
                            decorators,
                        )
                    },
                )
                .collect::<Vec<_>>()
        })
        .then(definition::with_decorators().repeated())
        .map(|(imports, others)| {
            let mut definitions = Vec::with_capacity(imports.len() + others.len());
            let mut definition_decorators = Vec::new();
            for (definition, decorators) in imports.into_iter().chain(others) {
                if !decorators.is_empty() {
                    definition_decorators.push(ast::DefinitionDecorators {
                        location: definition.location(),
                        decorators,
                    });
                }
                definitions.push(definition);
            }
            (definitions, definition_decorators)
        })
        .then_ignore(end())
        .parse(stream)?;

    let lines = LineNumbers::new(src);

    let module = ast::UntypedModule {
        kind,
        lines,
        definitions,
        definition_decorators,
        docs: vec![],
        name: "".to_string(),
        type_info: (),
    };

    Ok((module, extra))
}

#[cfg(test)]
mod tests {
    use super::{error::ErrorKind, module};
    use crate::assert_module;
    use crate::ast::ModuleKind;
    use miette::Diagnostic;

    #[test]
    fn onchain_declarations_retain_exact_definition_locations() {
        for declaration in [
            "fn answer() { 42 }",
            "pub fn answer() { 42 }",
            "validator answer { else(_) { True } }",
            "test answer() { True }",
            "bench answer(x via fuzz.any_int) { True }",
            "const answer = 42",
            "pub const answer = 42",
            "type Answer = Int",
            "use aiken/list",
        ] {
            let decorator = "@onchain(steps: 100, semantics: e)";
            let source = format!("{decorator}\n{declaration}");
            let (parsed, _) = module(&source, ModuleKind::Lib).unwrap();
            assert_eq!(parsed.definitions.len(), 1, "{declaration}");
            assert_eq!(parsed.definition_decorators.len(), 1, "{declaration}");
            let metadata = &parsed.definition_decorators[0];
            assert_eq!(metadata.location, parsed.definitions[0].location());
            assert_eq!(metadata.location.start, decorator.len() + 1);
            assert_eq!(&source[metadata.decorators[0].location.range()], decorator);
            let crate::ast::DecoratorKind::Onchain { arguments } = &metadata.decorators[0].kind
            else {
                panic!("expected onchain");
            };
            assert_eq!(arguments.len(), 2);
            assert!(matches!(&arguments[1].value,
                crate::ast::OnchainValue::Name { name } if name == "e"));
        }
    }

    #[test]
    fn onchain_invalid_settings_reach_type_checking() {
        for settings in [
            "",
            "()",
            "(semantics: C)",
            "(steps: 0)",
            "(steps: -1)",
            "(steps: nope)",
            "(steps: \"100\")",
            "(steps: @\"100\")",
            "(steps: 18446744073709551616)",
            "(steps: 1, steps: 2)",
            "(steps: 1, semantics: C, semantics: E)",
            "(steps: 1, unknown: thing)",
            "(steps: 1, semantics: \"C\")",
            "(steps: 1, semantics: X)",
        ] {
            let source = format!("@onchain{settings}\nfn answer() {{ 42 }}");
            let (parsed, _) = module(&source, ModuleKind::Lib).unwrap();
            assert_eq!(parsed.definition_decorators.len(), 1, "{settings}");
        }
    }

    #[test]
    fn onchain_preserves_argument_order_values_and_spans() {
        use crate::ast::{DecoratorKind, OnchainValue};
        use crate::parser::token::Base;
        let source =
            "@onchain(steps: -0x10, other: @\"x\", steps: 1_000, semantics: c)\nfn answer() { 42 }";
        let (parsed, _) = module(source, ModuleKind::Lib).unwrap();
        let DecoratorKind::Onchain { arguments } =
            &parsed.definition_decorators[0].decorators[0].kind
        else {
            panic!("expected onchain");
        };
        assert_eq!(
            arguments
                .iter()
                .map(|argument| argument.name.as_str())
                .collect::<Vec<_>>(),
            ["steps", "other", "steps", "semantics"]
        );
        assert!(matches!(&arguments[0].value,
            OnchainValue::Int { value, base: Base::Hexadecimal } if value == "-16"));
        assert!(matches!(&arguments[1].value,
            OnchainValue::String { value } if value == "x"));
        assert!(matches!(&arguments[2].value,
            OnchainValue::Int { value, base: Base::Decimal { numeric_underscore: true } } if value == "1000"));
        for (argument, expected) in arguments.iter().zip([
            "steps: -0x10",
            "other: @\"x\"",
            "steps: 1_000",
            "semantics: c",
        ]) {
            assert_eq!(&source[argument.location.range()], expected);
        }
    }

    #[test]
    fn onchain_preserves_duplicate_decorators_and_imports() {
        let source = "@onchain(steps: 1)\nuse aiken/list\n@onchain(steps: 2)\nuse aiken/list\n@onchain(steps: 3)\n@onchain(steps: 4)\nfn answer() { 42 }";
        let (parsed, _) = module(source, ModuleKind::Lib).unwrap();
        assert_eq!(parsed.definitions.len(), 3);
        assert_eq!(parsed.definition_decorators.len(), 3);
        assert_eq!(parsed.definition_decorators[2].decorators.len(), 2);
        for (metadata, definition) in parsed.definition_decorators.iter().zip(&parsed.definitions) {
            assert_eq!(metadata.location, definition.location());
        }
    }

    #[test]
    fn onchain_data_types_keep_existing_decorator_storage() {
        use crate::ast::{DecoratorKind, Definition};
        let source =
            "@onchain(steps: 1)\n@list\ntype Example { @onchain(steps: 2) @tag(7) Example(Int) }";
        let (parsed, _) = module(source, ModuleKind::Lib).unwrap();
        assert!(parsed.definition_decorators.is_empty());
        let Definition::DataType(data_type) = &parsed.definitions[0] else {
            panic!("expected data type");
        };
        assert!(matches!(
            data_type.decorators[0].kind,
            DecoratorKind::Onchain { .. }
        ));
        assert!(matches!(data_type.decorators[1].kind, DecoratorKind::List));
        assert!(matches!(
            data_type.constructors[0].decorators[0].kind,
            DecoratorKind::Onchain { .. }
        ));
        assert!(matches!(
            data_type.constructors[0].decorators[1].kind,
            DecoratorKind::Tag { value: 7, .. }
        ));
    }

    #[test]
    fn unexpected_eof_uses_source_byte_offset() {
        for src in [
            "fn foo() { 42",
            "fn foo() {\n  42\n\n   ",
            "fn foo() { \"héllo 🌍\"",
            "fn foo() { 42\n// trailing comment 🌍\n",
        ] {
            let errors = module(src, ModuleKind::Lib).unwrap_err();
            assert_eq!(errors.len(), 1, "source: {src:?}");
            assert!(matches!(errors[0].kind.as_ref(), ErrorKind::UnexpectedEnd));

            let labels = errors[0].labels().unwrap().collect::<Vec<_>>();
            assert_eq!(labels.len(), 1);
            assert_eq!(labels[0].offset(), src.len(), "source: {src:?}");
            assert_eq!(labels[0].len(), 1);
        }
    }

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
