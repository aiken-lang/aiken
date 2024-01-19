use crate::utils::span_to_lsp_range;
use aiken_lang::{
    ast::{Definition, ModuleKind, Span, UntypedDefinition, Use},
    line_numbers::LineNumbers,
};
use aiken_project::module::CheckedModule;
use itertools::Itertools;
use std::fs;

/// A freshly parsed module alongside its line numbers.
pub struct ParsedDocument {
    definitions: Vec<UntypedDefinition>,
    line_numbers: LineNumbers,
    source_code: String,
}

pub type AnnotatedEdit = (String, lsp_types::TextEdit);

/// Parse the target document as an 'UntypedModule' alongside its line numbers. This is useful in
/// case we need to manipulate the AST for a quickfix.
pub fn parse_document(document: &lsp_types::TextDocumentIdentifier) -> Option<ParsedDocument> {
    let file_path = document
        .uri
        .to_file_path()
        .expect("invalid text document uri?");

    let source_code = fs::read_to_string(file_path).ok()?;

    let line_numbers = LineNumbers::new(&source_code);

    // NOTE: The 'ModuleKind' second argument doesn't matter. This is just added to the final
    // object but has no influence on the parsing.
    let (untyped_module, _) = aiken_lang::parser::module(&source_code, ModuleKind::Lib).ok()?;

    Some(ParsedDocument {
        definitions: untyped_module.definitions,
        line_numbers,
        source_code,
    })
}

/// Insert some text at the given location.
fn insert_text(at: usize, line_numbers: &LineNumbers, new_text: String) -> lsp_types::TextEdit {
    let range = span_to_lsp_range(Span { start: at, end: at }, line_numbers);
    lsp_types::TextEdit { range, new_text }
}

/// Find a suitable location (Span) in the import list. The boolean in the answer indicates
/// whether the import is a newline or not. It is set to 'false' when adding a qualified import
/// to an existing list.
impl ParsedDocument {
    pub fn import(
        &self,
        import: &CheckedModule,
        unqualified: Option<&str>,
    ) -> Option<AnnotatedEdit> {
        let import_path = import.name.split('/').collect_vec();

        let mut last_import = None;

        for def in self.definitions.iter() {
            match def {
                Definition::Use(Use {
                    location,
                    module: existing_module,
                    unqualified: unqualified_list,
                    ..
                }) => {
                    last_import = Some(*location);

                    if import_path != existing_module.as_slice() {
                        continue;
                    }

                    match unqualified {
                        // There's already a matching qualified import, so we have nothing to do.
                        None => return None,
                        Some(unqualified) => {
                            let mut last_unqualified = None;

                            // Insert lexicographically, assuming unqualified imports are already
                            // ordered. If they are not, it doesn't really matter where we insert
                            // anyway.
                            for existing_unqualified in unqualified_list {
                                last_unqualified = Some(existing_unqualified.location);

                                let existing_name = existing_unqualified
                                    .as_name
                                    .as_ref()
                                    .unwrap_or(&existing_unqualified.name);

                                // The unqualified import already exist, nothing to do.
                                if unqualified == existing_name {
                                    return None;
                                // Current import is lexicographically greater, we can insert before
                                } else if unqualified < existing_name.as_str() {
                                    return Some(self.insert_qualified_before(
                                        import,
                                        unqualified,
                                        existing_unqualified.location,
                                    ));
                                } else {
                                    continue;
                                }
                            }

                            return match last_unqualified {
                                // Only happens if 'unqualified_list' is empty, in which case, we
                                // simply create a new unqualified list of import.
                                None => {
                                    Some(self.add_new_qualified(import, unqualified, *location))
                                }
                                // Happens if the new qualified import is lexicographically after
                                // all existing ones.
                                Some(location) => {
                                    Some(self.insert_qualified_after(import, unqualified, location))
                                }
                            };
                        }
                    }
                }
                _ => continue,
            }
        }

        // If the search above didn't lead to anything, we simply insert the import either:
        //
        // (a) After the last import statement if any;
        // (b) As the first statement in the module.
        Some(self.add_new_import_line(import, unqualified, last_import))
    }

    pub fn remove_import(&self, start: usize, is_qualified: bool) -> AnnotatedEdit {
        let offset = if is_qualified {
            let import_len = self
                .source_code
                .chars()
                .skip(start)
                .take_while(|c| c != &',' && c != &'}')
                .count();

            let has_trailing_comma = self
                .source_code
                .chars()
                .skip(start + import_len)
                .collect::<String>()
                .starts_with(',');

            import_len + if has_trailing_comma { 1 } else { 0 }
        } else {
            1 + self
                .source_code
                .chars()
                .skip(start)
                .take_while(|c| c != &'\n')
                .count()
        };

        let range = span_to_lsp_range(
            Span {
                start,
                end: start + offset,
            },
            &self.line_numbers,
        );

        let new_text = String::new();

        (
            "Remove redundant import".to_string(),
            lsp_types::TextEdit { range, new_text },
        )
    }

    fn insert_qualified_before(
        &self,
        import: &CheckedModule,
        unqualified: &str,
        location: Span,
    ) -> AnnotatedEdit {
        let title = format!("Use '{}' from {}", unqualified, import.name);
        (
            title,
            insert_text(
                location.start,
                &self.line_numbers,
                format!("{}, ", unqualified),
            ),
        )
    }

    fn insert_qualified_after(
        &self,
        import: &CheckedModule,
        unqualified: &str,
        location: Span,
    ) -> AnnotatedEdit {
        let title = format!("Use '{}' from {}", unqualified, import.name);
        (
            title,
            insert_text(
                location.end,
                &self.line_numbers,
                format!(", {}", unqualified),
            ),
        )
    }

    fn add_new_qualified(
        &self,
        import: &CheckedModule,
        unqualified: &str,
        location: Span,
    ) -> AnnotatedEdit {
        let title = format!("Use '{}' from {}", unqualified, import.name);
        (
            title,
            insert_text(
                location.end,
                &self.line_numbers,
                format!(".{{{}}}", unqualified),
            ),
        )
    }

    fn add_new_import_line(
        &self,
        import: &CheckedModule,
        unqualified: Option<&str>,
        location: Option<Span>,
    ) -> AnnotatedEdit {
        let import_line = format!(
            "use {}{}",
            import.name,
            match unqualified {
                Some(unqualified) => format!(".{{{}}}", unqualified),
                None => String::new(),
            }
        );

        let title = format!("Add new import line: {import_line}");

        (
            title,
            match location {
                None => insert_text(0, &self.line_numbers, format!("{import_line}\n")),
                Some(Span { end, .. }) => {
                    insert_text(end, &self.line_numbers, format!("\n{import_line}"))
                }
            },
        )
    }
}
