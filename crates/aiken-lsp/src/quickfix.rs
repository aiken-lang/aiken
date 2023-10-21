use crate::{
    edits::{self, AnnotatedEdit, ParsedDocument},
    server::lsp_project::LspProject,
};
use std::collections::HashMap;

const UNKNOWN_VARIABLE: &str = "aiken::check::unknown::variable";
const UNKNOWN_TYPE: &str = "aiken::check::unknown::type";
const UNKNOWN_CONSTRUCTOR: &str = "aiken::check::unknown::type_constructor";
const UNKNOWN_MODULE: &str = "aiken::check::unknown::module";

/// Errors for which we can provide quickfixes
#[allow(clippy::enum_variant_names)]
pub enum Quickfix {
    UnknownIdentifier,
    UnknownModule,
    UnknownConstructor,
}

fn match_code(diagnostic: &lsp_types::Diagnostic, expected: &str) -> bool {
    diagnostic.code == Some(lsp_types::NumberOrString::String(expected.to_string()))
}

/// Assert whether a diagnostic can be automatically fixed. Note that diagnostics often comes in
/// two severities, an error and hint; so we must be careful only addressing errors.
pub fn assert(diagnostic: &lsp_types::Diagnostic) -> Option<Quickfix> {
    let is_error = diagnostic.severity == Some(lsp_types::DiagnosticSeverity::ERROR);

    if !is_error {
        return None;
    }

    if match_code(diagnostic, UNKNOWN_VARIABLE) {
        return Some(Quickfix::UnknownIdentifier);
    }

    if match_code(diagnostic, UNKNOWN_TYPE) {
        return Some(Quickfix::UnknownIdentifier);
    }

    if match_code(diagnostic, UNKNOWN_CONSTRUCTOR) {
        return Some(Quickfix::UnknownConstructor);
    }

    if match_code(diagnostic, UNKNOWN_MODULE) {
        return Some(Quickfix::UnknownModule);
    }

    None
}

pub fn quickfix(
    compiler: &LspProject,
    text_document: &lsp_types::TextDocumentIdentifier,
    diagnostic: &lsp_types::Diagnostic,
    quickfix: &Quickfix,
) -> Vec<lsp_types::CodeAction> {
    let mut actions = Vec::new();

    if let Some(ref parsed_document) = edits::parse_document(text_document) {
        if let Some(serde_json::Value::String(ref data)) = diagnostic.data {
            let edits = match quickfix {
                Quickfix::UnknownIdentifier => unknown_identifier(compiler, parsed_document, data),
                Quickfix::UnknownModule => unknown_module(compiler, parsed_document, data),
                Quickfix::UnknownConstructor => {
                    unknown_constructor(compiler, parsed_document, data)
                }
            };

            for (title, edit) in edits.into_iter() {
                let mut changes = HashMap::new();
                changes.insert(text_document.uri.clone(), vec![edit]);
                actions.push(lsp_types::CodeAction {
                    title,
                    kind: Some(lsp_types::CodeActionKind::QUICKFIX),
                    diagnostics: Some(vec![diagnostic.clone()]),
                    is_preferred: Some(true),
                    disabled: None,
                    data: None,
                    command: None,
                    edit: Some(lsp_types::WorkspaceEdit {
                        changes: Some(changes),
                        document_changes: None,
                        change_annotations: None,
                    }),
                });
            }
        }
    }

    actions
}

fn unknown_identifier(
    compiler: &LspProject,
    parsed_document: &ParsedDocument,
    var_name: &str,
) -> Vec<AnnotatedEdit> {
    let mut edits = Vec::new();

    for module in compiler.project.modules() {
        if module.ast.has_definition(var_name) {
            if let Some(edit) = parsed_document.import(&module, Some(var_name)) {
                edits.push(edit)
            }
        }
    }

    edits
}

fn unknown_constructor(
    compiler: &LspProject,
    parsed_document: &ParsedDocument,
    constructor_name: &str,
) -> Vec<AnnotatedEdit> {
    let mut edits = Vec::new();

    for module in compiler.project.modules() {
        if module.ast.has_constructor(constructor_name) {
            if let Some(edit) = parsed_document.import(&module, Some(constructor_name)) {
                edits.push(edit)
            }
        }
    }

    edits
}

fn unknown_module(
    compiler: &LspProject,
    parsed_document: &ParsedDocument,
    module_name: &str,
) -> Vec<AnnotatedEdit> {
    let mut edits = Vec::new();

    for module in compiler.project.modules() {
        if module.name.ends_with(module_name) {
            if let Some(edit) = parsed_document.import(&module, None) {
                edits.push(edit);
            }
        }
    }

    edits
}
