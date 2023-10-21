use crate::{edits, server::lsp_project::LspProject};
use std::collections::HashMap;

const UNKNOWN_VARIABLE: &str = "aiken::check::unknown::variable";

const UNKNOWN_MODULE: &str = "aiken::check::unknown::module";

/// Errors for which we can provide quickfixes
pub enum Quickfix {
    UnknownVariable,
}

fn match_code(diagnostic: &lsp_types::Diagnostic, expected: &str) -> bool {
    diagnostic.code == Some(lsp_types::NumberOrString::String(expected.to_string()))
}

pub fn assert(diagnostic: &lsp_types::Diagnostic) -> Option<Quickfix> {
    let is_error = diagnostic.severity == Some(lsp_types::DiagnosticSeverity::ERROR);

    if is_error && match_code(diagnostic, UNKNOWN_VARIABLE) {
        return Some(Quickfix::UnknownVariable);
    }

    if is_error && match_code(diagnostic, UNKNOWN_MODULE) {
        todo!()
    }

    None
}

pub fn unknown_variable(
    compiler: &LspProject,
    text_document: &lsp_types::TextDocumentIdentifier,
    diagnostic: &lsp_types::Diagnostic,
) -> Vec<lsp_types::CodeAction> {
    let mut actions = Vec::new();

    if let Some(parsed_document) = edits::parse_document(text_document) {
        if let Some(serde_json::Value::String(ref var_name)) = diagnostic.data {
            for module in compiler.project.modules() {
                let mut changes = HashMap::new();
                if module.ast.has_definition(var_name) {
                    if let Some((title, edit)) = parsed_document.import(&module, Some(var_name)) {
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
        }
    }

    actions
}
