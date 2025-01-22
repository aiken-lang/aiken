use crate::{
    edits::{self, AnnotatedEdit, ParsedDocument},
    server::lsp_project::LspProject,
};
use std::{collections::HashMap, str::FromStr};

const UNKNOWN_VARIABLE: &str = "aiken::check::unknown::variable";
const UNKNOWN_TYPE: &str = "aiken::check::unknown::type";
const UNKNOWN_CONSTRUCTOR: &str = "aiken::check::unknown::type_constructor";
const UNKNOWN_MODULE: &str = "aiken::check::unknown::module";
const UNUSED_IMPORT_VALUE: &str = "aiken::check::unused:import::value";
const UNUSED_IMPORT_MODULE: &str = "aiken::check::unused::import::module";
const USE_LET: &str = "aiken::check::single_constructor_expect";
const UNUSED_RECORD_FIELDS: &str = "aiken::check::syntax::unused_record_fields";
const UTF8_BYTE_ARRAY_IS_VALID_HEX_STRING: &str =
    "aiken::check::syntax::bytearray_literal_is_hex_string";

/// Errors for which we can provide quickfixes
#[allow(clippy::enum_variant_names)]
pub enum Quickfix {
    UnknownIdentifier(lsp_types::Diagnostic),
    UnknownModule(lsp_types::Diagnostic),
    UnknownConstructor(lsp_types::Diagnostic),
    UnusedImports(Vec<lsp_types::Diagnostic>),
    Utf8ByteArrayIsValidHexString(lsp_types::Diagnostic),
    UseLet(lsp_types::Diagnostic),
    UnusedRecordFields(lsp_types::Diagnostic),
}

fn match_code(
    diagnostic: &lsp_types::Diagnostic,
    severity: lsp_types::DiagnosticSeverity,
    expected: &str,
) -> bool {
    diagnostic.code == Some(lsp_types::NumberOrString::String(expected.to_string()))
        && diagnostic.severity == Some(severity)
}

/// Assert whether a diagnostic can be automatically fixed. Note that diagnostics often comes in
/// two severities, an error and hint; so we must be careful only addressing errors.
pub fn assert(diagnostic: lsp_types::Diagnostic) -> Option<Quickfix> {
    use lsp_types::DiagnosticSeverity as Severity;

    if match_code(&diagnostic, Severity::ERROR, UNKNOWN_VARIABLE)
        || match_code(&diagnostic, Severity::ERROR, UNKNOWN_TYPE)
    {
        return Some(Quickfix::UnknownIdentifier(diagnostic));
    }

    if match_code(&diagnostic, Severity::ERROR, UNKNOWN_CONSTRUCTOR) {
        return Some(Quickfix::UnknownConstructor(diagnostic));
    }

    if match_code(&diagnostic, Severity::ERROR, UNKNOWN_MODULE) {
        return Some(Quickfix::UnknownModule(diagnostic));
    }

    if match_code(&diagnostic, Severity::WARNING, UNUSED_IMPORT_VALUE)
        || match_code(&diagnostic, Severity::WARNING, UNUSED_IMPORT_MODULE)
    {
        return Some(Quickfix::UnusedImports(vec![diagnostic]));
    }

    if match_code(
        &diagnostic,
        Severity::WARNING,
        UTF8_BYTE_ARRAY_IS_VALID_HEX_STRING,
    ) {
        return Some(Quickfix::Utf8ByteArrayIsValidHexString(diagnostic));
    }

    if match_code(&diagnostic, Severity::WARNING, USE_LET) {
        return Some(Quickfix::UseLet(diagnostic));
    }

    if match_code(&diagnostic, Severity::WARNING, UNUSED_RECORD_FIELDS) {
        return Some(Quickfix::UnusedRecordFields(diagnostic));
    }

    None
}

pub fn quickfix(
    compiler: &LspProject,
    text_document: &lsp_types::TextDocumentIdentifier,
    quickfix: &Quickfix,
) -> Vec<lsp_types::CodeAction> {
    let mut actions = Vec::new();

    if let Some(ref parsed_document) = edits::parse_document(text_document) {
        match quickfix {
            Quickfix::UnknownIdentifier(diagnostic) => {
                each_as_distinct_action(
                    &mut actions,
                    text_document,
                    diagnostic,
                    unknown_identifier(compiler, parsed_document, diagnostic.data.as_ref()),
                );
            }
            Quickfix::UnknownModule(diagnostic) => each_as_distinct_action(
                &mut actions,
                text_document,
                diagnostic,
                unknown_module(compiler, parsed_document, diagnostic.data.as_ref()),
            ),
            Quickfix::UnknownConstructor(diagnostic) => each_as_distinct_action(
                &mut actions,
                text_document,
                diagnostic,
                unknown_constructor(compiler, parsed_document, diagnostic.data.as_ref()),
            ),
            Quickfix::UnusedImports(diagnostics) => as_single_action(
                &mut actions,
                text_document,
                diagnostics.to_owned(),
                "Remove redundant imports",
                unused_imports(
                    parsed_document,
                    diagnostics
                        .iter()
                        .map(|diagnostic| diagnostic.data.as_ref())
                        .collect(),
                ),
            ),
            Quickfix::Utf8ByteArrayIsValidHexString(diagnostic) => each_as_distinct_action(
                &mut actions,
                text_document,
                diagnostic,
                utf8_byte_array_is_hex_string(diagnostic),
            ),
            Quickfix::UseLet(diagnostic) => each_as_distinct_action(
                &mut actions,
                text_document,
                diagnostic,
                use_let(diagnostic),
            ),
            Quickfix::UnusedRecordFields(diagnostic) => each_as_distinct_action(
                &mut actions,
                text_document,
                diagnostic,
                unused_record_fields(diagnostic),
            ),
        };
    }

    actions
}

fn each_as_distinct_action(
    actions: &mut Vec<lsp_types::CodeAction>,
    text_document: &lsp_types::TextDocumentIdentifier,
    diagnostic: &lsp_types::Diagnostic,
    edits: Vec<AnnotatedEdit>,
) {
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

fn as_single_action(
    actions: &mut Vec<lsp_types::CodeAction>,
    text_document: &lsp_types::TextDocumentIdentifier,
    diagnostics: Vec<lsp_types::Diagnostic>,
    title: &str,
    edits: Vec<AnnotatedEdit>,
) {
    let mut changes = HashMap::new();

    changes.insert(
        text_document.uri.clone(),
        edits.into_iter().map(|(_, b)| b).collect(),
    );

    actions.push(lsp_types::CodeAction {
        title: title.to_string(),
        kind: Some(lsp_types::CodeActionKind::QUICKFIX),
        diagnostics: Some(diagnostics),
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

fn unknown_identifier(
    compiler: &LspProject,
    parsed_document: &ParsedDocument,
    data: Option<&serde_json::Value>,
) -> Vec<AnnotatedEdit> {
    let mut edits = Vec::new();

    if let Some(serde_json::Value::String(ref var_name)) = data {
        for module in compiler.project.modules() {
            if module.ast.has_definition(var_name) {
                if let Some(edit) = parsed_document.import(&module, Some(var_name)) {
                    edits.push(edit)
                }
            }
        }
    }

    edits
}

fn unknown_constructor(
    compiler: &LspProject,
    parsed_document: &ParsedDocument,
    data: Option<&serde_json::Value>,
) -> Vec<AnnotatedEdit> {
    let mut edits = Vec::new();

    if let Some(serde_json::Value::String(ref constructor_name)) = data {
        for module in compiler.project.modules() {
            if module.ast.has_constructor(constructor_name) {
                if let Some(edit) = parsed_document.import(&module, Some(constructor_name)) {
                    edits.push(edit)
                }
            }
        }
    }

    edits
}

fn unknown_module(
    compiler: &LspProject,
    parsed_document: &ParsedDocument,
    data: Option<&serde_json::Value>,
) -> Vec<AnnotatedEdit> {
    let mut edits = Vec::new();

    if let Some(serde_json::Value::String(ref module_name)) = data {
        for module in compiler.project.modules() {
            if module.name.ends_with(module_name) {
                if let Some(edit) = parsed_document.import(&module, None) {
                    edits.push(edit);
                }
            }
        }
    }

    edits
}

fn unused_imports(
    parsed_document: &ParsedDocument,
    datas: Vec<Option<&serde_json::Value>>,
) -> Vec<AnnotatedEdit> {
    let mut edits = Vec::new();

    for data in datas.iter().rev().flatten() {
        if let serde_json::Value::String(ref args) = data {
            let args = args.split(',').collect::<Vec<&str>>();
            match args.as_slice() {
                &[is_qualified, start] => {
                    let start = start
                        .parse::<usize>()
                        .expect("malformed unused_imports argument: not a usize");

                    let is_qualified = FromStr::from_str(is_qualified)
                        .expect("malformed unused_imports argument: not a bool");

                    edits.push(parsed_document.remove_import(start, is_qualified));
                }
                _ => {
                    panic!("malformed unused_imports arguments: not a 2-tuple");
                }
            }
        }
    }

    edits
}

fn utf8_byte_array_is_hex_string(diagnostic: &lsp_types::Diagnostic) -> Vec<AnnotatedEdit> {
    let mut edits = Vec::new();

    if let Some(serde_json::Value::String(ref value)) = diagnostic.data.as_ref() {
        edits.push((
            "Prefix with #".to_string(),
            lsp_types::TextEdit {
                range: diagnostic.range,
                new_text: format!("#\"{value}\""),
            },
        ))
    }

    edits
}

fn use_let(diagnostic: &lsp_types::Diagnostic) -> Vec<AnnotatedEdit> {
    vec![(
        "Use 'let' instead of 'expect'".to_string(),
        lsp_types::TextEdit {
            range: diagnostic.range,
            new_text: "let".to_string(),
        },
    )]
}

fn unused_record_fields(diagnostic: &lsp_types::Diagnostic) -> Vec<AnnotatedEdit> {
    let mut edits = Vec::new();

    if let Some(serde_json::Value::String(new_text)) = diagnostic.data.as_ref() {
        edits.push((
            "Destructure using named fields".to_string(),
            lsp_types::TextEdit {
                range: diagnostic.range,
                new_text: new_text.clone(),
            },
        ));
    }

    edits
}
