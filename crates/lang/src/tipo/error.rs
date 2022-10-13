use std::sync::Arc;

use miette::Diagnostic;

use crate::ast::{Span, TodoKind};

use super::Type;

#[derive(Debug, thiserror::Error, Diagnostic)]
pub enum Error {}

#[derive(Debug, PartialEq, Clone)]
pub enum Warning {
    Todo {
        kind: TodoKind,
        location: Span,
        typ: Arc<Type>,
    },

    ImplicitlyDiscardedResult {
        location: Span,
    },

    UnusedLiteral {
        location: Span,
    },

    NoFieldsRecordUpdate {
        location: Span,
    },

    AllFieldsRecordUpdate {
        location: Span,
    },

    UnusedType {
        location: Span,
        imported: bool,
        name: String,
    },

    UnusedConstructor {
        location: Span,
        imported: bool,
        name: String,
    },

    UnusedImportedValue {
        location: Span,
        name: String,
    },

    UnusedImportedModule {
        location: Span,
        name: String,
    },

    UnusedPrivateModuleConstant {
        location: Span,
        name: String,
    },

    UnusedPrivateFunction {
        location: Span,
        name: String,
    },

    UnusedVariable {
        location: Span,
        name: String,
    },
}
