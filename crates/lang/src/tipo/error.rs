use std::sync::Arc;

use miette::Diagnostic;

use crate::ast::{BinOp, Span, TodoKind};

use super::Type;

#[derive(Debug, thiserror::Error, Diagnostic)]
pub enum Error {
    #[error("duplicate const {name}")]
    DuplicateConstName {
        #[label]
        location: Span,
        #[label]
        previous_location: Span,
        name: String,
    },

    #[error("duplicate import {name}")]
    DuplicateImport {
        #[label]
        location: Span,
        #[label]
        previous_location: Span,
        name: String,
    },

    #[error("duplicate name {label}")]
    DuplicateField {
        #[label]
        location: Span,
        label: String,
    },

    #[error("duplicate name {name}")]
    DuplicateName {
        #[label]
        location: Span,
        #[label]
        previous_location: Span,
        name: String,
    },

    #[error("duplicate type name {name}")]
    DuplicateTypeName {
        location: Span,
        previous_location: Span,
        name: String,
    },

    #[error("{name} has incorrect type arity expected {expected} but given {given}")]
    IncorrectTypeArity {
        location: Span,
        name: String,
        expected: usize,
        given: usize,
    },

    #[error("{name} contains keyword {keyword}")]
    KeywordInModuleName { name: String, keyword: String },

    #[error("{name} is a reserved module name")]
    ReservedModuleName { name: String },

    #[error("unexpected type hole")]
    UnexpectedTypeHole {
        #[label]
        location: Span,
    },

    #[error("unknown module {name}")]
    UnknownModule {
        location: Span,
        name: String,
        imported_modules: Vec<String>,
    },

    #[error("unknown module field {name} in module {module_name}")]
    UnknownModuleField {
        location: Span,
        name: String,
        module_name: String,
        value_constructors: Vec<String>,
        type_constructors: Vec<String>,
    },

    #[error("")]
    UnknownType {
        location: Span,
        name: String,
        types: Vec<String>,
    },

    #[error("")]
    UnknownTypeConstructorType {
        location: Span,
        name: String,
        type_constructors: Vec<String>,
    },

    #[error("")]
    UnknownTypeConstructorModule {
        location: Span,
        name: String,
        imported_modules: Vec<String>,
    },

    #[error("")]
    UnknownTypeConstructorModuleType {
        location: Span,
        name: String,
        module_name: Vec<String>,
        type_constructors: Vec<String>,
    },

    #[error("")]
    CouldNotUnify {
        location: Span,
        expected: Arc<Type>,
        given: Arc<Type>,
        situation: Option<UnifyErrorSituation>,
    },

    #[error("")]
    ExtraVarInAlternativePattern { location: Span, name: String },

    #[error("")]
    MissingVarInAlternativePattern { location: Span, name: String },

    #[error("")]
    DuplicateVarInPattern { location: Span, name: String },

    #[error("")]
    RecursiveType { location: Span },
}

impl Error {
    pub fn flip_unify(self) -> Error {
        match self {
            Error::CouldNotUnify {
                location,
                expected,
                given,
                situation: note,
            } => Error::CouldNotUnify {
                location,
                expected: given,
                given: expected,
                situation: note,
            },
            other => other,
        }
    }
}

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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnifyErrorSituation {
    /// Clauses in a case expression were found to return different types.
    CaseClauseMismatch,

    /// A function was found to return a value that did not match its return
    /// annotation.
    ReturnAnnotationMismatch,

    PipeTypeMismatch,

    /// The operands of a binary operator were incorrect.
    Operator(BinOp),

    /// A try expression returned a different error type to the previous try.
    TryErrorMismatch,

    /// The final value of a try expression was not a Result.
    TryReturnResult,
}
