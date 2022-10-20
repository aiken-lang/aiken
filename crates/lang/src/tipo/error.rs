use std::{collections::HashMap, sync::Arc};

use miette::Diagnostic;

use crate::ast::{BinOp, Span, TodoKind};

use super::Type;

// use aiken/pub

// pub fn do_thing() { pub.other() }

#[derive(Debug, thiserror::Error, Diagnostic)]
pub enum Error {
    #[error("duplicate argument {label}")]
    DuplicateArgument {
        #[label]
        location: Span,
        label: String,
    },

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

    #[error("incorrect arity expected {expected} but given {given}")]
    IncorrectArity {
        #[label]
        location: Span,
        expected: usize,
        given: usize,
        labels: Vec<String>,
    },

    #[error("{name} has incorrect type arity expected {expected} but given {given}")]
    IncorrectTypeArity {
        location: Span,
        name: String,
        expected: usize,
        given: usize,
    },

    #[error("not a function")]
    NotFn {
        #[label]
        location: Span,
        tipo: Arc<Type>,
    },

    #[error("{name} contains keyword {keyword}")]
    KeywordInModuleName { name: String, keyword: String },

    #[error("clause guard {name} is not local")]
    NonLocalClauseGuardVariable {
        #[label]
        location: Span,
        name: String,
    },

    #[error("positional argument after labeled")]
    PositionalArgumentAfterLabeled {
        #[label]
        location: Span,
    },

    #[error("private type leaked")]
    PrivateTypeLeak {
        #[label]
        location: Span,
        leaked: Type,
    },

    #[error("{name} is a reserved module name")]
    ReservedModuleName { name: String },

    #[error("unexpected labeled argument {label}")]
    UnexpectedLabeledArg {
        #[label]
        location: Span,
        label: String,
    },

    #[error("unexpected type hole")]
    UnexpectedTypeHole {
        #[label]
        location: Span,
    },

    #[error("unknown labels")]
    UnknownLabels {
        unknown: Vec<(String, Span)>,
        valid: Vec<String>,
        supplied: Vec<String>,
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

    #[error("unknown module value {name}")]
    UnknownModuleValue {
        #[label]
        location: Span,
        name: String,
        module_name: String,
        value_constructors: Vec<String>,
    },

    #[error("unknown type {name} in module {module_name}")]
    UnknownModuleType {
        #[label]
        location: Span,
        name: String,
        module_name: String,
        type_constructors: Vec<String>,
    },

    #[error("unknown type {name}")]
    UnknownType {
        #[label]
        location: Span,
        name: String,
        types: Vec<String>,
    },

    #[error("unknown variable {name}")]
    UnknownVariable {
        #[label]
        location: Span,
        name: String,
        variables: Vec<String>,
    },

    #[error("")]
    CouldNotUnify {
        #[label]
        location: Span,
        expected: Arc<Type>,
        given: Arc<Type>,
        situation: Option<UnifyErrorSituation>,
        rigid_type_names: HashMap<u64, String>,
    },

    #[error("")]
    ExtraVarInAlternativePattern {
        #[label]
        location: Span,
        name: String,
    },

    #[error("")]
    MissingVarInAlternativePattern {
        #[label]
        location: Span,
        name: String,
    },

    #[error("")]
    DuplicateVarInPattern {
        #[label]
        location: Span,
        name: String,
    },

    #[error("")]
    RecursiveType {
        #[label]
        location: Span,
    },
}

impl Error {
    pub fn flip_unify(self) -> Error {
        match self {
            Error::CouldNotUnify {
                location,
                expected,
                given,
                situation: note,
                rigid_type_names,
            } => Error::CouldNotUnify {
                location,
                expected: given,
                given: expected,
                situation: note,
                rigid_type_names,
            },
            other => other,
        }
    }

    pub fn with_unify_error_rigid_names(mut self, new_names: &HashMap<u64, String>) -> Self {
        match self {
            Error::CouldNotUnify {
                rigid_type_names: ref mut annotated_names,
                ..
            } => {
                *annotated_names = new_names.clone();
                self
            }
            _ => self,
        }
    }

    pub fn with_unify_error_situation(self, situation: UnifyErrorSituation) -> Self {
        match self {
            Self::CouldNotUnify {
                expected,
                given,
                location,
                rigid_type_names,
                ..
            } => Self::CouldNotUnify {
                expected,
                given,
                situation: Some(situation),
                location,
                rigid_type_names,
            },
            other => other,
        }
    }

    pub fn return_annotation_mismatch(self) -> Self {
        self.with_unify_error_situation(UnifyErrorSituation::ReturnAnnotationMismatch)
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
