use std::{collections::HashMap, sync::Arc};

use miette::Diagnostic;

use crate::ast::{BinOp, Span, TodoKind};

use super::Type;

#[derive(Debug, thiserror::Error, Diagnostic)]
pub enum Error {
    #[error("Duplicate argument\n\n{label}")]
    #[diagnostic(help("Try renaming it"))]
    DuplicateArgument {
        #[label]
        location: Span,
        label: String,
    },

    #[error("Duplicate const\n\n{name}")]
    #[diagnostic(help("Try renaming it"))]
    DuplicateConstName {
        #[label]
        location: Span,
        #[label]
        previous_location: Span,
        name: String,
    },

    #[error("Duplicate import\n\n{name}")]
    #[diagnostic(help("Try renaming it"))]
    DuplicateImport {
        #[label]
        location: Span,
        #[label]
        previous_location: Span,
        name: String,
    },

    #[error("Duplicate field\n\n{label}")]
    #[diagnostic(help("Try renaming it"))]
    DuplicateField {
        #[label]
        location: Span,
        label: String,
    },

    #[error("Duplicate name\n\n{name}")]
    #[diagnostic(help("Try renaming it"))]
    DuplicateName {
        #[label]
        location: Span,
        #[label]
        previous_location: Span,
        name: String,
    },

    #[error("Duplicate type name\n\n{name}")]
    #[diagnostic(help("Try renaming it"))]
    DuplicateTypeName {
        #[label]
        location: Span,
        #[label]
        previous_location: Span,
        name: String,
    },

    #[error("Incorrect arity\n\nExpected\n\n{expected}\n\nGiven\n\n{given}")]
    IncorrectArity {
        #[label]
        location: Span,
        expected: usize,
        given: usize,
        labels: Vec<String>,
    },

    #[error("Incorrect number of clause patterns\n\nExpected\n\n{expected}\n\nGiven\n\n{given}")]
    IncorrectNumClausePatterns {
        #[label]
        location: Span,
        expected: usize,
        given: usize,
    },

    #[error("Incorrect type arity for `{name}`\n\nExpected\n\n{expected}\n\nGiven\n\n{given}")]
    IncorrectTypeArity {
        #[label]
        location: Span,
        name: String,
        expected: usize,
        given: usize,
    },

    #[error("Non-exhaustive pattern match")]
    NotExhaustivePatternMatch {
        #[label]
        location: Span,
        unmatched: Vec<String>,
    },

    #[error("Not a function")]
    NotFn {
        #[label]
        location: Span,
        tipo: Arc<Type>,
    },

    #[error("Module\n\n{name}\n\ncontains keyword\n\n{keyword}")]
    KeywordInModuleName { name: String, keyword: String },

    #[error("Clause guard {name} is not local")]
    NonLocalClauseGuardVariable {
        #[label]
        location: Span,
        name: String,
    },

    #[error("Positional argument after labeled")]
    PositionalArgumentAfterLabeled {
        #[label]
        location: Span,
    },

    #[error("Private type leaked")]
    PrivateTypeLeak {
        #[label]
        location: Span,
        leaked: Type,
    },

    #[error("Record access unknown type")]
    RecordAccessUnknownType {
        #[label]
        location: Span,
    },

    #[error("Record update invalid constructor")]
    RecordUpdateInvalidConstructor {
        #[label]
        location: Span,
    },

    #[error("{name} is a reserved module name")]
    ReservedModuleName { name: String },

    #[error("Unexpected labeled argument\n\n{label}")]
    UnexpectedLabeledArg {
        #[label]
        location: Span,
        label: String,
    },

    #[error("Unexpected type hole")]
    UnexpectedTypeHole {
        #[label]
        location: Span,
    },

    #[error("Unknown labels")]
    UnknownLabels {
        unknown: Vec<(String, Span)>,
        valid: Vec<String>,
        supplied: Vec<String>,
    },

    #[error("Unknown module\n\n{name}")]
    UnknownModule {
        #[label]
        location: Span,
        name: String,
        imported_modules: Vec<String>,
    },

    #[error("Unknown module field\n\n{name}\n\nin module\n\n{module_name}")]
    UnknownModuleField {
        location: Span,
        name: String,
        module_name: String,
        value_constructors: Vec<String>,
        type_constructors: Vec<String>,
    },

    #[error("Unknown module value\n\n{name}")]
    UnknownModuleValue {
        #[label]
        location: Span,
        name: String,
        module_name: String,
        value_constructors: Vec<String>,
    },

    #[error("Unknown type\n\n{name}\n\nin module\n\n{module_name}")]
    UnknownModuleType {
        #[label]
        location: Span,
        name: String,
        module_name: String,
        type_constructors: Vec<String>,
    },

    #[error("Unknown record field\n\n{label}")]
    UnknownRecordField {
        #[label]
        location: Span,
        typ: Arc<Type>,
        label: String,
        fields: Vec<String>,
        situation: Option<UnknownRecordFieldSituation>,
    },

    #[error("Unknown type\n\n{name}")]
    UnknownType {
        #[label]
        location: Span,
        name: String,
        types: Vec<String>,
    },

    #[error("Unknown variable\n\n{name}")]
    UnknownVariable {
        #[label]
        location: Span,
        name: String,
        variables: Vec<String>,
    },

    #[error("Unnecessary spread operator")]
    UnnecessarySpreadOperator {
        #[label]
        location: Span,
        arity: usize,
    },

    #[error("Cannot update a type with multiple constructors")]
    UpdateMultiConstructorType {
        #[label]
        location: Span,
    },

    #[error(
        "Type Mismatch\n\nExpected type:\n\n{}\n\nFound type:\n\n{}\n",
        expected.to_pretty_with_names(rigid_type_names.clone(), 4),
        given.to_pretty_with_names(rigid_type_names.clone(), 4)
    )]
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
    pub fn call_situation(mut self) -> Self {
        if let Error::UnknownRecordField {
            ref mut situation, ..
        } = self
        {
            *situation = Some(UnknownRecordFieldSituation::FunctionCall);
        }
        self
    }

    pub fn case_clause_mismatch(self) -> Self {
        self.with_unify_error_situation(UnifyErrorSituation::CaseClauseMismatch)
    }

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

    pub fn inconsistent_try(self, return_value_is_result: bool) -> Self {
        self.with_unify_error_situation(if return_value_is_result {
            UnifyErrorSituation::TryErrorMismatch
        } else {
            UnifyErrorSituation::TryReturnResult
        })
    }

    pub fn operator_situation(self, binop: BinOp) -> Self {
        self.with_unify_error_situation(UnifyErrorSituation::Operator(binop))
    }

    pub fn return_annotation_mismatch(self) -> Self {
        self.with_unify_error_situation(UnifyErrorSituation::ReturnAnnotationMismatch)
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
}

#[derive(Debug, PartialEq, Clone, thiserror::Error, Diagnostic)]
pub enum Warning {
    #[error("todo")]
    Todo {
        kind: TodoKind,
        #[label]
        location: Span,
        tipo: Arc<Type>,
    },

    #[error("implicitly discarded result")]
    ImplicitlyDiscardedResult {
        #[label]
        location: Span,
    },

    #[error("unused literal")]
    UnusedLiteral {
        #[label]
        location: Span,
    },

    #[error("record update with no fields")]
    NoFieldsRecordUpdate {
        #[label]
        location: Span,
    },

    #[error("record update using all fields")]
    AllFieldsRecordUpdate {
        #[label]
        location: Span,
    },

    #[error("unused type {name}")]
    UnusedType {
        #[label]
        location: Span,
        imported: bool,
        name: String,
    },

    #[error("unused constructor {name}")]
    UnusedConstructor {
        #[label]
        location: Span,
        imported: bool,
        name: String,
    },

    #[error("unused imported value {name}")]
    UnusedImportedValue {
        #[label]
        location: Span,
        name: String,
    },

    #[error("unused imported module {name}")]
    UnusedImportedModule {
        #[label]
        location: Span,
        name: String,
    },

    #[error("unused private module constant {name}")]
    UnusedPrivateModuleConstant {
        #[label]
        location: Span,
        name: String,
    },

    #[error("unused private function {name}")]
    UnusedPrivateFunction {
        #[label]
        location: Span,
        name: String,
    },

    #[error("unused variable {name}")]
    UnusedVariable {
        #[label]
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnknownRecordFieldSituation {
    /// This unknown record field is being called as a function. i.e. `record.field()`
    FunctionCall,
}
