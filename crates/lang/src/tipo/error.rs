use std::{collections::HashMap, sync::Arc};

use miette::Diagnostic;

use crate::ast::{BinOp, Span, TodoKind};

use super::Type;

#[derive(Debug, thiserror::Error, Diagnostic)]
pub enum Error {
    #[error("Duplicate argument\n\n{label}\n")]
    #[diagnostic(help("Try renaming it"))]
    DuplicateArgument {
        #[label]
        location: Span,
        label: String,
    },

    #[error("Duplicate const\n\n{name}\n")]
    #[diagnostic(help("Try renaming it"))]
    DuplicateConstName {
        #[label]
        location: Span,
        #[label]
        previous_location: Span,
        name: String,
    },

    #[error("Duplicate import\n\n{name}\n")]
    #[diagnostic(help("Try renaming it"))]
    DuplicateImport {
        #[label]
        location: Span,
        #[label]
        previous_location: Span,
        name: String,
    },

    #[error("Duplicate field\n\n{label}\n")]
    #[diagnostic(help("Try renaming it"))]
    DuplicateField {
        #[label]
        location: Span,
        label: String,
    },

    #[error("Duplicate name\n\n{name}\n")]
    #[diagnostic(help("Try renaming it"))]
    DuplicateName {
        #[label]
        location: Span,
        #[label]
        previous_location: Span,
        name: String,
    },

    #[error("Duplicate type name\n\n{name}\n")]
    #[diagnostic(help("Try renaming it"))]
    DuplicateTypeName {
        #[label]
        location: Span,
        #[label]
        previous_location: Span,
        name: String,
    },

    #[error("Incorrect arity\n\nExpected\n\n    {expected}\n\nGiven\n\n    {given}\n")]
    IncorrectArity {
        #[label]
        location: Span,
        expected: usize,
        given: usize,
        labels: Vec<String>,
    },

    #[error("Incorrect number of clause patterns\n\nExpected\n\n{expected}\n\nGiven\n\n{given}\n")]
    IncorrectNumClausePatterns {
        #[label]
        location: Span,
        expected: usize,
        given: usize,
    },

    #[error("Incorrect type arity for `{name}`\n\nExpected\n\n{expected}\n\nGiven\n\n{given}\n")]
    IncorrectTypeArity {
        #[label]
        location: Span,
        name: String,
        expected: usize,
        given: usize,
    },

    #[error("Non-exhaustive pattern match\n")]
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

    #[error("Module\n\n{name}\n\ncontains keyword\n\n{keyword}\n")]
    KeywordInModuleName { name: String, keyword: String },

    #[error("Clause guard {name} is not local\n")]
    NonLocalClauseGuardVariable {
        #[label]
        location: Span,
        name: String,
    },

    #[error("Positional argument after labeled\n")]
    PositionalArgumentAfterLabeled {
        #[label]
        location: Span,
    },

    #[error("Private type leaked\n")]
    PrivateTypeLeak {
        #[label]
        location: Span,
        leaked: Type,
    },

    #[error("Record access unknown type\n")]
    RecordAccessUnknownType {
        #[label]
        location: Span,
    },

    #[error("Record update invalid constructor\n")]
    RecordUpdateInvalidConstructor {
        #[label]
        location: Span,
    },

    #[error("{name} is a reserved module name\n")]
    ReservedModuleName { name: String },

    #[error("Unexpected labeled argument\n\n{label}\n")]
    UnexpectedLabeledArg {
        #[label]
        location: Span,
        label: String,
    },

    #[error("Unexpected type hole\n")]
    UnexpectedTypeHole {
        #[label]
        location: Span,
    },

    #[error("Unknown labels\n")]
    UnknownLabels {
        unknown: Vec<(String, Span)>,
        valid: Vec<String>,
        supplied: Vec<String>,
    },

    #[error("Unknown module\n\n{name}\n")]
    UnknownModule {
        #[label]
        location: Span,
        name: String,
        imported_modules: Vec<String>,
    },

    #[error("Unknown module field\n\n{name}\n\nin module\n\n{module_name}\n")]
    UnknownModuleField {
        location: Span,
        name: String,
        module_name: String,
        value_constructors: Vec<String>,
        type_constructors: Vec<String>,
    },

    #[error("Unknown module value\n\n{name}\n")]
    UnknownModuleValue {
        #[label]
        location: Span,
        name: String,
        module_name: String,
        value_constructors: Vec<String>,
    },

    #[error("Unknown type\n\n{name}\n\nin module\n\n{module_name}\n")]
    UnknownModuleType {
        #[label]
        location: Span,
        name: String,
        module_name: String,
        type_constructors: Vec<String>,
    },

    #[error("Unknown record field\n\n{label}\n")]
    UnknownRecordField {
        #[label]
        location: Span,
        typ: Arc<Type>,
        label: String,
        fields: Vec<String>,
        situation: Option<UnknownRecordFieldSituation>,
    },

    #[error("Unknown type\n\n{name}\n")]
    UnknownType {
        #[label]
        location: Span,
        name: String,
        types: Vec<String>,
    },

    #[error("Unknown variable\n\n    {name}\n")]
    UnknownVariable {
        #[label]
        location: Span,
        name: String,
        variables: Vec<String>,
    },

    #[error("Unnecessary spread operator\n")]
    UnnecessarySpreadOperator {
        #[label]
        location: Span,
        arity: usize,
    },

    #[error("Cannot update a type with multiple constructors\n")]
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

    #[error("Recursive type detected\n")]
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
    #[error("Todo\n")]
    Todo {
        kind: TodoKind,
        #[label]
        location: Span,
        tipo: Arc<Type>,
    },

    #[error("Implicitly discarded result\n")]
    ImplicitlyDiscardedResult {
        #[label]
        location: Span,
    },

    #[error("Unused literal\n")]
    UnusedLiteral {
        #[label]
        location: Span,
    },

    #[error("Record update with no fields\n")]
    NoFieldsRecordUpdate {
        #[label]
        location: Span,
    },

    #[error("Record update using all fields\n")]
    AllFieldsRecordUpdate {
        #[label]
        location: Span,
    },

    #[error("Unused type {name}\n")]
    UnusedType {
        #[label]
        location: Span,
        imported: bool,
        name: String,
    },

    #[error("Unused constructor {name}\n")]
    UnusedConstructor {
        #[label]
        location: Span,
        imported: bool,
        name: String,
    },

    #[error("Unused imported value {name}\n")]
    UnusedImportedValue {
        #[label]
        location: Span,
        name: String,
    },

    #[error("Unused imported module {name}\n")]
    UnusedImportedModule {
        #[label]
        location: Span,
        name: String,
    },

    #[error("Unused private module constant {name}\n")]
    UnusedPrivateModuleConstant {
        #[label]
        location: Span,
        name: String,
    },

    #[error("Unused private function {name}\n")]
    UnusedPrivateFunction {
        #[label]
        location: Span,
        name: String,
    },

    #[error("Unused variable {name}\n")]
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
