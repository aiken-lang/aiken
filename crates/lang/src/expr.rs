use std::sync::Arc;

use vec1::Vec1;

use crate::{
    ast::{
        Annotation, Arg, AssignmentKind, BinOp, CallArg, Clause, IfBranch, Pattern,
        RecordUpdateSpread, Span, TodoKind, TypedRecordUpdateArg, UntypedRecordUpdateArg,
    },
    tipo::{ModuleValueConstructor, PatternConstructor, Type, ValueConstructor},
};

pub enum TypedExpr {
    Int {
        location: Span,
        tipo: Arc<Type>,
        value: String,
    },

    Float {
        location: Span,
        tipo: Arc<Type>,
        value: String,
    },

    String {
        location: Span,
        tipo: Arc<Type>,
        value: String,
    },

    Sequence {
        location: Span,
        expressions: Vec<Self>,
    },

    /// A chain of pipe expressions.
    /// By this point the type checker has expanded it into a series of
    /// assignments and function calls, but we still have a Pipeline AST node as
    /// even though it is identical to `Sequence` we want to use different
    /// locations when showing it in error messages, etc.
    Pipeline {
        location: Span,
        expressions: Vec<Self>,
    },

    Var {
        location: Span,
        constructor: ValueConstructor,
        name: String,
    },

    Fn {
        location: Span,
        tipo: Arc<Type>,
        is_capture: bool,
        args: Vec<Arg<Arc<Type>>>,
        body: Box<Self>,
        return_annotation: Option<Annotation>,
    },

    List {
        location: Span,
        tipo: Arc<Type>,
        elements: Vec<Self>,
        tail: Option<Box<Self>>,
    },

    Call {
        location: Span,
        tipo: Arc<Type>,
        fun: Box<Self>,
        args: Vec<CallArg<Self>>,
    },

    BinOp {
        location: Span,
        tipo: Arc<Type>,
        name: BinOp,
        left: Box<Self>,
        right: Box<Self>,
    },

    Assignment {
        location: Span,
        tipo: Arc<Type>,
        value: Box<Self>,
        pattern: Pattern<PatternConstructor, Arc<Type>>,
        kind: AssignmentKind,
    },

    Try {
        location: Span,
        tipo: Arc<Type>,
        value: Box<Self>,
        then: Box<Self>,
        pattern: Pattern<PatternConstructor, Arc<Type>>,
    },

    When {
        location: Span,
        tipo: Arc<Type>,
        subjects: Vec<Self>,
        clauses: Vec<Clause<Self, PatternConstructor, Arc<Type>, String>>,
    },

    If {
        location: Span,
        branches: Vec1<IfBranch<Self>>,
        final_else: Box<Self>,
        tipo: Arc<Type>,
    },

    RecordAccess {
        location: Span,
        tipo: Arc<Type>,
        label: String,
        index: u64,
        record: Box<Self>,
    },

    ModuleSelect {
        location: Span,
        tipo: Arc<Type>,
        label: String,
        module_name: String,
        module_alias: String,
        constructor: ModuleValueConstructor,
    },

    Tuple {
        location: Span,
        tipo: Arc<Type>,
        elems: Vec<Self>,
    },

    TupleIndex {
        location: Span,
        tipo: Arc<Type>,
        index: u64,
        tuple: Box<Self>,
    },

    Todo {
        location: Span,
        label: Option<String>,
        tipo: Arc<Type>,
    },

    RecordUpdate {
        location: Span,
        tipo: Arc<Type>,
        spread: Box<Self>,
        args: Vec<TypedRecordUpdateArg>,
    },

    Negate {
        location: Span,
        value: Box<Self>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum UntypedExpr {
    Int {
        location: Span,
        value: String,
    },

    Float {
        location: Span,
        value: String,
    },

    String {
        location: Span,
        value: String,
    },

    Sequence {
        location: Span,
        expressions: Vec<Self>,
    },

    Var {
        location: Span,
        name: String,
    },

    Fn {
        location: Span,
        is_capture: bool,
        arguments: Vec<Arg<()>>,
        body: Box<Self>,
        return_annotation: Option<Annotation>,
    },

    List {
        location: Span,
        elements: Vec<Self>,
        tail: Option<Box<Self>>,
    },

    Call {
        location: Span,
        fun: Box<Self>,
        arguments: Vec<CallArg<Self>>,
    },

    BinOp {
        location: Span,
        name: BinOp,
        left: Box<Self>,
        right: Box<Self>,
    },

    PipeLine {
        expressions: Vec1<Self>,
    },

    Assignment {
        location: Span,
        value: Box<Self>,
        pattern: Pattern<(), ()>,
        kind: AssignmentKind,
        annotation: Option<Annotation>,
    },

    Try {
        location: Span,
        value: Box<Self>,
        pattern: Pattern<(), ()>,
        then: Box<Self>,
        annotation: Option<Annotation>,
    },

    When {
        location: Span,
        subjects: Vec<Self>,
        clauses: Vec<Clause<Self, (), (), ()>>,
    },

    If {
        location: Span,
        branches: Vec1<IfBranch<Self>>,
        final_else: Box<Self>,
    },

    FieldAccess {
        location: Span,
        label: String,
        container: Box<Self>,
    },

    Tuple {
        location: Span,
        elems: Vec<Self>,
    },

    TupleIndex {
        location: Span,
        index: u64,
        tuple: Box<Self>,
    },

    Todo {
        kind: TodoKind,
        location: Span,
        label: Option<String>,
    },

    RecordUpdate {
        location: Span,
        constructor: Box<Self>,
        spread: RecordUpdateSpread,
        arguments: Vec<UntypedRecordUpdateArg>,
    },

    Negate {
        location: Span,
        value: Box<Self>,
    },
}

impl UntypedExpr {
    pub fn append_in_sequence(self, next: Self) -> Self {
        let location = Span {
            start: self.location().start,
            end: next.location().end,
            ..self.location()
        };

        match (self.clone(), next.clone()) {
            (
                Self::Sequence {
                    expressions: mut current_expressions,
                    ..
                },
                Self::Sequence {
                    expressions: mut next_expressions,
                    ..
                },
            ) => {
                current_expressions.append(&mut next_expressions);

                Self::Sequence {
                    location,
                    expressions: current_expressions,
                }
            }
            (
                _,
                Self::Sequence {
                    expressions: mut next_expressions,
                    ..
                },
            ) => {
                let mut current_expressions = vec![self];

                current_expressions.append(&mut next_expressions);

                Self::Sequence {
                    location,
                    expressions: current_expressions,
                }
            }

            (_, _) => Self::Sequence {
                location,
                expressions: vec![self, next],
            },
        }
    }

    pub fn location(&self) -> Span {
        match self {
            Self::Try { then, .. } => then.location(),
            Self::PipeLine { expressions, .. } => expressions.last().location(),
            Self::Fn { location, .. }
            | Self::Var { location, .. }
            | Self::Int { location, .. }
            | Self::Todo { location, .. }
            | Self::When { location, .. }
            | Self::Call { location, .. }
            | Self::List { location, .. }
            | Self::Float { location, .. }
            | Self::BinOp { location, .. }
            | Self::Tuple { location, .. }
            | Self::String { location, .. }
            | Self::Assignment { location, .. }
            | Self::TupleIndex { location, .. }
            | Self::FieldAccess { location, .. }
            | Self::RecordUpdate { location, .. }
            | Self::Negate { location, .. }
            | Self::If { location, .. } => *location,
            Self::Sequence {
                location,
                expressions,
                ..
            } => expressions.last().map(Self::location).unwrap_or(*location),
        }
    }
}
