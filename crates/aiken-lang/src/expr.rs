use std::sync::Arc;

use vec1::Vec1;

use crate::{
    ast::{
        self, Annotation, Arg, AssignmentKind, BinOp, ByteArrayFormatPreference, CallArg,
        DefinitionLocation, IfBranch, ParsedCallArg, Pattern, RecordUpdateSpread, Span, TraceKind,
        TypedClause, TypedRecordUpdateArg, UnOp, UntypedClause, UntypedRecordUpdateArg,
    },
    builtins::void,
    parser::token::Base,
    tipo::{ModuleValueConstructor, PatternConstructor, Type, ValueConstructor},
};

#[derive(Debug, Clone, PartialEq)]
pub enum TypedExpr {
    UInt {
        location: Span,
        tipo: Arc<Type>,
        value: String,
    },

    String {
        location: Span,
        tipo: Arc<Type>,
        value: String,
    },

    ByteArray {
        location: Span,
        tipo: Arc<Type>,
        bytes: Vec<u8>,
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

    Trace {
        location: Span,
        tipo: Arc<Type>,
        then: Box<Self>,
        text: Box<Self>,
    },

    When {
        location: Span,
        tipo: Arc<Type>,
        subject: Box<Self>,
        clauses: Vec<TypedClause>,
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
        index: usize,
        tuple: Box<Self>,
    },

    ErrorTerm {
        location: Span,
        tipo: Arc<Type>,
    },

    RecordUpdate {
        location: Span,
        tipo: Arc<Type>,
        spread: Box<Self>,
        args: Vec<TypedRecordUpdateArg>,
    },

    UnOp {
        location: Span,
        value: Box<Self>,
        tipo: Arc<Type>,
        op: UnOp,
    },
}

impl TypedExpr {
    pub fn tipo(&self) -> Arc<Type> {
        match self {
            Self::Var { constructor, .. } => constructor.tipo.clone(),
            Self::Trace { then, .. } => then.tipo(),
            Self::Fn { tipo, .. }
            | Self::UInt { tipo, .. }
            | Self::ErrorTerm { tipo, .. }
            | Self::When { tipo, .. }
            | Self::List { tipo, .. }
            | Self::Call { tipo, .. }
            | Self::If { tipo, .. }
            | Self::UnOp { tipo, .. }
            | Self::BinOp { tipo, .. }
            | Self::Tuple { tipo, .. }
            | Self::String { tipo, .. }
            | Self::ByteArray { tipo, .. }
            | Self::TupleIndex { tipo, .. }
            | Self::Assignment { tipo, .. }
            | Self::ModuleSelect { tipo, .. }
            | Self::RecordAccess { tipo, .. }
            | Self::RecordUpdate { tipo, .. } => tipo.clone(),
            Self::Pipeline { expressions, .. } | Self::Sequence { expressions, .. } => {
                expressions.last().map(TypedExpr::tipo).unwrap_or_else(void)
            }
        }
    }

    pub fn is_literal(&self) -> bool {
        matches!(
            self,
            Self::UInt { .. }
                | Self::List { .. }
                | Self::Tuple { .. }
                | Self::String { .. }
                | Self::ByteArray { .. }
        )
    }

    /// Returns `true` if the typed expr is [`Assignment`].
    pub fn is_assignment(&self) -> bool {
        matches!(self, Self::Assignment { .. })
    }

    pub fn definition_location(&self) -> Option<DefinitionLocation<'_>> {
        match self {
            TypedExpr::Fn { .. }
            | TypedExpr::UInt { .. }
            | TypedExpr::Trace { .. }
            | TypedExpr::List { .. }
            | TypedExpr::Call { .. }
            | TypedExpr::When { .. }
            | TypedExpr::ErrorTerm { .. }
            | TypedExpr::BinOp { .. }
            | TypedExpr::Tuple { .. }
            | TypedExpr::UnOp { .. }
            | TypedExpr::String { .. }
            | TypedExpr::Sequence { .. }
            | TypedExpr::Pipeline { .. }
            | TypedExpr::ByteArray { .. }
            | TypedExpr::Assignment { .. }
            | TypedExpr::TupleIndex { .. }
            | TypedExpr::RecordAccess { .. } => None,
            TypedExpr::If { .. } => None,

            // TODO: test
            // TODO: definition
            TypedExpr::RecordUpdate { .. } => None,

            // TODO: test
            TypedExpr::ModuleSelect {
                module_name,
                constructor,
                ..
            } => Some(DefinitionLocation {
                module: Some(module_name.as_str()),
                span: constructor.location(),
            }),

            // TODO: test
            TypedExpr::Var { constructor, .. } => Some(constructor.definition_location()),
        }
    }

    pub fn type_defining_location(&self) -> Span {
        match self {
            Self::Fn { location, .. }
            | Self::UInt { location, .. }
            | Self::Var { location, .. }
            | Self::Trace { location, .. }
            | Self::ErrorTerm { location, .. }
            | Self::When { location, .. }
            | Self::Call { location, .. }
            | Self::List { location, .. }
            | Self::BinOp { location, .. }
            | Self::Tuple { location, .. }
            | Self::String { location, .. }
            | Self::UnOp { location, .. }
            | Self::Pipeline { location, .. }
            | Self::ByteArray { location, .. }
            | Self::Assignment { location, .. }
            | Self::TupleIndex { location, .. }
            | Self::ModuleSelect { location, .. }
            | Self::RecordAccess { location, .. }
            | Self::RecordUpdate { location, .. } => *location,

            Self::If { branches, .. } => branches.first().body.type_defining_location(),

            Self::Sequence {
                expressions,
                location,
                ..
            } => expressions
                .last()
                .map(TypedExpr::location)
                .unwrap_or(*location),
        }
    }

    pub fn location(&self) -> Span {
        match self {
            Self::Fn { location, .. }
            | Self::UInt { location, .. }
            | Self::Trace { location, .. }
            | Self::Var { location, .. }
            | Self::ErrorTerm { location, .. }
            | Self::When { location, .. }
            | Self::Call { location, .. }
            | Self::If { location, .. }
            | Self::List { location, .. }
            | Self::BinOp { location, .. }
            | Self::Tuple { location, .. }
            | Self::String { location, .. }
            | Self::UnOp { location, .. }
            | Self::Sequence { location, .. }
            | Self::Pipeline { location, .. }
            | Self::ByteArray { location, .. }
            | Self::Assignment { location, .. }
            | Self::TupleIndex { location, .. }
            | Self::ModuleSelect { location, .. }
            | Self::RecordAccess { location, .. }
            | Self::RecordUpdate { location, .. } => *location,
        }
    }

    // This could be optimised in places to exit early if the first of a series
    // of expressions is after the byte index.
    pub fn find_node(&self, byte_index: usize) -> Option<&Self> {
        if !self.location().contains(byte_index) {
            return None;
        }

        match self {
            TypedExpr::ErrorTerm { .. }
            | TypedExpr::Var { .. }
            | TypedExpr::UInt { .. }
            | TypedExpr::String { .. }
            | TypedExpr::ByteArray { .. }
            | TypedExpr::ModuleSelect { .. } => Some(self),

            TypedExpr::Trace { text, then, .. } => text
                .find_node(byte_index)
                .or_else(|| then.find_node(byte_index))
                .or(Some(self)),

            TypedExpr::Pipeline { expressions, .. } | TypedExpr::Sequence { expressions, .. } => {
                expressions.iter().find_map(|e| e.find_node(byte_index))
            }

            TypedExpr::Fn { body, .. } => body.find_node(byte_index).or(Some(self)),

            TypedExpr::Tuple {
                elems: elements, ..
            }
            | TypedExpr::List { elements, .. } => elements
                .iter()
                .find_map(|e| e.find_node(byte_index))
                .or(Some(self)),

            TypedExpr::Call { fun, args, .. } => args
                .iter()
                .find_map(|arg| arg.find_node(byte_index))
                .or_else(|| fun.find_node(byte_index))
                .or(Some(self)),

            TypedExpr::BinOp { left, right, .. } => left
                .find_node(byte_index)
                .or_else(|| right.find_node(byte_index)),

            TypedExpr::Assignment { value, .. } => value.find_node(byte_index),

            TypedExpr::When {
                subject, clauses, ..
            } => subject
                .find_node(byte_index)
                .or_else(|| {
                    clauses
                        .iter()
                        .find_map(|clause| clause.find_node(byte_index))
                })
                .or(Some(self)),

            TypedExpr::RecordAccess {
                record: expression, ..
            }
            | TypedExpr::TupleIndex {
                tuple: expression, ..
            } => expression.find_node(byte_index).or(Some(self)),

            TypedExpr::RecordUpdate { spread, args, .. } => args
                .iter()
                .find_map(|arg| arg.find_node(byte_index))
                .or_else(|| spread.find_node(byte_index))
                .or(Some(self)),

            TypedExpr::If {
                branches,
                final_else,
                ..
            } => branches
                .iter()
                .find_map(|branch| {
                    branch
                        .condition
                        .find_node(byte_index)
                        .or_else(|| branch.body.find_node(byte_index))
                })
                .or_else(|| final_else.find_node(byte_index))
                .or(Some(self)),

            TypedExpr::UnOp { value, .. } => value.find_node(byte_index).or(Some(self)),
        }
    }
}

// Represent how a function was written so that we can format it back.
#[derive(Debug, Clone, PartialEq, Copy)]
pub enum FnStyle {
    Plain,
    Capture,
    BinOp(BinOp),
}

#[derive(Debug, Clone, PartialEq)]
pub enum UntypedExpr {
    UInt {
        location: Span,
        value: String,
        base: Base,
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
        fn_style: FnStyle,
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
        arguments: Vec<CallArg<Self>>,
        fun: Box<Self>,
        location: Span,
    },

    BinOp {
        location: Span,
        name: BinOp,
        left: Box<Self>,
        right: Box<Self>,
    },

    ByteArray {
        location: Span,
        bytes: Vec<u8>,
        preferred_format: ByteArrayFormatPreference,
    },

    PipeLine {
        expressions: Vec1<Self>,
        one_liner: bool,
    },

    Assignment {
        location: Span,
        value: Box<Self>,
        pattern: Pattern<(), ()>,
        kind: AssignmentKind,
        annotation: Option<Annotation>,
    },

    Trace {
        kind: TraceKind,
        location: Span,
        then: Box<Self>,
        text: Box<Self>,
    },

    TraceIfFalse {
        location: Span,
        value: Box<Self>,
    },

    When {
        location: Span,
        subject: Box<Self>,
        clauses: Vec<UntypedClause>,
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
        index: usize,
        tuple: Box<Self>,
    },

    ErrorTerm {
        location: Span,
    },

    RecordUpdate {
        location: Span,
        constructor: Box<Self>,
        spread: RecordUpdateSpread,
        arguments: Vec<UntypedRecordUpdateArg>,
    },

    UnOp {
        op: UnOp,
        location: Span,
        value: Box<Self>,
    },
}

pub const DEFAULT_TODO_STR: &str = "aiken::todo";

pub const DEFAULT_ERROR_STR: &str = "aiken::error";

impl UntypedExpr {
    pub fn todo(reason: Option<Self>, location: Span) -> Self {
        UntypedExpr::Trace {
            location,
            kind: TraceKind::Todo,
            then: Box::new(UntypedExpr::ErrorTerm { location }),
            text: Box::new(reason.unwrap_or_else(|| UntypedExpr::String {
                location,
                value: DEFAULT_TODO_STR.to_string(),
            })),
        }
    }

    pub fn fail(reason: Option<Self>, location: Span) -> Self {
        if let Some(reason) = reason {
            UntypedExpr::Trace {
                location,
                kind: TraceKind::Error,
                then: Box::new(UntypedExpr::ErrorTerm { location }),
                text: Box::new(reason),
            }
        } else {
            UntypedExpr::ErrorTerm { location }
        }
    }

    pub fn tuple_index(self, index: usize, location: Span) -> Self {
        UntypedExpr::TupleIndex {
            location: self.location().union(location),
            index,
            tuple: Box::new(self),
        }
    }

    pub fn field_access(self, label: String, location: Span) -> Self {
        UntypedExpr::FieldAccess {
            location: self.location().union(location),
            label,
            container: Box::new(self),
        }
    }

    pub fn call(self, args: Vec<ParsedCallArg>, location: Span) -> Self {
        let mut holes = Vec::new();

        let args = args
            .into_iter()
            .enumerate()
            .map(|(index, a)| match a {
                CallArg {
                    value: Some(value),
                    label,
                    location,
                } => CallArg {
                    value,
                    label,
                    location,
                },
                CallArg {
                    value: None,
                    label,
                    location,
                } => {
                    let name = format!("{}__{index}", ast::CAPTURE_VARIABLE);

                    holes.push(ast::Arg {
                        location: Span::empty(),
                        annotation: None,
                        arg_name: ast::ArgName::Named {
                            label: name.clone(),
                            name,
                            location: Span::empty(),
                            is_validator_param: false,
                        },
                        tipo: (),
                    });

                    ast::CallArg {
                        label,
                        location,
                        value: UntypedExpr::Var {
                            location,
                            name: format!("{}__{index}", ast::CAPTURE_VARIABLE),
                        },
                    }
                }
            })
            .collect();

        let call = UntypedExpr::Call {
            location: self.location().union(location),
            fun: Box::new(self),
            arguments: args,
        };

        if holes.is_empty() {
            call
        } else {
            UntypedExpr::Fn {
                location: call.location(),
                fn_style: FnStyle::Capture,
                arguments: holes,
                body: Box::new(call),
                return_annotation: None,
            }
        }
    }

    pub fn append_in_sequence(self, next: Self) -> Self {
        let location = Span {
            start: self.location().start,
            end: next.location().end,
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
            Self::PipeLine { expressions, .. } => expressions.last().location(),
            Self::Trace { then, .. } => then.location(),
            Self::TraceIfFalse { location, .. }
            | Self::Fn { location, .. }
            | Self::Var { location, .. }
            | Self::UInt { location, .. }
            | Self::ErrorTerm { location, .. }
            | Self::When { location, .. }
            | Self::Call { location, .. }
            | Self::List { location, .. }
            | Self::ByteArray { location, .. }
            | Self::BinOp { location, .. }
            | Self::Tuple { location, .. }
            | Self::String { location, .. }
            | Self::Assignment { location, .. }
            | Self::TupleIndex { location, .. }
            | Self::FieldAccess { location, .. }
            | Self::RecordUpdate { location, .. }
            | Self::UnOp { location, .. }
            | Self::If { location, .. } => *location,
            Self::Sequence {
                location,
                expressions,
                ..
            } => expressions.last().map(Self::location).unwrap_or(*location),
        }
    }

    pub fn start_byte_index(&self) -> usize {
        match self {
            Self::Sequence {
                expressions,
                location,
                ..
            } => expressions
                .first()
                .map(|e| e.start_byte_index())
                .unwrap_or(location.start),
            Self::PipeLine { expressions, .. } => expressions.first().start_byte_index(),
            Self::Trace { location, .. } | Self::Assignment { location, .. } => location.start,
            _ => self.location().start,
        }
    }

    pub fn binop_precedence(&self) -> u8 {
        match self {
            Self::BinOp { name, .. } => name.precedence(),
            Self::PipeLine { .. } => 0,
            _ => std::u8::MAX,
        }
    }

    pub fn is_simple_constant(&self) -> bool {
        matches!(
            self,
            Self::String { .. } | Self::UInt { .. } | Self::ByteArray { .. }
        )
    }
}
