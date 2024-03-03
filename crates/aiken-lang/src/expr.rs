use crate::{
    ast::{
        self, Annotation, Arg, AssignmentKind, BinOp, ByteArrayFormatPreference, CallArg, Curve,
        DataType, DataTypeKey, DefinitionLocation, IfBranch, Located, LogicalOpChainKind,
        ParsedCallArg, Pattern, RecordConstructorArg, RecordUpdateSpread, Span, TraceKind,
        TypedClause, TypedDataType, TypedRecordUpdateArg, UnOp, UntypedClause,
        UntypedRecordUpdateArg,
    },
    builtins::void,
    gen_uplc::builder::{
        check_replaceable_opaque_type, convert_opaque_type, lookup_data_type_by_tipo,
    },
    parser::token::Base,
    tipo::{ModuleValueConstructor, PatternConstructor, Type, TypeVar, ValueConstructor},
};
use indexmap::IndexMap;
use pallas::ledger::primitives::alonzo::{Constr, PlutusData};
use std::rc::Rc;
use uplc::{machine::value::from_pallas_bigint, KeyValuePairs};
use vec1::Vec1;

#[derive(Debug, Clone, PartialEq)]
pub enum TypedExpr {
    UInt {
        location: Span,
        tipo: Rc<Type>,
        value: String,
    },

    String {
        location: Span,
        tipo: Rc<Type>,
        value: String,
    },

    ByteArray {
        location: Span,
        tipo: Rc<Type>,
        bytes: Vec<u8>,
    },

    CurvePoint {
        location: Span,
        tipo: Rc<Type>,
        point: Box<Curve>,
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
        tipo: Rc<Type>,
        is_capture: bool,
        args: Vec<Arg<Rc<Type>>>,
        body: Box<Self>,
        return_annotation: Option<Annotation>,
    },

    List {
        location: Span,
        tipo: Rc<Type>,
        elements: Vec<Self>,
        tail: Option<Box<Self>>,
    },

    Call {
        location: Span,
        tipo: Rc<Type>,
        fun: Box<Self>,
        args: Vec<CallArg<Self>>,
    },

    BinOp {
        location: Span,
        tipo: Rc<Type>,
        name: BinOp,
        left: Box<Self>,
        right: Box<Self>,
    },

    Assignment {
        location: Span,
        tipo: Rc<Type>,
        value: Box<Self>,
        pattern: Pattern<PatternConstructor, Rc<Type>>,
        kind: AssignmentKind,
    },

    Trace {
        location: Span,
        tipo: Rc<Type>,
        then: Box<Self>,
        text: Box<Self>,
    },

    When {
        location: Span,
        tipo: Rc<Type>,
        subject: Box<Self>,
        clauses: Vec<TypedClause>,
    },

    If {
        location: Span,
        branches: Vec1<IfBranch<Self>>,
        final_else: Box<Self>,
        tipo: Rc<Type>,
    },

    RecordAccess {
        location: Span,
        tipo: Rc<Type>,
        label: String,
        index: u64,
        record: Box<Self>,
    },

    ModuleSelect {
        location: Span,
        tipo: Rc<Type>,
        label: String,
        module_name: String,
        module_alias: String,
        constructor: ModuleValueConstructor,
    },

    Tuple {
        location: Span,
        tipo: Rc<Type>,
        elems: Vec<Self>,
    },

    TupleIndex {
        location: Span,
        tipo: Rc<Type>,
        index: usize,
        tuple: Box<Self>,
    },

    ErrorTerm {
        location: Span,
        tipo: Rc<Type>,
    },

    RecordUpdate {
        location: Span,
        tipo: Rc<Type>,
        spread: Box<Self>,
        args: Vec<TypedRecordUpdateArg>,
    },

    UnOp {
        location: Span,
        value: Box<Self>,
        tipo: Rc<Type>,
        op: UnOp,
    },
}

impl TypedExpr {
    pub fn tipo(&self) -> Rc<Type> {
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
            | Self::RecordUpdate { tipo, .. }
            | Self::CurvePoint { tipo, .. } => tipo.clone(),
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
            | TypedExpr::RecordAccess { .. }
            | TypedExpr::CurvePoint { .. } => None,
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
            | Self::RecordUpdate { location, .. }
            | Self::CurvePoint { location, .. } => *location,

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
            | Self::RecordUpdate { location, .. }
            | Self::CurvePoint { location, .. } => *location,
        }
    }

    // This could be optimised in places to exit early if the first of a series
    // of expressions is after the byte index.
    pub fn find_node(&self, byte_index: usize) -> Option<Located<'_>> {
        if !self.location().contains(byte_index) {
            return None;
        }

        match self {
            TypedExpr::ErrorTerm { .. }
            | TypedExpr::Var { .. }
            | TypedExpr::UInt { .. }
            | TypedExpr::String { .. }
            | TypedExpr::ByteArray { .. }
            | TypedExpr::ModuleSelect { .. }
            | TypedExpr::CurvePoint { .. } => Some(Located::Expression(self)),

            TypedExpr::Trace { text, then, .. } => text
                .find_node(byte_index)
                .or_else(|| then.find_node(byte_index))
                .or(Some(Located::Expression(self))),

            TypedExpr::Pipeline { expressions, .. } | TypedExpr::Sequence { expressions, .. } => {
                expressions.iter().find_map(|e| e.find_node(byte_index))
            }

            TypedExpr::Fn { body, .. } => body
                .find_node(byte_index)
                .or(Some(Located::Expression(self))),

            TypedExpr::Tuple {
                elems: elements, ..
            }
            | TypedExpr::List { elements, .. } => elements
                .iter()
                .find_map(|e| e.find_node(byte_index))
                .or(Some(Located::Expression(self))),

            TypedExpr::Call { fun, args, .. } => args
                .iter()
                .find_map(|arg| arg.find_node(byte_index))
                .or_else(|| fun.find_node(byte_index))
                .or(Some(Located::Expression(self))),

            TypedExpr::BinOp { left, right, .. } => left
                .find_node(byte_index)
                .or_else(|| right.find_node(byte_index))
                .or(Some(Located::Expression(self))),

            TypedExpr::Assignment { value, pattern, .. } => pattern
                .find_node(byte_index, &value.tipo())
                .or_else(|| value.find_node(byte_index)),

            TypedExpr::When {
                subject, clauses, ..
            } => subject
                .find_node(byte_index)
                .or_else(|| {
                    clauses
                        .iter()
                        .find_map(|clause| clause.find_node(byte_index, &subject.tipo()))
                })
                .or(Some(Located::Expression(self))),

            TypedExpr::RecordAccess {
                record: expression, ..
            }
            | TypedExpr::TupleIndex {
                tuple: expression, ..
            } => expression
                .find_node(byte_index)
                .or(Some(Located::Expression(self))),

            TypedExpr::RecordUpdate { spread, args, .. } => args
                .iter()
                .find_map(|arg| arg.find_node(byte_index))
                .or_else(|| spread.find_node(byte_index))
                .or(Some(Located::Expression(self))),

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
                .or(Some(Located::Expression(self))),

            TypedExpr::UnOp { value, .. } => value
                .find_node(byte_index)
                .or(Some(Located::Expression(self))),
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

    CurvePoint {
        location: Span,
        point: Box<Curve>,
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

    LogicalOpChain {
        kind: LogicalOpChainKind,
        expressions: Vec<Self>,
        location: Span,
    },
}

pub const DEFAULT_TODO_STR: &str = "aiken::todo";

pub const DEFAULT_ERROR_STR: &str = "aiken::error";

impl UntypedExpr {
    // Reify some opaque 'PlutusData' into an 'UntypedExpr', using a Type annotation. We also need
    // an extra map to lookup record & enum constructor's names as they're completely erased when
    // in their PlutusData form, and the Type annotation only contains type name.
    //
    // The function performs some sanity check to ensure that the type does indeed somewhat
    // correspond to the data being given.
    pub fn reify(
        data_types: &IndexMap<&DataTypeKey, &TypedDataType>,
        data: PlutusData,
        tipo: &Type,
    ) -> Result<Self, String> {
        if let Type::Var { tipo } = tipo {
            if let TypeVar::Link { tipo } = &*tipo.borrow() {
                return UntypedExpr::reify(data_types, data, tipo);
            }
        }

        // NOTE: Opaque types are tricky. We can't tell from a type only if it is
        // opaque or not. We have to lookup its datatype definition.
        //
        // Also, we can't -- in theory -- peak into an opaque type. More so, if it
        // has a single constructor with a single argument, it is an zero-cost
        // wrapper. That means the underlying PlutusData has no residue of that
        // wrapper. So we have to manually reconstruct it before crawling further
        // down the type tree.
        if check_replaceable_opaque_type(tipo, data_types) {
            let DataType { name, .. } = lookup_data_type_by_tipo(data_types, tipo)
                .expect("Type just disappeared from known types? {tipo:?}");

            let inner_type = convert_opaque_type(&tipo.clone().into(), data_types, false);

            let value = UntypedExpr::reify(data_types, data, &inner_type)?;

            return Ok(UntypedExpr::Call {
                location: Span::empty(),
                arguments: vec![CallArg {
                    label: None,
                    location: Span::empty(),
                    value,
                }],
                fun: Box::new(UntypedExpr::Var {
                    name,
                    location: Span::empty(),
                }),
            });
        }

        match data {
            PlutusData::BigInt(ref i) => Ok(UntypedExpr::UInt {
                location: Span::empty(),
                base: Base::Decimal {
                    numeric_underscore: false,
                },
                value: from_pallas_bigint(i).to_string(),
            }),
            PlutusData::BoundedBytes(bytes) => Ok(UntypedExpr::ByteArray {
                location: Span::empty(),
                bytes: bytes.into(),
                preferred_format: ByteArrayFormatPreference::HexadecimalString,
            }),
            PlutusData::Array(args) => {
                match tipo {
                    Type::App {
                        module,
                        name,
                        args: type_args,
                        ..
                    } if module.is_empty() && name.as_str() == "List" => {
                        if let [inner] = &type_args[..] {
                            Ok(UntypedExpr::List {
                                location: Span::empty(),
                                elements: args
                                    .into_iter()
                                    .map(|arg| UntypedExpr::reify(data_types, arg, inner))
                                    .collect::<Result<Vec<_>, _>>()?,
                                tail: None,
                            })
                        } else {
                            Err("invalid List type annotation: the list has multiple type-parameters.".to_string())
                        }
                    }
                    Type::Tuple { elems } => Ok(UntypedExpr::Tuple {
                        location: Span::empty(),
                        elems: args
                            .into_iter()
                            .zip(elems)
                            .map(|(arg, arg_type)| UntypedExpr::reify(data_types, arg, arg_type))
                            .collect::<Result<Vec<_>, _>>()?,
                    }),
                    _ => Err(format!(
                        "invalid type annotation. expected List but got: {tipo:?}"
                    )),
                }
            }

            PlutusData::Constr(Constr {
                tag,
                any_constructor,
                fields,
            }) => {
                let ix = if tag == 102 {
                    any_constructor.unwrap() as usize
                } else if tag < 128 {
                    tag as usize - 121
                } else {
                    tag as usize - 1280 + 7
                };

                if let Type::App { .. } = tipo {
                    if let Some(DataType { constructors, .. }) =
                        lookup_data_type_by_tipo(data_types, tipo)
                    {
                        let constructor = &constructors[ix];

                        return if fields.is_empty() {
                            Ok(UntypedExpr::Var {
                                location: Span::empty(),
                                name: constructor.name.to_string(),
                            })
                        } else {
                            let arguments = fields
                                .into_iter()
                                .zip(constructor.arguments.iter())
                                .map(
                                    |(
                                        field,
                                        RecordConstructorArg {
                                            ref label,
                                            ref tipo,
                                            ..
                                        },
                                    )| {
                                        UntypedExpr::reify(data_types, field, tipo).map(|value| {
                                            CallArg {
                                                label: label.clone(),
                                                location: Span::empty(),
                                                value,
                                            }
                                        })
                                    },
                                )
                                .collect::<Result<Vec<_>, _>>()?;

                            Ok(UntypedExpr::Call {
                                location: Span::empty(),
                                arguments,
                                fun: Box::new(UntypedExpr::Var {
                                    name: constructor.name.to_string(),
                                    location: Span::empty(),
                                }),
                            })
                        };
                    }
                }

                Err(format!(
                    "invalid type annotation {tipo:?} for constructor: {tag:?} with {fields:?}"
                ))
            }

            PlutusData::Map(indef_or_def) => {
                let kvs = match indef_or_def {
                    KeyValuePairs::Def(kvs) => kvs,
                    KeyValuePairs::Indef(kvs) => kvs,
                };

                UntypedExpr::reify(
                    data_types,
                    PlutusData::Array(
                        kvs.into_iter()
                            .map(|(k, v)| PlutusData::Array(vec![k, v]))
                            .collect(),
                    ),
                    tipo,
                )
            }
        }
    }

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
                        doc: None,
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
            | Self::LogicalOpChain { location, .. }
            | Self::If { location, .. }
            | Self::CurvePoint { location, .. } => *location,
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
