use super::analysis_support::*;
use super::fuzzer_analysis::*;
use super::transition_prop::{LocalBinding, ProjectionPathItem};
use super::*;

/// A simplified intermediate representation of a step function body, suitable
/// for shallow translation into Lean 4 definitions. Only the constructs
/// commonly found in state-machine step functions are represented; everything
/// else becomes `Opaque`.
#[non_exhaustive]
#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
pub enum ShallowIr {
    /// Integer / UInt / Bool / ByteArray / String literal
    Const(ShallowConst),
    /// Local variable reference that is not known to be in Lean scope; verifier lowers it conservatively.
    Var { name: String, ty: ShallowIrType },
    /// Variable introduced by a transition-proposition existential and therefore in Lean scope.
    BoundVar { name: String, ty: ShallowIrType },
    /// Let binding: `let name = value; body`
    Let {
        name: String,
        value: Box<ShallowIr>,
        body: Box<ShallowIr>,
    },
    /// If-then-else
    If {
        cond: Box<ShallowIr>,
        then_branch: Box<ShallowIr>,
        else_branch: Box<ShallowIr>,
    },
    /// Pattern match (`when ... is`)
    Match {
        subject: Box<ShallowIr>,
        arms: Vec<ShallowIrArm>,
    },
    /// ADT constructor application
    Construct {
        module: String,
        constructor: String,
        tag: u64,
        fields: Vec<ShallowIr>,
    },
    /// Record field access
    FieldAccess {
        record: Box<ShallowIr>,
        index: u64,
        label: String,
        ty: ShallowIrType,
        kind: ShallowFieldAccessKind,
    },
    /// Record update `{ rec | field: val, ... }` represented as a full constructor update.
    RecordUpdate {
        record: Box<ShallowIr>,
        tag: u64,
        field_count: usize,
        updates: Vec<ShallowIrRecordUpdate>,
    },
    /// Binary operator
    BinOp {
        op: ShallowBinOp,
        left: Box<ShallowIr>,
        right: Box<ShallowIr>,
    },
    /// Tuple / pair literal
    Tuple(Vec<ShallowIr>),
    /// List literal. `tail` preserves spread lists like `[head, ..tail]`.
    ListLit {
        elements: Vec<ShallowIr>,
        tail: Option<Box<ShallowIr>>,
        ty: ShallowIrType,
    },
    /// A fuzzer combinator call — becomes ∃-bound variable in Lean.
    /// `kind` is the type of the generated value; `lo`/`hi` are bounds
    /// for integer fuzzers (None = unbounded).
    FuzzExistential {
        kind: ShallowIrType,
        lo: Option<i64>,
        hi: Option<i64>,
    },
    /// Subterm the translator does not recognise. Sound: becomes ∃ x : ty in Lean.
    ///
    /// `reason` is a free-form diagnostic message surfaced in audit logs and
    /// fallback error messages. `code` is an optional typed dispatch payload:
    /// when `Some(...)`, the verify-side pipeline recognises the variant and
    /// emits a hard, non-skippable error rather than letting the downstream
    /// emitter widen the marker-bearing `Opaque` to a fresh existential.
    /// `None` means the Opaque is a generic "unrecognised shape" that the
    /// fallback machinery can absorb.
    Opaque {
        ty: ShallowIrType,
        reason: String,
        #[serde(default)]
        #[serde(skip_serializing_if = "Option::is_none")]
        code: Option<OpaqueCode>,
    },
}

/// Typed dispatch payload for `ShallowIr::Opaque` markers that must reach the
/// verify-side pipeline as hard, non-skippable errors. Each variant maps to a
/// stable error code in `crates/aiken-project/src/verify/error_catalogue.rs`.
///
/// Producers populate this alongside the human-readable `reason` string;
/// consumers in `aiken-project::verify` match on the variant to dispatch
/// the correct catalogue code, replacing the legacy string-prefix sniffing
/// (`S0002:constructor_tag_unresolved:` etc.) that the integration plan
/// retired in commit 18.
#[non_exhaustive]
#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
pub enum OpaqueCode {
    /// `S0002` — `resolve_constructor_tag` returned `None` for a constructor
    /// reference that is not in the data-type registry. Carries the
    /// constructor name and the qualified type name for diagnostic
    /// attribution.
    ConstructorTagUnresolved { ctor: String, type_name: String },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub enum ShallowFieldAccessKind {
    ConstructorField,
    ListElement,
}

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
pub struct ShallowIrRecordUpdate {
    pub label: String,
    pub index: usize,
    pub value: ShallowIr,
}

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
pub struct ShallowIrArm {
    /// Constructor tag this arm matches (None for a catch-all variable pattern)
    pub tag: Option<u64>,
    /// Bound variable names (one per field, in order)
    pub bindings: Vec<String>,
    /// Additional refinement required by nested/literal subpatterns after the outer tag matches.
    #[serde(default)]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub guard: Option<Box<ShallowIr>>,
    /// Body of this arm
    pub body: ShallowIr,
}

#[non_exhaustive]
#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
pub enum ShallowConst {
    Int(String), // bigint as string
    Bool(bool),
    ByteArray(String), // hex-encoded
    String(String),
    Unit,
}

#[non_exhaustive]
#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
pub enum ShallowIrType {
    Int,
    Bool,
    ByteArray,
    String,
    Unit,
    Data,
    List(Box<ShallowIrType>),
    Pair(Box<ShallowIrType>, Box<ShallowIrType>),
    Adt(String), // qualified "module.Type" name
    Unknown,
}

#[non_exhaustive]
#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
pub enum ShallowBinOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Eq,
    NotEq,
    Lt,
    LtEq,
    Gt,
    GtEq,
    And,
    Or,
    Append, // ByteArray / String / List append
}

/// Translate a typed expression into a `ShallowIr` tree.
///
/// Never fails — unrecognised constructs become `ShallowIr::Opaque`.
/// Fuzzer combinator calls (module `aiken/fuzz`, `aiken-lang/fuzz`) become
/// `ShallowIr::FuzzExistential`, causing the random dimension to be expressed
/// as an existentially-quantified variable in the generated Lean predicate.
///
/// `data_types` is consulted to resolve constructor tags via
/// `resolve_constructor_tag`. Without this, every `Construct` node would be
/// emitted with `tag: 0` regardless of declaration order or `@tag(N)`
/// decorators, producing incorrect structural-equality predicates for
/// non-first constructors.
pub fn typed_expr_to_shallow_ir(
    expr: &TypedExpr,
    data_types: &IndexMap<&DataTypeKey, &TypedDataType>,
) -> ShallowIr {
    match expr {
        TypedExpr::UInt { value, .. } => ShallowIr::Const(ShallowConst::Int(value.clone())),

        TypedExpr::String { value, .. } => ShallowIr::Const(ShallowConst::String(value.clone())),

        TypedExpr::ByteArray { bytes, .. } => {
            ShallowIr::Const(ShallowConst::ByteArray(hex::encode(bytes)))
        }

        TypedExpr::Var {
            name, constructor, ..
        } => {
            // Zero-arity record constructors (e.g. `Scenario.Done`) are accessed
            // as `Var` nodes with no arguments. If we emit them as `ShallowIr::Var`
            // the downstream emitter produces a fresh existential `∃ _fuzz_N, ...`
            // which makes any `EqOutput(Done)` trivially true.
            //
            // Instead, encode them as `ShallowIr::Construct { fields: [] }` with
            // the canonical UPLC tag resolved from `data_types`. The tag is
            // determined by declaration order (or `@tag(N)` decorators) — see
            // `resolve_constructor_tag`. If lookup fails (`None`), we route
            // the node through `ShallowIr::Opaque` carrying the S0002 marker
            // so the verify pipeline emits a hard `ConstructorTagUnresolved`
            // error rather than silently emitting tag 0 (which would make
            // any non-first-constructor `EqOutput` vacuously satisfiable).
            if let ValueConstructorVariant::Record {
                arity,
                module,
                name: ctor_name,
                ..
            } = &constructor.variant
            {
                if *arity == 0 {
                    if let Some(value) = bool_constructor_literal(ctor_name, &constructor.tipo) {
                        return ShallowIr::Const(ShallowConst::Bool(value));
                    }
                    return match resolve_constructor_tag(&constructor.tipo, ctor_name, data_types) {
                        Some(tag) => ShallowIr::Construct {
                            module: module.clone(),
                            constructor: ctor_name.clone(),
                            tag,
                            fields: vec![],
                        },
                        None => {
                            let type_name = describe_tipo(&constructor.tipo);
                            ShallowIr::Opaque {
                                ty: shallow_ir_type(&constructor.tipo),
                                reason: s0002_reason_message(ctor_name, &type_name),
                                code: Some(OpaqueCode::ConstructorTagUnresolved {
                                    ctor: ctor_name.clone(),
                                    type_name,
                                }),
                            }
                        }
                    };
                }
            }
            ShallowIr::Var {
                name: name.clone(),
                ty: shallow_ir_type(&constructor.tipo),
            }
        }

        TypedExpr::Assignment { value, pattern, .. } => {
            // `let pat = value` — extract single-variable pattern name
            if let TypedPattern::Var { name, .. } = pattern {
                // we don't have a body here in Assignment alone — this is used
                // inside Sequence; return the value and let Sequence stitch it
                ShallowIr::Let {
                    name: name.clone(),
                    value: Box::new(typed_expr_to_shallow_ir(value, data_types)),
                    body: Box::new(ShallowIr::Const(ShallowConst::Unit)),
                }
            } else {
                ShallowIr::Opaque {
                    ty: shallow_ir_type(&expr.tipo()),
                    reason: "complex pattern in let binding".into(),
                    code: None,
                }
            }
        }

        TypedExpr::Sequence { expressions, .. } | TypedExpr::Pipeline { expressions, .. } => {
            translate_sequence(expressions, data_types)
        }

        TypedExpr::If {
            branches,
            final_else,
            ..
        } => {
            // Lean if-then-else chain
            translate_if_chain(branches, final_else, data_types)
        }

        TypedExpr::When {
            subject, clauses, ..
        } => {
            let subject_tipo = subject.tipo();
            // Translate every arm; if any arm refers to a constructor that
            // cannot be resolved in the data-type registry, `translate_clause`
            // returns `Err(<S0002 marker>)` and we collapse the whole `Match`
            // to a `ShallowIr::Opaque` carrying the same marker. This
            // guarantees the verify pipeline raises a hard `S0002`
            // `ConstructorTagUnresolved` error rather than emitting a `Match`
            // with a tag-0 arm that is vacuously satisfiable.
            let arms: Result<Vec<_>, _> = clauses
                .iter()
                .map(|c| translate_clause(c, subject.as_ref(), &subject_tipo, data_types))
                .collect();
            match arms {
                Ok(arms) => ShallowIr::Match {
                    subject: Box::new(typed_expr_to_shallow_ir(subject, data_types)),
                    arms,
                },
                Err(failure) => ShallowIr::Opaque {
                    ty: shallow_ir_type(&expr.tipo()),
                    reason: failure.reason,
                    code: Some(failure.code),
                },
            }
        }

        TypedExpr::RecordAccess {
            record,
            index,
            label,
            ..
        } => ShallowIr::FieldAccess {
            record: Box::new(typed_expr_to_shallow_ir(record, data_types)),
            index: *index,
            label: label.clone(),
            ty: shallow_ir_type(&expr.tipo()),
            kind: ShallowFieldAccessKind::ConstructorField,
        },

        TypedExpr::BinOp {
            name, left, right, ..
        } => {
            if let Some(op) = translate_binop(name) {
                ShallowIr::BinOp {
                    op,
                    left: Box::new(typed_expr_to_shallow_ir(left, data_types)),
                    right: Box::new(typed_expr_to_shallow_ir(right, data_types)),
                }
            } else {
                ShallowIr::Opaque {
                    ty: shallow_ir_type(&expr.tipo()),
                    reason: format!("unsupported binary operator `{}`", describe_binop(name)),
                    code: None,
                }
            }
        }

        TypedExpr::List { elements, tail, .. } => ShallowIr::ListLit {
            elements: elements
                .iter()
                .map(|e| typed_expr_to_shallow_ir(e, data_types))
                .collect(),
            tail: tail
                .as_ref()
                .map(|tail| Box::new(typed_expr_to_shallow_ir(tail.as_ref(), data_types))),
            ty: shallow_ir_type(&expr.tipo()),
        },

        TypedExpr::Tuple { elems, .. } => ShallowIr::Tuple(
            elems
                .iter()
                .map(|e| typed_expr_to_shallow_ir(e, data_types))
                .collect(),
        ),

        TypedExpr::Pair { fst, snd, .. } => ShallowIr::Tuple(vec![
            typed_expr_to_shallow_ir(fst, data_types),
            typed_expr_to_shallow_ir(snd, data_types),
        ]),

        TypedExpr::Call {
            fun, args, tipo, ..
        } => translate_call(fun, args, tipo, data_types),

        TypedExpr::Fn { body, .. } => {
            // Lambda: translate body directly (step function IS a lambda)
            typed_expr_to_shallow_ir(body, data_types)
        }

        TypedExpr::Trace { then, .. } => {
            // Drop trace, translate the continuation
            typed_expr_to_shallow_ir(then, data_types)
        }

        // Qualified zero-arity constructor (e.g. `Scenario.Done` imported from
        // another module). These appear as `TypedExpr::ModuleSelect` (not `Var`)
        // because they are accessed through a module qualifier. Without special
        // handling they fall through to `Opaque` → fresh existential → the
        // `EqOutput` branch becomes trivially True. Encoding as
        // `Construct { fields: [] }` with the resolved canonical tag produces
        // `Data.Constr (n : Integer) []` in the Lean output, which is a real
        // (non-trivial) constraint anchored at the right constructor index.
        TypedExpr::ModuleSelect {
            constructor:
                ModuleValueConstructor::Record {
                    arity: 0,
                    name: ctor_name,
                    tipo,
                    ..
                },
            module_alias,
            ..
        } => {
            if let Some(value) = bool_constructor_literal(ctor_name, tipo) {
                ShallowIr::Const(ShallowConst::Bool(value))
            } else {
                match resolve_constructor_tag(tipo, ctor_name, data_types) {
                    Some(tag) => ShallowIr::Construct {
                        module: module_alias.clone(),
                        constructor: ctor_name.clone(),
                        tag,
                        fields: vec![],
                    },
                    None => {
                        let type_name = describe_tipo(tipo);
                        ShallowIr::Opaque {
                            ty: shallow_ir_type(&expr.tipo()),
                            reason: s0002_reason_message(ctor_name, &type_name),
                            code: Some(OpaqueCode::ConstructorTagUnresolved {
                                ctor: ctor_name.clone(),
                                type_name,
                            }),
                        }
                    }
                }
            }
        }

        // Everything else: opaque
        _ => ShallowIr::Opaque {
            ty: shallow_ir_type(&expr.tipo()),
            reason: "unsupported TypedExpr variant".to_string(),
            code: None,
        },
    }
}

pub(super) fn binding_shallow_ir_type(binding: &LocalBinding) -> ShallowIrType {
    match binding {
        LocalBinding::PureExpr(expr) => shallow_ir_type(&expr.tipo()),
        LocalBinding::ExactIr { ty, .. }
        | LocalBinding::DrawnValue { ty, .. }
        | LocalBinding::Projection { ty, .. } => ty.clone(),
    }
}

pub(super) fn projected_binding(
    binding: &LocalBinding,
    index: u64,
    label: impl Into<String>,
    ty: ShallowIrType,
    kind: ShallowFieldAccessKind,
) -> Option<LocalBinding> {
    let label = label.into();
    let item = match kind {
        ShallowFieldAccessKind::ConstructorField => ProjectionPathItem::ConstructorField {
            index,
            label,
            ty: ty.clone(),
        },
        ShallowFieldAccessKind::ListElement => ProjectionPathItem::ListElement {
            index,
            label,
            ty: ty.clone(),
        },
    };
    match binding {
        LocalBinding::DrawnValue {
            lean_name,
            ty: source_ty,
            ..
        } => Some(LocalBinding::Projection {
            source_lean_name: lean_name.clone(),
            source_ty: source_ty.clone(),
            path: vec![item],
            ty,
        }),
        LocalBinding::Projection {
            source_lean_name,
            source_ty,
            path,
            ..
        } => {
            let mut path = path.clone();
            path.push(item);
            Some(LocalBinding::Projection {
                source_lean_name: source_lean_name.clone(),
                source_ty: source_ty.clone(),
                path,
                ty,
            })
        }
        LocalBinding::ExactIr { ir, .. } => {
            let projected_ir = exact_field_ir_from_shallow_ir(ir, index, kind)?;
            Some(LocalBinding::ExactIr {
                ty,
                ir: projected_ir,
            })
        }
        LocalBinding::PureExpr(_) => None,
    }
}

pub(super) fn constructor_pattern_argument_types(
    pattern: &TypedPattern,
    data_types: &IndexMap<&DataTypeKey, &TypedDataType>,
) -> Vec<ShallowIrType> {
    let TypedPattern::Constructor {
        name: ctor_name,
        arguments,
        tipo,
        ..
    } = pattern
    else {
        return Vec::new();
    };

    let fallback = || vec![ShallowIrType::Data; arguments.len()];
    let Some(data_type) = lookup_data_type_by_tipo(data_types, tipo.as_ref()) else {
        return fallback();
    };
    let Some(constructor) = data_type
        .constructors
        .iter()
        .find(|constructor| constructor.name == *ctor_name)
    else {
        return fallback();
    };

    constructor
        .arguments
        .iter()
        .map(|arg| shallow_ir_type(&arg.tipo))
        .collect()
}

pub(super) fn pure_expr_constructor_fields(expr: &TypedExpr) -> Option<Vec<TypedExpr>> {
    match terminal_expression(expr) {
        TypedExpr::Call { fun, args, .. } => match terminal_expression(fun) {
            TypedExpr::Var { constructor, .. } => match &constructor.variant {
                ValueConstructorVariant::Record { arity, .. } if *arity == args.len() => {
                    Some(args.iter().map(|arg| arg.value.clone()).collect())
                }
                _ => None,
            },
            TypedExpr::ModuleSelect {
                constructor: ModuleValueConstructor::Record { arity, .. },
                ..
            } if *arity == args.len() => Some(args.iter().map(|arg| arg.value.clone()).collect()),
            _ => None,
        },
        TypedExpr::Var { constructor, .. } => match &constructor.variant {
            ValueConstructorVariant::Record { arity, .. } if *arity == 0 => Some(Vec::new()),
            _ => None,
        },
        TypedExpr::ModuleSelect {
            constructor: ModuleValueConstructor::Record { arity, .. },
            ..
        } if *arity == 0 => Some(Vec::new()),
        _ => None,
    }
}

pub(super) fn bool_constructor_literal(ctor_name: &str, tipo: &Rc<Type>) -> Option<bool> {
    if tipo != &Type::bool() {
        return None;
    }
    match ctor_name {
        "True" => Some(true),
        "False" => Some(false),
        _ => None,
    }
}

pub(super) fn bind_pattern_to_locals(
    pattern: &TypedPattern,
    binding: &LocalBinding,
    data_types: &IndexMap<&DataTypeKey, &TypedDataType>,
    locals: &mut BTreeMap<String, LocalBinding>,
) {
    match pattern {
        TypedPattern::Var { name, .. } => {
            locals.insert(name.clone(), binding.clone());
        }
        TypedPattern::Assign { name, pattern, .. } => {
            locals.insert(name.clone(), binding.clone());
            bind_pattern_to_locals(pattern, binding, data_types, locals);
        }
        TypedPattern::Discard { .. } => {}
        TypedPattern::Pair { fst, snd, .. } => {
            if let LocalBinding::PureExpr(expr) = binding {
                if let TypedExpr::Pair {
                    fst: expr_fst,
                    snd: expr_snd,
                    ..
                } = terminal_expression(expr)
                {
                    bind_pattern_to_locals(
                        fst,
                        &LocalBinding::PureExpr(expr_fst.as_ref().clone()),
                        data_types,
                        locals,
                    );
                    bind_pattern_to_locals(
                        snd,
                        &LocalBinding::PureExpr(expr_snd.as_ref().clone()),
                        data_types,
                        locals,
                    );
                    return;
                }
            }

            let (fst_ty, snd_ty) = match binding_shallow_ir_type(binding) {
                ShallowIrType::Pair(fst_ty, snd_ty) => ((*fst_ty).clone(), (*snd_ty).clone()),
                _ => (ShallowIrType::Data, ShallowIrType::Data),
            };
            if let Some(fst_binding) =
                projected_binding(binding, 0, "0", fst_ty, ShallowFieldAccessKind::ListElement)
            {
                bind_pattern_to_locals(fst, &fst_binding, data_types, locals);
            }
            if let Some(snd_binding) =
                projected_binding(binding, 1, "1", snd_ty, ShallowFieldAccessKind::ListElement)
            {
                bind_pattern_to_locals(snd, &snd_binding, data_types, locals);
            }
        }
        TypedPattern::Tuple { elems, .. } => {
            if let LocalBinding::PureExpr(expr) = binding {
                if let TypedExpr::Tuple {
                    elems: expr_elems, ..
                } = terminal_expression(expr)
                {
                    for (pat, expr) in elems.iter().zip(expr_elems.iter()) {
                        bind_pattern_to_locals(
                            pat,
                            &LocalBinding::PureExpr(expr.clone()),
                            data_types,
                            locals,
                        );
                    }
                    return;
                }
            }

            let pair_types = match binding_shallow_ir_type(binding) {
                ShallowIrType::Pair(fst_ty, snd_ty) if elems.len() == 2 => {
                    Some([(*fst_ty).clone(), (*snd_ty).clone()])
                }
                _ => None,
            };

            for (index, pat) in elems.iter().enumerate() {
                let ty = pair_types
                    .as_ref()
                    .and_then(|tys| tys.get(index))
                    .cloned()
                    .unwrap_or(ShallowIrType::Data);
                if let Some(projected) = projected_binding(
                    binding,
                    index as u64,
                    index.to_string(),
                    ty,
                    ShallowFieldAccessKind::ListElement,
                ) {
                    bind_pattern_to_locals(pat, &projected, data_types, locals);
                }
            }
        }
        TypedPattern::Constructor { arguments, .. } => {
            if let LocalBinding::PureExpr(expr) = binding {
                if let Some(expr_args) = pure_expr_constructor_fields(expr) {
                    for (arg, expr_arg) in arguments.iter().zip(expr_args.iter()) {
                        bind_pattern_to_locals(
                            &arg.value,
                            &LocalBinding::PureExpr(expr_arg.clone()),
                            data_types,
                            locals,
                        );
                    }
                    return;
                }
            }

            let field_types = constructor_pattern_argument_types(pattern, data_types);
            for (index, arg) in arguments.iter().enumerate() {
                let ty = field_types
                    .get(index)
                    .cloned()
                    .unwrap_or(ShallowIrType::Data);
                let label = arg.label.clone().unwrap_or_else(|| index.to_string());
                if let Some(projected) = projected_binding(
                    binding,
                    index as u64,
                    label,
                    ty,
                    ShallowFieldAccessKind::ConstructorField,
                ) {
                    bind_pattern_to_locals(&arg.value, &projected, data_types, locals);
                }
            }
        }
        TypedPattern::List { .. } | TypedPattern::Int { .. } | TypedPattern::ByteArray { .. } => {}
    }
}

pub(super) fn unsupported_assignment_pattern_reason(
    pattern: &TypedPattern,
) -> Option<&'static str> {
    match pattern {
        TypedPattern::Var { .. } | TypedPattern::Discard { .. } => None,
        TypedPattern::Int { .. } => {
            Some("int-pattern bindings require an equality guard that is not emitted yet")
        }
        TypedPattern::ByteArray { .. } => {
            Some("bytearray-pattern bindings require an equality guard that is not emitted yet")
        }
        TypedPattern::Assign { pattern, .. } => unsupported_assignment_pattern_reason(pattern),
        TypedPattern::Pair { fst, snd, .. } => unsupported_assignment_pattern_reason(fst)
            .or_else(|| unsupported_assignment_pattern_reason(snd)),
        TypedPattern::Tuple { elems, .. } => {
            elems.iter().find_map(unsupported_assignment_pattern_reason)
        }
        TypedPattern::Constructor { arguments, .. } => arguments
            .iter()
            .find_map(|arg| unsupported_assignment_pattern_reason(&arg.value)),
        TypedPattern::List { .. } => Some("list-pattern bindings are not lowered faithfully yet"),
    }
}

pub(super) fn unsupported_pattern_binding_reason(pattern: &TypedPattern) -> Option<&'static str> {
    match pattern {
        TypedPattern::Var { .. }
        | TypedPattern::Discard { .. }
        | TypedPattern::Int { .. }
        | TypedPattern::ByteArray { .. } => None,
        TypedPattern::Assign { pattern, .. } => unsupported_pattern_binding_reason(pattern),
        TypedPattern::Pair { fst, snd, .. } => unsupported_pattern_binding_reason(fst)
            .or_else(|| unsupported_pattern_binding_reason(snd)),
        TypedPattern::Tuple { elems, .. } => {
            elems.iter().find_map(unsupported_pattern_binding_reason)
        }
        TypedPattern::Constructor { arguments, .. } => arguments
            .iter()
            .find_map(|arg| unsupported_pattern_binding_reason(&arg.value)),
        TypedPattern::List { .. } => Some("list-pattern bindings are not lowered faithfully yet"),
    }
}

pub(super) fn guarded_when_clause_fallback(
    unsupported: TransitionProp,
    body: TransitionProp,
    fallback: TransitionProp,
) -> TransitionProp {
    let _ = body;
    TransitionProp::Or(vec![unsupported, fallback])
}

pub(super) fn local_binding_to_shallow_ir(
    binding: &LocalBinding,
    data_types: &IndexMap<&DataTypeKey, &TypedDataType>,
    locals: &BTreeMap<String, LocalBinding>,
    visiting: &mut BTreeSet<String>,
) -> ShallowIr {
    match binding {
        LocalBinding::PureExpr(expr) => {
            typed_expr_to_shallow_ir_with_locals(expr, data_types, locals, visiting)
        }
        LocalBinding::ExactIr { ir, .. } => ir.clone(),
        LocalBinding::DrawnValue { lean_name, ty, .. } => ShallowIr::BoundVar {
            name: lean_name.clone(),
            ty: ty.clone(),
        },
        LocalBinding::Projection {
            source_lean_name,
            source_ty,
            path,
            ..
        } => {
            let mut ir = ShallowIr::BoundVar {
                name: source_lean_name.clone(),
                ty: source_ty.clone(),
            };
            for item in path {
                ir = match item {
                    ProjectionPathItem::ConstructorField { index, label, ty } => {
                        ShallowIr::FieldAccess {
                            record: Box::new(ir),
                            index: *index,
                            label: label.clone(),
                            ty: ty.clone(),
                            kind: ShallowFieldAccessKind::ConstructorField,
                        }
                    }
                    ProjectionPathItem::ListElement { index, label, ty } => {
                        ShallowIr::FieldAccess {
                            record: Box::new(ir),
                            index: *index,
                            label: label.clone(),
                            ty: ty.clone(),
                            kind: ShallowFieldAccessKind::ListElement,
                        }
                    }
                };
            }
            ir
        }
    }
}

pub(super) fn record_constructor_shape(
    tipo: &Rc<Type>,
    data_types: &IndexMap<&DataTypeKey, &TypedDataType>,
) -> Option<(String, String, u64, usize)> {
    let dt = lookup_data_type_by_tipo(data_types, tipo.as_ref())?;
    if dt.constructors.len() != 1 {
        return None;
    }
    let ctor = dt.constructors.first()?;
    let tag = resolve_constructor_tag(tipo, &ctor.name, data_types)?;
    Some((
        dt.name.clone(),
        ctor.name.clone(),
        tag,
        ctor.arguments.len(),
    ))
}

pub(super) fn typed_expr_to_shallow_ir_with_locals(
    expr: &TypedExpr,
    data_types: &IndexMap<&DataTypeKey, &TypedDataType>,
    locals: &BTreeMap<String, LocalBinding>,
    visiting: &mut BTreeSet<String>,
) -> ShallowIr {
    match expr {
        TypedExpr::Var {
            name, constructor, ..
        } if locals.contains_key(name) => {
            if let Some(binding) = locals.get(name) {
                if !visiting.insert(name.clone()) {
                    return ShallowIr::Opaque {
                        ty: shallow_ir_type(&constructor.tipo),
                        reason: format!("local binding cycle on '{name}' while lowering value"),
                        code: None,
                    };
                }
                let lowered = local_binding_to_shallow_ir(binding, data_types, locals, visiting);
                visiting.remove(name);
                return lowered;
            }
            ShallowIr::Var {
                name: name.clone(),
                ty: shallow_ir_type(&constructor.tipo),
            }
        }
        TypedExpr::UInt { value, .. } => ShallowIr::Const(ShallowConst::Int(value.clone())),
        TypedExpr::String { value, .. } => ShallowIr::Const(ShallowConst::String(value.clone())),
        TypedExpr::ByteArray { bytes, .. } => {
            ShallowIr::Const(ShallowConst::ByteArray(hex::encode(bytes)))
        }
        TypedExpr::Var {
            constructor, name, ..
        } => {
            if let ValueConstructorVariant::Record {
                arity,
                module,
                name: ctor_name,
                ..
            } = &constructor.variant
            {
                if *arity == 0 {
                    if let Some(value) = bool_constructor_literal(ctor_name, &constructor.tipo) {
                        return ShallowIr::Const(ShallowConst::Bool(value));
                    }
                    return match resolve_constructor_tag(&constructor.tipo, ctor_name, data_types) {
                        Some(tag) => ShallowIr::Construct {
                            module: module.clone(),
                            constructor: ctor_name.clone(),
                            tag,
                            fields: vec![],
                        },
                        None => {
                            let type_name = describe_tipo(&constructor.tipo);
                            ShallowIr::Opaque {
                                ty: shallow_ir_type(&constructor.tipo),
                                reason: s0002_reason_message(ctor_name, &type_name),
                                code: Some(OpaqueCode::ConstructorTagUnresolved {
                                    ctor: ctor_name.clone(),
                                    type_name,
                                }),
                            }
                        }
                    };
                }
            }
            ShallowIr::Var {
                name: name.clone(),
                ty: shallow_ir_type(&constructor.tipo),
            }
        }
        TypedExpr::Sequence { expressions, .. } | TypedExpr::Pipeline { expressions, .. } => {
            translate_sequence_with_locals(expressions, data_types, locals, visiting)
        }
        TypedExpr::If {
            branches,
            final_else,
            ..
        } => {
            let mut result =
                typed_expr_to_shallow_ir_with_locals(final_else, data_types, locals, visiting);
            for branch in branches.iter().rev() {
                result = ShallowIr::If {
                    cond: Box::new(typed_expr_to_shallow_ir_with_locals(
                        &branch.condition,
                        data_types,
                        locals,
                        visiting,
                    )),
                    then_branch: Box::new(typed_expr_to_shallow_ir_with_locals(
                        &branch.body,
                        data_types,
                        locals,
                        visiting,
                    )),
                    else_branch: Box::new(result),
                };
            }
            result
        }
        TypedExpr::When {
            subject, clauses, ..
        } => {
            let subject_tipo = subject.tipo();
            let mut visiting_subject_binding = BTreeSet::new();
            let subject_binding = binding_from_expr_with_locals(
                subject.as_ref(),
                locals,
                &mut visiting_subject_binding,
            );
            let arms: Result<Vec<_>, _> = clauses
                .iter()
                .map(|clause| {
                    translate_clause_with_locals(
                        clause,
                        &subject_tipo,
                        &subject_binding,
                        data_types,
                        locals,
                        visiting,
                    )
                })
                .collect();
            match arms {
                Ok(arms) => ShallowIr::Match {
                    subject: Box::new(typed_expr_to_shallow_ir_with_locals(
                        subject, data_types, locals, visiting,
                    )),
                    arms,
                },
                Err(failure) => ShallowIr::Opaque {
                    ty: shallow_ir_type(&expr.tipo()),
                    reason: failure.reason,
                    code: Some(failure.code),
                },
            }
        }
        TypedExpr::RecordAccess {
            record,
            index,
            label,
            ..
        } => ShallowIr::FieldAccess {
            record: Box::new(typed_expr_to_shallow_ir_with_locals(
                record, data_types, locals, visiting,
            )),
            index: *index,
            label: label.clone(),
            ty: shallow_ir_type(&expr.tipo()),
            kind: ShallowFieldAccessKind::ConstructorField,
        },
        TypedExpr::RecordUpdate {
            spread, args, tipo, ..
        } => {
            if let Some((_type_name, _ctor_name, tag, field_count)) =
                record_constructor_shape(tipo, data_types)
            {
                ShallowIr::RecordUpdate {
                    record: Box::new(typed_expr_to_shallow_ir_with_locals(
                        spread, data_types, locals, visiting,
                    )),
                    tag,
                    field_count,
                    updates: args
                        .iter()
                        .map(|arg| ShallowIrRecordUpdate {
                            label: arg.label.clone(),
                            index: arg.index,
                            value: typed_expr_to_shallow_ir_with_locals(
                                &arg.value, data_types, locals, visiting,
                            ),
                        })
                        .collect(),
                }
            } else {
                ShallowIr::Opaque {
                    ty: shallow_ir_type(tipo),
                    reason: "record update target is not a single-constructor record".to_string(),
                    code: None,
                }
            }
        }
        TypedExpr::BinOp {
            name, left, right, ..
        } => {
            if let Some(op) = translate_binop(name) {
                ShallowIr::BinOp {
                    op,
                    left: Box::new(typed_expr_to_shallow_ir_with_locals(
                        left, data_types, locals, visiting,
                    )),
                    right: Box::new(typed_expr_to_shallow_ir_with_locals(
                        right, data_types, locals, visiting,
                    )),
                }
            } else {
                ShallowIr::Opaque {
                    ty: shallow_ir_type(&expr.tipo()),
                    reason: format!("unsupported binary operator `{}`", describe_binop(name)),
                    code: None,
                }
            }
        }
        TypedExpr::List { elements, tail, .. } => ShallowIr::ListLit {
            elements: elements
                .iter()
                .map(|e| typed_expr_to_shallow_ir_with_locals(e, data_types, locals, visiting))
                .collect(),
            tail: tail.as_ref().map(|tail| {
                Box::new(typed_expr_to_shallow_ir_with_locals(
                    tail.as_ref(),
                    data_types,
                    locals,
                    visiting,
                ))
            }),
            ty: shallow_ir_type(&expr.tipo()),
        },
        TypedExpr::Tuple { elems, .. } => ShallowIr::Tuple(
            elems
                .iter()
                .map(|e| typed_expr_to_shallow_ir_with_locals(e, data_types, locals, visiting))
                .collect(),
        ),
        TypedExpr::Pair { fst, snd, .. } => ShallowIr::Tuple(vec![
            typed_expr_to_shallow_ir_with_locals(fst, data_types, locals, visiting),
            typed_expr_to_shallow_ir_with_locals(snd, data_types, locals, visiting),
        ]),
        TypedExpr::Call {
            fun, args, tipo, ..
        } => translate_call_with_locals(fun, args, tipo, data_types, locals, visiting),
        TypedExpr::Fn { body, .. } => {
            typed_expr_to_shallow_ir_with_locals(body, data_types, locals, visiting)
        }
        TypedExpr::Trace { then, .. } => {
            typed_expr_to_shallow_ir_with_locals(then, data_types, locals, visiting)
        }
        TypedExpr::ModuleSelect {
            constructor:
                ModuleValueConstructor::Record {
                    arity: 0,
                    name: ctor_name,
                    tipo,
                    ..
                },
            module_alias,
            ..
        } => {
            if let Some(value) = bool_constructor_literal(ctor_name, tipo) {
                ShallowIr::Const(ShallowConst::Bool(value))
            } else {
                match resolve_constructor_tag(tipo, ctor_name, data_types) {
                    Some(tag) => ShallowIr::Construct {
                        module: module_alias.clone(),
                        constructor: ctor_name.clone(),
                        tag,
                        fields: vec![],
                    },
                    None => {
                        let type_name = describe_tipo(tipo);
                        ShallowIr::Opaque {
                            ty: shallow_ir_type(&expr.tipo()),
                            reason: s0002_reason_message(ctor_name, &type_name),
                            code: Some(OpaqueCode::ConstructorTagUnresolved {
                                ctor: ctor_name.clone(),
                                type_name,
                            }),
                        }
                    }
                }
            }
        }
        _ => ShallowIr::Opaque {
            ty: shallow_ir_type(&expr.tipo()),
            reason: "unsupported TypedExpr variant".to_string(),
            code: None,
        },
    }
}

pub(super) fn translate_sequence_with_locals(
    expressions: &[TypedExpr],
    data_types: &IndexMap<&DataTypeKey, &TypedDataType>,
    locals: &BTreeMap<String, LocalBinding>,
    visiting: &mut BTreeSet<String>,
) -> ShallowIr {
    match expressions {
        [] => ShallowIr::Const(ShallowConst::Unit),
        [single] => typed_expr_to_shallow_ir_with_locals(single, data_types, locals, visiting),
        [TypedExpr::Assignment { value, pattern, .. }, rest @ ..] => {
            let mut scoped = locals.clone();
            if let Some(reason) = unsupported_assignment_pattern_reason(pattern) {
                let ty = rest
                    .last()
                    .map(|expr| shallow_ir_type(&expr.tipo()))
                    .unwrap_or(ShallowIrType::Unit);
                return ShallowIr::Opaque {
                    ty,
                    reason: reason.to_string(),
                    code: None,
                };
            }

            if let Some(payload_tipo) = extract_fuzzer_payload_type(value.tipo().as_ref()) {
                let binder = pattern_var_name(pattern)
                    .map(ToString::to_string)
                    .unwrap_or_else(|| format!("_draw_{}", expressions.len()));
                let binding = LocalBinding::DrawnValue {
                    lean_name: binder,
                    ty: shallow_ir_type(&payload_tipo),
                    domain: FuzzerSemantics::Data,
                };
                bind_pattern_to_locals(pattern, &binding, data_types, &mut scoped);
                translate_sequence_with_locals(rest, data_types, &scoped, visiting)
            } else {
                let value_ir =
                    typed_expr_to_shallow_ir_with_locals(value, data_types, locals, visiting);
                let mut binding_visiting = BTreeSet::new();
                let binding = binding_from_expr_with_locals(value, locals, &mut binding_visiting);
                bind_pattern_to_locals(pattern, &binding, data_types, &mut scoped);
                if let TypedPattern::Var { name, .. } = pattern {
                    ShallowIr::Let {
                        name: name.clone(),
                        value: Box::new(value_ir),
                        body: Box::new(translate_sequence_with_locals(
                            rest, data_types, &scoped, visiting,
                        )),
                    }
                } else {
                    ShallowIr::Let {
                        name: "_".into(),
                        value: Box::new(value_ir),
                        body: Box::new(translate_sequence_with_locals(
                            rest, data_types, &scoped, visiting,
                        )),
                    }
                }
            }
        }
        [head, rest @ ..] => ShallowIr::Let {
            name: "_".into(),
            value: Box::new(typed_expr_to_shallow_ir_with_locals(
                head, data_types, locals, visiting,
            )),
            body: Box::new(translate_sequence_with_locals(
                rest, data_types, locals, visiting,
            )),
        },
    }
}

fn pattern_is_trivial_binding(pattern: &TypedPattern) -> bool {
    match pattern {
        TypedPattern::Var { .. } | TypedPattern::Discard { .. } => true,
        TypedPattern::Assign { pattern, .. } => pattern_is_trivial_binding(pattern),
        _ => false,
    }
}

fn pattern_needs_refinement_guard(pattern: &TypedPattern) -> bool {
    match pattern {
        TypedPattern::Var { .. } | TypedPattern::Discard { .. } => false,
        TypedPattern::Assign { pattern, .. } => pattern_needs_refinement_guard(pattern),
        TypedPattern::Constructor { arguments, .. } => arguments
            .iter()
            .any(|arg| !pattern_is_trivial_binding(&arg.value)),
        TypedPattern::Int { .. }
        | TypedPattern::ByteArray { .. }
        | TypedPattern::Pair { .. }
        | TypedPattern::Tuple { .. } => true,
        // List patterns need length/tail guards to be exact. Do not claim a
        // precise refinement here until list-shape guards are emitted.
        TypedPattern::List { .. } => false,
    }
}

fn pattern_value_ir_from_subject(
    pattern: &TypedPattern,
    subject_ir: &ShallowIr,
    data_types: &IndexMap<&DataTypeKey, &TypedDataType>,
) -> Result<Option<ShallowIr>, ClauseTranslationFailure> {
    match pattern {
        TypedPattern::Var { .. } | TypedPattern::Discard { .. } => Ok(Some(subject_ir.clone())),
        TypedPattern::Assign { pattern, .. } => {
            pattern_value_ir_from_subject(pattern, subject_ir, data_types)
        }
        TypedPattern::Int { value, .. } => {
            Ok(Some(ShallowIr::Const(ShallowConst::Int(value.clone()))))
        }
        TypedPattern::ByteArray { value, .. } => Ok(Some(ShallowIr::Const(
            ShallowConst::ByteArray(hex::encode(value)),
        ))),
        TypedPattern::Pair { fst, snd, .. } => Ok(Some(ShallowIr::Tuple(vec![
            pattern_value_ir_from_subject(
                fst,
                &ShallowIr::FieldAccess {
                    record: Box::new(subject_ir.clone()),
                    index: 0,
                    label: "0".to_string(),
                    ty: ShallowIrType::Data,
                    kind: ShallowFieldAccessKind::ListElement,
                },
                data_types,
            )?
            .unwrap_or_else(|| ShallowIr::Const(ShallowConst::Unit)),
            pattern_value_ir_from_subject(
                snd,
                &ShallowIr::FieldAccess {
                    record: Box::new(subject_ir.clone()),
                    index: 1,
                    label: "1".to_string(),
                    ty: ShallowIrType::Data,
                    kind: ShallowFieldAccessKind::ListElement,
                },
                data_types,
            )?
            .unwrap_or_else(|| ShallowIr::Const(ShallowConst::Unit)),
        ]))),
        TypedPattern::Tuple { elems, .. } => {
            let mut fields = Vec::with_capacity(elems.len());
            for (index, elem) in elems.iter().enumerate() {
                let field_ir = ShallowIr::FieldAccess {
                    record: Box::new(subject_ir.clone()),
                    index: index as u64,
                    label: index.to_string(),
                    ty: ShallowIrType::Data,
                    kind: ShallowFieldAccessKind::ListElement,
                };
                fields.push(
                    pattern_value_ir_from_subject(elem, &field_ir, data_types)?
                        .unwrap_or_else(|| ShallowIr::Const(ShallowConst::Unit)),
                );
            }
            Ok(Some(ShallowIr::Tuple(fields)))
        }
        TypedPattern::Constructor {
            name: ctor_name,
            arguments,
            tipo,
            ..
        } => {
            let tag = resolve_constructor_tag(tipo, ctor_name, data_types).ok_or_else(|| {
                let type_name = describe_tipo(tipo);
                ClauseTranslationFailure {
                    reason: s0002_reason_message(ctor_name, &type_name),
                    code: OpaqueCode::ConstructorTagUnresolved {
                        ctor: ctor_name.clone(),
                        type_name,
                    },
                }
            })?;
            let field_types = constructor_pattern_argument_types(pattern, data_types);
            let mut fields = Vec::with_capacity(arguments.len());
            for (index, arg) in arguments.iter().enumerate() {
                let field_ir = ShallowIr::FieldAccess {
                    record: Box::new(subject_ir.clone()),
                    index: index as u64,
                    label: arg.label.clone().unwrap_or_else(|| index.to_string()),
                    ty: field_types
                        .get(index)
                        .cloned()
                        .unwrap_or(ShallowIrType::Data),
                    kind: ShallowFieldAccessKind::ConstructorField,
                };
                fields.push(
                    pattern_value_ir_from_subject(&arg.value, &field_ir, data_types)?
                        .unwrap_or_else(|| ShallowIr::Const(ShallowConst::Unit)),
                );
            }
            Ok(Some(ShallowIr::Construct {
                module: "".to_string(),
                constructor: ctor_name.clone(),
                tag,
                fields,
            }))
        }
        TypedPattern::List { .. } => Ok(None),
    }
}

fn pattern_refinement_guard(
    pattern: &TypedPattern,
    subject_ir: &ShallowIr,
    data_types: &IndexMap<&DataTypeKey, &TypedDataType>,
) -> Result<Option<Box<ShallowIr>>, ClauseTranslationFailure> {
    if !pattern_needs_refinement_guard(pattern) {
        return Ok(None);
    }

    let Some(pattern_ir) = pattern_value_ir_from_subject(pattern, subject_ir, data_types)? else {
        return Ok(None);
    };

    Ok(Some(Box::new(ShallowIr::BinOp {
        op: ShallowBinOp::Eq,
        left: Box::new(subject_ir.clone()),
        right: Box::new(pattern_ir),
    })))
}

pub(super) fn translate_clause_with_locals(
    clause: &TypedClause,
    subject_tipo: &Rc<Type>,
    subject_binding: &LocalBinding,
    data_types: &IndexMap<&DataTypeKey, &TypedDataType>,
    locals: &BTreeMap<String, LocalBinding>,
    visiting: &mut BTreeSet<String>,
) -> Result<ShallowIrArm, ClauseTranslationFailure> {
    let (tag, bindings) = match &clause.pattern {
        TypedPattern::Constructor {
            name: ctor_name,
            arguments,
            ..
        } => {
            let tag =
                resolve_constructor_tag(subject_tipo, ctor_name, data_types).ok_or_else(|| {
                    let type_name = describe_tipo(subject_tipo);
                    ClauseTranslationFailure {
                        reason: s0002_reason_message(ctor_name, &type_name),
                        code: OpaqueCode::ConstructorTagUnresolved {
                            ctor: ctor_name.clone(),
                            type_name,
                        },
                    }
                })?;
            let bindings = arguments
                .iter()
                .map(|arg| match &arg.value {
                    TypedPattern::Var { name, .. } => name.clone(),
                    _ => "_".into(),
                })
                .collect();
            (Some(tag), bindings)
        }
        TypedPattern::Var { name, .. } => (None, vec![name.clone()]),
        TypedPattern::Discard { .. } => (None, vec!["_".into()]),
        _ => (None, vec![]),
    };
    let mut guard_visiting = visiting.clone();
    let subject_ir =
        local_binding_to_shallow_ir(subject_binding, data_types, locals, &mut guard_visiting);
    let guard = pattern_refinement_guard(&clause.pattern, &subject_ir, data_types)?;
    let mut clause_locals = locals.clone();
    bind_pattern_to_locals(
        &clause.pattern,
        subject_binding,
        data_types,
        &mut clause_locals,
    );
    Ok(ShallowIrArm {
        tag,
        bindings,
        guard,
        body: typed_expr_to_shallow_ir_with_locals(
            &clause.then,
            data_types,
            &clause_locals,
            visiting,
        ),
    })
}

pub(super) fn translate_call_with_locals(
    fun: &TypedExpr,
    args: &[CallArg<TypedExpr>],
    tipo: &Rc<Type>,
    data_types: &IndexMap<&DataTypeKey, &TypedDataType>,
    locals: &BTreeMap<String, LocalBinding>,
    visiting: &mut BTreeSet<String>,
) -> ShallowIr {
    if let Some(fuzz_ir) = try_fuzz_existential(fun, args, tipo) {
        return fuzz_ir;
    }
    if extract_module_fn_identity(fun)
        .is_some_and(|(module, name)| module.is_empty() && name == "as_data")
        && let [value] = args
    {
        return typed_expr_to_shallow_ir_with_locals(&value.value, data_types, locals, visiting);
    }
    if extract_module_fn_identity(fun)
        .is_some_and(|(module, name)| module == "aiken/collection/list" && name == "concat")
        && let [left, right] = args
    {
        return ShallowIr::BinOp {
            op: ShallowBinOp::Append,
            left: Box::new(typed_expr_to_shallow_ir_with_locals(
                &left.value,
                data_types,
                locals,
                visiting,
            )),
            right: Box::new(typed_expr_to_shallow_ir_with_locals(
                &right.value,
                data_types,
                locals,
                visiting,
            )),
        };
    }
    if let TypedExpr::Var { constructor, .. } = fun {
        if let ValueConstructorVariant::Record {
            name: ctor_name,
            arity,
            module,
            ..
        } = &constructor.variant
        {
            if *arity == args.len() {
                return match resolve_constructor_tag(&constructor.tipo, ctor_name, data_types) {
                    Some(tag) => ShallowIr::Construct {
                        module: module.clone(),
                        constructor: ctor_name.clone(),
                        tag,
                        fields: args
                            .iter()
                            .map(|a| {
                                typed_expr_to_shallow_ir_with_locals(
                                    &a.value, data_types, locals, visiting,
                                )
                            })
                            .collect(),
                    },
                    None => {
                        let type_name = describe_tipo(&constructor.tipo);
                        ShallowIr::Opaque {
                            ty: shallow_ir_type(tipo),
                            reason: s0002_reason_message(ctor_name, &type_name),
                            code: Some(OpaqueCode::ConstructorTagUnresolved {
                                ctor: ctor_name.clone(),
                                type_name,
                            }),
                        }
                    }
                };
            }
        }
    }
    if let TypedExpr::ModuleSelect {
        constructor:
            ModuleValueConstructor::Record {
                name: ctor_name,
                arity,
                tipo: ctor_tipo,
                ..
            },
        module_alias,
        ..
    } = fun
    {
        if *arity == args.len() {
            return match resolve_constructor_tag(ctor_tipo, ctor_name, data_types) {
                Some(tag) => ShallowIr::Construct {
                    module: module_alias.clone(),
                    constructor: ctor_name.clone(),
                    tag,
                    fields: args
                        .iter()
                        .map(|a| {
                            typed_expr_to_shallow_ir_with_locals(
                                &a.value, data_types, locals, visiting,
                            )
                        })
                        .collect(),
                },
                None => {
                    let type_name = describe_tipo(ctor_tipo);
                    ShallowIr::Opaque {
                        ty: shallow_ir_type(tipo),
                        reason: s0002_reason_message(ctor_name, &type_name),
                        code: Some(OpaqueCode::ConstructorTagUnresolved {
                            ctor: ctor_name.clone(),
                            type_name,
                        }),
                    }
                }
            };
        }
    }
    ShallowIr::Opaque {
        ty: shallow_ir_type(tipo),
        reason: "unrecognised function call".into(),
        code: None,
    }
}

/// Resolve the canonical UPLC constructor tag for `ctor_name` belonging to
/// the data type identified by `container_tipo`.
///
/// `container_tipo` may be either the ADT directly (`Type::App`) or a
/// `Type::Fn { ret: ADT, .. }` (the type of an n-ary constructor function);
/// `lookup_data_type_by_tipo` handles both shapes.
///
/// Returns `None` when either the type is not in the data-type registry
/// or the constructor is not declared on that type. Prior versions
/// silently fell back to tag `0`, which is unsound for non-first
/// constructors: the resulting `transition = Data.Constr 0 [...]` equality
/// in the generated Lean predicate is unsatisfiable for any constructor
/// at index ≥ 1, which makes the implication vacuously true and lets the
/// surrounding theorem pass trivially. Callers receiving `None` must
/// route the affected expression through `ShallowIr::Opaque` carrying the
/// typed [`OpaqueCode::ConstructorTagUnresolved`] payload so the verify
/// pipeline emits a hard `S0002` `ConstructorTagUnresolved` error.
///
/// `get_constr_index_variant` is the canonical source of constructor-tag
/// truth; it honours `@tag(N)` decorators and falls back to declaration
/// order. Do not reimplement that logic here.
pub(super) fn resolve_constructor_tag(
    container_tipo: &Rc<Type>,
    ctor_name: &str,
    data_types: &IndexMap<&DataTypeKey, &TypedDataType>,
) -> Option<u64> {
    let dt = lookup_data_type_by_tipo(data_types, container_tipo.as_ref())?;
    let (idx, _) = get_constr_index_variant(&dt, ctor_name)?;
    Some(idx as u64)
}

/// Format the human-readable diagnostic message written into
/// `ShallowIr::Opaque.reason` when constructor-tag resolution fails.
/// Surfaces in audit logs and fallback messages if the typed-payload
/// dispatch path is ever bypassed; the dispatcher itself reads
/// `OpaqueCode::ConstructorTagUnresolved { ctor, type_name }` directly.
///
/// Commit 18 retired the wire-format prefix (`S0002:constructor_tag_unresolved:`)
/// and the corresponding parser in favour of the typed payload — this
/// helper now produces a clean, prefix-free message.
///
/// Exposed `pub` so downstream test fixtures (in particular the
/// verify-side regression tests in `crates/aiken-project/src/verify/tests.rs`)
/// can construct the diagnostic string in lockstep with the typed
/// `OpaqueCode::ConstructorTagUnresolved { … }` payload they attach. The
/// previous `pub fn s0002_reason_message_for_test` test wrapper has been
/// retired because the bare function is now safe to expose.
pub fn s0002_reason_message(ctor: &str, type_name: &str) -> String {
    format!(
        "constructor '{ctor}' of type '{type_name}' not in data-type registry; \
         cannot resolve UPLC tag (S0002)"
    )
}

/// Best-effort human-readable name for a `Type`, walking through
/// `Type::Fn { ret, .. }` and `TypeVar::Link` indirections to find the
/// underlying `Type::App { module, name, .. }` qualifier. Returns
/// `"<module>.<Type>"` if found, falling back to the printer's pretty
/// representation. Used purely for diagnostic message text in
/// [`s0002_reason_message`]; never affects program semantics.
pub(super) fn describe_tipo(tipo: &Type) -> String {
    match tipo {
        Type::Fn { ret, .. } => describe_tipo(ret.as_ref()),
        Type::App { module, name, .. } => {
            if module.is_empty() {
                name.clone()
            } else {
                format!("{module}.{name}")
            }
        }
        Type::Var { tipo: var, .. } => {
            // `var` is `Rc<RefCell<TypeVar>>`. `RefCell::borrow` shares a
            // method name with `std::borrow::Borrow`, which is in scope
            // at module level — disambiguate by going through `RefCell`
            // explicitly (mirrors `lookup_data_type_by_tipo`'s walk over
            // `Type::Var`).
            if let TypeVar::Link { tipo } =
                <_ as core::ops::Deref>::deref(&std::cell::RefCell::borrow(var.as_ref()))
            {
                describe_tipo(tipo.as_ref())
            } else {
                tipo.to_pretty(0)
            }
        }
        _ => tipo.to_pretty(0),
    }
}

pub(super) fn data_with_schema_type_name(tipo: &Type) -> Option<String> {
    match tipo {
        Type::Fn { ret, .. } => data_with_schema_type_name(ret.as_ref()),
        Type::App {
            module, name, args, ..
        } => {
            let base = if module.is_empty() {
                name.clone()
            } else {
                format!("{module}.{name}")
            };
            if args.is_empty() {
                Some(base)
            } else {
                let rendered_args = args
                    .iter()
                    .map(|arg| {
                        data_with_schema_type_name(arg.as_ref()).unwrap_or_else(|| arg.to_pretty(0))
                    })
                    .collect::<Vec<_>>()
                    .join(",");
                Some(format!("{base}<{rendered_args}>"))
            }
        }
        Type::Var { tipo, .. } => {
            if let TypeVar::Link { tipo } =
                <_ as core::ops::Deref>::deref(&std::cell::RefCell::borrow(tipo.as_ref()))
            {
                data_with_schema_type_name(tipo.as_ref())
            } else {
                None
            }
        }
        Type::Tuple { elems, .. } => Some(format!(
            "({})",
            elems
                .iter()
                .map(|elem| data_with_schema_type_name(elem.as_ref())
                    .unwrap_or_else(|| elem.to_pretty(0)))
                .collect::<Vec<_>>()
                .join(",")
        )),
        Type::Pair { fst, snd, .. } => Some(format!(
            "Pair<{},{}>",
            data_with_schema_type_name(fst.as_ref()).unwrap_or_else(|| fst.to_pretty(0)),
            data_with_schema_type_name(snd.as_ref()).unwrap_or_else(|| snd.to_pretty(0))
        )),
    }
}

pub(super) fn describe_binop(op: &BinOp) -> &'static str {
    match op {
        BinOp::And => "&&",
        BinOp::Or => "||",
        BinOp::Eq => "==",
        BinOp::NotEq => "!=",
        BinOp::LtInt => "<",
        BinOp::LtEqInt => "<=",
        BinOp::GtEqInt => ">=",
        BinOp::GtInt => ">",
        BinOp::AddInt => "+",
        BinOp::SubInt => "-",
        BinOp::MultInt => "*",
        BinOp::DivInt => "/",
        BinOp::ModInt => "%",
    }
}

pub(super) fn describe_acceptance(acceptance: &StateMachineAcceptance) -> &'static str {
    match acceptance {
        StateMachineAcceptance::AcceptsSuccess => "accepts success",
        StateMachineAcceptance::AcceptsFailure => "accepts failure",
    }
}

pub(super) fn describe_exact_value(value: &FuzzerExactValue) -> String {
    match value {
        FuzzerExactValue::Bool(value) => value.to_string(),
        FuzzerExactValue::ByteArray(bytes) => format!("0x{}", hex::encode(bytes)),
        FuzzerExactValue::String(value) => format!("\"{value}\""),
    }
}

pub(super) fn describe_semantic_type(tipo: &SemanticType) -> String {
    match tipo {
        SemanticType::Int => "Int".to_string(),
        SemanticType::Bool => "Bool".to_string(),
        SemanticType::ByteArray => "ByteArray".to_string(),
        SemanticType::String => "String".to_string(),
        SemanticType::Data => "Data".to_string(),
        SemanticType::List(inner) => format!("List<{}>", describe_semantic_type(inner)),
        SemanticType::Tuple(items) => format!(
            "({})",
            items
                .iter()
                .map(describe_semantic_type)
                .collect::<Vec<_>>()
                .join(", ")
        ),
        SemanticType::Pair(first, second) => format!(
            "Pair<{}, {}>",
            describe_semantic_type(first),
            describe_semantic_type(second)
        ),
        SemanticType::Unsupported(name) => format!("unsupported({name})"),
    }
}

pub(super) fn describe_semantics(semantics: &FuzzerSemantics) -> String {
    match semantics {
        FuzzerSemantics::Bool => "Bool".to_string(),
        FuzzerSemantics::IntRange { min, max } => match (min, max) {
            (Some(min), Some(max)) => format!("IntRange[{min}, {max}]"),
            (Some(min), None) => format!("IntRange[{min}, ∞]"),
            (None, Some(max)) => format!("IntRange[-∞, {max}]"),
            (None, None) => "Int".to_string(),
        },
        FuzzerSemantics::ByteArrayRange { min_len, max_len } => match (min_len, max_len) {
            (Some(min), Some(max)) => format!("ByteArray(length {min}..{max})"),
            (Some(min), None) => format!("ByteArray(length >= {min})"),
            (None, Some(max)) => format!("ByteArray(length <= {max})"),
            (None, None) => "ByteArray".to_string(),
        },
        FuzzerSemantics::String => "String".to_string(),
        FuzzerSemantics::Data => "Data".to_string(),
        FuzzerSemantics::DataWithSchema { type_name } => format!("DataWithSchema<{type_name}>"),
        FuzzerSemantics::Exact(value) => format!("Exact({})", describe_exact_value(value)),
        FuzzerSemantics::OneOf(values) => format!(
            "OneOf({})",
            values
                .iter()
                .map(describe_exact_value)
                .collect::<Vec<_>>()
                .join(", ")
        ),
        FuzzerSemantics::Product(items) => format!(
            "Product({})",
            items
                .iter()
                .map(describe_semantics)
                .collect::<Vec<_>>()
                .join(", ")
        ),
        FuzzerSemantics::List {
            element,
            min_len,
            max_len,
        } => match (min_len, max_len) {
            (Some(min), Some(max)) => {
                format!("List<{}>(length {min}..{max})", describe_semantics(element))
            }
            (Some(min), None) => format!("List<{}>(length >= {min})", describe_semantics(element)),
            (None, Some(max)) => format!("List<{}>(length <= {max})", describe_semantics(element)),
            (None, None) => format!("List<{}>", describe_semantics(element)),
        },
        FuzzerSemantics::Constructors { tags } => format!(
            "Constructors(tags={})",
            tags.iter()
                .map(ToString::to_string)
                .collect::<Vec<_>>()
                .join(", ")
        ),
        FuzzerSemantics::StateMachineTrace {
            acceptance,
            state_type,
            output_semantics,
            ..
        } => format!(
            "StateMachineTrace(acceptance={}, state={}, output={})",
            describe_acceptance(acceptance),
            describe_semantic_type(state_type),
            describe_semantics(output_semantics)
        ),
        FuzzerSemantics::Opaque { reason } => format!("Opaque({reason})"),
    }
}

pub(super) fn shallow_ir_type_from_semantics(semantics: &FuzzerSemantics) -> ShallowIrType {
    match semantics {
        FuzzerSemantics::Bool => ShallowIrType::Bool,
        FuzzerSemantics::IntRange { .. } => ShallowIrType::Int,
        FuzzerSemantics::ByteArrayRange { .. } => ShallowIrType::ByteArray,
        FuzzerSemantics::String => ShallowIrType::String,
        FuzzerSemantics::Data | FuzzerSemantics::DataWithSchema { .. } => ShallowIrType::Data,
        FuzzerSemantics::Exact(FuzzerExactValue::Bool(_)) => ShallowIrType::Bool,
        FuzzerSemantics::Exact(FuzzerExactValue::ByteArray(_)) => ShallowIrType::ByteArray,
        FuzzerSemantics::Exact(FuzzerExactValue::String(_)) => ShallowIrType::String,
        FuzzerSemantics::OneOf(values) => {
            values
                .first()
                .map_or(ShallowIrType::Data, |value| match value {
                    FuzzerExactValue::Bool(_) => ShallowIrType::Bool,
                    FuzzerExactValue::ByteArray(_) => ShallowIrType::ByteArray,
                    FuzzerExactValue::String(_) => ShallowIrType::String,
                })
        }
        FuzzerSemantics::Product(items) if items.len() == 2 => ShallowIrType::Pair(
            Box::new(shallow_ir_type_from_semantics(&items[0])),
            Box::new(shallow_ir_type_from_semantics(&items[1])),
        ),
        FuzzerSemantics::Product(_) => ShallowIrType::Data,
        FuzzerSemantics::List { element, .. } => {
            ShallowIrType::List(Box::new(shallow_ir_type_from_semantics(element)))
        }
        FuzzerSemantics::Constructors { .. }
        | FuzzerSemantics::StateMachineTrace { .. }
        | FuzzerSemantics::Opaque { .. } => ShallowIrType::Data,
    }
}

/// A `ShallowIr` tree is *vacuous* when the verifier cannot emit it as an exact
/// counted-output `Data` expression on the theorem path.
///
/// This is stricter than “contains some structure”: a root only counts as
/// non-vacuous when the downstream `verify.rs` emitter can preserve the value
/// relationship without freshening it to a new existential witness. Typed-code
/// `Opaque` markers (for example `S0002`) remain a carve-out: they are not
/// exact data, but they must survive to the verify-side hard-error dispatcher.
pub(super) fn shallow_ir_has_known_constructor_layout(ir: &ShallowIr) -> bool {
    match ir {
        ShallowIr::Construct { .. } => true,
        ShallowIr::RecordUpdate {
            record, updates, ..
        } => {
            shallow_ir_has_known_constructor_layout(record)
                && updates
                    .iter()
                    .all(|update| shallow_ir_is_exact_data_expr(&update.value))
        }
        ShallowIr::BoundVar {
            ty: ShallowIrType::Adt(_),
            ..
        } => true,
        ShallowIr::FieldAccess {
            record,
            ty: ShallowIrType::Adt(_),
            ..
        } => shallow_ir_is_exact_data_expr(record),
        ShallowIr::Let { body, .. } => shallow_ir_has_known_constructor_layout(body),
        ShallowIr::If {
            then_branch,
            else_branch,
            ..
        } => {
            shallow_ir_has_known_constructor_layout(then_branch)
                && shallow_ir_has_known_constructor_layout(else_branch)
        }
        ShallowIr::Match { arms, .. } => {
            !arms.is_empty()
                && arms
                    .iter()
                    .all(|arm| shallow_ir_has_known_constructor_layout(&arm.body))
        }
        _ => false,
    }
}

pub(super) fn shallow_ir_field_access_is_exact(
    record: &ShallowIr,
    index: u64,
    kind: ShallowFieldAccessKind,
) -> bool {
    if let Some(projected) = exact_field_ir_from_shallow_ir(record, index, kind) {
        return shallow_ir_is_exact_data_expr(&projected);
    }

    match (record, kind) {
        (
            ShallowIr::BoundVar {
                ty: ShallowIrType::Pair(_, _),
                ..
            },
            ShallowFieldAccessKind::ListElement,
        ) => index < 2,
        (_, ShallowFieldAccessKind::ConstructorField) => {
            shallow_ir_has_known_constructor_layout(record)
        }
        _ => false,
    }
}

pub(super) fn shallow_ir_is_exact_int_expr(ir: &ShallowIr) -> bool {
    if find_first_typed_opaque_in_shallow_ir(ir).is_some() {
        return true;
    }

    match ir {
        ShallowIr::Const(ShallowConst::Int(_)) => true,
        ShallowIr::BoundVar {
            ty: ShallowIrType::Int,
            ..
        } => true,
        ShallowIr::FieldAccess {
            record,
            index,
            kind,
            ty: ShallowIrType::Int,
            ..
        } => shallow_ir_field_access_is_exact(record, *index, *kind),
        ShallowIr::BinOp { op, left, right } => match op {
            ShallowBinOp::Add
            | ShallowBinOp::Sub
            | ShallowBinOp::Mul
            | ShallowBinOp::Div
            | ShallowBinOp::Mod => {
                shallow_ir_is_exact_int_expr(left) && shallow_ir_is_exact_int_expr(right)
            }
            _ => false,
        },
        ShallowIr::Let { value, body, .. } => {
            shallow_ir_is_exact_data_expr(value) && shallow_ir_is_exact_int_expr(body)
        }
        ShallowIr::If {
            cond,
            then_branch,
            else_branch,
        } => {
            shallow_ir_is_exact_bool_expr(cond)
                && shallow_ir_is_exact_int_expr(then_branch)
                && shallow_ir_is_exact_int_expr(else_branch)
        }
        ShallowIr::Match { subject, arms } => {
            shallow_ir_is_exact_data_expr(subject)
                && arms
                    .iter()
                    .all(|arm| shallow_ir_is_exact_int_expr(&arm.body))
        }
        _ => false,
    }
}

pub(super) fn shallow_ir_is_exact_bool_expr(ir: &ShallowIr) -> bool {
    if find_first_typed_opaque_in_shallow_ir(ir).is_some() {
        return true;
    }

    match ir {
        ShallowIr::Const(ShallowConst::Bool(_)) => true,
        ShallowIr::BoundVar {
            ty: ShallowIrType::Bool,
            ..
        } => true,
        ShallowIr::FieldAccess {
            record,
            index,
            kind,
            ty: ShallowIrType::Bool,
            ..
        } => shallow_ir_field_access_is_exact(record, *index, *kind),
        ShallowIr::BinOp { op, left, right } => match op {
            ShallowBinOp::And | ShallowBinOp::Or => {
                shallow_ir_is_exact_bool_expr(left) && shallow_ir_is_exact_bool_expr(right)
            }
            ShallowBinOp::Eq | ShallowBinOp::NotEq => {
                (shallow_ir_is_exact_int_expr(left) && shallow_ir_is_exact_int_expr(right))
                    || (shallow_ir_is_exact_data_expr(left) && shallow_ir_is_exact_data_expr(right))
            }
            ShallowBinOp::Lt | ShallowBinOp::LtEq | ShallowBinOp::Gt | ShallowBinOp::GtEq => {
                shallow_ir_is_exact_int_expr(left) && shallow_ir_is_exact_int_expr(right)
            }
            _ => false,
        },
        ShallowIr::Match { subject, arms } => {
            shallow_ir_is_exact_data_expr(subject)
                && arms
                    .iter()
                    .all(|arm| shallow_ir_is_exact_data_expr(&arm.body))
        }
        _ => false,
    }
}

pub(super) fn shallow_ir_is_exact_data_expr(ir: &ShallowIr) -> bool {
    if find_first_typed_opaque_in_shallow_ir(ir).is_some() {
        return true;
    }

    match ir {
        ShallowIr::Const(_) | ShallowIr::BoundVar { .. } => true,
        ShallowIr::Construct { fields, .. } => fields.iter().all(shallow_ir_is_exact_data_expr),
        ShallowIr::Tuple(elems) => elems.iter().all(shallow_ir_is_exact_data_expr),
        ShallowIr::ListLit { elements, tail, .. } => {
            elements.iter().all(shallow_ir_is_exact_data_expr)
                && tail
                    .as_ref()
                    .is_none_or(|tail| shallow_ir_is_exact_data_expr(tail.as_ref()))
        }
        ShallowIr::FieldAccess {
            record,
            index,
            kind,
            ..
        } => shallow_ir_field_access_is_exact(record, *index, *kind),
        ShallowIr::RecordUpdate {
            record, updates, ..
        } => {
            shallow_ir_is_exact_data_expr(record)
                && updates
                    .iter()
                    .all(|update| shallow_ir_is_exact_data_expr(&update.value))
        }
        ShallowIr::BinOp { op, left, right } => match op {
            ShallowBinOp::Append => {
                shallow_ir_is_exact_data_expr(left) && shallow_ir_is_exact_data_expr(right)
            }
            ShallowBinOp::Add
            | ShallowBinOp::Sub
            | ShallowBinOp::Mul
            | ShallowBinOp::Div
            | ShallowBinOp::Mod => {
                shallow_ir_is_exact_int_expr(left) && shallow_ir_is_exact_int_expr(right)
            }
            _ => false,
        },
        ShallowIr::Let { value, body, .. } => {
            shallow_ir_is_exact_data_expr(value) && shallow_ir_is_exact_data_expr(body)
        }
        ShallowIr::If {
            cond,
            then_branch,
            else_branch,
        } => {
            shallow_ir_is_exact_bool_expr(cond)
                && shallow_ir_is_exact_data_expr(then_branch)
                && shallow_ir_is_exact_data_expr(else_branch)
        }
        ShallowIr::Match { subject, arms } => {
            shallow_ir_is_exact_data_expr(subject)
                && arms
                    .iter()
                    .all(|arm| shallow_ir_is_exact_data_expr(&arm.body))
        }
        ShallowIr::Var { .. } | ShallowIr::FuzzExistential { .. } | ShallowIr::Opaque { .. } => {
            false
        }
    }
}

pub(super) fn shallow_ir_is_vacuous(ir: &ShallowIr) -> bool {
    !shallow_ir_is_exact_data_expr(ir)
}

pub(super) fn shallow_ir_type(tipo: &Rc<Type>) -> ShallowIrType {
    match tipo.as_ref() {
        Type::App {
            module, name, args, ..
        } => match (module.as_str(), name.as_str()) {
            ("", "Int") | ("aiken", "Int") => ShallowIrType::Int,
            ("", "Bool") | ("aiken", "Bool") => ShallowIrType::Bool,
            ("", "ByteArray") | ("aiken", "ByteArray") => ShallowIrType::ByteArray,
            ("", "String") | ("aiken", "String") => ShallowIrType::String,
            ("", "Void") | ("aiken", "Void") => ShallowIrType::Unit,
            ("", "Data") | ("aiken", "Data") => ShallowIrType::Data,
            ("aiken", "List") | ("", "List") if args.len() == 1 => {
                ShallowIrType::List(Box::new(shallow_ir_type(&args[0])))
            }
            ("aiken", "Pair") | ("", "Pair") if args.len() == 2 => ShallowIrType::Pair(
                Box::new(shallow_ir_type(&args[0])),
                Box::new(shallow_ir_type(&args[1])),
            ),
            (mod_, name_) => ShallowIrType::Adt(format!("{mod_}/{name_}")),
        },
        Type::Tuple { elems, .. } if elems.len() == 2 => ShallowIrType::Pair(
            Box::new(shallow_ir_type(&elems[0])),
            Box::new(shallow_ir_type(&elems[1])),
        ),
        Type::Pair { fst, snd, .. } => ShallowIrType::Pair(
            Box::new(shallow_ir_type(fst)),
            Box::new(shallow_ir_type(snd)),
        ),
        _ => ShallowIrType::Unknown,
    }
}

pub(super) fn translate_sequence(
    exprs: &[TypedExpr],
    data_types: &IndexMap<&DataTypeKey, &TypedDataType>,
) -> ShallowIr {
    match exprs {
        [] => ShallowIr::Const(ShallowConst::Unit),
        [single] => typed_expr_to_shallow_ir(single, data_types),
        [
            TypedExpr::Assignment {
                value,
                pattern: TypedPattern::Var { name, .. },
                ..
            },
            rest @ ..,
        ] => ShallowIr::Let {
            name: name.clone(),
            value: Box::new(typed_expr_to_shallow_ir(value, data_types)),
            body: Box::new(translate_sequence(rest, data_types)),
        },
        [head, rest @ ..] => {
            // Non-binding expression; treat as let _ = head in rest
            ShallowIr::Let {
                name: "_".into(),
                value: Box::new(typed_expr_to_shallow_ir(head, data_types)),
                body: Box::new(translate_sequence(rest, data_types)),
            }
        }
    }
}

pub(super) fn translate_if_chain(
    branches: &Vec1<TypedIfBranch>,
    final_else: &TypedExpr,
    data_types: &IndexMap<&DataTypeKey, &TypedDataType>,
) -> ShallowIr {
    let mut result = typed_expr_to_shallow_ir(final_else, data_types);
    for branch in branches.iter().rev() {
        result = ShallowIr::If {
            cond: Box::new(typed_expr_to_shallow_ir(&branch.condition, data_types)),
            then_branch: Box::new(typed_expr_to_shallow_ir(&branch.body, data_types)),
            else_branch: Box::new(result),
        };
    }
    result
}

/// Failure result from `translate_clause` when the clause's constructor
/// cannot be resolved. Carries both the human-readable diagnostic message
/// (for audit logs and fallback) and the typed dispatch code (used by the
/// verify pipeline to raise a hard `S0002` error).
pub(super) struct ClauseTranslationFailure {
    pub(super) reason: String,
    pub(super) code: OpaqueCode,
}

pub(super) fn translate_clause(
    clause: &TypedClause,
    subject_expr: &TypedExpr,
    subject_tipo: &Rc<Type>,
    data_types: &IndexMap<&DataTypeKey, &TypedDataType>,
) -> Result<ShallowIrArm, ClauseTranslationFailure> {
    let (tag, bindings) = match &clause.pattern {
        TypedPattern::Constructor {
            name: ctor_name,
            arguments,
            ..
        } => {
            // Resolve the canonical UPLC tag for this pattern via the data-
            // type registry. If the constructor is not in the registry we
            // bail out with a typed `OpaqueCode::ConstructorTagUnresolved`
            // payload so the caller can collapse the entire `Match` to
            // `Opaque` rather than emit a vacuously-satisfiable arm at tag 0.
            let tag =
                resolve_constructor_tag(subject_tipo, ctor_name, data_types).ok_or_else(|| {
                    let type_name = describe_tipo(subject_tipo);
                    ClauseTranslationFailure {
                        reason: s0002_reason_message(ctor_name, &type_name),
                        code: OpaqueCode::ConstructorTagUnresolved {
                            ctor: ctor_name.clone(),
                            type_name,
                        },
                    }
                })?;
            let bindings: Vec<String> = arguments
                .iter()
                .map(|arg| match &arg.value {
                    TypedPattern::Var { name, .. } => name.clone(),
                    _ => "_".into(),
                })
                .collect();
            (Some(tag), bindings)
        }
        TypedPattern::Var { name, .. } => (None, vec![name.clone()]),
        TypedPattern::Discard { .. } => (None, vec!["_".into()]),
        _ => (None, vec![]),
    };
    let subject_ir = typed_expr_to_shallow_ir(subject_expr, data_types);
    let guard = pattern_refinement_guard(&clause.pattern, &subject_ir, data_types)?;
    let mut clause_locals = BTreeMap::new();
    bind_pattern_to_locals(
        &clause.pattern,
        &LocalBinding::PureExpr(subject_expr.clone()),
        data_types,
        &mut clause_locals,
    );
    let mut visiting = BTreeSet::new();
    Ok(ShallowIrArm {
        tag,
        bindings,
        guard,
        body: typed_expr_to_shallow_ir_with_locals(
            &clause.then,
            data_types,
            &clause_locals,
            &mut visiting,
        ),
    })
}

pub(super) fn translate_call(
    fun: &TypedExpr,
    args: &[CallArg<TypedExpr>],
    tipo: &Rc<Type>,
    data_types: &IndexMap<&DataTypeKey, &TypedDataType>,
) -> ShallowIr {
    // Check for fuzzer combinator calls — these become FuzzExistential
    if let Some(fuzz_ir) = try_fuzz_existential(fun, args, tipo) {
        return fuzz_ir;
    }
    if extract_module_fn_identity(fun)
        .is_some_and(|(module, name)| module.is_empty() && name == "as_data")
        && let [value] = args
    {
        return typed_expr_to_shallow_ir(&value.value, data_types);
    }
    if extract_module_fn_identity(fun)
        .is_some_and(|(module, name)| module == "aiken/collection/list" && name == "concat")
        && let [left, right] = args
    {
        return ShallowIr::BinOp {
            op: ShallowBinOp::Append,
            left: Box::new(typed_expr_to_shallow_ir(&left.value, data_types)),
            right: Box::new(typed_expr_to_shallow_ir(&right.value, data_types)),
        };
    }

    // Constructor call: Var that resolves to a constructor
    if let TypedExpr::Var { constructor, .. } = fun {
        if let ValueConstructorVariant::Record {
            name: ctor_name,
            arity,
            module,
            ..
        } = &constructor.variant
        {
            if *arity == args.len() {
                // For n-ary constructors, `constructor.tipo` is
                // `Type::Fn { ret: ADT, .. }` — `lookup_data_type_by_tipo`
                // walks through the `ret` automatically. If lookup fails,
                // route through `Opaque` with the S0002 marker so the
                // verify pipeline raises a hard `ConstructorTagUnresolved`
                // error rather than silently producing a vacuously-true
                // `EqOutput` constraint.
                return match resolve_constructor_tag(&constructor.tipo, ctor_name, data_types) {
                    Some(tag) => ShallowIr::Construct {
                        module: module.clone(),
                        constructor: ctor_name.clone(),
                        tag,
                        fields: args
                            .iter()
                            .map(|a| typed_expr_to_shallow_ir(&a.value, data_types))
                            .collect(),
                    },
                    None => {
                        let type_name = describe_tipo(&constructor.tipo);
                        ShallowIr::Opaque {
                            ty: shallow_ir_type(tipo),
                            reason: s0002_reason_message(ctor_name, &type_name),
                            code: Some(OpaqueCode::ConstructorTagUnresolved {
                                ctor: ctor_name.clone(),
                                type_name,
                            }),
                        }
                    }
                };
            }
        }
    }

    // Module select (qualified constructor)
    if let TypedExpr::ModuleSelect {
        constructor:
            ModuleValueConstructor::Record {
                name: ctor_name,
                arity,
                tipo: ctor_tipo,
                ..
            },
        module_alias,
        ..
    } = fun
    {
        if *arity == args.len() {
            return match resolve_constructor_tag(ctor_tipo, ctor_name, data_types) {
                Some(tag) => ShallowIr::Construct {
                    module: module_alias.clone(),
                    constructor: ctor_name.clone(),
                    tag,
                    fields: args
                        .iter()
                        .map(|a| typed_expr_to_shallow_ir(&a.value, data_types))
                        .collect(),
                },
                None => {
                    let type_name = describe_tipo(ctor_tipo);
                    ShallowIr::Opaque {
                        ty: shallow_ir_type(tipo),
                        reason: s0002_reason_message(ctor_name, &type_name),
                        code: Some(OpaqueCode::ConstructorTagUnresolved {
                            ctor: ctor_name.clone(),
                            type_name,
                        }),
                    }
                }
            };
        }
    }

    // Unknown call — opaque
    ShallowIr::Opaque {
        ty: shallow_ir_type(tipo),
        reason: "unrecognised function call".into(),
        code: None,
    }
}

pub(super) fn try_fuzz_existential(
    fun: &TypedExpr,
    args: &[CallArg<TypedExpr>],
    tipo: &Rc<Type>,
) -> Option<ShallowIr> {
    let Some((module, fn_name)) = extract_module_fn_identity(fun) else {
        return None;
    };

    if module != STDLIB_FUZZ_MODULE {
        return None;
    }

    // In Aiken, type constructors start with an uppercase letter while
    // functions/combinators start with lowercase. A call from a "fuzz" module
    // whose name starts with uppercase is a data-constructor call (e.g.
    // `Scenario.Step(...)` from `aiken/fuzz/scenario`), NOT a fuzzer combinator.
    // Returning `None` here lets `translate_call` handle it as `ShallowIr::Construct`
    // instead of mistakenly emitting a `FuzzExistential`.
    if fn_name.starts_with(|c: char| c.is_uppercase()) {
        return None;
    }

    let Some(payload_tipo) = extract_fuzzer_payload_type(tipo.as_ref()) else {
        return None;
    };
    let payload_ty = shallow_ir_type(&payload_tipo);

    let (lo, hi) = match fn_name.as_str() {
        "int_between" if args.len() == 2 => {
            let lo = const_int_value(&args[0].value);
            let hi = const_int_value(&args[1].value);
            if lo.is_none() || hi.is_none() {
                return Some(ShallowIr::Opaque {
                    ty: shallow_ir_type(tipo),
                    reason: "fuzz.int_between bounds are outside the verifier existential's i64 precision"
                        .into(),
                    code: None,
                });
            }
            (lo, hi)
        }
        "int_at_least" if args.len() == 1 => {
            let lo = const_int_value(&args[0].value);
            if lo.is_none() {
                return Some(ShallowIr::Opaque {
                    ty: shallow_ir_type(tipo),
                    reason: "fuzz.int_at_least bound is outside the verifier existential's i64 precision"
                        .into(),
                    code: None,
                });
            }
            (lo, None)
        }
        "int_at_most" if args.len() == 1 => {
            let hi = const_int_value(&args[0].value);
            if hi.is_none() {
                return Some(ShallowIr::Opaque {
                    ty: shallow_ir_type(tipo),
                    reason:
                        "fuzz.int_at_most bound is outside the verifier existential's i64 precision"
                            .into(),
                    code: None,
                });
            }
            (None, hi)
        }
        _ => (None, None),
    };

    Some(ShallowIr::FuzzExistential {
        kind: payload_ty,
        lo,
        hi,
    })
}

pub(super) fn const_int_value(expr: &TypedExpr) -> Option<i64> {
    match expr {
        TypedExpr::UInt { value, .. } => value.parse().ok(),
        TypedExpr::UnOp {
            op: UnOp::Negate,
            value,
            ..
        } => const_int_value(value).and_then(i64::checked_neg),
        _ => None,
    }
}

pub(super) fn translate_binop(op: &BinOp) -> Option<ShallowBinOp> {
    Some(match op {
        BinOp::AddInt => ShallowBinOp::Add,
        BinOp::SubInt => ShallowBinOp::Sub,
        BinOp::MultInt => ShallowBinOp::Mul,
        BinOp::DivInt => ShallowBinOp::Div,
        BinOp::ModInt => ShallowBinOp::Mod,
        BinOp::Eq => ShallowBinOp::Eq,
        BinOp::NotEq => ShallowBinOp::NotEq,
        BinOp::LtInt => ShallowBinOp::Lt,
        BinOp::LtEqInt => ShallowBinOp::LtEq,
        BinOp::GtInt => ShallowBinOp::Gt,
        BinOp::GtEqInt => ShallowBinOp::GtEq,
        BinOp::And => ShallowBinOp::And,
        BinOp::Or => ShallowBinOp::Or,
    })
}
