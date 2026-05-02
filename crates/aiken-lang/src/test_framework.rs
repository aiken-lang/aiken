#![allow(clippy::collapsible_if)]
// The verifier-lowering code keeps nested guard checks explicit so each extraction branch remains auditable.

#[cfg(test)]
use crate::ast::{Annotation, RecordConstructor, RecordConstructorArg};
use crate::{
    ast::{
        BinOp, DataTypeKey, FunctionAccessKey, IfBranch, OnTestFailure, Span, TraceLevel, Tracing,
        TypedArg, TypedClause, TypedDataType, TypedFunction, TypedIfBranch, TypedPattern,
        TypedTest, UnOp,
    },
    expr::{CallArg, TypedExpr, UntypedExpr},
    format::Formatter,
    gen_uplc::{CodeGenerator, builder::get_constr_index_variant},
    parser::token::Base,
    plutus_version::PlutusVersion,
    tipo::{
        ModuleValueConstructor, Type, TypeVar, ValueConstructorVariant, convert_opaque_type,
        find_and_replace_generics, get_generic_id_and_type, lookup_data_type_by_tipo,
        pretty::Printer,
    },
};
use cryptoxide::{blake2b::Blake2b, digest::Digest};
use indexmap::IndexMap;
use itertools::Itertools;
use num_bigint::BigInt;
use owo_colors::{OwoColorize, Stream, Stream::Stderr};
use pallas_primitives::alonzo::{Constr, PlutusData};
use patricia_tree::PatriciaMap;
#[cfg(not(target_family = "wasm"))]
use std::time::Duration;
use std::{
    borrow::Borrow,
    cmp::Ordering,
    collections::{BTreeMap, BTreeSet, HashMap},
    convert::TryFrom,
    fmt::{Debug, Display},
    ops::Deref,
    path::PathBuf,
    rc::Rc,
};
use uplc::{
    ast::{Constant, Data, Name, NamedDeBruijn, Program, Term},
    machine::{cost_model::ExBudget, eval_result::EvalResult},
};
use vec1::{Vec1, vec1};

const STDLIB_FUZZ_MODULE: &str = "aiken/fuzz";
const MAX_FINITE_MAPPER_SOURCE_CASES: usize = 64;
const MAX_FINITE_DOMAIN_CASES: usize = 64;
#[cfg(test)]
const STDLIB_FUZZ_SCENARIO_MODULE: &str = "aiken/fuzz/scenario";

/// A simplified intermediate representation of a step function body, suitable
/// for shallow translation into Lean 4 definitions. Only the constructs
/// commonly found in state-machine step functions are represented; everything
/// else becomes `Opaque`.
#[non_exhaustive]
#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
pub enum ShallowIr {
    /// Integer / UInt / Bool / ByteArray / String literal
    Const(ShallowConst),
    /// Local variable reference
    Var { name: String, ty: ShallowIrType },
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
    },
    /// Record update `{ rec | field: val, ... }`
    RecordUpdate {
        record: Box<ShallowIr>,
        updates: Vec<(String, ShallowIr)>,
    },
    /// Binary operator
    BinOp {
        op: ShallowBinOp,
        left: Box<ShallowIr>,
        right: Box<ShallowIr>,
    },
    /// Tuple / pair literal
    Tuple(Vec<ShallowIr>),
    /// List literal
    ListLit {
        elements: Vec<ShallowIr>,
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

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
pub struct ShallowIrArm {
    /// Constructor tag this arm matches (None for a catch-all variable pattern)
    pub tag: Option<u64>,
    /// Bound variable names (one per field, in order)
    pub bindings: Vec<String>,
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
                .map(|c| translate_clause(c, &subject_tipo, data_types))
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

        TypedExpr::List { elements, .. } => ShallowIr::ListLit {
            elements: elements
                .iter()
                .map(|e| typed_expr_to_shallow_ir(e, data_types))
                .collect(),
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
        } => match resolve_constructor_tag(tipo, ctor_name, data_types) {
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
        },

        // Everything else: opaque
        _ => ShallowIr::Opaque {
            ty: shallow_ir_type(&expr.tipo()),
            reason: "unsupported TypedExpr variant".to_string(),
            code: None,
        },
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
fn resolve_constructor_tag(
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
fn describe_tipo(tipo: &Type) -> String {
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

fn data_with_schema_type_name(tipo: &Type) -> Option<String> {
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

fn describe_binop(op: &BinOp) -> &'static str {
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

fn describe_acceptance(acceptance: &StateMachineAcceptance) -> &'static str {
    match acceptance {
        StateMachineAcceptance::AcceptsSuccess => "accepts success",
        StateMachineAcceptance::AcceptsFailure => "accepts failure",
    }
}

fn describe_exact_value(value: &FuzzerExactValue) -> String {
    match value {
        FuzzerExactValue::Bool(value) => value.to_string(),
        FuzzerExactValue::ByteArray(bytes) => format!("0x{}", hex::encode(bytes)),
        FuzzerExactValue::String(value) => format!("\"{value}\""),
    }
}

fn describe_semantic_type(tipo: &SemanticType) -> String {
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

fn describe_semantics(semantics: &FuzzerSemantics) -> String {
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

/// A `ShallowIr` tree is *vacuous* when the emitter in `verify.rs` would
/// reify its root as a fresh `Data` existential `∃ v : Data, transition = v`
/// instead of a structural equality.  Handing such a trivially-True
/// predicate to Blaster as the precondition of a halt theorem gives no
/// additional precision but forces Z3 to chew through the encoding,
/// which the 84c32cae commit message recorded as a cause of hangs.
///
/// The root shapes currently widened to fresh existentials by
/// `emit_shallow_ir_as_lean_data` in `verify.rs` are:
///   - `ShallowIr::Var`       (unresolved reference, e.g. a named step fn)
///   - `ShallowIr::Opaque`    (translator did not recognise this shape)
///   - `ShallowIr::If`        (no direct Data-level literal form)
///   - `ShallowIr::Match`     (ditto)
///   - `ShallowIr::FieldAccess`/`RecordUpdate`/`BinOp` (ditto)
///
/// `Let` is not itself vacuous — the emitter passes through to its body —
/// so we strip `Let` layers before classifying the root.  A tree rooted
/// at `Construct`, `Tuple`, `ListLit`, or `Const` is not vacuous: the
/// emitter produces a structural `Data.Constr`/`Data.List`/primitive
/// expression that yields a useful `transition = <constructor>` Lean
/// predicate.  For those the step_fn path is safe to take.
///
/// S1 deliberately classifies monadic step bodies (which begin with
/// `TypedExpr::If` or sequenced `Let`-into-`If`) as vacuous: the fuzzer
/// combinators inside still erase to `ctx.fresh(Data)` in the current
/// emitter.  S2 (combinator recognizers) is expected to enrich the IR so
/// this predicate can return `false` for those cases as well.
///
/// Typed-code carve-out: an `Opaque` carrying a typed `OpaqueCode` (today,
/// `ConstructorTagUnresolved` for S0002) is *not* vacuous. Even though its
/// emitter shape is identical to a generic `Opaque` (both reduce to a
/// fresh existential), the verify-side dispatcher in
/// `aiken-project::verify::build_state_machine_trace_reachability_helpers`
/// must inspect the typed payload to raise a hard, non-skippable error.
/// Treating typed-code `Opaque`s as vacuous would let the upstream filter
/// in `state_machine_trace_from_test_arguments` swallow the marker,
/// leaving the user with a generic skippable `FallbackRequired` rather
/// than the promised hard error. All other `Opaque` shapes remain
/// vacuous.
fn shallow_ir_is_vacuous(ir: &ShallowIr) -> bool {
    let mut current = ir;
    loop {
        match current {
            ShallowIr::Let { body, .. } => current = body.as_ref(),
            // S0002 carve-out: typed-code-bearing `Opaque` must reach the
            // verify-side dispatcher to emit a hard error rather than be
            // absorbed as a vacuous existential. Today only
            // `OpaqueCode::ConstructorTagUnresolved` is defined; future
            // typed codes will share this carve-out.
            ShallowIr::Opaque { code: Some(_), .. } => {
                return false;
            }
            ShallowIr::Var { .. }
            | ShallowIr::Opaque { .. }
            | ShallowIr::If { .. }
            | ShallowIr::Match { .. }
            | ShallowIr::FieldAccess { .. }
            | ShallowIr::RecordUpdate { .. }
            | ShallowIr::BinOp { .. } => return true,
            ShallowIr::Const(_)
            | ShallowIr::Construct { .. }
            | ShallowIr::Tuple(_)
            | ShallowIr::ListLit { .. }
            | ShallowIr::FuzzExistential { .. } => return false,
        }
    }
}

fn shallow_ir_type(tipo: &Rc<Type>) -> ShallowIrType {
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

fn translate_sequence(
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

fn translate_if_chain(
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
struct ClauseTranslationFailure {
    reason: String,
    code: OpaqueCode,
}

fn translate_clause(
    clause: &TypedClause,
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
    Ok(ShallowIrArm {
        tag,
        bindings,
        body: typed_expr_to_shallow_ir(&clause.then, data_types),
    })
}

fn translate_call(
    fun: &TypedExpr,
    args: &[CallArg<TypedExpr>],
    tipo: &Rc<Type>,
    data_types: &IndexMap<&DataTypeKey, &TypedDataType>,
) -> ShallowIr {
    // Check for fuzzer combinator calls — these become FuzzExistential
    if let Some(fuzz_ir) = try_fuzz_existential(fun, args, tipo) {
        return fuzz_ir;
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

const FUZZ_MODULES: &[&str] = &["aiken/fuzz", "aiken-lang/fuzz", "fuzz"];

fn try_fuzz_existential(
    fun: &TypedExpr,
    args: &[CallArg<TypedExpr>],
    tipo: &Rc<Type>,
) -> Option<ShallowIr> {
    // Extract the module and function name from the callee
    let (module, fn_name) = match fun {
        TypedExpr::ModuleSelect {
            module_name, label, ..
        } => (module_name.as_str(), label.as_str()),
        TypedExpr::Var { name, .. } => ("", name.as_str()),
        _ => return None,
    };

    let is_fuzz = FUZZ_MODULES.iter().any(|m| module.contains(m))
        || module.ends_with("/fuzz")
        || module == "fuzz";

    if !is_fuzz {
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

    // Extract the payload type from Fuzzer<T>
    let payload_ty = extract_fuzzer_payload_type(tipo.as_ref())
        .map(|t| shallow_ir_type(&t))
        .unwrap_or(ShallowIrType::Unknown);

    let (lo, hi) = match fn_name {
        "int_between" if args.len() == 2 => {
            let lo = const_int_value(&args[0].value);
            let hi = const_int_value(&args[1].value);
            (lo, hi)
        }
        "int_at_least" if args.len() == 1 => (const_int_value(&args[0].value), None),
        "int_at_most" if args.len() == 1 => (None, const_int_value(&args[0].value)),
        _ => (None, None),
    };

    Some(ShallowIr::FuzzExistential {
        kind: payload_ty,
        lo,
        hi,
    })
}

fn const_int_value(expr: &TypedExpr) -> Option<i64> {
    match expr {
        TypedExpr::UInt { value, .. } => value.parse().ok(),
        _ => None,
    }
}

fn translate_binop(op: &BinOp) -> Option<ShallowBinOp> {
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

#[derive(Debug, Clone, Copy)]
pub enum RunnableKind {
    Test,
    Bench,
}

/// ----- Test -----------------------------------------------------------------
///
/// Aiken supports two kinds of tests: unit and property. A unit test is a simply
/// UPLC program which returns must be a lambda that returns a boolean.
///
/// A property on the other-hand is a template for generating tests, which is also
/// a lambda but that takes an extra argument. The argument is generated from a
/// fuzzer which is meant to yield random values in a pseudo-random (albeit seeded)
/// sequence. On failures, the value that caused a failure is simplified using an
/// approach similar to what's described in MiniThesis<https://github.com/DRMacIver/minithesis>,
/// which is a simplified version of Hypothesis, a property-based testing framework
/// with integrated shrinking.
///
/// Our approach could perhaps be called "microthesis", as it implements a subset of
/// minithesis. More specifically, we do not currently support pre-conditions, nor
/// targets.
///
// Public enum; boxing the large property-test variant would force constructor and match churn.
#[allow(clippy::large_enum_variant)]
#[derive(Debug, Clone)]
pub enum Test {
    UnitTest(UnitTest),
    PropertyTest(PropertyTest),
    Benchmark(Benchmark),
}

unsafe impl Send for Test {}

impl Test {
    pub fn unit_test(
        generator: &mut CodeGenerator<'_>,
        test: TypedTest,
        module_name: String,
        input_path: PathBuf,
    ) -> Test {
        let program = generator.generate_raw(&test.body, &[], &module_name);

        let assertion = match test.body.try_into() {
            Err(..) => None,
            Ok(Assertion { bin_op, head, tail }) => {
                let as_constant = |generator: &mut CodeGenerator<'_>, side| {
                    Program::<NamedDeBruijn>::try_from(generator.generate_raw(
                        &side,
                        &[],
                        &module_name,
                    ))
                    .expect("failed to convert assertion operand to NamedDeBruijn")
                    .eval(ExBudget::max())
                    .unwrap_constant()
                    .map(|cst| (cst, side.tipo()))
                };

                // Assertion at this point is evaluated so it's not just a normal assertion
                Some(Assertion {
                    bin_op,
                    head: as_constant(generator, head.expect("cannot be Err at this point")),
                    tail: tail
                        .expect("cannot be Err at this point")
                        .try_mapped(|e| as_constant(generator, e)),
                })
            }
        };

        Test::UnitTest(UnitTest {
            input_path,
            module: module_name,
            name: test.name,
            program,
            assertion,
            on_test_failure: test.on_test_failure,
        })
    }

    pub fn property_test(
        input_path: PathBuf,
        module: String,
        name: String,
        on_test_failure: OnTestFailure,
        program: Program<Name>,
        fuzzer: Fuzzer<Name>,
    ) -> Test {
        Test::PropertyTest(PropertyTest::new(
            input_path,
            module,
            name,
            on_test_failure,
            program,
            fuzzer,
        ))
    }

    pub fn from_function_definition(
        generator: &mut CodeGenerator<'_>,
        test: TypedTest,
        module_name: String,
        input_path: PathBuf,
        kind: RunnableKind,
    ) -> Test {
        if test.arguments.is_empty() {
            if matches!(kind, RunnableKind::Bench) {
                unreachable!("benchmark must have at least one argument");
            } else {
                Self::unit_test(generator, test, module_name, input_path)
            }
        } else if matches!(kind, RunnableKind::Test) {
            Test::PropertyTest(
                PropertyTest::from_function_definition(generator, test, module_name, input_path)
                    .test,
            )
        } else {
            let parameter = test.arguments.first().unwrap().to_owned();

            let via = parameter.via.clone();

            let type_info = parameter.arg.tipo.clone();

            let stripped_type_info = convert_opaque_type(&type_info, generator.data_types(), true);

            let program = generator.clone().generate_raw(
                &test.body,
                &[TypedArg {
                    tipo: stripped_type_info.clone(),
                    ..parameter.clone().into()
                }],
                &module_name,
            );

            // NOTE: We need not to pass any parameter to the fuzzer/sampler here because the fuzzer
            // argument is a Data constructor which needs not any conversion. So we can just safely
            // apply onto it later.
            let generator_program = generator.clone().generate_raw(&via, &[], &module_name);

            Test::Benchmark(Benchmark {
                input_path,
                module: module_name,
                name: test.name,
                program,
                on_test_failure: test.on_test_failure,
                sampler: Sampler {
                    program: generator_program,
                    type_info,
                    stripped_type_info,
                },
            })
        }
    }

    pub fn run(
        self,
        seed: u32,
        max_success: usize,
        plutus_version: &PlutusVersion,
        tracing: Tracing,
    ) -> TestResult<(Constant, Rc<Type>), PlutusData> {
        match self {
            Test::UnitTest(unit_test) => {
                TestResult::UnitTestResult(unit_test.run(plutus_version, tracing))
            }
            Test::PropertyTest(property_test) => {
                TestResult::PropertyTestResult(property_test.run(seed, max_success, plutus_version))
            }
            Test::Benchmark(benchmark) => {
                TestResult::BenchmarkResult(benchmark.run(seed, max_success, plutus_version))
            }
        }
    }
}

/// ----- UnitTest -----------------------------------------------------------------
///
#[derive(Debug, Clone)]
pub struct UnitTest {
    pub input_path: PathBuf,
    pub module: String,
    pub name: String,
    pub on_test_failure: OnTestFailure,
    pub program: Program<Name>,
    pub assertion: Option<Assertion<(Constant, Rc<Type>)>>,
}

unsafe impl Send for UnitTest {}

impl UnitTest {
    pub fn run(
        self,
        plutus_version: &PlutusVersion,
        tracing: Tracing,
    ) -> UnitTestResult<(Constant, Rc<Type>)> {
        let eval_result = Program::<NamedDeBruijn>::try_from(self.program.clone())
            .unwrap()
            .eval_version(ExBudget::max(), &plutus_version.into());

        let is_evaluation_failure = eval_result.failed(true, &plutus_version.into());

        let success = match self.on_test_failure {
            OnTestFailure::SucceedEventually | OnTestFailure::SucceedImmediately => {
                is_evaluation_failure
            }
            OnTestFailure::FailImmediately => !is_evaluation_failure,
        };

        let mut logs = Vec::new();
        if let Err(err) = eval_result.result()
            && tracing.trace_level(false) == TraceLevel::Verbose
        {
            logs.push(format!("{err}"))
        }
        logs.extend(eval_result.logs());

        UnitTestResult {
            success,
            test: self.to_owned(),
            spent_budget: eval_result.cost(),
            logs,
            assertion: self.assertion,
        }
    }
}

/// ----- PropertyTest -----------------------------------------------------------------

#[derive(Debug, Clone)]
pub struct PropertyTest {
    pub input_path: PathBuf,
    pub module: String,
    pub name: String,
    pub on_test_failure: OnTestFailure,
    pub program: Program<Name>,
    pub fuzzer: Fuzzer<Name>,
}

/// Additional verifier/export metadata computed while compiling a property test.
#[derive(Debug, Clone)]
pub struct PropertyTestAnalysis {
    pub return_type: Rc<Type>,
    pub fuzzer: FuzzerAnalysis,
}

/// Additional verifier/export metadata computed from a property test fuzzer.
#[derive(Debug, Clone)]
pub struct FuzzerAnalysis {
    pub normalized: NormalizedFuzzer,
    pub constraint: FuzzerConstraint,
    pub semantics: FuzzerSemantics,
}

/// A property test plus the verifier/export metadata derived during compilation.
#[derive(Debug, Clone)]
pub struct AnalyzedPropertyTest {
    pub test: PropertyTest,
    pub analysis: PropertyTestAnalysis,
}

unsafe impl Send for PropertyTest {}

/// Typed constraint IR describing what a fuzzer is known to produce.
///
/// This is re-exported from the project crate as `FuzzerConstraint` in the
/// export manifest. It supports composable constraints for arbitrary fuzzer
/// output shapes (integers, tuples, lists, mapped values, etc.).
#[non_exhaustive]
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum FuzzerExactValue {
    Bool(bool),
    ByteArray(Vec<u8>),
    String(String),
}

// Public semantic IR; direct variants preserve the public construction/matching API.
#[non_exhaustive]
#[allow(clippy::large_enum_variant)]
#[derive(Debug, Clone, PartialEq)]
pub enum FuzzerSemantics {
    Bool,
    IntRange {
        min: Option<String>,
        max: Option<String>,
    },
    ByteArrayRange {
        min_len: Option<usize>,
        max_len: Option<usize>,
    },
    String,
    Data,
    /// The fuzzer produces values of a typed Aiken ADT, represented as Plutus `Data`.
    /// A structural schema predicate (`isValid_TypeName : Data -> Prop`) must be
    /// generated from the test's `fuzzer_data_schema` field and used as a
    /// precondition for the generated Lean theorem to remain sound -- a naïve
    /// `∀ x : Data, ...` would admit values the validator rejects and make the
    /// theorem false.
    ///
    /// The `type_name` (qualified `module.Type`, or just `Type` when no module
    /// qualifier is available) is recorded solely for Lean predicate naming.
    DataWithSchema {
        type_name: String,
    },
    Exact(FuzzerExactValue),
    OneOf(Vec<FuzzerExactValue>),
    Product(Vec<FuzzerSemantics>),
    List {
        element: Box<FuzzerSemantics>,
        min_len: Option<usize>,
        max_len: Option<usize>,
    },
    Constructors {
        tags: Vec<u64>,
    },
    StateMachineTrace {
        acceptance: StateMachineAcceptance,
        state_type: SemanticType,
        step_input_types: Vec<SemanticType>,
        label_type: SemanticType,
        event_type: SemanticType,
        transition_semantics: StateMachineTransitionSemantics,
        output_semantics: Box<FuzzerSemantics>,
        /// Shallow IR of the step function body, for universal-theorem
        /// generation. `None` when translation was not attempted or yielded
        /// no useful information.
        step_function_ir: Option<ShallowIr>,
        /// Human-readable reason why `step_function_ir` is `None` (if it is).
        step_ir_unsupported_reason: Option<String>,
        /// Proposition-level translation of the step function body used to
        /// generate `isValidTransition` predicates for universal Lean
        /// theorems for supported state-machine transition shapes. Coexists with
        /// `step_function_ir`: the ShallowIr field feeds the existing
        /// `step_fn` emitter, while this field feeds the new
        /// `isValidTransition` emitter.
        ///
        /// `None` means no TransitionProp translation was produced — either
        /// because the step body had no structural content we recognise
        /// yet, or because the translation yielded only `Unsupported`
        /// leaves and carries no extractable constraint.
        transition_prop: Option<TransitionProp>,
        /// Shallow IR of the initial-state expression (the first argument to
        /// `scenario.ok(init_state, step)`). Used to emit a
        /// `isValidTrace` predicate anchored at the concrete starting
        /// state rather than a vacuous over-approximation. `None` when the
        /// expression could not be translated (sound: `isValidTrace` falls
        /// back to a fresh `Data` existential, which only widens the
        /// precondition).
        initial_state_shallow_ir: Option<ShallowIr>,
    },
    Opaque {
        reason: String,
    },
}

#[non_exhaustive]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum StateMachineAcceptance {
    AcceptsSuccess,
    AcceptsFailure,
}

#[derive(Debug, Clone, PartialEq)]
pub struct StateMachineTransitionSemantics {
    pub terminal_tag: u64,
    pub step_tag: u64,
    pub label_field_index: usize,
    pub next_state_field_index: usize,
    pub event_field_index: usize,
    pub state_semantics: Box<FuzzerSemantics>,
    pub step_input_semantics: Vec<FuzzerSemantics>,
    pub label_semantics: Box<FuzzerSemantics>,
    pub event_semantics: Box<FuzzerSemantics>,
}

#[non_exhaustive]
#[derive(Debug, Clone, PartialEq)]
pub enum SemanticType {
    Int,
    Bool,
    ByteArray,
    String,
    Data,
    List(Box<SemanticType>),
    Tuple(Vec<SemanticType>),
    Pair(Box<SemanticType>, Box<SemanticType>),
    Unsupported(String),
}

#[non_exhaustive]
#[derive(Debug, Clone, PartialEq)]
pub enum FuzzerConstraint {
    /// No constraint known; the fuzzer may produce any value of the given type.
    Any,
    /// Integer in a closed range [min, max].
    IntRange { min: String, max: String },
    /// ByteString length in a closed range [min_len, max_len].
    ByteStringLenRange { min_len: usize, max_len: usize },
    /// Exact scalar value.
    Exact(FuzzerExactValue),
    /// Finite scalar set. Empty sets are invalid and singletons canonicalize to `Exact`.
    OneOf(Vec<FuzzerExactValue>),
    /// A tuple whose elements each carry their own constraint.
    Tuple(Vec<FuzzerConstraint>),
    /// A list whose elements satisfy `elem`, with optional length bounds.
    List {
        elem: Box<FuzzerConstraint>,
        min_len: Option<usize>,
        max_len: Option<usize>,
    },
    /// Finite set of nullary ADT constructors represented as `Data.Constr tag []`.
    DataConstructorTags { tags: Vec<u64> },
    /// A mapped constraint: the underlying constraint describes the input domain.
    Map(Box<FuzzerConstraint>),
    /// Conjunction of constraints (all must hold).
    And(Vec<FuzzerConstraint>),
    /// Constraint could not be extracted; includes a human-readable reason.
    Unsupported { reason: String },
}

#[non_exhaustive]
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum UnaryMapperShape {
    Identity,
    ConstBool(bool),
    ConstByteArray(Vec<u8>),
    ConstString(String),
    FiniteScalar(Vec<FuzzerExactValue>),
    ConstInt(String),
    IntAffine { scale: i8, offset: String },
    ConstructorMap(BTreeMap<String, String>),
    Unknown,
}

/// Proposition-level IR capturing the semantics of a state-machine `step`
/// function as a nest of existential quantifiers, conjunctions, and
/// disjunctions — the shape required to generate universally quantified
/// `isValidTransition` Lean predicates.
///
/// This is distinct from `ShallowIr`, which is value-level: a step body
/// like `let action <- and_then(spend_redeemer())` is a *sequenced
/// predicate over existential witnesses*, not a plain expression, so it
/// cannot be faithfully captured by `ShallowIr` alone.
///
/// Translation rules from normalized fuzzer bodies (see
/// `normalized_fuzzer_to_transition_prop`):
///
/// | Source pattern                            | TransitionProp node            |
/// |-------------------------------------------|--------------------------------|
/// | `and_then(src, fn(x) { body })`           | `Exists { domain, body }`      |
/// | `fork*(branches)`                         | `Or([translate(b) ...])`       |
/// | `such_that(f, p)`                         | `Exists { body: And([Pure p, ...]) }` |
/// | `return(Scenario.Step(lbl, st, txn))`     | `EqOutput(...)`                |
/// | `return(Scenario.Done)`                   | `EqOutput(Scenario.Done)`      |
/// | Unknown                                   | `Unsupported { reason, .. }`   |
// NOTE: no serde derives here — `TransitionProp::Exists::domain` holds a
// `Box<FuzzerSemantics>`, and the aiken-lang `FuzzerSemantics` is not
// (de)serializable (it carries `TypedExpr`-flavoured leaves indirectly via
// `Opaque { reason }` only, but the mirror type in `aiken-project::export`
// is what gets serialized). When a serializable mirror is needed, add one
// there and convert in `aiken-project::convert_semantics`.
#[derive(Debug, Clone, PartialEq)]
pub enum TransitionProp {
    /// A pure boolean Aiken sub-expression (lowerable to a Lean Prop via `ShallowIr`).
    Pure(ShallowIr),
    /// `∃ binder : ty ∈ domain, body` — from `and_then` / `bind`.
    /// `domain` captures the fuzzer semantics of the bound source so the
    /// corresponding Lean existential can be restricted to its support.
    Exists {
        binder: String,
        ty: ShallowIrType,
        /// Boxed to break the `FuzzerSemantics ↔ TransitionProp` recursion
        /// cycle (a `FuzzerSemantics::StateMachineTrace` carries an
        /// `Option<TransitionProp>`).
        domain: Box<FuzzerSemantics>,
        body: Box<TransitionProp>,
    },
    /// Conjunction of propositions (all must hold).
    And(Vec<TransitionProp>),
    /// Disjunction of propositions (any may hold) — from `fork*` / `either`.
    Or(Vec<TransitionProp>),
    /// `if cond then t else e` where `cond` is a pure boolean expression.
    IfThenElse {
        cond: ShallowIr,
        t: Box<TransitionProp>,
        e: Box<TransitionProp>,
    },
    /// `when scrutinee is { ... }` — disjunction indexed by constructor tag.
    /// Uses the same arm shape as `ShallowIrArm` so the existing Lean
    /// pattern-match machinery can be reused.
    Match {
        scrutinee: ShallowIr,
        arms: Vec<TransitionPropArm>,
    },
    /// `transition = <expression>` — binds the output transition value.
    /// Produced by `return(...)` in the step-function body.
    EqOutput(ShallowIr),
    /// A call to a module-level sub-generator function whose body is not
    /// inlined. The Lean emitter will generate an opaque predicate stub
    /// `{prefix}_{fn_name}_prop : Data → Data → Prop` and reference it
    /// as `{prefix}_{fn_name}_prop state {binding_var}` at the call site.
    ///
    /// Unlike `Unsupported`, which widens to `True`, `SubGenerator` preserves
    /// the *name* of the constraint so the generated Lean is auditable and the
    /// opaque predicate can later be filled in with the real body.
    SubGenerator {
        /// Aiken module where the function is defined (e.g. `"permissions/test"`).
        module: String,
        /// Function name in the Aiken source (e.g. `"scenario_inputs_baseline"`).
        fn_name: String,
    },
    /// A constraint we could not lower. The `reason` must be preserved in
    /// the generated `.lean` file as an audit comment; the extractor must
    /// never silently drop a constraint because dropping widens the
    /// precondition and is unsound.
    Unsupported {
        reason: String,
        source_location: Option<String>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub struct TransitionPropArm {
    /// Constructor tag this arm matches (None for a catch-all variable pattern).
    pub tag: Option<u64>,
    /// Bound variable names (one per field, in order).
    pub bindings: Vec<String>,
    /// Body proposition of this arm.
    pub body: TransitionProp,
}

#[non_exhaustive]
#[derive(Debug, Clone, PartialEq)]
pub enum NormalizedFuzzer {
    Opaque {
        expr: Box<TypedExpr>,
        reason: String,
    },
    Primitive {
        output_type: Rc<Type>,
        /// Optional constraint extracted from a recognized stdlib fuzzer call.
        known_constraint: Option<FuzzerConstraint>,
    },
    Map {
        source: Box<NormalizedFuzzer>,
        source_output_type: Rc<Type>,
        output_type: Rc<Type>,
        mapper_shape: UnaryMapperShape,
    },
    Bind {
        source: Box<NormalizedFuzzer>,
        result: Box<NormalizedFuzzer>,
    },
    Product {
        elements: Vec<NormalizedFuzzer>,
    },
    List {
        element: Box<NormalizedFuzzer>,
        min_len: Option<usize>,
        max_len: Option<usize>,
    },
    StateMachineTrace {
        acceptance: StateMachineAcceptance,
        output_type: Rc<Type>,
        initial_state: Box<TypedExpr>,
        step_function: Box<TypedExpr>,
    },
}

#[derive(Debug, Clone)]
pub struct Fuzzer<T> {
    pub program: Program<T>,

    pub type_info: Rc<Type>,

    /// A version of the Fuzzer's type that has gotten rid of
    /// all erasable opaque type. This is needed in order to
    /// generate Plutus data with the appropriate shape.
    pub stripped_type_info: Rc<Type>,
}

#[cfg(test)]
fn normalize_fuzzer_from_via(
    via: &TypedExpr,
    current_module: &str,
    known_functions: &IndexMap<&FunctionAccessKey, &TypedFunction>,
) -> NormalizedFuzzer {
    normalize_fuzzer_from_via_with_constants(via, current_module, known_functions, &IndexMap::new())
}

fn normalize_fuzzer_from_via_with_constants(
    via: &TypedExpr,
    current_module: &str,
    known_functions: &IndexMap<&FunctionAccessKey, &TypedFunction>,
    known_constants: &IndexMap<&FunctionAccessKey, &TypedExpr>,
) -> NormalizedFuzzer {
    let function_index = index_known_functions(known_functions);
    let constant_index = index_known_constants(known_constants);
    let mut visiting_functions = BTreeSet::new();

    normalize_fuzzer_from_expr(
        via,
        current_module,
        &function_index,
        &constant_index,
        &BTreeMap::new(),
        &mut visiting_functions,
    )
}

fn opaque_normalized_fuzzer(expr: &TypedExpr, reason: impl Into<String>) -> NormalizedFuzzer {
    NormalizedFuzzer::Opaque {
        expr: Box::new(terminal_expression(expr).clone()),
        reason: reason.into(),
    }
}

fn normalize_fuzzer_from_expr(
    expr: &TypedExpr,
    current_module: &str,
    function_index: &FunctionIndex<'_>,
    constant_index: &ConstantIndex<'_>,
    local_values: &BTreeMap<String, TypedExpr>,
    visiting_functions: &mut BTreeSet<(String, String)>,
) -> NormalizedFuzzer {
    if let TypedExpr::Pipeline { expressions, .. } | TypedExpr::Sequence { expressions, .. } = expr
    {
        return normalize_fuzzer_from_sequence(
            expressions,
            current_module,
            function_index,
            constant_index,
            local_values,
            visiting_functions,
        );
    }

    let expr = terminal_expression(expr);

    let mut visiting_local_aliases = BTreeSet::new();
    if let Some(normalized) =
        normalize_state_machine_trace_from_expr(expr, local_values, &mut visiting_local_aliases)
    {
        return normalized;
    }

    if extract_fuzzer_payload_type(expr.tipo().as_ref()).is_none() {
        return opaque_normalized_fuzzer(
            expr,
            format!(
                "expression '{}' does not have built-in Fuzzer type",
                describe_expr(expr)
            ),
        );
    }

    match expr {
        TypedExpr::Var {
            name, constructor, ..
        } if matches!(
            constructor.variant,
            ValueConstructorVariant::LocalVariable { .. }
        ) =>
        {
            local_values.get(name).map_or_else(
                || opaque_normalized_fuzzer(expr, format!("unbound local fuzzer alias '{name}'")),
                |bound_expr| {
                    normalize_fuzzer_from_expr(
                        bound_expr,
                        current_module,
                        function_index,
                        constant_index,
                        local_values,
                        visiting_functions,
                    )
                },
            )
        }
        TypedExpr::Call { fun, args, .. } => normalize_fuzzer_from_call(
            expr,
            fun.as_ref(),
            args,
            current_module,
            function_index,
            constant_index,
            local_values,
            visiting_functions,
        ),
        // Peel control-flow: if every branch normalizes to the same shape,
        // lift it; otherwise fall back to an unconstrained primitive over
        // the output type. This is a *sound over-approximation*: widening
        // the fuzzer's semantic domain never invalidates a universally
        // quantified proof.
        TypedExpr::If {
            branches,
            final_else,
            tipo,
            ..
        } => {
            let mut normalized_branches: Vec<NormalizedFuzzer> =
                Vec::with_capacity(branches.len() + 1);
            for branch in branches.iter() {
                normalized_branches.push(normalize_fuzzer_from_expr(
                    &branch.body,
                    current_module,
                    function_index,
                    constant_index,
                    local_values,
                    visiting_functions,
                ));
            }
            normalized_branches.push(normalize_fuzzer_from_expr(
                final_else.as_ref(),
                current_module,
                function_index,
                constant_index,
                local_values,
                visiting_functions,
            ));
            merge_branch_normalizations(normalized_branches, tipo.as_ref())
        }
        TypedExpr::When { clauses, tipo, .. } => {
            let normalized_branches: Vec<NormalizedFuzzer> = clauses
                .iter()
                .map(|clause| {
                    normalize_fuzzer_from_expr(
                        &clause.then,
                        current_module,
                        function_index,
                        constant_index,
                        local_values,
                        visiting_functions,
                    )
                })
                .collect();
            merge_branch_normalizations(normalized_branches, tipo.as_ref())
        }
        // A fuzzer value expressed directly as `fn(prng) { ... }` (the shape
        // used inside `fuzz.int()`, `fuzz.constant(_)`, etc.) is a primitive
        // leaf: we cannot inspect its body statically here without much more
        // analysis, so treat it as unconstrained over its payload type.
        TypedExpr::Fn { .. } => primitive_from_fuzzer_expr(expr),
        _ => normalize_fuzzer_from_resolved_function(
            expr,
            current_module,
            function_index,
            constant_index,
            local_values,
            visiting_functions,
        )
        .unwrap_or_else(|| {
            opaque_normalized_fuzzer(
                expr,
                format!(
                    "fuzzer expression '{}' is not structurally understood yet",
                    describe_expr(expr)
                ),
            )
        }),
    }
}

/// Combine per-branch normalizations from an `if`/`when` scrutinee into a
/// single normalization for the whole expression.
///
/// The goal is to avoid ever producing `Opaque` for a fuzzer whose output
/// type is known: over-approximating the semantic domain of a single branch
/// up to "any value of T" is sound for universally-quantified tests.
fn merge_branch_normalizations(
    branches: Vec<NormalizedFuzzer>,
    expr_type: &Type,
) -> NormalizedFuzzer {
    // If every branch independently normalized to the same shape, we can lift
    // it wholesale. Otherwise fall back to an unconstrained primitive over the
    // expression's fuzzer payload type.
    if let Some(first) = branches.first() {
        if branches.iter().all(|b| b == first) {
            return first.clone();
        }
    }

    let Some(output_type) = extract_fuzzer_payload_type(expr_type) else {
        // The whole expression isn't a `Fuzzer<T>`: nothing sensible to lift.
        return opaque_normalized_fuzzer(
            &TypedExpr::ErrorTerm {
                location: Span::empty(),
                tipo: Rc::new(expr_type.clone()),
            },
            "control-flow expression does not have Fuzzer type",
        );
    };

    NormalizedFuzzer::Primitive {
        output_type,
        known_constraint: None,
    }
}

/// Construct an unconstrained primitive fuzzer normalization for an
/// expression whose type is a `Fuzzer<T>`, falling back to opaque otherwise.
fn primitive_from_fuzzer_expr(expr: &TypedExpr) -> NormalizedFuzzer {
    if let Some(output_type) = extract_fuzzer_payload_type(expr.tipo().as_ref()) {
        NormalizedFuzzer::Primitive {
            output_type,
            known_constraint: None,
        }
    } else {
        opaque_normalized_fuzzer(
            expr,
            format!(
                "fuzzer expression '{}' is not structurally understood yet",
                describe_expr(expr)
            ),
        )
    }
}

fn normalize_fuzzer_from_sequence(
    expressions: &[TypedExpr],
    current_module: &str,
    function_index: &FunctionIndex<'_>,
    constant_index: &ConstantIndex<'_>,
    local_values: &BTreeMap<String, TypedExpr>,
    visiting_functions: &mut BTreeSet<(String, String)>,
) -> NormalizedFuzzer {
    let Some(last) = expressions.last() else {
        return opaque_normalized_fuzzer(
            &TypedExpr::Sequence {
                location: Span::empty(),
                expressions: vec![],
            },
            "empty sequence cannot normalize to a fuzzer",
        );
    };

    let mut scoped_values = local_values.clone();
    for expr in expressions.iter().take(expressions.len().saturating_sub(1)) {
        if let TypedExpr::Assignment { pattern, value, .. } = expr {
            if let Some(name) = pattern_var_name(pattern) {
                scoped_values.insert(name.to_string(), value.as_ref().clone());
            }
        }
    }

    normalize_fuzzer_from_expr(
        last,
        current_module,
        function_index,
        constant_index,
        &scoped_values,
        visiting_functions,
    )
}

fn normalize_state_machine_trace_from_expr(
    expr: &TypedExpr,
    local_values: &BTreeMap<String, TypedExpr>,
    visiting_local_aliases: &mut BTreeSet<String>,
) -> Option<NormalizedFuzzer> {
    let expr = terminal_expression(expr);

    match expr {
        TypedExpr::Call {
            fun, args, tipo, ..
        } => {
            let (resolved_fun, resolved_args) =
                flatten_call_head_and_args(fun.as_ref(), args, local_values).unwrap_or_else(|| {
                    (
                        terminal_expression(fun.as_ref()).clone(),
                        collect_call_argument_values(args),
                    )
                });

            // In production, `tipo` on a via-expression is `Fuzzer<Payload>`
            // (a `Type::Fn { prng -> Option<(prng, Payload)> }`), because the
            // `via` clause is checked against the Fuzzer return type of the
            // callee. The state-machine acceptance inference, however, needs
            // the raw `Payload` type (`List<T>` or `(List<Label>, List<T>)`)
            // to match. Peel the Fuzzer wrapper here if present; otherwise
            // fall through to the original tipo for callers (like the cfg(test)
            // unit fixtures) that already pass the payload directly.
            let payload_tipo = extract_fuzzer_payload_type(tipo.as_ref());
            let output_type: &Type = payload_tipo.as_deref().unwrap_or_else(|| tipo.as_ref());

            normalize_state_machine_trace_from_call(&resolved_fun, output_type, &resolved_args)
        }
        TypedExpr::Var {
            name, constructor, ..
        } if matches!(
            constructor.variant,
            ValueConstructorVariant::LocalVariable { .. }
        ) =>
        {
            let bound_expr = local_values.get(name)?;
            if !visiting_local_aliases.insert(name.clone()) {
                return None;
            }

            let normalized = normalize_state_machine_trace_from_expr(
                bound_expr,
                local_values,
                visiting_local_aliases,
            );
            visiting_local_aliases.remove(name);
            normalized
        }
        _ => None,
    }
}

fn normalize_state_machine_trace_from_call(
    callee: &TypedExpr,
    output_type: &Type,
    args: &[TypedExpr],
) -> Option<NormalizedFuzzer> {
    // Gate on the callee identity when we can positively identify it.
    // Only reject callees from modules that are clearly NOT fuzz/test related
    // (i.e., stdlib modules like "aiken/list", "aiken/int", etc. that happen to
    // have matching type signatures). If the callee module is unknown or is the
    // user's own module, fall through to type-based checking.
    //
    // TODO: A more precise check would match on the specific combinator name
    // (e.g., "trace" or "run_scenario") in addition to the module, but the
    // current type-based check is already quite specific (requires the exact
    // state-machine type signature pattern).
    if let Some((module, _name)) = extract_module_fn_identity(callee) {
        let is_known_non_fuzz_stdlib = module.starts_with("aiken/")
            && !module.contains("fuzz")
            && !module.contains("test")
            && !module.contains("scenario");
        if is_known_non_fuzz_stdlib {
            return None;
        }
    }

    let acceptance = infer_state_machine_acceptance_from_output_type(output_type)?;
    let [initial_state, step_function] = args else {
        return None;
    };

    if expression_has_fuzzer_type(initial_state) {
        return None;
    }

    let (step_args, step_ret) = function_signature(step_function.tipo().as_ref())?;

    if step_args.is_empty() || extract_fuzzer_payload_type(step_ret.as_ref()).is_none() {
        return None;
    }

    Some(NormalizedFuzzer::StateMachineTrace {
        acceptance,
        output_type: Rc::new(output_type.clone()),
        initial_state: Box::new(initial_state.clone()),
        step_function: Box::new(step_function.clone()),
    })
}

#[allow(clippy::too_many_arguments)]
fn normalize_fuzzer_from_call(
    expr: &TypedExpr,
    fun: &TypedExpr,
    args: &[CallArg<TypedExpr>],
    current_module: &str,
    function_index: &FunctionIndex<'_>,
    constant_index: &ConstantIndex<'_>,
    local_values: &BTreeMap<String, TypedExpr>,
    visiting_functions: &mut BTreeSet<(String, String)>,
) -> NormalizedFuzzer {
    // S2 SUBSET: Beta-reduce when the callee is an `Fn` literal (possibly
    // reached through a chain of local aliases).  This handles `fn() { ... }`
    // thunks invoked as `baseline()` inside the `fork*_and_then` stdlib body
    // — without this, such call sites fall through to helper descent, fail
    // to resolve the `Fn` literal (which `resolve_function_from_expr` does
    // not handle), and return `Opaque`.
    //
    // Beta reduction is structurally sound: the call reduces to the body
    // with formal parameters bound to the actual arguments, which is the
    // operational meaning of function application.  We reuse the existing
    // `local_values` substitution mechanism (as `normalize_fuzzer_from_helper_call`
    // does for module helpers) rather than rewriting the AST.
    if let Some(normalized) = try_beta_reduce_fuzzer_call(
        fun,
        args,
        current_module,
        function_index,
        constant_index,
        local_values,
        visiting_functions,
    ) {
        return normalized;
    }

    if let Some(normalized) = normalize_structural_fuzzer_call(
        expr,
        args,
        current_module,
        function_index,
        constant_index,
        local_values,
        visiting_functions,
    ) {
        return normalized;
    }

    // STDLIB PRIMITIVE SHORTCUT: For known aiken/fuzz primitive fuzzers
    // (int_between, int_at_least, int_at_most, constant), extract the
    // bounds directly without descending into the function body.
    // This MUST run before helper descent because `normalize_fuzzer_from_helper_call`
    // would descend into these stdlib bodies and return a non-opaque result
    // (without bounds) that bypasses `try_extract_primitive_constraint_structurally`.
    if args
        .iter()
        .all(|arg| !expression_has_fuzzer_type(&arg.value))
    {
        if let Some(output_type) = extract_fuzzer_payload_type(expr.tipo().as_ref()) {
            if let Some(known_constraint) = try_extract_primitive_constraint_structurally(
                fun,
                args,
                expr.tipo().as_ref(),
                constant_index,
                local_values,
            ) {
                return NormalizedFuzzer::Primitive {
                    output_type,
                    known_constraint: Some(known_constraint),
                };
            }
        }
    }

    // STDLIB LIST SHORTCUT: For known aiken/fuzz list fuzzers (list_between,
    // list_at_least, list_at_most), extract length bounds directly without
    // descending into the helper body.
    //
    // This MUST run before helper descent.  When `normalize_structural_fuzzer_call`
    // is given a call like `fuzz.list_between(fuzz.bool(), 0, 3)`, it tries to
    // verify that the element fuzzer's payload type structurally equals the list's
    // element type.  However, after Aiken's type-inference pass the element type in
    // `List<Bool>` may be stored as `Type::Var { Link(Bool) }` (a type-variable
    // linked to the concrete Bool type), while `extract_fuzzer_payload_type` on the
    // `fuzz.bool()` argument returns the concrete `Type::App { "Bool" }`.  The
    // `PartialEq` implementation for `Type` does NOT follow links, so the check
    // returns `false` and `normalize_structural_fuzzer_call` returns `None`.
    // Helper descent then resolves the stdlib body and returns a `NormalizedFuzzer::List`
    // without bounds (because min/max are parameters in the stdlib body, not literals).
    //
    // By intercepting known stdlib list constructors here—before helper descent—we
    // bypass the type-equality check entirely and extract bounds directly from the
    // call-site arguments, where the literal values are visible.
    if let Some((module, fn_name)) = extract_module_fn_identity(fun) {
        if module == STDLIB_FUZZ_MODULE
            && matches!(
                fn_name.as_str(),
                "list_between" | "list_at_least" | "list_at_most"
            )
        {
            if let Some(output_type) = extract_fuzzer_payload_type(expr.tipo().as_ref()) {
                if output_type.is_list() {
                    let fuzzer_args: Vec<&CallArg<TypedExpr>> = args
                        .iter()
                        .filter(|arg| expression_has_fuzzer_type(&arg.value))
                        .collect();
                    if fuzzer_args.len() == 1 {
                        let (min_len, max_len) = try_extract_list_length_bounds(
                            expr,
                            args,
                            constant_index,
                            local_values,
                        );
                        return NormalizedFuzzer::List {
                            element: Box::new(normalize_fuzzer_from_expr(
                                &fuzzer_args[0].value,
                                current_module,
                                function_index,
                                constant_index,
                                local_values,
                                visiting_functions,
                            )),
                            min_len,
                            max_len,
                        };
                    }
                }
            }
        }
    }

    // Descend into helper bodies when possible so that user-defined wrappers,
    // stdlib re-exports, and renames still expose their structural shape
    // (e.g., `negate_fuzzer() = fuzz.map(fuzz.int_between(1, 50), negate)`).
    let helper_result = normalize_fuzzer_from_helper_call(
        fun,
        args,
        current_module,
        function_index,
        constant_index,
        local_values,
        visiting_functions,
    );

    // If helper descent produced a non-opaque structure, trust it.
    if let Some(normalized) = helper_result.as_ref() {
        if !matches!(normalized, NormalizedFuzzer::Opaque { .. }) {
            return normalized.clone();
        }
    }

    // Fall back to primitive classification only when helper descent hit an
    // opacity rooted in control flow (an `if`/`when`/anonymous function body
    // whose shape we don't yet analyze), or when the callee isn't resolvable
    // to a helper at all. Stay opaque for helpers whose bodies are genuine
    // placeholders like `todo`/`fail`: those do not produce *any* value of
    // T, so widening their domain to "all of T" would introduce values the
    // program cannot actually generate and would mask real latent bugs.
    let allow_primitive_fallback = match helper_result.as_ref() {
        None => true,
        Some(_) => {
            helper_body_is_control_flow_shaped(fun, current_module, function_index, local_values)
        }
    };

    if allow_primitive_fallback
        && args
            .iter()
            .all(|arg| !expression_has_fuzzer_type(&arg.value))
    {
        if let Some(output_type) = extract_fuzzer_payload_type(expr.tipo().as_ref()) {
            let known_constraint = try_extract_primitive_constraint_structurally(
                fun,
                args,
                expr.tipo().as_ref(),
                constant_index,
                local_values,
            );
            return NormalizedFuzzer::Primitive {
                output_type,
                known_constraint,
            };
        }
    }

    // Preserve the helper's opaque reason if we have one.
    if let Some(normalized) = helper_result {
        return normalized;
    }

    opaque_normalized_fuzzer(
        expr,
        format!(
            "call '{}' is a Fuzzer but its structural shape is not recognized",
            describe_expr(fun)
        ),
    )
}

/// Is the helper function's body shaped like control flow or an anonymous
/// fuzzer lambda — i.e., one of the patterns the normalizer does not yet
/// inspect but whose output is still a valid fuzzer over the declared
/// payload type?
///
/// Concretely: `if`/`when` expressions (as seen in `fuzz.int_between`,
/// `fuzz.bytearray_between`, etc.) and direct `fn(prng) { ... }` lambdas
/// (as seen in `fuzz.int`, `fuzz.constant`, etc.). For these, falling
/// back to an unconstrained primitive is sound.
///
/// Placeholders like `todo`/`fail` produce `TypedExpr::ErrorTerm` and are
/// explicitly *not* matched here: widening their semantic domain would
/// invent values the program never produces.
fn helper_body_is_control_flow_shaped(
    fun: &TypedExpr,
    current_module: &str,
    function_index: &FunctionIndex<'_>,
    local_values: &BTreeMap<String, TypedExpr>,
) -> bool {
    let mut visiting_local_aliases = BTreeSet::new();
    let Some(resolved) = resolve_function_from_expr(
        fun,
        current_module,
        function_index,
        local_values,
        &mut visiting_local_aliases,
    ) else {
        return false;
    };

    let body = terminal_expression(&resolved.function.body);
    matches!(
        body,
        TypedExpr::If { .. } | TypedExpr::When { .. } | TypedExpr::Fn { .. }
    )
}

/// Extract a constraint from a primitive-leaf fuzzer call *structurally*,
/// without matching on function names.
///
/// A call is treated as a primitive leaf when it returns a `Fuzzer<T>` and
/// takes no Fuzzer arguments; this captures the shape of stdlib fuzzers like
/// `fuzz.int()`, `fuzz.int_between(min, max)`, `fuzz.bool()`, user-defined
/// re-exports, etc. The body of such a function is not statically inspected
/// here, so we cannot prove that its values lie in any particular range.
///
/// The conservative and sound choice for universally-quantified property
/// tests is to over-approximate the domain: if we emit no constraint, the
/// downstream verifier will quantify universally over `T`, which widens the
/// proof obligation (never under-approximates). The caller is expected to
/// wrap this in `NormalizedFuzzer::Primitive { known_constraint, .. }` and
/// let the semantics layer fall back to `default_semantics_for_type(T)`
/// (e.g. unbounded `IntRange { None, None }` for `Int`).
///
/// Returning `None` is the safe default. This function is structured as a
/// future extension point: it may later grow a body-shape analysis that can
/// *prove* tighter bounds from literal arguments, but any such refinement
/// must be sound — if we cannot structurally prove the body stays within
/// `[arg0, arg1]`, we must return `None` and let the verifier over-approximate.
fn try_extract_primitive_constraint_structurally(
    fun: &TypedExpr,
    args: &[CallArg<TypedExpr>],
    _call_tipo: &Type,
    constant_index: &ConstantIndex<'_>,
    local_values: &BTreeMap<String, TypedExpr>,
) -> Option<FuzzerConstraint> {
    // Name-gated extraction: only trust literal argument bounds when the
    // callee is a stdlib `aiken/fuzz` primitive whose semantics we know.
    // Anything else (user-defined wrappers, re-exports under a different
    // module) falls through to `None` so the verifier over-approximates.
    let (module, fn_name) = extract_module_fn_identity(fun)?;
    if module != STDLIB_FUZZ_MODULE {
        return None;
    }

    match (fn_name.as_str(), args) {
        ("int_between", [lo_arg, hi_arg]) => {
            let lo = try_extract_int_literal(&lo_arg.value, constant_index, local_values)?;
            let hi = try_extract_int_literal(&hi_arg.value, constant_index, local_values)?;
            // Normalize swapped args so min ≤ max (BigInt comparison handles
            // values outside the i128 range correctly).
            let (lo, hi) = if lo <= hi { (lo, hi) } else { (hi, lo) };
            Some(FuzzerConstraint::IntRange {
                min: lo.to_string(),
                max: hi.to_string(),
            })
        }
        ("int_at_least", [min_arg]) => {
            let min = try_extract_int_literal(&min_arg.value, constant_index, local_values)?;
            // `i128::MAX.to_string()` is used as a sentinel for "no upper bound".
            // `unbounded_int_sentinel_to_none` strips it back to `None` downstream.
            Some(FuzzerConstraint::IntRange {
                min: min.to_string(),
                max: i128::MAX.to_string(),
            })
        }
        ("int_at_most", [max_arg]) => {
            let max = try_extract_int_literal(&max_arg.value, constant_index, local_values)?;
            // `i128::MIN.to_string()` is used as a sentinel for "no lower bound".
            Some(FuzzerConstraint::IntRange {
                min: i128::MIN.to_string(),
                max: max.to_string(),
            })
        }
        ("constant", [value_arg]) if value_arg.value.tipo().is_int() => {
            let v = try_extract_int_literal(&value_arg.value, constant_index, local_values)?;
            Some(FuzzerConstraint::IntRange {
                min: v.to_string(),
                max: v.to_string(),
            })
        }
        ("bytearray_between", [lo_arg, hi_arg]) => {
            let lo = extract_bytearray_len(&lo_arg.value, constant_index, local_values)?;
            let hi = extract_bytearray_len(&hi_arg.value, constant_index, local_values)?;
            // Normalize swapped args so min ≤ max.
            let (lo, hi) = if lo <= hi { (lo, hi) } else { (hi, lo) };
            Some(FuzzerConstraint::ByteStringLenRange {
                min_len: lo,
                max_len: hi,
            })
        }
        ("bytearray_at_most", [max_arg]) => {
            // `bytearray_at_most(n)` produces bytearrays of length [0, n].
            // Length 0 is the natural lower bound; no sentinel needed.
            let max = extract_bytearray_len(&max_arg.value, constant_index, local_values)?;
            Some(FuzzerConstraint::ByteStringLenRange {
                min_len: 0,
                max_len: max,
            })
        }
        // `bytearray_at_least(n)` has no representable upper bound in
        // `ByteStringLenRange { min_len: usize, max_len: usize }`. Unlike the
        // `IntRange` case, downstream semantics have no sentinel handler that
        // would strip a `usize::MAX` back to `None`, so emitting one would
        // silently narrow the domain. Fall through to `Unsupported` so the
        // verifier over-approximates.
        _ => None,
    }
}

/// Extract a non-negative length literal suitable for a `ByteArray` length bound.
///
/// Mirrors the usize-clamping logic in `try_extract_list_length_bounds` but
/// lives at module scope so both the list-length and bytearray-length
/// extractors can share it. Returns `None` when the literal is negative or
/// exceeds `usize::MAX`.
fn extract_bytearray_len(
    expr: &TypedExpr,
    constant_index: &ConstantIndex<'_>,
    local_values: &BTreeMap<String, TypedExpr>,
) -> Option<usize> {
    let n = try_extract_int_literal(expr, constant_index, local_values)?;
    if n.sign() == num_bigint::Sign::Minus {
        return None;
    }
    let (_, digits) = n.to_u64_digits();
    if digits.len() > 1 || digits.first().is_some_and(|&d| d > usize::MAX as u64) {
        return None;
    }
    digits.first().map(|&d| d as usize).or(Some(0))
}

/// Extract the module and function name from a callee expression.
///
/// Handles two AST forms that both represent a module-qualified function call:
/// - `TypedExpr::Var` with `ModuleFn` variant — produced by the type-checker
///   when the callee has already been resolved to a module function during
///   scope resolution (e.g. local `let f = fuzz.int_between` aliases).
/// - `TypedExpr::ModuleSelect` — the canonical representation of a qualified
///   call `module.function(...)` in source code (e.g. `fuzz.int_between(lo, hi)`).
fn extract_module_fn_identity(fun: &TypedExpr) -> Option<(String, String)> {
    let fun = terminal_expression(fun);
    match fun {
        TypedExpr::Var { constructor, .. } => match &constructor.variant {
            ValueConstructorVariant::ModuleFn { module, name, .. } => {
                Some((module.clone(), name.clone()))
            }
            _ => None,
        },
        TypedExpr::ModuleSelect {
            module_name,
            label,
            constructor: crate::tipo::ModuleValueConstructor::Fn { module, name, .. },
            ..
        } => {
            // Use the resolved module/name from the constructor when available,
            // falling back to the surface module_name/label. The constructor
            // carries the canonical module path even when external functions
            // redirect to a different module.
            let _ = (module_name, label); // surface names available for debugging
            Some((module.clone(), name.clone()))
        }
        TypedExpr::ModuleSelect { .. } => None,
        _ => None,
    }
}

/// Try to extract an integer literal from a TypedExpr.
/// Handles UInt literals, negated UInt literals, local variable aliases, and module constants.
///
/// Returns a `BigInt` to support arbitrary-precision integer bounds (e.g. values that
/// overflow `i128`). This is necessary because Aiken's `Int` type is arbitrary-precision
/// and stdlib fuzzers like `fuzz.int_at_least(2^127)` must be representable.
///
/// Used by the stdlib-gated primitive constraint extractor to read literal
/// bounds from calls such as `aiken/fuzz.int_between(lo, hi)`. Also covered
/// by tests that pin the recursion-depth invariant around constant aliasing.
fn try_extract_int_literal(
    expr: &TypedExpr,
    constant_index: &ConstantIndex<'_>,
    local_values: &BTreeMap<String, TypedExpr>,
) -> Option<BigInt> {
    try_extract_int_literal_inner(expr, constant_index, local_values, 0)
}

const INT_LITERAL_MAX_DEPTH: u8 = 16;

fn try_extract_int_literal_inner(
    expr: &TypedExpr,
    constant_index: &ConstantIndex<'_>,
    local_values: &BTreeMap<String, TypedExpr>,
    depth: u8,
) -> Option<BigInt> {
    if depth > INT_LITERAL_MAX_DEPTH {
        return None;
    }
    let expr = terminal_expression(expr);
    match expr {
        TypedExpr::UInt { value, .. } => value.parse::<BigInt>().ok(),
        TypedExpr::UnOp {
            op: UnOp::Negate,
            value,
            ..
        } => {
            let inner = try_extract_int_literal_inner(
                value.as_ref(),
                constant_index,
                local_values,
                depth + 1,
            )?;
            Some(-inner)
        }
        TypedExpr::Var {
            name, constructor, ..
        } => match &constructor.variant {
            ValueConstructorVariant::LocalVariable { .. } => {
                let bound = local_values.get(name)?;
                try_extract_int_literal_inner(bound, constant_index, local_values, depth + 1)
            }
            ValueConstructorVariant::ModuleConstant { module, name, .. } => {
                let const_expr = constant_index.get(module.as_str())?.get(name.as_str())?;
                try_extract_int_literal_inner(const_expr, constant_index, local_values, depth + 1)
            }
            _ => None,
        },
        _ => None,
    }
}

/// Try to extract an exact non-Int scalar value (Bool, String, ByteArray) from a TypedExpr.
#[cfg(test)]
fn try_extract_exact_scalar(expr: &TypedExpr) -> Option<FuzzerExactValue> {
    let expr = terminal_expression(expr);
    match expr {
        TypedExpr::Var {
            name, constructor, ..
        } if constructor.tipo.is_bool() => match &constructor.variant {
            ValueConstructorVariant::Record { arity, module, .. }
                if module.is_empty() && *arity == 0 =>
            {
                match name.as_str() {
                    "True" => Some(FuzzerExactValue::Bool(true)),
                    "False" => Some(FuzzerExactValue::Bool(false)),
                    _ => None,
                }
            }
            _ => None,
        },
        TypedExpr::String { value, .. } => Some(FuzzerExactValue::String(value.clone())),
        TypedExpr::ByteArray { bytes, .. } => Some(FuzzerExactValue::ByteArray(bytes.clone())),
        _ => None,
    }
}

#[derive(Debug, Clone, Copy)]
enum IntBoundSide {
    Min,
    Max,
}

/// Detect the "unbounded" string sentinels that
/// `try_extract_stdlib_primitive_constraint` plants when normalizing
/// `fuzz.int_at_least(_)` / `fuzz.int_at_most(_)` into the closed-range
/// `FuzzerConstraint::IntRange { min, max }` schema.
///
/// Returns `None` for the sentinel (unbounded side) and `Some(original)`
/// otherwise, so callers can build a `FuzzerSemantics::IntRange` whose
/// half-open structure faithfully describes the original fuzzer.
fn unbounded_int_sentinel_to_none(bound: &str, side: IntBoundSide) -> Option<String> {
    let sentinel = match side {
        IntBoundSide::Min => i128::MIN.to_string(),
        IntBoundSide::Max => i128::MAX.to_string(),
    };
    if bound == sentinel {
        None
    } else {
        Some(bound.to_string())
    }
}

/// Convert a known constraint into semantics when the types match.
fn semantics_from_known_constraint(
    constraint: &FuzzerConstraint,
    output_type: &Type,
) -> Option<FuzzerSemantics> {
    match constraint {
        FuzzerConstraint::IntRange { min, max } if output_type.is_int() => {
            // SOUNDNESS: `int_at_least`/`int_at_most` stuff i128::MIN/MAX into
            // the unbounded side as string sentinels (see
            // `try_extract_stdlib_primitive_constraint`). Runtime integers are
            // arbitrary-precision, so emitting those as literal Lean bounds
            // would narrow the verification domain and miss counterexamples
            // outside [i128::MIN, i128::MAX]. Strip the sentinels here so the
            // downstream Lean emitter produces a half-open formula.
            Some(FuzzerSemantics::IntRange {
                min: unbounded_int_sentinel_to_none(min, IntBoundSide::Min),
                max: unbounded_int_sentinel_to_none(max, IntBoundSide::Max),
            })
        }
        FuzzerConstraint::ByteStringLenRange { min_len, max_len } if output_type.is_bytearray() => {
            Some(FuzzerSemantics::ByteArrayRange {
                min_len: Some(*min_len),
                max_len: Some(*max_len),
            })
        }
        FuzzerConstraint::Exact(FuzzerExactValue::Bool(b)) if output_type.is_bool() => {
            Some(FuzzerSemantics::Exact(FuzzerExactValue::Bool(*b)))
        }
        FuzzerConstraint::Exact(FuzzerExactValue::ByteArray(bytes))
            if output_type.is_bytearray() =>
        {
            Some(FuzzerSemantics::Exact(FuzzerExactValue::ByteArray(
                bytes.clone(),
            )))
        }
        FuzzerConstraint::Exact(FuzzerExactValue::String(value)) if output_type.is_string() => {
            Some(FuzzerSemantics::Exact(FuzzerExactValue::String(
                value.clone(),
            )))
        }
        FuzzerConstraint::OneOf(values) => {
            match canonicalize_finite_scalar_domain(output_type, values.clone()) {
                Ok(CanonicalFiniteScalarDomain::Exact(value)) => {
                    Some(FuzzerSemantics::Exact(value))
                }
                Ok(CanonicalFiniteScalarDomain::OneOf(values)) => {
                    Some(FuzzerSemantics::OneOf(values))
                }
                Err(_) => None,
            }
        }
        _ => None,
    }
}

/// Try to extract list length bounds structurally, without matching on
/// function names.
///
/// A list-shaped fuzzer call (return type `Fuzzer<List<T>>` with exactly one
/// Fuzzer argument) *may* accept scalar length bounds, but we cannot know
/// from the call alone that the non-Fuzzer scalar arguments are interpreted
/// as lengths. The user's constraint is that name matching (e.g. gating on
/// `list_between`/`list_at_least`/`list_at_most`) is not permitted, so the
/// safe and sound default is to return no length bounds: downstream
/// verification then quantifies universally over all list lengths, which
/// widens the proof obligation without under-approximating any domain.
///
/// Kept as a function (rather than inlined) so it remains an extension
/// point: a future version may structurally inspect the callee's body to
/// prove that particular scalar args constrain the output list's length.
fn try_extract_list_length_bounds(
    expr: &TypedExpr,
    args: &[CallArg<TypedExpr>],
    constant_index: &ConstantIndex<'_>,
    local_values: &BTreeMap<String, TypedExpr>,
) -> (Option<usize>, Option<usize>) {
    // Name-gated extraction: only trust literal length arguments when the
    // callee is a stdlib `aiken/fuzz` list constructor. Unknown wrappers
    // fall through to `(None, None)` so the verifier over-approximates.
    let TypedExpr::Call { fun, .. } = expr else {
        return (None, None);
    };
    let Some((module, fn_name)) = extract_module_fn_identity(fun) else {
        return (None, None);
    };
    if module != STDLIB_FUZZ_MODULE {
        return (None, None);
    }

    let extract_len = |arg: &CallArg<TypedExpr>| -> Option<usize> {
        let n = try_extract_int_literal(&arg.value, constant_index, local_values)?;
        // List lengths must be non-negative and fit in usize.
        if n.sign() == num_bigint::Sign::Minus {
            return None;
        }
        let (_, digits) = n.to_u64_digits();
        if digits.len() > 1 || digits.first().is_some_and(|&d| d > usize::MAX as u64) {
            return None;
        }
        digits.first().map(|&d| d as usize).or(Some(0))
    };

    match (fn_name.as_str(), args) {
        ("list_between", [_elem, min_arg, max_arg]) => {
            let min = extract_len(min_arg);
            let max = extract_len(max_arg);
            // Normalize swapped args so min ≤ max.
            match (min, max) {
                (Some(a), Some(b)) if a > b => (Some(b), Some(a)),
                _ => (min, max),
            }
        }
        ("list_at_least", [_elem, min_arg]) => (extract_len(min_arg), None),
        ("list_at_most", [_elem, max_arg]) => (None, extract_len(max_arg)),
        _ => (None, None),
    }
}

fn normalize_structural_fuzzer_call(
    expr: &TypedExpr,
    args: &[CallArg<TypedExpr>],
    current_module: &str,
    function_index: &FunctionIndex<'_>,
    constant_index: &ConstantIndex<'_>,
    local_values: &BTreeMap<String, TypedExpr>,
    visiting_functions: &mut BTreeSet<(String, String)>,
) -> Option<NormalizedFuzzer> {
    if let [source, mapper] = args {
        if expression_has_fuzzer_type(&source.value) {
            if expression_is_bind_continuation(&mapper.value) {
                return Some(NormalizedFuzzer::Bind {
                    source: Box::new(normalize_fuzzer_from_expr(
                        &source.value,
                        current_module,
                        function_index,
                        constant_index,
                        local_values,
                        visiting_functions,
                    )),
                    result: Box::new(normalize_fuzzer_from_continuation(
                        &mapper.value,
                        current_module,
                        function_index,
                        constant_index,
                        local_values,
                        visiting_functions,
                    )),
                });
            }

            if expression_is_pure_mapper(&mapper.value) {
                let source_output_type = extract_fuzzer_payload_type(source.value.tipo().as_ref())?;
                let output_type = extract_fuzzer_payload_type(expr.tipo().as_ref())?;
                let source_expr = &source.value;
                let source = normalize_fuzzer_from_expr(
                    source_expr,
                    current_module,
                    function_index,
                    constant_index,
                    local_values,
                    visiting_functions,
                );
                let mut mapper_shape = summarize_unary_mapper_shape(
                    &mapper.value,
                    current_module,
                    function_index,
                    local_values,
                );
                if let Some(finite_shape) = summarize_finite_scalar_mapper_shape(
                    &source,
                    &mapper.value,
                    output_type.as_ref(),
                    current_module,
                    function_index,
                    local_values,
                ) {
                    mapper_shape = finite_shape;
                }

                let mapper_returns_bool =
                    function_return_type(&mapper.value).is_some_and(|(_, ret)| ret.is_bool());
                let is_such_that_filter = match expr {
                    TypedExpr::Call { fun, .. } => extract_module_fn_identity(fun.as_ref()),
                    _ => None,
                }
                .is_some_and(|(module, fn_name)| {
                    module == STDLIB_FUZZ_MODULE && fn_name == "such_that"
                });

                if is_such_that_filter
                    && mapper_returns_bool
                    && source_output_type.as_ref() == output_type.as_ref()
                {
                    return Some(source);
                }

                if mapper_shape == UnaryMapperShape::Identity {
                    return Some(source);
                }

                return Some(NormalizedFuzzer::Map {
                    source: Box::new(source),
                    source_output_type,
                    output_type,
                    mapper_shape,
                });
            }
        }
    }

    // Only classify as Product when the output type is actually a tuple or pair.
    // This prevents custom helpers with cross-element invariants from being modeled
    // as cartesian products.
    let output_is_product = extract_fuzzer_payload_type(expr.tipo().as_ref())
        .is_some_and(|t| t.is_tuple() || t.is_pair());

    if output_is_product
        && args.len() >= 2
        && args
            .iter()
            .all(|arg| expression_has_fuzzer_type(&arg.value))
    {
        return Some(NormalizedFuzzer::Product {
            elements: args
                .iter()
                .map(|arg| {
                    normalize_fuzzer_from_expr(
                        &arg.value,
                        current_module,
                        function_index,
                        constant_index,
                        local_values,
                        visiting_functions,
                    )
                })
                .collect(),
        });
    }

    if output_is_product && args.len() >= 3 {
        let arity = args.len() - 1;
        let sources = &args[..arity];
        let mapper = &args[arity].value;

        if sources
            .iter()
            .all(|arg| expression_has_fuzzer_type(&arg.value))
            && mapn_mapper_arg_order(mapper, arity, current_module, function_index, local_values)
                .is_some()
        {
            let normalized_sources: Vec<NormalizedFuzzer> = sources
                .iter()
                .map(|arg| {
                    normalize_fuzzer_from_expr(
                        &arg.value,
                        current_module,
                        function_index,
                        constant_index,
                        local_values,
                        visiting_functions,
                    )
                })
                .collect();

            let ordered =
                mapn_mapper_arg_order(mapper, arity, current_module, function_index, local_values)
                    .expect("checked is_some above")
                    .into_iter()
                    .map(|index| normalized_sources[index].clone())
                    .collect();

            return Some(NormalizedFuzzer::Product { elements: ordered });
        }
    }
    if let Some(output_type) = extract_fuzzer_payload_type(expr.tipo().as_ref()) {
        if output_type.is_list() {
            let inner_types = output_type.get_inner_types();
            let fuzzer_args: Vec<&CallArg<TypedExpr>> = args
                .iter()
                .filter(|arg| expression_has_fuzzer_type(&arg.value))
                .collect();

            if inner_types.len() == 1 && fuzzer_args.len() == 1 && args.len() <= 3 {
                if let Some(source_output_type) =
                    extract_fuzzer_payload_type(fuzzer_args[0].value.tipo().as_ref())
                {
                    if source_output_type.as_ref() == inner_types[0].as_ref() {
                        let (min_len, max_len) = try_extract_list_length_bounds(
                            expr,
                            args,
                            constant_index,
                            local_values,
                        );
                        return Some(NormalizedFuzzer::List {
                            element: Box::new(normalize_fuzzer_from_expr(
                                &fuzzer_args[0].value,
                                current_module,
                                function_index,
                                constant_index,
                                local_values,
                                visiting_functions,
                            )),
                            min_len,
                            max_len,
                        });
                    }
                }
            }
        }
    }

    None
}

fn normalize_fuzzer_from_continuation(
    continuation: &TypedExpr,
    current_module: &str,
    function_index: &FunctionIndex<'_>,
    constant_index: &ConstantIndex<'_>,
    local_values: &BTreeMap<String, TypedExpr>,
    visiting_functions: &mut BTreeSet<(String, String)>,
) -> NormalizedFuzzer {
    let continuation = terminal_expression(continuation);

    match continuation {
        TypedExpr::Fn { body, .. } => normalize_fuzzer_from_expr(
            body,
            current_module,
            function_index,
            constant_index,
            local_values,
            visiting_functions,
        ),
        _ => {
            let Some((resolved, resolved_locals, _applied_arg_count)) =
                resolve_function_with_applied_args(
                    continuation,
                    current_module,
                    function_index,
                    local_values,
                )
            else {
                return opaque_normalized_fuzzer(
                    continuation,
                    "bind continuation is not a resolvable function",
                );
            };

            let key = (resolved.module_name.clone(), resolved.function_name.clone());
            if !visiting_functions.insert(key.clone()) {
                return opaque_normalized_fuzzer(
                    continuation,
                    format!(
                        "recursive bind continuation detected at {}.{}",
                        resolved.module_name, resolved.function_name
                    ),
                );
            }

            let result = normalize_fuzzer_from_expr(
                &resolved.function.body,
                &resolved.module_name,
                function_index,
                constant_index,
                &resolved_locals,
                visiting_functions,
            );
            visiting_functions.remove(&key);
            result
        }
    }
}

fn normalize_fuzzer_from_helper_call(
    fun: &TypedExpr,
    args: &[CallArg<TypedExpr>],
    current_module: &str,
    function_index: &FunctionIndex<'_>,
    constant_index: &ConstantIndex<'_>,
    local_values: &BTreeMap<String, TypedExpr>,
    visiting_functions: &mut BTreeSet<(String, String)>,
) -> Option<NormalizedFuzzer> {
    let mut visiting_local_aliases = BTreeSet::new();
    let resolved = resolve_function_from_expr(
        fun,
        current_module,
        function_index,
        local_values,
        &mut visiting_local_aliases,
    )?;

    if args.len() > resolved.function.arguments.len() {
        return None;
    }

    let key = (resolved.module_name.clone(), resolved.function_name.clone());
    if !visiting_functions.insert(key.clone()) {
        return Some(opaque_normalized_fuzzer(
            fun,
            format!(
                "recursive helper fuzzer detected at {}.{}",
                resolved.module_name, resolved.function_name
            ),
        ));
    }

    let mut helper_locals = local_values.clone();
    for (param, arg) in resolved.function.arguments.iter().zip(args.iter()) {
        if let Some(name) = param.get_variable_name() {
            let mut visiting_local_aliases = BTreeSet::new();
            let materialized = materialize_local_alias_argument(
                &arg.value,
                local_values,
                &mut visiting_local_aliases,
            );
            helper_locals.insert(name.to_string(), materialized);
        }
    }

    let result = normalize_fuzzer_from_expr(
        &resolved.function.body,
        &resolved.module_name,
        function_index,
        constant_index,
        &helper_locals,
        visiting_functions,
    );
    visiting_functions.remove(&key);
    Some(result)
}

fn normalize_fuzzer_from_resolved_function(
    expr: &TypedExpr,
    current_module: &str,
    function_index: &FunctionIndex<'_>,
    constant_index: &ConstantIndex<'_>,
    local_values: &BTreeMap<String, TypedExpr>,
    visiting_functions: &mut BTreeSet<(String, String)>,
) -> Option<NormalizedFuzzer> {
    let (resolved, resolved_locals, applied_arg_count) =
        resolve_function_with_applied_args(expr, current_module, function_index, local_values)?;
    let remaining_args = resolved
        .function
        .arguments
        .len()
        .saturating_sub(applied_arg_count);

    if remaining_args != 0 {
        return None;
    }

    let key = (resolved.module_name.clone(), resolved.function_name.clone());
    if !visiting_functions.insert(key.clone()) {
        return Some(opaque_normalized_fuzzer(
            expr,
            format!(
                "recursive helper fuzzer detected at {}.{}",
                resolved.module_name, resolved.function_name
            ),
        ));
    }

    let result = normalize_fuzzer_from_expr(
        &resolved.function.body,
        &resolved.module_name,
        function_index,
        constant_index,
        &resolved_locals,
        visiting_functions,
    );
    visiting_functions.remove(&key);
    Some(result)
}

/// S2 SUBSET: Beta-reduce a call whose callee is an `Fn` literal
/// (directly or reached through a chain of local aliases).
///
/// Returns `Some(normalized_body)` when the callee resolves to a
/// `TypedExpr::Fn { args: params, body, .. }` with `params.len() == args.len()`,
/// in which case the body is normalized in an environment extended with
/// bindings `param_name -> actual_arg_expr` for each parameter.
///
/// Returns `None` if the callee is not an `Fn` literal or if arity does
/// not match (partial application is not supported here — the existing
/// helper/resolved-function descent handles those cases via
/// `resolve_function_with_applied_args`).
///
/// This fixes the long-standing gap where zero-argument thunks like
/// `fn() { scenario_inputs_baseline(st) }` passed to `fork*_and_then` are
/// invoked as `baseline()` inside the stdlib body, but `baseline` is only
/// known locally — `resolve_function_from_expr` refuses to descend into a
/// raw `Fn` literal, so the call would otherwise normalize to `Opaque`.
#[allow(clippy::too_many_arguments)]
fn try_beta_reduce_fuzzer_call(
    fun: &TypedExpr,
    args: &[CallArg<TypedExpr>],
    current_module: &str,
    function_index: &FunctionIndex<'_>,
    constant_index: &ConstantIndex<'_>,
    local_values: &BTreeMap<String, TypedExpr>,
    visiting_functions: &mut BTreeSet<(String, String)>,
) -> Option<NormalizedFuzzer> {
    // Walk through local aliases and nested calls to reach the ultimate
    // callee head and collect any pre-applied arguments along the way.
    let (head, preapplied) = flatten_call_head_and_args(fun, &[], local_values)?;

    let TypedExpr::Fn {
        args: fn_params,
        body,
        ..
    } = terminal_expression(&head)
    else {
        return None;
    };

    // Only handle fully-saturated application for now. Partial application of
    // an inline `Fn` is rare in practice for fuzzer combinators and is safer
    // to leave to the existing opaque-fallback path.
    let total_args = preapplied.len() + args.len();
    if fn_params.len() != total_args {
        return None;
    }

    // Bind each formal parameter to the corresponding actual argument in a
    // fresh scope derived from the caller's locals. Materialize local-alias
    // arguments eagerly so the callee body sees the concrete expression
    // rather than an opaque local Var.
    let mut bound_locals = local_values.clone();
    let all_args: Vec<TypedExpr> = preapplied
        .into_iter()
        .chain(args.iter().map(|a| a.value.clone()))
        .collect();
    for (param, arg) in fn_params.iter().zip(all_args.iter()) {
        if let Some(name) = param.get_variable_name() {
            let mut visiting_local_aliases = BTreeSet::new();
            let materialized =
                materialize_local_alias_argument(arg, local_values, &mut visiting_local_aliases);
            bound_locals.insert(name.to_string(), materialized);
        }
    }

    Some(normalize_fuzzer_from_expr(
        body,
        current_module,
        function_index,
        constant_index,
        &bound_locals,
        visiting_functions,
    ))
}

fn expression_has_fuzzer_type(expr: &TypedExpr) -> bool {
    extract_fuzzer_payload_type(expr.tipo().as_ref()).is_some()
}

fn expression_is_pure_mapper(expr: &TypedExpr) -> bool {
    !expression_has_fuzzer_type(expr)
        && function_return_type(expr).is_some_and(|(args, ret)| {
            args.len() == 1 && extract_fuzzer_payload_type(ret.as_ref()).is_none()
        })
}

fn summarize_unary_mapper_shape(
    mapper: &TypedExpr,
    current_module: &str,
    function_index: &FunctionIndex<'_>,
    local_values: &BTreeMap<String, TypedExpr>,
) -> UnaryMapperShape {
    let mut mapper_expr = terminal_expression(mapper).clone();
    let mut mapper_module = current_module.to_string();
    let mut mapper_locals = local_values.clone();
    let mut visiting_functions = BTreeSet::new();

    loop {
        let mapper = terminal_expression(&mapper_expr);
        match mapper {
            TypedExpr::Fn { args, body, .. } => {
                return summarize_unary_mapper_body(args, body, &mapper_locals);
            }
            _ => {
                let Some((resolved, resolved_locals, applied_arg_count)) =
                    resolve_function_with_applied_args(
                        mapper,
                        &mapper_module,
                        function_index,
                        &mapper_locals,
                    )
                else {
                    return UnaryMapperShape::Unknown;
                };

                let key = (resolved.module_name.clone(), resolved.function_name.clone());
                if !visiting_functions.insert(key) {
                    return UnaryMapperShape::Unknown;
                }

                let remaining_args = resolved
                    .function
                    .arguments
                    .len()
                    .saturating_sub(applied_arg_count);

                if remaining_args == 1 {
                    return summarize_unary_mapper_body(
                        &resolved.function.arguments[applied_arg_count..],
                        &resolved.function.body,
                        &resolved_locals,
                    );
                }

                if remaining_args == 0 {
                    mapper_expr = resolved.function.body.clone();
                    mapper_module = resolved.module_name;
                    mapper_locals = resolved_locals;
                    continue;
                }

                return UnaryMapperShape::Unknown;
            }
        }
    }
}

fn summarize_unary_mapper_body(
    args: &[TypedArg],
    body: &TypedExpr,
    local_values: &BTreeMap<String, TypedExpr>,
) -> UnaryMapperShape {
    if args.len() != 1 {
        return UnaryMapperShape::Unknown;
    }
    let Some(arg_name) = args[0].get_variable_name() else {
        return UnaryMapperShape::Unknown;
    };

    let mut visiting_local_aliases = BTreeSet::new();
    if let Some(constant_shape) =
        resolve_exact_constant_mapper(body, local_values, &mut visiting_local_aliases)
    {
        return constant_shape;
    }

    let mut visiting_local_aliases = BTreeSet::new();
    if let Some(constant_shape) =
        resolve_tautological_bool_mapper(body, arg_name, local_values, &mut visiting_local_aliases)
    {
        return constant_shape;
    }

    let mut visiting_local_aliases = BTreeSet::new();
    if let Some(constructor_map) = resolve_nullary_constructor_mapper(
        body,
        arg_name,
        local_values,
        &mut visiting_local_aliases,
    ) {
        return UnaryMapperShape::ConstructorMap(constructor_map);
    }

    let mut visiting_local_aliases = BTreeSet::new();
    if resolve_identity_mapper(body, arg_name, local_values, &mut visiting_local_aliases) {
        return UnaryMapperShape::Identity;
    }

    let mut visiting_local_aliases = BTreeSet::new();
    let Some((scale, offset)) =
        resolve_int_affine_mapper(body, arg_name, local_values, &mut visiting_local_aliases)
    else {
        return UnaryMapperShape::Unknown;
    };

    if scale == BigInt::from(0) {
        return UnaryMapperShape::ConstInt(offset.to_string());
    }
    if scale == BigInt::from(1) {
        if offset == BigInt::from(0) {
            return UnaryMapperShape::Identity;
        }

        return UnaryMapperShape::IntAffine {
            scale: 1,
            offset: offset.to_string(),
        };
    }
    if scale == BigInt::from(-1) {
        return UnaryMapperShape::IntAffine {
            scale: -1,
            offset: offset.to_string(),
        };
    }

    UnaryMapperShape::Unknown
}

fn resolve_identity_mapper(
    expr: &TypedExpr,
    arg_name: &str,
    local_values: &BTreeMap<String, TypedExpr>,
    visiting_local_aliases: &mut BTreeSet<String>,
) -> bool {
    let expr = terminal_expression(expr);
    match expr {
        TypedExpr::Var {
            name, constructor, ..
        } if matches!(
            constructor.variant,
            ValueConstructorVariant::LocalVariable { .. }
        ) =>
        {
            if name == arg_name {
                return true;
            }

            let Some(bound_expr) = local_values.get(name) else {
                return false;
            };
            if !visiting_local_aliases.insert(name.clone()) {
                return false;
            }

            let resolved =
                resolve_identity_mapper(bound_expr, arg_name, local_values, visiting_local_aliases);
            visiting_local_aliases.remove(name);
            resolved
        }
        _ => false,
    }
}

fn resolve_exact_constant_mapper(
    expr: &TypedExpr,
    local_values: &BTreeMap<String, TypedExpr>,
    visiting_local_aliases: &mut BTreeSet<String>,
) -> Option<UnaryMapperShape> {
    let expr = terminal_expression(expr);
    match expr {
        TypedExpr::Var {
            name, constructor, ..
        } if matches!(
            constructor.variant,
            ValueConstructorVariant::LocalVariable { .. }
        ) =>
        {
            let bound_expr = local_values.get(name)?;
            if !visiting_local_aliases.insert(name.clone()) {
                return None;
            }

            let resolved =
                resolve_exact_constant_mapper(bound_expr, local_values, visiting_local_aliases);
            visiting_local_aliases.remove(name);
            resolved
        }
        TypedExpr::Var {
            name, constructor, ..
        } if constructor.tipo.is_bool() => match &constructor.variant {
            ValueConstructorVariant::Record { arity, module, .. }
                if module.is_empty() && *arity == 0 =>
            {
                match name.as_str() {
                    "True" => Some(UnaryMapperShape::ConstBool(true)),
                    "False" => Some(UnaryMapperShape::ConstBool(false)),
                    _ => None,
                }
            }
            _ => None,
        },
        TypedExpr::String { value, .. } => Some(UnaryMapperShape::ConstString(value.clone())),
        TypedExpr::ByteArray { bytes, .. } => Some(UnaryMapperShape::ConstByteArray(bytes.clone())),
        _ => None,
    }
}

fn resolve_tautological_bool_mapper(
    expr: &TypedExpr,
    arg_name: &str,
    local_values: &BTreeMap<String, TypedExpr>,
    visiting_local_aliases: &mut BTreeSet<String>,
) -> Option<UnaryMapperShape> {
    let expr = terminal_expression(expr);
    match expr {
        TypedExpr::Var {
            name, constructor, ..
        } if matches!(
            constructor.variant,
            ValueConstructorVariant::LocalVariable { .. }
        ) =>
        {
            let bound_expr = local_values.get(name)?;
            if !visiting_local_aliases.insert(name.clone()) {
                return None;
            }

            let resolved = resolve_tautological_bool_mapper(
                bound_expr,
                arg_name,
                local_values,
                visiting_local_aliases,
            );
            visiting_local_aliases.remove(name);
            resolved
        }
        TypedExpr::BinOp {
            name, left, right, ..
        } if matches!(name, BinOp::Eq | BinOp::NotEq) => {
            let mut visiting_left_aliases = BTreeSet::new();
            let mut visiting_right_aliases = BTreeSet::new();

            if resolve_identity_mapper(
                left.as_ref(),
                arg_name,
                local_values,
                &mut visiting_left_aliases,
            ) && resolve_identity_mapper(
                right.as_ref(),
                arg_name,
                local_values,
                &mut visiting_right_aliases,
            ) {
                Some(UnaryMapperShape::ConstBool(matches!(name, BinOp::Eq)))
            } else {
                None
            }
        }
        _ => None,
    }
}

fn resolve_nullary_constructor_mapper(
    expr: &TypedExpr,
    arg_name: &str,
    local_values: &BTreeMap<String, TypedExpr>,
    visiting_local_aliases: &mut BTreeSet<String>,
) -> Option<BTreeMap<String, String>> {
    let expr = terminal_expression(expr);
    match expr {
        TypedExpr::Var {
            name, constructor, ..
        } if matches!(
            constructor.variant,
            ValueConstructorVariant::LocalVariable { .. }
        ) =>
        {
            if name == arg_name {
                return None;
            }

            let bound_expr = local_values.get(name)?;
            if !visiting_local_aliases.insert(name.clone()) {
                return None;
            }

            let resolved = resolve_nullary_constructor_mapper(
                bound_expr,
                arg_name,
                local_values,
                visiting_local_aliases,
            );
            visiting_local_aliases.remove(name);
            resolved
        }
        TypedExpr::When {
            subject, clauses, ..
        } => {
            let mut visiting_subject_aliases = BTreeSet::new();
            if !expression_resolves_to_local_name(
                subject.as_ref(),
                arg_name,
                local_values,
                &mut visiting_subject_aliases,
            ) {
                return None;
            }

            let mut constructor_map = BTreeMap::new();
            for clause in clauses {
                let source_constructor = nullary_constructor_pattern_name(&clause.pattern)?;
                let mut visiting_then_aliases = BTreeSet::new();
                let output_constructor = resolve_nullary_constructor_value_name(
                    &clause.then,
                    local_values,
                    &mut visiting_then_aliases,
                )?;

                if constructor_map
                    .insert(source_constructor, output_constructor)
                    .is_some()
                {
                    return None;
                }
            }

            if constructor_map.is_empty() {
                return None;
            }

            Some(constructor_map)
        }
        _ => None,
    }
}

fn expression_resolves_to_local_name(
    expr: &TypedExpr,
    target_name: &str,
    local_values: &BTreeMap<String, TypedExpr>,
    visiting_local_aliases: &mut BTreeSet<String>,
) -> bool {
    let expr = terminal_expression(expr);
    match expr {
        TypedExpr::Var {
            name, constructor, ..
        } if matches!(
            constructor.variant,
            ValueConstructorVariant::LocalVariable { .. }
        ) =>
        {
            if name == target_name {
                return true;
            }

            let Some(bound_expr) = local_values.get(name) else {
                return false;
            };
            if !visiting_local_aliases.insert(name.clone()) {
                return false;
            }

            let resolves = expression_resolves_to_local_name(
                bound_expr,
                target_name,
                local_values,
                visiting_local_aliases,
            );
            visiting_local_aliases.remove(name);
            resolves
        }
        _ => false,
    }
}

fn nullary_constructor_pattern_name(pattern: &TypedPattern) -> Option<String> {
    match pattern {
        TypedPattern::Assign { pattern, .. } => nullary_constructor_pattern_name(pattern.as_ref()),
        TypedPattern::Constructor {
            name, arguments, ..
        } if arguments.is_empty() => Some(name.clone()),
        _ => None,
    }
}

fn resolve_nullary_constructor_value_name(
    expr: &TypedExpr,
    local_values: &BTreeMap<String, TypedExpr>,
    visiting_local_aliases: &mut BTreeSet<String>,
) -> Option<String> {
    let expr = terminal_expression(expr);
    match expr {
        TypedExpr::Var {
            name, constructor, ..
        } if matches!(
            constructor.variant,
            ValueConstructorVariant::LocalVariable { .. }
        ) =>
        {
            let bound_expr = local_values.get(name)?;
            if !visiting_local_aliases.insert(name.clone()) {
                return None;
            }

            let resolved = resolve_nullary_constructor_value_name(
                bound_expr,
                local_values,
                visiting_local_aliases,
            );
            visiting_local_aliases.remove(name);
            resolved
        }
        TypedExpr::Var {
            name, constructor, ..
        } => match &constructor.variant {
            ValueConstructorVariant::Record { arity, .. } if *arity == 0 => Some(name.clone()),
            _ => None,
        },
        _ => None,
    }
}

fn resolve_int_affine_mapper(
    expr: &TypedExpr,
    arg_name: &str,
    local_values: &BTreeMap<String, TypedExpr>,
    visiting_local_aliases: &mut BTreeSet<String>,
) -> Option<(BigInt, BigInt)> {
    let expr = terminal_expression(expr);
    match expr {
        TypedExpr::UInt { value, base, .. } => {
            Some((BigInt::from(0), parse_uint_bigint(value, base)?))
        }
        TypedExpr::Var {
            name, constructor, ..
        } if constructor.tipo.is_int()
            && matches!(
                constructor.variant,
                ValueConstructorVariant::LocalVariable { .. }
            ) =>
        {
            if name == arg_name {
                return Some((BigInt::from(1), BigInt::from(0)));
            }

            let bound_expr = local_values.get(name)?;
            if !visiting_local_aliases.insert(name.clone()) {
                return None;
            }

            let resolved = resolve_int_affine_mapper(
                bound_expr,
                arg_name,
                local_values,
                visiting_local_aliases,
            );
            visiting_local_aliases.remove(name);
            resolved
        }
        TypedExpr::UnOp {
            op: UnOp::Negate,
            value,
            ..
        } => {
            let (scale, offset) = resolve_int_affine_mapper(
                value.as_ref(),
                arg_name,
                local_values,
                visiting_local_aliases,
            )?;
            Some((-scale, -offset))
        }
        TypedExpr::BinOp {
            name, left, right, ..
        } => {
            let (left_scale, left_offset) = resolve_int_affine_mapper(
                left.as_ref(),
                arg_name,
                local_values,
                visiting_local_aliases,
            )?;
            let (right_scale, right_offset) = resolve_int_affine_mapper(
                right.as_ref(),
                arg_name,
                local_values,
                visiting_local_aliases,
            )?;

            match name {
                BinOp::AddInt => Some((left_scale + right_scale, left_offset + right_offset)),
                BinOp::SubInt => Some((left_scale - right_scale, left_offset - right_offset)),
                _ => None,
            }
        }
        _ => None,
    }
}

#[derive(Debug, Clone, PartialEq)]
enum FiniteEvalValue {
    Int(BigInt),
    Bool(bool),
    ByteArray(Vec<u8>),
    String(String),
}

fn normalized_int_range(normalized: &NormalizedFuzzer) -> Option<(String, String)> {
    match normalized {
        NormalizedFuzzer::Primitive {
            known_constraint: Some(FuzzerConstraint::IntRange { min, max }),
            ..
        } => Some((min.clone(), max.clone())),
        _ => None,
    }
}

fn enumerate_capped_int_range(min: &str, max: &str, cap: usize) -> Option<Vec<BigInt>> {
    let lo = parse_decimal_bigint(min)?;
    let hi = parse_decimal_bigint(max)?;
    if lo > hi {
        return None;
    }

    let cases = &hi - &lo + BigInt::from(1);
    if cases > BigInt::from(cap) {
        return None;
    }

    let mut values = Vec::new();
    let mut current = lo;
    while current <= hi {
        values.push(current.clone());
        current += 1;
    }
    Some(values)
}

fn summarize_finite_scalar_mapper_shape(
    source: &NormalizedFuzzer,
    mapper: &TypedExpr,
    output_type: &Type,
    current_module: &str,
    function_index: &FunctionIndex<'_>,
    local_values: &BTreeMap<String, TypedExpr>,
) -> Option<UnaryMapperShape> {
    if !output_type.is_string() {
        return None;
    }

    let (min, max) = normalized_int_range(source)?;
    let source_values = enumerate_capped_int_range(&min, &max, MAX_FINITE_MAPPER_SOURCE_CASES)?;

    let mut mapper_expr = terminal_expression(mapper).clone();
    let mut mapper_module = current_module.to_string();
    let mut mapper_locals = local_values.clone();
    let mut visiting_functions = BTreeSet::new();

    loop {
        let mapper = terminal_expression(&mapper_expr);
        match mapper {
            TypedExpr::Fn { args, body, .. } => {
                return evaluate_finite_scalar_mapper_body(
                    args,
                    body,
                    &mapper_locals,
                    source_values,
                );
            }
            _ => {
                let Some((resolved, resolved_locals, applied_arg_count)) =
                    resolve_function_with_applied_args(
                        mapper,
                        &mapper_module,
                        function_index,
                        &mapper_locals,
                    )
                else {
                    return None;
                };

                let key = (resolved.module_name.clone(), resolved.function_name.clone());
                if !visiting_functions.insert(key) {
                    return None;
                }

                let remaining_args = resolved
                    .function
                    .arguments
                    .len()
                    .saturating_sub(applied_arg_count);

                if remaining_args == 1 {
                    return evaluate_finite_scalar_mapper_body(
                        &resolved.function.arguments[applied_arg_count..],
                        &resolved.function.body,
                        &resolved_locals,
                        source_values,
                    );
                }

                if remaining_args == 0 {
                    mapper_expr = resolved.function.body.clone();
                    mapper_module = resolved.module_name;
                    mapper_locals = resolved_locals;
                    continue;
                }

                return None;
            }
        }
    }
}

fn evaluate_finite_scalar_mapper_body(
    args: &[TypedArg],
    body: &TypedExpr,
    local_values: &BTreeMap<String, TypedExpr>,
    source_values: Vec<BigInt>,
) -> Option<UnaryMapperShape> {
    if args.len() != 1 {
        return None;
    }
    let arg_name = args[0].get_variable_name()?;

    let mut values = Vec::new();
    for source_value in source_values {
        let mut env = BTreeMap::new();
        env.insert(arg_name.to_string(), FiniteEvalValue::Int(source_value));
        let value = eval_finite_mapper_expr(body, arg_name, local_values, &mut env)?;
        match value {
            FiniteEvalValue::String(value) => values.push(FuzzerExactValue::String(value)),
            FiniteEvalValue::ByteArray(bytes) => values.push(FuzzerExactValue::ByteArray(bytes)),
            FiniteEvalValue::Bool(value) => values.push(FuzzerExactValue::Bool(value)),
            FiniteEvalValue::Int(_) => return None,
        }
    }

    if values.is_empty() {
        None
    } else {
        Some(UnaryMapperShape::FiniteScalar(values))
    }
}

fn eval_finite_mapper_expr(
    expr: &TypedExpr,
    arg_name: &str,
    local_values: &BTreeMap<String, TypedExpr>,
    env: &mut BTreeMap<String, FiniteEvalValue>,
) -> Option<FiniteEvalValue> {
    match expr {
        TypedExpr::Sequence { expressions, .. } | TypedExpr::Pipeline { expressions, .. } => {
            eval_finite_mapper_sequence(expressions, arg_name, local_values, env)
        }
        TypedExpr::Trace { then, .. } => {
            eval_finite_mapper_expr(then.as_ref(), arg_name, local_values, env)
        }
        TypedExpr::UInt { value, base, .. } => {
            Some(FiniteEvalValue::Int(parse_uint_bigint(value, base)?))
        }
        TypedExpr::String { value, .. } => Some(FiniteEvalValue::String(value.clone())),
        TypedExpr::ByteArray { bytes, .. } => Some(FiniteEvalValue::ByteArray(bytes.clone())),
        TypedExpr::Var {
            name, constructor, ..
        } if matches!(
            constructor.variant,
            ValueConstructorVariant::LocalVariable { .. }
        ) =>
        {
            if let Some(value) = env.get(name) {
                return Some(value.clone());
            }
            let bound_expr = local_values.get(name)?;
            eval_finite_mapper_expr(bound_expr, arg_name, local_values, env)
        }
        TypedExpr::Var {
            name, constructor, ..
        } if constructor.tipo.is_bool() => match &constructor.variant {
            ValueConstructorVariant::Record { arity, module, .. }
                if module.is_empty() && *arity == 0 =>
            {
                match name.as_str() {
                    "True" => Some(FiniteEvalValue::Bool(true)),
                    "False" => Some(FiniteEvalValue::Bool(false)),
                    _ => None,
                }
            }
            _ => None,
        },
        TypedExpr::If {
            branches,
            final_else,
            ..
        } => {
            for branch in branches {
                if branch.is.is_some() {
                    return None;
                }
                let condition =
                    eval_finite_mapper_expr(&branch.condition, arg_name, local_values, env)?;
                let FiniteEvalValue::Bool(condition) = condition else {
                    return None;
                };
                if condition {
                    return eval_finite_mapper_expr(&branch.body, arg_name, local_values, env);
                }
            }
            eval_finite_mapper_expr(final_else.as_ref(), arg_name, local_values, env)
        }
        TypedExpr::When {
            subject, clauses, ..
        } => {
            let subject = eval_finite_mapper_expr(subject.as_ref(), arg_name, local_values, env)?;
            for clause in clauses {
                if finite_eval_value_matches_pattern(&subject, &clause.pattern)? {
                    return eval_finite_mapper_expr(&clause.then, arg_name, local_values, env);
                }
            }
            None
        }
        TypedExpr::BinOp {
            name, left, right, ..
        } => {
            let left = eval_finite_mapper_expr(left.as_ref(), arg_name, local_values, env)?;
            match name {
                BinOp::And => {
                    let FiniteEvalValue::Bool(left) = left else {
                        return None;
                    };
                    if !left {
                        return Some(FiniteEvalValue::Bool(false));
                    }
                    let right =
                        eval_finite_mapper_expr(right.as_ref(), arg_name, local_values, env)?;
                    let FiniteEvalValue::Bool(right) = right else {
                        return None;
                    };
                    Some(FiniteEvalValue::Bool(right))
                }
                BinOp::Or => {
                    let FiniteEvalValue::Bool(left) = left else {
                        return None;
                    };
                    if left {
                        return Some(FiniteEvalValue::Bool(true));
                    }
                    let right =
                        eval_finite_mapper_expr(right.as_ref(), arg_name, local_values, env)?;
                    let FiniteEvalValue::Bool(right) = right else {
                        return None;
                    };
                    Some(FiniteEvalValue::Bool(right))
                }
                _ => {
                    let right =
                        eval_finite_mapper_expr(right.as_ref(), arg_name, local_values, env)?;
                    eval_finite_mapper_bin_op(*name, left, right)
                }
            }
        }
        TypedExpr::UnOp { op, value, .. } => {
            let value = eval_finite_mapper_expr(value.as_ref(), arg_name, local_values, env)?;
            match (op, value) {
                (UnOp::Not, FiniteEvalValue::Bool(value)) => Some(FiniteEvalValue::Bool(!value)),
                (UnOp::Negate, FiniteEvalValue::Int(value)) => Some(FiniteEvalValue::Int(-value)),
                _ => None,
            }
        }
        _ => None,
    }
}

fn eval_finite_mapper_sequence(
    expressions: &[TypedExpr],
    arg_name: &str,
    local_values: &BTreeMap<String, TypedExpr>,
    env: &mut BTreeMap<String, FiniteEvalValue>,
) -> Option<FiniteEvalValue> {
    let (last, prefix) = expressions.split_last()?;
    let mut scoped_env = env.clone();
    for expr in prefix {
        let TypedExpr::Assignment {
            value,
            pattern,
            kind,
            ..
        } = expr
        else {
            return None;
        };
        if !kind.is_let() {
            return None;
        }
        let value =
            eval_finite_mapper_expr(value.as_ref(), arg_name, local_values, &mut scoped_env)?;
        match pattern {
            TypedPattern::Var { name, .. } => {
                scoped_env.insert(name.clone(), value);
            }
            TypedPattern::Discard { .. } => {}
            _ => return None,
        }
    }
    eval_finite_mapper_expr(last, arg_name, local_values, &mut scoped_env)
}

fn eval_finite_mapper_bin_op(
    op: BinOp,
    left: FiniteEvalValue,
    right: FiniteEvalValue,
) -> Option<FiniteEvalValue> {
    match (op, left, right) {
        (BinOp::Eq, left, right) => Some(FiniteEvalValue::Bool(left == right)),
        (BinOp::NotEq, left, right) => Some(FiniteEvalValue::Bool(left != right)),
        (BinOp::LtInt, FiniteEvalValue::Int(left), FiniteEvalValue::Int(right)) => {
            Some(FiniteEvalValue::Bool(left < right))
        }
        (BinOp::LtEqInt, FiniteEvalValue::Int(left), FiniteEvalValue::Int(right)) => {
            Some(FiniteEvalValue::Bool(left <= right))
        }
        (BinOp::GtEqInt, FiniteEvalValue::Int(left), FiniteEvalValue::Int(right)) => {
            Some(FiniteEvalValue::Bool(left >= right))
        }
        (BinOp::GtInt, FiniteEvalValue::Int(left), FiniteEvalValue::Int(right)) => {
            Some(FiniteEvalValue::Bool(left > right))
        }
        (BinOp::AddInt, FiniteEvalValue::Int(left), FiniteEvalValue::Int(right)) => {
            Some(FiniteEvalValue::Int(left + right))
        }
        (BinOp::SubInt, FiniteEvalValue::Int(left), FiniteEvalValue::Int(right)) => {
            Some(FiniteEvalValue::Int(left - right))
        }
        _ => None,
    }
}

fn finite_eval_value_matches_pattern(
    value: &FiniteEvalValue,
    pattern: &TypedPattern,
) -> Option<bool> {
    match pattern {
        TypedPattern::Discard { .. } => Some(true),
        TypedPattern::Int {
            value: pattern,
            base,
            ..
        } => {
            let FiniteEvalValue::Int(value) = value else {
                return Some(false);
            };
            let pattern = parse_uint_bigint(pattern, base)?;
            Some(value == &pattern)
        }
        TypedPattern::ByteArray { value: pattern, .. } => {
            let FiniteEvalValue::ByteArray(value) = value else {
                return Some(false);
            };
            Some(value == pattern)
        }
        TypedPattern::Assign { pattern, .. } => {
            finite_eval_value_matches_pattern(value, pattern.as_ref())
        }
        _ => None,
    }
}

fn parse_uint_bigint(value: &str, base: &Base) -> Option<BigInt> {
    let digits = value.replace('_', "");
    let radix = match base {
        Base::Decimal { .. } => 10,
        Base::Hexadecimal => 16,
    };

    BigInt::parse_bytes(digits.as_bytes(), radix)
}

fn expression_is_bind_continuation(expr: &TypedExpr) -> bool {
    !expression_has_fuzzer_type(expr)
        && function_return_type(expr).is_some_and(|(args, ret)| {
            args.len() == 1 && extract_fuzzer_payload_type(ret.as_ref()).is_some()
        })
}

fn function_return_type(expr: &TypedExpr) -> Option<(Vec<Rc<Type>>, Rc<Type>)> {
    function_signature(expr.tipo().as_ref())
}

fn function_signature(tipo: &Type) -> Option<(Vec<Rc<Type>>, Rc<Type>)> {
    match tipo {
        Type::Fn { args, ret, .. } => Some((args.clone(), ret.clone())),
        Type::Var { tipo, .. } => match tipo.as_ref().borrow().deref() {
            TypeVar::Link { tipo } => function_signature(tipo.as_ref()),
            _ => None,
        },
        _ => None,
    }
}

fn nullary_constructor_tags_for_type(
    tipo: &Type,
    data_types: &IndexMap<&DataTypeKey, &TypedDataType>,
) -> Option<Vec<u64>> {
    let data_type = lookup_data_type_by_tipo(data_types, tipo)?;
    if data_type.constructors.is_empty()
        || !data_type
            .constructors
            .iter()
            .all(|constructor| constructor.arguments.is_empty())
    {
        return None;
    }

    Some(
        data_type
            .constructors
            .iter()
            .enumerate()
            .map(|(index, _)| index as u64)
            .collect(),
    )
}

fn pushforward_nullary_constructor_tags(
    source_tags: &[u64],
    source_output_type: &Type,
    output_type: &Type,
    constructor_map: &BTreeMap<String, String>,
    data_types: &IndexMap<&DataTypeKey, &TypedDataType>,
) -> Option<Vec<u64>> {
    let source_data_type = lookup_data_type_by_tipo(data_types, source_output_type)?;
    let output_data_type = lookup_data_type_by_tipo(data_types, output_type)?;

    let output_tags_by_name: BTreeMap<String, u64> = output_data_type
        .constructors
        .iter()
        .enumerate()
        .filter_map(|(tag, constructor)| {
            constructor
                .arguments
                .is_empty()
                .then_some((constructor.name.clone(), tag as u64))
        })
        .collect();

    let mut output_tags = BTreeSet::new();
    for source_tag in source_tags {
        let source_constructor = source_data_type.constructors.get(*source_tag as usize)?;
        if !source_constructor.arguments.is_empty() {
            return None;
        }

        let mapped_constructor_name = constructor_map.get(source_constructor.name.as_str())?;
        let mapped_tag = output_tags_by_name.get(mapped_constructor_name)?;
        output_tags.insert(*mapped_tag);
    }

    Some(output_tags.into_iter().collect())
}

fn parse_decimal_bigint(value: &str) -> Option<BigInt> {
    value.parse::<BigInt>().ok()
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum FiniteDomainError {
    Empty,
    Heterogeneous,
    OutputTypeMismatch,
    TooLarge,
}

#[derive(Debug, Clone, PartialEq)]
enum CanonicalFiniteScalarDomain {
    Exact(FuzzerExactValue),
    OneOf(Vec<FuzzerExactValue>),
}

fn exact_value_matches_output_type(output_type: &Type, value: &FuzzerExactValue) -> bool {
    match value {
        FuzzerExactValue::Bool(_) => output_type.is_bool(),
        FuzzerExactValue::ByteArray(_) => output_type.is_bytearray(),
        FuzzerExactValue::String(_) => output_type.is_string(),
    }
}

fn exact_value_kind(value: &FuzzerExactValue) -> u8 {
    match value {
        FuzzerExactValue::Bool(_) => 0,
        FuzzerExactValue::ByteArray(_) => 1,
        FuzzerExactValue::String(_) => 2,
    }
}

fn compare_exact_values(a: &FuzzerExactValue, b: &FuzzerExactValue) -> Ordering {
    match (a, b) {
        (FuzzerExactValue::Bool(a), FuzzerExactValue::Bool(b)) => a.cmp(b),
        (FuzzerExactValue::ByteArray(a), FuzzerExactValue::ByteArray(b)) => a.cmp(b),
        (FuzzerExactValue::String(a), FuzzerExactValue::String(b)) => {
            a.as_bytes().cmp(b.as_bytes())
        }
        _ => exact_value_kind(a).cmp(&exact_value_kind(b)),
    }
}

fn canonicalize_finite_scalar_domain(
    output_type: &Type,
    mut values: Vec<FuzzerExactValue>,
) -> Result<CanonicalFiniteScalarDomain, FiniteDomainError> {
    if values.is_empty() {
        return Err(FiniteDomainError::Empty);
    }

    let kind = exact_value_kind(&values[0]);
    if values.iter().any(|value| exact_value_kind(value) != kind) {
        return Err(FiniteDomainError::Heterogeneous);
    }
    if values
        .iter()
        .any(|value| !exact_value_matches_output_type(output_type, value))
    {
        return Err(FiniteDomainError::OutputTypeMismatch);
    }

    values.sort_by(compare_exact_values);
    values.dedup();

    if values.len() > MAX_FINITE_DOMAIN_CASES {
        return Err(FiniteDomainError::TooLarge);
    }

    if values.len() == 1 {
        Ok(CanonicalFiniteScalarDomain::Exact(
            values.into_iter().next().expect("len checked"),
        ))
    } else {
        Ok(CanonicalFiniteScalarDomain::OneOf(values))
    }
}

fn apply_unary_map_constraint_precision(
    mapper_shape: &UnaryMapperShape,
    source_constraint: FuzzerConstraint,
    source_output_type: &Type,
    output_type: &Type,
    data_types: &IndexMap<&DataTypeKey, &TypedDataType>,
) -> FuzzerConstraint {
    match mapper_shape {
        UnaryMapperShape::Identity => source_constraint,
        UnaryMapperShape::ConstBool(value) => {
            FuzzerConstraint::Exact(FuzzerExactValue::Bool(*value))
        }
        UnaryMapperShape::ConstByteArray(bytes) => {
            FuzzerConstraint::Exact(FuzzerExactValue::ByteArray(bytes.clone()))
        }
        UnaryMapperShape::ConstString(value) => {
            FuzzerConstraint::Exact(FuzzerExactValue::String(value.clone()))
        }
        UnaryMapperShape::FiniteScalar(values) => {
            match canonicalize_finite_scalar_domain(output_type, values.clone()) {
                Ok(CanonicalFiniteScalarDomain::Exact(value)) => FuzzerConstraint::Exact(value),
                Ok(CanonicalFiniteScalarDomain::OneOf(values)) => FuzzerConstraint::OneOf(values),
                Err(_) => FuzzerConstraint::Map(Box::new(source_constraint)),
            }
        }
        UnaryMapperShape::ConstInt(value) => FuzzerConstraint::IntRange {
            min: value.clone(),
            max: value.clone(),
        },
        UnaryMapperShape::IntAffine { scale, offset } => {
            if let Some(transformed) =
                apply_int_affine_constraint(&source_constraint, *scale, offset)
            {
                transformed
            } else {
                FuzzerConstraint::Map(Box::new(source_constraint))
            }
        }
        UnaryMapperShape::ConstructorMap(constructor_map) => {
            if let FuzzerConstraint::DataConstructorTags { tags } = &source_constraint {
                if let Some(tags) = pushforward_nullary_constructor_tags(
                    tags,
                    source_output_type,
                    output_type,
                    constructor_map,
                    data_types,
                ) {
                    return FuzzerConstraint::DataConstructorTags { tags };
                }
            }

            FuzzerConstraint::Map(Box::new(source_constraint))
        }
        UnaryMapperShape::Unknown => FuzzerConstraint::Map(Box::new(source_constraint)),
    }
}

fn apply_int_affine_constraint(
    source_constraint: &FuzzerConstraint,
    scale: i8,
    offset: &str,
) -> Option<FuzzerConstraint> {
    let FuzzerConstraint::IntRange { min, max } = source_constraint else {
        return None;
    };

    let offset_value = parse_decimal_bigint(offset)?;
    let min_value = parse_decimal_bigint(min)?;
    let max_value = parse_decimal_bigint(max)?;
    let scale_value = BigInt::from(scale);

    let transformed_min = &scale_value * min_value + &offset_value;
    let transformed_max = &scale_value * max_value + &offset_value;
    let (min, max) = if scale < 0 {
        (transformed_max, transformed_min)
    } else {
        (transformed_min, transformed_max)
    };

    Some(FuzzerConstraint::IntRange {
        min: min.to_string(),
        max: max.to_string(),
    })
}

fn apply_unary_map_semantics_precision(
    mapper_shape: &UnaryMapperShape,
    source_semantics: FuzzerSemantics,
    source_output_type: &Type,
    output_type: &Type,
    data_types: &IndexMap<&DataTypeKey, &TypedDataType>,
) -> FuzzerSemantics {
    match mapper_shape {
        UnaryMapperShape::Identity => source_semantics,
        UnaryMapperShape::ConstBool(value) => {
            FuzzerSemantics::Exact(FuzzerExactValue::Bool(*value))
        }
        UnaryMapperShape::ConstByteArray(bytes) => {
            FuzzerSemantics::Exact(FuzzerExactValue::ByteArray(bytes.clone()))
        }
        UnaryMapperShape::ConstString(value) => {
            FuzzerSemantics::Exact(FuzzerExactValue::String(value.clone()))
        }
        UnaryMapperShape::FiniteScalar(values) => {
            match canonicalize_finite_scalar_domain(output_type, values.clone()) {
                Ok(CanonicalFiniteScalarDomain::Exact(value)) => FuzzerSemantics::Exact(value),
                Ok(CanonicalFiniteScalarDomain::OneOf(values)) => FuzzerSemantics::OneOf(values),
                Err(_) => default_semantics_for_type(output_type, data_types),
            }
        }
        UnaryMapperShape::ConstInt(value) => FuzzerSemantics::IntRange {
            min: Some(value.clone()),
            max: Some(value.clone()),
        },
        UnaryMapperShape::IntAffine { scale, offset } => {
            if let Some(transformed) = apply_int_affine_semantics(&source_semantics, *scale, offset)
            {
                transformed
            } else {
                // Fall back to unconstrained output: mapping an unconstrained
                // integer through an affine shape yields an unconstrained
                // integer, which is a sound over-approximation.
                default_semantics_for_type(output_type, data_types)
            }
        }
        UnaryMapperShape::ConstructorMap(constructor_map) => {
            if let FuzzerSemantics::Constructors { tags } = &source_semantics {
                if let Some(tags) = pushforward_nullary_constructor_tags(
                    tags,
                    source_output_type,
                    output_type,
                    constructor_map,
                    data_types,
                ) {
                    return FuzzerSemantics::Constructors { tags };
                }
            }

            // Sound over-approximation: unknown constructor map from an
            // unconstrained source yields the default (unconstrained)
            // semantics for the output type.
            default_semantics_for_type(output_type, data_types)
        }
        // A mapper whose shape we do not understand still produces values
        // of the output type. Over-approximate to the unconstrained domain
        // for that type rather than failing to emit a theorem at all.
        UnaryMapperShape::Unknown => default_semantics_for_type(output_type, data_types),
    }
}

fn apply_int_affine_semantics(
    source_semantics: &FuzzerSemantics,
    scale: i8,
    offset: &str,
) -> Option<FuzzerSemantics> {
    let FuzzerSemantics::IntRange { min, max } = source_semantics else {
        return None;
    };
    let offset_value = parse_decimal_bigint(offset)?;
    let transformed_min = apply_int_affine_bound(min, scale, &offset_value)?;
    let transformed_max = apply_int_affine_bound(max, scale, &offset_value)?;
    let (min, max) = if scale < 0 {
        (transformed_max, transformed_min)
    } else {
        (transformed_min, transformed_max)
    };

    Some(FuzzerSemantics::IntRange { min, max })
}

fn apply_int_affine_bound(
    bound: &Option<String>,
    scale: i8,
    offset: &BigInt,
) -> Option<Option<String>> {
    let Some(bound) = bound.as_ref() else {
        return Some(None);
    };
    let bound_value = parse_decimal_bigint(bound)?;
    let transformed = BigInt::from(scale) * bound_value + offset;

    Some(Some(transformed.to_string()))
}

#[allow(clippy::only_used_in_recursion)]
fn normalized_fuzzer_constraint(
    normalized: &NormalizedFuzzer,
    current_module: &str,
    function_index: &FunctionIndex<'_>,
    constant_index: &ConstantIndex<'_>,
    data_types: &IndexMap<&DataTypeKey, &TypedDataType>,
    local_values: &BTreeMap<String, TypedExpr>,
    visiting_functions: &mut BTreeSet<(String, String)>,
) -> FuzzerConstraint {
    match normalized {
        NormalizedFuzzer::Opaque { reason, .. } => FuzzerConstraint::Unsupported {
            reason: reason.clone(),
        },
        NormalizedFuzzer::Primitive {
            output_type,
            known_constraint,
        } => {
            if let Some(constraint) = known_constraint {
                return constraint.clone();
            }
            if let Some(tags) = nullary_constructor_tags_for_type(output_type.as_ref(), data_types)
            {
                FuzzerConstraint::DataConstructorTags { tags }
            } else {
                FuzzerConstraint::Any
            }
        }
        NormalizedFuzzer::Map {
            source,
            source_output_type,
            output_type,
            mapper_shape,
        } => {
            let source_constraint = normalized_fuzzer_constraint(
                source,
                current_module,
                function_index,
                constant_index,
                data_types,
                local_values,
                visiting_functions,
            );

            apply_unary_map_constraint_precision(
                mapper_shape,
                source_constraint,
                source_output_type.as_ref(),
                output_type.as_ref(),
                data_types,
            )
        }
        NormalizedFuzzer::Bind { source, result } => {
            let result_constraint = normalized_fuzzer_constraint(
                result,
                current_module,
                function_index,
                constant_index,
                data_types,
                local_values,
                visiting_functions,
            );
            let source_constraint = normalized_fuzzer_constraint(
                source,
                current_module,
                function_index,
                constant_index,
                data_types,
                local_values,
                visiting_functions,
            );
            intersect_constraints(source_constraint, result_constraint)
        }
        NormalizedFuzzer::Product { elements } => FuzzerConstraint::Tuple(
            elements
                .iter()
                .map(|element| {
                    normalized_fuzzer_constraint(
                        element,
                        current_module,
                        function_index,
                        constant_index,
                        data_types,
                        local_values,
                        visiting_functions,
                    )
                })
                .collect(),
        ),
        NormalizedFuzzer::List {
            element,
            min_len,
            max_len,
        } => FuzzerConstraint::List {
            elem: Box::new(normalized_fuzzer_constraint(
                element,
                current_module,
                function_index,
                constant_index,
                data_types,
                local_values,
                visiting_functions,
            )),
            min_len: *min_len,
            max_len: *max_len,
        },
        NormalizedFuzzer::StateMachineTrace {
            output_type,
            initial_state,
            step_function,
            ..
        } => match state_machine_trace_semantics_from_normalized(
            output_type.as_ref(),
            initial_state,
            step_function,
            data_types,
            function_index,
            visiting_functions,
        ) {
            Some(FuzzerSemantics::StateMachineTrace { acceptance, .. }) => {
                state_machine_trace_constraint_for_acceptance(acceptance)
            }
            Some(FuzzerSemantics::Opaque { reason }) => FuzzerConstraint::Unsupported { reason },
            Some(_) => FuzzerConstraint::Unsupported {
                reason: "state-machine trace analysis produced an unexpected semantic form"
                    .to_string(),
            },
            None => FuzzerConstraint::Unsupported {
                reason: format!(
                    "state-machine trace normalization does not match output type '{}'",
                    pretty_print_type(output_type.as_ref())
                ),
            },
        },
    }
}

#[allow(clippy::too_many_arguments, clippy::only_used_in_recursion)]
fn normalized_fuzzer_semantics(
    normalized: &NormalizedFuzzer,
    current_module: &str,
    function_index: &FunctionIndex<'_>,
    constant_index: &ConstantIndex<'_>,
    data_types: &IndexMap<&DataTypeKey, &TypedDataType>,
    output_type: &Type,
    local_values: &BTreeMap<String, TypedExpr>,
    visiting_functions: &mut BTreeSet<(String, String)>,
) -> FuzzerSemantics {
    match normalized {
        NormalizedFuzzer::Opaque { reason, .. } => opaque_semantics(reason.clone()),
        NormalizedFuzzer::Primitive {
            known_constraint, ..
        } => {
            if let Some(constraint) = known_constraint {
                if let Some(sem) = semantics_from_known_constraint(constraint, output_type) {
                    return sem;
                }
            }
            if let Some(tags) = nullary_constructor_tags_for_type(output_type, data_types) {
                FuzzerSemantics::Constructors { tags }
            } else {
                default_semantics_for_type(output_type, data_types)
            }
        }
        NormalizedFuzzer::Map {
            source,
            source_output_type,
            output_type: map_output_type,
            mapper_shape,
        } => {
            let source_semantics = normalized_fuzzer_semantics(
                source,
                current_module,
                function_index,
                constant_index,
                data_types,
                source_output_type.as_ref(),
                local_values,
                visiting_functions,
            );

            apply_unary_map_semantics_precision(
                mapper_shape,
                source_semantics,
                source_output_type.as_ref(),
                map_output_type.as_ref(),
                data_types,
            )
        }
        NormalizedFuzzer::Bind { source, result } => {
            let result_semantics = normalized_fuzzer_semantics(
                result,
                current_module,
                function_index,
                constant_index,
                data_types,
                output_type,
                local_values,
                visiting_functions,
            );
            let source_semantics = normalized_fuzzer_semantics(
                source,
                current_module,
                function_index,
                constant_index,
                data_types,
                output_type,
                local_values,
                visiting_functions,
            );
            intersect_semantics(source_semantics, result_semantics)
        }
        NormalizedFuzzer::Product { elements } => {
            let inner_types = output_type.get_inner_types();
            if !(output_type.is_tuple() || output_type.is_pair()) {
                return opaque_semantics(format!(
                    "product normalization does not match output type '{}'",
                    pretty_print_type(output_type)
                ));
            }
            if inner_types.len() != elements.len() {
                return opaque_semantics(format!(
                    "product normalization arity {} does not match output type '{}'",
                    elements.len(),
                    pretty_print_type(output_type)
                ));
            }

            FuzzerSemantics::Product(
                elements
                    .iter()
                    .zip(inner_types.iter())
                    .map(|(element, inner_type)| {
                        normalized_fuzzer_semantics(
                            element,
                            current_module,
                            function_index,
                            constant_index,
                            data_types,
                            inner_type.as_ref(),
                            local_values,
                            visiting_functions,
                        )
                    })
                    .collect(),
            )
        }
        NormalizedFuzzer::List {
            element,
            min_len,
            max_len,
        } => {
            let inner_types = output_type.get_inner_types();
            if !(output_type.is_list() && inner_types.len() == 1) {
                return opaque_semantics(format!(
                    "list normalization does not match output type '{}'",
                    pretty_print_type(output_type)
                ));
            }

            FuzzerSemantics::List {
                element: Box::new(normalized_fuzzer_semantics(
                    element,
                    current_module,
                    function_index,
                    constant_index,
                    data_types,
                    inner_types[0].as_ref(),
                    local_values,
                    visiting_functions,
                )),
                min_len: *min_len,
                max_len: *max_len,
            }
        }
        NormalizedFuzzer::StateMachineTrace {
            output_type,
            initial_state,
            step_function,
            ..
        } => state_machine_trace_semantics_from_normalized(
            output_type.as_ref(),
            initial_state,
            step_function,
            data_types,
            function_index,
            visiting_functions,
        )
        .unwrap_or_else(|| {
            opaque_semantics(format!(
                "state-machine trace normalization does not match output type '{}'",
                pretty_print_type(output_type.as_ref())
            ))
        }),
    }
}

#[cfg(test)]
fn extract_constraint_from_via(
    via: &TypedExpr,
    current_module: &str,
    known_functions: &IndexMap<&FunctionAccessKey, &TypedFunction>,
) -> FuzzerConstraint {
    extract_constraint_from_via_with_constants_and_data_types(
        via,
        current_module,
        known_functions,
        &IndexMap::new(),
        &IndexMap::new(),
    )
}

#[cfg(test)]
fn extract_constraint_from_via_with_data_types(
    via: &TypedExpr,
    current_module: &str,
    known_functions: &IndexMap<&FunctionAccessKey, &TypedFunction>,
    data_types: &IndexMap<&DataTypeKey, &TypedDataType>,
) -> FuzzerConstraint {
    extract_constraint_from_via_with_constants_and_data_types(
        via,
        current_module,
        known_functions,
        &IndexMap::new(),
        data_types,
    )
}

#[cfg(test)]
fn extract_constraint_from_via_with_constants(
    via: &TypedExpr,
    current_module: &str,
    known_functions: &IndexMap<&FunctionAccessKey, &TypedFunction>,
    known_constants: &IndexMap<&FunctionAccessKey, &TypedExpr>,
) -> FuzzerConstraint {
    extract_constraint_from_via_with_constants_and_data_types(
        via,
        current_module,
        known_functions,
        known_constants,
        &IndexMap::new(),
    )
}

fn extract_constraint_from_via_with_constants_and_data_types(
    via: &TypedExpr,
    current_module: &str,
    known_functions: &IndexMap<&FunctionAccessKey, &TypedFunction>,
    known_constants: &IndexMap<&FunctionAccessKey, &TypedExpr>,
    data_types: &IndexMap<&DataTypeKey, &TypedDataType>,
) -> FuzzerConstraint {
    let normalized = normalize_fuzzer_from_via_with_constants(
        via,
        current_module,
        known_functions,
        known_constants,
    );
    let function_index = index_known_functions(known_functions);
    let constant_index = index_known_constants(known_constants);
    let mut visiting_functions = BTreeSet::new();

    normalized_fuzzer_constraint(
        &normalized,
        current_module,
        &function_index,
        &constant_index,
        data_types,
        &BTreeMap::new(),
        &mut visiting_functions,
    )
}

/// Intersect two constraints from a Bind's source and result.
/// When both are IntRange, compute the intersection. Otherwise, prefer
/// the result constraint (conservative — never widens the domain).
fn intersect_constraints(source: FuzzerConstraint, result: FuzzerConstraint) -> FuzzerConstraint {
    match (&source, &result) {
        (
            FuzzerConstraint::IntRange {
                min: s_min,
                max: s_max,
            },
            FuzzerConstraint::IntRange {
                min: r_min,
                max: r_max,
            },
        ) => {
            let min = intersect_int_bound_max(Some(s_min), Some(r_min));
            let max = intersect_int_bound_min(Some(s_max), Some(r_max));
            if let (Some(lo), Some(hi)) = (&min, &max) {
                if lo
                    .parse::<i128>()
                    .ok()
                    .zip(hi.parse::<i128>().ok())
                    .is_some_and(|(l, h)| l > h)
                {
                    return result;
                }
            }
            FuzzerConstraint::IntRange {
                min: min.unwrap_or_else(|| r_min.clone()),
                max: max.unwrap_or_else(|| r_max.clone()),
            }
        }
        (FuzzerConstraint::Any, _) | (FuzzerConstraint::Unsupported { .. }, _) => result,
        (_, FuzzerConstraint::Any) | (_, FuzzerConstraint::Unsupported { .. }) => source,
        _ => result,
    }
}

/// Intersect two semantics from a Bind's source and result.
/// When both are IntRange, compute the intersection. Otherwise, prefer
/// the result semantics (conservative — never widens the domain).
fn intersect_semantics(source: FuzzerSemantics, result: FuzzerSemantics) -> FuzzerSemantics {
    match (&source, &result) {
        (
            FuzzerSemantics::IntRange {
                min: s_min,
                max: s_max,
            },
            FuzzerSemantics::IntRange {
                min: r_min,
                max: r_max,
            },
        ) => {
            let min = intersect_optional_int_bound_max(s_min.as_deref(), r_min.as_deref());
            let max = intersect_optional_int_bound_min(s_max.as_deref(), r_max.as_deref());
            if let (Some(lo), Some(hi)) = (&min, &max) {
                if lo
                    .parse::<i128>()
                    .ok()
                    .zip(hi.parse::<i128>().ok())
                    .is_some_and(|(l, h)| l > h)
                {
                    return result;
                }
            }
            FuzzerSemantics::IntRange { min, max }
        }
        (FuzzerSemantics::Opaque { .. }, _) => result,
        (_, FuzzerSemantics::Opaque { .. }) => source,
        _ => result,
    }
}

/// Pick the larger of two int bound strings (for lower bound intersection).
fn intersect_int_bound_max(a: Option<&str>, b: Option<&str>) -> Option<String> {
    match (a, b) {
        (Some(a), Some(b)) => {
            let va = a.parse::<i128>().ok()?;
            let vb = b.parse::<i128>().ok()?;
            Some(va.max(vb).to_string())
        }
        (Some(a), None) => Some(a.to_string()),
        (None, Some(b)) => Some(b.to_string()),
        (None, None) => None,
    }
}

/// Pick the smaller of two int bound strings (for upper bound intersection).
fn intersect_int_bound_min(a: Option<&str>, b: Option<&str>) -> Option<String> {
    match (a, b) {
        (Some(a), Some(b)) => {
            let va = a.parse::<i128>().ok()?;
            let vb = b.parse::<i128>().ok()?;
            Some(va.min(vb).to_string())
        }
        (Some(a), None) => Some(a.to_string()),
        (None, Some(b)) => Some(b.to_string()),
        (None, None) => None,
    }
}

/// Pick the larger of two optional int bound strings (for lower bound intersection).
fn intersect_optional_int_bound_max(a: Option<&str>, b: Option<&str>) -> Option<String> {
    match (a, b) {
        (Some(a), Some(b)) => {
            let va = a.parse::<i128>().ok()?;
            let vb = b.parse::<i128>().ok()?;
            Some(va.max(vb).to_string())
        }
        (Some(a), None) => Some(a.to_string()),
        (None, Some(b)) => Some(b.to_string()),
        (None, None) => None,
    }
}

/// Pick the smaller of two optional int bound strings (for upper bound intersection).
fn intersect_optional_int_bound_min(a: Option<&str>, b: Option<&str>) -> Option<String> {
    match (a, b) {
        (Some(a), Some(b)) => {
            let va = a.parse::<i128>().ok()?;
            let vb = b.parse::<i128>().ok()?;
            Some(va.min(vb).to_string())
        }
        (Some(a), None) => Some(a.to_string()),
        (None, Some(b)) => Some(b.to_string()),
        (None, None) => None,
    }
}

fn opaque_semantics(reason: impl Into<String>) -> FuzzerSemantics {
    FuzzerSemantics::Opaque {
        reason: reason.into(),
    }
}

fn default_semantics_for_type(
    tipo: &Type,
    data_types: &IndexMap<&DataTypeKey, &TypedDataType>,
) -> FuzzerSemantics {
    if tipo.is_bool() {
        FuzzerSemantics::Bool
    } else if tipo.is_int() {
        FuzzerSemantics::IntRange {
            min: None,
            max: None,
        }
    } else if tipo.is_bytearray() {
        FuzzerSemantics::ByteArrayRange {
            min_len: None,
            max_len: None,
        }
    } else if tipo.is_string() {
        FuzzerSemantics::String
    } else if tipo.is_data() {
        FuzzerSemantics::Data
    } else if tipo.is_list() {
        match tipo.get_inner_types().as_slice() {
            [element_type] => FuzzerSemantics::List {
                element: Box::new(default_semantics_for_type(
                    element_type.as_ref(),
                    data_types,
                )),
                min_len: None,
                max_len: None,
            },
            _ => opaque_semantics("list type is missing its element type"),
        }
    } else if tipo.is_tuple() || tipo.is_pair() {
        FuzzerSemantics::Product(
            tipo.get_inner_types()
                .iter()
                .map(|inner| default_semantics_for_type(inner.as_ref(), data_types))
                .collect(),
        )
    } else if let Some((module, name)) = tipo.qualifier() {
        if let Some(tags) = nullary_constructor_tags_for_type(tipo, data_types) {
            return FuzzerSemantics::Constructors { tags };
        }
        // Non-nullary qualified ADT: lower as `Data` with a structural schema
        // predicate. The schema itself comes from the test's
        // `fuzzer_data_schema` at proof-generation time; here we only record
        // the type name for Lean predicate naming.
        FuzzerSemantics::DataWithSchema {
            type_name: data_with_schema_type_name(tipo).unwrap_or_else(|| {
                if module.is_empty() {
                    name.to_string()
                } else {
                    format!("{module}.{name}")
                }
            }),
        }
    } else {
        opaque_semantics("semantic export for this type is not implemented yet")
    }
}

fn pretty_print_type(tipo: &Type) -> String {
    let mut printer = Printer::new();
    printer.print(tipo).to_pretty_string(80)
}

fn semantic_type_name(tipo: &Type) -> String {
    if let Some((module, name)) = tipo.qualifier() {
        if module.is_empty() {
            name.to_string()
        } else {
            format!("{module}.{name}")
        }
    } else {
        pretty_print_type(tipo)
    }
}

fn semantic_type_from_type(tipo: &Type) -> SemanticType {
    if tipo.is_int() {
        return SemanticType::Int;
    }
    if tipo.is_bool() {
        return SemanticType::Bool;
    }
    if tipo.is_bytearray() {
        return SemanticType::ByteArray;
    }
    if tipo.is_string() {
        return SemanticType::String;
    }
    if tipo.is_data() {
        return SemanticType::Data;
    }

    match tipo {
        Type::App {
            name, args, module, ..
        } if name == "List" && module.is_empty() => {
            let inner = args
                .first()
                .map(|a| semantic_type_from_type(a))
                .unwrap_or(SemanticType::Unsupported("List<?>".into()));
            SemanticType::List(Box::new(inner))
        }
        Type::Tuple { elems, .. } => {
            SemanticType::Tuple(elems.iter().map(|e| semantic_type_from_type(e)).collect())
        }
        Type::Pair { fst, snd, .. } => SemanticType::Pair(
            Box::new(semantic_type_from_type(fst)),
            Box::new(semantic_type_from_type(snd)),
        ),
        Type::Var { tipo, .. } => {
            let borrowed = tipo.as_ref().borrow();
            match borrowed.deref() {
                TypeVar::Link { tipo: linked } => semantic_type_from_type(linked.as_ref()),
                _ => SemanticType::Unsupported("type variable".to_string()),
            }
        }
        _ => SemanticType::Unsupported(semantic_type_name(tipo)),
    }
}

fn state_machine_trace_output_semantics(
    acceptance: StateMachineAcceptance,
    label_type: &Type,
    event_type: &Type,
    data_types: &IndexMap<&DataTypeKey, &TypedDataType>,
) -> FuzzerSemantics {
    match acceptance {
        StateMachineAcceptance::AcceptsSuccess => FuzzerSemantics::List {
            element: Box::new(default_semantics_for_type(event_type, data_types)),
            min_len: Some(0),
            max_len: None,
        },
        StateMachineAcceptance::AcceptsFailure => {
            let mut labels = default_semantics_for_type(label_type, data_types);
            if let FuzzerSemantics::List { min_len, .. } = &mut labels {
                *min_len = Some(1);
            }

            FuzzerSemantics::Product(vec![
                labels,
                FuzzerSemantics::List {
                    element: Box::new(default_semantics_for_type(event_type, data_types)),
                    min_len: Some(1),
                    max_len: Some(1),
                },
            ])
        }
    }
}

fn is_prng_type(tipo: &Type) -> bool {
    match tipo {
        Type::App { module, name, .. } => name == "PRNG" && module.is_empty(),
        Type::Var { tipo, .. } => match tipo.as_ref().borrow().deref() {
            TypeVar::Link { tipo } => is_prng_type(tipo.as_ref()),
            _ => false,
        },
        _ => false,
    }
}

fn extract_fuzzer_payload_type(tipo: &Type) -> Option<Rc<Type>> {
    match tipo {
        Type::Fn { args, ret, .. } if args.len() == 1 && is_prng_type(args[0].as_ref()) => {
            match ret.as_ref() {
                Type::App {
                    module, name, args, ..
                } if name == "Option" && module.is_empty() => {
                    let inner = args.first()?;
                    match inner.as_ref() {
                        Type::Tuple { elems, .. }
                            if elems.len() == 2 && is_prng_type(elems[0].as_ref()) =>
                        {
                            Some(elems[1].clone())
                        }
                        _ => None,
                    }
                }
                _ => None,
            }
        }
        Type::Var { tipo, .. } => match tipo.as_ref().borrow().deref() {
            TypeVar::Link { tipo } => extract_fuzzer_payload_type(tipo.as_ref()),
            _ => None,
        },
        // Minimal, sound handling for the direct `Type::App` representation of
        // `Fuzzer<T>`. In today's codebase, `Fuzzer` is always materialized as a
        // `Type::Fn` (see `Type::fuzzer` in `ast/well_known.rs:183`) even through
        // transparent type aliases, so this branch is currently unreachable and
        // is purely defensive. We only accept a `Type::App` when:
        //   * its `name` is exactly the prelude `Fuzzer` constructor,
        //   * its `module` is empty (prelude-level, matching how `PRNG` and
        //     `Option` are checked elsewhere in this file), and
        //   * it carries exactly one type argument (the payload `T`).
        // This mirrors the safety posture of `is_prng_type` and `is_option`
        // rather than attempting to walk arbitrary user-defined aliases, which
        // would require resolving alias bodies and is out of scope.
        //
        // If this arm ever starts firing unexpectedly (e.g. after a change to
        // alias resolution), the downstream opaque-type gate in verify.rs
        // (`fuzzer_semantics_contains_opaque` at ~line 3021) will catch any
        // opaque output domain and return `FallbackRequired` before any proof
        // is attempted.
        Type::App {
            module, name, args, ..
        } if name == "Fuzzer" && module.is_empty() && args.len() == 1 => Some(args[0].clone()),
        _ => None,
    }
}

type StateMachineTraceFields = (u64, u64, Rc<Type>, Rc<Type>, SemanticType, SemanticType);

fn extract_state_machine_trace_fields(
    transition_type: &Rc<Type>,
    state_type: &Rc<Type>,
    data_types: &IndexMap<&DataTypeKey, &TypedDataType>,
) -> Result<StateMachineTraceFields, String> {
    let data_type =
        lookup_data_type_by_tipo(data_types, transition_type.as_ref()).ok_or_else(|| {
            format!(
                "state-machine step output '{}' is not a known transition data type",
                pretty_print_type(transition_type.as_ref())
            )
        })?;

    let zero_arity_ctors: Vec<_> = data_type
        .constructors
        .iter()
        .enumerate()
        .filter(|(_, ctor)| ctor.arguments.is_empty())
        .collect();
    let step_constructors: Vec<_> = data_type
        .constructors
        .iter()
        .enumerate()
        .filter(|(_, ctor)| ctor.arguments.len() == 3)
        .collect();

    if zero_arity_ctors.len() != 1
        || step_constructors.len() != 1
        || data_type.constructors.len() != 2
    {
        return Err(format!(
            "state-machine transition type '{}' must have one terminal constructor and one 3-field step constructor",
            pretty_print_type(transition_type.as_ref())
        ));
    }

    // Use `get_constr_index_variant` so `@tag(N)` decorators on the
    // constructors are honoured. Falling back to the enumeration index
    // preserves prior behaviour when the decorator lookup is unavailable.
    let terminal_tag = get_constr_index_variant(&data_type, &zero_arity_ctors[0].1.name)
        .map(|(i, _)| i as u64)
        .unwrap_or(zero_arity_ctors[0].0 as u64);
    let step_tag = get_constr_index_variant(&data_type, &step_constructors[0].1.name)
        .map(|(i, _)| i as u64)
        .unwrap_or(step_constructors[0].0 as u64);

    let mono_types: IndexMap<u64, Rc<Type>> = match transition_type.as_ref() {
        Type::App { args, .. } => data_type
            .typed_parameters
            .iter()
            .zip(args.iter())
            .flat_map(|(generic, arg)| get_generic_id_and_type(generic.as_ref(), arg.as_ref()))
            .collect(),
        _ => IndexMap::new(),
    };

    let step_fields: Vec<Rc<Type>> = step_constructors[0]
        .1
        .arguments
        .iter()
        .map(|field| find_and_replace_generics(&field.tipo, &mono_types))
        .collect();

    let next_state_type = convert_opaque_type(&step_fields[1], data_types, true);
    if next_state_type.as_ref() != state_type.as_ref() {
        return Err(format!(
            "state-machine transition state field '{}' does not match initial state type '{}'",
            pretty_print_type(next_state_type.as_ref()),
            pretty_print_type(state_type.as_ref())
        ));
    }

    let label_type = convert_opaque_type(&step_fields[0], data_types, true);
    let event_type = convert_opaque_type(&step_fields[2], data_types, true);
    let label_semantic_type = semantic_type_from_type(label_type.as_ref());
    let event_semantic_type = semantic_type_from_type(event_type.as_ref());

    Ok((
        terminal_tag,
        step_tag,
        label_type,
        event_type,
        label_semantic_type,
        event_semantic_type,
    ))
}

fn infer_state_machine_acceptance_from_output_type(
    output_type: &Type,
) -> Option<StateMachineAcceptance> {
    if output_type.is_list() {
        return Some(StateMachineAcceptance::AcceptsSuccess);
    }

    if output_type.is_tuple() || output_type.is_pair() {
        let inner = output_type.get_inner_types();
        if inner.len() == 2 && inner.iter().all(|tipo| tipo.is_list()) {
            return Some(StateMachineAcceptance::AcceptsFailure);
        }
    }

    None
}

fn state_machine_trace_constraint_for_acceptance(
    acceptance: StateMachineAcceptance,
) -> FuzzerConstraint {
    let unbounded_list = || FuzzerConstraint::List {
        elem: Box::new(FuzzerConstraint::Any),
        min_len: Some(0),
        max_len: None,
    };

    match acceptance {
        StateMachineAcceptance::AcceptsSuccess => unbounded_list(),
        StateMachineAcceptance::AcceptsFailure => {
            FuzzerConstraint::Tuple(vec![unbounded_list(), unbounded_list()])
        }
    }
}

fn state_machine_trace_semantics_from_normalized(
    output_type: &Type,
    initial_state: &TypedExpr,
    step_function: &TypedExpr,
    data_types: &IndexMap<&DataTypeKey, &TypedDataType>,
    function_index: &FunctionIndex<'_>,
    visiting_functions: &mut BTreeSet<(String, String)>,
) -> Option<FuzzerSemantics> {
    let output_type = convert_opaque_type(&Rc::new(output_type.clone()), data_types, true);
    let args = make_synthetic_call_args(vec![initial_state.clone(), step_function.clone()]);

    extract_state_machine_trace_semantics_from_call(
        output_type.as_ref(),
        &args,
        data_types,
        function_index,
        visiting_functions,
    )
}

fn extract_state_machine_trace_semantics_from_call(
    output_type: &Type,
    args: &[CallArg<TypedExpr>],
    data_types: &IndexMap<&DataTypeKey, &TypedDataType>,
    function_index: &FunctionIndex<'_>,
    visiting_functions: &mut BTreeSet<(String, String)>,
) -> Option<FuzzerSemantics> {
    let acceptance = infer_state_machine_acceptance_from_output_type(output_type)?;

    if args.len() != 2 {
        return Some(opaque_semantics(format!(
            "state-machine trace lowering expects 2 arguments, got {}",
            args.len()
        )));
    }

    let state_type = convert_opaque_type(&args[0].value.tipo(), data_types, true);
    let step_type = convert_opaque_type(&args[1].value.tipo(), data_types, true);

    let Type::Fn {
        args: step_args,
        ret,
        ..
    } = step_type.as_ref()
    else {
        return Some(opaque_semantics(format!(
            "state-machine trace step argument is not a function, got '{}'",
            pretty_print_type(step_type.as_ref())
        )));
    };

    if step_args.is_empty() {
        return Some(opaque_semantics(
            "state-machine trace step function must take state as its first argument",
        ));
    }

    let step_state_type = convert_opaque_type(&step_args[0], data_types, true);
    if step_state_type.as_ref() != state_type.as_ref() {
        return Some(opaque_semantics(format!(
            "state-machine trace step state type '{}' does not match initial state type '{}'",
            pretty_print_type(step_state_type.as_ref()),
            pretty_print_type(state_type.as_ref())
        )));
    }

    let Some(transition_type) = extract_fuzzer_payload_type(ret.as_ref()) else {
        return Some(opaque_semantics(format!(
            "state-machine trace step return type '{}' is not Fuzzer<transition>",
            pretty_print_type(ret.as_ref())
        )));
    };
    let transition_type = convert_opaque_type(&transition_type, data_types, true);

    let (terminal_tag, step_tag, label_raw_type, event_raw_type, label_type, event_type) =
        match extract_state_machine_trace_fields(&transition_type, &state_type, data_types) {
            Ok(fields) => fields,
            Err(reason) => {
                return Some(opaque_semantics(format!(
                    "state-machine trace lowering cannot infer transition shape: {reason}"
                )));
            }
        };

    let step_input_raw_types: Vec<Rc<Type>> = step_args
        .iter()
        .skip(1)
        .map(|arg| convert_opaque_type(arg, data_types, true))
        .collect();
    let step_input_types: Vec<SemanticType> = step_input_raw_types
        .iter()
        .map(|tipo| semantic_type_from_type(tipo.as_ref()))
        .collect();
    let state_semantics = Box::new(default_semantics_for_type(state_type.as_ref(), data_types));
    let step_input_semantics = step_input_raw_types
        .iter()
        .map(|tipo| default_semantics_for_type(tipo.as_ref(), data_types))
        .collect();
    let label_semantics = Box::new(default_semantics_for_type(
        label_raw_type.as_ref(),
        data_types,
    ));
    let event_semantics = Box::new(default_semantics_for_type(
        event_raw_type.as_ref(),
        data_types,
    ));
    let transition_semantics = StateMachineTransitionSemantics {
        terminal_tag,
        step_tag,
        label_field_index: 0,
        next_state_field_index: 1,
        event_field_index: 2,
        state_semantics,
        step_input_semantics,
        label_semantics,
        event_semantics,
    };
    let output_semantics = Box::new(state_machine_trace_output_semantics(
        acceptance,
        label_raw_type.as_ref(),
        event_raw_type.as_ref(),
        data_types,
    ));

    // Assumption: the output_semantics derived from transition fields (label_type,
    // event_type) must be consistent with the declared fuzzer output type. We derive
    // acceptance mode from the output type and then independently derive output
    // semantics from transition fields. If the transition ADT's field types don't
    // match what the acceptance mode expects, the generated proof predicates will
    // be unsound. This is currently not validated because the SemanticType and
    // FuzzerOutputType representations are not directly comparable at this stage.
    //
    // TODO(4.1): Add a structural validation step that checks the inferred
    // output_semantics shape (e.g., List for AcceptsSuccess, Product([List, List])
    // for AcceptsFailure) against the declared fuzzer output type. If they disagree,
    // fall back to opaque with a diagnostic explaining the mismatch.
    debug_assert!(
        matches!(
            (&acceptance, output_semantics.as_ref()),
            (
                StateMachineAcceptance::AcceptsSuccess,
                FuzzerSemantics::List { .. }
            ) | (
                StateMachineAcceptance::AcceptsFailure,
                FuzzerSemantics::Product(_)
            )
        ),
        "state-machine output semantics shape does not match acceptance mode: acceptance={}, semantics={}",
        describe_acceptance(&acceptance),
        describe_semantics(&output_semantics)
    );

    // For inline lambda step functions the expression is a `TypedExpr::Fn`
    // and `typed_expr_to_shallow_ir` translates it directly.  For a named
    // function reference (e.g. `scenario.ok(initial_state, step)`) the
    // expression is a `TypedExpr::Var`.  In that case the translator would
    // produce a bare `ShallowIr::Var { name: "step" }`, which the Lean
    // emitter reifies as a fresh existential `∃ _fuzz_0 : Data,
    // transition = _fuzz_0` — trivially True and therefore useless.
    //
    // S1: resolve a `ModuleFn` Var to its body via `function_index` and
    // translate that body to `ShallowIr`.  If the result is trivially
    // vacuous (see `shallow_ir_is_vacuous`), fall back to `None` so the
    // caller takes the concrete `native_decide` witness path — sound and
    // efficient — instead of handing Blaster a premise that simplifies to
    // True.  Monadic step bodies (`if st.done { return(Done) } else {
    // let action <- and_then(...) ... }`) classify as vacuous today
    // because their root reduces to `ShallowIr::If` and the combinators
    // erase to `ctx.fresh(Data)`; future IR enrichment can make
    // those bodies structurally useful.
    //
    // Recursion is guarded by `visiting_functions`: if the step function
    // is already being visited (mutual recursion through an `and_then`
    // continuation, etc.), we bail out with `None`.
    let step_function_ir = match &args[1].value {
        TypedExpr::Var { constructor, .. } => {
            if let ValueConstructorVariant::ModuleFn { module, name, .. } = &constructor.variant {
                let key = (module.clone(), name.clone());
                if !visiting_functions.insert(key.clone()) {
                    // Recursive step function — body lookup would loop.
                    None
                } else {
                    let ir = find_function(function_index, module, name)
                        .map(|function| typed_expr_to_shallow_ir(&function.body, data_types))
                        .filter(|ir| !shallow_ir_is_vacuous(ir));
                    visiting_functions.remove(&key);
                    ir
                }
            } else {
                // Local or other non-resolvable Var — no body to retrieve.
                None
            }
        }
        expr => {
            let ir = typed_expr_to_shallow_ir(expr, data_types);
            if shallow_ir_is_vacuous(&ir) {
                None
            } else {
                Some(ir)
            }
        }
    };

    // S3 (infrastructure-only): attempt to translate the step function body
    // into a `TransitionProp`. The translation is deliberately conservative:
    // if normalization produces a `NormalizedFuzzer::Opaque` leaf (which is
    // common today because `normalize_fuzzer_from_expr` does not yet
    // recognise `return`/`and_then`/`fork*` in step-function bodies — that
    // lands in Issue S2), the resulting `TransitionProp` will be a single
    // `Unsupported` leaf. In that case we return `None` so downstream emission
    // is a no-op until S4 wires the predicate in. When S2 enriches the
    // normalizer, this field will start carrying meaningful structure without
    // further changes here.
    let transition_prop = transition_prop_from_step_function(
        &args[1].value,
        function_index,
        data_types,
        visiting_functions,
    );

    // S4: capture the initial-state expression's ShallowIr so the Lean
    // emitter can pin `isValidTrace` to the concrete starting state
    // rather than an unconstrained existential.
    let initial_state_shallow_ir = {
        let ir = typed_expr_to_shallow_ir(&args[0].value, data_types);
        if shallow_ir_is_vacuous(&ir) {
            None
        } else {
            Some(ir)
        }
    };

    Some(FuzzerSemantics::StateMachineTrace {
        acceptance,
        state_type: semantic_type_from_type(state_type.as_ref()),
        step_input_types,
        label_type,
        event_type,
        transition_semantics,
        output_semantics,
        step_function_ir,
        step_ir_unsupported_reason: None,
        transition_prop,
        initial_state_shallow_ir,
    })
}

/// Compute a `TransitionProp` from a step-function expression if possible,
/// returning `None` when the translation would yield no extractable content.
///
/// Unlike the early S3 implementation — which normalized the step-function
/// body into a `NormalizedFuzzer` first, losing `If`/`fork*` structural
/// information through `merge_branch_normalizations` — this dispatches
/// directly on `TypedExpr` so the monadic control flow (`if`, `and_then`,
/// `fork*_and_then`) maps one-to-one onto `TransitionProp::IfThenElse`,
/// `Exists`, and `Or`. See the design note in
/// `typed_expr_to_transition_prop` for the supported shapes.
fn transition_prop_from_step_function(
    step_function: &TypedExpr,
    function_index: &FunctionIndex<'_>,
    data_types: &IndexMap<&DataTypeKey, &TypedDataType>,
    visiting_functions: &mut BTreeSet<(String, String)>,
) -> Option<TransitionProp> {
    // Resolve `step` to its body when it's a module-level function reference,
    // mirroring the `step_function_ir` path above. For inline `TypedExpr::Fn`
    // step functions we descend into the body directly.
    let (body_expr, current_module): (&TypedExpr, String) = match step_function {
        TypedExpr::Var { constructor, .. } => {
            if let ValueConstructorVariant::ModuleFn { module, name, .. } = &constructor.variant {
                let key = (module.clone(), name.clone());
                // Guard recursion: if we're already inside this function,
                // bail out rather than loop.
                if visiting_functions.contains(&key) {
                    return None;
                }
                match find_function(function_index, module, name) {
                    Some(function) => (&function.body, module.clone()),
                    None => return None,
                }
            } else {
                return None;
            }
        }
        TypedExpr::Fn { body, .. } => (body.as_ref(), String::new()),
        _ => return None,
    };

    let constant_index: ConstantIndex<'_> = HashMap::new();
    let local_values: BTreeMap<String, TypedExpr> = BTreeMap::new();
    let visiting_locals: BTreeSet<String> = BTreeSet::new();
    let mut visiting_value_aliases: BTreeSet<String> = BTreeSet::new();

    let prop = typed_expr_to_transition_prop(
        body_expr,
        &current_module,
        function_index,
        &constant_index,
        &local_values,
        data_types,
        visiting_functions,
        &visiting_locals,
        &mut visiting_value_aliases,
    );

    if transition_prop_is_trivially_unsupported(&prop) {
        None
    } else {
        Some(prop)
    }
}

/// Translate a `NormalizedFuzzer` into a `TransitionProp`.
///
/// This is the core transition-proposition translation pass. The mapping is:
///
/// - `Bind { source, result }` → `Exists { domain = semantics(source),
///   body = translate(result) }` (the `and_then` / `let x <- ...` case).
/// - `StateMachineTrace` / `Product` / `Map` / `List` at top level are not
///   expected inside a step-function body and are rejected as `Unsupported`.
/// - `Primitive { known_constraint: Exact(v) }` → `EqOutput(v)` when the
///   constraint is an exact scalar (the `return(v)` case).
/// - `Primitive` without a usable constraint → `Unsupported` so the drop is
///   audited (sound: never silently widens the precondition).
/// - `Opaque { reason }` → `Unsupported { reason }`.
///
/// `Or` / `IfThenElse` / `Match` / `And` arms land here once S2 enriches
/// `normalize_fuzzer_from_expr` with `fork*` / `such_that` / `either`
/// recognizers; today those patterns still degrade to `Opaque`.
pub fn normalized_fuzzer_to_transition_prop(normalized: &NormalizedFuzzer) -> TransitionProp {
    match normalized {
        NormalizedFuzzer::Opaque { reason, .. } => TransitionProp::Unsupported {
            reason: reason.clone(),
            source_location: None,
        },
        NormalizedFuzzer::Primitive {
            known_constraint, ..
        } => match known_constraint {
            Some(FuzzerConstraint::Exact(exact)) => {
                TransitionProp::EqOutput(shallow_ir_from_fuzzer_exact_value(exact))
            }
            _ => TransitionProp::Unsupported {
                reason: "primitive fuzzer leaf without an exact-value constraint \
                         — no extractable transition proposition"
                    .to_string(),
                source_location: None,
            },
        },
        NormalizedFuzzer::Bind { source, result } => {
            // `let x <- source; result`. We cannot recover `x`'s name from
            // the normalized form (it is erased during normalization), so
            // we synthesize a placeholder binder — this is fine for
            // generated Lean because the emitter uses a fresh-name
            // generator.
            let domain = fuzzer_semantics_of_normalized(source);
            TransitionProp::Exists {
                binder: "_bind".to_string(),
                ty: ShallowIrType::Data,
                domain: Box::new(domain),
                body: Box::new(normalized_fuzzer_to_transition_prop(result)),
            }
        }
        NormalizedFuzzer::Map { source, .. } => {
            // A map `fuzz.map(f, g)` denotes `{ g v | v ∈ ⟦f⟧ }`. Without
            // lowering `g` we cannot assert `transition = g(v)` precisely,
            // so we fall back to the source domain as an existential and
            // mark the body as unsupported. Preserving the source is
            // important: the source domain is a sound (never-widening)
            // over-approximation of what the map can produce.
            let domain = fuzzer_semantics_of_normalized(source);
            TransitionProp::Exists {
                binder: "_map".to_string(),
                ty: ShallowIrType::Data,
                domain: Box::new(domain),
                body: Box::new(TransitionProp::Unsupported {
                    reason: "map mapper body is not yet lowered to a proposition".to_string(),
                    source_location: None,
                }),
            }
        }
        NormalizedFuzzer::Product { .. } => TransitionProp::Unsupported {
            reason: "product-shaped fuzzer is not a valid step-function body".to_string(),
            source_location: None,
        },
        NormalizedFuzzer::List { .. } => TransitionProp::Unsupported {
            reason: "list-shaped fuzzer is not a valid step-function body".to_string(),
            source_location: None,
        },
        NormalizedFuzzer::StateMachineTrace { .. } => TransitionProp::Unsupported {
            reason: "nested state-machine trace inside a step-function body is not supported"
                .to_string(),
            source_location: None,
        },
    }
}

/// Best-effort semantics extraction for a `NormalizedFuzzer` sub-tree.
///
/// This is used by `normalized_fuzzer_to_transition_prop` to fill the
/// `domain` field of `Exists` binders. The full-fidelity variant,
/// `normalized_fuzzer_semantics`, requires a `data_types` index that is
/// not threaded through the TransitionProp path, so we compute a
/// lightweight, structural semantics here. When richer information is
/// needed, S4 will thread the index in.
fn fuzzer_semantics_of_normalized(normalized: &NormalizedFuzzer) -> FuzzerSemantics {
    match normalized {
        NormalizedFuzzer::Opaque { reason, .. } => FuzzerSemantics::Opaque {
            reason: reason.clone(),
        },
        NormalizedFuzzer::Primitive {
            known_constraint, ..
        } => match known_constraint {
            Some(FuzzerConstraint::IntRange { min, max }) => FuzzerSemantics::IntRange {
                min: Some(min.clone()),
                max: Some(max.clone()),
            },
            Some(FuzzerConstraint::ByteStringLenRange { min_len, max_len }) => {
                FuzzerSemantics::ByteArrayRange {
                    min_len: Some(*min_len),
                    max_len: Some(*max_len),
                }
            }
            Some(FuzzerConstraint::Exact(v)) => FuzzerSemantics::Exact(v.clone()),
            Some(FuzzerConstraint::OneOf(values)) => FuzzerSemantics::OneOf(values.clone()),
            Some(FuzzerConstraint::DataConstructorTags { tags }) => {
                FuzzerSemantics::Constructors { tags: tags.clone() }
            }
            _ => FuzzerSemantics::Data,
        },
        NormalizedFuzzer::Map { .. } => FuzzerSemantics::Data,
        NormalizedFuzzer::Bind { result, .. } => fuzzer_semantics_of_normalized(result),
        NormalizedFuzzer::Product { elements } => FuzzerSemantics::Product(
            elements
                .iter()
                .map(fuzzer_semantics_of_normalized)
                .collect(),
        ),
        NormalizedFuzzer::List {
            element,
            min_len,
            max_len,
        } => FuzzerSemantics::List {
            element: Box::new(fuzzer_semantics_of_normalized(element)),
            min_len: *min_len,
            max_len: *max_len,
        },
        NormalizedFuzzer::StateMachineTrace { .. } => FuzzerSemantics::Opaque {
            reason: "nested state-machine trace cannot appear as a sub-fuzzer domain".to_string(),
        },
    }
}

/// Lower a `FuzzerExactValue` to a `ShallowIr` constant for embedding in
/// `TransitionProp::EqOutput`.
fn shallow_ir_from_fuzzer_exact_value(exact: &FuzzerExactValue) -> ShallowIr {
    match exact {
        FuzzerExactValue::Bool(b) => ShallowIr::Const(ShallowConst::Bool(*b)),
        FuzzerExactValue::ByteArray(bytes) => {
            ShallowIr::Const(ShallowConst::ByteArray(hex::encode(bytes)))
        }
        FuzzerExactValue::String(s) => ShallowIr::Const(ShallowConst::String(s.clone())),
    }
}

/// Structural counterpart to the rendered-Lean-text vacuity probe in
/// `aiken-project::verify::transition_body_is_vacuous`.  Returns `true`
/// iff the `TransitionProp` would, after lowering, produce a Lean
/// `isValidTransition` body that is universally provable — i.e. carries
/// no semantic constraint.
///
/// **Refinement vs. text predicate.**  This predicate is strictly more
/// precise than the text-based version: the text predicate gives up on
/// any non-trivial `∧` / `∨` shape (it does not recurse through those
/// connectives), whereas the structural version recurses on every
/// composite node.  As a result, `And([Unsupported, Unsupported])` is
/// flagged here but not by the text predicate.  The drift sentinel
/// pinned in `aiken-project::verify::tests` asserts the safe direction:
/// every prop the text predicate flags as vacuous, this one flags too.
///
/// **Why not just match the AST shape directly?**  Because the lowering
/// step in `verify::emit_transition_prop_as_lean` widens several
/// variants to literal `True`:
///   * `Pure(_)`   — S4-widened (the inner `ShallowIr` is dropped).
///   * `Unsupported { .. }` — widened by definition. The soundness rule is
///     that we never silently drop an unlowerable constraint without a
///     `True` over-approximation.
///   * Empty `And([])` / `Or([])` / `Match { arms: [] }` — emitted as
///     literal `True` (an empty conjunction is `True` and an empty
///     disjunction was deliberately widened to `True` in the emitter
///     to avoid an unsatisfiable precondition).
///
/// Mirroring those widenings here keeps the structural verdict in lock-
/// step with what actually lands in the generated `.lean` file.
///
/// **Exhaustive match (no wildcard).**  Any future `TransitionProp`
/// variant must be classified explicitly.  Default-falsing a new variant
/// would silently widen it past the vacuity guard, which is the exact
/// soundness regression this predicate exists to prevent.
fn exists_bound_output_domain_constrains_transition(
    binder: &str,
    domain: &FuzzerSemantics,
    body: &TransitionProp,
) -> bool {
    matches!(
        body,
        TransitionProp::EqOutput(ShallowIr::Var { name, .. })
            if name == binder
                && !matches!(domain, FuzzerSemantics::Data | FuzzerSemantics::Opaque { .. })
    )
}

pub fn transition_prop_is_vacuous(tp: &TransitionProp) -> bool {
    match tp {
        // `Pure(_)` is currently widened to literal `True` by the Lean
        // emitter (S4 audit log entry "Pure(ShallowIr) condition widened
        // to True").  Until the emitter learns to lower `Pure` precisely,
        // this is structurally equivalent to a `True` widening.
        TransitionProp::Pure(_) => true,
        // `EqOutput(ir)` is only non-vacuous when the rhs lowers to a real
        // structural `Data` expression. Roots that `emit_shallow_ir_as_lean_data`
        // widens to a fresh existential (`Var`, generic `Opaque`, `If`, etc.)
        // still render as `∃ v, transition = v`, which is trivially satisfiable.
        // Reuse the ShallowIr classifier so `EqOutput(Opaque{S0002})` keeps its
        // typed-code carve-out while constructor-shaped rhs values remain
        // non-vacuous.
        TransitionProp::EqOutput(ir) => shallow_ir_is_vacuous(ir),
        // `SubGenerator` lowers to a reference to a named opaque predicate
        // (`__SGPFX__{fn}_prop state binding`).  The predicate body is
        // unknown to us, but the AST node carries a real constraint name
        // that downstream tooling (E0018 hard-error path) refuses to
        // discharge silently.  Treat as non-vacuous.
        TransitionProp::SubGenerator { .. } => false,
        // `Unsupported` is widened to literal `True` per the soundness rule.
        TransitionProp::Unsupported { .. } => true,
        // `Exists { binder, domain, body }`: most existentials add no information
        // when the body is vacuous, so recurse on the body. One narrow carve-out
        // matters for bind-shaped `let x <- source; return(x)`: the local
        // `EqOutput(Var x)` body still renders like a vacuous equality, but the
        // theorem-level output-semantics precondition restricts `transition` to
        // the bind domain. Treat only that bound-output/domain pair as
        // non-vacuous; truly unconstrained (`Data`) or opaque domains remain
        // vacuous.
        TransitionProp::Exists {
            binder,
            domain,
            body,
            ..
        } => {
            if exists_bound_output_domain_constrains_transition(
                binder,
                domain.as_ref(),
                body.as_ref(),
            ) {
                false
            } else {
                transition_prop_is_vacuous(body)
            }
        }
        // `And(parts)`: empty → emitted as literal `True` (vacuous);
        // non-empty → vacuous iff every conjunct is vacuous.  `iter().all()`
        // returns `true` for empty iterators, which gives the right answer
        // in both cases.
        TransitionProp::And(parts) => parts.iter().all(transition_prop_is_vacuous),
        // `Or(branches)`: empty → emitter widens to `True` (logged as
        // "empty Or(branches) widened to True"); non-empty → vacuous iff
        // every branch is vacuous.  Same `iter().all()` argument as `And`.
        TransitionProp::Or(branches) => branches.iter().all(transition_prop_is_vacuous),
        // `IfThenElse { cond, t, e }`: emitted as
        // `((cond=true ∧ t) ∨ (cond=false ∧ e))`.  When both branches are
        // vacuous this is `(cond=true ∨ cond=false)` which is True for
        // decidable Bool — strictly more precise than the text predicate
        // (which gives up on top-level `∨`).  Safe to refine.
        TransitionProp::IfThenElse { t, e, .. } => {
            transition_prop_is_vacuous(t) && transition_prop_is_vacuous(e)
        }
        // `Match { arms }`: empty → emitted as `True` (vacuous); non-empty
        // → emitted as a disjunction of arm bodies.  Vacuous iff every
        // arm body is vacuous.  Same `iter().all()` reasoning as `Or`.
        TransitionProp::Match { arms, .. } => {
            arms.iter().all(|arm| transition_prop_is_vacuous(&arm.body))
        }
    }
}

/// A `TransitionProp` is trivially unsupported when every leaf it contains
/// is `Unsupported` — there is no extractable transition content for Lean
/// emission, so we treat it as `None` at the storage level.
///
/// This is more permissive than a shallow `matches!(... Unsupported)`: a
/// deep tree whose root is `Or`/`IfThenElse`/`Exists` still counts as
/// trivial when every leaf is `Unsupported`, because the containing
/// `Or(Unsupported, Unsupported)` carries no more information than a bare
/// `Unsupported` would.
fn transition_prop_is_trivially_unsupported(prop: &TransitionProp) -> bool {
    match prop {
        TransitionProp::Unsupported { .. } => true,
        // SubGenerator carries a named opaque predicate — not trivially unsupported.
        TransitionProp::SubGenerator { .. } => false,
        TransitionProp::EqOutput(_) => false,
        TransitionProp::Pure(_) => false,
        TransitionProp::Exists { body, .. } => transition_prop_is_trivially_unsupported(body),
        TransitionProp::And(children) | TransitionProp::Or(children) => {
            !children.is_empty()
                && children
                    .iter()
                    .all(transition_prop_is_trivially_unsupported)
        }
        TransitionProp::IfThenElse { t, e, .. } => {
            transition_prop_is_trivially_unsupported(t)
                && transition_prop_is_trivially_unsupported(e)
        }
        TransitionProp::Match { arms, .. } => {
            !arms.is_empty()
                && arms
                    .iter()
                    .all(|arm| transition_prop_is_trivially_unsupported(&arm.body))
        }
    }
}

/// Collect all unique `(module, fn_name)` pairs from `SubGenerator` leaves
/// in `prop`. Used by the Lean emitter to pre-declare opaque predicate stubs.
/// Order is deterministic (DFS, left-to-right) and duplicates are removed
/// while preserving first-occurrence order.
pub fn collect_sub_generators_from_prop(prop: &TransitionProp) -> Vec<(String, String)> {
    let mut seen: Vec<(String, String)> = Vec::new();
    collect_sub_generators_inner(prop, &mut seen);
    seen
}

fn collect_sub_generators_inner(prop: &TransitionProp, acc: &mut Vec<(String, String)>) {
    match prop {
        TransitionProp::SubGenerator { module, fn_name } => {
            let entry = (module.clone(), fn_name.clone());
            if !acc.contains(&entry) {
                acc.push(entry);
            }
        }
        TransitionProp::Exists { body, .. } => collect_sub_generators_inner(body, acc),
        TransitionProp::And(parts) | TransitionProp::Or(parts) => {
            for p in parts {
                collect_sub_generators_inner(p, acc);
            }
        }
        TransitionProp::IfThenElse { t, e, .. } => {
            collect_sub_generators_inner(t, acc);
            collect_sub_generators_inner(e, acc);
        }
        TransitionProp::Match { arms, .. } => {
            for arm in arms {
                collect_sub_generators_inner(&arm.body, acc);
            }
        }
        TransitionProp::Unsupported { .. }
        | TransitionProp::EqOutput(_)
        | TransitionProp::Pure(_) => {}
    }
}

/// Walk a `ShallowIr` tree and return the first `Opaque` carrying a typed
/// `OpaqueCode` (today: only `ConstructorTagUnresolved`). Returns `None` if
/// no such node exists.
///
/// The verify-side dispatcher uses this to detect typed-code-bearing nodes
/// nested inside structural shapes (e.g. `Construct { fields: [..., Opaque{S0002}] }`)
/// or stripped of `Let` wrappers, mirroring the carve-out in
/// `shallow_ir_is_vacuous`. The walk visits every child node so a marker
/// buried beneath structural wrapping is not missed.
///
/// Renamed (commit 18) from `find_first_s0002_opaque_in_shallow_ir` to
/// reflect the typed-payload model that retired the `S0002_REASON_PREFIX`
/// string-prefix bridge.
pub fn find_first_typed_opaque_in_shallow_ir(ir: &ShallowIr) -> Option<&OpaqueCode> {
    fn visit(ir: &ShallowIr) -> Option<&OpaqueCode> {
        match ir {
            ShallowIr::Opaque {
                code: Some(code), ..
            } => Some(code),
            ShallowIr::Opaque { .. }
            | ShallowIr::Const(_)
            | ShallowIr::Var { .. }
            | ShallowIr::FuzzExistential { .. } => None,
            ShallowIr::Let { value, body, .. } => visit(value).or_else(|| visit(body)),
            ShallowIr::If {
                cond,
                then_branch,
                else_branch,
            } => visit(cond)
                .or_else(|| visit(then_branch))
                .or_else(|| visit(else_branch)),
            ShallowIr::Match { subject, arms } => visit(subject).or_else(|| {
                for arm in arms {
                    if let Some(found) = visit(&arm.body) {
                        return Some(found);
                    }
                }
                None
            }),
            ShallowIr::Construct { fields, .. } => {
                for field in fields {
                    if let Some(found) = visit(field) {
                        return Some(found);
                    }
                }
                None
            }
            ShallowIr::FieldAccess { record, .. } => visit(record),
            ShallowIr::RecordUpdate { record, updates } => visit(record).or_else(|| {
                for (_, val) in updates {
                    if let Some(found) = visit(val) {
                        return Some(found);
                    }
                }
                None
            }),
            ShallowIr::BinOp { left, right, .. } => visit(left).or_else(|| visit(right)),
            ShallowIr::Tuple(elems)
            | ShallowIr::ListLit {
                elements: elems, ..
            } => {
                for elem in elems {
                    if let Some(found) = visit(elem) {
                        return Some(found);
                    }
                }
                None
            }
        }
    }
    visit(ir)
}

/// Walk a `TransitionProp` tree and return the first `ShallowIr::Opaque`
/// carrying a typed `OpaqueCode`. Returns `None` if no such node exists.
///
/// The verify-side `try_generate_two_phase_proof` calls this *before* the
/// generic vacuity guard so an `EqOutput(Opaque{code: Some(...)})` (or any
/// nested `EqOutput`/`Pure` carrying a typed code) raises a hard,
/// non-skippable error rather than being silently widened to `True` and
/// routed through the skippable `FallbackRequired` path.
///
/// The walk descends into every recursive `TransitionProp` shape and into
/// every `ShallowIr` leaf via `find_first_typed_opaque_in_shallow_ir`,
/// because the typed code may live arbitrarily deep inside an expression
/// (e.g. `EqOutput(Construct { fields: [Opaque{S0002}] })`).
///
/// Renamed (commit 18) from `find_first_s0002_opaque_in_transition_prop`.
pub fn find_first_typed_opaque_in_transition_prop(prop: &TransitionProp) -> Option<OpaqueCode> {
    match prop {
        TransitionProp::EqOutput(ir) | TransitionProp::Pure(ir) => {
            find_first_typed_opaque_in_shallow_ir(ir).cloned()
        }
        TransitionProp::Exists { body, .. } => find_first_typed_opaque_in_transition_prop(body),
        TransitionProp::And(parts) | TransitionProp::Or(parts) => {
            for p in parts {
                if let Some(found) = find_first_typed_opaque_in_transition_prop(p) {
                    return Some(found);
                }
            }
            None
        }
        TransitionProp::IfThenElse { cond, t, e } => {
            // `cond` is a ShallowIr; check it too for completeness.
            if let Some(found) = find_first_typed_opaque_in_shallow_ir(cond) {
                return Some(found.clone());
            }
            find_first_typed_opaque_in_transition_prop(t)
                .or_else(|| find_first_typed_opaque_in_transition_prop(e))
        }
        TransitionProp::Match { scrutinee, arms } => {
            if let Some(found) = find_first_typed_opaque_in_shallow_ir(scrutinee) {
                return Some(found.clone());
            }
            for arm in arms {
                if let Some(found) = find_first_typed_opaque_in_transition_prop(&arm.body) {
                    return Some(found);
                }
            }
            None
        }
        TransitionProp::SubGenerator { .. } | TransitionProp::Unsupported { .. } => None,
    }
}

// ============================================================================
// Direct TypedExpr -> TransitionProp translation
// ============================================================================
//
// This translation preserves the structural shape of the step-function body
// (`if`, `and_then`, `fork*_and_then`, `return`), which the earlier
// `NormalizedFuzzer`-based translation erased via `merge_branch_normalizations`.
//
// The recognizers are *shape-based*, not *name-based*: we detect
// `and_then(src, fn(x) { body })` by checking that the callee has two args
// where the first expression has a `Fuzzer<T>` type and the second is a
// unary continuation returning a `Fuzzer<U>`. Likewise for `fork*_and_then`
// we look for zero-arg `fn() -> Fuzzer<T>` thunk arguments. This keeps the
// translation robust to renames and to user-defined wrappers that follow
// the same operational shape.

/// Translate a step-function body `TypedExpr` into a `TransitionProp`.
///
/// | TypedExpr shape                              | TransitionProp node        |
/// |----------------------------------------------|----------------------------|
/// | `If { cond, then, else }`                    | `IfThenElse { cond, t, e }`|
/// | `Call(fun, [src, fn(x){body}])` where `src`  |                            |
/// | is `Fuzzer<T>` and `fn` is `T -> Fuzzer<U>`  | `Exists { binder = x, .. }`|
/// | `Call(fun, args)` with zero-arg fuzzer       |                            |
/// | thunks (fork-style)                          | `Or([translate(body_i)])`  |
/// | `Call(fun, [v])` where `fun` returns `Fuzzer`| `EqOutput(shallow_ir(v))`  |
/// | `Var` (local alias)                          | lookup in `local_values`   |
/// | `Fn { args, body }`                          | translate body             |
/// | `Sequence`/`Pipeline`                        | translate terminal expr    |
/// | everything else                              | `Unsupported { reason }`   |
#[allow(clippy::too_many_arguments)]
fn typed_expr_to_transition_prop(
    expr: &TypedExpr,
    current_module: &str,
    function_index: &FunctionIndex<'_>,
    constant_index: &ConstantIndex<'_>,
    local_values: &BTreeMap<String, TypedExpr>,
    data_types: &IndexMap<&DataTypeKey, &TypedDataType>,
    visiting_functions: &mut BTreeSet<(String, String)>,
    // H3: track binder names whose substitution is currently in flight so that
    // `translate_bind` can detect a re-binding cycle (a nested bind that
    // shadows the same name) and avoid infinite recursion. The naive scheme
    // here just *prevents* the cycle — a faithful inner-shadowing rewrite is
    // explicitly deferred (see plan §H3 / debate-outcome row 6: the
    // `LocalValueKind` refactor is post-merge).
    visiting_locals: &BTreeSet<String>,
    // H3 follow-up — lookup-side visited-set guard. Tracks the chain of
    // `local_values` keys whose RHS is currently being recursively
    // resolved by the `Var`-arm lookup. Mirrors the pattern used by
    // `materialize_local_alias_argument`: insert on entry, remove on exit.
    // This makes the Var arm self-protecting against ANY cyclic
    // `local_values` entry — bind-site self-references, leading-let
    // shadow chains (`let x = 1; let x = x` or
    // `let x = 0; let y = x; let x = y`), or any future writer that
    // produces a self- or mutually-referential entry. See the safety
    // comment at the Var arm for the architectural rationale.
    //
    // Mutability rationale: this differs from `visiting_locals` (which
    // is `&BTreeSet`) because the Var-arm guard is a per-step
    // insert/remove pattern (matching `materialize_local_alias_argument`
    // at line 6341) rather than a clone-and-extend at a single call
    // site. Cloning per Var lookup would be wasteful in the common
    // no-cycle case.
    visiting_value_aliases: &mut BTreeSet<String>,
) -> TransitionProp {
    // Collect non-monadic `let` bindings from the leading elements of any
    // Sequence/Pipeline before peeling to the terminal expression. A binding
    // `let x = v` (Assignment with a non-Fuzzer value) extends local_values
    // so that `Var "x"` references inside the terminal body can be substituted.
    // Fuzzer-typed assignments are NOT collected here — they are monadic binds
    // and must be handled by the bind/fork shape detectors below.
    let mut local_values_extended;
    let local_values = if let TypedExpr::Sequence { expressions, .. }
    | TypedExpr::Pipeline { expressions, .. } = expr
    {
        if expressions.len() > 1 {
            local_values_extended = local_values.clone();
            for e in expressions.iter().take(expressions.len().saturating_sub(1)) {
                if let TypedExpr::Assignment { pattern, value, .. } = e {
                    // Only collect pure (non-Fuzzer) bindings.
                    if extract_fuzzer_payload_type(value.tipo().as_ref()).is_none() {
                        if let Some(name) = pattern_var_name(pattern) {
                            local_values_extended.insert(name.to_string(), value.as_ref().clone());
                        }
                    }
                }
            }
            &local_values_extended
        } else {
            local_values
        }
    } else {
        local_values
    };
    let expr = terminal_expression(expr);

    match expr {
        TypedExpr::If {
            branches,
            final_else,
            ..
        } => {
            // Build the if/else-if/else chain right-associated: the final
            // `else` is the innermost branch. Single-branch `if` becomes a
            // straight `IfThenElse`.
            let mut result = typed_expr_to_transition_prop(
                final_else.as_ref(),
                current_module,
                function_index,
                constant_index,
                local_values,
                data_types,
                visiting_functions,
                visiting_locals,
                visiting_value_aliases,
            );
            for branch in branches.iter().rev() {
                let cond_ir = typed_expr_to_shallow_ir(&branch.condition, data_types);
                let then_prop = typed_expr_to_transition_prop(
                    &branch.body,
                    current_module,
                    function_index,
                    constant_index,
                    local_values,
                    data_types,
                    visiting_functions,
                    visiting_locals,
                    visiting_value_aliases,
                );
                result = TransitionProp::IfThenElse {
                    cond: cond_ir,
                    t: Box::new(then_prop),
                    e: Box::new(result),
                };
            }
            result
        }

        TypedExpr::Call {
            fun, args, tipo, ..
        } => {
            // Shape-based recognizers. Order matters: try the most specific
            // shapes (bind, fork) before falling back to return/unsupported.
            if let Some((source, binder, cont_body)) = detect_bind_call(args) {
                return translate_bind(
                    source,
                    binder,
                    cont_body,
                    current_module,
                    function_index,
                    constant_index,
                    local_values,
                    data_types,
                    visiting_functions,
                    visiting_locals,
                    visiting_value_aliases,
                );
            }

            if let Some((thunk_bodies, continuation)) = detect_fork_call(args) {
                let branches: Vec<TransitionProp> = thunk_bodies
                    .into_iter()
                    .map(|body| {
                        typed_expr_to_transition_prop(
                            body,
                            current_module,
                            function_index,
                            constant_index,
                            local_values,
                            data_types,
                            visiting_functions,
                            visiting_locals,
                            visiting_value_aliases,
                        )
                    })
                    .collect();
                let or_prop = TransitionProp::Or(branches);

                // `fork*_and_then(..., cont)` — wrap the Or-of-thunks in
                // `Exists { binder, body = translate(cont_body) }`. The
                // fork chooses one thunk; its output flows through `cont`
                // as the binder of an existential. Leaving the binder type
                // as `Data` / falling back to the call result type is a
                // SOUND over-approximation: any actual fork output still
                // lies in the domain of the chosen thunk.
                //
                // NOTE: H3 only threads binders through `translate_bind`
                // (the `and_then` path). The fork continuation's binder
                // is not added to `local_values` here — the plan scopes
                // H3 to bind only. We still pass `visiting_locals`
                // through unchanged so any nested `translate_bind` inside
                // `cont_body` keeps the cycle guard accurate.
                if let Some((binder, cont_body)) = continuation {
                    let body_prop = typed_expr_to_transition_prop(
                        cont_body,
                        current_module,
                        function_index,
                        constant_index,
                        local_values,
                        data_types,
                        visiting_functions,
                        visiting_locals,
                        visiting_value_aliases,
                    );
                    return TransitionProp::Exists {
                        binder,
                        ty: ShallowIrType::Data,
                        domain: Box::new(FuzzerSemantics::Data),
                        body: Box::new(TransitionProp::And(vec![or_prop, body_prop])),
                    };
                }

                return or_prop;
            }

            if let Some(inner) = detect_return_call(fun, args, tipo.as_ref()) {
                return TransitionProp::EqOutput(typed_expr_to_shallow_ir(inner, data_types));
            }

            // NOTE: fuzz.map(src, fn(x) { f(x) }) is NOT specially handled here.
            // A `detect_map_call` path was considered but removed because
            // `typed_expr_to_shallow_ir` does not consult `local_values`, so any
            // `Var "x"` inside the mapper body would become a fresh existential
            // unrelated to the outer binder — producing a misleading predicate.
            // TODO: once `typed_expr_to_shallow_ir` accepts a substitution map,
            // re-introduce `detect_map_call` with proper binder threading.
            // For now, fuzz.map calls fall through to the S6 sub-generator inliner,
            // which may resolve them if the mapper is a named stdlib function.

            // S6: if the callee is a module-level function whose result is a
            // Fuzzer, first attempt to inline its body as a nested TransitionProp.
            // This eliminates opaque stubs for sub-generators whose bodies can be
            // faithfully translated with the existing machinery.
            // Fall back to a named `SubGenerator` stub if:
            //   (a) the callee can't be resolved (external / opaque),
            //   (b) we're already visiting it (cycle guard), or
            //   (c) the inlined result is `Unsupported` (body not translatable).
            if extract_fuzzer_payload_type(tipo.as_ref()).is_some() {
                if let TypedExpr::Var { constructor, .. } = fun.as_ref() {
                    if let ValueConstructorVariant::ModuleFn { module, name, .. } =
                        &constructor.variant
                    {
                        // Attempt inlining via function resolution.
                        // Track the resolved identity separately so the fallback
                        // stub uses the *defined* module/name (which may differ
                        // from the constructor's textual module when the call is
                        // resolved through a local-alias chain).
                        let mut stub_module = module.clone();
                        let mut stub_name = name.clone();
                        if let Some((resolved, resolved_locals, _)) =
                            resolve_function_with_applied_args(
                                expr,
                                current_module,
                                function_index,
                                local_values,
                            )
                        {
                            // Use the resolved identity for the fallback stub so it
                            // correctly names the defining module even when the call
                            // was reached via a local alias in a different module.
                            stub_module = resolved.module_name.clone();
                            stub_name = resolved.function_name.clone();
                            let key =
                                (resolved.module_name.clone(), resolved.function_name.clone());
                            if visiting_functions.insert(key.clone()) {
                                // Crossing a function boundary into the sub-generator's
                                // body: start with a fresh `visiting_locals` set since
                                // the binders inside the inlined function body live in
                                // a different scope than the caller's binders.
                                //
                                // Same scope-reset rationale applies to
                                // `visiting_value_aliases`: the sub-generator's
                                // body has its own `local_values` (`resolved_locals`),
                                // so any alias-chain cycle is an intra-callee
                                // concern. Reusing the caller's set would
                                // incorrectly poison lookups inside the callee.
                                let fresh_visiting_locals: BTreeSet<String> = BTreeSet::new();
                                let mut fresh_visiting_value_aliases: BTreeSet<String> =
                                    BTreeSet::new();
                                let inlined = typed_expr_to_transition_prop(
                                    &resolved.function.body,
                                    &resolved.module_name,
                                    function_index,
                                    constant_index,
                                    &resolved_locals,
                                    data_types,
                                    visiting_functions,
                                    &fresh_visiting_locals,
                                    &mut fresh_visiting_value_aliases,
                                );
                                visiting_functions.remove(&key);
                                // Only use the inlined result when it's not
                                // trivially Unsupported — otherwise fall through
                                // to the named stub which is more auditable.
                                if !matches!(inlined, TransitionProp::Unsupported { .. }) {
                                    return inlined;
                                }
                            }
                        }
                        // Fallback: emit named opaque stub using the resolved
                        // (defining) module/name for accurate auditability.
                        return TransitionProp::SubGenerator {
                            module: stub_module,
                            fn_name: stub_name,
                        };
                    }
                }
            }

            TransitionProp::Unsupported {
                reason: format!(
                    "step-function call '{}' is not a recognized bind/fork/return shape",
                    describe_expr(fun)
                ),
                source_location: None,
            }
        }

        TypedExpr::Var {
            name, constructor, ..
        } if matches!(
            constructor.variant,
            ValueConstructorVariant::LocalVariable { .. }
        ) =>
        {
            // Substitute local aliases, if any. Otherwise this is an opaque
            // reference (e.g. the state parameter `st`) that carries no
            // extractable proposition on its own.
            //
            // H3 termination — lookup-side visited-set guard. This arm
            // recurses into the substituted expression. To guarantee
            // termination regardless of what entries `local_values`
            // contains, the Var arm self-protects with the
            // `visiting_value_aliases` set: we insert `name` before
            // recursing on the looked-up value and remove it after the
            // recursive call returns. If `name` is already present, we
            // emit a precision-imprecise `TransitionProp::Unsupported`
            // marker and bail. Architecturally this mirrors
            // `materialize_local_alias_argument` (line 6341) which uses
            // the same pattern for a similar resolution path.
            //
            // Why lookup-side and not writer-side: the bind/fork
            // pipeline has TWO writers into `local_values`
            // (`translate_bind`'s `extended_locals.insert` and the
            // leading-`let` collector's `local_values_extended.insert`),
            // and any future writer would have to re-prove
            // non-cyclicity to be sound. Auditing every writer is
            // brittle; absorbing the risk at the single lookup point
            // is robust. The Var arm is now unconditionally
            // terminating against ANY current or future
            // `local_values` entry, including:
            //   - bind-site self-references (`and_then(g, fn(g) {..})`),
            //     which `translate_bind`'s self-reference guard
            //     (`free_vars_in_typed_expr(source).contains(&binder)`)
            //     catches early as belt-and-suspenders for a clearer
            //     error phrase;
            //   - leading-let direct shadows (`let x = 1; let x = x`),
            //     which the leading-`let` collector installs as
            //     `local_values["x"] = Var "x"`;
            //   - leading-let mutual shadows
            //     (`let x = 0; let y = x; let x = y`), which install
            //     `lv["x"] = Var "y"` and `lv["y"] = Var "x"`.
            //
            // The `visiting_locals` parameter is threaded through but
            // NOT consulted here: it tracks bind-site cycles for
            // `translate_bind`, a different concern than alias-chain
            // cycles. The two are orthogonal.
            //
            // Cross-function recursion (sub-generator inlining) is
            // handled by `visiting_functions`, which is independent of
            // `visiting_value_aliases`.
            if let Some(bound) = local_values.get(name) {
                // Visited-set guard: if we're already resolving this
                // name, we have a cycle in the alias chain. Bail with
                // a precision-imprecise marker. This is the safety
                // net against ALL local_values writers — bind-site
                // and leading-let alike.
                if !visiting_value_aliases.insert(name.clone()) {
                    return TransitionProp::Unsupported {
                        reason: format!(
                            "Unsupported: local-alias cycle on '{name}' detected; \
                             lookup recursion bailed to prevent infinite loop. \
                             This precision gap will be closed in a future Aiken release."
                        ),
                        source_location: None,
                    };
                }
                let result = typed_expr_to_transition_prop(
                    bound,
                    current_module,
                    function_index,
                    constant_index,
                    local_values,
                    data_types,
                    visiting_functions,
                    visiting_locals,
                    visiting_value_aliases,
                );
                visiting_value_aliases.remove(name);
                result
            } else {
                TransitionProp::Unsupported {
                    reason: format!(
                        "step-function variable '{name}' has no known transition content",
                    ),
                    source_location: None,
                }
            }
        }

        TypedExpr::Fn { body, .. } => typed_expr_to_transition_prop(
            body.as_ref(),
            current_module,
            function_index,
            constant_index,
            local_values,
            data_types,
            visiting_functions,
            visiting_locals,
            visiting_value_aliases,
        ),

        TypedExpr::Trace { then, .. } => typed_expr_to_transition_prop(
            then.as_ref(),
            current_module,
            function_index,
            constant_index,
            local_values,
            data_types,
            visiting_functions,
            visiting_locals,
            visiting_value_aliases,
        ),

        TypedExpr::When {
            subject, clauses, ..
        } => {
            // Translate each clause's body and produce Or over all branches.
            // Pattern-bound `Var` patterns are added to local_values as aliases
            // for the subject so clauses that reference the bound name via a
            // `Var` lookup can substitute. More complex patterns are not
            // destructured here — clauses fall through to whatever shape their
            // body translates into; Or over branches remains sound.
            //
            // H2 (E0033) — When → Or per-clause logging: any clause whose
            // pattern is NOT `Var` and NOT `Discard` introduces binders that
            // we cannot thread through the precondition (constructor-
            // conditional lowering is not yet implemented). The clause body
            // is therefore wrapped in `And([Unsupported, clause_prop])` so
            // that the verify-side lowering (`emit_transition_prop_as_lean`)
            // pushes a per-clause `[E0033]` entry into `unsupported_log`,
            // bumping the test's `over_approximations` counter. The wrap
            // does NOT change semantics: the Lean translation widens
            // `Unsupported` to `True`, and `True ∧ clause_prop` is
            // logically `clause_prop`. Per the debate-outcome row, we emit
            // ONLY per-clause; no aggregate `When → Or` log is added — that
            // would false-positive on `fork`-derived `Or` nodes.
            if clauses.is_empty() {
                return TransitionProp::Unsupported {
                    reason: "When expression with no clauses".to_string(),
                    source_location: None,
                };
            }
            let branches: Vec<TransitionProp> = clauses
                .iter()
                .map(|clause| {
                    let mut clause_locals = local_values.clone();
                    if let TypedPattern::Var { name, .. } = &clause.pattern {
                        // Add the clause's pattern variable as a local alias for subject.
                        clause_locals.insert(name.clone(), subject.as_ref().clone());
                    }
                    let clause_prop = typed_expr_to_transition_prop(
                        &clause.then,
                        current_module,
                        function_index,
                        constant_index,
                        &clause_locals,
                        data_types,
                        visiting_functions,
                        visiting_locals,
                        visiting_value_aliases,
                    );
                    // Per-clause E0033 widening log. `Var` and `Discard`
                    // patterns introduce no constructor-conditional
                    // constraint and therefore need no widening note.
                    match &clause.pattern {
                        TypedPattern::Var { .. } | TypedPattern::Discard { .. } => clause_prop,
                        other => {
                            let pattern_desc = describe_pattern(other);
                            let binders = collect_pattern_binders(other);
                            let location = other.location();
                            TransitionProp::And(vec![
                                TransitionProp::Unsupported {
                                    reason: format!(
                                        "[E0033] when_pattern_constructor_var_dropped: \
                                         clause pattern `{}` dropped binders {} @ {}:{}; \
                                         constructor-conditional constraints not yet enforced \
                                         (will be supported in a future Aiken release)",
                                        pattern_desc,
                                        binders.join(", "),
                                        current_module,
                                        location.start,
                                    ),
                                    source_location: Some(format!(
                                        "{}:{}",
                                        current_module, location.start,
                                    )),
                                },
                                clause_prop,
                            ])
                        }
                    }
                })
                .collect();
            if branches.len() == 1 {
                branches.into_iter().next().unwrap()
            } else {
                TransitionProp::Or(branches)
            }
        }

        _ => TransitionProp::Unsupported {
            reason: format!(
                "step-function expression '{}' is not a recognized transition shape",
                describe_expr(expr)
            ),
            source_location: None,
        },
    }
}

type BindCall<'a> = (&'a TypedExpr, String, &'a TypedExpr);
type ForkCall<'a> = (Vec<&'a TypedExpr>, Option<(String, &'a TypedExpr)>);

/// Detect `and_then`/bind-style calls by *shape*:
///   - exactly two arguments,
///   - the first is a `Fuzzer<T>`,
///   - the second is a continuation `T -> Fuzzer<U>`, which we recognize
///     through `expression_is_bind_continuation`.
///
/// Returns `Some((source, binder_name, continuation_body))` when the call
/// matches and the continuation is a `Fn` literal we can peel.
fn detect_bind_call(args: &[CallArg<TypedExpr>]) -> Option<BindCall<'_>> {
    if args.len() != 2 {
        return None;
    }
    let source = &args[0].value;
    let cont = &args[1].value;
    if !expression_has_fuzzer_type(source) {
        return None;
    }
    if !expression_is_bind_continuation(cont) {
        return None;
    }
    let cont_term = terminal_expression(cont);
    match cont_term {
        TypedExpr::Fn {
            args: fn_args,
            body,
            ..
        } if fn_args.len() == 1 => {
            let name = fn_args[0]
                .get_variable_name()
                .unwrap_or("_bind")
                .to_string();
            Some((source, name, body.as_ref()))
        }
        _ => None,
    }
}

/// Build the `Exists { binder, domain, body }` node for a recognized bind
/// call. The domain is extracted from the source fuzzer via
/// `normalize_fuzzer_from_expr` + `fuzzer_semantics_of_normalized`.
///
/// H3 (plan §H3) — naive substitute-on-recurse: extend `local_values` with
/// `binder -> source` so that `Var binder` references inside `cont_body`
/// can resolve through the threaded substitution map instead of widening
/// to `True`. The substitution is intentionally the *source fuzzer
/// expression* (not a witness) — the `LocalValueKind` refactor that would
/// distinguish "alias to source" from "drawn-from-domain witness" is
/// explicitly deferred (see plan debate-outcome row 6).
///
/// Two guards prevent infinite recursion in the `Var`-lookup arm of
/// `typed_expr_to_transition_prop`:
///
/// 1. **Bind-on-bind cycle guard**: if `binder` already appears in
///    `visiting_locals` (a nested re-bind of the same name shadows an
///    outer one) we skip the `local_values` extension to avoid losing
///    the outer binding to a subsequent `Var` lookup, and emit a
///    `TransitionProp::Unsupported` marker whose reason contains the
///    stable substring `cyclic monadic-bind binder`.
///
/// 2. **Self-referential source guard** (H3 follow-up): if `binder`
///    appears as a free `Var` *inside* `source` (e.g. `and_then(g,
///    fn(g) { ... })` where the source is `Var g`), inserting
///    `local_values[binder] = source` would create a self-referential
///    entry like `local_values["g"] = Var "g"`. The `Var`-lookup arm
///    would then recurse on itself indefinitely and stack-overflow the
///    lowering. The fix is identical in shape to the cycle guard: skip
///    the insertion and emit a marker, but with the distinct phrase
///    `self-referential monadic-bind binder` so future error-filtering
///    logic can disambiguate.
///
/// In either skipped-insertion case the body still contributes its
/// constraints — the result is `And([Unsupported, body_prop])`, which
/// under the Lean widening rule (`Unsupported ↦ True`) collapses to
/// `True ∧ body_prop = body_prop` semantically while preserving the
/// audit log. If both guards would fire (a cyclic re-bind whose source
/// also references the binder), the cyclic guard takes priority: the
/// `cyclic` test runs first and short-circuits the `self_referential`
/// computation (`let self_referential = !cyclic && ...`), so the
/// emitted marker carries the stable substring
/// `cyclic monadic-bind binder`.
///
/// Note: both guards are precision-preserving early terminations.
/// Even if a future change altered or removed them, termination is
/// still guaranteed by the lookup-side visited-set guard
/// (`visiting_value_aliases`) at the `Var` arm of
/// `typed_expr_to_transition_prop`. These bind-site guards remain
/// useful for clearer error phrases and for stopping the recursion
/// one step earlier (at the bind site) than the lookup-site fallback
/// (at the next Var dereference).
#[allow(clippy::too_many_arguments)]
fn translate_bind(
    source: &TypedExpr,
    binder: String,
    cont_body: &TypedExpr,
    current_module: &str,
    function_index: &FunctionIndex<'_>,
    constant_index: &ConstantIndex<'_>,
    local_values: &BTreeMap<String, TypedExpr>,
    data_types: &IndexMap<&DataTypeKey, &TypedDataType>,
    visiting_functions: &mut BTreeSet<(String, String)>,
    visiting_locals: &BTreeSet<String>,
    visiting_value_aliases: &mut BTreeSet<String>,
) -> TransitionProp {
    let normalized_source = normalize_fuzzer_from_expr(
        source,
        current_module,
        function_index,
        constant_index,
        local_values,
        visiting_functions,
    );
    let domain = fuzzer_semantics_of_normalized(&normalized_source);

    let binder_ty = extract_fuzzer_payload_type(source.tipo().as_ref())
        .map(|t| shallow_ir_type(&t))
        .unwrap_or(ShallowIrType::Data);

    // H3: thread `binder -> source` into `local_values` for the
    // continuation body, gated by the two guards described in the doc
    // comment above (bind-on-bind cycle vs self-referential source).
    let cyclic = visiting_locals.contains(&binder);
    let self_referential = !cyclic && free_vars_in_typed_expr(source).contains(&binder);
    let mut extended_locals = local_values.clone();
    if !cyclic && !self_referential {
        extended_locals.insert(binder.clone(), source.clone());
    }

    // Recurse with the binder added to `visiting_locals` so any nested
    // `translate_bind` re-binding the same name detects the cycle.
    let mut extended_visiting = visiting_locals.clone();
    extended_visiting.insert(binder.clone());

    let body_prop = typed_expr_to_transition_prop(
        cont_body,
        current_module,
        function_index,
        constant_index,
        &extended_locals,
        data_types,
        visiting_functions,
        &extended_visiting,
        visiting_value_aliases,
    );

    // Use the bind-site location (the continuation body's location is
    // the closest available — the bind call wraps it). This mirrors
    // commit 10's H2 source-location convention.
    let final_body = if self_referential {
        let location = cont_body.location();
        let marker = TransitionProp::Unsupported {
            reason: format!(
                "Unsupported: self-referential monadic-bind binder '{binder}' \
                 detected (binder name appears free in the bind's source \
                 expression); threading skipped to prevent infinite recursion \
                 in Var-lookup. This precision gap will be closed in a future \
                 Aiken release."
            ),
            source_location: Some(format!("{}:{}", current_module, location.start)),
        };
        TransitionProp::And(vec![marker, body_prop])
    } else if cyclic {
        let location = cont_body.location();
        let cycle_marker = TransitionProp::Unsupported {
            reason: format!(
                "Unsupported: cyclic monadic-bind binder '{binder}' detected; \
                 threading skipped to prevent infinite recursion. \
                 This precision gap will be closed in a future Aiken release."
            ),
            source_location: Some(format!("{}:{}", current_module, location.start)),
        };
        TransitionProp::And(vec![cycle_marker, body_prop])
    } else {
        body_prop
    };

    TransitionProp::Exists {
        binder,
        ty: binder_ty,
        domain: Box::new(domain),
        body: Box::new(final_body),
    }
}

/// Detect `fork*_and_then`-style calls by *shape*:
///   - at least two arguments that are zero-arg `fn() -> Fuzzer<T>` thunks
///     (the branches).
///   - optionally a continuation `fn(a) -> Fuzzer<b>` as a later argument
///     (the `let (x, ...) <- fork_and_then(...)` desugaring).
///
/// Returns `Some((vec_of_thunk_bodies, optional_continuation))` when the
/// call matches. The thunk bodies are the peeled `Fn { body: ... }` of
/// each branch; the continuation (if any) is its `(binder_name, body)`.
fn detect_fork_call(args: &[CallArg<TypedExpr>]) -> Option<ForkCall<'_>> {
    let thunk_bodies: Vec<&TypedExpr> = args
        .iter()
        .filter_map(|a| zero_arg_fuzzer_thunk_body(&a.value))
        .collect();
    if thunk_bodies.len() < 2 {
        return None;
    }

    // Look for a bind-style continuation argument. In `fork2_and_then`
    // this is the trailing `continue: fn(a) -> Fuzzer<b>` parameter.
    let continuation = args.iter().find_map(|a| {
        if expression_is_bind_continuation(&a.value) {
            let term = terminal_expression(&a.value);
            match term {
                TypedExpr::Fn {
                    args: fn_args,
                    body,
                    ..
                } if fn_args.len() == 1 => {
                    let name = fn_args[0]
                        .get_variable_name()
                        .unwrap_or("_fork_cont")
                        .to_string();
                    Some((name, body.as_ref()))
                }
                _ => None,
            }
        } else {
            None
        }
    });

    Some((thunk_bodies, continuation))
}

/// If `expr` is an inline `fn() -> Fuzzer<T>` zero-arg thunk, return its
/// body. Otherwise `None`.
fn zero_arg_fuzzer_thunk_body(expr: &TypedExpr) -> Option<&TypedExpr> {
    let term = terminal_expression(expr);
    match term {
        TypedExpr::Fn { args, body, .. } if args.is_empty() && expression_has_fuzzer_type(body) => {
            Some(body.as_ref())
        }
        _ => None,
    }
}

/// Detect `constant`/`return`-style calls by *shape*:
///   - exactly one argument,
///   - the call's result type is `Fuzzer<T>`,
///   - the sole argument is *not* a `Fuzzer<_>` (it's the value being
///     wrapped).
///
/// Returns `Some(inner_value)` when the call matches.
/// Compare two types for semantic equality, collapsing type-variable links
/// at every level. The standard `PartialEq` for `Type` compares `Rc` pointers
/// for `Type::Var`, so two `Var` instances linked to the same concrete type
/// may compare unequal. This function resolves links before comparing.
fn types_semantically_equal(a: &Type, b: &Type) -> bool {
    use std::ops::Deref;

    // Collapse top-level links on both sides.
    let a_rc;
    let b_rc;
    let a = match a {
        Type::Var { tipo, .. } => match tipo.as_ref().borrow().deref() {
            TypeVar::Link { tipo } => {
                a_rc = tipo.clone();
                a_rc.as_ref()
            }
            _ => a,
        },
        _ => a,
    };
    let b = match b {
        Type::Var { tipo, .. } => match tipo.as_ref().borrow().deref() {
            TypeVar::Link { tipo } => {
                b_rc = tipo.clone();
                b_rc.as_ref()
            }
            _ => b,
        },
        _ => b,
    };

    match (a, b) {
        (
            Type::App {
                module: m1,
                name: n1,
                args: a1,
                ..
            },
            Type::App {
                module: m2,
                name: n2,
                args: a2,
                ..
            },
        ) => {
            m1 == m2
                && n1 == n2
                && a1.len() == a2.len()
                && a1
                    .iter()
                    .zip(a2)
                    .all(|(x, y)| types_semantically_equal(x, y))
        }
        (
            Type::Fn {
                args: a1, ret: r1, ..
            },
            Type::Fn {
                args: a2, ret: r2, ..
            },
        ) => {
            types_semantically_equal(r1, r2)
                && a1.len() == a2.len()
                && a1
                    .iter()
                    .zip(a2)
                    .all(|(x, y)| types_semantically_equal(x, y))
        }
        (Type::Tuple { elems: e1, .. }, Type::Tuple { elems: e2, .. }) => {
            e1.len() == e2.len()
                && e1
                    .iter()
                    .zip(e2)
                    .all(|(x, y)| types_semantically_equal(x, y))
        }
        (
            Type::Pair {
                fst: f1, snd: s1, ..
            },
            Type::Pair {
                fst: f2, snd: s2, ..
            },
        ) => types_semantically_equal(f1, f2) && types_semantically_equal(s1, s2),
        // Two different unresolved type variables: fall back to pointer equality.
        (Type::Var { tipo: t1, .. }, Type::Var { tipo: t2, .. }) => Rc::ptr_eq(t1, t2),
        _ => false,
    }
}

fn detect_return_call<'a>(
    _fun: &'a TypedExpr,
    args: &'a [CallArg<TypedExpr>],
    call_tipo: &Type,
) -> Option<&'a TypedExpr> {
    if args.len() != 1 {
        return None;
    }
    let inner = &args[0].value;
    // Guard: the call result must be a Fuzzer<A>.
    let payload_type = extract_fuzzer_payload_type(call_tipo)?;
    // Guard: the argument itself must not be a Fuzzer (a `bind`/`and_then`
    // would look identical to a `return` at the surface level otherwise).
    if expression_has_fuzzer_type(inner) {
        return None;
    }
    // Guard: the Fuzzer's payload type must equal the argument's type.
    // This distinguishes `return(x : A) -> Fuzzer<A>` from user-defined
    // sub-generators like `gen_inputs(st : State) -> Fuzzer<Scenario<State>>`
    // where the payload type (Scenario<State>) differs from the argument
    // type (State). Without this check, sub-generators are incorrectly
    // treated as `EqOutput(shallow_ir(arg))`.
    //
    // We use `types_semantically_equal` rather than `PartialEq` to correctly
    // handle type-variable links — `Type::Var { tipo: Link { T } }` and `T`
    // are semantically the same but compare unequal via pointer-based Eq.
    if !types_semantically_equal(payload_type.as_ref(), inner.tipo().as_ref()) {
        return None;
    }
    Some(inner)
}

fn extract_semantics_from_via_with_constants(
    via: &TypedExpr,
    current_module: &str,
    known_functions: &IndexMap<&FunctionAccessKey, &TypedFunction>,
    known_constants: &IndexMap<&FunctionAccessKey, &TypedExpr>,
    data_types: &IndexMap<&DataTypeKey, &TypedDataType>,
    output_type: &Type,
) -> FuzzerSemantics {
    let normalized = normalize_fuzzer_from_via_with_constants(
        via,
        current_module,
        known_functions,
        known_constants,
    );
    let function_index = index_known_functions(known_functions);
    let constant_index = index_known_constants(known_constants);
    let mut visiting_functions = BTreeSet::new();

    normalized_fuzzer_semantics(
        &normalized,
        current_module,
        &function_index,
        &constant_index,
        data_types,
        output_type,
        &BTreeMap::new(),
        &mut visiting_functions,
    )
}

#[cfg(test)]
fn extract_semantics_from_via(
    via: &TypedExpr,
    current_module: &str,
    known_functions: &IndexMap<&FunctionAccessKey, &TypedFunction>,
    data_types: &IndexMap<&DataTypeKey, &TypedDataType>,
    output_type: &Type,
) -> FuzzerSemantics {
    extract_semantics_from_via_with_constants(
        via,
        current_module,
        known_functions,
        &IndexMap::new(),
        data_types,
        output_type,
    )
}

#[cfg(test)]
fn semantics_from_constraint(constraint: &FuzzerConstraint, output_type: &Type) -> FuzzerSemantics {
    match constraint {
        FuzzerConstraint::Any => default_semantics_for_type(output_type, &IndexMap::new()),
        FuzzerConstraint::IntRange { min, max } => {
            if output_type.is_int() {
                FuzzerSemantics::IntRange {
                    min: Some(min.clone()),
                    max: Some(max.clone()),
                }
            } else {
                opaque_semantics(format!(
                    "integer-range constraint does not match output type '{}'",
                    describe_tipo(output_type)
                ))
            }
        }
        FuzzerConstraint::ByteStringLenRange { min_len, max_len } => {
            if output_type.is_bytearray() {
                FuzzerSemantics::ByteArrayRange {
                    min_len: Some(*min_len),
                    max_len: Some(*max_len),
                }
            } else {
                opaque_semantics(format!(
                    "bytearray-length constraint does not match output type '{}'",
                    describe_tipo(output_type)
                ))
            }
        }
        FuzzerConstraint::Exact(value) => FuzzerSemantics::Exact(value.clone()),
        FuzzerConstraint::OneOf(values) => {
            match canonicalize_finite_scalar_domain(output_type, values.clone()) {
                Ok(CanonicalFiniteScalarDomain::Exact(value)) => FuzzerSemantics::Exact(value),
                Ok(CanonicalFiniteScalarDomain::OneOf(values)) => FuzzerSemantics::OneOf(values),
                Err(_) => opaque_semantics(format!(
                    "finite scalar constraint does not match output type '{}'",
                    describe_tipo(output_type)
                )),
            }
        }
        FuzzerConstraint::Tuple(elems) => {
            let inner_types = output_type.get_inner_types();
            if !(output_type.is_tuple() || output_type.is_pair()) {
                return opaque_semantics(format!(
                    "product constraint does not match output type '{}'",
                    describe_tipo(output_type)
                ));
            }
            if inner_types.len() != elems.len() {
                return opaque_semantics(format!(
                    "product constraint arity {} does not match output type '{}'",
                    elems.len(),
                    describe_tipo(output_type)
                ));
            }
            FuzzerSemantics::Product(
                elems
                    .iter()
                    .zip(inner_types.iter())
                    .map(|(elem, inner_type)| semantics_from_constraint(elem, inner_type.as_ref()))
                    .collect(),
            )
        }
        FuzzerConstraint::List {
            elem,
            min_len,
            max_len,
        } => match output_type.get_inner_types().as_slice() {
            [element_type] if output_type.is_list() => FuzzerSemantics::List {
                element: Box::new(semantics_from_constraint(elem, element_type.as_ref())),
                min_len: *min_len,
                max_len: *max_len,
            },
            _ => opaque_semantics(format!(
                "list constraint does not match output type '{}'",
                describe_tipo(output_type)
            )),
        },
        FuzzerConstraint::DataConstructorTags { tags } => {
            FuzzerSemantics::Constructors { tags: tags.clone() }
        }
        FuzzerConstraint::Map(inner) => {
            // Propagate inner semantics through the map. The inner constraint's
            // semantics are a valid over-approximation: the map can transform values
            // but cannot widen the source domain. If the inner semantics don't match
            // the output type, semantics_from_constraint handles the mismatch by
            // producing Opaque.
            semantics_from_constraint(inner, output_type)
        }
        FuzzerConstraint::And(constraints) => {
            // Intersect compatible constraints. Collect semantics from each part.
            let inner_semantics: Vec<FuzzerSemantics> = constraints
                .iter()
                .map(|c| semantics_from_constraint(c, output_type))
                .collect();

            // Try intersecting IntRange constraints.
            let int_ranges: Vec<&FuzzerSemantics> = inner_semantics
                .iter()
                .filter(|s| matches!(s, FuzzerSemantics::IntRange { .. }))
                .collect();

            if !int_ranges.is_empty()
                && int_ranges.len()
                    == inner_semantics
                        .iter()
                        .filter(|s| !matches!(s, FuzzerSemantics::Opaque { .. }))
                        .count()
            {
                // All non-opaque constraints are IntRange — intersect them.
                let mut result_min: Option<String> = None;
                let mut result_max: Option<String> = None;
                for s in &int_ranges {
                    if let FuzzerSemantics::IntRange { min, max } = s {
                        if let Some(m) = min {
                            result_min = Some(match result_min {
                                Some(existing) => {
                                    let a: i128 = existing.parse().unwrap_or(i128::MIN);
                                    let b: i128 = m.parse().unwrap_or(i128::MIN);
                                    a.max(b).to_string()
                                }
                                None => m.clone(),
                            });
                        }
                        if let Some(m) = max {
                            result_max = Some(match result_max {
                                Some(existing) => {
                                    let a: i128 = existing.parse().unwrap_or(i128::MAX);
                                    let b: i128 = m.parse().unwrap_or(i128::MAX);
                                    a.min(b).to_string()
                                }
                                None => m.clone(),
                            });
                        }
                    }
                }
                // Guard against inverted (empty) ranges from disjoint
                // constraints, e.g. [10,20] AND [30,40] => min=30, max=20.
                if let (Some(lo), Some(hi)) = (&result_min, &result_max) {
                    if lo
                        .parse::<i128>()
                        .ok()
                        .zip(hi.parse::<i128>().ok())
                        .is_some_and(|(l, h)| l > h)
                    {
                        return default_semantics_for_type(output_type, &IndexMap::new());
                    }
                }

                return FuzzerSemantics::IntRange {
                    min: result_min,
                    max: result_max,
                };
            }

            // Fall back: take the first non-Any, non-Opaque semantics.
            for s in &inner_semantics {
                match s {
                    FuzzerSemantics::Opaque { .. } => continue,
                    FuzzerSemantics::IntRange {
                        min: None,
                        max: None,
                    } => continue,
                    FuzzerSemantics::ByteArrayRange {
                        min_len: None,
                        max_len: None,
                    } => continue,
                    other => return other.clone(),
                }
            }

            // All constraints are Any/Opaque — use default semantics.
            default_semantics_for_type(output_type, &IndexMap::new())
        }
        FuzzerConstraint::Unsupported { reason } => opaque_semantics(reason.clone()),
    }
}

type FunctionIndex<'a> = HashMap<String, HashMap<String, &'a TypedFunction>>;
type ConstantIndex<'a> = HashMap<String, HashMap<String, &'a TypedExpr>>;

#[derive(Debug, Clone)]
struct ResolvedFunction<'a> {
    module_name: String,
    function_name: String,
    function: &'a TypedFunction,
}

fn index_known_functions<'a>(
    known_functions: &'a IndexMap<&FunctionAccessKey, &TypedFunction>,
) -> FunctionIndex<'a> {
    let mut index: FunctionIndex<'a> = HashMap::new();
    for (key, function) in known_functions {
        index
            .entry(key.module_name.clone())
            .or_default()
            .insert(key.function_name.clone(), *function);
    }
    index
}

fn index_known_constants<'a>(
    known_constants: &'a IndexMap<&FunctionAccessKey, &TypedExpr>,
) -> ConstantIndex<'a> {
    let mut index: ConstantIndex<'a> = HashMap::new();
    for (key, expr) in known_constants {
        index
            .entry(key.module_name.clone())
            .or_default()
            .insert(key.function_name.clone(), *expr);
    }
    index
}

fn find_function<'a>(
    function_index: &'a FunctionIndex<'a>,
    module_name: &str,
    function_name: &str,
) -> Option<&'a TypedFunction> {
    function_index.get(module_name)?.get(function_name).copied()
}

fn pattern_var_name(pattern: &TypedPattern) -> Option<&str> {
    match pattern {
        TypedPattern::Var { name, .. } | TypedPattern::Assign { name, .. } => Some(name.as_str()),
        _ => None,
    }
}

fn collect_call_argument_values(args: &[CallArg<TypedExpr>]) -> Vec<TypedExpr> {
    args.iter().map(|arg| arg.value.clone()).collect()
}

fn make_synthetic_call_args(values: Vec<TypedExpr>) -> Vec<CallArg<TypedExpr>> {
    values
        .into_iter()
        .map(|value| CallArg {
            label: None,
            location: Span::empty(),
            value,
        })
        .collect()
}

/// Flatten a callable expression by resolving local aliases and collecting
/// arguments from partial applications.
fn flatten_call_head_and_args(
    fun: &TypedExpr,
    args: &[CallArg<TypedExpr>],
    local_values: &BTreeMap<String, TypedExpr>,
) -> Option<(TypedExpr, Vec<TypedExpr>)> {
    let mut resolved_args = collect_call_argument_values(args);
    let mut current = terminal_expression(fun).clone();
    let mut visiting_local_aliases = BTreeSet::new();

    loop {
        let terminal = terminal_expression(&current).clone();
        match terminal {
            TypedExpr::Var {
                name, constructor, ..
            } if matches!(
                constructor.variant,
                ValueConstructorVariant::LocalVariable { .. }
            ) =>
            {
                let bound_expr = local_values.get(&name)?;
                if !visiting_local_aliases.insert(name) {
                    return None;
                }
                current = bound_expr.clone();
            }
            TypedExpr::Call { fun, args, .. } => {
                let mut prefix = collect_call_argument_values(&args);
                prefix.extend(resolved_args);
                resolved_args = prefix;
                current = fun.as_ref().clone();
            }
            other => return Some((other, resolved_args)),
        }
    }
}

/// Resolve a function expression while collecting/binding any pre-applied
/// arguments from partial applications and local aliases.
fn resolve_function_with_applied_args<'a>(
    expr: &TypedExpr,
    current_module: &str,
    function_index: &'a FunctionIndex<'a>,
    local_values: &BTreeMap<String, TypedExpr>,
) -> Option<(ResolvedFunction<'a>, BTreeMap<String, TypedExpr>, usize)> {
    let (resolved_head, applied_args) = flatten_call_head_and_args(expr, &[], local_values)
        .unwrap_or_else(|| (terminal_expression(expr).clone(), Vec::new()));

    let mut visiting_local_aliases = BTreeSet::new();
    let resolved = resolve_function_from_expr(
        &resolved_head,
        current_module,
        function_index,
        local_values,
        &mut visiting_local_aliases,
    )?;

    if applied_args.len() > resolved.function.arguments.len() {
        return None;
    }

    let mut resolved_locals = local_values.clone();
    for (param, arg) in resolved.function.arguments.iter().zip(applied_args.iter()) {
        if let Some(name) = param.get_variable_name() {
            let mut visiting_local_aliases = BTreeSet::new();
            let materialized =
                materialize_local_alias_argument(arg, local_values, &mut visiting_local_aliases);
            resolved_locals.insert(name.to_string(), materialized);
        }
    }

    Some((resolved, resolved_locals, applied_args.len()))
}

fn resolve_local_var_name_with_aliases(
    expr: &TypedExpr,
    local_values: &BTreeMap<String, TypedExpr>,
    visiting_local_aliases: &mut BTreeSet<String>,
) -> Option<String> {
    let expr = terminal_expression(expr);

    let TypedExpr::Var {
        name, constructor, ..
    } = expr
    else {
        return None;
    };

    if !matches!(
        constructor.variant,
        ValueConstructorVariant::LocalVariable { .. }
    ) {
        return None;
    }

    let Some(bound_expr) = local_values.get(name) else {
        return Some(name.clone());
    };

    if !visiting_local_aliases.insert(name.clone()) {
        return None;
    }

    let resolved =
        resolve_local_var_name_with_aliases(bound_expr, local_values, visiting_local_aliases);
    visiting_local_aliases.remove(name);
    resolved
}

fn describe_expr(expr: &TypedExpr) -> String {
    match expr {
        TypedExpr::Call { .. } => "call".to_string(),
        TypedExpr::Var { name, .. } => format!("variable '{name}'"),
        TypedExpr::Fn { .. } => "function literal".to_string(),
        TypedExpr::Pipeline { .. } => "pipeline".to_string(),
        TypedExpr::Sequence { .. } => "sequence".to_string(),
        TypedExpr::ModuleSelect {
            module_name, label, ..
        } => {
            format!("module selection '{module_name}.{label}'")
        }
        _ => "expression".to_string(),
    }
}

/// H2 — produce a short, human-readable description of a `TypedPattern`
/// suitable for inclusion in an audit log entry. The shape mirrors the
/// way patterns appear in source: `Some(x)`, `Cons(head, tail)`,
/// `(a, _)`, etc. This is intentionally NOT a full pretty-printer: only
/// the constructor head and the immediate binders are surfaced.
///
/// Used by the `When → Or` widening site to record which patterns were
/// dropped (their constructor-tag conditional was widened to `True`)
/// in the per-clause `[E0033]` `unsupported_log` entry.
fn describe_pattern(pat: &TypedPattern) -> String {
    match pat {
        TypedPattern::Var { name, .. } => name.clone(),
        TypedPattern::Discard { name, .. } => name.clone(),
        TypedPattern::Int { value, .. } => value.clone(),
        TypedPattern::ByteArray { .. } => "<bytearray>".to_string(),
        TypedPattern::Assign { name, pattern, .. } => {
            format!("{} as {}", describe_pattern(pattern), name)
        }
        TypedPattern::List { elements, tail, .. } => {
            let inner: Vec<String> = elements.iter().map(describe_pattern).collect();
            match tail {
                Some(t) => format!("[{}, ..{}]", inner.join(", "), describe_pattern(t)),
                None => format!("[{}]", inner.join(", ")),
            }
        }
        TypedPattern::Constructor {
            name, arguments, ..
        } => {
            if arguments.is_empty() {
                name.clone()
            } else {
                let inner: Vec<String> = arguments
                    .iter()
                    .map(|arg| describe_pattern(&arg.value))
                    .collect();
                format!("{}({})", name, inner.join(", "))
            }
        }
        TypedPattern::Pair { fst, snd, .. } => {
            format!("Pair({}, {})", describe_pattern(fst), describe_pattern(snd))
        }
        TypedPattern::Tuple { elems, .. } => {
            let inner: Vec<String> = elems.iter().map(describe_pattern).collect();
            format!("({})", inner.join(", "))
        }
    }
}

/// H2 — recursively collect all `Var` binder names introduced by a
/// pattern. Discard, literal, and bytearray patterns introduce no
/// binders. Constructor / List / Pair / Tuple patterns recurse into
/// their sub-patterns. `Assign` (e.g. `[_, _] as the_list`) contributes
/// both the assigned name and any binders inside the inner pattern.
///
/// Used by the `When → Or` widening site to record which binders were
/// dropped (i.e. would have been threaded through the precondition if
/// constructor-conditional lowering existed) in the per-clause
/// `[E0033]` `unsupported_log` entry.
fn collect_pattern_binders(pat: &TypedPattern) -> Vec<String> {
    fn walk(pat: &TypedPattern, out: &mut Vec<String>) {
        match pat {
            TypedPattern::Var { name, .. } => out.push(name.clone()),
            TypedPattern::Discard { .. }
            | TypedPattern::Int { .. }
            | TypedPattern::ByteArray { .. } => {}
            TypedPattern::Assign { name, pattern, .. } => {
                out.push(name.clone());
                walk(pattern, out);
            }
            TypedPattern::List { elements, tail, .. } => {
                for e in elements {
                    walk(e, out);
                }
                if let Some(t) = tail {
                    walk(t, out);
                }
            }
            TypedPattern::Constructor { arguments, .. } => {
                for arg in arguments {
                    walk(&arg.value, out);
                }
            }
            TypedPattern::Pair { fst, snd, .. } => {
                walk(fst, out);
                walk(snd, out);
            }
            TypedPattern::Tuple { elems, .. } => {
                for e in elems {
                    walk(e, out);
                }
            }
        }
    }
    let mut out = Vec::new();
    walk(pat, &mut out);
    out
}

fn materialize_local_alias_argument(
    expr: &TypedExpr,
    local_values: &BTreeMap<String, TypedExpr>,
    visiting_local_aliases: &mut BTreeSet<String>,
) -> TypedExpr {
    let expr = terminal_expression(expr);

    let TypedExpr::Var {
        name, constructor, ..
    } = expr
    else {
        return expr.clone();
    };

    if !matches!(
        constructor.variant,
        ValueConstructorVariant::LocalVariable { .. }
    ) {
        return expr.clone();
    }

    let Some(bound_expr) = local_values.get(name) else {
        return expr.clone();
    };

    if !visiting_local_aliases.insert(name.clone()) {
        return expr.clone();
    }

    let resolved =
        materialize_local_alias_argument(bound_expr, local_values, visiting_local_aliases);
    visiting_local_aliases.remove(name);
    resolved
}

#[allow(clippy::only_used_in_recursion)]
fn resolve_function_from_expr<'a>(
    expr: &TypedExpr,
    current_module: &str,
    function_index: &'a FunctionIndex<'a>,
    local_values: &BTreeMap<String, TypedExpr>,
    visiting_local_aliases: &mut BTreeSet<String>,
) -> Option<ResolvedFunction<'a>> {
    match expr {
        TypedExpr::Var {
            name, constructor, ..
        } => match &constructor.variant {
            ValueConstructorVariant::ModuleFn { module, name, .. } => {
                let function = find_function(function_index, module, name)?;
                Some(ResolvedFunction {
                    module_name: module.clone(),
                    function_name: name.clone(),
                    function,
                })
            }
            ValueConstructorVariant::LocalVariable { .. } => {
                if let Some(bound_expr) = local_values.get(name) {
                    if !visiting_local_aliases.insert(name.clone()) {
                        return None;
                    }
                    let result = resolve_function_from_expr(
                        bound_expr,
                        current_module,
                        function_index,
                        local_values,
                        visiting_local_aliases,
                    );
                    visiting_local_aliases.remove(name);
                    result
                } else {
                    None
                }
            }
            _ => None,
        },
        TypedExpr::ModuleSelect {
            constructor: ModuleValueConstructor::Fn { module, name, .. },
            ..
        } => {
            let function = find_function(function_index, module, name)?;
            Some(ResolvedFunction {
                module_name: module.clone(),
                function_name: name.clone(),
                function,
            })
        }
        _ => None,
    }
}

#[cfg(test)]
fn map2_mapper_arg_order(
    mapper: &TypedExpr,
    current_module: &str,
    function_index: &FunctionIndex<'_>,
    local_values: &BTreeMap<String, TypedExpr>,
) -> Option<[usize; 2]> {
    let order = mapn_mapper_arg_order(mapper, 2, current_module, function_index, local_values)?;
    let [first, second] = order.as_slice() else {
        return None;
    };
    Some([*first, *second])
}

fn mapn_mapper_arg_order(
    mapper: &TypedExpr,
    arity: usize,
    current_module: &str,
    function_index: &FunctionIndex<'_>,
    local_values: &BTreeMap<String, TypedExpr>,
) -> Option<Vec<usize>> {
    if arity < 2 {
        return None;
    }

    let mut mapper_expr = terminal_expression(mapper).clone();
    let mut mapper_module = current_module.to_string();
    let mut mapper_locals = local_values.clone();
    let mut visiting_functions = BTreeSet::new();

    loop {
        let mapper = terminal_expression(&mapper_expr);
        match mapper {
            TypedExpr::Fn { args, body, .. } => {
                return mapn_tuple_arg_order(args, body, arity, &mapper_locals);
            }
            _ => {
                let (resolved, resolved_locals, applied_arg_count) =
                    resolve_function_with_applied_args(
                        mapper,
                        &mapper_module,
                        function_index,
                        &mapper_locals,
                    )?;
                let key = (resolved.module_name.clone(), resolved.function_name.clone());
                if !visiting_functions.insert(key) {
                    return None;
                }

                let remaining_args = resolved
                    .function
                    .arguments
                    .len()
                    .saturating_sub(applied_arg_count);

                if remaining_args == arity {
                    return mapn_tuple_arg_order(
                        &resolved.function.arguments[applied_arg_count..],
                        &resolved.function.body,
                        arity,
                        &resolved_locals,
                    );
                }

                if remaining_args == 0 {
                    mapper_expr = resolved.function.body.clone();
                    mapper_module = resolved.module_name;
                    mapper_locals = resolved_locals;
                    continue;
                }

                return None;
            }
        }
    }
}

fn mapn_tuple_arg_order(
    args: &[TypedArg],
    body: &TypedExpr,
    arity: usize,
    local_values: &BTreeMap<String, TypedExpr>,
) -> Option<Vec<usize>> {
    if args.len() != arity {
        return None;
    }
    let arg_names: Vec<String> = args
        .iter()
        .map(|arg| arg.get_variable_name().map(|name| name.to_string()))
        .collect::<Option<Vec<_>>>()?;

    let body = terminal_expression(body);
    let TypedExpr::Tuple { elems, .. } = body else {
        return None;
    };
    if elems.len() != arity {
        return None;
    }

    let mut seen = vec![false; arity];
    let mut order = Vec::with_capacity(arity);

    for elem in elems {
        let index = tuple_elem_arg_index_by_names(elem, &arg_names, local_values)?;
        if seen[index] {
            return None;
        }
        seen[index] = true;
        order.push(index);
    }

    Some(order)
}

fn tuple_elem_arg_index_by_names(
    elem: &TypedExpr,
    arg_names: &[String],
    local_values: &BTreeMap<String, TypedExpr>,
) -> Option<usize> {
    let mut visiting_local_aliases = BTreeSet::new();
    let name =
        resolve_local_var_name_with_aliases(elem, local_values, &mut visiting_local_aliases)?;

    arg_names.iter().position(|arg_name| arg_name == &name)
}

fn terminal_expression(mut expr: &TypedExpr) -> &TypedExpr {
    loop {
        match expr {
            TypedExpr::Pipeline { expressions, .. } | TypedExpr::Sequence { expressions, .. } => {
                if let Some(last) = expressions.last() {
                    expr = last;
                } else {
                    return expr;
                }
            }
            _ => return expr,
        }
    }
}

/// Collect the set of free `Var` names referenced anywhere in `expr`.
///
/// Used by `translate_bind` to gate the self-referential
/// `local_values[binder] -> source` insertion: if `source` mentions
/// `binder` (free), inserting the alias would create a self-referential
/// substitution entry that the `Var`-lookup arm of
/// `typed_expr_to_transition_prop` recurses on indefinitely
/// (stack-overflowing the lowering on legal Aiken code such as
/// `let g = some_fuzzer; and_then(g, fn(g) { g })`).
///
/// Binding-introducing variants subtract their bound names from the
/// inner walk:
///   - `Fn { args, body }` subtracts each `arg.name` from the body's
///     free-vars (a lambda parameter is locally bound).
///   - `When` clause patterns subtract the pattern's binders from the
///     clause body's free-vars.
///   - `Assignment { pattern, value }` subtracts the pattern binders
///     from any *subsequent* expressions in a `Sequence`/`Pipeline`,
///     but NOT from the `value` itself (the binding is recursive in
///     name-resolution but `let` is not let-rec in Aiken). The
///     assignment's own value still counts its free vars.
///
/// Variants without sub-expressions (literals, `ErrorTerm`,
/// `ModuleSelect`, non-`LocalVariable` `Var` like module fns / record
/// constructors) contribute no free vars.
fn free_vars_in_typed_expr(expr: &TypedExpr) -> BTreeSet<String> {
    let mut out = BTreeSet::new();
    collect_free_vars(expr, &mut out);
    out
}

fn collect_free_vars(expr: &TypedExpr, out: &mut BTreeSet<String>) {
    match expr {
        // Literals — no free vars.
        TypedExpr::UInt { .. }
        | TypedExpr::String { .. }
        | TypedExpr::ByteArray { .. }
        | TypedExpr::CurvePoint { .. }
        | TypedExpr::ErrorTerm { .. } => {}

        // Module-level references contribute no *local* free vars.
        // Only `LocalVariable` references are meaningful for the
        // self-reference check (binder names always resolve as
        // `LocalVariable`).
        TypedExpr::Var {
            name, constructor, ..
        } => {
            if matches!(
                constructor.variant,
                ValueConstructorVariant::LocalVariable { .. }
            ) {
                out.insert(name.clone());
            }
        }

        // ModuleSelect references a module-level value; no local free
        // vars.
        TypedExpr::ModuleSelect { .. } => {}

        // Sequence/Pipeline: walk every sub-expression. Assignment
        // binders introduced earlier in the sequence shadow later
        // free-var occurrences of the same name. Track shadowed names
        // and skip them when collecting from the tail.
        TypedExpr::Sequence { expressions, .. } | TypedExpr::Pipeline { expressions, .. } => {
            let mut shadowed: BTreeSet<String> = BTreeSet::new();
            for sub in expressions {
                let mut sub_free = BTreeSet::new();
                collect_free_vars(sub, &mut sub_free);
                for name in sub_free {
                    if !shadowed.contains(&name) {
                        out.insert(name);
                    }
                }
                if let TypedExpr::Assignment { pattern, .. } = sub {
                    for binder in collect_pattern_binders(pattern) {
                        shadowed.insert(binder);
                    }
                }
            }
        }

        // Lambda: subtract its parameter names from the body's free
        // vars before merging.
        TypedExpr::Fn { args, body, .. } => {
            let mut body_free = BTreeSet::new();
            collect_free_vars(body, &mut body_free);
            for arg in args {
                if let Some(name) = arg.get_variable_name() {
                    body_free.remove(name);
                }
            }
            for name in body_free {
                out.insert(name);
            }
        }

        TypedExpr::List { elements, tail, .. } => {
            for e in elements {
                collect_free_vars(e, out);
            }
            if let Some(t) = tail {
                collect_free_vars(t, out);
            }
        }

        TypedExpr::Call { fun, args, .. } => {
            collect_free_vars(fun, out);
            for a in args {
                collect_free_vars(&a.value, out);
            }
        }

        TypedExpr::BinOp { left, right, .. } => {
            collect_free_vars(left, out);
            collect_free_vars(right, out);
        }

        // Assignment value contributes its free vars; the assignment's
        // own pattern binders only affect *subsequent* expressions in
        // a sequence (handled in the Sequence/Pipeline arm above).
        TypedExpr::Assignment { value, .. } => {
            collect_free_vars(value, out);
        }

        TypedExpr::Trace { then, text, .. } => {
            collect_free_vars(then, out);
            collect_free_vars(text, out);
        }

        // When: subject contributes its free vars; each clause's body
        // is walked with the clause pattern's binders subtracted.
        TypedExpr::When {
            subject, clauses, ..
        } => {
            collect_free_vars(subject, out);
            for clause in clauses {
                let mut clause_free = BTreeSet::new();
                collect_free_vars(&clause.then, &mut clause_free);
                for binder in collect_pattern_binders(&clause.pattern) {
                    clause_free.remove(&binder);
                }
                for name in clause_free {
                    out.insert(name);
                }
            }
        }

        TypedExpr::If {
            branches,
            final_else,
            ..
        } => {
            for branch in branches.iter() {
                collect_free_vars(&branch.condition, out);
                collect_free_vars(&branch.body, out);
            }
            collect_free_vars(final_else, out);
        }

        TypedExpr::RecordAccess { record, .. } => {
            collect_free_vars(record, out);
        }

        TypedExpr::Tuple { elems, .. } => {
            for e in elems {
                collect_free_vars(e, out);
            }
        }

        TypedExpr::Pair { fst, snd, .. } => {
            collect_free_vars(fst, out);
            collect_free_vars(snd, out);
        }

        TypedExpr::TupleIndex { tuple, .. } => {
            collect_free_vars(tuple, out);
        }

        TypedExpr::RecordUpdate { spread, args, .. } => {
            collect_free_vars(spread, out);
            for a in args {
                collect_free_vars(&a.value, out);
            }
        }

        TypedExpr::UnOp { value, .. } => {
            collect_free_vars(value, out);
        }
    }
}

#[derive(Debug, Clone, thiserror::Error, miette::Diagnostic)]
#[error("Fuzzer exited unexpectedly: {uplc_error}.")]
pub struct FuzzerError {
    logs: Vec<String>,
    uplc_error: uplc::machine::Error,
}

#[derive(Debug, Clone)]
pub enum Event {
    Simplifying {
        choices: usize,
    },
    Simplified {
        #[cfg(not(target_family = "wasm"))]
        duration: Duration,
        #[cfg(target_family = "wasm")]
        duration: (),
        steps: usize,
    },
}

impl Display for Event {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::result::Result<(), std::fmt::Error> {
        match self {
            Event::Simplifying { choices } => f.write_str(&format!(
                "{} {}",
                "  Simplifying"
                    .if_supports_color(Stderr, |s| s.bold())
                    .if_supports_color(Stderr, |s| s.purple()),
                format!("counterexample from {choices} choices")
                    .if_supports_color(Stderr, |s| s.bold()),
            )),
            #[cfg(target_family = "wasm")]
            Event::Simplified { steps, .. } => f.write_str(&format!(
                "{} {}",
                "   Simplified"
                    .if_supports_color(Stderr, |s| s.bold())
                    .if_supports_color(Stderr, |s| s.purple()),
                format!("counterexample after {steps} steps",)
                    .if_supports_color(Stderr, |s| s.bold()),
            )),
            #[cfg(not(target_family = "wasm"))]
            Event::Simplified { duration, steps } => f.write_str(&format!(
                "{} {}",
                "   Simplified"
                    .if_supports_color(Stderr, |s| s.bold())
                    .if_supports_color(Stderr, |s| s.purple()),
                format!(
                    "counterexample in {} after {steps} steps",
                    if duration.as_secs() == 0 {
                        format!("{}ms", duration.as_millis())
                    } else {
                        format!("{}s", duration.as_secs())
                    }
                )
                .if_supports_color(Stderr, |s| s.bold()),
            )),
        }
    }
}

impl PropertyTest {
    fn new(
        input_path: PathBuf,
        module: String,
        name: String,
        on_test_failure: OnTestFailure,
        program: Program<Name>,
        fuzzer: Fuzzer<Name>,
    ) -> Self {
        Self {
            input_path,
            module,
            name,
            on_test_failure,
            program,
            fuzzer,
        }
    }

    pub fn from_function_definition(
        generator: &mut CodeGenerator<'_>,
        test: TypedTest,
        module_name: String,
        input_path: PathBuf,
    ) -> AnalyzedPropertyTest {
        let TypedTest {
            name,
            on_test_failure,
            body,
            arguments,
            return_type,
            ..
        } = test;

        let parameter = arguments
            .first()
            .expect("property tests must have at least one argument")
            .to_owned();

        let via = parameter.via.clone();
        let normalized = normalize_fuzzer_from_via_with_constants(
            &via,
            module_name.as_str(),
            generator.functions(),
            generator.constants(),
        );
        let constraint = extract_constraint_from_via_with_constants_and_data_types(
            &via,
            module_name.as_str(),
            generator.functions(),
            generator.constants(),
            generator.data_types(),
        );
        let type_info = parameter.arg.tipo.clone();

        let stripped_type_info = convert_opaque_type(&type_info, generator.data_types(), true);
        let semantics = extract_semantics_from_via_with_constants(
            &via,
            module_name.as_str(),
            generator.functions(),
            generator.constants(),
            generator.data_types(),
            stripped_type_info.as_ref(),
        );

        let program = generator.clone().generate_raw(
            &body,
            &[TypedArg {
                tipo: stripped_type_info.clone(),
                ..parameter.clone().into()
            }],
            &module_name,
        );

        // NOTE: We need not to pass any parameter to the fuzzer/sampler here because the fuzzer
        // argument is a Data constructor which needs not any conversion. So we can just safely
        // apply onto it later.
        let generator_program = generator.clone().generate_raw(&via, &[], &module_name);

        AnalyzedPropertyTest {
            test: PropertyTest::new(
                input_path,
                module_name,
                name,
                on_test_failure,
                program,
                Fuzzer {
                    program: generator_program,
                    stripped_type_info,
                    type_info,
                },
            ),
            analysis: PropertyTestAnalysis {
                return_type,
                fuzzer: FuzzerAnalysis {
                    normalized,
                    constraint,
                    semantics,
                },
            },
        }
    }

    pub const DEFAULT_MAX_SUCCESS: usize = 100;

    /// Run a property test from a given seed. The property is run at most DEFAULT_MAX_SUCCESS times. It
    /// may stops earlier on failure; in which case a 'counterexample' is returned.
    pub fn run(
        self,
        seed: u32,
        n: usize,
        plutus_version: &PlutusVersion,
    ) -> PropertyTestResult<PlutusData> {
        let mut labels = BTreeMap::new();
        let mut remaining = n;

        let (logs, counterexample, iterations) = match self.run_n_times(
            &mut remaining,
            Prng::from_seed(seed),
            &mut labels,
            plutus_version,
        ) {
            Ok(None) => (Vec::new(), Ok(None), n),
            Ok(Some(counterexample)) => (
                self.eval(&counterexample.value, plutus_version).logs(),
                Ok(Some(counterexample.value)),
                n - remaining,
            ),
            Err(FuzzerError { logs, uplc_error }) => (logs, Err(uplc_error), n - remaining + 1),
        };

        PropertyTestResult {
            test: self,
            counterexample,
            iterations,
            labels,
            logs,
        }
    }

    pub fn run_n_times<'a>(
        &'a self,
        remaining: &mut usize,
        initial_prng: Prng,
        labels: &mut BTreeMap<String, usize>,
        plutus_version: &'a PlutusVersion,
    ) -> Result<Option<Counterexample<'a>>, FuzzerError> {
        let mut prng = initial_prng;
        let mut counterexample = None;

        while *remaining > 0 && counterexample.is_none() {
            (prng, counterexample) = self.run_once(prng, labels, plutus_version)?;
            *remaining -= 1;
        }

        Ok(counterexample)
    }

    fn run_once<'a>(
        &'a self,
        prng: Prng,
        labels: &mut BTreeMap<String, usize>,
        plutus_version: &'a PlutusVersion,
    ) -> Result<(Prng, Option<Counterexample<'a>>), FuzzerError> {
        use OnTestFailure::*;

        let (next_prng, value) = prng
            .sample(&self.fuzzer.program)?
            .expect("A seeded PRNG returned 'None' which indicates a fuzzer is ill-formed and implemented wrongly; please contact library's authors.");

        let result = self.eval(&value, plutus_version);

        for label in result.labels() {
            // NOTE: There may be other log outputs that interefere with labels. So *by
            // convention*, we treat as label strings that starts with a NUL byte, which
            // should be a guard sufficient to prevent inadvertent clashes.
            labels
                .entry(label)
                .and_modify(|count| *count += 1)
                .or_insert(1);
        }

        let is_failure = result.failed(true, &plutus_version.into());

        let is_success = !is_failure;

        let keep_counterexample = match self.on_test_failure {
            FailImmediately | SucceedImmediately => is_failure,
            SucceedEventually => is_success,
        };

        if keep_counterexample {
            let mut counterexample = Counterexample {
                value,
                choices: next_prng.choices(),
                cache: Cache::new(|choices| {
                    match Prng::from_choices(choices).sample(&self.fuzzer.program) {
                        Err(..) => Status::Invalid,
                        Ok(None) => Status::Invalid,
                        Ok(Some((_, value))) => {
                            let is_failure = self
                                .eval(&value, plutus_version)
                                .failed(true, &plutus_version.into());

                            match self.on_test_failure {
                                FailImmediately | SucceedImmediately => {
                                    if is_failure {
                                        Status::Keep(value)
                                    } else {
                                        Status::Ignore
                                    }
                                }

                                SucceedEventually => {
                                    if is_failure {
                                        Status::Ignore
                                    } else {
                                        Status::Keep(value)
                                    }
                                }
                            }
                        }
                    }
                }),
            };

            if !counterexample.choices.is_empty() {
                counterexample.simplify();
            }

            Ok((next_prng, Some(counterexample)))
        } else {
            Ok((next_prng, None))
        }
    }

    pub fn eval(&self, value: &PlutusData, plutus_version: &PlutusVersion) -> EvalResult {
        let program = self.program.apply_data(value.clone());

        Program::<NamedDeBruijn>::try_from(program)
            .unwrap()
            .eval_version(ExBudget::max(), &plutus_version.into())
    }
}

/// ----- Benchmark -----------------------------------------------------------------

#[derive(Debug, Clone)]
pub struct Sampler<T> {
    pub program: Program<T>,

    pub type_info: Rc<Type>,

    /// A version of the Fuzzer's type that has gotten rid of
    /// all erasable opaque type. This is needed in order to
    /// generate Plutus data with the appropriate shape.
    pub stripped_type_info: Rc<Type>,
}

#[derive(Debug, Clone, thiserror::Error, miette::Diagnostic)]
pub enum BenchmarkError {
    #[error("Sampler exited unexpectedly: {uplc_error}.")]
    SamplerError {
        logs: Vec<String>,
        uplc_error: uplc::machine::Error,
    },
    #[error("Bench exited unexpectedly: {uplc_error}.")]
    BenchError {
        logs: Vec<String>,
        uplc_error: uplc::machine::Error,
    },
}

impl BenchmarkError {
    pub fn logs(&self) -> &[String] {
        match self {
            BenchmarkError::SamplerError { logs, .. } | BenchmarkError::BenchError { logs, .. } => {
                logs.as_slice()
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct Benchmark {
    pub input_path: PathBuf,
    pub module: String,
    pub name: String,
    pub on_test_failure: OnTestFailure,
    pub program: Program<Name>,
    pub sampler: Sampler<Name>,
}

unsafe impl Send for Benchmark {}

impl Benchmark {
    pub const DEFAULT_MAX_SIZE: usize = 30;

    pub fn run(
        self,
        seed: u32,
        max_size: usize,
        plutus_version: &PlutusVersion,
    ) -> BenchmarkResult {
        let mut measures = Vec::with_capacity(max_size);
        let mut prng = Prng::from_seed(seed);
        let mut error = None;
        let mut size = 0;

        while error.is_none() && max_size >= size {
            let fuzzer = self
                .sampler
                .program
                .apply_term(&Term::Constant(Constant::Integer(size.into()).into()));

            match prng.sample(&fuzzer) {
                Ok(None) => {
                    panic!(
                        "A seeded PRNG returned 'None' which indicates a sampler is ill-formed and implemented wrongly; please contact library's authors."
                    );
                }

                Ok(Some((new_prng, value))) => {
                    prng = new_prng;
                    let result = self.eval(&value, plutus_version);
                    match result.result() {
                        Ok(_) => measures.push((size, result.cost())),
                        Err(uplc_error) => {
                            error = Some(BenchmarkError::BenchError {
                                logs: result.logs(),
                                uplc_error,
                            });
                        }
                    }
                }

                Err(FuzzerError { logs, uplc_error }) => {
                    error = Some(BenchmarkError::SamplerError { logs, uplc_error });
                }
            }

            size += 1;
        }

        BenchmarkResult {
            bench: self,
            measures,
            error,
        }
    }

    pub fn eval(&self, value: &PlutusData, plutus_version: &PlutusVersion) -> EvalResult {
        let program = self.program.apply_data(value.clone());

        Program::<NamedDeBruijn>::try_from(program)
            .unwrap()
            .eval_version(ExBudget::max(), &plutus_version.into())
    }
}

/// ----- PRNG -----------------------------------------------------------------
///
/// A Pseudo-random generator (PRNG) used to produce random values for fuzzers.
/// Note that the randomness isn't actually managed by the Rust framework, it
/// entirely relies on properties of hashing algorithm on-chain (e.g. blake2b).
///
/// The PRNG can have two forms:
///
/// 1. Seeded: which occurs during the initial run of a property. Each time a
///    number is drawn from the PRNG, a new seed is created. We retain all the
///    choices drawn in a _choices_ vector.
///
/// 2. Replayed: which is used to replay a Prng sequenced from a list of known
///    choices. This happens when shrinking an example. Instead of trying to
///    shrink the value directly, we shrink the PRNG sequence with the hope that
///    it will generate a smaller value. This implies that generators tend to
///    generate smaller values when drawing smaller numbers.
///
#[derive(Debug)]
pub enum Prng {
    Seeded { choices: Vec<u8>, uplc: PlutusData },
    Replayed { choices: Vec<u8>, uplc: PlutusData },
}

impl Prng {
    /// Constructor tag for Prng's 'Seeded'
    const SEEDED: u64 = 0;
    /// Constructor tag for Prng's 'Replayed'
    const REPLAYED: u64 = 1;

    /// Constructor tag for Option's 'Some'
    const SOME: u64 = 0;
    /// Constructor tag for Option's 'None'
    const NONE: u64 = 1;

    pub fn uplc(&self) -> PlutusData {
        match self {
            Prng::Seeded { uplc, .. } => uplc.clone(),
            Prng::Replayed { uplc, .. } => uplc.clone(),
        }
    }

    pub fn choices(&self) -> Vec<u8> {
        match self {
            Prng::Seeded { choices, .. } => {
                let mut choices = choices.to_vec();
                choices.reverse();
                choices
            }
            Prng::Replayed { choices, .. } => choices.to_vec(),
        }
    }

    /// Construct a Pseudo-random number generator from a seed.
    pub fn from_seed(seed: u32) -> Prng {
        let mut digest = [0u8; 32];
        let mut context = Blake2b::new(32);
        context.input(&seed.to_be_bytes()[..]);
        context.result(&mut digest);

        Prng::Seeded {
            choices: vec![],
            uplc: Data::constr(
                Prng::SEEDED,
                vec![
                    Data::bytestring(digest.to_vec()), // Prng's seed
                    Data::bytestring(vec![]),          // Random choices
                ],
            ),
        }
    }

    /// Construct a Pseudo-random number generator from a pre-defined list of choices.
    pub fn from_choices(choices: &[u8]) -> Prng {
        Prng::Replayed {
            uplc: Data::constr(
                Prng::REPLAYED,
                vec![
                    Data::integer(choices.len().into()),
                    Data::bytestring(choices.iter().rev().cloned().collect::<Vec<_>>()),
                ],
            ),
            choices: choices.to_vec(),
        }
    }

    /// Generate a pseudo-random value from a fuzzer using the given PRNG.
    pub fn sample(
        &self,
        fuzzer: &Program<Name>,
    ) -> Result<Option<(Prng, PlutusData)>, FuzzerError> {
        let program = Program::<NamedDeBruijn>::try_from(fuzzer.apply_data(self.uplc())).unwrap();
        let result = program.eval(ExBudget::max());
        result
            .result()
            .map_err(|uplc_error| FuzzerError {
                logs: result.logs(),
                uplc_error,
            })
            .map(Prng::from_result)
    }

    /// Obtain a Prng back from a fuzzer execution. As a reminder, fuzzers have the following
    /// signature:
    ///
    /// `type Fuzzer<a> = fn(Prng) -> Option<(Prng, a)>`
    ///
    /// In nominal scenarios (i.e. when the fuzzer is made from a seed and evolve pseudo-randomly),
    /// it cannot yield 'None'. When replayed however, we can't easily guarantee that the changes
    /// made during shrinking aren't breaking underlying invariants (if only, because we run out of
    /// values to replay). In such case, the replayed sequence is simply invalid and the fuzzer
    /// aborted altogether with 'None'.
    pub fn from_result(result: Term<NamedDeBruijn>) -> Option<(Self, PlutusData)> {
        /// Interpret the given 'PlutusData' as one of two Prng constructors.
        fn as_prng(cst: &PlutusData) -> Prng {
            if let PlutusData::Constr(Constr { tag, fields, .. }) = cst {
                if *tag == 121 + Prng::SEEDED
                    && let [
                        PlutusData::BoundedBytes(bytes),
                        PlutusData::BoundedBytes(choices),
                    ] = &fields[..]
                {
                    return Prng::Seeded {
                        choices: choices.to_vec(),
                        uplc: Data::constr(
                            Prng::SEEDED,
                            vec![
                                PlutusData::BoundedBytes(bytes.to_owned()),
                                // Clear choices between seeded runs, to not
                                // accumulate ALL choices ever made.
                                PlutusData::BoundedBytes(vec![].into()),
                            ],
                        ),
                    };
                }

                if *tag == 121 + Prng::REPLAYED
                    && let [PlutusData::BigInt(..), PlutusData::BoundedBytes(choices)] = &fields[..]
                {
                    return Prng::Replayed {
                        choices: choices.to_vec(),
                        uplc: cst.clone(),
                    };
                }
            }

            unreachable!("malformed Prng: {cst:#?}")
        }

        if let Term::Constant(rc) = &result
            && let Constant::Data(PlutusData::Constr(Constr { tag, fields, .. })) = &rc.borrow()
        {
            if *tag == 121 + Prng::SOME
                && let [PlutusData::Array(elems)] = &fields[..]
                && let [new_seed, value] = &elems[..]
            {
                return Some((as_prng(new_seed), value.clone()));
            }

            // May occurs when replaying a fuzzer from a shrinked sequence of
            // choices. If we run out of choices, or a choice end up being
            // invalid as per the expectation, the fuzzer can't go further and
            // fail.
            if *tag == 121 + Prng::NONE {
                return None;
            }
        }

        unreachable!("Fuzzer yielded a malformed result? {result:#?}")
    }
}

/// ----- Counterexample -----------------------------------------------------------------
///
/// A counterexample is constructed from a test failure. It holds a value, and a sequence
/// of random choices that led to this value. It holds a reference to the underlying
/// property and fuzzer. In many cases, a counterexample can be simplified (a.k.a "shrinked")
/// into a smaller counterexample.
pub struct Counterexample<'a> {
    pub value: PlutusData,
    pub choices: Vec<u8>,
    pub cache: Cache<'a, PlutusData>,
}

impl Counterexample<'_> {
    fn consider(&mut self, choices: &[u8]) -> bool {
        if choices == self.choices {
            return true;
        }

        match self.cache.get(choices) {
            Status::Invalid | Status::Ignore => false,
            Status::Keep(value) => {
                // If these new choices are shorter or smaller, then we pick them
                // as new choices and inform that it's been an improvement.
                if choices.len() <= self.choices.len() || choices < &self.choices[..] {
                    self.value = value;
                    self.choices = choices.to_vec();
                    true
                } else {
                    false
                }
            }
        }
    }

    /// Try to simplify a 'Counterexample' by manipulating the random sequence of generated values
    /// (a.k.a. choices). While the implementation is quite involved, the strategy is rather simple
    /// at least conceptually:
    ///
    /// Each time a (seeded) fuzzer generates a new value and a new seed, it also stores the
    /// generated value in a vector, which we call 'choices'. If we re-run the test case with this
    /// exact choice sequence, we end up with the exact same outcome.
    ///
    /// But, we can tweak chunks of this sequence in hope to generate a _smaller sequence_, thus
    /// generally resulting in a _smaller counterexample_. Each transformations is applied on
    /// chunks of size 8, 4, 2 and 1; until we no longer make progress (i.e. hit a fix point).
    ///
    /// As per MiniThesis, we consider the following transformations:
    ///
    /// - Deleting chunks
    /// - Transforming chunks into sequence of zeroes
    /// - Replacing chunks of values with smaller values
    /// - Sorting chunks in ascending order
    /// - Swapping nearby pairs
    /// - Redistributing values between nearby pairs
    pub fn simplify(&mut self) {
        let mut prev;

        let mut steps = 0;

        #[cfg(not(target_family = "wasm"))]
        let now = std::time::Instant::now();

        eprintln!(
            "{}",
            Event::Simplifying {
                choices: self.choices.len(),
            }
        );

        loop {
            prev = self.choices.clone();

            // First try deleting each choice we made in chunks. We try longer chunks because this
            // allows us to delete whole composite elements: e.g. deleting an element from a
            // generated list requires us to delete both the choice of whether to include it and
            // also the element itself, which may involve more than one choice.
            let mut k = 8;
            while k > 0 {
                let (mut i, mut underflow) = if self.choices.len() < k {
                    (0, true)
                } else {
                    (self.choices.len() - k, false)
                };

                while !underflow {
                    if i >= self.choices.len() {
                        (i, underflow) = i.overflowing_sub(1);
                        steps += 1;
                        continue;
                    }

                    let j = i + k;

                    let mut choices = [
                        &self.choices[..i],
                        if j < self.choices.len() {
                            &self.choices[j..]
                        } else {
                            &[]
                        },
                    ]
                    .concat();

                    if !self.consider(&choices) {
                        // Perform an extra reduction step that decrease the size of choices near
                        // the end, to cope with dependencies between choices, e.g. drawing a
                        // number as a list length, and then drawing that many elements.
                        //
                        // This isn't perfect, but allows to make progresses in many cases.
                        if i > 0 && choices[i - 1] > 0 {
                            choices[i - 1] -= 1;
                            if self.consider(&choices) {
                                i += 1;
                            };
                        }

                        (i, underflow) = i.overflowing_sub(1);
                    }

                    steps += 1;
                }

                k /= 2
            }

            if !self.choices.is_empty() {
                // Now we try replacing region of choices with zeroes. Note that unlike the above we
                // skip k = 1 because we handle that in the next step. Often (but not always) a block
                // of all zeroes is the smallest value that a region can be.
                let mut k = 8;
                while k > 1 {
                    let mut i = self.choices.len();
                    while i >= k {
                        steps += 1;
                        let ivs = (i - k..i).map(|j| (j, 0)).collect::<Vec<_>>();
                        i -= if self.replace(ivs) { k } else { 1 }
                    }
                    k /= 2
                }

                // Replace choices with smaller value, by doing a binary search. This will replace n
                // with 0 or n - 1, if possible, but will also more efficiently replace it with, a
                // smaller number than doing multiple subtractions would.
                let (mut i, mut underflow) = (self.choices.len() - 1, false);
                while !underflow {
                    steps += 1;
                    self.binary_search_replace(0, self.choices[i], |v| vec![(i, v)]);
                    (i, underflow) = i.overflowing_sub(1);
                }

                // Sort out of orders chunks in ascending order
                let mut k = 8;
                while k > 1 {
                    let mut i = self.choices.len() - 1;
                    while i >= k {
                        steps += 1;
                        let (from, to) = (i - k, i);
                        self.replace(
                            (from..to)
                                .zip(self.choices[from..to].iter().cloned().sorted())
                                .collect(),
                        );
                        i -= 1;
                    }
                    k /= 2
                }

                // Try adjusting nearby pairs by:
                //
                // - Swapping them if they are out-of-order
                // - Redistributing values between them.
                for k in [2, 1] {
                    let mut j = self.choices.len() - 1;
                    while j >= k {
                        let i = j - k;

                        // Swap
                        if self.choices[i] > self.choices[j] {
                            self.replace(vec![(i, self.choices[j]), (j, self.choices[i])]);
                        }

                        let iv = self.choices[i];
                        let jv = self.choices[j];

                        // Replace
                        if iv > 0 && jv <= u8::MAX - iv {
                            self.binary_search_replace(0, iv, |v| vec![(i, v), (j, jv + (iv - v))]);
                        }

                        steps += 1;

                        j -= 1
                    }
                }
            }

            // If we've reached a fixed point, then we cannot shrink further. We've reached a
            // (local) minimum, which is as good as a counterexample we'll get with this approach.
            if prev.as_slice() == self.choices.as_slice() {
                break;
            }
        }

        eprintln!(
            "{}",
            Event::Simplified {
                #[cfg(not(target_family = "wasm"))]
                duration: now.elapsed(),
                #[cfg(target_family = "wasm")]
                duration: (),
                steps,
            }
        );
    }

    /// Try to replace a value with a smaller value by doing a binary search between
    /// two extremes. This converges relatively fast in order to shrink down values.
    fn binary_search_replace<F>(&mut self, lo: u8, hi: u8, f: F) -> u8
    where
        F: Fn(u8) -> Vec<(usize, u8)>,
    {
        if self.replace(f(lo)) {
            return lo;
        }

        let mut lo = lo;
        let mut hi = hi;

        while lo + 1 < hi {
            let mid = lo + (hi - lo) / 2;
            if self.replace(f(mid)) {
                hi = mid;
            } else {
                lo = mid;
            }
        }

        hi
    }

    // Replace values in the choices vector, based on the index-value list provided
    // and consider the resulting choices.
    fn replace(&mut self, ivs: Vec<(usize, u8)>) -> bool {
        let mut choices = self.choices.clone();

        for (i, v) in ivs {
            if i >= choices.len() {
                return false;
            }
            choices[i] = v;
        }

        self.consider(&choices)
    }
}

/// ----- Cache -----------------------------------------------------------------------
///
/// A simple cache as a Patricia-trie to look for already explored options. The simplification
/// steps does often generate the same paths and the generation of new test values as well as the
/// properties can take a significant time.
///
/// Yet, sequences have interesting properties:
///
/// 1. The generation and test execution is entirely deterministic.
///
///
pub struct Cache<'a, T> {
    db: PatriciaMap<Status<T>>,
    #[allow(clippy::type_complexity)]
    run: Box<dyn Fn(&[u8]) -> Status<T> + 'a>,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Status<T> {
    Keep(T),
    Ignore,
    Invalid,
}

impl<'a, T> Cache<'a, T>
where
    T: PartialEq + Clone,
{
    pub fn new<F>(run: F) -> Cache<'a, T>
    where
        F: Fn(&[u8]) -> Status<T> + 'a,
    {
        Cache {
            db: PatriciaMap::new(),
            run: Box::new(run),
        }
    }

    pub fn size(&self) -> usize {
        self.db.len()
    }

    pub fn get(&mut self, choices: &[u8]) -> Status<T> {
        if let Some((prefix, status)) = self.db.get_longest_common_prefix(choices) {
            let status = status.clone();
            if status != Status::Invalid || prefix == choices {
                return status;
            }
        }

        let status = self.run.deref()(choices);

        // Clear longer path on non-invalid cases, as we will never reach them
        // again due to a now-shorter prefix found.
        //
        // This hopefully keeps the cache under a reasonable size as we prune
        // the tree as we discover shorter paths.
        if status != Status::Invalid {
            let keys = self
                .db
                .iter_prefix(choices)
                .map(|(k, _)| k)
                .collect::<Vec<_>>();
            for k in keys {
                self.db.remove(k);
            }
        }

        self.db.insert(choices, status.clone());

        status
    }
}

// ----------------------------------------------------------------------------
//
// TestResult
//
// ----------------------------------------------------------------------------

// Public enum; boxing result variants would ripple through telemetry and error callsites.
#[allow(clippy::large_enum_variant)]
#[derive(Debug, Clone)]
pub enum TestResult<U, T> {
    UnitTestResult(UnitTestResult<U>),
    PropertyTestResult(PropertyTestResult<T>),
    BenchmarkResult(BenchmarkResult),
}

unsafe impl<U, T> Send for TestResult<U, T> {}

impl TestResult<(Constant, Rc<Type>), PlutusData> {
    pub fn reify(
        self,
        data_types: &IndexMap<&DataTypeKey, &TypedDataType>,
    ) -> TestResult<UntypedExpr, UntypedExpr> {
        match self {
            TestResult::UnitTestResult(test) => TestResult::UnitTestResult(test.reify(data_types)),
            TestResult::PropertyTestResult(test) => {
                TestResult::PropertyTestResult(test.reify(data_types))
            }
            TestResult::BenchmarkResult(result) => TestResult::BenchmarkResult(result),
        }
    }
}

impl<U, T> TestResult<U, T> {
    pub fn is_success(&self) -> bool {
        match self {
            TestResult::UnitTestResult(UnitTestResult { success, .. }) => *success,
            TestResult::PropertyTestResult(PropertyTestResult {
                counterexample: Err(..),
                ..
            }) => false,
            TestResult::PropertyTestResult(PropertyTestResult {
                counterexample: Ok(counterexample),
                test,
                ..
            }) => match test.on_test_failure {
                OnTestFailure::FailImmediately | OnTestFailure::SucceedEventually => {
                    counterexample.is_none()
                }
                OnTestFailure::SucceedImmediately => counterexample.is_some(),
            },
            TestResult::BenchmarkResult(BenchmarkResult { error, .. }) => error.is_none(),
        }
    }

    pub fn module(&self) -> &str {
        match self {
            TestResult::UnitTestResult(UnitTestResult { test, .. }) => test.module.as_str(),
            TestResult::PropertyTestResult(PropertyTestResult { test, .. }) => test.module.as_str(),
            TestResult::BenchmarkResult(BenchmarkResult { bench, .. }) => bench.module.as_str(),
        }
    }

    pub fn title(&self) -> &str {
        match self {
            TestResult::UnitTestResult(UnitTestResult { test, .. }) => test.name.as_str(),
            TestResult::PropertyTestResult(PropertyTestResult { test, .. }) => test.name.as_str(),
            TestResult::BenchmarkResult(BenchmarkResult { bench, .. }) => bench.name.as_str(),
        }
    }

    pub fn logs(&self) -> &[String] {
        match self {
            TestResult::UnitTestResult(UnitTestResult { logs, .. })
            | TestResult::PropertyTestResult(PropertyTestResult { logs, .. }) => logs,
            TestResult::BenchmarkResult(BenchmarkResult { error, .. }) => {
                error.as_ref().map(|e| e.logs()).unwrap_or_default()
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct UnitTestResult<T> {
    pub success: bool,
    pub spent_budget: ExBudget,
    pub logs: Vec<String>,
    pub test: UnitTest,
    pub assertion: Option<Assertion<T>>,
}

unsafe impl<T> Send for UnitTestResult<T> {}

impl UnitTestResult<(Constant, Rc<Type>)> {
    pub fn reify(
        self,
        data_types: &IndexMap<&DataTypeKey, &TypedDataType>,
    ) -> UnitTestResult<UntypedExpr> {
        UnitTestResult {
            success: self.success,
            spent_budget: self.spent_budget,
            logs: self.logs,
            test: self.test,
            assertion: self.assertion.and_then(|assertion| {
                // No need to spend time/cpu on reifying assertions for successful
                // tests since they aren't shown.
                if self.success {
                    return None;
                }

                Some(Assertion {
                    bin_op: assertion.bin_op,
                    head: assertion.head.map(|(cst, tipo)| {
                        UntypedExpr::reify_constant(data_types, cst, tipo)
                            .expect("failed to reify assertion operand?")
                    }),
                    tail: assertion.tail.map(|xs| {
                        xs.mapped(|(cst, tipo)| {
                            UntypedExpr::reify_constant(data_types, cst, tipo)
                                .expect("failed to reify assertion operand?")
                        })
                    }),
                })
            }),
        }
    }
}

#[derive(Debug, Clone)]
pub struct PropertyTestResult<T> {
    pub test: PropertyTest,
    pub counterexample: Result<Option<T>, uplc::machine::Error>,
    pub iterations: usize,
    pub labels: BTreeMap<String, usize>,
    pub logs: Vec<String>,
}

unsafe impl<T> Send for PropertyTestResult<T> {}

impl PropertyTestResult<PlutusData> {
    pub fn reify(
        self,
        data_types: &IndexMap<&DataTypeKey, &TypedDataType>,
    ) -> PropertyTestResult<UntypedExpr> {
        PropertyTestResult {
            counterexample: self.counterexample.map(|ok| {
                ok.map(|counterexample| {
                    UntypedExpr::reify_data(
                        data_types,
                        counterexample,
                        self.test.fuzzer.type_info.clone(),
                    )
                    .expect("failed to reify counterexample?")
                })
            }),
            iterations: self.iterations,
            test: self.test,
            labels: self.labels,
            logs: self.logs,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Assertion<T> {
    pub bin_op: BinOp,
    pub head: Result<T, ()>,
    pub tail: Result<Vec1<T>, ()>,
}

impl TryFrom<TypedExpr> for Assertion<TypedExpr> {
    type Error = ();

    fn try_from(body: TypedExpr) -> Result<Self, Self::Error> {
        match body {
            TypedExpr::BinOp {
                name,
                tipo,
                left,
                right,
                ..
            } if tipo == Type::bool() => {
                // 'and' and 'or' are left-associative operators.
                match (*right).clone().try_into() {
                    Ok(Assertion {
                        bin_op,
                        head: Ok(head),
                        tail: Ok(tail),
                        ..
                    }) if bin_op == name => {
                        let mut both = vec1![head];
                        both.extend(tail);
                        Ok(Assertion {
                            bin_op: name,
                            head: Ok(*left),
                            tail: Ok(both),
                        })
                    }
                    _ => Ok(Assertion {
                        bin_op: name,
                        head: Ok(*left),
                        tail: Ok(vec1![*right]),
                    }),
                }
            }

            // NOTE drill through trace-if-false operators for better errors.
            TypedExpr::If {
                branches,
                final_else,
                ..
            } => {
                if let [
                    IfBranch {
                        condition, body, ..
                    },
                ] = &branches[..]
                {
                    let then_is_true = match body {
                        TypedExpr::Var {
                            name, constructor, ..
                        } => name == "True" && constructor.tipo == Type::bool(),
                        _ => false,
                    };

                    let else_is_wrapped_false = match *final_else {
                        TypedExpr::Trace { then, .. } => match *then {
                            TypedExpr::Var {
                                name, constructor, ..
                            } => name == "False" && constructor.tipo == Type::bool(),
                            _ => false,
                        },
                        _ => false,
                    };

                    if then_is_true && else_is_wrapped_false {
                        return condition.to_owned().try_into();
                    }
                }

                Err(())
            }

            TypedExpr::Trace { then, .. } => (*then).try_into(),

            TypedExpr::Sequence { expressions, .. } | TypedExpr::Pipeline { expressions, .. } => {
                if let Ok(Assertion {
                    bin_op,
                    head: Ok(head),
                    tail: Ok(tail),
                }) = expressions.last().unwrap().to_owned().try_into()
                {
                    let replace = |expr| {
                        let mut expressions = expressions.clone();
                        expressions.pop();
                        expressions.push(expr);
                        TypedExpr::Sequence {
                            expressions,
                            location: Span::empty(),
                        }
                    };

                    Ok(Assertion {
                        bin_op,
                        head: Ok(replace(head)),
                        tail: Ok(tail.mapped(replace)),
                    })
                } else {
                    Err(())
                }
            }

            TypedExpr::Call {
                args,
                location,
                fun,
                ..
            } => {
                // Unwind backpassing if any, or calls to function that contain binary ops.
                if let Some((last_arg, first_args)) = args.split_last()
                    && let TypedExpr::Fn {
                        body: last_arg_body,
                        location: last_arg_location,
                        tipo: last_arg_tipo,
                        is_capture: last_arg_is_capture,
                        return_annotation: last_arg_return_annotation,
                        args: last_arg_args,
                    } = &last_arg.value
                {
                    let Assertion { bin_op, head, tail } = Self::try_from(*last_arg_body.clone())?;

                    let new_callback_tipo = |body: &TypedExpr| -> Rc<Type> {
                        match last_arg_tipo.as_ref() {
                            Type::Fn {
                                args,
                                ret: _ret,
                                alias,
                            } => Rc::new(Type::Fn {
                                args: args.clone(),
                                ret: body.tipo(), // Replace the return type to match head.
                                alias: alias.clone(),
                            }),
                            Type::App { .. }
                            | Type::Var { .. }
                            | Type::Pair { .. }
                            | Type::Tuple { .. } => {
                                unreachable!(
                                    "guard above on 'last_arg.value' guarantees that type is necessarily a function (Fn)"
                                )
                            }
                        }
                    };

                    let new_fun = |body: &TypedExpr, callback_tipo: Rc<Type>| -> Box<TypedExpr> {
                        let fun_tipo = match fun.as_ref().tipo().as_ref() {
                            Type::Fn {
                                args,
                                alias,
                                ret: _,
                            } => {
                                let mut args = args
                                    .split_last()
                                    .expect("function has at least one arg")
                                    .1
                                    .to_vec();
                                args.push(callback_tipo); // Replace last callback argument
                                Rc::new(Type::Fn {
                                    args,
                                    ret: body.tipo(), // Replace overall return type
                                    alias: alias.clone(),
                                })
                            }
                            Type::App { .. }
                            | Type::Var { .. }
                            | Type::Pair { .. }
                            | Type::Tuple { .. } => {
                                unreachable!(
                                    "guard above on 'last_arg.value' guarantees that type is necessarily a function (Fn)"
                                )
                            }
                        };

                        let mut fun = fun.clone();
                        fun.replace_type(fun_tipo);

                        fun
                    };

                    let new_args =
                        |body: TypedExpr, callback_tipo: Rc<Type>| -> Vec<CallArg<TypedExpr>> {
                            let mut args = first_args.to_vec();
                            args.push(CallArg {
                                label: last_arg.label.clone(),
                                location: last_arg.location,
                                value: TypedExpr::Fn {
                                    location: *last_arg_location,
                                    tipo: callback_tipo.clone(),
                                    is_capture: *last_arg_is_capture,
                                    return_annotation: last_arg_return_annotation.clone(),
                                    args: last_arg_args.clone(),
                                    body: Box::new(body),
                                },
                            });
                            args
                        };

                    return Ok(Assertion {
                        bin_op,
                        head: head.map(|body| {
                            let callback_tipo = new_callback_tipo(&body);
                            TypedExpr::Call {
                                location,
                                tipo: body.tipo(),
                                fun: new_fun(&body, callback_tipo.clone()),
                                args: new_args(body, callback_tipo.clone()),
                            }
                        }),
                        tail: tail.map(|tail| {
                            tail.mapped(|body| {
                                let callback_tipo = new_callback_tipo(&body);
                                TypedExpr::Call {
                                    location,
                                    tipo: body.tipo(),
                                    fun: new_fun(&body, callback_tipo.clone()),
                                    args: new_args(body, callback_tipo.clone()),
                                }
                            })
                        }),
                    });
                }
                Err(())
            }

            _ => Err(()),
        }
    }
}

pub struct AssertionStyleOptions<'a> {
    red: Box<dyn Fn(String) -> String + 'a>,
    bold: Box<dyn Fn(String) -> String + 'a>,
}

impl<'a> AssertionStyleOptions<'a> {
    pub fn new(stream: Option<&'a Stream>) -> Self {
        match stream {
            Some(stream) => Self {
                red: Box::new(|s| {
                    s.if_supports_color(stream.to_owned(), |s| s.red())
                        .to_string()
                }),
                bold: Box::new(|s| {
                    s.if_supports_color(stream.to_owned(), |s| s.bold())
                        .to_string()
                }),
            },
            None => Self {
                red: Box::new(|s| s),
                bold: Box::new(|s| s),
            },
        }
    }
}

impl Assertion<UntypedExpr> {
    #[allow(clippy::just_underscores_and_digits)]
    pub fn to_string(&self, expect_failure: bool, style: &AssertionStyleOptions) -> String {
        let red = |s: &str| style.red.as_ref()(s.to_string());
        let x = |s: &str| style.red.as_ref()(style.bold.as_ref()(format!("× {s}")));

        // head did not map to a constant
        if self.head.is_err() {
            return x("program failed");
        }

        // any value in tail did not map to a constant
        if self.tail.is_err() {
            return x("program failed");
        }

        fn fmt_side(side: &UntypedExpr, red: &dyn Fn(&str) -> String) -> String {
            let __ = red("│");

            Formatter::new()
                .expr(side, false)
                .to_pretty_string(60)
                .lines()
                .map(|line| format!("{__} {line}"))
                .collect::<Vec<String>>()
                .join("\n")
        }

        let left = fmt_side(self.head.as_ref().unwrap(), &red);

        let tail = self.tail.as_ref().unwrap();

        let right = fmt_side(tail.first(), &red);

        format!(
            "{}{}{}",
            x("expected"),
            if expect_failure && self.bin_op == BinOp::Or {
                x(" neither\n")
            } else {
                "\n".to_string()
            },
            if expect_failure {
                match self.bin_op {
                    BinOp::And => [
                        left,
                        x("and"),
                        [
                            tail.mapped_ref(|s| fmt_side(s, &red))
                                .join(format!("\n{}\n", x("and")).as_str()),
                            if tail.len() > 1 {
                                x("to not all be true")
                            } else {
                                x("to not both be true")
                            },
                        ]
                        .join("\n"),
                    ],
                    BinOp::Or => [
                        left,
                        x("nor"),
                        [
                            tail.mapped_ref(|s| fmt_side(s, &red))
                                .join(format!("\n{}\n", x("nor")).as_str()),
                            x("to be true"),
                        ]
                        .join("\n"),
                    ],
                    BinOp::Eq => [left, x("to not equal"), right],
                    BinOp::NotEq => [left, x("to not be different"), right],
                    BinOp::LtInt => [left, x("to not be lower than"), right],
                    BinOp::LtEqInt => [left, x("to not be lower than or equal to"), right],
                    BinOp::GtInt => [left, x("to not be greater than"), right],
                    BinOp::GtEqInt => [left, x("to not be greater than or equal to"), right],
                    _ => unreachable!("unexpected non-boolean binary operator in assertion?"),
                }
                .join("\n")
            } else {
                match self.bin_op {
                    BinOp::And => [
                        left,
                        x("and"),
                        [
                            tail.mapped_ref(|s| fmt_side(s, &red))
                                .join(format!("\n{}\n", x("and")).as_str()),
                            if tail.len() > 1 {
                                x("to all be true")
                            } else {
                                x("to both be true")
                            },
                        ]
                        .join("\n"),
                    ],
                    BinOp::Or => [
                        left,
                        x("or"),
                        [
                            tail.mapped_ref(|s| fmt_side(s, &red))
                                .join(format!("\n{}\n", x("or")).as_str()),
                            x("to be true"),
                        ]
                        .join("\n"),
                    ],
                    BinOp::Eq => [left, x("to equal"), right],
                    BinOp::NotEq => [left, x("to not equal"), right],
                    BinOp::LtInt => [left, x("to be lower than"), right],
                    BinOp::LtEqInt => [left, x("to be lower than or equal to"), right],
                    BinOp::GtInt => [left, x("to be greater than"), right],
                    BinOp::GtEqInt => [left, x("to be greater than or equal to"), right],
                    _ => unreachable!("unexpected non-boolean binary operator in assertion?"),
                }
                .join("\n")
            }
        )
    }
}

#[derive(Debug, Clone)]
pub struct BenchmarkResult {
    pub bench: Benchmark,
    pub measures: Vec<(usize, ExBudget)>,
    pub error: Option<BenchmarkError>,
}

unsafe impl Send for BenchmarkResult {}
unsafe impl Sync for BenchmarkResult {}

#[cfg(test)]
mod test {
    use super::*;
    use crate::ast::{CallArg, TypedClause};
    use crate::parser::token::Base;
    use crate::tipo::{ValueConstructor, ValueConstructorVariant};

    fn local_var(name: &str, tipo: Rc<Type>) -> TypedExpr {
        TypedExpr::Var {
            location: Span::empty(),
            constructor: ValueConstructor::public(
                tipo.clone(),
                ValueConstructorVariant::LocalVariable {
                    location: Span::empty(),
                },
            ),
            name: name.to_string(),
        }
    }

    fn module_fn_var(name: &str, module: &str, tipo: Rc<Type>) -> TypedExpr {
        TypedExpr::Var {
            location: Span::empty(),
            constructor: ValueConstructor::public(
                tipo.clone(),
                ValueConstructorVariant::ModuleFn {
                    name: name.to_string(),
                    field_map: None,
                    module: module.to_string(),
                    arity: 0,
                    location: Span::empty(),
                    builtin: None,
                },
            ),
            name: name.to_string(),
        }
    }

    fn module_const_var(name: &str, module: &str, tipo: Rc<Type>) -> TypedExpr {
        TypedExpr::Var {
            location: Span::empty(),
            constructor: ValueConstructor::public(
                tipo.clone(),
                ValueConstructorVariant::ModuleConstant {
                    location: Span::empty(),
                    module: module.to_string(),
                    name: name.to_string(),
                },
            ),
            name: name.to_string(),
        }
    }

    fn fuzz_var(name: &str, tipo: Rc<Type>) -> TypedExpr {
        module_fn_var(name, STDLIB_FUZZ_MODULE, tipo)
    }

    fn make_map2_mapper(elems: Vec<TypedExpr>) -> TypedExpr {
        let int_tipo = Type::int();
        let tuple_tipo = Type::tuple(vec![int_tipo.clone(), int_tipo.clone()]);

        TypedExpr::Fn {
            location: Span::empty(),
            tipo: Type::function(vec![int_tipo.clone(), int_tipo.clone()], tuple_tipo.clone()),
            is_capture: false,
            args: vec![
                TypedArg::new("a", int_tipo.clone()),
                TypedArg::new("b", int_tipo),
            ],
            body: Box::new(TypedExpr::Tuple {
                location: Span::empty(),
                tipo: tuple_tipo,
                elems,
            }),
            return_annotation: None,
        }
    }

    fn make_mapn_mapper(arg_names: &[String], elems: Vec<TypedExpr>) -> TypedExpr {
        let int_tipo = Type::int();
        let tuple_tipo = Type::tuple(vec![int_tipo.clone(); arg_names.len()]);

        TypedExpr::Fn {
            location: Span::empty(),
            tipo: Type::function(vec![int_tipo.clone(); arg_names.len()], tuple_tipo.clone()),
            is_capture: false,
            args: arg_names
                .iter()
                .map(|name| TypedArg::new(name, int_tipo.clone()))
                .collect(),
            body: Box::new(TypedExpr::Tuple {
                location: Span::empty(),
                tipo: tuple_tipo,
                elems,
            }),
            return_annotation: None,
        }
    }

    fn map2_mapper_tipo() -> Rc<Type> {
        let int_tipo = Type::int();
        Type::function(
            vec![int_tipo.clone(), int_tipo.clone()],
            Type::tuple(vec![int_tipo.clone(), int_tipo]),
        )
    }

    fn make_named_map2_mapper(name: &str) -> TypedExpr {
        module_fn_var(name, "math", map2_mapper_tipo())
    }

    fn make_named_map2_mapper_function(
        name: &str,
        elems: Vec<TypedExpr>,
    ) -> (FunctionAccessKey, TypedFunction) {
        let int_tipo = Type::int();
        let tuple_tipo = Type::tuple(vec![int_tipo.clone(), int_tipo.clone()]);

        (
            FunctionAccessKey {
                module_name: "math".to_string(),
                function_name: name.to_string(),
            },
            TypedFunction {
                arguments: vec![
                    TypedArg::new("a", int_tipo.clone()),
                    TypedArg::new("b", int_tipo.clone()),
                ],
                body: TypedExpr::Tuple {
                    location: Span::empty(),
                    tipo: tuple_tipo.clone(),
                    elems,
                },
                doc: None,
                location: Span::empty(),
                name: name.to_string(),
                public: false,
                return_annotation: None,
                return_type: tuple_tipo,
                end_position: 0,
                on_test_failure: OnTestFailure::FailImmediately,
            },
        )
    }

    fn uint_lit(value: &str) -> TypedExpr {
        TypedExpr::UInt {
            location: Span::empty(),
            tipo: Type::int(),
            value: value.to_string(),
            base: Base::Decimal {
                numeric_underscore: false,
            },
        }
    }

    fn call_arg(value: TypedExpr) -> CallArg<TypedExpr> {
        CallArg {
            label: None,
            location: Span::empty(),
            value,
        }
    }

    fn make_int_between_via(min: &str, max: &str) -> TypedExpr {
        TypedExpr::Call {
            location: Span::empty(),
            tipo: Type::int(),
            fun: Box::new(fuzz_var(
                "int_between",
                Type::function(vec![Type::int(), Type::int()], Type::int()),
            )),
            args: vec![call_arg(uint_lit(min)), call_arg(uint_lit(max))],
        }
    }

    fn make_map2_via(fuzzer_a: TypedExpr, fuzzer_b: TypedExpr, mapper: TypedExpr) -> TypedExpr {
        TypedExpr::Call {
            location: Span::empty(),
            tipo: Type::tuple(vec![Type::int(), Type::int()]),
            fun: Box::new(fuzz_var(
                "map2",
                Type::function(
                    vec![Type::int(), Type::int(), Type::int()],
                    Type::tuple(vec![Type::int(), Type::int()]),
                ),
            )),
            args: vec![call_arg(fuzzer_a), call_arg(fuzzer_b), call_arg(mapper)],
        }
    }

    fn make_mapn_via(map_name: &str, fuzzers: Vec<TypedExpr>, mapper: TypedExpr) -> TypedExpr {
        let tuple_tipo = Type::tuple(vec![Type::int(); fuzzers.len()]);
        let mut args: Vec<CallArg<TypedExpr>> = fuzzers.into_iter().map(call_arg).collect();
        args.push(call_arg(mapper));

        TypedExpr::Call {
            location: Span::empty(),
            tipo: tuple_tipo.clone(),
            fun: Box::new(fuzz_var(
                map_name,
                Type::function(vec![Type::int(); args.len()], tuple_tipo),
            )),
            args,
        }
    }

    fn make_map_via(fuzzer_a: TypedExpr, mapper: TypedExpr) -> TypedExpr {
        TypedExpr::Call {
            location: Span::empty(),
            tipo: Type::int(),
            fun: Box::new(fuzz_var(
                "map",
                Type::function(
                    vec![
                        Type::function(vec![Type::int()], Type::int()),
                        Type::function(vec![Type::int()], Type::int()),
                    ],
                    Type::function(vec![Type::int()], Type::int()),
                ),
            )),
            args: vec![call_arg(fuzzer_a), call_arg(mapper)],
        }
    }

    fn make_and_then_via(
        input: TypedExpr,
        continuation: TypedExpr,
        return_type: Rc<Type>,
    ) -> TypedExpr {
        TypedExpr::Call {
            location: Span::empty(),
            tipo: return_type.clone(),
            fun: Box::new(fuzz_var(
                "and_then",
                Type::function(
                    vec![
                        Type::int(),
                        Type::function(vec![Type::int()], return_type.clone()),
                    ],
                    return_type,
                ),
            )),
            args: vec![call_arg(input), call_arg(continuation)],
        }
    }

    fn make_tuple4_via(
        fuzzer_a: TypedExpr,
        fuzzer_b: TypedExpr,
        fuzzer_c: TypedExpr,
        fuzzer_d: TypedExpr,
    ) -> TypedExpr {
        TypedExpr::Call {
            location: Span::empty(),
            tipo: Type::tuple(vec![Type::int(), Type::int(), Type::int(), Type::int()]),
            fun: Box::new(fuzz_var(
                "tuple4",
                Type::function(
                    vec![Type::int(), Type::int(), Type::int(), Type::int()],
                    Type::tuple(vec![Type::int(), Type::int(), Type::int(), Type::int()]),
                ),
            )),
            args: vec![
                call_arg(fuzzer_a),
                call_arg(fuzzer_b),
                call_arg(fuzzer_c),
                call_arg(fuzzer_d),
            ],
        }
    }

    fn negate_expr(value: TypedExpr) -> TypedExpr {
        TypedExpr::UnOp {
            location: Span::empty(),
            value: Box::new(value),
            tipo: Type::int(),
            op: UnOp::Negate,
        }
    }

    fn make_named_unary_negate_mapper_function(name: &str) -> (FunctionAccessKey, TypedFunction) {
        let int_tipo = Type::int();
        (
            FunctionAccessKey {
                module_name: "math".to_string(),
                function_name: name.to_string(),
            },
            TypedFunction {
                arguments: vec![TypedArg::new("n", int_tipo.clone())],
                body: negate_expr(local_var("n", int_tipo.clone())),
                doc: None,
                location: Span::empty(),
                name: name.to_string(),
                public: false,
                return_annotation: None,
                return_type: int_tipo,
                end_position: 0,
                on_test_failure: OnTestFailure::FailImmediately,
            },
        )
    }

    fn make_named_unary_identity_mapper_function(
        name: &str,
        payload_type: Rc<Type>,
    ) -> (FunctionAccessKey, TypedFunction) {
        (
            FunctionAccessKey {
                module_name: "math".to_string(),
                function_name: name.to_string(),
            },
            TypedFunction {
                arguments: vec![TypedArg::new("x", payload_type.clone())],
                body: local_var("x", payload_type.clone()),
                doc: None,
                location: Span::empty(),
                name: name.to_string(),
                public: false,
                return_annotation: None,
                return_type: payload_type,
                end_position: 0,
                on_test_failure: OnTestFailure::FailImmediately,
            },
        )
    }

    fn make_unresolved_unary_mapper(name: &str, payload_type: Rc<Type>) -> TypedExpr {
        local_var(
            name,
            Type::function(vec![payload_type.clone()], payload_type),
        )
    }

    fn make_unresolved_unary_mapper_with_types(
        name: &str,
        input_type: Rc<Type>,
        output_type: Rc<Type>,
    ) -> TypedExpr {
        local_var(name, Type::function(vec![input_type], output_type))
    }

    fn make_zero_arg_function(
        name: &str,
        return_type: Rc<Type>,
        body: TypedExpr,
    ) -> (FunctionAccessKey, TypedFunction) {
        (
            FunctionAccessKey {
                module_name: "math".to_string(),
                function_name: name.to_string(),
            },
            TypedFunction {
                arguments: vec![],
                body,
                doc: None,
                location: Span::empty(),
                name: name.to_string(),
                public: false,
                return_annotation: None,
                return_type,
                end_position: 0,
                on_test_failure: OnTestFailure::FailImmediately,
            },
        )
    }

    fn make_zero_arg_call(name: &str, return_type: Rc<Type>) -> TypedExpr {
        TypedExpr::Call {
            location: Span::empty(),
            tipo: return_type.clone(),
            fun: Box::new(module_fn_var(
                name,
                "math",
                Type::function(vec![], return_type),
            )),
            args: vec![],
        }
    }

    fn make_leaf_fuzzer_call(name: &str, payload_type: Rc<Type>) -> TypedExpr {
        let fuzzer_type = Type::fuzzer(payload_type);
        TypedExpr::Call {
            location: Span::empty(),
            tipo: fuzzer_type.clone(),
            fun: Box::new(module_fn_var(
                name,
                "math",
                Type::function(vec![], fuzzer_type),
            )),
            args: vec![],
        }
    }

    fn make_typed_int_between_fuzzer(min: &str, max: &str) -> TypedExpr {
        let output_type = Type::int();
        let fuzzer_type = Type::fuzzer(output_type.clone());
        TypedExpr::Call {
            location: Span::empty(),
            tipo: fuzzer_type.clone(),
            fun: Box::new(module_fn_var(
                "int_between",
                STDLIB_FUZZ_MODULE,
                Type::function(vec![Type::int(), Type::int()], fuzzer_type),
            )),
            args: vec![call_arg(uint_lit(min)), call_arg(uint_lit(max))],
        }
    }

    fn make_typed_bool_fuzzer() -> TypedExpr {
        let output_type = Type::bool();
        let fuzzer_type = Type::fuzzer(output_type);
        TypedExpr::Call {
            location: Span::empty(),
            tipo: fuzzer_type.clone(),
            fun: Box::new(module_fn_var(
                "bool",
                STDLIB_FUZZ_MODULE,
                Type::function(vec![], fuzzer_type),
            )),
            args: vec![],
        }
    }

    fn make_identity_mapper(arg_name: &str, payload_type: Rc<Type>) -> TypedExpr {
        TypedExpr::Fn {
            location: Span::empty(),
            tipo: Type::function(vec![payload_type.clone()], payload_type.clone()),
            is_capture: false,
            args: vec![TypedArg::new(arg_name, payload_type.clone())],
            body: Box::new(local_var(arg_name, payload_type)),
            return_annotation: None,
        }
    }

    fn bool_constructor(value: bool) -> TypedExpr {
        let name = if value { "True" } else { "False" };

        TypedExpr::Var {
            location: Span::empty(),
            constructor: ValueConstructor::public(
                Type::bool(),
                ValueConstructorVariant::Record {
                    name: name.to_string(),
                    arity: 0,
                    field_map: None,
                    location: Span::empty(),
                    module: String::new(),
                    constructors_count: 2,
                },
            ),
            name: name.to_string(),
        }
    }

    fn make_unary_mapper(
        arg_name: &str,
        input_type: Rc<Type>,
        output_type: Rc<Type>,
        body: TypedExpr,
    ) -> TypedExpr {
        TypedExpr::Fn {
            location: Span::empty(),
            tipo: Type::function(vec![input_type.clone()], output_type),
            is_capture: false,
            args: vec![TypedArg::new(arg_name, input_type)],
            body: Box::new(body),
            return_annotation: None,
        }
    }

    fn make_constant_bool_mapper(input_type: Rc<Type>, value: bool) -> TypedExpr {
        make_unary_mapper("x", input_type, Type::bool(), bool_constructor(value))
    }

    fn make_not_bool_mapper(arg_name: &str) -> TypedExpr {
        make_unary_mapper(
            arg_name,
            Type::bool(),
            Type::bool(),
            TypedExpr::UnOp {
                location: Span::empty(),
                value: Box::new(local_var(arg_name, Type::bool())),
                tipo: Type::bool(),
                op: UnOp::Not,
            },
        )
    }

    fn make_tautology_body(arg_name: &str, input_type: Rc<Type>) -> TypedExpr {
        TypedExpr::BinOp {
            location: Span::empty(),
            tipo: Type::bool(),
            name: BinOp::Eq,
            left: Box::new(local_var(arg_name, input_type.clone())),
            right: Box::new(local_var(arg_name, input_type)),
        }
    }

    fn make_constant_int_mapper(input_type: Rc<Type>, value: &str) -> TypedExpr {
        make_unary_mapper("x", input_type, Type::int(), uint_lit(value))
    }

    fn string_lit(value: &str) -> TypedExpr {
        TypedExpr::String {
            location: Span::empty(),
            tipo: Type::string(),
            value: value.to_string(),
        }
    }

    fn int_eq_expr(arg_name: &str, value: &str) -> TypedExpr {
        TypedExpr::BinOp {
            location: Span::empty(),
            tipo: Type::bool(),
            name: BinOp::Eq,
            left: Box::new(local_var(arg_name, Type::int())),
            right: Box::new(uint_lit(value)),
        }
    }

    fn finite_string_if_mapper_body(arg_name: &str) -> TypedExpr {
        TypedExpr::If {
            location: Span::empty(),
            tipo: Type::string(),
            branches: vec1::vec1![
                IfBranch {
                    condition: int_eq_expr(arg_name, "0"),
                    body: string_lit("world"),
                    is: None,
                    location: Span::empty(),
                },
                IfBranch {
                    condition: int_eq_expr(arg_name, "1"),
                    body: string_lit("hello"),
                    is: None,
                    location: Span::empty(),
                },
                IfBranch {
                    condition: int_eq_expr(arg_name, "2"),
                    body: string_lit("test"),
                    is: None,
                    location: Span::empty(),
                },
            ],
            final_else: Box::new(string_lit("")),
        }
    }

    fn finite_string_if_mapper() -> TypedExpr {
        make_unary_mapper(
            "i",
            Type::int(),
            Type::string(),
            finite_string_if_mapper_body("i"),
        )
    }

    fn int_pattern(value: &str) -> TypedPattern {
        TypedPattern::Int {
            location: Span::empty(),
            value: value.to_string(),
            base: Base::Decimal {
                numeric_underscore: false,
            },
        }
    }

    fn finite_string_when_mapper() -> TypedExpr {
        let body = TypedExpr::When {
            location: Span::empty(),
            tipo: Type::string(),
            subject: Box::new(local_var("i", Type::int())),
            clauses: vec![
                TypedClause {
                    location: Span::empty(),
                    pattern: int_pattern("0"),
                    then: string_lit("world"),
                },
                TypedClause {
                    location: Span::empty(),
                    pattern: int_pattern("1"),
                    then: string_lit("hello"),
                },
                TypedClause {
                    location: Span::empty(),
                    pattern: int_pattern("2"),
                    then: string_lit("test"),
                },
                TypedClause {
                    location: Span::empty(),
                    pattern: TypedPattern::Discard {
                        name: "_".to_string(),
                        location: Span::empty(),
                    },
                    then: string_lit(""),
                },
            ],
        };

        make_unary_mapper("i", Type::int(), Type::string(), body)
    }

    fn expected_finite_string_values() -> Vec<FuzzerExactValue> {
        vec![
            FuzzerExactValue::String("".to_string()),
            FuzzerExactValue::String("hello".to_string()),
            FuzzerExactValue::String("test".to_string()),
            FuzzerExactValue::String("world".to_string()),
        ]
    }

    fn make_named_finite_string_mapper_function(name: &str) -> (FunctionAccessKey, TypedFunction) {
        (
            FunctionAccessKey {
                module_name: "math".to_string(),
                function_name: name.to_string(),
            },
            TypedFunction {
                arguments: vec![TypedArg::new("i", Type::int())],
                body: finite_string_if_mapper_body("i"),
                doc: None,
                location: Span::empty(),
                name: name.to_string(),
                public: false,
                return_annotation: None,
                return_type: Type::string(),
                end_position: 0,
                on_test_failure: OnTestFailure::FailImmediately,
            },
        )
    }

    fn make_add_int_mapper(offset: &str) -> TypedExpr {
        let int_type = Type::int();
        make_unary_mapper(
            "x",
            int_type.clone(),
            int_type.clone(),
            TypedExpr::BinOp {
                location: Span::empty(),
                tipo: int_type.clone(),
                name: BinOp::AddInt,
                left: Box::new(local_var("x", int_type)),
                right: Box::new(uint_lit(offset)),
            },
        )
    }

    fn make_named_unary_constant_int_mapper_function(
        name: &str,
        value: &str,
    ) -> (FunctionAccessKey, TypedFunction) {
        let int_tipo = Type::int();
        (
            FunctionAccessKey {
                module_name: "math".to_string(),
                function_name: name.to_string(),
            },
            TypedFunction {
                arguments: vec![TypedArg::new("x", int_tipo.clone())],
                body: uint_lit(value),
                doc: None,
                location: Span::empty(),
                name: name.to_string(),
                public: false,
                return_annotation: None,
                return_type: int_tipo,
                end_position: 0,
                on_test_failure: OnTestFailure::FailImmediately,
            },
        )
    }

    fn make_named_unary_tautology_mapper_function(
        name: &str,
        input_type: Rc<Type>,
    ) -> (FunctionAccessKey, TypedFunction) {
        (
            FunctionAccessKey {
                module_name: "math".to_string(),
                function_name: name.to_string(),
            },
            TypedFunction {
                arguments: vec![TypedArg::new("x", input_type.clone())],
                body: make_tautology_body("x", input_type),
                doc: None,
                location: Span::empty(),
                name: name.to_string(),
                public: false,
                return_annotation: None,
                return_type: Type::bool(),
                end_position: 0,
                on_test_failure: OnTestFailure::FailImmediately,
            },
        )
    }

    fn make_bind_continuation(
        name: &str,
        input_type: Rc<Type>,
        output_type: Rc<Type>,
    ) -> TypedExpr {
        module_fn_var(
            name,
            "math",
            Type::function(vec![input_type], Type::fuzzer(output_type)),
        )
    }

    fn make_inline_bind_continuation(
        arg_name: &str,
        input_type: Rc<Type>,
        body: TypedExpr,
        output_type: Rc<Type>,
    ) -> TypedExpr {
        TypedExpr::Fn {
            location: Span::empty(),
            tipo: Type::function(vec![input_type.clone()], Type::fuzzer(output_type)),
            is_capture: false,
            args: vec![TypedArg::new(arg_name, input_type)],
            body: Box::new(body),
            return_annotation: None,
        }
    }

    fn make_typed_map_call(
        source: TypedExpr,
        mapper: TypedExpr,
        output_type: Rc<Type>,
    ) -> TypedExpr {
        TypedExpr::Call {
            location: Span::empty(),
            tipo: Type::fuzzer(output_type.clone()),
            fun: Box::new(module_fn_var(
                "anything_but_map",
                "math",
                Type::function(
                    vec![source.tipo(), mapper.tipo()],
                    Type::fuzzer(output_type),
                ),
            )),
            args: vec![call_arg(source), call_arg(mapper)],
        }
    }

    fn make_typed_bind_call(
        source: TypedExpr,
        continuation: TypedExpr,
        output_type: Rc<Type>,
    ) -> TypedExpr {
        TypedExpr::Call {
            location: Span::empty(),
            tipo: Type::fuzzer(output_type.clone()),
            fun: Box::new(module_fn_var(
                "anything_but_and_then",
                "math",
                Type::function(
                    vec![source.tipo(), continuation.tipo()],
                    Type::fuzzer(output_type),
                ),
            )),
            args: vec![call_arg(source), call_arg(continuation)],
        }
    }

    /// Build a filter/such_that call: (Fuzzer<a>, fn(a) -> Bool) -> Fuzzer<a>
    fn make_typed_filter_call(source: TypedExpr, predicate: TypedExpr) -> TypedExpr {
        let payload_type =
            extract_fuzzer_payload_type(source.tipo().as_ref()).expect("source must be a Fuzzer");
        TypedExpr::Call {
            location: Span::empty(),
            tipo: Type::fuzzer(payload_type.clone()),
            fun: Box::new(module_fn_var(
                "such_that",
                STDLIB_FUZZ_MODULE,
                Type::function(
                    vec![source.tipo(), predicate.tipo()],
                    Type::fuzzer(payload_type),
                ),
            )),
            args: vec![call_arg(source), call_arg(predicate)],
        }
    }

    /// Build a Bool-returning predicate lambda: fn(a) -> Bool
    fn make_bool_predicate(arg_name: &str, input_type: Rc<Type>) -> TypedExpr {
        TypedExpr::Fn {
            location: Span::empty(),
            tipo: Type::function(vec![input_type.clone()], Type::bool()),
            is_capture: false,
            args: vec![TypedArg::new(arg_name, input_type)],
            body: Box::new(bool_constructor(true)),
            return_annotation: None,
        }
    }

    fn make_typed_product_call(left: TypedExpr, right: TypedExpr) -> TypedExpr {
        let output_type = Type::tuple(vec![Type::int(), Type::int()]);
        TypedExpr::Call {
            location: Span::empty(),
            tipo: Type::fuzzer(output_type.clone()),
            fun: Box::new(module_fn_var(
                "anything_but_both",
                "math",
                Type::function(vec![left.tipo(), right.tipo()], Type::fuzzer(output_type)),
            )),
            args: vec![call_arg(left), call_arg(right)],
        }
    }

    fn make_typed_map2_product_call(
        first: TypedExpr,
        second: TypedExpr,
        mapper: TypedExpr,
    ) -> TypedExpr {
        let output_type = Type::tuple(vec![Type::int(), Type::int()]);
        TypedExpr::Call {
            location: Span::empty(),
            tipo: Type::fuzzer(output_type.clone()),
            fun: Box::new(module_fn_var(
                "anything_but_map2",
                "math",
                Type::function(
                    vec![first.tipo(), second.tipo(), mapper.tipo()],
                    Type::fuzzer(output_type),
                ),
            )),
            args: vec![call_arg(first), call_arg(second), call_arg(mapper)],
        }
    }

    fn make_typed_list_call(element: TypedExpr, element_payload_type: Rc<Type>) -> TypedExpr {
        let output_type = Type::list(element_payload_type);
        TypedExpr::Call {
            location: Span::empty(),
            tipo: Type::fuzzer(output_type.clone()),
            fun: Box::new(module_fn_var(
                "anything_but_list",
                "math",
                Type::function(vec![element.tipo()], Type::fuzzer(output_type)),
            )),
            args: vec![call_arg(element)],
        }
    }

    fn make_zero_arg_fuzzer_function(
        name: &str,
        payload_type: Rc<Type>,
        body: TypedExpr,
    ) -> (FunctionAccessKey, TypedFunction) {
        make_zero_arg_function(name, Type::fuzzer(payload_type), body)
    }

    fn make_zero_arg_fuzzer_call(name: &str, payload_type: Rc<Type>) -> TypedExpr {
        make_zero_arg_call(name, Type::fuzzer(payload_type))
    }

    fn assert_normalized_leaf(normalized: NormalizedFuzzer) {
        assert!(matches!(normalized, NormalizedFuzzer::Primitive { .. }));
    }

    fn assert_normalized_map(normalized: NormalizedFuzzer) {
        match normalized {
            NormalizedFuzzer::Map { source, .. } => {
                assert_normalized_leaf(*source);
            }
            other => panic!("expected map normalization, got {other:?}"),
        }
    }

    fn assert_normalized_bind(normalized: NormalizedFuzzer) {
        match normalized {
            NormalizedFuzzer::Bind { source, result } => {
                assert_normalized_leaf(*source);
                assert!(matches!(*result, NormalizedFuzzer::Opaque { .. }));
            }
            other => panic!("expected bind normalization, got {other:?}"),
        }
    }

    fn assert_normalized_product(normalized: NormalizedFuzzer, len: usize) {
        match normalized {
            NormalizedFuzzer::Product { elements } => {
                assert_eq!(elements.len(), len);
                elements.into_iter().for_each(assert_normalized_leaf);
            }
            other => panic!("expected product normalization, got {other:?}"),
        }
    }

    fn assert_normalized_list(normalized: NormalizedFuzzer) {
        match normalized {
            NormalizedFuzzer::List {
                element,
                min_len,
                max_len,
            } => {
                assert_normalized_leaf(*element);
                assert_eq!(min_len, None);
                assert_eq!(max_len, None);
            }
            other => panic!("expected list normalization, got {other:?}"),
        }
    }

    fn empty_known_functions<'a>() -> IndexMap<&'a FunctionAccessKey, &'a TypedFunction> {
        IndexMap::new()
    }

    fn empty_known_constants<'a>() -> IndexMap<&'a FunctionAccessKey, &'a TypedExpr> {
        IndexMap::new()
    }

    fn make_nullary_constructor_type(module_name: &str, type_name: &str) -> Rc<Type> {
        Rc::new(Type::App {
            public: true,
            contains_opaque: false,
            module: module_name.to_string(),
            name: type_name.to_string(),
            args: vec![],
            alias: None,
        })
    }

    fn make_nullary_constructor_data_types(
        module_name: &str,
        type_name: &str,
        constructor_names: &[&str],
    ) -> IndexMap<DataTypeKey, TypedDataType> {
        let constructors = constructor_names
            .iter()
            .map(|name| RecordConstructor {
                decorators: vec![],
                location: Span::empty(),
                name: (*name).to_string(),
                arguments: vec![],
                doc: None,
                sugar: false,
            })
            .collect();

        let data_type = TypedDataType {
            decorators: vec![],
            constructors,
            doc: None,
            location: Span::empty(),
            name: type_name.to_string(),
            opaque: false,
            parameters: vec![],
            public: true,
            typed_parameters: vec![],
        };

        let mut data_types = IndexMap::new();
        data_types.insert(
            DataTypeKey {
                module_name: module_name.to_string(),
                defined_type: type_name.to_string(),
            },
            data_type,
        );
        data_types
    }

    fn make_nullary_constructor_value(
        module_name: &str,
        type_name: &str,
        constructor_name: &str,
        constructors_count: u16,
    ) -> TypedExpr {
        let output_type = make_nullary_constructor_type(module_name, type_name);

        TypedExpr::Var {
            location: Span::empty(),
            constructor: ValueConstructor::public(
                output_type,
                ValueConstructorVariant::Record {
                    name: constructor_name.to_string(),
                    arity: 0,
                    field_map: None,
                    location: Span::empty(),
                    module: module_name.to_string(),
                    constructors_count,
                },
            ),
            name: constructor_name.to_string(),
        }
    }

    fn make_nullary_constructor_mapper_body(
        arg_name: &str,
        input_module: &str,
        input_type_name: &str,
        output_module: &str,
        output_type_name: &str,
        mappings: &[(&str, &str)],
        output_constructors_count: u16,
    ) -> TypedExpr {
        let input_type = make_nullary_constructor_type(input_module, input_type_name);
        let output_type = make_nullary_constructor_type(output_module, output_type_name);

        let clauses = mappings
            .iter()
            .map(|(source_constructor, output_constructor)| TypedClause {
                location: Span::empty(),
                pattern: TypedPattern::constructor(
                    source_constructor,
                    &[],
                    input_type.clone(),
                    Span::empty(),
                ),
                then: make_nullary_constructor_value(
                    output_module,
                    output_type_name,
                    output_constructor,
                    output_constructors_count,
                ),
            })
            .collect();

        TypedExpr::When {
            location: Span::empty(),
            tipo: output_type,
            subject: Box::new(local_var(arg_name, input_type)),
            clauses,
        }
    }

    fn make_nullary_constructor_mapper(
        input_module: &str,
        input_type_name: &str,
        output_module: &str,
        output_type_name: &str,
        mappings: &[(&str, &str)],
        output_constructors_count: u16,
    ) -> TypedExpr {
        let input_type = make_nullary_constructor_type(input_module, input_type_name);
        let output_type = make_nullary_constructor_type(output_module, output_type_name);

        make_unary_mapper(
            "state",
            input_type,
            output_type,
            make_nullary_constructor_mapper_body(
                "state",
                input_module,
                input_type_name,
                output_module,
                output_type_name,
                mappings,
                output_constructors_count,
            ),
        )
    }

    fn make_named_nullary_constructor_mapper_function(
        name: &str,
        input_module: &str,
        input_type_name: &str,
        output_module: &str,
        output_type_name: &str,
        mappings: &[(&str, &str)],
        output_constructors_count: u16,
    ) -> (FunctionAccessKey, TypedFunction) {
        let input_type = make_nullary_constructor_type(input_module, input_type_name);
        let output_type = make_nullary_constructor_type(output_module, output_type_name);

        (
            FunctionAccessKey {
                module_name: "math".to_string(),
                function_name: name.to_string(),
            },
            TypedFunction {
                arguments: vec![TypedArg::new("state", input_type.clone())],
                body: make_nullary_constructor_mapper_body(
                    "state",
                    input_module,
                    input_type_name,
                    output_module,
                    output_type_name,
                    mappings,
                    output_constructors_count,
                ),
                doc: None,
                location: Span::empty(),
                name: name.to_string(),
                public: false,
                return_annotation: None,
                return_type: output_type,
                end_position: 0,
                on_test_failure: OnTestFailure::FailImmediately,
            },
        )
    }

    #[test]
    fn normalize_fuzzer_map_shape_is_name_agnostic() {
        let via = make_typed_map_call(
            make_leaf_fuzzer_call("primitive_source", Type::int()),
            make_unresolved_unary_mapper("f", Type::int()),
            Type::int(),
        );

        let normalized = normalize_fuzzer_from_via(&via, "math", &empty_known_functions());
        assert_normalized_map(normalized);
    }

    #[test]
    fn normalize_fuzzer_bind_shape_is_name_agnostic() {
        let via = make_typed_bind_call(
            make_leaf_fuzzer_call("primitive_source", Type::int()),
            make_bind_continuation("next_step", Type::int(), Type::bool()),
            Type::bool(),
        );

        let normalized = normalize_fuzzer_from_via(&via, "math", &empty_known_functions());
        assert_normalized_bind(normalized);
    }

    #[test]
    fn normalize_fuzzer_direct_product_shape_is_name_agnostic() {
        let via = make_typed_product_call(
            make_leaf_fuzzer_call("lhs", Type::int()),
            make_leaf_fuzzer_call("rhs", Type::int()),
        );

        let normalized = normalize_fuzzer_from_via(&via, "math", &empty_known_functions());
        assert_normalized_product(normalized, 2);
    }

    #[test]
    fn normalize_fuzzer_mapped_product_shape_is_name_agnostic() {
        let via = make_typed_map2_product_call(
            make_leaf_fuzzer_call("lhs", Type::int()),
            make_leaf_fuzzer_call("rhs", Type::int()),
            make_map2_mapper(vec![
                local_var("a", Type::int()),
                local_var("b", Type::int()),
            ]),
        );

        let normalized = normalize_fuzzer_from_via(&via, "math", &empty_known_functions());
        assert_normalized_product(normalized, 2);
    }

    #[test]
    fn normalize_fuzzer_zero_arg_wrapper_unwraps_structurally() {
        let (helper_key, helper_fn) = make_zero_arg_fuzzer_function(
            "custom_wrapper",
            Type::int(),
            make_typed_map_call(
                make_leaf_fuzzer_call("primitive_source", Type::int()),
                make_unresolved_unary_mapper("f", Type::int()),
                Type::int(),
            ),
        );
        let mut functions = empty_known_functions();
        functions.insert(&helper_key, &helper_fn);

        let via = make_zero_arg_fuzzer_call("custom_wrapper", Type::int());
        let normalized = normalize_fuzzer_from_via(&via, "math", &functions);
        assert_normalized_map(normalized);
    }

    #[test]
    fn normalize_fuzzer_list_shape_is_name_agnostic() {
        let via = make_typed_list_call(make_leaf_fuzzer_call("elem", Type::int()), Type::int());

        let normalized = normalize_fuzzer_from_via(&via, "math", &empty_known_functions());
        assert_normalized_list(normalized);
    }

    #[test]
    fn normalize_fuzzer_name_collision_is_not_special_cased() {
        let (helper_key, helper_fn) = make_zero_arg_fuzzer_function(
            "map",
            Type::int(),
            make_leaf_fuzzer_call("primitive_source", Type::int()),
        );
        let mut functions = empty_known_functions();
        functions.insert(&helper_key, &helper_fn);

        let via = make_zero_arg_fuzzer_call("map", Type::int());
        let normalized = normalize_fuzzer_from_via(&via, "math", &functions);
        assert!(matches!(normalized, NormalizedFuzzer::Primitive { .. }));
    }

    #[test]
    fn normalize_fuzzer_sequence_alias_unwraps_structurally() {
        let map_expr = make_typed_map_call(
            make_leaf_fuzzer_call("primitive_source", Type::int()),
            make_unresolved_unary_mapper("f", Type::int()),
            Type::int(),
        );
        let alias_type = map_expr.tipo();
        let via = TypedExpr::Sequence {
            location: Span::empty(),
            expressions: vec![
                TypedExpr::Assignment {
                    location: Span::empty(),
                    tipo: alias_type.clone(),
                    value: Box::new(map_expr),
                    pattern: TypedPattern::var("alias"),
                    kind: crate::ast::AssignmentKind::Let { backpassing: () },
                    comment: None,
                },
                local_var("alias", alias_type),
            ],
        };

        let normalized = normalize_fuzzer_from_via(&via, "math", &empty_known_functions());
        assert_normalized_map(normalized);
    }

    #[test]
    fn normalize_fuzzer_recursive_wrapper_cycle_is_opaque() {
        // A mutually-recursive fuzzer cycle has no base case the normalizer
        // can widen from (the helper body is just another `Call` to the
        // peer, not a control-flow or lambda shape whose output type we
        // can trust). Keeping such cycles opaque is both sound and
        // informative — the user almost certainly wrote a bug if the
        // fuzzer recurses without a base case.
        let (left_key, left_fn) = make_zero_arg_fuzzer_function(
            "left",
            Type::int(),
            make_zero_arg_fuzzer_call("right", Type::int()),
        );
        let (right_key, right_fn) = make_zero_arg_fuzzer_function(
            "right",
            Type::int(),
            make_zero_arg_fuzzer_call("left", Type::int()),
        );
        let mut functions = empty_known_functions();
        functions.insert(&left_key, &left_fn);
        functions.insert(&right_key, &right_fn);

        let normalized = normalize_fuzzer_from_via(
            &make_zero_arg_fuzzer_call("left", Type::int()),
            "math",
            &functions,
        );

        match normalized {
            NormalizedFuzzer::Opaque { reason, .. } => {
                assert!(reason.contains("recursive helper fuzzer detected"));
            }
            other => panic!("expected opaque recursive normalization, got {other:?}"),
        }
    }

    #[test]
    fn normalize_fuzzer_beta_reduces_zero_arg_fn_literal_call() {
        // S2 SUBSET: A call whose callee is a zero-argument `Fn` literal
        // (e.g. `fn() { leaf() }()`) should beta-reduce to the body.  Without
        // this, the call falls through to helper descent (which cannot resolve
        // a raw `Fn` literal via `resolve_function_from_expr`) and normalizes
        // to `Opaque`, erasing the structural shape of the body.
        //
        // This shape arises inside `fork*_and_then` stdlib bodies, where the
        // thunk parameters (`baseline`, `branch1`, …) are substituted with
        // `fn() { scenario_inputs_X(st) }` locals and invoked as `baseline()`.
        let int_tipo = Type::int();
        let fuzzer_tipo = Type::fuzzer(int_tipo.clone());

        let leaf = make_leaf_fuzzer_call("primitive_source", int_tipo.clone());
        let thunk = TypedExpr::Fn {
            location: Span::empty(),
            tipo: Type::function(vec![], fuzzer_tipo.clone()),
            is_capture: false,
            args: vec![],
            body: Box::new(leaf),
            return_annotation: None,
        };
        let via = TypedExpr::Call {
            location: Span::empty(),
            tipo: fuzzer_tipo,
            fun: Box::new(thunk),
            args: vec![],
        };

        let normalized = normalize_fuzzer_from_via(&via, "math", &empty_known_functions());
        assert_normalized_leaf(normalized);
    }

    #[test]
    fn normalize_fuzzer_beta_reduces_fn_literal_through_local_alias() {
        // S2 SUBSET: Beta reduction must also work when the `Fn` literal is
        // reached through a chain of local aliases — this is the exact shape
        // produced by helper descent into `fork*_and_then`, where formal
        // parameters (`baseline`) are bound in `local_values` to the caller's
        // thunk expressions.  We simulate that binding with a `let`.
        let int_tipo = Type::int();
        let fuzzer_tipo = Type::fuzzer(int_tipo.clone());

        let leaf = make_leaf_fuzzer_call("primitive_source", int_tipo.clone());
        let thunk_tipo = Type::function(vec![], fuzzer_tipo.clone());
        let thunk = TypedExpr::Fn {
            location: Span::empty(),
            tipo: thunk_tipo.clone(),
            is_capture: false,
            args: vec![],
            body: Box::new(leaf),
            return_annotation: None,
        };

        // let baseline = fn() { primitive_source() } ; baseline()
        let alias_call = TypedExpr::Call {
            location: Span::empty(),
            tipo: fuzzer_tipo.clone(),
            fun: Box::new(local_var("baseline", thunk_tipo.clone())),
            args: vec![],
        };
        let via = TypedExpr::Sequence {
            location: Span::empty(),
            expressions: vec![
                TypedExpr::Assignment {
                    location: Span::empty(),
                    tipo: thunk_tipo,
                    value: Box::new(thunk),
                    pattern: TypedPattern::var("baseline"),
                    kind: crate::ast::AssignmentKind::Let { backpassing: () },
                    comment: None,
                },
                alias_call,
            ],
        };

        let normalized = normalize_fuzzer_from_via(&via, "math", &empty_known_functions());
        assert_normalized_leaf(normalized);
    }

    #[test]
    fn extract_constraint_name_agnostic_map_preserves_map_domain_for_unknown_mapper() {
        // With stdlib-name-gated extraction restored, `int_between(1, 3)`
        // extracts `IntRange { min: "1", max: "3" }`. `map(int_between(1, 3), f)`
        // therefore wraps that range inside `Map`.
        let via = make_typed_map_call(
            make_typed_int_between_fuzzer("1", "3"),
            make_unresolved_unary_mapper("f", Type::int()),
            Type::int(),
        );

        let constraint = extract_constraint_from_via(&via, "math", &empty_known_functions());
        assert_eq!(
            constraint,
            FuzzerConstraint::Map(Box::new(FuzzerConstraint::IntRange {
                min: "1".to_string(),
                max: "3".to_string(),
            }))
        );
    }

    #[test]
    fn extract_constraint_name_agnostic_identity_map_preserves_source_domain() {
        // The source `int_between(1, 3)` now extracts bounds [1, 3]. An
        // identity mapper propagates the source's constraint unchanged.
        let via = make_typed_map_call(
            make_typed_int_between_fuzzer("1", "3"),
            make_identity_mapper("n", Type::int()),
            Type::int(),
        );

        let constraint = extract_constraint_from_via(&via, "math", &empty_known_functions());
        assert_eq!(
            constraint,
            FuzzerConstraint::IntRange {
                min: "1".to_string(),
                max: "3".to_string(),
            }
        );
    }

    #[test]
    fn extract_fuzzer_payload_type_handles_type_fn_form() {
        // Sanity check on the baseline: `Type::fuzzer(Int)` (the canonical
        // `Type::Fn` materialization of `Fuzzer<Int>`) should yield `Int`.
        let payload = Type::int();
        let fuzzer = Type::fuzzer(payload.clone());

        let extracted = extract_fuzzer_payload_type(fuzzer.as_ref())
            .expect("canonical Fuzzer<Int> must yield Int payload");

        assert_eq!(extracted.as_ref(), payload.as_ref());
    }

    #[test]
    fn extract_fuzzer_payload_type_handles_type_app_alias() {
        // Direct `Type::App { name: "Fuzzer", args: [T] }` representation of
        // `Fuzzer<Int>`. In today's codebase transparent aliases collapse to
        // the underlying `Type::Fn`, but this branch is exercised defensively
        // so any future alias/FFI path that surfaces a `Type::App`-shaped
        // Fuzzer still extracts the payload correctly.
        let payload = Type::int();
        let fuzzer_app = Rc::new(Type::App {
            public: true,
            contains_opaque: false,
            module: String::new(),
            name: "Fuzzer".to_string(),
            args: vec![payload.clone()],
            alias: None,
        });

        let extracted = extract_fuzzer_payload_type(fuzzer_app.as_ref())
            .expect("Type::App form of Fuzzer<Int> must yield Int payload");

        assert_eq!(extracted.as_ref(), payload.as_ref());
    }

    #[test]
    fn extract_fuzzer_payload_type_rejects_non_prelude_fuzzer_app() {
        // A `Type::App` named "Fuzzer" from a non-prelude module must NOT be
        // unwrapped: only the prelude `Fuzzer` (empty module) is recognized,
        // mirroring how `is_prng_type` and `is_option` check names.
        let payload = Type::int();
        let foreign_fuzzer = Rc::new(Type::App {
            public: true,
            contains_opaque: false,
            module: "some/other/module".to_string(),
            name: "Fuzzer".to_string(),
            args: vec![payload],
            alias: None,
        });

        assert!(extract_fuzzer_payload_type(foreign_fuzzer.as_ref()).is_none());
    }

    #[test]
    fn extract_fuzzer_payload_type_rejects_type_app_wrong_arity() {
        // The `args.len() == 1` guard must reject zero-arg and multi-arg App
        // forms even if module and name match the prelude Fuzzer.
        let payload = Type::int();

        let zero_arg = Rc::new(Type::App {
            public: true,
            contains_opaque: false,
            module: String::new(),
            name: "Fuzzer".to_string(),
            args: vec![],
            alias: None,
        });
        assert!(
            extract_fuzzer_payload_type(zero_arg.as_ref()).is_none(),
            "zero-arg Fuzzer App must be rejected"
        );

        let two_arg = Rc::new(Type::App {
            public: true,
            contains_opaque: false,
            module: String::new(),
            name: "Fuzzer".to_string(),
            args: vec![payload.clone(), payload],
            alias: None,
        });
        assert!(
            extract_fuzzer_payload_type(two_arg.as_ref()).is_none(),
            "two-arg Fuzzer App must be rejected"
        );
    }

    fn make_typed_int_at_least_fuzzer(min: &str) -> TypedExpr {
        let output_type = Type::int();
        let fuzzer_type = Type::fuzzer(output_type.clone());
        TypedExpr::Call {
            location: Span::empty(),
            tipo: fuzzer_type.clone(),
            fun: Box::new(module_fn_var(
                "int_at_least",
                STDLIB_FUZZ_MODULE,
                Type::function(vec![Type::int()], fuzzer_type),
            )),
            args: vec![call_arg(uint_lit(min))],
        }
    }

    fn make_typed_int_at_most_fuzzer(max: &str) -> TypedExpr {
        let output_type = Type::int();
        let fuzzer_type = Type::fuzzer(output_type.clone());
        TypedExpr::Call {
            location: Span::empty(),
            tipo: fuzzer_type.clone(),
            fun: Box::new(module_fn_var(
                "int_at_most",
                STDLIB_FUZZ_MODULE,
                Type::function(vec![Type::int()], fuzzer_type),
            )),
            args: vec![call_arg(uint_lit(max))],
        }
    }

    /// `fuzz.int_at_least(5)` is recognized as a stdlib fuzzer and produces
    /// the closed-range `IntRange { min: "5", max: i128::MAX }` constraint;
    /// the semantics layer strips the `i128::MAX` sentinel into an open
    /// upper bound so downstream Lean emission sees a half-open range and
    /// no 39-digit literal leaks through.
    #[test]
    fn int_at_least_semantics_has_open_upper_bound() {
        let via = make_typed_int_at_least_fuzzer("5");
        let data_types: IndexMap<&DataTypeKey, &TypedDataType> = IndexMap::new();

        let constraint = extract_constraint_from_via(&via, "math", &empty_known_functions());
        assert_eq!(
            constraint,
            FuzzerConstraint::IntRange {
                min: "5".to_string(),
                max: i128::MAX.to_string(),
            }
        );

        let semantics = extract_semantics_from_via(
            &via,
            "math",
            &empty_known_functions(),
            &data_types,
            Type::int().as_ref(),
        );

        assert_eq!(
            semantics,
            FuzzerSemantics::IntRange {
                min: Some("5".to_string()),
                max: None,
            }
        );

        // Defence-in-depth: no i128 sentinel should leak into the rendered
        // semantics. The sentinel is stripped by
        // `unbounded_int_sentinel_to_none`, leaving a half-open range.
        let rendered = format!("{semantics:?}");
        assert!(
            !rendered.contains(&i128::MAX.to_string()),
            "int_at_least semantics leaked i128::MAX sentinel: {rendered}"
        );
        assert!(
            !rendered.contains(&i128::MIN.to_string()),
            "int_at_least semantics leaked i128::MIN sentinel: {rendered}"
        );
        assert!(
            !contains_39_plus_digit_run(&rendered),
            "int_at_least semantics contains a 39+ digit numeric string: {rendered}"
        );
    }

    /// Symmetric regression for `fuzz.int_at_most(10)`. The stdlib recognizer
    /// extracts `IntRange { min: i128::MIN, max: "10" }` and the semantics
    /// layer strips the `i128::MIN` sentinel into an open lower bound.
    #[test]
    fn int_at_most_semantics_has_open_lower_bound() {
        let via = make_typed_int_at_most_fuzzer("10");
        let data_types: IndexMap<&DataTypeKey, &TypedDataType> = IndexMap::new();

        let constraint = extract_constraint_from_via(&via, "math", &empty_known_functions());
        assert_eq!(
            constraint,
            FuzzerConstraint::IntRange {
                min: i128::MIN.to_string(),
                max: "10".to_string(),
            }
        );

        let semantics = extract_semantics_from_via(
            &via,
            "math",
            &empty_known_functions(),
            &data_types,
            Type::int().as_ref(),
        );

        assert_eq!(
            semantics,
            FuzzerSemantics::IntRange {
                min: None,
                max: Some("10".to_string()),
            }
        );

        let rendered = format!("{semantics:?}");
        assert!(
            !rendered.contains(&i128::MAX.to_string()),
            "int_at_most semantics leaked i128::MAX sentinel: {rendered}"
        );
        assert!(
            !rendered.contains(&i128::MIN.to_string()),
            "int_at_most semantics leaked i128::MIN sentinel: {rendered}"
        );
        assert!(
            !contains_39_plus_digit_run(&rendered),
            "int_at_most semantics contains a 39+ digit numeric string: {rendered}"
        );
    }

    /// Scans a string for any run of 39 or more consecutive ASCII digits.
    /// `i128::MAX` / `i128::MIN` serialize to 39-digit runs (ignoring the sign),
    /// so this catches both sentinels and any accidental leak of similarly
    /// sized literals.
    fn contains_39_plus_digit_run(s: &str) -> bool {
        let mut run = 0usize;
        for c in s.chars() {
            if c.is_ascii_digit() {
                run += 1;
                if run >= 39 {
                    return true;
                }
            } else {
                run = 0;
            }
        }
        false
    }

    #[test]
    fn extract_constraint_name_agnostic_named_identity_mapper_uses_function_body_shape() {
        // The inner `int_between(1, 3)` now extracts [1, 3] via the stdlib
        // recognizer. A named identity mapper structurally collapses the
        // map to its source, so the outer constraint is that same range.
        let (identity_key, identity_fn) =
            make_named_unary_identity_mapper_function("identity", Type::int());
        let mut functions = empty_known_functions();
        functions.insert(&identity_key, &identity_fn);

        let via = make_typed_map_call(
            make_typed_int_between_fuzzer("1", "3"),
            module_fn_var(
                "identity",
                "math",
                Type::function(vec![Type::int()], Type::int()),
            ),
            Type::int(),
        );

        let constraint = extract_constraint_from_via(&via, "math", &functions);
        assert_eq!(
            constraint,
            FuzzerConstraint::IntRange {
                min: "1".to_string(),
                max: "3".to_string(),
            }
        );
    }

    #[test]
    fn extract_constraint_name_agnostic_constant_bool_map_is_exact() {
        let via = make_typed_map_call(
            make_leaf_fuzzer_call("seed", Type::int()),
            make_constant_bool_mapper(Type::int(), true),
            Type::bool(),
        );

        let constraint = extract_constraint_from_via(&via, "math", &empty_known_functions());
        assert_eq!(
            constraint,
            FuzzerConstraint::Exact(FuzzerExactValue::Bool(true))
        );
    }

    #[test]
    fn extract_constraint_named_tautology_bool_map_is_exact() {
        let (mapper_key, mapper_fn) =
            make_named_unary_tautology_mapper_function("tautology", Type::bool());
        let mut functions = empty_known_functions();
        functions.insert(&mapper_key, &mapper_fn);

        let via = make_typed_map_call(
            make_typed_bool_fuzzer(),
            module_fn_var(
                "tautology",
                "math",
                Type::function(vec![Type::bool()], Type::bool()),
            ),
            Type::bool(),
        );

        let constraint = extract_constraint_from_via(&via, "math", &functions);
        assert_eq!(
            constraint,
            FuzzerConstraint::Exact(FuzzerExactValue::Bool(true))
        );
    }

    #[test]
    fn extract_constraint_name_agnostic_constant_int_map_is_singleton_range() {
        let via = make_typed_map_call(
            make_leaf_fuzzer_call("seed", Type::int()),
            make_constant_int_mapper(Type::int(), "41"),
            Type::int(),
        );

        let constraint = extract_constraint_from_via(&via, "math", &empty_known_functions());
        assert_eq!(
            constraint,
            FuzzerConstraint::IntRange {
                min: "41".to_string(),
                max: "41".to_string(),
            }
        );
    }

    #[test]
    fn extract_constraint_name_agnostic_named_constant_int_mapper_uses_function_body_shape() {
        let (mapper_key, mapper_fn) =
            make_named_unary_constant_int_mapper_function("always_7", "7");
        let mut functions = empty_known_functions();
        functions.insert(&mapper_key, &mapper_fn);

        let via = make_typed_map_call(
            make_leaf_fuzzer_call("seed", Type::int()),
            module_fn_var(
                "always_7",
                "math",
                Type::function(vec![Type::int()], Type::int()),
            ),
            Type::int(),
        );

        let constraint = extract_constraint_from_via(&via, "math", &functions);
        assert_eq!(
            constraint,
            FuzzerConstraint::IntRange {
                min: "7".to_string(),
                max: "7".to_string(),
            }
        );
    }

    #[test]
    fn extract_constraint_finite_string_if_mapper_is_oneof() {
        let via = make_typed_map_call(
            make_typed_int_between_fuzzer("0", "3"),
            finite_string_if_mapper(),
            Type::string(),
        );

        let constraint = extract_constraint_from_via(&via, "math", &empty_known_functions());
        assert_eq!(
            constraint,
            FuzzerConstraint::OneOf(expected_finite_string_values())
        );
    }

    #[test]
    fn extract_constraint_finite_string_when_mapper_is_oneof() {
        let via = make_typed_map_call(
            make_typed_int_between_fuzzer("0", "3"),
            finite_string_when_mapper(),
            Type::string(),
        );

        let constraint = extract_constraint_from_via(&via, "math", &empty_known_functions());
        assert_eq!(
            constraint,
            FuzzerConstraint::OneOf(expected_finite_string_values())
        );
    }

    #[test]
    fn extract_constraint_finite_string_singleton_canonicalizes_to_exact() {
        let via = make_typed_map_call(
            make_typed_int_between_fuzzer("0", "3"),
            make_unary_mapper("i", Type::int(), Type::string(), string_lit("same")),
            Type::string(),
        );

        let constraint = extract_constraint_from_via(&via, "math", &empty_known_functions());
        assert_eq!(
            constraint,
            FuzzerConstraint::Exact(FuzzerExactValue::String("same".to_string()))
        );
    }

    #[test]
    fn extract_constraint_name_agnostic_nested_constant_then_affine_map_transforms_range() {
        let source = make_typed_map_call(
            make_leaf_fuzzer_call("seed", Type::int()),
            make_constant_int_mapper(Type::int(), "41"),
            Type::int(),
        );
        let via = make_typed_map_call(source, make_add_int_mapper("1"), Type::int());

        let constraint = extract_constraint_from_via(&via, "math", &empty_known_functions());
        assert_eq!(
            constraint,
            FuzzerConstraint::IntRange {
                min: "42".to_string(),
                max: "42".to_string(),
            }
        );
    }

    #[test]
    fn extract_constraint_name_agnostic_bind_uses_continuation_shape() {
        // `int_between(5, 8)` now extracts [5, 8] via the stdlib recognizer.
        // A bind's output domain is the continuation's domain, so the
        // result reflects the continuation's extracted bounds.
        let via = make_typed_bind_call(
            make_leaf_fuzzer_call("seed", Type::int()),
            make_inline_bind_continuation(
                "x",
                Type::int(),
                make_typed_int_between_fuzzer("5", "8"),
                Type::int(),
            ),
            Type::int(),
        );

        let constraint = extract_constraint_from_via(&via, "math", &empty_known_functions());
        assert_eq!(
            constraint,
            FuzzerConstraint::IntRange {
                min: "5".to_string(),
                max: "8".to_string(),
            }
        );
    }

    #[test]
    fn extract_constraint_name_agnostic_product_uses_element_shapes() {
        // Each product element now carries its extracted range under the
        // stdlib recognizer; the tuple-level constraint records both.
        let via = make_typed_product_call(
            make_typed_int_between_fuzzer("0", "10"),
            make_typed_int_between_fuzzer("20", "30"),
        );

        let constraint = extract_constraint_from_via(&via, "math", &empty_known_functions());
        assert_eq!(
            constraint,
            FuzzerConstraint::Tuple(vec![
                FuzzerConstraint::IntRange {
                    min: "0".to_string(),
                    max: "10".to_string(),
                },
                FuzzerConstraint::IntRange {
                    min: "20".to_string(),
                    max: "30".to_string(),
                },
            ])
        );
    }

    #[test]
    fn extract_constraint_name_agnostic_list_uses_element_shape() {
        // The element fuzzer is now recognized as `int_between`, producing
        // `IntRange { 0, 10 }`. The enclosing list call is `anything_but_list`
        // (not a stdlib `list_*` function) so no length bounds are extracted.
        let via = make_typed_list_call(make_typed_int_between_fuzzer("0", "10"), Type::int());

        let constraint = extract_constraint_from_via(&via, "math", &empty_known_functions());
        assert_eq!(
            constraint,
            FuzzerConstraint::List {
                elem: Box::new(FuzzerConstraint::IntRange {
                    min: "0".to_string(),
                    max: "10".to_string(),
                }),
                min_len: None,
                max_len: None,
            }
        );
    }

    #[test]
    fn extract_constraint_name_agnostic_nullary_constructor_output_uses_constructor_domain() {
        let output_type = make_nullary_constructor_type("permissions", "Outcome");
        let via = make_leaf_fuzzer_call("custom_outcome_fuzzer", output_type.clone());
        let owned_data_types =
            make_nullary_constructor_data_types("permissions", "Outcome", &["Ok", "Ko"]);
        let data_types: IndexMap<&DataTypeKey, &TypedDataType> = owned_data_types.iter().collect();

        let constraint = extract_constraint_from_via_with_data_types(
            &via,
            "math",
            &empty_known_functions(),
            &data_types,
        );
        assert_eq!(
            constraint,
            FuzzerConstraint::DataConstructorTags { tags: vec![0, 1] }
        );
    }

    #[test]
    fn extract_constraint_name_agnostic_constructor_map_pushes_forward_nullary_domain() {
        let source_type = make_nullary_constructor_type("workflow", "Stage");
        let output_type = make_nullary_constructor_type("approval", "Decision");
        let via = make_typed_map_call(
            make_leaf_fuzzer_call("stage_fuzzer", source_type.clone()),
            make_nullary_constructor_mapper(
                "workflow",
                "Stage",
                "approval",
                "Decision",
                &[("Idle", "Allow"), ("Busy", "Review")],
                3,
            ),
            output_type.clone(),
        );

        let mut owned_data_types =
            make_nullary_constructor_data_types("workflow", "Stage", &["Idle", "Busy"]);
        owned_data_types.extend(make_nullary_constructor_data_types(
            "approval",
            "Decision",
            &["Allow", "Deny", "Review"],
        ));
        let data_types: IndexMap<&DataTypeKey, &TypedDataType> = owned_data_types.iter().collect();

        let constraint = extract_constraint_from_via_with_data_types(
            &via,
            "math",
            &empty_known_functions(),
            &data_types,
        );

        assert_eq!(
            constraint,
            FuzzerConstraint::DataConstructorTags { tags: vec![0, 2] }
        );
    }

    #[test]
    fn extract_constraint_name_agnostic_named_constructor_mapper_uses_function_body_shape() {
        let source_type = make_nullary_constructor_type("workflow", "Stage");
        let output_type = make_nullary_constructor_type("approval", "Decision");
        let (mapper_key, mapper_fn) = make_named_nullary_constructor_mapper_function(
            "collapse_stage",
            "workflow",
            "Stage",
            "approval",
            "Decision",
            &[("Idle", "Deny"), ("Busy", "Deny")],
            3,
        );
        let mut functions = empty_known_functions();
        functions.insert(&mapper_key, &mapper_fn);

        let via = make_typed_map_call(
            make_leaf_fuzzer_call("stage_fuzzer", source_type.clone()),
            module_fn_var(
                "collapse_stage",
                "math",
                Type::function(vec![source_type.clone()], output_type.clone()),
            ),
            output_type.clone(),
        );

        let mut owned_data_types =
            make_nullary_constructor_data_types("workflow", "Stage", &["Idle", "Busy"]);
        owned_data_types.extend(make_nullary_constructor_data_types(
            "approval",
            "Decision",
            &["Allow", "Deny", "Review"],
        ));
        let data_types: IndexMap<&DataTypeKey, &TypedDataType> = owned_data_types.iter().collect();

        let constraint =
            extract_constraint_from_via_with_data_types(&via, "math", &functions, &data_types);

        assert_eq!(
            constraint,
            FuzzerConstraint::DataConstructorTags { tags: vec![1] }
        );
    }

    #[test]
    fn extract_constraint_name_agnostic_constructor_map_requires_total_mapping() {
        let source_type = make_nullary_constructor_type("workflow", "Stage");
        let output_type = make_nullary_constructor_type("approval", "Decision");
        let via = make_typed_map_call(
            make_leaf_fuzzer_call("stage_fuzzer", source_type.clone()),
            make_nullary_constructor_mapper(
                "workflow",
                "Stage",
                "approval",
                "Decision",
                &[("Idle", "Allow")],
                3,
            ),
            output_type.clone(),
        );

        let mut owned_data_types =
            make_nullary_constructor_data_types("workflow", "Stage", &["Idle", "Busy"]);
        owned_data_types.extend(make_nullary_constructor_data_types(
            "approval",
            "Decision",
            &["Allow", "Deny", "Review"],
        ));
        let data_types: IndexMap<&DataTypeKey, &TypedDataType> = owned_data_types.iter().collect();

        let constraint = extract_constraint_from_via_with_data_types(
            &via,
            "math",
            &empty_known_functions(),
            &data_types,
        );

        assert_eq!(
            constraint,
            FuzzerConstraint::Map(Box::new(FuzzerConstraint::DataConstructorTags {
                tags: vec![0, 1],
            }))
        );
    }

    #[test]
    fn extract_constraint_name_agnostic_constructor_map_unresolved_mapper_is_conservative() {
        let source_type = make_nullary_constructor_type("workflow", "Stage");
        let output_type = make_nullary_constructor_type("approval", "Decision");
        let via = make_typed_map_call(
            make_leaf_fuzzer_call("stage_fuzzer", source_type.clone()),
            make_unresolved_unary_mapper_with_types(
                "next_stage",
                source_type.clone(),
                output_type.clone(),
            ),
            output_type.clone(),
        );

        let mut owned_data_types =
            make_nullary_constructor_data_types("workflow", "Stage", &["Idle", "Busy"]);
        owned_data_types.extend(make_nullary_constructor_data_types(
            "approval",
            "Decision",
            &["Allow", "Deny", "Review"],
        ));
        let data_types: IndexMap<&DataTypeKey, &TypedDataType> = owned_data_types.iter().collect();

        let constraint = extract_constraint_from_via_with_data_types(
            &via,
            "math",
            &empty_known_functions(),
            &data_types,
        );

        assert_eq!(
            constraint,
            FuzzerConstraint::Map(Box::new(FuzzerConstraint::DataConstructorTags {
                tags: vec![0, 1],
            }))
        );
    }

    #[test]
    fn extract_constraint_name_agnostic_constructor_map_recursive_mapper_cycle_is_conservative() {
        let source_type = make_nullary_constructor_type("workflow", "Stage");
        let output_type = make_nullary_constructor_type("approval", "Decision");
        let mapper_type = Type::function(vec![source_type.clone()], output_type.clone());

        let (left_key, left_fn) = make_zero_arg_function(
            "left_mapper",
            mapper_type.clone(),
            make_zero_arg_call("right_mapper", mapper_type.clone()),
        );
        let (right_key, right_fn) = make_zero_arg_function(
            "right_mapper",
            mapper_type.clone(),
            make_zero_arg_call("left_mapper", mapper_type.clone()),
        );
        let mut functions = empty_known_functions();
        functions.insert(&left_key, &left_fn);
        functions.insert(&right_key, &right_fn);

        let via = make_typed_map_call(
            make_leaf_fuzzer_call("stage_fuzzer", source_type.clone()),
            make_zero_arg_call("left_mapper", mapper_type),
            output_type.clone(),
        );

        let mut owned_data_types =
            make_nullary_constructor_data_types("workflow", "Stage", &["Idle", "Busy"]);
        owned_data_types.extend(make_nullary_constructor_data_types(
            "approval",
            "Decision",
            &["Allow", "Deny", "Review"],
        ));
        let data_types: IndexMap<&DataTypeKey, &TypedDataType> = owned_data_types.iter().collect();

        let constraint =
            extract_constraint_from_via_with_data_types(&via, "math", &functions, &data_types);

        assert_eq!(
            constraint,
            FuzzerConstraint::Map(Box::new(FuzzerConstraint::DataConstructorTags {
                tags: vec![0, 1],
            }))
        );
    }

    #[test]
    fn extract_semantics_name_agnostic_nullary_constructor_output_uses_constructor_semantics() {
        let output_type = make_nullary_constructor_type("permissions", "Outcome");
        let via = make_leaf_fuzzer_call("custom_outcome_fuzzer", output_type.clone());
        let owned_data_types =
            make_nullary_constructor_data_types("permissions", "Outcome", &["Ok", "Ko"]);
        let data_types: IndexMap<&DataTypeKey, &TypedDataType> = owned_data_types.iter().collect();

        let semantics = extract_semantics_from_via(
            &via,
            "math",
            &empty_known_functions(),
            &data_types,
            output_type.as_ref(),
        );
        assert_eq!(
            semantics,
            FuzzerSemantics::Constructors { tags: vec![0, 1] }
        );
    }

    #[test]
    fn extract_semantics_name_agnostic_constructor_map_pushes_forward_nullary_domain() {
        let source_type = make_nullary_constructor_type("workflow", "Stage");
        let output_type = make_nullary_constructor_type("approval", "Decision");
        let via = make_typed_map_call(
            make_leaf_fuzzer_call("stage_fuzzer", source_type.clone()),
            make_nullary_constructor_mapper(
                "workflow",
                "Stage",
                "approval",
                "Decision",
                &[("Idle", "Allow"), ("Busy", "Review")],
                3,
            ),
            output_type.clone(),
        );

        let mut owned_data_types =
            make_nullary_constructor_data_types("workflow", "Stage", &["Idle", "Busy"]);
        owned_data_types.extend(make_nullary_constructor_data_types(
            "approval",
            "Decision",
            &["Allow", "Deny", "Review"],
        ));
        let data_types: IndexMap<&DataTypeKey, &TypedDataType> = owned_data_types.iter().collect();

        let semantics = extract_semantics_from_via(
            &via,
            "math",
            &empty_known_functions(),
            &data_types,
            output_type.as_ref(),
        );

        assert_eq!(
            semantics,
            FuzzerSemantics::Constructors { tags: vec![0, 2] }
        );
    }

    #[test]
    fn extract_semantics_name_agnostic_named_constructor_mapper_uses_function_body_shape() {
        let source_type = make_nullary_constructor_type("workflow", "Stage");
        let output_type = make_nullary_constructor_type("approval", "Decision");
        let (mapper_key, mapper_fn) = make_named_nullary_constructor_mapper_function(
            "collapse_stage",
            "workflow",
            "Stage",
            "approval",
            "Decision",
            &[("Idle", "Deny"), ("Busy", "Deny")],
            3,
        );
        let mut functions = empty_known_functions();
        functions.insert(&mapper_key, &mapper_fn);

        let via = make_typed_map_call(
            make_leaf_fuzzer_call("stage_fuzzer", source_type.clone()),
            module_fn_var(
                "collapse_stage",
                "math",
                Type::function(vec![source_type.clone()], output_type.clone()),
            ),
            output_type.clone(),
        );

        let mut owned_data_types =
            make_nullary_constructor_data_types("workflow", "Stage", &["Idle", "Busy"]);
        owned_data_types.extend(make_nullary_constructor_data_types(
            "approval",
            "Decision",
            &["Allow", "Deny", "Review"],
        ));
        let data_types: IndexMap<&DataTypeKey, &TypedDataType> = owned_data_types.iter().collect();

        let semantics =
            extract_semantics_from_via(&via, "math", &functions, &data_types, output_type.as_ref());

        assert_eq!(semantics, FuzzerSemantics::Constructors { tags: vec![1] });
    }

    #[test]
    fn extract_semantics_name_agnostic_constructor_map_requires_total_mapping() {
        let source_type = make_nullary_constructor_type("workflow", "Stage");
        let output_type = make_nullary_constructor_type("approval", "Decision");
        let via = make_typed_map_call(
            make_leaf_fuzzer_call("stage_fuzzer", source_type.clone()),
            make_nullary_constructor_mapper(
                "workflow",
                "Stage",
                "approval",
                "Decision",
                &[("Idle", "Allow")],
                3,
            ),
            output_type.clone(),
        );

        let mut owned_data_types =
            make_nullary_constructor_data_types("workflow", "Stage", &["Idle", "Busy"]);
        owned_data_types.extend(make_nullary_constructor_data_types(
            "approval",
            "Decision",
            &["Allow", "Deny", "Review"],
        ));
        let data_types: IndexMap<&DataTypeKey, &TypedDataType> = owned_data_types.iter().collect();

        let semantics = extract_semantics_from_via(
            &via,
            "math",
            &empty_known_functions(),
            &data_types,
            output_type.as_ref(),
        );

        // Partial constructor map falls back to the default semantics for the
        // output type, which is now the full nullary constructor domain for
        // `approval.Decision`. This remains a sound over-approximation.
        assert_eq!(
            semantics,
            FuzzerSemantics::Constructors {
                tags: vec![0, 1, 2]
            }
        );
    }

    #[test]
    fn extract_semantics_name_agnostic_constructor_map_unresolved_mapper_falls_back_to_default() {
        let source_type = make_nullary_constructor_type("workflow", "Stage");
        let output_type = make_nullary_constructor_type("approval", "Decision");
        let via = make_typed_map_call(
            make_leaf_fuzzer_call("stage_fuzzer", source_type.clone()),
            make_unresolved_unary_mapper_with_types(
                "next_stage",
                source_type.clone(),
                output_type.clone(),
            ),
            output_type.clone(),
        );

        let mut owned_data_types =
            make_nullary_constructor_data_types("workflow", "Stage", &["Idle", "Busy"]);
        owned_data_types.extend(make_nullary_constructor_data_types(
            "approval",
            "Decision",
            &["Allow", "Deny", "Review"],
        ));
        let data_types: IndexMap<&DataTypeKey, &TypedDataType> = owned_data_types.iter().collect();

        let semantics = extract_semantics_from_via(
            &via,
            "math",
            &empty_known_functions(),
            &data_types,
            output_type.as_ref(),
        );

        // Unresolved mapper falls back to the default semantics for the
        // output type, which is now the full nullary constructor domain for
        // `approval.Decision`. This remains a sound over-approximation.
        assert_eq!(
            semantics,
            FuzzerSemantics::Constructors {
                tags: vec![0, 1, 2]
            }
        );
    }

    #[test]
    fn extract_semantics_name_agnostic_constructor_map_recursive_cycle_falls_back_to_default() {
        let source_type = make_nullary_constructor_type("workflow", "Stage");
        let output_type = make_nullary_constructor_type("approval", "Decision");
        let mapper_type = Type::function(vec![source_type.clone()], output_type.clone());

        let (left_key, left_fn) = make_zero_arg_function(
            "left_mapper",
            mapper_type.clone(),
            make_zero_arg_call("right_mapper", mapper_type.clone()),
        );
        let (right_key, right_fn) = make_zero_arg_function(
            "right_mapper",
            mapper_type.clone(),
            make_zero_arg_call("left_mapper", mapper_type.clone()),
        );
        let mut functions = empty_known_functions();
        functions.insert(&left_key, &left_fn);
        functions.insert(&right_key, &right_fn);

        let via = make_typed_map_call(
            make_leaf_fuzzer_call("stage_fuzzer", source_type.clone()),
            make_zero_arg_call("left_mapper", mapper_type),
            output_type.clone(),
        );

        let mut owned_data_types =
            make_nullary_constructor_data_types("workflow", "Stage", &["Idle", "Busy"]);
        owned_data_types.extend(make_nullary_constructor_data_types(
            "approval",
            "Decision",
            &["Allow", "Deny", "Review"],
        ));
        let data_types: IndexMap<&DataTypeKey, &TypedDataType> = owned_data_types.iter().collect();

        let semantics =
            extract_semantics_from_via(&via, "math", &functions, &data_types, output_type.as_ref());

        // Recursive mapper cycle falls back to the default semantics for the
        // output type, which is now the full nullary constructor domain for
        // `approval.Decision`. This remains a sound over-approximation.
        assert_eq!(
            semantics,
            FuzzerSemantics::Constructors {
                tags: vec![0, 1, 2]
            }
        );
    }

    #[test]
    fn extract_semantics_name_agnostic_bind_uses_continuation_shape() {
        // The continuation's source `int_between(5, 8)` now produces the
        // closed range [5, 8] via the stdlib recognizer; the bind's
        // semantic output reflects the continuation's bounds.
        let via = make_typed_bind_call(
            make_leaf_fuzzer_call("seed", Type::int()),
            make_inline_bind_continuation(
                "x",
                Type::int(),
                make_typed_int_between_fuzzer("5", "8"),
                Type::int(),
            ),
            Type::int(),
        );
        let data_types: IndexMap<&DataTypeKey, &TypedDataType> = IndexMap::new();

        let semantics = extract_semantics_from_via(
            &via,
            "math",
            &empty_known_functions(),
            &data_types,
            Type::int().as_ref(),
        );
        assert_eq!(
            semantics,
            FuzzerSemantics::IntRange {
                min: Some("5".to_string()),
                max: Some("8".to_string()),
            }
        );
    }

    #[test]
    fn extract_semantics_name_agnostic_product_uses_element_shapes() {
        // Both components now extract their bounds via the stdlib
        // recognizer; the product semantics records the tuple shape with
        // each element's closed range preserved.
        let via = make_typed_product_call(
            make_typed_int_between_fuzzer("0", "10"),
            make_typed_int_between_fuzzer("20", "30"),
        );
        let data_types: IndexMap<&DataTypeKey, &TypedDataType> = IndexMap::new();

        let semantics = extract_semantics_from_via(
            &via,
            "math",
            &empty_known_functions(),
            &data_types,
            Type::tuple(vec![Type::int(), Type::int()]).as_ref(),
        );
        assert_eq!(
            semantics,
            FuzzerSemantics::Product(vec![
                FuzzerSemantics::IntRange {
                    min: Some("0".to_string()),
                    max: Some("10".to_string()),
                },
                FuzzerSemantics::IntRange {
                    min: Some("20".to_string()),
                    max: Some("30".to_string()),
                },
            ])
        );
    }

    #[test]
    fn extract_semantics_name_agnostic_list_uses_element_shape() {
        // The element fuzzer is now recognized, producing a closed range
        // element semantic. The enclosing list call is not a stdlib
        // `list_*` function, so length bounds remain absent.
        let via = make_typed_list_call(make_typed_int_between_fuzzer("0", "10"), Type::int());
        let data_types: IndexMap<&DataTypeKey, &TypedDataType> = IndexMap::new();

        let semantics = extract_semantics_from_via(
            &via,
            "math",
            &empty_known_functions(),
            &data_types,
            Type::list(Type::int()).as_ref(),
        );
        assert_eq!(
            semantics,
            FuzzerSemantics::List {
                element: Box::new(FuzzerSemantics::IntRange {
                    min: Some("0".to_string()),
                    max: Some("10".to_string()),
                }),
                min_len: None,
                max_len: None,
            }
        );
    }

    #[test]
    fn extract_semantics_name_agnostic_map_falls_back_to_default_for_unknown_mapper() {
        // Previously, an unknown mapper shape produced `Opaque` semantics
        // which caused downstream Lean emission to abort with a "semantic
        // domain is opaque" error. The new behavior over-approximates to
        // the default semantics for the output type, allowing universal
        // quantification (a sound widening) to proceed instead. Even though
        // the source now carries bounds, `Map(_)` with an opaque mapper
        // collapses to the default unbounded `IntRange`.
        let via = make_typed_map_call(
            make_typed_int_between_fuzzer("1", "3"),
            make_unresolved_unary_mapper("f", Type::int()),
            Type::int(),
        );
        let data_types: IndexMap<&DataTypeKey, &TypedDataType> = IndexMap::new();

        let semantics = extract_semantics_from_via(
            &via,
            "math",
            &empty_known_functions(),
            &data_types,
            Type::int().as_ref(),
        );
        assert_eq!(
            semantics,
            FuzzerSemantics::IntRange {
                min: None,
                max: None,
            }
        );
    }

    #[test]
    fn extract_semantics_name_agnostic_identity_map_preserves_source_domain() {
        // The source `int_between(1, 3)` now carries [1, 3]; an identity
        // map preserves that range unchanged in the semantic layer.
        let via = make_typed_map_call(
            make_typed_int_between_fuzzer("1", "3"),
            make_identity_mapper("n", Type::int()),
            Type::int(),
        );
        let data_types: IndexMap<&DataTypeKey, &TypedDataType> = IndexMap::new();

        let semantics = extract_semantics_from_via(
            &via,
            "math",
            &empty_known_functions(),
            &data_types,
            Type::int().as_ref(),
        );
        assert_eq!(
            semantics,
            FuzzerSemantics::IntRange {
                min: Some("1".to_string()),
                max: Some("3".to_string()),
            }
        );
    }

    #[test]
    fn extract_semantics_name_agnostic_named_identity_mapper_uses_function_body_shape() {
        // The inner source now carries bounds via the stdlib recognizer;
        // a named identity mapper structurally collapses the map, so the
        // outer semantics are the source's closed range.
        let (identity_key, identity_fn) =
            make_named_unary_identity_mapper_function("identity", Type::int());
        let mut functions = empty_known_functions();
        functions.insert(&identity_key, &identity_fn);
        let data_types: IndexMap<&DataTypeKey, &TypedDataType> = IndexMap::new();

        let via = make_typed_map_call(
            make_typed_int_between_fuzzer("1", "3"),
            module_fn_var(
                "identity",
                "math",
                Type::function(vec![Type::int()], Type::int()),
            ),
            Type::int(),
        );

        let semantics =
            extract_semantics_from_via(&via, "math", &functions, &data_types, Type::int().as_ref());
        assert_eq!(
            semantics,
            FuzzerSemantics::IntRange {
                min: Some("1".to_string()),
                max: Some("3".to_string()),
            }
        );
    }

    #[test]
    fn extract_semantics_finite_string_if_mapper_is_oneof() {
        let via = make_typed_map_call(
            make_typed_int_between_fuzzer("0", "3"),
            finite_string_if_mapper(),
            Type::string(),
        );
        let data_types: IndexMap<&DataTypeKey, &TypedDataType> = IndexMap::new();

        let semantics = extract_semantics_from_via(
            &via,
            "math",
            &empty_known_functions(),
            &data_types,
            Type::string().as_ref(),
        );
        assert_eq!(
            semantics,
            FuzzerSemantics::OneOf(expected_finite_string_values())
        );
    }

    #[test]
    fn extract_semantics_helper_wrapped_finite_string_mapper_is_oneof() {
        let (mapper_key, mapper_fn) = make_named_finite_string_mapper_function("label_for_i");
        let mut functions = empty_known_functions();
        functions.insert(&mapper_key, &mapper_fn);
        let data_types: IndexMap<&DataTypeKey, &TypedDataType> = IndexMap::new();

        let via = make_typed_map_call(
            make_typed_int_between_fuzzer("0", "3"),
            module_fn_var(
                "label_for_i",
                "math",
                Type::function(vec![Type::int()], Type::string()),
            ),
            Type::string(),
        );

        let semantics = extract_semantics_from_via(
            &via,
            "math",
            &functions,
            &data_types,
            Type::string().as_ref(),
        );
        assert_eq!(
            semantics,
            FuzzerSemantics::OneOf(expected_finite_string_values())
        );
    }

    #[test]
    fn extract_semantics_finite_string_source_range_above_cap_stays_generic_string() {
        let via = make_typed_map_call(
            make_typed_int_between_fuzzer("0", "999999999999999999999999999999999999999999"),
            finite_string_if_mapper(),
            Type::string(),
        );
        let data_types: IndexMap<&DataTypeKey, &TypedDataType> = IndexMap::new();

        let semantics = extract_semantics_from_via(
            &via,
            "math",
            &empty_known_functions(),
            &data_types,
            Type::string().as_ref(),
        );
        assert_eq!(semantics, FuzzerSemantics::String);
    }

    #[test]
    fn extract_semantics_finite_string_source_range_outside_i128_above_cap_uses_bigint() {
        let via = make_typed_map_call(
            make_typed_int_between_fuzzer(
                "170141183460469231731687303715884105728",
                "170141183460469231731687303715884105828",
            ),
            finite_string_if_mapper(),
            Type::string(),
        );
        let data_types: IndexMap<&DataTypeKey, &TypedDataType> = IndexMap::new();

        let semantics = extract_semantics_from_via(
            &via,
            "math",
            &empty_known_functions(),
            &data_types,
            Type::string().as_ref(),
        );
        assert_eq!(semantics, FuzzerSemantics::String);
    }

    #[test]
    fn extract_semantics_finite_string_non_literal_mapper_stays_generic_string() {
        let mapper = make_unary_mapper(
            "i",
            Type::int(),
            Type::string(),
            local_var("unresolved_label", Type::string()),
        );
        let via = make_typed_map_call(
            make_typed_int_between_fuzzer("0", "3"),
            mapper,
            Type::string(),
        );
        let data_types: IndexMap<&DataTypeKey, &TypedDataType> = IndexMap::new();

        let semantics = extract_semantics_from_via(
            &via,
            "math",
            &empty_known_functions(),
            &data_types,
            Type::string().as_ref(),
        );
        assert_eq!(semantics, FuzzerSemantics::String);
    }

    #[test]
    fn extract_semantics_finite_string_mixed_literal_outputs_stay_generic_string() {
        let body = TypedExpr::If {
            location: Span::empty(),
            tipo: Type::string(),
            branches: vec1::vec1![IfBranch {
                condition: int_eq_expr("i", "0"),
                body: string_lit("ok"),
                is: None,
                location: Span::empty(),
            }],
            final_else: Box::new(TypedExpr::ByteArray {
                location: Span::empty(),
                tipo: Type::byte_array(),
                bytes: vec![0],
                preferred_format: crate::ast::ByteArrayFormatPreference::HexadecimalString,
            }),
        };
        let via = make_typed_map_call(
            make_typed_int_between_fuzzer("0", "1"),
            make_unary_mapper("i", Type::int(), Type::string(), body),
            Type::string(),
        );
        let data_types: IndexMap<&DataTypeKey, &TypedDataType> = IndexMap::new();

        let semantics = extract_semantics_from_via(
            &via,
            "math",
            &empty_known_functions(),
            &data_types,
            Type::string().as_ref(),
        );
        assert_eq!(semantics, FuzzerSemantics::String);
    }

    #[test]
    fn extract_semantics_name_agnostic_constant_bool_map_is_exact() {
        let via = make_typed_map_call(
            make_leaf_fuzzer_call("seed", Type::int()),
            make_constant_bool_mapper(Type::int(), true),
            Type::bool(),
        );
        let data_types: IndexMap<&DataTypeKey, &TypedDataType> = IndexMap::new();

        let semantics = extract_semantics_from_via(
            &via,
            "math",
            &empty_known_functions(),
            &data_types,
            Type::bool().as_ref(),
        );
        assert_eq!(
            semantics,
            FuzzerSemantics::Exact(FuzzerExactValue::Bool(true))
        );
    }

    #[test]
    fn extract_semantics_named_tautology_bool_map_is_exact() {
        let (mapper_key, mapper_fn) =
            make_named_unary_tautology_mapper_function("tautology", Type::bool());
        let mut functions = empty_known_functions();
        functions.insert(&mapper_key, &mapper_fn);
        let data_types: IndexMap<&DataTypeKey, &TypedDataType> = IndexMap::new();

        let via = make_typed_map_call(
            make_typed_bool_fuzzer(),
            module_fn_var(
                "tautology",
                "math",
                Type::function(vec![Type::bool()], Type::bool()),
            ),
            Type::bool(),
        );

        let semantics = extract_semantics_from_via(
            &via,
            "math",
            &functions,
            &data_types,
            Type::bool().as_ref(),
        );
        assert_eq!(
            semantics,
            FuzzerSemantics::Exact(FuzzerExactValue::Bool(true))
        );
    }

    #[test]
    fn extract_semantics_name_agnostic_constant_int_map_is_singleton_range() {
        let via = make_typed_map_call(
            make_leaf_fuzzer_call("seed", Type::int()),
            make_constant_int_mapper(Type::int(), "41"),
            Type::int(),
        );
        let data_types: IndexMap<&DataTypeKey, &TypedDataType> = IndexMap::new();

        let semantics = extract_semantics_from_via(
            &via,
            "math",
            &empty_known_functions(),
            &data_types,
            Type::int().as_ref(),
        );
        assert_eq!(
            semantics,
            FuzzerSemantics::IntRange {
                min: Some("41".to_string()),
                max: Some("41".to_string()),
            }
        );
    }

    #[test]
    fn extract_semantics_name_agnostic_named_constant_int_mapper_uses_function_body_shape() {
        let (mapper_key, mapper_fn) =
            make_named_unary_constant_int_mapper_function("always_7", "7");
        let mut functions = empty_known_functions();
        functions.insert(&mapper_key, &mapper_fn);
        let data_types: IndexMap<&DataTypeKey, &TypedDataType> = IndexMap::new();

        let via = make_typed_map_call(
            make_leaf_fuzzer_call("seed", Type::int()),
            module_fn_var(
                "always_7",
                "math",
                Type::function(vec![Type::int()], Type::int()),
            ),
            Type::int(),
        );

        let semantics =
            extract_semantics_from_via(&via, "math", &functions, &data_types, Type::int().as_ref());
        assert_eq!(
            semantics,
            FuzzerSemantics::IntRange {
                min: Some("7".to_string()),
                max: Some("7".to_string()),
            }
        );
    }

    #[test]
    fn extract_semantics_name_agnostic_nested_constant_then_affine_map_transforms_range() {
        let source = make_typed_map_call(
            make_leaf_fuzzer_call("seed", Type::int()),
            make_constant_int_mapper(Type::int(), "41"),
            Type::int(),
        );
        let via = make_typed_map_call(source, make_add_int_mapper("1"), Type::int());
        let data_types: IndexMap<&DataTypeKey, &TypedDataType> = IndexMap::new();

        let semantics = extract_semantics_from_via(
            &via,
            "math",
            &empty_known_functions(),
            &data_types,
            Type::int().as_ref(),
        );
        assert_eq!(
            semantics,
            FuzzerSemantics::IntRange {
                min: Some("42".to_string()),
                max: Some("42".to_string()),
            }
        );
    }

    #[test]
    fn extract_constraint_unknown_typed_fuzzer_shape_is_unsupported() {
        let via = make_typed_bind_call(
            make_leaf_fuzzer_call("seed", Type::int()),
            uint_lit("0"),
            Type::int(),
        );

        let constraint = extract_constraint_from_via(&via, "math", &empty_known_functions());
        assert!(matches!(constraint, FuzzerConstraint::Unsupported { .. }));
    }

    #[test]
    fn extract_semantics_unknown_typed_fuzzer_shape_is_opaque() {
        let via = make_typed_bind_call(
            make_leaf_fuzzer_call("seed", Type::int()),
            uint_lit("0"),
            Type::int(),
        );
        let data_types: IndexMap<&DataTypeKey, &TypedDataType> = IndexMap::new();

        let semantics = extract_semantics_from_via(
            &via,
            "math",
            &empty_known_functions(),
            &data_types,
            Type::int().as_ref(),
        );
        assert!(matches!(semantics, FuzzerSemantics::Opaque { .. }));
    }

    #[test]
    fn map2_identity_mapper_preserves_order() {
        let int_tipo = Type::int();
        let mapper = make_map2_mapper(vec![
            local_var("a", int_tipo.clone()),
            local_var("b", int_tipo),
        ]);

        let functions = empty_known_functions();
        let function_index = index_known_functions(&functions);
        assert_eq!(
            map2_mapper_arg_order(&mapper, "math", &function_index, &BTreeMap::new()),
            Some([0, 1])
        );
    }

    #[test]
    fn map2_swapped_mapper_reports_swapped_order() {
        let int_tipo = Type::int();
        let mapper = make_map2_mapper(vec![
            local_var("b", int_tipo.clone()),
            local_var("a", int_tipo),
        ]);

        let functions = empty_known_functions();
        let function_index = index_known_functions(&functions);
        assert_eq!(
            map2_mapper_arg_order(&mapper, "math", &function_index, &BTreeMap::new()),
            Some([1, 0])
        );
    }

    #[test]
    fn extract_constraint_map3_permuted_mapper_is_unsupported() {
        let int_tipo = Type::int();
        let arg_names = vec!["a".to_string(), "b".to_string(), "c".to_string()];
        let mapper = make_mapn_mapper(
            &arg_names,
            vec![
                local_var("c", int_tipo.clone()),
                local_var("a", int_tipo.clone()),
                local_var("b", int_tipo),
            ],
        );

        let via = make_mapn_via(
            "map3",
            vec![
                make_int_between_via("0", "9"),
                make_int_between_via("10", "19"),
                make_int_between_via("20", "29"),
            ],
            mapper,
        );

        let functions = empty_known_functions();
        let constraint = extract_constraint_from_via(&via, "math", &functions);
        assert!(matches!(constraint, FuzzerConstraint::Unsupported { .. }));
    }

    #[test]
    fn extract_constraint_map10_reverse_mapper_is_unsupported() {
        let int_tipo = Type::int();
        let arg_names: Vec<String> = (0..10).map(|i| format!("a{i}")).collect();
        let mapper_elems: Vec<TypedExpr> = arg_names
            .iter()
            .rev()
            .map(|name| local_var(name, int_tipo.clone()))
            .collect();
        let mapper = make_mapn_mapper(&arg_names, mapper_elems);

        let fuzzers: Vec<TypedExpr> = (0..10)
            .map(|i| {
                let min = (i * 10).to_string();
                let max = (i * 10 + 9).to_string();
                make_int_between_via(&min, &max)
            })
            .collect();
        let via = make_mapn_via("map10", fuzzers, mapper);

        let functions = empty_known_functions();
        let constraint = extract_constraint_from_via(&via, "math", &functions);
        assert!(matches!(constraint, FuzzerConstraint::Unsupported { .. }));
    }

    #[test]
    fn extract_constraint_map2_identity_mapper_is_unsupported() {
        let int_tipo = Type::int();
        let mapper = make_map2_mapper(vec![
            local_var("a", int_tipo.clone()),
            local_var("b", int_tipo),
        ]);

        let via = make_map2_via(
            make_int_between_via("0", "10"),
            make_int_between_via("20", "30"),
            mapper,
        );

        let functions = empty_known_functions();
        let constraint = extract_constraint_from_via(&via, "math", &functions);
        assert!(matches!(constraint, FuzzerConstraint::Unsupported { .. }));
    }

    #[test]
    fn extract_constraint_map2_swapped_mapper_is_unsupported() {
        let int_tipo = Type::int();
        let mapper = make_map2_mapper(vec![
            local_var("b", int_tipo.clone()),
            local_var("a", int_tipo),
        ]);

        let via = make_map2_via(
            make_int_between_via("0", "10"),
            make_int_between_via("20", "30"),
            mapper,
        );

        let functions = empty_known_functions();
        let constraint = extract_constraint_from_via(&via, "math", &functions);
        assert!(matches!(constraint, FuzzerConstraint::Unsupported { .. }));
    }

    #[test]
    fn extract_constraint_ignores_local_int_between_name_collision() {
        let via = TypedExpr::Call {
            location: Span::empty(),
            tipo: Type::int(),
            fun: Box::new(local_var(
                "int_between",
                Type::function(vec![Type::int(), Type::int()], Type::int()),
            )),
            args: vec![call_arg(uint_lit("0")), call_arg(uint_lit("10"))],
        };

        let functions = empty_known_functions();
        assert!(matches!(
            extract_constraint_from_via(&via, "math", &functions),
            FuzzerConstraint::Unsupported { .. }
        ));
    }

    #[test]
    fn extract_constraint_does_not_resolve_local_shadow_to_module_function() {
        let (fn_key, fn_def) = make_zero_arg_function(
            "wrapped_fuzzer",
            Type::int(),
            make_int_between_via("0", "10"),
        );
        let mut functions = empty_known_functions();
        functions.insert(&fn_key, &fn_def);

        // Local variable has the same identifier as a module function, but must
        // not be resolved as that function by name.
        let via = TypedExpr::Call {
            location: Span::empty(),
            tipo: Type::int(),
            fun: Box::new(local_var(
                "wrapped_fuzzer",
                Type::function(vec![], Type::int()),
            )),
            args: vec![],
        };

        assert!(matches!(
            extract_constraint_from_via(&via, "math", &functions),
            FuzzerConstraint::Unsupported { .. }
        ));
    }

    #[test]
    fn extract_constraint_ignores_non_fuzz_module_int_between() {
        let via = TypedExpr::Call {
            location: Span::empty(),
            tipo: Type::int(),
            fun: Box::new(module_fn_var(
                "int_between",
                "my/custom/module",
                Type::function(vec![Type::int(), Type::int()], Type::int()),
            )),
            args: vec![call_arg(uint_lit("0")), call_arg(uint_lit("10"))],
        };

        let functions = empty_known_functions();
        assert!(matches!(
            extract_constraint_from_via(&via, "math", &functions),
            FuzzerConstraint::Unsupported { .. }
        ));
    }

    #[test]
    fn extract_constraint_ignores_local_map2_name_collision() {
        let int_tipo = Type::int();
        let mapper = make_map2_mapper(vec![
            local_var("a", int_tipo.clone()),
            local_var("b", int_tipo),
        ]);

        let via = TypedExpr::Call {
            location: Span::empty(),
            tipo: Type::tuple(vec![Type::int(), Type::int()]),
            fun: Box::new(local_var(
                "map2",
                Type::function(
                    vec![Type::int(), Type::int(), Type::int()],
                    Type::tuple(vec![Type::int(), Type::int()]),
                ),
            )),
            args: vec![
                call_arg(make_int_between_via("0", "10")),
                call_arg(make_int_between_via("20", "30")),
                call_arg(mapper),
            ],
        };

        let functions = empty_known_functions();
        assert!(matches!(
            extract_constraint_from_via(&via, "math", &functions),
            FuzzerConstraint::Unsupported { .. }
        ));
    }

    #[test]
    fn extract_constraint_map2_does_not_resolve_local_mapper_shadow() {
        let int_tipo = Type::int();
        let (fn_key, fn_def) = make_named_map2_mapper_function(
            "pair_mapper",
            vec![
                local_var("a", int_tipo.clone()),
                local_var("b", int_tipo.clone()),
            ],
        );
        let mut functions = empty_known_functions();
        functions.insert(&fn_key, &fn_def);

        let via = make_map2_via(
            make_int_between_via("0", "10"),
            make_int_between_via("20", "30"),
            local_var(
                "pair_mapper",
                Type::function(
                    vec![Type::int(), Type::int()],
                    Type::tuple(vec![Type::int(), Type::int()]),
                ),
            ),
        );

        assert!(matches!(
            extract_constraint_from_via(&via, "math", &functions),
            FuzzerConstraint::Unsupported { .. }
        ));
    }

    #[test]
    fn extract_constraint_map2_named_mapper_without_definition_is_unsupported() {
        let mapper = make_named_map2_mapper("int_pair");
        let via = make_map2_via(
            make_int_between_via("0", "10"),
            make_int_between_via("20", "30"),
            mapper,
        );

        let functions = empty_known_functions();
        assert!(matches!(
            extract_constraint_from_via(&via, "math", &functions),
            FuzzerConstraint::Unsupported { .. }
        ));
    }

    #[test]
    fn extract_constraint_map2_named_identity_mapper_uses_function_definition() {
        let int_tipo = Type::int();
        let (fn_key, fn_def) = make_named_map2_mapper_function(
            "int_pair",
            vec![local_var("a", int_tipo.clone()), local_var("b", int_tipo)],
        );

        let mut functions = empty_known_functions();
        functions.insert(&fn_key, &fn_def);

        let mapper = make_named_map2_mapper("int_pair");
        let via = make_map2_via(
            make_int_between_via("0", "10"),
            make_int_between_via("20", "30"),
            mapper,
        );

        let constraint = extract_constraint_from_via(&via, "math", &functions);
        assert!(matches!(constraint, FuzzerConstraint::Unsupported { .. }));
    }

    #[test]
    fn extract_constraint_map2_named_swapped_mapper_uses_function_definition() {
        let int_tipo = Type::int();
        let (fn_key, fn_def) = make_named_map2_mapper_function(
            "swapped_int_pair",
            vec![local_var("b", int_tipo.clone()), local_var("a", int_tipo)],
        );

        let mut functions = empty_known_functions();
        functions.insert(&fn_key, &fn_def);

        let mapper = make_named_map2_mapper("swapped_int_pair");
        let via = make_map2_via(
            make_int_between_via("0", "10"),
            make_int_between_via("20", "30"),
            mapper,
        );

        let constraint = extract_constraint_from_via(&via, "math", &functions);
        assert!(matches!(constraint, FuzzerConstraint::Unsupported { .. }));
    }

    #[test]
    fn extract_constraint_map2_with_partially_applied_named_mapper_reorders() {
        let int_tipo = Type::int();
        let tuple_tipo = Type::tuple(vec![int_tipo.clone(), int_tipo.clone()]);
        let mapper_key = FunctionAccessKey {
            module_name: "math".to_string(),
            function_name: "pair_with_flag".to_string(),
        };
        let mapper_fn = TypedFunction {
            arguments: vec![
                TypedArg::new("flag", int_tipo.clone()),
                TypedArg::new("a", int_tipo.clone()),
                TypedArg::new("b", int_tipo.clone()),
            ],
            body: TypedExpr::Tuple {
                location: Span::empty(),
                tipo: tuple_tipo.clone(),
                elems: vec![
                    local_var("b", int_tipo.clone()),
                    local_var("a", int_tipo.clone()),
                ],
            },
            doc: None,
            location: Span::empty(),
            name: "pair_with_flag".to_string(),
            public: false,
            return_annotation: None,
            return_type: tuple_tipo.clone(),
            end_position: 0,
            on_test_failure: OnTestFailure::FailImmediately,
        };

        let mut functions = empty_known_functions();
        functions.insert(&mapper_key, &mapper_fn);

        let mapper = TypedExpr::Call {
            location: Span::empty(),
            tipo: Type::function(vec![Type::int(), Type::int()], tuple_tipo.clone()),
            fun: Box::new(module_fn_var(
                "pair_with_flag",
                "math",
                Type::function(
                    vec![Type::int(), Type::int(), Type::int()],
                    tuple_tipo.clone(),
                ),
            )),
            args: vec![call_arg(uint_lit("1"))],
        };

        let via = make_map2_via(
            make_int_between_via("0", "10"),
            make_int_between_via("20", "30"),
            mapper,
        );

        let constraint = extract_constraint_from_via(&via, "math", &functions);
        assert!(matches!(constraint, FuzzerConstraint::Unsupported { .. }));
    }

    #[test]
    fn extract_constraint_int_between_basic() {
        let via = make_int_between_via("5", "100");
        let functions = empty_known_functions();
        assert!(matches!(
            extract_constraint_from_via(&via, "math", &functions),
            FuzzerConstraint::Unsupported { .. }
        ));
    }

    #[test]
    fn extract_constraint_int_between_uses_plutus_floor_division_for_negatives() {
        let lower = TypedExpr::BinOp {
            location: Span::empty(),
            tipo: Type::int(),
            name: BinOp::DivInt,
            left: Box::new(negate_expr(uint_lit("8"))),
            right: Box::new(uint_lit("3")),
        };
        let upper = TypedExpr::BinOp {
            location: Span::empty(),
            tipo: Type::int(),
            name: BinOp::ModInt,
            left: Box::new(negate_expr(uint_lit("8"))),
            right: Box::new(uint_lit("3")),
        };
        let via = TypedExpr::Call {
            location: Span::empty(),
            tipo: Type::int(),
            fun: Box::new(fuzz_var(
                "int_between",
                Type::function(vec![Type::int(), Type::int()], Type::int()),
            )),
            args: vec![call_arg(lower), call_arg(upper)],
        };

        let functions = empty_known_functions();
        assert!(matches!(
            extract_constraint_from_via(&via, "math", &functions),
            FuzzerConstraint::Unsupported { .. }
        ));
    }

    #[test]
    fn extract_constraint_int_between_uses_plutus_modulo_divisor_sign() {
        let lower = TypedExpr::BinOp {
            location: Span::empty(),
            tipo: Type::int(),
            name: BinOp::DivInt,
            left: Box::new(uint_lit("8")),
            right: Box::new(negate_expr(uint_lit("3"))),
        };
        let upper = TypedExpr::BinOp {
            location: Span::empty(),
            tipo: Type::int(),
            name: BinOp::ModInt,
            left: Box::new(uint_lit("8")),
            right: Box::new(negate_expr(uint_lit("3"))),
        };
        let via = TypedExpr::Call {
            location: Span::empty(),
            tipo: Type::int(),
            fun: Box::new(fuzz_var(
                "int_between",
                Type::function(vec![Type::int(), Type::int()], Type::int()),
            )),
            args: vec![call_arg(lower), call_arg(upper)],
        };

        let functions = empty_known_functions();
        assert!(matches!(
            extract_constraint_from_via(&via, "math", &functions),
            FuzzerConstraint::Unsupported { .. }
        ));
    }

    #[test]
    fn extract_constraint_int_between_does_not_resolve_local_shadow_constant() {
        let via = TypedExpr::Call {
            location: Span::empty(),
            tipo: Type::int(),
            fun: Box::new(fuzz_var(
                "int_between",
                Type::function(vec![Type::int(), Type::int()], Type::int()),
            )),
            args: vec![
                call_arg(local_var("bound", Type::int())),
                call_arg(uint_lit("10")),
            ],
        };

        let functions = empty_known_functions();
        let mut constants = empty_known_constants();
        let key = FunctionAccessKey {
            module_name: "math".to_string(),
            function_name: "bound".to_string(),
        };
        let value = uint_lit("0");
        constants.insert(&key, &value);

        assert!(matches!(
            extract_constraint_from_via_with_constants(&via, "math", &functions, &constants),
            FuzzerConstraint::Unsupported { .. }
        ));
    }

    #[test]
    fn extract_constraint_int_between_with_module_constant_bounds() {
        let via = TypedExpr::Call {
            location: Span::empty(),
            tipo: Type::int(),
            fun: Box::new(fuzz_var(
                "int_between",
                Type::function(vec![Type::int(), Type::int()], Type::int()),
            )),
            args: vec![
                call_arg(module_const_var(
                    "core_development",
                    "permissions_examples",
                    Type::int(),
                )),
                call_arg(module_const_var(
                    "core_development",
                    "permissions_examples",
                    Type::int(),
                )),
            ],
        };

        let functions = empty_known_functions();
        let mut constants = empty_known_constants();
        let key = FunctionAccessKey {
            module_name: "permissions_examples".to_string(),
            function_name: "core_development".to_string(),
        };
        let value = uint_lit("0");
        constants.insert(&key, &value);

        assert!(matches!(
            extract_constraint_from_via_with_constants(
                &via,
                "permissions_examples",
                &functions,
                &constants,
            ),
            FuzzerConstraint::Unsupported { .. }
        ));
    }

    #[test]
    fn extract_constraint_int_between_with_nested_module_constant_bounds() {
        let via = TypedExpr::Call {
            location: Span::empty(),
            tipo: Type::int(),
            fun: Box::new(fuzz_var(
                "int_between",
                Type::function(vec![Type::int(), Type::int()], Type::int()),
            )),
            args: vec![
                call_arg(module_const_var(
                    "core_development",
                    "permissions_examples",
                    Type::int(),
                )),
                call_arg(module_const_var(
                    "core_development",
                    "permissions_examples",
                    Type::int(),
                )),
            ],
        };

        let functions = empty_known_functions();
        let mut constants = empty_known_constants();

        let key_core = FunctionAccessKey {
            module_name: "permissions_examples".to_string(),
            function_name: "core_development".to_string(),
        };
        let key_base = FunctionAccessKey {
            module_name: "permissions_examples".to_string(),
            function_name: "base_scope".to_string(),
        };

        let value_core = module_const_var("base_scope", "permissions_examples", Type::int());
        let value_base = uint_lit("0");

        constants.insert(&key_core, &value_core);
        constants.insert(&key_base, &value_base);

        assert!(matches!(
            extract_constraint_from_via_with_constants(
                &via,
                "permissions_examples",
                &functions,
                &constants,
            ),
            FuzzerConstraint::Unsupported { .. }
        ));
    }

    #[test]
    fn extract_constraint_int_no_args() {
        let via = TypedExpr::Call {
            location: Span::empty(),
            tipo: Type::int(),
            fun: Box::new(fuzz_var("int", Type::function(vec![], Type::int()))),
            args: vec![],
        };
        let functions = empty_known_functions();
        assert!(matches!(
            extract_constraint_from_via(&via, "math", &functions),
            FuzzerConstraint::Unsupported { .. }
        ));
    }

    #[test]
    fn extract_constraint_int_at_least_small() {
        let via = TypedExpr::Call {
            location: Span::empty(),
            tipo: Type::int(),
            fun: Box::new(fuzz_var(
                "int_at_least",
                Type::function(vec![Type::int()], Type::int()),
            )),
            args: vec![call_arg(uint_lit("10"))],
        };
        let functions = empty_known_functions();
        assert!(matches!(
            extract_constraint_from_via(&via, "math", &functions),
            FuzzerConstraint::Unsupported { .. }
        ));
    }

    #[test]
    fn extract_constraint_int_at_most_small() {
        let via = TypedExpr::Call {
            location: Span::empty(),
            tipo: Type::int(),
            fun: Box::new(fuzz_var(
                "int_at_most",
                Type::function(vec![Type::int()], Type::int()),
            )),
            args: vec![call_arg(uint_lit("10"))],
        };
        let functions = empty_known_functions();
        assert!(matches!(
            extract_constraint_from_via(&via, "math", &functions),
            FuzzerConstraint::Unsupported { .. }
        ));
    }

    #[test]
    fn extract_constraint_constant_int() {
        let via = TypedExpr::Call {
            location: Span::empty(),
            tipo: Type::int(),
            fun: Box::new(fuzz_var(
                "constant",
                Type::function(vec![Type::int()], Type::int()),
            )),
            args: vec![call_arg(uint_lit("42"))],
        };
        let functions = empty_known_functions();
        assert!(matches!(
            extract_constraint_from_via(&via, "math", &functions),
            FuzzerConstraint::Unsupported { .. }
        ));
    }

    #[test]
    fn extract_constraint_map_wraps_inner() {
        let via = TypedExpr::Call {
            location: Span::empty(),
            tipo: Type::int(),
            fun: Box::new(fuzz_var(
                "map",
                Type::function(vec![Type::int(), Type::int()], Type::int()),
            )),
            args: vec![
                call_arg(make_int_between_via("0", "10")),
                call_arg(local_var(
                    "f",
                    Type::function(vec![Type::int()], Type::int()),
                )),
            ],
        };
        let functions = empty_known_functions();
        let constraint = extract_constraint_from_via(&via, "math", &functions);
        assert!(matches!(constraint, FuzzerConstraint::Unsupported { .. }));
    }

    #[test]
    fn extract_constraint_map_with_named_negate_mapper_transforms_bounds() {
        let (negate_key, negate_fn) = make_named_unary_negate_mapper_function("negate");
        let mut functions = empty_known_functions();
        functions.insert(&negate_key, &negate_fn);

        let via = make_map_via(
            make_int_between_via("1", "50"),
            module_fn_var(
                "negate",
                "math",
                Type::function(vec![Type::int()], Type::int()),
            ),
        );

        let constraint = extract_constraint_from_via(&via, "math", &functions);
        assert!(matches!(constraint, FuzzerConstraint::Unsupported { .. }));
    }

    #[test]
    fn extract_constraint_such_that_preserves_inner_domain() {
        // A filter (such_that) propagates the source's constraint unchanged.
        // Under the stdlib-gated recognizer, `int_between(1, 50)` extracts
        // `IntRange { 1, 50 }`; the filter then propagates that range.
        let source = make_typed_int_between_fuzzer("1", "50");
        let predicate = make_bool_predicate("x", Type::int());
        let via = make_typed_filter_call(source, predicate);
        let functions = empty_known_functions();
        let constraint = extract_constraint_from_via(&via, "math", &functions);

        assert_eq!(
            constraint,
            FuzzerConstraint::IntRange {
                min: "1".to_string(),
                max: "50".to_string(),
            }
        );
    }

    #[test]
    fn extract_constraint_such_that_const_false_preserves_inner_domain() {
        let source = make_typed_int_between_fuzzer("1", "50");
        let predicate = make_constant_bool_mapper(Type::int(), false);
        let via = make_typed_filter_call(source, predicate);
        let functions = empty_known_functions();
        let constraint = extract_constraint_from_via(&via, "math", &functions);

        assert_eq!(
            constraint,
            FuzzerConstraint::IntRange {
                min: "1".to_string(),
                max: "50".to_string(),
            }
        );
    }

    #[test]
    fn extract_constraint_bool_such_that_identity_preserves_direct_bool_domain() {
        let source = make_typed_bool_fuzzer();
        let predicate = make_identity_mapper("flag", Type::bool());
        let via = make_typed_filter_call(source, predicate);
        let functions = empty_known_functions();
        let constraint = extract_constraint_from_via(&via, "math", &functions);

        assert_eq!(constraint, FuzzerConstraint::Any);
    }

    #[test]
    fn extract_constraint_bool_such_that_identity_preserves_opaque_source_domain() {
        let source = make_leaf_fuzzer_call("seed", Type::bool());
        let predicate = make_identity_mapper("flag", Type::bool());
        let via = make_typed_filter_call(source, predicate);
        let functions = empty_known_functions();
        let constraint = extract_constraint_from_via(&via, "math", &functions);

        assert_eq!(constraint, FuzzerConstraint::Any);
    }

    #[test]
    fn extract_semantics_bool_such_that_identity_preserves_direct_bool_domain() {
        let source = make_typed_bool_fuzzer();
        let predicate = make_identity_mapper("flag", Type::bool());
        let via = make_typed_filter_call(source, predicate);
        let data_types: IndexMap<&DataTypeKey, &TypedDataType> = IndexMap::new();

        let semantics = extract_semantics_from_via(
            &via,
            "math",
            &empty_known_functions(),
            &data_types,
            Type::bool().as_ref(),
        );
        assert_eq!(semantics, FuzzerSemantics::Bool);
    }

    #[test]
    fn extract_semantics_bool_such_that_identity_preserves_opaque_source_domain() {
        let source = make_leaf_fuzzer_call("seed", Type::bool());
        let predicate = make_identity_mapper("flag", Type::bool());
        let via = make_typed_filter_call(source, predicate);
        let data_types: IndexMap<&DataTypeKey, &TypedDataType> = IndexMap::new();

        let semantics = extract_semantics_from_via(
            &via,
            "math",
            &empty_known_functions(),
            &data_types,
            Type::bool().as_ref(),
        );
        assert_eq!(semantics, FuzzerSemantics::Bool);
    }

    #[test]
    fn normalize_fuzzer_bool_map_negation_stays_map() {
        let via = make_typed_map_call(
            make_typed_bool_fuzzer(),
            make_not_bool_mapper("b"),
            Type::bool(),
        );

        let normalized = normalize_fuzzer_from_via(&via, "math", &empty_known_functions());
        assert_normalized_map(normalized);
    }

    #[test]
    fn extract_constraint_map_to_bool_is_not_filter() {
        // A map from Int to Bool is NOT a filter: source payload (Int) != output payload (Bool).
        // This should remain classified as Map, not be collapsed to the source.
        let source = make_typed_int_between_fuzzer("1", "50");
        let mapper =
            make_unresolved_unary_mapper_with_types("is_positive", Type::int(), Type::bool());
        let via = make_typed_map_call(source, mapper, Type::bool());
        let functions = empty_known_functions();
        let constraint = extract_constraint_from_via(&via, "math", &functions);

        assert!(
            matches!(constraint, FuzzerConstraint::Map(_)),
            "map-to-Bool should stay as Map, got: {:?}",
            constraint
        );
    }

    #[test]
    fn extract_constraint_map_with_partially_applied_named_mapper_transforms_bounds() {
        let int_tipo = Type::int();
        let mapper_key = FunctionAccessKey {
            module_name: "math".to_string(),
            function_name: "negate_with_offset".to_string(),
        };
        let mapper_fn = TypedFunction {
            arguments: vec![
                TypedArg::new("offset", int_tipo.clone()),
                TypedArg::new("n", int_tipo.clone()),
            ],
            body: negate_expr(local_var("n", int_tipo.clone())),
            doc: None,
            location: Span::empty(),
            name: "negate_with_offset".to_string(),
            public: false,
            return_annotation: None,
            return_type: int_tipo.clone(),
            end_position: 0,
            on_test_failure: OnTestFailure::FailImmediately,
        };
        let mut functions = empty_known_functions();
        functions.insert(&mapper_key, &mapper_fn);

        let mapper = TypedExpr::Call {
            location: Span::empty(),
            tipo: Type::function(vec![Type::int()], Type::int()),
            fun: Box::new(module_fn_var(
                "negate_with_offset",
                "math",
                Type::function(vec![Type::int(), Type::int()], Type::int()),
            )),
            args: vec![call_arg(uint_lit("0"))],
        };
        let via = make_map_via(make_int_between_via("1", "50"), mapper);

        let constraint = extract_constraint_from_via(&via, "math", &functions);
        assert!(matches!(constraint, FuzzerConstraint::Unsupported { .. }));
    }

    #[test]
    fn extract_constraint_zero_arg_helper_call_is_unwrapped() {
        let (helper_key, helper_fn) =
            make_zero_arg_function("helper_fuzzer", Type::int(), make_int_between_via("3", "7"));
        let mut functions = empty_known_functions();
        functions.insert(&helper_key, &helper_fn);

        let via = make_zero_arg_call("helper_fuzzer", Type::int());
        let constraint = extract_constraint_from_via(&via, "math", &functions);

        assert!(matches!(constraint, FuzzerConstraint::Unsupported { .. }));
    }

    #[test]
    fn extract_constraint_zero_arg_helper_with_map_negate_transforms_bounds() {
        let (negate_key, negate_fn) = make_named_unary_negate_mapper_function("negate");
        let (fuzzer_key, fuzzer_fn) = make_zero_arg_function(
            "negate_fuzzer",
            Type::int(),
            make_map_via(
                make_int_between_via("1", "50"),
                module_fn_var(
                    "negate",
                    "math",
                    Type::function(vec![Type::int()], Type::int()),
                ),
            ),
        );
        let mut functions = empty_known_functions();
        functions.insert(&negate_key, &negate_fn);
        functions.insert(&fuzzer_key, &fuzzer_fn);

        let via = make_zero_arg_call("negate_fuzzer", Type::int());
        let constraint = extract_constraint_from_via(&via, "math", &functions);

        assert!(matches!(constraint, FuzzerConstraint::Unsupported { .. }));
    }

    #[test]
    fn extract_constraint_parameterized_helper_inlines_arguments() {
        let lo_var = local_var("lo", Type::int());
        let upper = TypedExpr::BinOp {
            location: Span::empty(),
            tipo: Type::int(),
            name: BinOp::AddInt,
            left: Box::new(lo_var.clone()),
            right: Box::new(uint_lit("5")),
        };

        let helper_key = FunctionAccessKey {
            module_name: "math".to_string(),
            function_name: "bounded".to_string(),
        };
        let helper_fn = TypedFunction {
            arguments: vec![TypedArg::new("lo", Type::int())],
            body: TypedExpr::Call {
                location: Span::empty(),
                tipo: Type::int(),
                fun: Box::new(fuzz_var(
                    "int_between",
                    Type::function(vec![Type::int(), Type::int()], Type::int()),
                )),
                args: vec![call_arg(lo_var), call_arg(upper)],
            },
            doc: None,
            location: Span::empty(),
            name: "bounded".to_string(),
            public: false,
            return_annotation: None,
            return_type: Type::int(),
            end_position: 0,
            on_test_failure: OnTestFailure::FailImmediately,
        };

        let mut functions = empty_known_functions();
        functions.insert(&helper_key, &helper_fn);

        let via = TypedExpr::Call {
            location: Span::empty(),
            tipo: Type::int(),
            fun: Box::new(module_fn_var(
                "bounded",
                "math",
                Type::function(vec![Type::int()], Type::int()),
            )),
            args: vec![call_arg(uint_lit("7"))],
        };

        assert!(matches!(
            extract_constraint_from_via(&via, "math", &functions),
            FuzzerConstraint::Unsupported { .. }
        ));
    }

    #[test]
    fn extract_constraint_parameterized_helper_preserves_caller_local_aliases() {
        let lo_var = local_var("lo", Type::int());
        let upper = TypedExpr::BinOp {
            location: Span::empty(),
            tipo: Type::int(),
            name: BinOp::AddInt,
            left: Box::new(lo_var.clone()),
            right: Box::new(uint_lit("5")),
        };

        let helper_key = FunctionAccessKey {
            module_name: "math".to_string(),
            function_name: "bounded".to_string(),
        };
        let helper_fn = TypedFunction {
            arguments: vec![TypedArg::new("lo", Type::int())],
            body: TypedExpr::Call {
                location: Span::empty(),
                tipo: Type::int(),
                fun: Box::new(fuzz_var(
                    "int_between",
                    Type::function(vec![Type::int(), Type::int()], Type::int()),
                )),
                args: vec![call_arg(lo_var), call_arg(upper)],
            },
            doc: None,
            location: Span::empty(),
            name: "bounded".to_string(),
            public: false,
            return_annotation: None,
            return_type: Type::int(),
            end_position: 0,
            on_test_failure: OnTestFailure::FailImmediately,
        };

        let mut functions = empty_known_functions();
        functions.insert(&helper_key, &helper_fn);

        let via = TypedExpr::Sequence {
            location: Span::empty(),
            expressions: vec![
                TypedExpr::Assignment {
                    location: Span::empty(),
                    tipo: Type::int(),
                    value: Box::new(uint_lit("7")),
                    pattern: TypedPattern::var("lo"),
                    kind: crate::ast::AssignmentKind::Let { backpassing: () },
                    comment: None,
                },
                TypedExpr::Call {
                    location: Span::empty(),
                    tipo: Type::int(),
                    fun: Box::new(module_fn_var(
                        "bounded",
                        "math",
                        Type::function(vec![Type::int()], Type::int()),
                    )),
                    args: vec![call_arg(local_var("lo", Type::int()))],
                },
            ],
        };

        assert!(matches!(
            extract_constraint_from_via(&via, "math", &functions),
            FuzzerConstraint::Unsupported { .. }
        ));
    }

    #[test]
    fn extract_constraint_partial_int_between_alias_is_resolved() {
        let int_between_tipo = Type::function(vec![Type::int(), Type::int()], Type::int());
        let between_from_two_tipo = Type::function(vec![Type::int()], Type::int());

        let via = TypedExpr::Sequence {
            location: Span::empty(),
            expressions: vec![
                TypedExpr::Assignment {
                    location: Span::empty(),
                    tipo: between_from_two_tipo.clone(),
                    value: Box::new(TypedExpr::Call {
                        location: Span::empty(),
                        tipo: between_from_two_tipo.clone(),
                        fun: Box::new(fuzz_var("int_between", int_between_tipo)),
                        args: vec![call_arg(uint_lit("2"))],
                    }),
                    pattern: TypedPattern::var("between_from_two"),
                    kind: crate::ast::AssignmentKind::Let { backpassing: () },
                    comment: None,
                },
                TypedExpr::Call {
                    location: Span::empty(),
                    tipo: Type::int(),
                    fun: Box::new(local_var("between_from_two", between_from_two_tipo)),
                    args: vec![call_arg(uint_lit("9"))],
                },
            ],
        };

        let functions = empty_known_functions();
        assert!(matches!(
            extract_constraint_from_via(&via, "math", &functions),
            FuzzerConstraint::Unsupported { .. }
        ));
    }

    #[test]
    fn extract_constraint_partial_helper_alias_is_resolved() {
        let bounded_key = FunctionAccessKey {
            module_name: "math".to_string(),
            function_name: "bounded".to_string(),
        };
        let bounded_fn = TypedFunction {
            arguments: vec![
                TypedArg::new("lo", Type::int()),
                TypedArg::new("hi", Type::int()),
            ],
            body: TypedExpr::Call {
                location: Span::empty(),
                tipo: Type::int(),
                fun: Box::new(fuzz_var(
                    "int_between",
                    Type::function(vec![Type::int(), Type::int()], Type::int()),
                )),
                args: vec![
                    call_arg(local_var("lo", Type::int())),
                    call_arg(local_var("hi", Type::int())),
                ],
            },
            doc: None,
            location: Span::empty(),
            name: "bounded".to_string(),
            public: false,
            return_annotation: None,
            return_type: Type::int(),
            end_position: 0,
            on_test_failure: OnTestFailure::FailImmediately,
        };

        let bounded_tipo = Type::function(vec![Type::int(), Type::int()], Type::int());
        let at_most_ten_tipo = Type::function(vec![Type::int()], Type::int());
        let via = TypedExpr::Sequence {
            location: Span::empty(),
            expressions: vec![
                TypedExpr::Assignment {
                    location: Span::empty(),
                    tipo: at_most_ten_tipo.clone(),
                    value: Box::new(TypedExpr::Call {
                        location: Span::empty(),
                        tipo: at_most_ten_tipo.clone(),
                        fun: Box::new(module_fn_var("bounded", "math", bounded_tipo)),
                        args: vec![call_arg(uint_lit("0"))],
                    }),
                    pattern: TypedPattern::var("at_most_ten"),
                    kind: crate::ast::AssignmentKind::Let { backpassing: () },
                    comment: None,
                },
                TypedExpr::Call {
                    location: Span::empty(),
                    tipo: Type::int(),
                    fun: Box::new(local_var("at_most_ten", at_most_ten_tipo)),
                    args: vec![call_arg(uint_lit("10"))],
                },
            ],
        };

        let mut functions = empty_known_functions();
        functions.insert(&bounded_key, &bounded_fn);

        assert!(matches!(
            extract_constraint_from_via(&via, "math", &functions),
            FuzzerConstraint::Unsupported { .. }
        ));
    }

    #[test]
    fn extract_constraint_recursive_helper_is_unsupported() {
        let helper_key = FunctionAccessKey {
            module_name: "math".to_string(),
            function_name: "looping".to_string(),
        };
        let helper_fn = TypedFunction {
            arguments: vec![],
            body: TypedExpr::Call {
                location: Span::empty(),
                tipo: Type::int(),
                fun: Box::new(module_fn_var(
                    "looping",
                    "math",
                    Type::function(vec![], Type::int()),
                )),
                args: vec![],
            },
            doc: None,
            location: Span::empty(),
            name: "looping".to_string(),
            public: false,
            return_annotation: None,
            return_type: Type::int(),
            end_position: 0,
            on_test_failure: OnTestFailure::FailImmediately,
        };

        let mut functions = empty_known_functions();
        functions.insert(&helper_key, &helper_fn);

        let via = make_zero_arg_call("looping", Type::int());
        assert!(matches!(
            extract_constraint_from_via(&via, "math", &functions),
            FuzzerConstraint::Unsupported { .. }
        ));
    }

    #[test]
    fn extract_semantics_recursive_fuzzer_wrapper_cycle_is_opaque() {
        // See `normalize_fuzzer_recursive_wrapper_cycle_is_opaque` for the
        // rationale: mutually-recursive cycles without a base case are
        // kept opaque so that downstream verification can surface a real
        // error rather than silently widen to "any value of T".
        let (left_key, left_fn) = make_zero_arg_fuzzer_function(
            "left",
            Type::int(),
            make_zero_arg_fuzzer_call("right", Type::int()),
        );
        let (right_key, right_fn) = make_zero_arg_fuzzer_function(
            "right",
            Type::int(),
            make_zero_arg_fuzzer_call("left", Type::int()),
        );
        let mut functions = empty_known_functions();
        functions.insert(&left_key, &left_fn);
        functions.insert(&right_key, &right_fn);

        let via = make_zero_arg_fuzzer_call("left", Type::int());
        let data_types: IndexMap<&DataTypeKey, &TypedDataType> = IndexMap::new();
        let semantics =
            extract_semantics_from_via(&via, "math", &functions, &data_types, Type::int().as_ref());

        assert!(
            matches!(
                &semantics,
                FuzzerSemantics::Opaque { reason }
                    if reason.contains("recursive helper fuzzer detected")
            ),
            "recursive helper semantics must remain conservative: {semantics:?}"
        );
    }

    #[test]
    fn extract_constraint_sequence_tracks_local_alias_bindings() {
        let between_tipo = Type::function(vec![Type::int(), Type::int()], Type::int());
        let sequence = TypedExpr::Sequence {
            location: Span::empty(),
            expressions: vec![
                TypedExpr::Assignment {
                    location: Span::empty(),
                    tipo: between_tipo.clone(),
                    value: Box::new(fuzz_var("int_between", between_tipo.clone())),
                    pattern: TypedPattern::var("between"),
                    kind: crate::ast::AssignmentKind::Let { backpassing: () },
                    comment: None,
                },
                TypedExpr::Call {
                    location: Span::empty(),
                    tipo: Type::int(),
                    fun: Box::new(local_var("between", between_tipo)),
                    args: vec![call_arg(uint_lit("2")), call_arg(uint_lit("9"))],
                },
            ],
        };

        let functions = empty_known_functions();
        assert!(matches!(
            extract_constraint_from_via(&sequence, "math", &functions),
            FuzzerConstraint::Unsupported { .. }
        ));
    }

    #[test]
    fn extract_constraint_list_between_rejects_negative_or_reversed_bounds() {
        let negative_min = TypedExpr::Call {
            location: Span::empty(),
            tipo: Type::int(),
            fun: Box::new(fuzz_var(
                "list_between",
                Type::function(vec![Type::int(), Type::int(), Type::int()], Type::int()),
            )),
            args: vec![
                call_arg(make_int_between_via("0", "10")),
                call_arg(negate_expr(uint_lit("1"))),
                call_arg(uint_lit("5")),
            ],
        };

        let reversed = TypedExpr::Call {
            location: Span::empty(),
            tipo: Type::int(),
            fun: Box::new(fuzz_var(
                "list_between",
                Type::function(vec![Type::int(), Type::int(), Type::int()], Type::int()),
            )),
            args: vec![
                call_arg(make_int_between_via("0", "10")),
                call_arg(uint_lit("6")),
                call_arg(uint_lit("2")),
            ],
        };

        let functions = empty_known_functions();
        assert!(matches!(
            extract_constraint_from_via(&negative_min, "math", &functions),
            FuzzerConstraint::Unsupported { .. }
        ));
        assert!(matches!(
            extract_constraint_from_via(&reversed, "math", &functions),
            FuzzerConstraint::Unsupported { .. }
        ));
    }

    #[test]
    fn extract_constraint_tuple4_collects_component_bounds() {
        let via = make_tuple4_via(
            make_int_between_via("0", "5"),
            make_int_between_via("10", "15"),
            make_int_between_via("20", "25"),
            make_int_between_via("30", "35"),
        );
        let functions = empty_known_functions();
        let constraint = extract_constraint_from_via(&via, "math", &functions);

        assert!(matches!(constraint, FuzzerConstraint::Unsupported { .. }));
    }

    #[test]
    fn extract_constraint_and_then_requires_resolvable_continuation() {
        let via = TypedExpr::Call {
            location: Span::empty(),
            tipo: Type::int(),
            fun: Box::new(fuzz_var(
                "and_then",
                Type::function(vec![Type::int(), Type::int()], Type::int()),
            )),
            args: vec![
                call_arg(make_int_between_via("1", "5")),
                call_arg(local_var(
                    "f",
                    Type::function(vec![Type::int()], Type::int()),
                )),
            ],
        };
        let functions = empty_known_functions();
        assert!(matches!(
            extract_constraint_from_via(&via, "math", &functions),
            FuzzerConstraint::Unsupported { .. }
        ));
    }

    #[test]
    fn extract_constraint_and_then_uses_continuation_output_domain_only() {
        let list_int_tipo = Type::list(Type::int());
        let continuation = TypedExpr::Fn {
            location: Span::empty(),
            tipo: Type::function(vec![Type::int()], list_int_tipo.clone()),
            is_capture: false,
            args: vec![TypedArg::new("n", Type::int())],
            body: Box::new(TypedExpr::Call {
                location: Span::empty(),
                tipo: list_int_tipo.clone(),
                fun: Box::new(fuzz_var(
                    "list",
                    Type::function(vec![Type::int()], list_int_tipo.clone()),
                )),
                args: vec![call_arg(make_int_between_via("0", "3"))],
            }),
            return_annotation: None,
        };

        let via = TypedExpr::Call {
            location: Span::empty(),
            tipo: list_int_tipo.clone(),
            fun: Box::new(fuzz_var(
                "and_then",
                Type::function(
                    vec![
                        Type::int(),
                        Type::function(vec![Type::int()], list_int_tipo.clone()),
                    ],
                    list_int_tipo,
                ),
            )),
            args: vec![
                call_arg(make_int_between_via("1", "5")),
                call_arg(continuation),
            ],
        };

        let functions = empty_known_functions();
        assert!(matches!(
            extract_constraint_from_via(&via, "math", &functions),
            FuzzerConstraint::Unsupported { .. }
        ));
    }

    #[test]
    fn extract_constraint_and_then_with_helper_returning_lambda_is_resolved() {
        let int_tipo = Type::int();
        let continuation_key = FunctionAccessKey {
            module_name: "math".to_string(),
            function_name: "build_window_continuation".to_string(),
        };
        let continuation_fn = TypedFunction {
            arguments: vec![TypedArg::new("lo", int_tipo.clone())],
            body: TypedExpr::Fn {
                location: Span::empty(),
                tipo: Type::function(vec![int_tipo.clone()], int_tipo.clone()),
                is_capture: false,
                args: vec![TypedArg::new("n", int_tipo.clone())],
                body: Box::new(TypedExpr::Call {
                    location: Span::empty(),
                    tipo: int_tipo.clone(),
                    fun: Box::new(fuzz_var(
                        "int_between",
                        Type::function(vec![Type::int(), Type::int()], Type::int()),
                    )),
                    args: vec![
                        call_arg(local_var("lo", int_tipo.clone())),
                        call_arg(uint_lit("10")),
                    ],
                }),
                return_annotation: None,
            },
            doc: None,
            location: Span::empty(),
            name: "build_window_continuation".to_string(),
            public: false,
            return_annotation: None,
            return_type: Type::function(vec![int_tipo.clone()], int_tipo.clone()),
            end_position: 0,
            on_test_failure: OnTestFailure::FailImmediately,
        };

        let mut functions = empty_known_functions();
        functions.insert(&continuation_key, &continuation_fn);

        let continuation = TypedExpr::Call {
            location: Span::empty(),
            tipo: Type::function(vec![Type::int()], Type::int()),
            fun: Box::new(module_fn_var(
                "build_window_continuation",
                "math",
                Type::function(
                    vec![Type::int()],
                    Type::function(vec![Type::int()], Type::int()),
                ),
            )),
            args: vec![call_arg(uint_lit("2"))],
        };
        let via = make_and_then_via(make_int_between_via("1", "5"), continuation, Type::int());

        assert!(matches!(
            extract_constraint_from_via(&via, "math", &functions),
            FuzzerConstraint::Unsupported { .. }
        ));
    }

    #[test]
    fn extract_constraint_and_then_with_partially_applied_multiarg_helper_is_resolved() {
        let int_tipo = Type::int();
        let continuation_key = FunctionAccessKey {
            module_name: "math".to_string(),
            function_name: "bounded_continuation".to_string(),
        };
        let continuation_fn = TypedFunction {
            arguments: vec![
                TypedArg::new("lo", int_tipo.clone()),
                TypedArg::new("hi", int_tipo.clone()),
                TypedArg::new("n", int_tipo.clone()),
            ],
            body: TypedExpr::Call {
                location: Span::empty(),
                tipo: int_tipo.clone(),
                fun: Box::new(fuzz_var(
                    "int_between",
                    Type::function(vec![Type::int(), Type::int()], Type::int()),
                )),
                args: vec![
                    call_arg(local_var("lo", int_tipo.clone())),
                    call_arg(local_var("hi", int_tipo.clone())),
                ],
            },
            doc: None,
            location: Span::empty(),
            name: "bounded_continuation".to_string(),
            public: false,
            return_annotation: None,
            return_type: int_tipo.clone(),
            end_position: 0,
            on_test_failure: OnTestFailure::FailImmediately,
        };

        let mut functions = empty_known_functions();
        functions.insert(&continuation_key, &continuation_fn);

        let continuation = TypedExpr::Call {
            location: Span::empty(),
            tipo: Type::function(vec![Type::int()], Type::int()),
            fun: Box::new(module_fn_var(
                "bounded_continuation",
                "math",
                Type::function(vec![Type::int(), Type::int(), Type::int()], Type::int()),
            )),
            args: vec![call_arg(uint_lit("2")), call_arg(uint_lit("9"))],
        };
        let via = make_and_then_via(make_int_between_via("1", "5"), continuation, Type::int());

        assert!(matches!(
            extract_constraint_from_via(&via, "math", &functions),
            FuzzerConstraint::Unsupported { .. }
        ));
    }

    #[test]
    fn extract_constraint_both_produces_tuple() {
        let via = TypedExpr::Call {
            location: Span::empty(),
            tipo: Type::tuple(vec![Type::int(), Type::int()]),
            fun: Box::new(fuzz_var(
                "both",
                Type::function(
                    vec![Type::int(), Type::int()],
                    Type::tuple(vec![Type::int(), Type::int()]),
                ),
            )),
            args: vec![
                call_arg(make_int_between_via("0", "10")),
                call_arg(make_int_between_via("20", "30")),
            ],
        };
        let functions = empty_known_functions();
        assert!(matches!(
            extract_constraint_from_via(&via, "math", &functions),
            FuzzerConstraint::Unsupported { .. }
        ));
    }

    #[test]
    fn extract_constraint_list_no_bounds() {
        let via = TypedExpr::Call {
            location: Span::empty(),
            tipo: Type::int(), // simplification
            fun: Box::new(fuzz_var(
                "list",
                Type::function(vec![Type::int()], Type::int()),
            )),
            args: vec![call_arg(make_int_between_via("0", "10"))],
        };
        let functions = empty_known_functions();
        assert!(matches!(
            extract_constraint_from_via(&via, "math", &functions),
            FuzzerConstraint::Unsupported { .. }
        ));
    }

    #[test]
    fn extract_constraint_list_between_with_bounds() {
        let via = TypedExpr::Call {
            location: Span::empty(),
            tipo: Type::int(),
            fun: Box::new(fuzz_var(
                "list_between",
                Type::function(vec![Type::int(), Type::int(), Type::int()], Type::int()),
            )),
            args: vec![
                call_arg(make_int_between_via("0", "10")),
                call_arg(uint_lit("2")),
                call_arg(uint_lit("5")),
            ],
        };
        let functions = empty_known_functions();
        assert!(matches!(
            extract_constraint_from_via(&via, "math", &functions),
            FuzzerConstraint::Unsupported { .. }
        ));
    }

    fn make_typed_bytearray_between_fuzzer(min: &str, max: &str) -> TypedExpr {
        let output_type = Type::byte_array();
        let fuzzer_type = Type::fuzzer(output_type.clone());
        TypedExpr::Call {
            location: Span::empty(),
            tipo: fuzzer_type.clone(),
            fun: Box::new(module_fn_var(
                "bytearray_between",
                STDLIB_FUZZ_MODULE,
                Type::function(vec![Type::int(), Type::int()], fuzzer_type),
            )),
            args: vec![call_arg(uint_lit(min)), call_arg(uint_lit(max))],
        }
    }

    fn make_typed_bytearray_at_most_fuzzer(max: &str) -> TypedExpr {
        let output_type = Type::byte_array();
        let fuzzer_type = Type::fuzzer(output_type.clone());
        TypedExpr::Call {
            location: Span::empty(),
            tipo: fuzzer_type.clone(),
            fun: Box::new(module_fn_var(
                "bytearray_at_most",
                STDLIB_FUZZ_MODULE,
                Type::function(vec![Type::int()], fuzzer_type),
            )),
            args: vec![call_arg(uint_lit(max))],
        }
    }

    fn make_typed_bytearray_at_least_fuzzer(min: &str) -> TypedExpr {
        let output_type = Type::byte_array();
        let fuzzer_type = Type::fuzzer(output_type.clone());
        TypedExpr::Call {
            location: Span::empty(),
            tipo: fuzzer_type.clone(),
            fun: Box::new(module_fn_var(
                "bytearray_at_least",
                STDLIB_FUZZ_MODULE,
                Type::function(vec![Type::int()], fuzzer_type),
            )),
            args: vec![call_arg(uint_lit(min))],
        }
    }

    #[test]
    fn extract_constraint_bytearray_between_with_bounds() {
        let via = make_typed_bytearray_between_fuzzer("2", "5");
        let functions = empty_known_functions();
        assert_eq!(
            extract_constraint_from_via(&via, "math", &functions),
            FuzzerConstraint::ByteStringLenRange {
                min_len: 2,
                max_len: 5,
            }
        );
    }

    #[test]
    fn extract_constraint_bytearray_between_equal_bounds_pins_length() {
        // Regression for `prop_from_script`-style tests that use
        // `fuzz.bytearray_between(28, 28)` to mint a fixed-width ByteArray
        // (here, a 28-byte script hash). Must produce an equal-bounds range
        // so downstream semantics yield `ByteArrayRange { Some(28), Some(28) }`
        // rather than falling through to `Unsupported` and triggering a skip.
        let via = make_typed_bytearray_between_fuzzer("28", "28");
        let functions = empty_known_functions();
        assert_eq!(
            extract_constraint_from_via(&via, "math", &functions),
            FuzzerConstraint::ByteStringLenRange {
                min_len: 28,
                max_len: 28,
            }
        );
    }

    #[test]
    fn extract_constraint_bytearray_between_swapped_bounds_are_normalized() {
        // Mirrors `int_between`'s swap-normalization: user-facing sugar that
        // tolerates `(hi, lo)` ordering must still land on `min ≤ max` so
        // downstream emission doesn't produce an empty or inverted domain.
        let via = make_typed_bytearray_between_fuzzer("7", "3");
        let functions = empty_known_functions();
        assert_eq!(
            extract_constraint_from_via(&via, "math", &functions),
            FuzzerConstraint::ByteStringLenRange {
                min_len: 3,
                max_len: 7,
            }
        );
    }

    #[test]
    fn extract_constraint_bytearray_at_most_emits_zero_min_bound() {
        // `bytearray_at_most(n)` bounds length to `[0, n]`. Length 0 is a
        // sound, representable lower bound for bytearrays — no sentinel dance
        // required (contrast `int_at_least`, whose lower bound is unbounded
        // and uses an `i128::MIN` sentinel).
        let via = make_typed_bytearray_at_most_fuzzer("32");
        let functions = empty_known_functions();
        assert_eq!(
            extract_constraint_from_via(&via, "math", &functions),
            FuzzerConstraint::ByteStringLenRange {
                min_len: 0,
                max_len: 32,
            }
        );
    }

    #[test]
    fn extract_constraint_bytearray_at_least_does_not_produce_bytestring_len_range() {
        // `bytearray_at_least(n)` has an unbounded upper length.
        // `ByteStringLenRange` cannot represent "unbounded max" without a
        // sentinel, and unlike `IntRange` no downstream sentinel-stripping
        // exists. We therefore do NOT return a `ByteStringLenRange` for this
        // callee — any other constraint shape (Unsupported or a wider
        // over-approximation) is acceptable, since it keeps the verifier
        // sound. The key negative assertion is that no `ByteStringLenRange`
        // with a fabricated upper bound leaks downstream.
        let via = make_typed_bytearray_at_least_fuzzer("4");
        let functions = empty_known_functions();
        let constraint = extract_constraint_from_via(&via, "math", &functions);
        assert!(
            !matches!(constraint, FuzzerConstraint::ByteStringLenRange { .. }),
            "bytearray_at_least must not fabricate a bounded length range; got {:?}",
            constraint,
        );
    }

    #[test]
    fn extract_constraint_pipeline_uses_last() {
        let via = TypedExpr::Pipeline {
            location: Span::empty(),
            expressions: vec![
                local_var("ignored", Type::int()),
                make_int_between_via("3", "7"),
            ],
        };
        let functions = empty_known_functions();
        assert!(matches!(
            extract_constraint_from_via(&via, "math", &functions),
            FuzzerConstraint::Unsupported { .. }
        ));
    }

    #[test]
    fn extract_constraint_unknown_function_returns_unsupported() {
        let via = TypedExpr::Call {
            location: Span::empty(),
            tipo: Type::int(),
            fun: Box::new(fuzz_var(
                "some_unknown_fuzzer",
                Type::function(vec![], Type::int()),
            )),
            args: vec![],
        };
        let functions = empty_known_functions();
        assert!(matches!(
            extract_constraint_from_via(&via, "math", &functions),
            FuzzerConstraint::Unsupported { .. }
        ));
    }

    fn scenario_semantics_fixture() -> (
        IndexMap<DataTypeKey, TypedDataType>,
        Rc<Type>,
        Rc<Type>,
        Rc<Type>,
    ) {
        let state_type = Rc::new(Type::App {
            public: true,
            contains_opaque: false,
            module: "permissions".to_string(),
            name: "State".to_string(),
            args: vec![],
            alias: None,
        });
        let input_type = Rc::new(Type::App {
            public: true,
            contains_opaque: false,
            module: "cardano/transaction".to_string(),
            name: "Input".to_string(),
            args: vec![],
            alias: None,
        });
        let transaction_type = Rc::new(Type::App {
            public: true,
            contains_opaque: false,
            module: "cardano/transaction".to_string(),
            name: "Transaction".to_string(),
            args: vec![],
            alias: None,
        });
        let state_generic = Type::generic_var(0);

        let scenario_data_type = TypedDataType {
            decorators: vec![],
            constructors: vec![
                RecordConstructor {
                    decorators: vec![],
                    location: Span::empty(),
                    name: "Done".to_string(),
                    arguments: vec![],
                    doc: None,
                    sugar: false,
                },
                RecordConstructor {
                    decorators: vec![],
                    location: Span::empty(),
                    name: "Step".to_string(),
                    arguments: vec![
                        RecordConstructorArg {
                            label: Some("labels".to_string()),
                            annotation: Annotation::Constructor {
                                location: Span::empty(),
                                module: None,
                                name: "List".to_string(),
                                arguments: vec![Annotation::Constructor {
                                    location: Span::empty(),
                                    module: None,
                                    name: "String".to_string(),
                                    arguments: vec![],
                                }],
                            },
                            location: Span::empty(),
                            tipo: Type::list(Type::string()),
                            doc: None,
                        },
                        RecordConstructorArg {
                            label: Some("state".to_string()),
                            annotation: Annotation::Var {
                                location: Span::empty(),
                                name: "st".to_string(),
                            },
                            location: Span::empty(),
                            tipo: state_generic.clone(),
                            doc: None,
                        },
                        RecordConstructorArg {
                            label: Some("event".to_string()),
                            annotation: Annotation::Constructor {
                                location: Span::empty(),
                                module: Some("cardano/transaction".to_string()),
                                name: "Transaction".to_string(),
                                arguments: vec![],
                            },
                            location: Span::empty(),
                            tipo: transaction_type.clone(),
                            doc: None,
                        },
                    ],
                    doc: None,
                    sugar: false,
                },
            ],
            doc: None,
            location: Span::empty(),
            name: "Scenario".to_string(),
            opaque: false,
            parameters: vec!["st".to_string()],
            public: true,
            typed_parameters: vec![state_generic],
        };

        let mut data_types = IndexMap::new();
        data_types.insert(
            DataTypeKey {
                module_name: STDLIB_FUZZ_SCENARIO_MODULE.to_string(),
                defined_type: "Scenario".to_string(),
            },
            scenario_data_type,
        );

        (data_types, state_type, input_type, transaction_type)
    }

    fn expected_scenario_transition_semantics() -> StateMachineTransitionSemantics {
        StateMachineTransitionSemantics {
            terminal_tag: 0,
            step_tag: 1,
            label_field_index: 0,
            next_state_field_index: 1,
            event_field_index: 2,
            state_semantics: Box::new(FuzzerSemantics::DataWithSchema {
                type_name: "permissions.State".to_string(),
            }),
            step_input_semantics: vec![FuzzerSemantics::List {
                element: Box::new(FuzzerSemantics::DataWithSchema {
                    type_name: "cardano/transaction.Input".to_string(),
                }),
                min_len: None,
                max_len: None,
            }],
            label_semantics: Box::new(FuzzerSemantics::List {
                element: Box::new(FuzzerSemantics::String),
                min_len: None,
                max_len: None,
            }),
            event_semantics: Box::new(FuzzerSemantics::DataWithSchema {
                type_name: "cardano/transaction.Transaction".to_string(),
            }),
        }
    }

    #[test]
    fn extract_constraint_scenario_like_non_fuzzer_call_is_unsupported() {
        let functions = empty_known_functions();

        for name in ["ok", "ko", "report_coverage"] {
            let via = TypedExpr::Call {
                location: Span::empty(),
                tipo: Type::int(),
                fun: Box::new(module_fn_var(
                    name,
                    STDLIB_FUZZ_SCENARIO_MODULE,
                    Type::function(vec![Type::int(), Type::int()], Type::int()),
                )),
                args: vec![
                    call_arg(make_int_between_via("0", "10")),
                    call_arg(uint_lit("0")),
                ],
            };

            assert!(
                matches!(
                    extract_constraint_from_via(&via, "math", &functions),
                    FuzzerConstraint::Unsupported { .. }
                ),
                "scenario-like call '{name}' without Fuzzer typing must fail closed"
            );
        }
    }

    #[test]
    fn normalize_fuzzer_state_machine_trace_shape_is_name_agnostic() {
        let (_owned_data_types, state_type, input_type, transaction_type) =
            scenario_semantics_fixture();
        let step_type = Type::function(
            vec![state_type.clone(), Type::list(input_type.clone())],
            Type::fuzzer(Rc::new(Type::App {
                public: true,
                contains_opaque: false,
                module: STDLIB_FUZZ_SCENARIO_MODULE.to_string(),
                name: "Scenario".to_string(),
                args: vec![state_type.clone()],
                alias: None,
            })),
        );
        let via = TypedExpr::Call {
            location: Span::empty(),
            tipo: Type::list(transaction_type.clone()),
            fun: Box::new(module_fn_var(
                "anything_but_ok",
                "math",
                Type::function(
                    vec![state_type.clone(), step_type.clone()],
                    Type::list(transaction_type),
                ),
            )),
            args: vec![
                call_arg(local_var("initial_state", state_type)),
                call_arg(local_var("step", step_type)),
            ],
        };
        let functions = empty_known_functions();

        assert!(matches!(
            normalize_fuzzer_from_via(&via, "permissions", &functions),
            NormalizedFuzzer::StateMachineTrace {
                acceptance: StateMachineAcceptance::AcceptsSuccess,
                ..
            }
        ));
    }

    #[test]
    fn normalize_fuzzer_state_machine_trace_rejects_known_non_fuzz_stdlib() {
        // The negative filter at normalize_state_machine_trace_from_call must reject
        // known stdlib modules that don't look like fuzz/test/scenario combinators.
        // This regression test pins that `aiken/list` is rejected even when its
        // structural signature matches (state, step_fn) -> List<T>.
        //
        // NOTE: The recognizer is intentionally name-agnostic (commit 2e6c95f0) and
        // accepts user modules with matching shape. Only known non-fuzz stdlib
        // modules are excluded by the negative filter. Converting to a positive
        // allowlist requires a team decision — do not change the filter here.
        let (_owned_data_types, state_type, input_type, transaction_type) =
            scenario_semantics_fixture();
        let step_type = Type::function(
            vec![state_type.clone(), Type::list(input_type.clone())],
            Type::fuzzer(Rc::new(Type::App {
                public: true,
                contains_opaque: false,
                module: STDLIB_FUZZ_SCENARIO_MODULE.to_string(),
                name: "Scenario".to_string(),
                args: vec![state_type.clone()],
                alias: None,
            })),
        );
        let via = TypedExpr::Call {
            location: Span::empty(),
            tipo: Type::list(transaction_type.clone()),
            fun: Box::new(module_fn_var(
                "fold",
                "aiken/list",
                Type::function(
                    vec![state_type.clone(), step_type.clone()],
                    Type::list(transaction_type),
                ),
            )),
            args: vec![
                call_arg(local_var("initial_state", state_type)),
                call_arg(local_var("step", step_type)),
            ],
        };
        let functions = empty_known_functions();

        let result = normalize_fuzzer_from_via(&via, "permissions", &functions);

        assert!(!matches!(
            result,
            NormalizedFuzzer::StateMachineTrace { .. }
        ));
    }

    #[test]
    fn extract_constraint_state_machine_trace_is_name_agnostic() {
        let (owned_data_types, state_type, input_type, transaction_type) =
            scenario_semantics_fixture();
        let data_types: IndexMap<&DataTypeKey, &TypedDataType> = owned_data_types.iter().collect();
        let step_type = Type::function(
            vec![state_type.clone(), Type::list(input_type.clone())],
            Type::fuzzer(Rc::new(Type::App {
                public: true,
                contains_opaque: false,
                module: STDLIB_FUZZ_SCENARIO_MODULE.to_string(),
                name: "Scenario".to_string(),
                args: vec![state_type.clone()],
                alias: None,
            })),
        );
        let via = TypedExpr::Call {
            location: Span::empty(),
            tipo: Type::list(transaction_type.clone()),
            fun: Box::new(module_fn_var(
                "anything_but_ok",
                "math",
                Type::function(
                    vec![state_type.clone(), step_type.clone()],
                    Type::list(transaction_type.clone()),
                ),
            )),
            args: vec![
                call_arg(local_var("initial_state", state_type.clone())),
                call_arg(local_var("step", step_type)),
            ],
        };
        let functions = empty_known_functions();

        assert_eq!(
            extract_constraint_from_via_with_data_types(
                &via,
                "permissions",
                &functions,
                &data_types,
            ),
            FuzzerConstraint::List {
                elem: Box::new(FuzzerConstraint::Any),
                min_len: Some(0),
                max_len: None,
            }
        );
    }

    #[test]
    fn extract_semantics_scenario_ok_is_state_machine_trace() {
        let (owned_data_types, state_type, input_type, transaction_type) =
            scenario_semantics_fixture();
        let data_types: IndexMap<&DataTypeKey, &TypedDataType> = owned_data_types.iter().collect();
        let step_type = Type::function(
            vec![state_type.clone(), Type::list(input_type.clone())],
            Type::fuzzer(Rc::new(Type::App {
                public: true,
                contains_opaque: false,
                module: STDLIB_FUZZ_SCENARIO_MODULE.to_string(),
                name: "Scenario".to_string(),
                args: vec![state_type.clone()],
                alias: None,
            })),
        );
        let via = TypedExpr::Call {
            location: Span::empty(),
            tipo: Type::list(transaction_type.clone()),
            fun: Box::new(module_fn_var(
                "ok",
                STDLIB_FUZZ_SCENARIO_MODULE,
                Type::function(
                    vec![state_type.clone(), step_type.clone()],
                    Type::list(transaction_type.clone()),
                ),
            )),
            args: vec![
                call_arg(local_var("initial_state", state_type.clone())),
                call_arg(local_var("step", step_type)),
            ],
        };
        let functions = empty_known_functions();

        assert_eq!(
            extract_semantics_from_via(
                &via,
                "permissions",
                &functions,
                &data_types,
                Type::list(transaction_type.clone()).as_ref(),
            ),
            FuzzerSemantics::StateMachineTrace {
                acceptance: StateMachineAcceptance::AcceptsSuccess,
                state_type: SemanticType::Unsupported("permissions.State".to_string()),
                step_input_types: vec![SemanticType::List(Box::new(SemanticType::Unsupported(
                    "cardano/transaction.Input".to_string()
                ),))],
                label_type: SemanticType::List(Box::new(SemanticType::String)),
                event_type: SemanticType::Unsupported(
                    "cardano/transaction.Transaction".to_string()
                ),
                transition_semantics: expected_scenario_transition_semantics(),
                output_semantics: Box::new(FuzzerSemantics::List {
                    element: Box::new(FuzzerSemantics::DataWithSchema {
                        type_name: "cardano/transaction.Transaction".to_string(),
                    }),
                    min_len: Some(0),
                    max_len: None,
                }),
                step_function_ir: None,
                step_ir_unsupported_reason: None,
                transition_prop: None,
                initial_state_shallow_ir: None,
            }
        );
    }

    #[test]
    fn extract_semantics_state_machine_trace_is_name_agnostic() {
        let (owned_data_types, state_type, input_type, transaction_type) =
            scenario_semantics_fixture();
        let data_types: IndexMap<&DataTypeKey, &TypedDataType> = owned_data_types.iter().collect();
        let step_type = Type::function(
            vec![state_type.clone(), Type::list(input_type.clone())],
            Type::fuzzer(Rc::new(Type::App {
                public: true,
                contains_opaque: false,
                module: STDLIB_FUZZ_SCENARIO_MODULE.to_string(),
                name: "Scenario".to_string(),
                args: vec![state_type.clone()],
                alias: None,
            })),
        );
        let via = TypedExpr::Call {
            location: Span::empty(),
            tipo: Type::list(transaction_type.clone()),
            fun: Box::new(module_fn_var(
                "anything_but_ok",
                "math",
                Type::function(
                    vec![state_type.clone(), step_type.clone()],
                    Type::list(transaction_type.clone()),
                ),
            )),
            args: vec![
                call_arg(local_var("initial_state", state_type.clone())),
                call_arg(local_var("step", step_type)),
            ],
        };
        let functions = empty_known_functions();

        assert_eq!(
            extract_semantics_from_via(
                &via,
                "permissions",
                &functions,
                &data_types,
                Type::list(transaction_type.clone()).as_ref(),
            ),
            FuzzerSemantics::StateMachineTrace {
                acceptance: StateMachineAcceptance::AcceptsSuccess,
                state_type: SemanticType::Unsupported("permissions.State".to_string()),
                step_input_types: vec![SemanticType::List(Box::new(SemanticType::Unsupported(
                    "cardano/transaction.Input".to_string()
                ),))],
                label_type: SemanticType::List(Box::new(SemanticType::String)),
                event_type: SemanticType::Unsupported(
                    "cardano/transaction.Transaction".to_string()
                ),
                transition_semantics: expected_scenario_transition_semantics(),
                output_semantics: Box::new(FuzzerSemantics::List {
                    element: Box::new(FuzzerSemantics::DataWithSchema {
                        type_name: "cardano/transaction.Transaction".to_string(),
                    }),
                    min_len: Some(0),
                    max_len: None,
                }),
                step_function_ir: None,
                step_ir_unsupported_reason: None,
                transition_prop: None,
                initial_state_shallow_ir: None,
            }
        );
    }

    #[test]
    fn extract_semantics_scenario_ko_is_state_machine_trace() {
        let (owned_data_types, state_type, input_type, transaction_type) =
            scenario_semantics_fixture();
        let data_types: IndexMap<&DataTypeKey, &TypedDataType> = owned_data_types.iter().collect();
        let step_type = Type::function(
            vec![state_type.clone(), Type::list(input_type.clone())],
            Type::fuzzer(Rc::new(Type::App {
                public: true,
                contains_opaque: false,
                module: STDLIB_FUZZ_SCENARIO_MODULE.to_string(),
                name: "Scenario".to_string(),
                args: vec![state_type.clone()],
                alias: None,
            })),
        );
        let via = TypedExpr::Call {
            location: Span::empty(),
            tipo: Type::tuple(vec![
                Type::list(Type::string()),
                Type::list(transaction_type.clone()),
            ]),
            fun: Box::new(module_fn_var(
                "ko",
                STDLIB_FUZZ_SCENARIO_MODULE,
                Type::function(
                    vec![state_type.clone(), step_type.clone()],
                    Type::tuple(vec![
                        Type::list(Type::string()),
                        Type::list(transaction_type.clone()),
                    ]),
                ),
            )),
            args: vec![
                call_arg(local_var("initial_state", state_type.clone())),
                call_arg(local_var("step", step_type)),
            ],
        };
        let functions = empty_known_functions();

        assert_eq!(
            extract_semantics_from_via(
                &via,
                "permissions",
                &functions,
                &data_types,
                Type::tuple(vec![
                    Type::list(Type::string()),
                    Type::list(transaction_type.clone()),
                ])
                .as_ref(),
            ),
            FuzzerSemantics::StateMachineTrace {
                acceptance: StateMachineAcceptance::AcceptsFailure,
                state_type: SemanticType::Unsupported("permissions.State".to_string()),
                step_input_types: vec![SemanticType::List(Box::new(SemanticType::Unsupported(
                    "cardano/transaction.Input".to_string()
                ),))],
                label_type: SemanticType::List(Box::new(SemanticType::String)),
                event_type: SemanticType::Unsupported(
                    "cardano/transaction.Transaction".to_string()
                ),
                transition_semantics: expected_scenario_transition_semantics(),
                output_semantics: Box::new(FuzzerSemantics::Product(vec![
                    FuzzerSemantics::List {
                        element: Box::new(FuzzerSemantics::String),
                        min_len: Some(1),
                        max_len: None,
                    },
                    FuzzerSemantics::List {
                        element: Box::new(FuzzerSemantics::DataWithSchema {
                            type_name: "cardano/transaction.Transaction".to_string(),
                        }),
                        min_len: Some(1),
                        max_len: Some(1),
                    },
                ])),
                step_function_ir: None,
                step_ir_unsupported_reason: None,
                transition_prop: None,
                initial_state_shallow_ir: None,
            }
        );
    }

    #[test]
    fn state_machine_trace_output_semantics_uses_data_with_schema_for_adt_event_type() {
        // Regression for Issue 14 gap: state-machine trace `output_semantics` must
        // lower a non-nullary qualified ADT event type as `DataWithSchema`, not
        // `Opaque`. Prior to this fix, `state_machine_trace_output_semantics` took
        // `&SemanticType` and called `default_semantics_for_semantic_type`, which
        // mapped `SemanticType::Unsupported(_)` to `FuzzerSemantics::Opaque` and
        // bypassed the `default_semantics_for_type` ADT branch.
        let (owned_data_types, state_type, input_type, transaction_type) =
            scenario_semantics_fixture();
        let data_types: IndexMap<&DataTypeKey, &TypedDataType> = owned_data_types.iter().collect();
        let step_type = Type::function(
            vec![state_type.clone(), Type::list(input_type.clone())],
            Type::fuzzer(Rc::new(Type::App {
                public: true,
                contains_opaque: false,
                module: STDLIB_FUZZ_SCENARIO_MODULE.to_string(),
                name: "Scenario".to_string(),
                args: vec![state_type.clone()],
                alias: None,
            })),
        );
        let via = TypedExpr::Call {
            location: Span::empty(),
            tipo: Type::list(transaction_type.clone()),
            fun: Box::new(module_fn_var(
                "ok",
                STDLIB_FUZZ_SCENARIO_MODULE,
                Type::function(
                    vec![state_type.clone(), step_type.clone()],
                    Type::list(transaction_type.clone()),
                ),
            )),
            args: vec![
                call_arg(local_var("initial_state", state_type.clone())),
                call_arg(local_var("step", step_type)),
            ],
        };
        let functions = empty_known_functions();

        let semantics = extract_semantics_from_via(
            &via,
            "permissions",
            &functions,
            &data_types,
            Type::list(transaction_type.clone()).as_ref(),
        );

        match semantics {
            FuzzerSemantics::StateMachineTrace {
                output_semantics,
                transition_semantics,
                ..
            } => {
                // Event type `cardano/transaction.Transaction` is a non-nullary
                // qualified ADT and must be lowered as `DataWithSchema`, not
                // `Opaque`, in both the `output_semantics` List element and the
                // transition's `event_semantics`.
                assert_eq!(
                    output_semantics.as_ref(),
                    &FuzzerSemantics::List {
                        element: Box::new(FuzzerSemantics::DataWithSchema {
                            type_name: "cardano/transaction.Transaction".to_string(),
                        }),
                        min_len: Some(0),
                        max_len: None,
                    },
                    "AcceptsSuccess output_semantics must use DataWithSchema for non-nullary qualified ADT event type",
                );
                assert_eq!(
                    transition_semantics.event_semantics.as_ref(),
                    &FuzzerSemantics::DataWithSchema {
                        type_name: "cardano/transaction.Transaction".to_string(),
                    },
                    "transition event_semantics must use DataWithSchema for non-nullary qualified ADT event type",
                );
            }
            other => panic!("expected StateMachineTrace, got {:?}", other),
        }
    }

    #[test]
    fn extract_constraint_nested_both_with_map() {
        // both(map(int_between(0,10), f), int_between(20,30))
        let inner_map = TypedExpr::Call {
            location: Span::empty(),
            tipo: Type::int(),
            fun: Box::new(fuzz_var(
                "map",
                Type::function(vec![Type::int(), Type::int()], Type::int()),
            )),
            args: vec![
                call_arg(make_int_between_via("0", "10")),
                call_arg(local_var(
                    "f",
                    Type::function(vec![Type::int()], Type::int()),
                )),
            ],
        };
        let via = TypedExpr::Call {
            location: Span::empty(),
            tipo: Type::tuple(vec![Type::int(), Type::int()]),
            fun: Box::new(fuzz_var(
                "both",
                Type::function(
                    vec![Type::int(), Type::int()],
                    Type::tuple(vec![Type::int(), Type::int()]),
                ),
            )),
            args: vec![
                call_arg(inner_map),
                call_arg(make_int_between_via("20", "30")),
            ],
        };
        let functions = empty_known_functions();
        assert!(matches!(
            extract_constraint_from_via(&via, "math", &functions),
            FuzzerConstraint::Unsupported { .. }
        ));
    }

    #[test]
    fn extract_constraint_both_with_extra_arguments_is_unsupported() {
        let via = TypedExpr::Call {
            location: Span::empty(),
            tipo: Type::tuple(vec![Type::int(), Type::int(), Type::int()]),
            fun: Box::new(fuzz_var(
                "both",
                Type::function(
                    vec![Type::int(), Type::int(), Type::int()],
                    Type::tuple(vec![Type::int(), Type::int(), Type::int()]),
                ),
            )),
            args: vec![
                call_arg(make_int_between_via("0", "10")),
                call_arg(make_int_between_via("20", "30")),
                call_arg(make_int_between_via("40", "50")),
            ],
        };
        let functions = empty_known_functions();
        assert!(
            matches!(
                extract_constraint_from_via(&via, "math", &functions),
                FuzzerConstraint::Unsupported { .. }
            ),
            "both should reject unexpected arity"
        );
    }

    #[test]
    fn derive_semantics_for_int_range_is_generic_ir() {
        assert_eq!(
            semantics_from_constraint(
                &FuzzerConstraint::IntRange {
                    min: "1".to_string(),
                    max: "10".to_string(),
                },
                Type::int().as_ref(),
            ),
            FuzzerSemantics::IntRange {
                min: Some("1".to_string()),
                max: Some("10".to_string()),
            }
        );
    }

    #[test]
    fn derive_semantics_for_tuple_is_product() {
        assert_eq!(
            semantics_from_constraint(
                &FuzzerConstraint::Tuple(vec![
                    FuzzerConstraint::IntRange {
                        min: "0".to_string(),
                        max: "3".to_string(),
                    },
                    FuzzerConstraint::ByteStringLenRange {
                        min_len: 2,
                        max_len: 4,
                    },
                ]),
                Type::tuple(vec![Type::int(), Type::byte_array()]).as_ref(),
            ),
            FuzzerSemantics::Product(vec![
                FuzzerSemantics::IntRange {
                    min: Some("0".to_string()),
                    max: Some("3".to_string()),
                },
                FuzzerSemantics::ByteArrayRange {
                    min_len: Some(2),
                    max_len: Some(4),
                },
            ])
        );
    }

    #[test]
    fn derive_semantics_for_list_of_unsupported_type_yields_data_with_schema_leaf() {
        // After Issue 14, qualified non-nullary ADTs (e.g.
        // cardano/transaction.Transaction) lower to
        // `FuzzerSemantics::DataWithSchema` so the state-machine path can
        // attach a structural schema predicate. The leaf is no longer
        // classified as `Opaque` -- `fuzzer_semantics_contains_opaque`
        // returns false for DataWithSchema leaves, which is what unblocks
        // the Transaction-domain state-machine proofs.
        let transaction_type = Rc::new(Type::App {
            public: true,
            contains_opaque: false,
            module: "cardano/transaction".to_string(),
            name: "Transaction".to_string(),
            args: vec![],
            alias: None,
        });

        let semantics = semantics_from_constraint(
            &FuzzerConstraint::List {
                elem: Box::new(FuzzerConstraint::Any),
                min_len: Some(0),
                max_len: None,
            },
            Type::list(transaction_type).as_ref(),
        );

        assert!(
            matches!(
                &semantics,
                FuzzerSemantics::List {
                    element,
                    min_len: Some(0),
                    max_len: None,
                } if matches!(
                    element.as_ref(),
                    FuzzerSemantics::DataWithSchema { type_name }
                    if type_name == "cardano/transaction.Transaction"
                )
            ),
            "expected DataWithSchema leaf for non-nullary qualified ADT, got {semantics:?}"
        );
    }

    #[test]
    fn derive_semantics_for_map_propagates_inner_int_range() {
        assert_eq!(
            semantics_from_constraint(
                &FuzzerConstraint::Map(Box::new(FuzzerConstraint::IntRange {
                    min: "5".to_string(),
                    max: "15".to_string(),
                })),
                Type::int().as_ref(),
            ),
            FuzzerSemantics::IntRange {
                min: Some("5".to_string()),
                max: Some("15".to_string()),
            }
        );
    }

    #[test]
    fn derive_semantics_for_map_type_mismatch_is_opaque() {
        // Map wrapping an IntRange but output type is ByteArray — type mismatch
        assert!(matches!(
            semantics_from_constraint(
                &FuzzerConstraint::Map(Box::new(FuzzerConstraint::IntRange {
                    min: "0".to_string(),
                    max: "10".to_string(),
                })),
                Type::byte_array().as_ref(),
            ),
            FuzzerSemantics::Opaque { .. }
        ));
    }

    #[test]
    fn derive_semantics_for_and_intersects_int_ranges() {
        assert_eq!(
            semantics_from_constraint(
                &FuzzerConstraint::And(vec![
                    FuzzerConstraint::IntRange {
                        min: "0".to_string(),
                        max: "100".to_string(),
                    },
                    FuzzerConstraint::IntRange {
                        min: "50".to_string(),
                        max: "200".to_string(),
                    },
                ]),
                Type::int().as_ref(),
            ),
            FuzzerSemantics::IntRange {
                min: Some("50".to_string()),
                max: Some("100".to_string()),
            }
        );
    }

    #[test]
    fn derive_semantics_for_and_all_any_produces_default() {
        // And with all Any constraints — should produce default semantics for the type.
        let semantics = semantics_from_constraint(
            &FuzzerConstraint::And(vec![FuzzerConstraint::Any, FuzzerConstraint::Any]),
            Type::int().as_ref(),
        );
        assert_eq!(
            semantics,
            FuzzerSemantics::IntRange {
                min: None,
                max: None,
            }
        );
    }

    #[test]
    fn derive_semantics_for_and_disjoint_ranges_produces_default() {
        // [10, 20] AND [30, 40] are disjoint — intersection is empty,
        // so we should fall back to default (unbounded) semantics.
        let semantics = semantics_from_constraint(
            &FuzzerConstraint::And(vec![
                FuzzerConstraint::IntRange {
                    min: "10".to_string(),
                    max: "20".to_string(),
                },
                FuzzerConstraint::IntRange {
                    min: "30".to_string(),
                    max: "40".to_string(),
                },
            ]),
            Type::int().as_ref(),
        );
        assert_eq!(
            semantics,
            FuzzerSemantics::IntRange {
                min: None,
                max: None,
            }
        );
    }

    #[test]
    fn extract_exact_scalar_bool_true() {
        assert_eq!(
            try_extract_exact_scalar(&bool_constructor(true)),
            Some(FuzzerExactValue::Bool(true)),
        );
    }

    #[test]
    fn extract_exact_scalar_bool_false() {
        assert_eq!(
            try_extract_exact_scalar(&bool_constructor(false)),
            Some(FuzzerExactValue::Bool(false)),
        );
    }

    #[test]
    fn extract_exact_scalar_string() {
        let expr = TypedExpr::String {
            location: Span::empty(),
            tipo: Type::string(),
            value: "hello".to_string(),
        };
        assert_eq!(
            try_extract_exact_scalar(&expr),
            Some(FuzzerExactValue::String("hello".to_string())),
        );
    }

    #[test]
    fn extract_exact_scalar_bytearray() {
        let expr = TypedExpr::ByteArray {
            location: Span::empty(),
            tipo: Type::byte_array(),
            bytes: vec![0xDE, 0xAD],
            preferred_format: crate::ast::ByteArrayFormatPreference::HexadecimalString,
        };
        assert_eq!(
            try_extract_exact_scalar(&expr),
            Some(FuzzerExactValue::ByteArray(vec![0xDE, 0xAD])),
        );
    }

    #[test]
    fn canonicalize_finite_scalar_domain_sorts_dedups_and_preserves_oneof() {
        let domain = canonicalize_finite_scalar_domain(
            Type::string().as_ref(),
            vec![
                FuzzerExactValue::String("world".to_string()),
                FuzzerExactValue::String("hello".to_string()),
                FuzzerExactValue::String("".to_string()),
                FuzzerExactValue::String("hello".to_string()),
                FuzzerExactValue::String("test".to_string()),
            ],
        )
        .expect("valid finite string domain");

        assert_eq!(
            domain,
            CanonicalFiniteScalarDomain::OneOf(expected_finite_string_values())
        );
    }

    #[test]
    fn canonicalize_finite_scalar_domain_singleton_becomes_exact() {
        let domain = canonicalize_finite_scalar_domain(
            Type::string().as_ref(),
            vec![
                FuzzerExactValue::String("same".to_string()),
                FuzzerExactValue::String("same".to_string()),
            ],
        )
        .expect("valid singleton string domain");

        assert_eq!(
            domain,
            CanonicalFiniteScalarDomain::Exact(FuzzerExactValue::String("same".to_string()))
        );
    }

    #[test]
    fn canonicalize_finite_scalar_domain_rejects_mismatch_and_empty() {
        assert_eq!(
            canonicalize_finite_scalar_domain(Type::string().as_ref(), vec![]),
            Err(FiniteDomainError::Empty)
        );
        assert_eq!(
            canonicalize_finite_scalar_domain(
                Type::string().as_ref(),
                vec![FuzzerExactValue::ByteArray(vec![0])]
            ),
            Err(FiniteDomainError::OutputTypeMismatch)
        );
        assert_eq!(
            canonicalize_finite_scalar_domain(
                Type::string().as_ref(),
                vec![
                    FuzzerExactValue::String("a".to_string()),
                    FuzzerExactValue::ByteArray(vec![0]),
                ]
            ),
            Err(FiniteDomainError::Heterogeneous)
        );
    }

    #[test]
    fn describe_semantics_renders_oneof_deterministically() {
        assert_eq!(
            describe_semantics(&FuzzerSemantics::OneOf(expected_finite_string_values())),
            "OneOf(\"\", \"hello\", \"test\", \"world\")"
        );
    }

    #[test]
    fn extract_exact_scalar_int_returns_none() {
        assert_eq!(try_extract_exact_scalar(&uint_lit("42")), None);
    }

    // --- R6: INT_LITERAL_MAX_DEPTH cycle guard ---
    //
    // `try_extract_int_literal_inner` bounds its own recursion via
    // `INT_LITERAL_MAX_DEPTH` to prevent stack overflows from adversarial or
    // pathological expressions (deeply nested negations, circular local
    // aliases, or mutually-referential module constants). These tests pin
    // down the exact boundary: depth <= MAX succeeds, depth > MAX returns
    // None, and the guard triggers without panicking.

    /// Wrap `expr` in `depth` layers of `UnOp::Negate`.
    fn nested_negate(expr: TypedExpr, depth: usize) -> TypedExpr {
        let mut acc = expr;
        for _ in 0..depth {
            acc = negate_expr(acc);
        }
        acc
    }

    #[test]
    fn int_literal_max_depth_constant_is_sixteen() {
        // Pin the constant to its documented value. If it changes, the
        // boundary tests below should be updated deliberately.
        assert_eq!(INT_LITERAL_MAX_DEPTH, 16);
    }

    #[test]
    fn try_extract_int_literal_at_max_depth_resolves_value() {
        // 16 negations around a UInt is exactly at the depth limit
        // (the UInt is read at depth == MAX, which is allowed since the
        // guard uses strict `>` comparison). 16 is even, so the result
        // equals the original value.
        let constants = HashMap::new();
        let locals = BTreeMap::new();
        let expr = nested_negate(uint_lit("7"), 16);
        assert_eq!(
            try_extract_int_literal(&expr, &constants, &locals),
            Some(BigInt::from(7)),
            "16-level nesting must still resolve (depth == MAX is allowed)"
        );
    }

    #[test]
    fn try_extract_int_literal_below_max_depth_resolves_with_sign() {
        // 15 negations: odd count flips the sign.
        let constants = HashMap::new();
        let locals = BTreeMap::new();
        let expr = nested_negate(uint_lit("7"), 15);
        assert_eq!(
            try_extract_int_literal(&expr, &constants, &locals),
            Some(BigInt::from(-7)),
            "15-level nesting (odd) must resolve to the negated literal"
        );
    }

    #[test]
    fn try_extract_int_literal_above_max_depth_returns_none() {
        // 17 negations: the guard must trip at the innermost recursive call
        // (depth == 17 triggers the `depth > INT_LITERAL_MAX_DEPTH` check)
        // and the function must return None rather than panic or overflow.
        let constants = HashMap::new();
        let locals = BTreeMap::new();
        let expr = nested_negate(uint_lit("7"), 17);
        assert_eq!(
            try_extract_int_literal(&expr, &constants, &locals),
            None,
            "17-level nesting (depth > MAX) must be rejected by the cycle guard"
        );
    }

    #[test]
    fn try_extract_int_literal_far_above_max_depth_terminates() {
        // A grossly deep nesting should terminate gracefully. The absolute
        // depth here (64) would blow the stack without the guard, so this
        // test doubles as a regression guard against removing the check.
        let constants = HashMap::new();
        let locals = BTreeMap::new();
        let expr = nested_negate(uint_lit("7"), 64);
        assert_eq!(
            try_extract_int_literal(&expr, &constants, &locals),
            None,
            "extreme nesting must terminate with None (guard must short-circuit)"
        );
    }

    #[test]
    fn try_extract_int_literal_local_alias_chain_above_max_depth_returns_none() {
        // Local-variable aliases also increment depth at each hop. A chain
        // of 20 aliases must trip the guard even though no negation is
        // involved — this covers the `LocalVariable` recursion path.
        let constants = HashMap::new();
        let mut locals: BTreeMap<String, TypedExpr> = BTreeMap::new();
        let int_tipo = Type::int();

        // x0 = 42; x1 = x0; x2 = x1; ... x20 = x19
        locals.insert("x0".to_string(), uint_lit("42"));
        for i in 1..=20 {
            let prev = local_var(&format!("x{}", i - 1), int_tipo.clone());
            locals.insert(format!("x{i}"), prev);
        }

        let expr = local_var("x20", int_tipo);
        assert_eq!(
            try_extract_int_literal(&expr, &constants, &locals),
            None,
            "20-deep local alias chain must be rejected by the cycle guard"
        );
    }

    #[test]
    fn try_extract_int_literal_short_local_alias_chain_resolves() {
        // A chain of 3 aliases (well under the limit) must still resolve to
        // confirm the alias-resolution path is exercised by these tests.
        let constants = HashMap::new();
        let mut locals: BTreeMap<String, TypedExpr> = BTreeMap::new();
        let int_tipo = Type::int();
        locals.insert("a".to_string(), uint_lit("99"));
        locals.insert("b".to_string(), local_var("a", int_tipo.clone()));
        locals.insert("c".to_string(), local_var("b", int_tipo.clone()));

        let expr = local_var("c", int_tipo);
        assert_eq!(
            try_extract_int_literal(&expr, &constants, &locals),
            Some(BigInt::from(99)),
            "short alias chain must resolve through the alias path"
        );
    }

    // --- R7: nested Bind and Map(Map) normalization ---
    //
    // The fuzzer normalizer must handle arbitrarily deep Bind/Map nestings
    // without losing structural information. These tests pin the recursive
    // descent behavior: nested Binds preserve a Bind-shaped result, nested
    // Maps collapse to a Map tree over a Primitive leaf, and mixed chains
    // preserve the outermost constructor.

    #[test]
    fn normalize_fuzzer_nested_bind_depth_four_preserves_bind_shape() {
        // Build a Bind(Bind(Bind(Bind(Primitive)))) — depth 4.
        // Each layer uses an inline lambda whose body is the next Bind call,
        // which is itself a Fuzzer<Int>.
        let int_ty = Type::int();

        let innermost = make_typed_int_between_fuzzer("1", "2");

        let level3 = make_typed_bind_call(
            make_leaf_fuzzer_call("p3", int_ty.clone()),
            make_inline_bind_continuation("x3", int_ty.clone(), innermost, int_ty.clone()),
            int_ty.clone(),
        );
        let level2 = make_typed_bind_call(
            make_leaf_fuzzer_call("p2", int_ty.clone()),
            make_inline_bind_continuation("x2", int_ty.clone(), level3, int_ty.clone()),
            int_ty.clone(),
        );
        let level1 = make_typed_bind_call(
            make_leaf_fuzzer_call("p1", int_ty.clone()),
            make_inline_bind_continuation("x1", int_ty.clone(), level2, int_ty.clone()),
            int_ty.clone(),
        );
        let outer = make_typed_bind_call(
            make_leaf_fuzzer_call("p0", int_ty.clone()),
            make_inline_bind_continuation("x0", int_ty.clone(), level1, int_ty.clone()),
            int_ty.clone(),
        );

        let normalized = normalize_fuzzer_from_via(&outer, "math", &empty_known_functions());

        // Walk the resulting Bind chain: each layer must be a Bind whose
        // source is a Primitive leaf (the `pN` fuzzer) and whose result is
        // the next Bind layer.
        fn expect_bind_chain(n: NormalizedFuzzer, remaining: usize) {
            if remaining == 0 {
                // Innermost: the stdlib recognizer extracts [1, 2] from
                // `int_between(1, 2)` on the `aiken/fuzz` module.
                match n {
                    NormalizedFuzzer::Primitive {
                        known_constraint, ..
                    } => {
                        assert_eq!(
                            known_constraint,
                            Some(FuzzerConstraint::IntRange {
                                min: "1".to_string(),
                                max: "2".to_string(),
                            }),
                            "innermost primitive must carry the extracted stdlib bounds"
                        );
                    }
                    other => {
                        panic!("expected innermost Primitive, got {other:?}");
                    }
                }
                return;
            }
            match n {
                NormalizedFuzzer::Bind { source, result } => {
                    assert!(
                        matches!(*source, NormalizedFuzzer::Primitive { .. }),
                        "each Bind source must remain a Primitive leaf"
                    );
                    expect_bind_chain(*result, remaining - 1);
                }
                other => panic!("expected Bind at remaining={remaining}, got {other:?}"),
            }
        }

        // 4 bind layers => 4 nested Binds before the Primitive core.
        expect_bind_chain(normalized, 4);
    }

    #[test]
    fn normalize_fuzzer_map_of_map_depth_four_preserves_map_shape() {
        // Build Map(Map(Map(Map(Primitive)))) using `anything_but_map` so the
        // name-agnostic path is exercised. Each inner mapper is a
        // distinguishable unary function, so no collapse to Identity occurs.
        let int_ty = Type::int();

        let level0 = make_typed_map_call(
            make_leaf_fuzzer_call("seed", int_ty.clone()),
            make_add_int_mapper("1"),
            int_ty.clone(),
        );
        let level1 = make_typed_map_call(level0, make_add_int_mapper("2"), int_ty.clone());
        let level2 = make_typed_map_call(level1, make_add_int_mapper("3"), int_ty.clone());
        let level3 = make_typed_map_call(level2, make_add_int_mapper("4"), int_ty.clone());

        let normalized = normalize_fuzzer_from_via(&level3, "math", &empty_known_functions());

        // We expect exactly 4 nested Map layers over a Primitive leaf.
        fn expect_map_chain(n: NormalizedFuzzer, remaining: usize) {
            if remaining == 0 {
                assert!(
                    matches!(n, NormalizedFuzzer::Primitive { .. }),
                    "innermost of Map chain must be a Primitive leaf, got {n:?}"
                );
                return;
            }
            match n {
                NormalizedFuzzer::Map { source, .. } => {
                    expect_map_chain(*source, remaining - 1);
                }
                other => panic!("expected Map at remaining={remaining}, got {other:?}"),
            }
        }
        expect_map_chain(normalized, 4);
    }

    #[test]
    fn normalize_fuzzer_map_of_map_depth_four_yields_map_over_unconstrained_source() {
        // Same Map(Map(Map(Map(...)))) shape, but checked through the
        // constraint extractor which is the actual proof-pipeline entry
        // point. With the stdlib-gated recognizer, `int_between(0, 5)`
        // extracts `IntRange { 0, 5 }`. Each affine mapper transforms the
        // range; `+1 +2 +3 +4 = +10` so the innermost IntRange becomes
        // `[10, 15]` after being composed through all four maps. The
        // constraint extractor collapses the chain of nested `IntRange`
        // transformations into a single `IntRange` at the leaf.
        let int_ty = Type::int();

        let source = make_typed_map_call(
            make_typed_int_between_fuzzer("0", "5"),
            make_add_int_mapper("1"),
            int_ty.clone(),
        );
        let lvl1 = make_typed_map_call(source, make_add_int_mapper("2"), int_ty.clone());
        let lvl2 = make_typed_map_call(lvl1, make_add_int_mapper("3"), int_ty.clone());
        let lvl3 = make_typed_map_call(lvl2, make_add_int_mapper("4"), int_ty);

        let constraint = extract_constraint_from_via(&lvl3, "math", &empty_known_functions());

        // We only pin that bound information is no longer lost: the
        // outer constraint must surface some IntRange (possibly nested
        // under `Map(_)` depending on whether every affine step folds
        // into the range) rather than collapsing to `Any` or
        // `Unsupported`.
        fn contains_int_range(c: &FuzzerConstraint) -> bool {
            match c {
                FuzzerConstraint::IntRange { .. } => true,
                FuzzerConstraint::Map(inner) => contains_int_range(inner),
                FuzzerConstraint::And(items) | FuzzerConstraint::Tuple(items) => {
                    items.iter().any(contains_int_range)
                }
                FuzzerConstraint::List { elem, .. } => contains_int_range(elem),
                _ => false,
            }
        }
        assert!(
            contains_int_range(&constraint),
            "Map(Map(Map(Map(IntRange)))) chain must preserve some IntRange, got {constraint:?}"
        );
    }

    #[test]
    fn normalize_fuzzer_nested_bind_continuation_returns_unconstrained_domain() {
        // Build Bind(seed, \x0. Bind(p1, \x1. Bind(p2, \x2. int_between(3,7)))).
        // Under the stdlib-gated recognizer, `int_between(3, 7)` now carries
        // `IntRange { 3, 7 }`, and the nested Bind propagates the innermost
        // continuation's constraint.
        let int_ty = Type::int();

        let innermost = make_typed_int_between_fuzzer("3", "7");
        let mid = make_typed_bind_call(
            make_leaf_fuzzer_call("p2", int_ty.clone()),
            make_inline_bind_continuation("x2", int_ty.clone(), innermost, int_ty.clone()),
            int_ty.clone(),
        );
        let outer_continuation_body = make_typed_bind_call(
            make_leaf_fuzzer_call("p1", int_ty.clone()),
            make_inline_bind_continuation("x1", int_ty.clone(), mid, int_ty.clone()),
            int_ty.clone(),
        );
        let outer = make_typed_bind_call(
            make_leaf_fuzzer_call("seed", int_ty.clone()),
            make_inline_bind_continuation(
                "x0",
                int_ty.clone(),
                outer_continuation_body,
                int_ty.clone(),
            ),
            int_ty.clone(),
        );

        let constraint = extract_constraint_from_via(&outer, "math", &empty_known_functions());
        assert_eq!(
            constraint,
            FuzzerConstraint::IntRange {
                min: "3".to_string(),
                max: "7".to_string(),
            },
            "nested Bind must propagate the innermost continuation's extracted domain"
        );
    }

    #[test]
    fn normalize_fuzzer_map_wrapping_bind_preserves_outer_shape() {
        // Map(Bind(...)) — the outer normalization should expose a Map
        // layer whose source is a Bind (not collapsed or opaqued).
        let int_ty = Type::int();

        let inner_bind = make_typed_bind_call(
            make_leaf_fuzzer_call("seed", int_ty.clone()),
            make_inline_bind_continuation(
                "x",
                int_ty.clone(),
                make_typed_int_between_fuzzer("5", "8"),
                int_ty.clone(),
            ),
            int_ty.clone(),
        );

        let outer_map = make_typed_map_call(inner_bind, make_add_int_mapper("1"), int_ty);

        let normalized = normalize_fuzzer_from_via(&outer_map, "math", &empty_known_functions());

        match normalized {
            NormalizedFuzzer::Map { source, .. } => match *source {
                NormalizedFuzzer::Bind {
                    source: bind_source,
                    result,
                } => {
                    assert!(
                        matches!(*bind_source, NormalizedFuzzer::Primitive { .. }),
                        "Bind source under Map must stay a Primitive leaf"
                    );
                    assert!(
                        matches!(*result, NormalizedFuzzer::Primitive { .. }),
                        "Bind continuation under Map must resolve to a Primitive (int_between)"
                    );
                }
                other => panic!("expected Bind under Map, got {other:?}"),
            },
            other => panic!("expected Map at the outer level, got {other:?}"),
        }
    }

    #[test]
    fn normalize_fuzzer_bind_wrapping_map_exposes_bind_over_map() {
        // Bind(Map(...), \x. fuzzer). The normalizer must see the Map as
        // the Bind's source; otherwise the mapper's effect on the sampling
        // domain would be lost.
        let int_ty = Type::int();

        let inner_map = make_typed_map_call(
            make_leaf_fuzzer_call("seed", int_ty.clone()),
            make_add_int_mapper("5"),
            int_ty.clone(),
        );

        let outer_bind = make_typed_bind_call(
            inner_map,
            make_inline_bind_continuation(
                "x",
                int_ty.clone(),
                make_typed_int_between_fuzzer("0", "3"),
                int_ty.clone(),
            ),
            int_ty.clone(),
        );

        let normalized = normalize_fuzzer_from_via(&outer_bind, "math", &empty_known_functions());

        match normalized {
            NormalizedFuzzer::Bind { source, result } => {
                assert!(
                    matches!(*source, NormalizedFuzzer::Map { .. }),
                    "Bind source must be the inner Map, not a collapsed primitive"
                );
                assert!(
                    matches!(*result, NormalizedFuzzer::Primitive { .. }),
                    "Bind continuation must normalize to the inner int_between primitive"
                );
            }
            other => panic!("expected Bind at the outer level, got {other:?}"),
        }
    }

    #[test]
    fn test_cache() {
        let called = std::cell::RefCell::new(0);

        let mut cache = Cache::new(|choices| {
            called.replace_with(|n| *n + 1);

            match choices {
                [0, 0, 0] => Status::Keep(true),
                _ => {
                    if choices.len() <= 2 {
                        Status::Invalid
                    } else {
                        Status::Ignore
                    }
                }
            }
        });

        assert_eq!(cache.get(&[1, 1]), Status::Invalid); // Fn executed
        assert_eq!(cache.get(&[1, 1, 2, 3]), Status::Ignore); // Fn executed
        assert_eq!(cache.get(&[1, 1, 2]), Status::Ignore); // Fnexecuted
        assert_eq!(cache.get(&[1, 1, 2, 2]), Status::Ignore); // Cached result
        assert_eq!(cache.get(&[1, 1, 2, 1]), Status::Ignore); // Cached result
        assert_eq!(cache.get(&[0, 1, 2]), Status::Ignore); // Fn executed
        assert_eq!(cache.get(&[0, 0, 0]), Status::Keep(true)); // Fn executed
        assert_eq!(cache.get(&[0, 0, 0]), Status::Keep(true)); // Cached result

        assert_eq!(called.borrow().deref().to_owned(), 5, "execution calls");
        assert_eq!(cache.size(), 4, "cache size");
    }

    #[test]
    fn default_semantics_for_type_recognizes_nullary_enum() {
        let output_type = make_nullary_constructor_type("scenario", "Outcome");
        let owned_data_types =
            make_nullary_constructor_data_types("scenario", "Outcome", &["Ok", "Ko"]);
        let data_types: IndexMap<&DataTypeKey, &TypedDataType> = owned_data_types.iter().collect();

        let semantics = default_semantics_for_type(output_type.as_ref(), &data_types);

        assert_eq!(
            semantics,
            FuzzerSemantics::Constructors { tags: vec![0, 1] }
        );
    }

    #[test]
    fn default_semantics_for_type_returns_data_with_schema_for_non_nullary_adt() {
        let output_type = Rc::new(Type::App {
            public: true,
            contains_opaque: false,
            module: "widgets".to_string(),
            name: "Widget".to_string(),
            args: vec![],
            alias: None,
        });

        let data_type = TypedDataType {
            decorators: vec![],
            constructors: vec![RecordConstructor {
                decorators: vec![],
                location: Span::empty(),
                name: "Widget".to_string(),
                arguments: vec![RecordConstructorArg {
                    label: Some("size".to_string()),
                    annotation: Annotation::Constructor {
                        location: Span::empty(),
                        module: None,
                        name: "Int".to_string(),
                        arguments: vec![],
                    },
                    location: Span::empty(),
                    tipo: Type::int(),
                    doc: None,
                }],
                doc: None,
                sugar: false,
            }],
            doc: None,
            location: Span::empty(),
            name: "Widget".to_string(),
            opaque: false,
            parameters: vec![],
            public: true,
            typed_parameters: vec![],
        };

        let mut owned_data_types: IndexMap<DataTypeKey, TypedDataType> = IndexMap::new();
        owned_data_types.insert(
            DataTypeKey {
                module_name: "widgets".to_string(),
                defined_type: "Widget".to_string(),
            },
            data_type,
        );
        let data_types: IndexMap<&DataTypeKey, &TypedDataType> = owned_data_types.iter().collect();

        let semantics = default_semantics_for_type(output_type.as_ref(), &data_types);

        assert_eq!(
            semantics,
            FuzzerSemantics::DataWithSchema {
                type_name: "widgets.Widget".to_string(),
            },
            "expected DataWithSchema for non-nullary qualified ADT, got {semantics:?}"
        );
    }

    #[test]
    fn default_semantics_for_type_preserves_data_with_schema_generic_arguments() {
        let result_int = Rc::new(Type::App {
            public: true,
            contains_opaque: false,
            module: "widgets".to_string(),
            name: "Result".to_string(),
            args: vec![Type::int()],
            alias: None,
        });
        let result_bytes = Rc::new(Type::App {
            public: true,
            contains_opaque: false,
            module: "widgets".to_string(),
            name: "Result".to_string(),
            args: vec![Type::byte_array()],
            alias: None,
        });
        let data_types: IndexMap<&DataTypeKey, &TypedDataType> = IndexMap::new();

        let int_semantics = default_semantics_for_type(result_int.as_ref(), &data_types);
        let bytes_semantics = default_semantics_for_type(result_bytes.as_ref(), &data_types);

        assert_eq!(
            int_semantics,
            FuzzerSemantics::DataWithSchema {
                type_name: "widgets.Result<Int>".to_string(),
            }
        );
        assert_eq!(
            bytes_semantics,
            FuzzerSemantics::DataWithSchema {
                type_name: "widgets.Result<ByteArray>".to_string(),
            }
        );
        assert_ne!(int_semantics, bytes_semantics);
    }

    #[test]
    fn default_semantics_for_type_returns_data_with_schema_for_unknown_qualified_adt() {
        // Unknown qualified ADTs (not in the data_types map) now fall into
        // the DataWithSchema arm: we can't enumerate constructors, but we
        // still quantify over `Data` and defer structural constraints to the
        // exported fuzzer_data_schema (which may or may not exist at
        // proof-generation time).
        let output_type = make_nullary_constructor_type("unknown_mod", "Unknown");
        let data_types: IndexMap<&DataTypeKey, &TypedDataType> = IndexMap::new();

        let semantics = default_semantics_for_type(output_type.as_ref(), &data_types);

        assert_eq!(
            semantics,
            FuzzerSemantics::DataWithSchema {
                type_name: "unknown_mod.Unknown".to_string(),
            },
            "expected DataWithSchema for unknown qualified ADT, got {semantics:?}"
        );
    }

    #[test]
    fn default_semantics_for_type_threads_data_types_through_list_element() {
        let owned_data_types =
            make_nullary_constructor_data_types("scenario", "Outcome", &["Ok", "Ko"]);
        let data_types: IndexMap<&DataTypeKey, &TypedDataType> = owned_data_types.iter().collect();

        let outcome_type = make_nullary_constructor_type("scenario", "Outcome");
        let list_type = Type::list(outcome_type);

        let semantics = default_semantics_for_type(list_type.as_ref(), &data_types);

        assert_eq!(
            semantics,
            FuzzerSemantics::List {
                element: Box::new(FuzzerSemantics::Constructors { tags: vec![0, 1] }),
                min_len: None,
                max_len: None,
            }
        );
    }

    #[test]
    fn default_semantics_for_type_threads_data_types_through_tuple_elements() {
        let owned_data_types =
            make_nullary_constructor_data_types("scenario", "Outcome", &["Ok", "Ko"]);
        let data_types: IndexMap<&DataTypeKey, &TypedDataType> = owned_data_types.iter().collect();

        let outcome_type = make_nullary_constructor_type("scenario", "Outcome");
        let tuple_type = Type::tuple(vec![outcome_type, Type::int()]);

        let semantics = default_semantics_for_type(tuple_type.as_ref(), &data_types);

        assert_eq!(
            semantics,
            FuzzerSemantics::Product(vec![
                FuzzerSemantics::Constructors { tags: vec![0, 1] },
                FuzzerSemantics::IntRange {
                    min: None,
                    max: None,
                },
            ])
        );
    }

    // ---------------------------------------------------------------
    // S3 — TransitionProp translation
    // ---------------------------------------------------------------

    #[test]
    fn transition_prop_from_opaque_is_unsupported() {
        let normalized = NormalizedFuzzer::Opaque {
            expr: Box::new(local_var("step", Type::int())),
            reason: "opaque leaf".to_string(),
        };
        let prop = normalized_fuzzer_to_transition_prop(&normalized);
        match prop {
            TransitionProp::Unsupported { reason, .. } => {
                assert_eq!(reason, "opaque leaf");
            }
            other => panic!("expected Unsupported, got {other:?}"),
        }
    }

    #[test]
    fn transition_prop_from_primitive_exact_is_eq_output() {
        let normalized = NormalizedFuzzer::Primitive {
            output_type: Type::bool(),
            known_constraint: Some(FuzzerConstraint::Exact(FuzzerExactValue::Bool(true))),
        };
        let prop = normalized_fuzzer_to_transition_prop(&normalized);
        match prop {
            TransitionProp::EqOutput(ShallowIr::Const(ShallowConst::Bool(true))) => {}
            other => panic!("expected EqOutput(Const(Bool(true))), got {other:?}"),
        }
    }

    #[test]
    fn transition_prop_from_bind_produces_exists() {
        // Bind { source = int in [0,10], result = return true }
        let source = NormalizedFuzzer::Primitive {
            output_type: Type::int(),
            known_constraint: Some(FuzzerConstraint::IntRange {
                min: "0".to_string(),
                max: "10".to_string(),
            }),
        };
        let result = NormalizedFuzzer::Primitive {
            output_type: Type::bool(),
            known_constraint: Some(FuzzerConstraint::Exact(FuzzerExactValue::Bool(true))),
        };
        let normalized = NormalizedFuzzer::Bind {
            source: Box::new(source),
            result: Box::new(result),
        };
        let prop = normalized_fuzzer_to_transition_prop(&normalized);
        match prop {
            TransitionProp::Exists { domain, body, .. } => {
                assert!(matches!(
                    *domain,
                    FuzzerSemantics::IntRange {
                        min: Some(ref lo),
                        max: Some(ref hi),
                    } if lo == "0" && hi == "10"
                ));
                assert!(matches!(
                    *body,
                    TransitionProp::EqOutput(ShallowIr::Const(ShallowConst::Bool(true)))
                ));
            }
            other => panic!("expected Exists, got {other:?}"),
        }
    }

    #[test]
    fn state_machine_trace_field_transition_prop_defaults_to_none_today() {
        // Until Issue S2 enriches `normalize_fuzzer_from_expr` with
        // `return`/`and_then`/`fork*` recognizers, step-function bodies
        // bottom out in `Opaque` → `Unsupported` and the field collapses
        // to `None`. This test pins that invariant so a regression that
        // starts populating `transition_prop` with spurious structure is
        // noticed immediately.
        let (owned_data_types, state_type, input_type, transaction_type) =
            scenario_semantics_fixture();
        let data_types: IndexMap<&DataTypeKey, &TypedDataType> = owned_data_types.iter().collect();
        let step_type = Type::function(
            vec![state_type.clone(), Type::list(input_type.clone())],
            Type::fuzzer(Rc::new(Type::App {
                public: true,
                contains_opaque: false,
                module: STDLIB_FUZZ_SCENARIO_MODULE.to_string(),
                name: "Scenario".to_string(),
                args: vec![state_type.clone()],
                alias: None,
            })),
        );
        let via = TypedExpr::Call {
            location: Span::empty(),
            tipo: Type::list(transaction_type.clone()),
            fun: Box::new(module_fn_var(
                "ok",
                STDLIB_FUZZ_SCENARIO_MODULE,
                Type::function(
                    vec![state_type.clone(), step_type.clone()],
                    Type::list(transaction_type.clone()),
                ),
            )),
            args: vec![
                call_arg(local_var("initial_state", state_type.clone())),
                call_arg(local_var("step", step_type)),
            ],
        };
        let functions = empty_known_functions();

        let semantics = extract_semantics_from_via(
            &via,
            "permissions",
            &functions,
            &data_types,
            Type::list(transaction_type.clone()).as_ref(),
        );

        match semantics {
            FuzzerSemantics::StateMachineTrace {
                transition_prop, ..
            } => {
                assert!(
                    transition_prop.is_none(),
                    "transition_prop should currently be None for opaque step functions (got {transition_prop:?})"
                );
            }
            other => panic!("expected StateMachineTrace, got {other:?}"),
        }
    }

    // ---------------------------------------------------------------
    // S3 — direct TypedExpr -> TransitionProp translation
    // ---------------------------------------------------------------

    type TransitionPropTestContext = (
        FunctionIndex<'static>,
        ConstantIndex<'static>,
        BTreeMap<String, TypedExpr>,
        IndexMap<&'static DataTypeKey, &'static TypedDataType>,
        BTreeSet<(String, String)>,
    );

    fn empty_transition_prop_context() -> TransitionPropTestContext {
        (
            HashMap::new(),
            HashMap::new(),
            BTreeMap::new(),
            IndexMap::new(),
            BTreeSet::new(),
        )
    }

    #[test]
    fn typed_expr_to_transition_prop_if_produces_if_then_else() {
        // `if True { return(0) } else { return(1) }` — note: the exact cond
        // content isn't important here; we only want to see IfThenElse.
        let (function_index, constant_index, local_values, empty_data_types, mut visiting) =
            empty_transition_prop_context();

        let int_fuzzer = Type::fuzzer(Type::int());
        let return_fn_type = Type::function(vec![Type::int()], int_fuzzer.clone());

        let make_return = |value: &str| TypedExpr::Call {
            location: Span::empty(),
            tipo: int_fuzzer.clone(),
            fun: Box::new(module_fn_var(
                "constant",
                STDLIB_FUZZ_MODULE,
                return_fn_type.clone(),
            )),
            args: vec![call_arg(uint_lit(value))],
        };

        let cond = bool_constructor(true);
        let then_branch = make_return("0");
        let else_branch = make_return("1");

        let if_expr = TypedExpr::If {
            location: Span::empty(),
            tipo: int_fuzzer.clone(),
            branches: vec1::vec1![IfBranch {
                condition: cond,
                body: then_branch,
                is: None,
                location: Span::empty(),
            }],
            final_else: Box::new(else_branch),
        };

        let prop = typed_expr_to_transition_prop(
            &if_expr,
            "test_mod",
            &function_index,
            &constant_index,
            &local_values,
            &empty_data_types,
            &mut visiting,
            &BTreeSet::new(),
            &mut BTreeSet::new(),
        );

        match prop {
            TransitionProp::IfThenElse { t, e, .. } => {
                assert!(
                    matches!(
                        *t,
                        TransitionProp::EqOutput(ShallowIr::Const(ShallowConst::Int(ref v)))
                            if v == "0"
                    ),
                    "expected then = EqOutput(Int 0), got {t:?}"
                );
                assert!(
                    matches!(
                        *e,
                        TransitionProp::EqOutput(ShallowIr::Const(ShallowConst::Int(ref v)))
                            if v == "1"
                    ),
                    "expected else = EqOutput(Int 1), got {e:?}"
                );
            }
            other => panic!("expected IfThenElse, got {other:?}"),
        }
    }

    #[test]
    fn typed_expr_to_transition_prop_and_then_produces_exists() {
        // `and_then(int_between(0, 10), fn(x) { return(x) })` — the
        // continuation is the pure passthrough, so the body translates to
        // `EqOutput(Var x)`.
        let (function_index, constant_index, local_values, empty_data_types, mut visiting) =
            empty_transition_prop_context();

        let int_fuzzer = Type::fuzzer(Type::int());
        let source = make_typed_int_between_fuzzer("0", "10");

        let x = local_var("x", Type::int());
        let return_body = TypedExpr::Call {
            location: Span::empty(),
            tipo: int_fuzzer.clone(),
            fun: Box::new(module_fn_var(
                "constant",
                STDLIB_FUZZ_MODULE,
                Type::function(vec![Type::int()], int_fuzzer.clone()),
            )),
            args: vec![call_arg(x)],
        };

        let continuation =
            make_inline_bind_continuation("x", Type::int(), return_body, Type::int());

        let bind_call = make_typed_bind_call(source, continuation, Type::int());

        let prop = typed_expr_to_transition_prop(
            &bind_call,
            "test_mod",
            &function_index,
            &constant_index,
            &local_values,
            &empty_data_types,
            &mut visiting,
            &BTreeSet::new(),
            &mut BTreeSet::new(),
        );

        match prop {
            TransitionProp::Exists { binder, body, .. } => {
                assert_eq!(binder, "x", "expected binder 'x', got '{binder}'");
                assert!(
                    matches!(
                        *body,
                        TransitionProp::EqOutput(ShallowIr::Var { ref name, .. })
                            if name == "x"
                    ),
                    "expected body = EqOutput(Var x), got {body:?}"
                );
            }
            other => panic!("expected Exists, got {other:?}"),
        }
    }

    #[test]
    fn typed_expr_to_transition_prop_fork_produces_or() {
        // `fork2_and_then(weight, fn() { return(0) }, fn() { return(1) },
        // fn() { return(2) })` — three zero-arg fuzzer thunks, so the call
        // translates to a three-way `Or`.
        let (function_index, constant_index, local_values, empty_data_types, mut visiting) =
            empty_transition_prop_context();

        let int_fuzzer = Type::fuzzer(Type::int());
        let return_fn_type = Type::function(vec![Type::int()], int_fuzzer.clone());

        let make_return_thunk = |value: &str| TypedExpr::Fn {
            location: Span::empty(),
            tipo: Type::function(vec![], int_fuzzer.clone()),
            is_capture: false,
            args: vec![],
            body: Box::new(TypedExpr::Call {
                location: Span::empty(),
                tipo: int_fuzzer.clone(),
                fun: Box::new(module_fn_var(
                    "constant",
                    STDLIB_FUZZ_MODULE,
                    return_fn_type.clone(),
                )),
                args: vec![call_arg(uint_lit(value))],
            }),
            return_annotation: None,
        };

        let call_expr = TypedExpr::Call {
            location: Span::empty(),
            tipo: int_fuzzer.clone(),
            fun: Box::new(module_fn_var(
                "fork2_and_then",
                STDLIB_FUZZ_SCENARIO_MODULE,
                Type::function(
                    vec![
                        Type::int(),
                        Type::function(vec![], int_fuzzer.clone()),
                        Type::function(vec![], int_fuzzer.clone()),
                        Type::function(vec![], int_fuzzer.clone()),
                    ],
                    int_fuzzer.clone(),
                ),
            )),
            args: vec![
                call_arg(uint_lit("1")),
                call_arg(make_return_thunk("0")),
                call_arg(make_return_thunk("1")),
                call_arg(make_return_thunk("2")),
            ],
        };

        let prop = typed_expr_to_transition_prop(
            &call_expr,
            "test_mod",
            &function_index,
            &constant_index,
            &local_values,
            &empty_data_types,
            &mut visiting,
            &BTreeSet::new(),
            &mut BTreeSet::new(),
        );

        match prop {
            TransitionProp::Or(branches) => {
                assert_eq!(branches.len(), 3, "expected 3 branches, got {branches:?}");
                for (i, branch) in branches.iter().enumerate() {
                    let expected = i.to_string();
                    assert!(
                        matches!(
                            branch,
                            TransitionProp::EqOutput(ShallowIr::Const(ShallowConst::Int(v)))
                                if v == &expected
                        ),
                        "expected branch {i} = EqOutput(Int {expected}), got {branch:?}"
                    );
                }
            }
            other => panic!("expected Or, got {other:?}"),
        }
    }

    #[test]
    fn typed_expr_to_transition_prop_fork_with_continuation_produces_exists_wrapping_or() {
        // `fork2_and_then(weight, fn() { return(0) }, fn() { return(1) },
        // fn() { return(2) }, fn(x) { return(x) })` — three thunks plus a
        // trailing continuation. We expect:
        //   Exists { binder = "x",
        //            body = And([ Or([EqOutput 0, EqOutput 1, EqOutput 2]),
        //                         EqOutput(Var x) ]) }
        let (function_index, constant_index, local_values, empty_data_types, mut visiting) =
            empty_transition_prop_context();

        let int_fuzzer = Type::fuzzer(Type::int());
        let return_fn_type = Type::function(vec![Type::int()], int_fuzzer.clone());

        let make_return_thunk = |value: &str| TypedExpr::Fn {
            location: Span::empty(),
            tipo: Type::function(vec![], int_fuzzer.clone()),
            is_capture: false,
            args: vec![],
            body: Box::new(TypedExpr::Call {
                location: Span::empty(),
                tipo: int_fuzzer.clone(),
                fun: Box::new(module_fn_var(
                    "constant",
                    STDLIB_FUZZ_MODULE,
                    return_fn_type.clone(),
                )),
                args: vec![call_arg(uint_lit(value))],
            }),
            return_annotation: None,
        };

        let cont_body = TypedExpr::Call {
            location: Span::empty(),
            tipo: int_fuzzer.clone(),
            fun: Box::new(module_fn_var(
                "constant",
                STDLIB_FUZZ_MODULE,
                return_fn_type.clone(),
            )),
            args: vec![call_arg(local_var("x", Type::int()))],
        };
        let continuation = make_inline_bind_continuation("x", Type::int(), cont_body, Type::int());

        let call_expr = TypedExpr::Call {
            location: Span::empty(),
            tipo: int_fuzzer.clone(),
            fun: Box::new(module_fn_var(
                "fork2_and_then",
                STDLIB_FUZZ_SCENARIO_MODULE,
                Type::function(
                    vec![
                        Type::int(),
                        Type::function(vec![], int_fuzzer.clone()),
                        Type::function(vec![], int_fuzzer.clone()),
                        Type::function(vec![], int_fuzzer.clone()),
                        Type::function(vec![Type::int()], int_fuzzer.clone()),
                    ],
                    int_fuzzer.clone(),
                ),
            )),
            args: vec![
                call_arg(uint_lit("1")),
                call_arg(make_return_thunk("0")),
                call_arg(make_return_thunk("1")),
                call_arg(make_return_thunk("2")),
                call_arg(continuation),
            ],
        };

        let prop = typed_expr_to_transition_prop(
            &call_expr,
            "test_mod",
            &function_index,
            &constant_index,
            &local_values,
            &empty_data_types,
            &mut visiting,
            &BTreeSet::new(),
            &mut BTreeSet::new(),
        );

        match prop {
            TransitionProp::Exists { binder, body, .. } => {
                assert_eq!(binder, "x");
                match *body {
                    TransitionProp::And(parts) => {
                        assert_eq!(parts.len(), 2);
                        match &parts[0] {
                            TransitionProp::Or(branches) => {
                                assert_eq!(branches.len(), 3);
                            }
                            other => panic!("expected Or as first And leg, got {other:?}"),
                        }
                        match &parts[1] {
                            TransitionProp::EqOutput(ShallowIr::Var { name, .. })
                                if name == "x" => {}
                            other => {
                                panic!("expected EqOutput(Var x) as second And leg, got {other:?}")
                            }
                        }
                    }
                    other => panic!("expected And, got {other:?}"),
                }
            }
            other => panic!("expected Exists wrapping fork, got {other:?}"),
        }
    }

    #[test]
    fn typed_expr_to_transition_prop_return_produces_eq_output() {
        // `constant(42)` — zero-arg wrapper around a pure value.
        let (function_index, constant_index, local_values, empty_data_types, mut visiting) =
            empty_transition_prop_context();

        let int_fuzzer = Type::fuzzer(Type::int());
        let call_expr = TypedExpr::Call {
            location: Span::empty(),
            tipo: int_fuzzer.clone(),
            fun: Box::new(module_fn_var(
                "constant",
                STDLIB_FUZZ_MODULE,
                Type::function(vec![Type::int()], int_fuzzer),
            )),
            args: vec![call_arg(uint_lit("42"))],
        };

        let prop = typed_expr_to_transition_prop(
            &call_expr,
            "test_mod",
            &function_index,
            &constant_index,
            &local_values,
            &empty_data_types,
            &mut visiting,
            &BTreeSet::new(),
            &mut BTreeSet::new(),
        );

        match prop {
            TransitionProp::EqOutput(ShallowIr::Const(ShallowConst::Int(v))) => {
                assert_eq!(v, "42");
            }
            other => panic!("expected EqOutput(Int 42), got {other:?}"),
        }
    }

    #[test]
    fn typed_expr_to_transition_prop_when_produces_or_of_clauses() {
        // `when x is { a -> constant(42); b -> constant(0) }` — two arms,
        // each returning a pure Fuzzer value. Expect: Or([EqOutput 42, EqOutput 0]).
        let (function_index, constant_index, local_values, empty_data_types, mut visiting) =
            empty_transition_prop_context();

        let int_fuzzer = Type::fuzzer(Type::int());
        let return_fn_type = Type::function(vec![Type::int()], int_fuzzer.clone());

        let make_return = |value: &str| TypedExpr::Call {
            location: Span::empty(),
            tipo: int_fuzzer.clone(),
            fun: Box::new(module_fn_var(
                "constant",
                STDLIB_FUZZ_MODULE,
                return_fn_type.clone(),
            )),
            args: vec![call_arg(uint_lit(value))],
        };

        let subject = local_var("x", Type::int());
        let when_expr = TypedExpr::When {
            location: Span::empty(),
            tipo: int_fuzzer.clone(),
            subject: Box::new(subject),
            clauses: vec![
                TypedClause {
                    location: Span::empty(),
                    pattern: TypedPattern::var("a"),
                    then: make_return("42"),
                },
                TypedClause {
                    location: Span::empty(),
                    pattern: TypedPattern::var("b"),
                    then: make_return("0"),
                },
            ],
        };

        let prop = typed_expr_to_transition_prop(
            &when_expr,
            "test_mod",
            &function_index,
            &constant_index,
            &local_values,
            &empty_data_types,
            &mut visiting,
            &BTreeSet::new(),
            &mut BTreeSet::new(),
        );

        match prop {
            TransitionProp::Or(branches) => {
                assert_eq!(branches.len(), 2, "expected 2 branches, got {branches:?}");
                assert!(
                    matches!(
                        &branches[0],
                        TransitionProp::EqOutput(ShallowIr::Const(ShallowConst::Int(v)))
                            if v == "42"
                    ),
                    "expected branch 0 = EqOutput(Int 42), got {:?}",
                    branches[0]
                );
                assert!(
                    matches!(
                        &branches[1],
                        TransitionProp::EqOutput(ShallowIr::Const(ShallowConst::Int(v)))
                            if v == "0"
                    ),
                    "expected branch 1 = EqOutput(Int 0), got {:?}",
                    branches[1]
                );
            }
            other => panic!("expected Or for When, got {other:?}"),
        }
    }

    /// Helper: traverse a `TransitionProp` tree and count the leaves that
    /// would, after `verify.rs::emit_transition_prop_as_lean` runs, push
    /// an entry onto `unsupported_log`. This mirrors the production
    /// counter exactly: every `Pure`, `Unsupported`, `SubGenerator`,
    /// `Match` (with arms), and empty `Or` widens; `EqOutput`,
    /// `Exists`/`And`/`Or`/`IfThenElse` are walked recursively.
    fn count_widening_leaves(prop: &TransitionProp) -> usize {
        match prop {
            TransitionProp::Pure(_) => 1,
            TransitionProp::Unsupported { .. } => 1,
            TransitionProp::SubGenerator { .. } => 1,
            TransitionProp::Match { arms, .. } => {
                let mut n = 1; // the Match itself widens the scrutinee dispatch
                for arm in arms {
                    n += count_widening_leaves(&arm.body);
                }
                n
            }
            TransitionProp::EqOutput(_) => 0,
            TransitionProp::Exists { body, .. } => count_widening_leaves(body),
            TransitionProp::And(parts) | TransitionProp::Or(parts) => {
                if parts.is_empty() {
                    1 // empty Or widens to True
                } else {
                    parts.iter().map(count_widening_leaves).sum()
                }
            }
            TransitionProp::IfThenElse { t, e, .. } => {
                count_widening_leaves(t) + count_widening_leaves(e)
            }
        }
    }

    /// Helper: collect every `Unsupported.reason` whose text contains a
    /// given substring. Used to assert that the per-clause `[E0033]`
    /// entry has been emitted at the expected positions.
    fn collect_unsupported_reasons_containing(prop: &TransitionProp, needle: &str) -> Vec<String> {
        fn walk(prop: &TransitionProp, needle: &str, out: &mut Vec<String>) {
            match prop {
                TransitionProp::Unsupported { reason, .. } => {
                    if reason.contains(needle) {
                        out.push(reason.clone());
                    }
                }
                TransitionProp::Exists { body, .. } => walk(body, needle, out),
                TransitionProp::And(parts) | TransitionProp::Or(parts) => {
                    for p in parts {
                        walk(p, needle, out);
                    }
                }
                TransitionProp::Match { arms, .. } => {
                    for arm in arms {
                        walk(&arm.body, needle, out);
                    }
                }
                TransitionProp::IfThenElse { t, e, .. } => {
                    walk(t, needle, out);
                    walk(e, needle, out);
                }
                TransitionProp::Pure(_)
                | TransitionProp::EqOutput(_)
                | TransitionProp::SubGenerator { .. } => {}
            }
        }
        let mut out = Vec::new();
        walk(prop, needle, &mut out);
        out
    }

    /// H2 / E0033 — `when` clauses that destructure constructor patterns
    /// must each contribute one widening note to `unsupported_log`. The
    /// counter the verify-side pipeline reads (`over_approximations`) is
    /// the length of that log, so we assert the structural invariant
    /// directly: every constructor clause produces exactly one
    /// `Unsupported` leaf carrying the `[E0033]` marker. This is the
    /// `when_constructor_pattern_increments_over_approximations` row
    /// from plan §"Test Plan" §B.
    #[test]
    fn when_constructor_pattern_increments_over_approximations() {
        let (function_index, constant_index, local_values, empty_data_types, mut visiting) =
            empty_transition_prop_context();

        let int_fuzzer = Type::fuzzer(Type::int());
        let return_fn_type = Type::function(vec![Type::int()], int_fuzzer.clone());
        let make_return = |value: &str| TypedExpr::Call {
            location: Span::empty(),
            tipo: int_fuzzer.clone(),
            fun: Box::new(module_fn_var(
                "constant",
                STDLIB_FUZZ_MODULE,
                return_fn_type.clone(),
            )),
            args: vec![call_arg(uint_lit(value))],
        };

        // Build a `when` with three clauses:
        //   Some(x) -> constant(1)        (constructor: should log)
        //   None    -> constant(2)        (constructor, no binders: should still log)
        //   _other  -> constant(3)        (Discard: must NOT log)
        let option_ty = Rc::new(Type::App {
            public: true,
            contains_opaque: false,
            module: "option".to_string(),
            name: "Option".to_string(),
            args: vec![Type::int()],
            alias: None,
        });
        let some_pattern = TypedPattern::constructor(
            "Some",
            &[CallArg::var("x", Span::empty())],
            option_ty.clone(),
            Span::create(10, 7),
        );
        let none_pattern =
            TypedPattern::constructor("None", &[], option_ty.clone(), Span::create(20, 4));
        let discard_pattern = TypedPattern::Discard {
            name: "_other".to_string(),
            location: Span::create(30, 6),
        };

        let when_expr = TypedExpr::When {
            location: Span::empty(),
            tipo: int_fuzzer.clone(),
            subject: Box::new(local_var("x", option_ty)),
            clauses: vec![
                TypedClause {
                    location: Span::empty(),
                    pattern: some_pattern,
                    then: make_return("1"),
                },
                TypedClause {
                    location: Span::empty(),
                    pattern: none_pattern,
                    then: make_return("2"),
                },
                TypedClause {
                    location: Span::empty(),
                    pattern: discard_pattern,
                    then: make_return("3"),
                },
            ],
        };

        let prop = typed_expr_to_transition_prop(
            &when_expr,
            "test_mod",
            &function_index,
            &constant_index,
            &local_values,
            &empty_data_types,
            &mut visiting,
            &BTreeSet::new(),
            &mut BTreeSet::new(),
        );

        // The two constructor clauses contribute one `[E0033]` Unsupported
        // each; the discard clause contributes none. The remaining branch
        // bodies are pure `EqOutput` and don't widen.
        let e0033_entries = collect_unsupported_reasons_containing(&prop, "[E0033]");
        assert_eq!(
            e0033_entries.len(),
            2,
            "expected exactly 2 [E0033] widening entries (one per constructor clause), \
             got {} — entries: {:?}",
            e0033_entries.len(),
            e0033_entries,
        );

        // The total widening-leaf count (the production
        // `over_approximations` counter when the verify-side lowering
        // runs) must be exactly 2 here — no other widening source is
        // present in this lowered tree.
        let total = count_widening_leaves(&prop);
        assert_eq!(
            total, 2,
            "expected over_approximations=2 (one per constructor clause), got {total}",
        );

        // Outer shape is still `Or` over branches — the wrap is
        // structurally inside each branch via `And`, not a sibling.
        match &prop {
            TransitionProp::Or(branches) => {
                assert_eq!(branches.len(), 3, "expected 3 branches, got {branches:?}");
                assert!(
                    matches!(&branches[0], TransitionProp::And(parts) if parts.len() == 2),
                    "branch 0 (Some) must be And([Unsupported, body]), got {:?}",
                    branches[0],
                );
                assert!(
                    matches!(&branches[1], TransitionProp::And(parts) if parts.len() == 2),
                    "branch 1 (None) must be And([Unsupported, body]), got {:?}",
                    branches[1],
                );
                assert!(
                    !matches!(&branches[2], TransitionProp::And(_)),
                    "branch 2 (Discard) must NOT be wrapped in And, got {:?}",
                    branches[2],
                );
            }
            other => panic!("expected Or for When, got {other:?}"),
        }
    }

    /// H2 / E0033 — the per-clause `[E0033]` log entry must include both
    /// the human-readable pattern description AND every binder name the
    /// pattern would have introduced. This is the
    /// `when_constructor_pattern_log_includes_pattern_and_binders` row
    /// from plan §"Test Plan" §B.
    #[test]
    fn when_constructor_pattern_log_includes_pattern_and_binders() {
        let (function_index, constant_index, local_values, empty_data_types, mut visiting) =
            empty_transition_prop_context();

        let int_fuzzer = Type::fuzzer(Type::int());
        let return_fn_type = Type::function(vec![Type::int()], int_fuzzer.clone());
        let make_return = |value: &str| TypedExpr::Call {
            location: Span::empty(),
            tipo: int_fuzzer.clone(),
            fun: Box::new(module_fn_var(
                "constant",
                STDLIB_FUZZ_MODULE,
                return_fn_type.clone(),
            )),
            args: vec![call_arg(uint_lit(value))],
        };

        // Construct a single-clause `when` with `Cons(head, tail)` so we
        // can pin both binder names in the log entry. The clause-pattern
        // `Span::create(7, 14)` lets us also assert the source-location
        // suffix (start byte == 7).
        let list_ty = Rc::new(Type::App {
            public: true,
            contains_opaque: false,
            module: "stdlib".to_string(),
            name: "List".to_string(),
            args: vec![Type::int()],
            alias: None,
        });
        let cons_pattern = TypedPattern::constructor(
            "Cons",
            &[
                CallArg::var("head", Span::empty()),
                CallArg::var("tail", Span::empty()),
            ],
            list_ty.clone(),
            Span::create(7, 14),
        );
        // A second discard clause keeps the outer `Or` structure non-trivial
        // (single-branch `When` collapses to the inner branch).
        let discard_pattern = TypedPattern::Discard {
            name: "_".to_string(),
            location: Span::empty(),
        };

        let when_expr = TypedExpr::When {
            location: Span::empty(),
            tipo: int_fuzzer.clone(),
            subject: Box::new(local_var("xs", list_ty)),
            clauses: vec![
                TypedClause {
                    location: Span::empty(),
                    pattern: cons_pattern,
                    then: make_return("1"),
                },
                TypedClause {
                    location: Span::empty(),
                    pattern: discard_pattern,
                    then: make_return("0"),
                },
            ],
        };

        let prop = typed_expr_to_transition_prop(
            &when_expr,
            "permissions/test",
            &function_index,
            &constant_index,
            &local_values,
            &empty_data_types,
            &mut visiting,
            &BTreeSet::new(),
            &mut BTreeSet::new(),
        );

        let e0033_entries = collect_unsupported_reasons_containing(&prop, "[E0033]");
        assert_eq!(
            e0033_entries.len(),
            1,
            "expected exactly one [E0033] entry for the Cons clause, \
             got {} — entries: {:?}",
            e0033_entries.len(),
            e0033_entries,
        );
        let reason = &e0033_entries[0];
        // Pattern description must include the constructor head and binders
        // (i.e. "Cons(head, tail)").
        assert!(
            reason.contains("Cons(head, tail)"),
            "reason must include the pattern description `Cons(head, tail)`; \
             got {reason:?}",
        );
        // Each binder name must appear individually in the human-readable list.
        assert!(
            reason.contains("head"),
            "reason must list the `head` binder; got {reason:?}",
        );
        assert!(
            reason.contains("tail"),
            "reason must list the `tail` binder; got {reason:?}",
        );
        // Catalogue feature slug appears verbatim so capabilities/JSON can
        // grep for it without parsing the wider sentence.
        assert!(
            reason.contains("when_pattern_constructor_var_dropped"),
            "reason must include the feature slug; got {reason:?}",
        );
        // Module + clause-pattern start byte appear in the source-location
        // suffix produced by the format string.
        assert!(
            reason.contains("permissions/test:7"),
            "reason must include `<module>:<start>` (`permissions/test:7`); \
             got {reason:?}",
        );

        // Also verify the structured `source_location` field carries the
        // same `module:start` string — this is what `verify.rs` appends
        // as the `@ ...` suffix in `unsupported_log`.
        fn find_e0033_source(prop: &TransitionProp) -> Option<String> {
            match prop {
                TransitionProp::Unsupported {
                    reason,
                    source_location,
                } if reason.contains("[E0033]") => source_location.clone(),
                TransitionProp::Exists { body, .. } => find_e0033_source(body),
                TransitionProp::And(parts) | TransitionProp::Or(parts) => {
                    parts.iter().find_map(find_e0033_source)
                }
                TransitionProp::Match { arms, .. } => {
                    arms.iter().find_map(|arm| find_e0033_source(&arm.body))
                }
                TransitionProp::IfThenElse { t, e, .. } => {
                    find_e0033_source(t).or_else(|| find_e0033_source(e))
                }
                _ => None,
            }
        }
        assert_eq!(
            find_e0033_source(&prop).as_deref(),
            Some("permissions/test:7"),
            "source_location must be `<module>:<clause-pattern-start>`",
        );
    }

    /// H2 / E0033 — `Var` and `Discard` patterns must NOT trigger the
    /// per-clause widening note: they bind everything (or nothing) and
    /// drop no constructor-conditional constraints. This pins the
    /// negative case so a future refactor cannot accidentally widen the
    /// trigger condition.
    #[test]
    fn when_var_or_discard_pattern_does_not_log_e0033() {
        let (function_index, constant_index, local_values, empty_data_types, mut visiting) =
            empty_transition_prop_context();

        let int_fuzzer = Type::fuzzer(Type::int());
        let return_fn_type = Type::function(vec![Type::int()], int_fuzzer.clone());
        let make_return = |value: &str| TypedExpr::Call {
            location: Span::empty(),
            tipo: int_fuzzer.clone(),
            fun: Box::new(module_fn_var(
                "constant",
                STDLIB_FUZZ_MODULE,
                return_fn_type.clone(),
            )),
            args: vec![call_arg(uint_lit(value))],
        };

        let when_expr = TypedExpr::When {
            location: Span::empty(),
            tipo: int_fuzzer.clone(),
            subject: Box::new(local_var("x", Type::int())),
            clauses: vec![
                TypedClause {
                    location: Span::empty(),
                    pattern: TypedPattern::var("a"),
                    then: make_return("42"),
                },
                TypedClause {
                    location: Span::empty(),
                    pattern: TypedPattern::Discard {
                        name: "_".to_string(),
                        location: Span::empty(),
                    },
                    then: make_return("0"),
                },
            ],
        };

        let prop = typed_expr_to_transition_prop(
            &when_expr,
            "test_mod",
            &function_index,
            &constant_index,
            &local_values,
            &empty_data_types,
            &mut visiting,
            &BTreeSet::new(),
            &mut BTreeSet::new(),
        );

        let e0033_entries = collect_unsupported_reasons_containing(&prop, "[E0033]");
        assert!(
            e0033_entries.is_empty(),
            "Var/Discard clauses must not emit [E0033] entries; got {e0033_entries:?}",
        );
        // And the structural shape is the original Or([EqOutput, EqOutput])
        // — no `And` wrapping introduced.
        match &prop {
            TransitionProp::Or(branches) => {
                assert_eq!(branches.len(), 2);
                assert!(matches!(
                    branches[0],
                    TransitionProp::EqOutput(ShallowIr::Const(ShallowConst::Int(_)))
                ));
                assert!(matches!(
                    branches[1],
                    TransitionProp::EqOutput(ShallowIr::Const(ShallowConst::Int(_)))
                ));
            }
            other => panic!("expected Or, got {other:?}"),
        }
    }

    /// Defensive unit test for `describe_pattern` and
    /// `collect_pattern_binders` — these helpers are used inside the
    /// per-clause E0033 audit log and must produce stable text for the
    /// patterns the lowering encounters in practice.
    #[test]
    fn describe_pattern_and_collect_binders_cover_common_shapes() {
        let int_ty = Type::int();
        let option_int = Rc::new(Type::App {
            public: true,
            contains_opaque: false,
            module: "option".to_string(),
            name: "Option".to_string(),
            args: vec![int_ty.clone()],
            alias: None,
        });

        // `Var x`
        let var_pat = TypedPattern::var("x");
        assert_eq!(describe_pattern(&var_pat), "x");
        assert_eq!(collect_pattern_binders(&var_pat), vec!["x".to_string()]);

        // `_`
        let discard = TypedPattern::Discard {
            name: "_".to_string(),
            location: Span::empty(),
        };
        assert_eq!(describe_pattern(&discard), "_");
        assert!(collect_pattern_binders(&discard).is_empty());

        // `Some(x)`
        let some_x = TypedPattern::constructor(
            "Some",
            &[CallArg::var("x", Span::empty())],
            option_int.clone(),
            Span::empty(),
        );
        assert_eq!(describe_pattern(&some_x), "Some(x)");
        assert_eq!(collect_pattern_binders(&some_x), vec!["x".to_string()]);

        // `None`
        let none = TypedPattern::constructor("None", &[], option_int, Span::empty());
        assert_eq!(describe_pattern(&none), "None");
        assert!(collect_pattern_binders(&none).is_empty());

        // `(a, _, c)` tuple
        let tuple = TypedPattern::Tuple {
            location: Span::empty(),
            elems: vec![
                TypedPattern::var("a"),
                TypedPattern::Discard {
                    name: "_".to_string(),
                    location: Span::empty(),
                },
                TypedPattern::var("c"),
            ],
        };
        assert_eq!(describe_pattern(&tuple), "(a, _, c)");
        assert_eq!(
            collect_pattern_binders(&tuple),
            vec!["a".to_string(), "c".to_string()]
        );

        // `[head, ..tail]`
        let list = TypedPattern::List {
            location: Span::empty(),
            elements: vec![TypedPattern::var("head")],
            tail: Some(Box::new(TypedPattern::var("tail"))),
        };
        assert_eq!(describe_pattern(&list), "[head, ..tail]");
        assert_eq!(
            collect_pattern_binders(&list),
            vec!["head".to_string(), "tail".to_string()]
        );
    }

    #[test]
    fn typed_expr_to_transition_prop_inlines_resolvable_sub_generator() {
        // A module-level sub-generator whose body is translatable should be
        // inlined into the resulting `TransitionProp`, not emitted as an
        // opaque `SubGenerator` stub. Here, `my_gen()` has body
        // `constant(42)`, which translates to `EqOutput(Int 42)`. The outer
        // call `my_gen()` from another module should therefore translate to
        // the same shape rather than `SubGenerator { ... }`.
        let int_fuzzer = Type::fuzzer(Type::int());

        // Body of the sub-generator: `constant(42)`.
        let body = TypedExpr::Call {
            location: Span::empty(),
            tipo: int_fuzzer.clone(),
            fun: Box::new(module_fn_var(
                "constant",
                STDLIB_FUZZ_MODULE,
                Type::function(vec![Type::int()], int_fuzzer.clone()),
            )),
            args: vec![call_arg(uint_lit("42"))],
        };

        let sub_gen_fn = TypedFunction {
            arguments: vec![],
            body,
            doc: None,
            location: Span::empty(),
            name: "my_gen".to_string(),
            public: true,
            return_annotation: None,
            return_type: int_fuzzer.clone(),
            end_position: 0,
            on_test_failure: OnTestFailure::FailImmediately,
        };

        let key = FunctionAccessKey {
            module_name: "gens".to_string(),
            function_name: "my_gen".to_string(),
        };

        let mut known_functions: IndexMap<&FunctionAccessKey, &TypedFunction> = IndexMap::new();
        known_functions.insert(&key, &sub_gen_fn);
        let function_index = index_known_functions(&known_functions);

        let constant_index: ConstantIndex<'_> = HashMap::new();
        let local_values: BTreeMap<String, TypedExpr> = BTreeMap::new();
        let empty_data_types: IndexMap<&DataTypeKey, &TypedDataType> = IndexMap::new();
        let mut visiting: BTreeSet<(String, String)> = BTreeSet::new();

        // Outer call: `my_gen()` from a different module.
        let call_expr = TypedExpr::Call {
            location: Span::empty(),
            tipo: int_fuzzer.clone(),
            fun: Box::new(module_fn_var(
                "my_gen",
                "gens",
                Type::function(vec![], int_fuzzer),
            )),
            args: vec![],
        };

        let prop = typed_expr_to_transition_prop(
            &call_expr,
            "caller_mod",
            &function_index,
            &constant_index,
            &local_values,
            &empty_data_types,
            &mut visiting,
            &BTreeSet::new(),
            &mut BTreeSet::new(),
        );

        match prop {
            TransitionProp::EqOutput(ShallowIr::Const(ShallowConst::Int(v))) => {
                assert_eq!(v, "42", "expected inlined body EqOutput(Int 42)");
            }
            TransitionProp::SubGenerator { module, fn_name } => panic!(
                "expected inlined EqOutput(Int 42), but got SubGenerator {{ {module}.{fn_name} }}",
            ),
            other => panic!("expected inlined EqOutput(Int 42), got {other:?}"),
        }
    }

    #[test]
    fn typed_expr_to_transition_prop_collects_non_monadic_let_bindings() {
        // Regression: a non-Fuzzer `let` binding preceding the terminal
        // `fuzz.return(...)` of a step body must be collected into
        // `local_values` so that `Var` references inside the terminal can be
        // resolved. Prior to this fix, `terminal_expression` peeled the
        // sequence wrapper and discarded the binding entirely, so any inner
        // `Var "redeemer"` reference produced an opaque/Unsupported result.
        //
        // Body shape:
        //
        //     let redeemer = 42
        //     fuzz.return(Step(redeemer))
        //
        // Expected: `EqOutput(Construct { constructor: "Step", ... })` —
        // demonstrating the path runs cleanly with the binding in scope.
        let (owned, scenario_ty) = scenario_like_data_types();
        let data_types: IndexMap<&DataTypeKey, &TypedDataType> = owned.iter().collect();

        let (function_index, constant_index, _, _, mut visiting) = empty_transition_prop_context();

        // `Step` constructor: `fn(Data) -> Scenario`, second constructor (tag 1).
        let step_fn_ty = Type::function(vec![Type::int()], scenario_ty.clone());
        let step_ctor = TypedExpr::Var {
            location: Span::empty(),
            constructor: ValueConstructor::public(
                step_fn_ty,
                ValueConstructorVariant::Record {
                    name: "Step".to_string(),
                    arity: 1,
                    field_map: None,
                    location: Span::empty(),
                    module: "mod".to_string(),
                    constructors_count: 2,
                },
            ),
            name: "Step".to_string(),
        };

        // Local `Var "redeemer"` reference (LocalVariable) — used inside the
        // terminal's payload position.
        let redeemer_var = local_var("redeemer", Type::int());

        // `Step(redeemer)` — the payload of `fuzz.return`.
        let step_call = TypedExpr::Call {
            location: Span::empty(),
            tipo: scenario_ty.clone(),
            fun: Box::new(step_ctor),
            args: vec![call_arg(redeemer_var)],
        };

        // `fuzz.return(Step(redeemer))` — Fuzzer<Scenario>.
        let scenario_fuzzer_ty = Type::fuzzer(scenario_ty.clone());
        let return_fn_ty = Type::function(vec![scenario_ty.clone()], scenario_fuzzer_ty.clone());
        let fuzz_return_call = TypedExpr::Call {
            location: Span::empty(),
            tipo: scenario_fuzzer_ty.clone(),
            fun: Box::new(module_fn_var("constant", STDLIB_FUZZ_MODULE, return_fn_ty)),
            args: vec![call_arg(step_call)],
        };

        // `let redeemer = 42` — non-Fuzzer Int binding, MUST be collected.
        let assignment = TypedExpr::Assignment {
            location: Span::empty(),
            tipo: Type::int(),
            value: Box::new(uint_lit("42")),
            pattern: TypedPattern::var("redeemer"),
            kind: crate::ast::AssignmentKind::Let { backpassing: () },
            comment: None,
        };

        // Sequence: [ let redeemer = 42; fuzz.return(Step(redeemer)) ]
        let sequence = TypedExpr::Sequence {
            location: Span::empty(),
            expressions: vec![assignment, fuzz_return_call],
        };

        // Empty starting local_values — the binding must be picked up from
        // the leading Assignment in the Sequence.
        let local_values: BTreeMap<String, TypedExpr> = BTreeMap::new();

        let prop = typed_expr_to_transition_prop(
            &sequence,
            "test_mod",
            &function_index,
            &constant_index,
            &local_values,
            &data_types,
            &mut visiting,
            &BTreeSet::new(),
            &mut BTreeSet::new(),
        );

        // The result must be `EqOutput(Construct { Step, ... })` — NOT
        // `Unsupported` (which would indicate the `let` binding was dropped
        // and the path failed) and NOT a bare opaque shape.
        match prop {
            TransitionProp::EqOutput(ShallowIr::Construct {
                ref constructor,
                tag,
                ref fields,
                ..
            }) => {
                assert_eq!(constructor, "Step");
                assert_eq!(tag, 1, "Step is the second constructor (index 1)");
                assert_eq!(fields.len(), 1, "Step has exactly one field (redeemer)");
                // The redeemer field — whether substituted to a constant
                // or left as a Var — must NOT be an Opaque/Unsupported
                // marker indicating a translation failure.
                assert!(
                    !matches!(fields[0], ShallowIr::Opaque { .. }),
                    "redeemer field must not translate to an Opaque marker: {:?}",
                    fields[0]
                );
            }
            TransitionProp::Unsupported { reason, .. } => {
                panic!("expected EqOutput(Construct {{ Step }}), got Unsupported: {reason}")
            }
            other => panic!("expected EqOutput(Construct {{ Step }}), got {other:?}"),
        }
    }

    /// H3 — naive substitute-on-recurse threads a bind binder into
    /// `local_values` so a `Var` lookup inside the continuation body
    /// resolves to the source fuzzer expression instead of widening to a
    /// vacuous `Unsupported` (which the Lean stage would translate to
    /// `True`).
    ///
    /// Pre-H3 behaviour: `and_then(int_between(0, 10), fn(x) { x })`
    /// produced `Exists { binder: x, body: Unsupported("...variable 'x'
    /// has no known transition content") }`. The body widened to `True`
    /// downstream, dropping the `int_between(0, 10)` constraint
    /// completely.
    ///
    /// Post-H3 behaviour: the same expression produces `Exists { binder:
    /// x, body: SubGenerator { module: "aiken/fuzz", fn_name:
    /// "int_between" } }`. The threaded `x -> int_between(0, 10)`
    /// substitution causes the `Var x` lookup to recurse into the source
    /// fuzzer expression, which lowers via the existing fuzzer
    /// recognition to a `SubGenerator` stub — preserving the *name* of
    /// the source so the generated Lean is auditable instead of vacuous.
    #[test]
    fn translate_bind_threading_resolves_var_lookup() {
        let (function_index, constant_index, local_values, empty_data_types, mut visiting) =
            empty_transition_prop_context();

        let int_type = Type::int();

        // Source: `int_between(0, 10)` — typed `Fuzzer<Int>`.
        let source = make_typed_int_between_fuzzer("0", "10");

        // Continuation body is the bare local variable reference `x`.
        // Crucially this is NOT wrapped in a `return(...)`, so the body
        // path goes through the `TypedExpr::Var` arm of
        // `typed_expr_to_transition_prop` (the Var-lookup site). This is
        // the path H3 thread enables; pre-H3 the lookup would miss
        // because nothing extends `local_values` for monadic binds.
        let cont_body = local_var("x", int_type.clone());

        // Continuation: `fn(x: Int) -> Fuzzer<Int> { x }`. The synthetic
        // body type doesn't match the declared Fuzzer return type — but
        // `typed_expr_to_transition_prop` does not typecheck here; the
        // shape detector only requires `expression_is_bind_continuation`
        // to recognise this as a 1-arg fn returning a Fuzzer.
        let cont =
            make_inline_bind_continuation("x", int_type.clone(), cont_body, int_type.clone());

        let bind_call = make_typed_bind_call(source, cont, int_type.clone());

        let prop = typed_expr_to_transition_prop(
            &bind_call,
            "test_mod",
            &function_index,
            &constant_index,
            &local_values,
            &empty_data_types,
            &mut visiting,
            &BTreeSet::new(),
            &mut BTreeSet::new(),
        );

        match prop {
            TransitionProp::Exists { binder, body, .. } => {
                assert_eq!(binder, "x", "expected binder 'x', got '{binder}'");
                match *body {
                    TransitionProp::SubGenerator { module, fn_name } => {
                        assert_eq!(
                            module, "aiken/fuzz",
                            "expected H3 to resolve `Var x` -> `int_between(0,10)` -> SubGenerator(aiken/fuzz, int_between); got module {module}",
                        );
                        assert_eq!(
                            fn_name, "int_between",
                            "expected fn_name `int_between`; got {fn_name}",
                        );
                    }
                    TransitionProp::Unsupported { reason, .. } => {
                        panic!(
                            "H3 regression: bind body widened to Unsupported, threading lost. \
                             Pre-H3 reason was 'variable x has no known transition content'; \
                             post-H3 should resolve via local_values to a SubGenerator stub. \
                             got: {reason}"
                        )
                    }
                    other => {
                        panic!("expected SubGenerator(aiken/fuzz, int_between), got {other:?}")
                    }
                }
            }
            other => panic!("expected Exists {{ binder: x, .. }}, got {other:?}"),
        }
    }

    /// H3 cycle guard — when a nested `translate_bind` re-binds a name
    /// that's already on the visiting stack, the inner bind must NOT
    /// shadow the outer entry in `local_values` (doing so naively would
    /// either lose the outer binding when the inner scope ends or, worse,
    /// loop on Var-lookup recursion). Instead the inner site emits a
    /// `TransitionProp::Unsupported` audit marker and wraps the body in
    /// `And([Unsupported, body_prop])` so:
    ///   1. The cycle is broken (no infinite recursion).
    ///   2. The precision loss is logged in the unsupported-log via the
    ///      Lean widening pipeline (the marker widens to `True`,
    ///      preserving body semantics: `True ∧ body_prop = body_prop`).
    ///   3. The reason text contains the stable substring
    ///      "cyclic monadic-bind binder" and the offending binder name,
    ///      so downstream tooling can detect this case.
    #[test]
    fn translate_bind_cycle_guard_falls_through_to_unsupported() {
        let (function_index, constant_index, local_values, empty_data_types, mut visiting) =
            empty_transition_prop_context();

        let int_type = Type::int();

        // Inner cont body is `Var x`. After inner-bind substitution is
        // skipped by the cycle guard, `local_values` still has the *outer*
        // `x -> int_between(0, 10)` entry — so the Var lookup resolves
        // through the outer source. This is the precision-imprecise
        // fallthrough the Unsupported marker documents.
        let inner_cont_body = local_var("x", int_type.clone());
        let inner_cont =
            make_inline_bind_continuation("x", int_type.clone(), inner_cont_body, int_type.clone());
        // Inner source: a *different* fuzzer, so we can verify which
        // source the body is substituted with after the cycle is hit.
        let inner_source = make_typed_int_between_fuzzer("100", "200");
        let inner_bind = make_typed_bind_call(inner_source, inner_cont, int_type.clone());

        // Outer continuation wraps the inner bind. Its arg is also `x`,
        // so the inner site sees `binder = "x"` already in
        // `visiting_locals`.
        let outer_cont =
            make_inline_bind_continuation("x", int_type.clone(), inner_bind, int_type.clone());
        let outer_source = make_typed_int_between_fuzzer("0", "10");
        let outer_bind = make_typed_bind_call(outer_source, outer_cont, int_type.clone());

        let prop = typed_expr_to_transition_prop(
            &outer_bind,
            "test_mod",
            &function_index,
            &constant_index,
            &local_values,
            &empty_data_types,
            &mut visiting,
            &BTreeSet::new(),
            &mut BTreeSet::new(),
        );

        // Outer bind: Exists { binder: x, body: <inner exists> }
        let inner_exists_body = match prop {
            TransitionProp::Exists { binder, body, .. } => {
                assert_eq!(binder, "x", "outer binder should be x; got {binder}");
                *body
            }
            other => panic!("expected outer Exists {{ binder: x, .. }}, got {other:?}"),
        };

        // Inner bind: Exists { binder: x, body: And([Unsupported, body_prop]) }
        let inner_body = match inner_exists_body {
            TransitionProp::Exists { binder, body, .. } => {
                assert_eq!(binder, "x", "inner binder should be x; got {binder}");
                *body
            }
            other => panic!("expected inner Exists {{ binder: x, .. }}, got {other:?}"),
        };

        // Body must be And([Unsupported(cyclic...), body_prop]).
        match inner_body {
            TransitionProp::And(parts) => {
                assert_eq!(
                    parts.len(),
                    2,
                    "expected 2-part And([Unsupported, body_prop]); got {} parts: {parts:?}",
                    parts.len(),
                );
                // First part: Unsupported with cycle-guard reason text.
                match &parts[0] {
                    TransitionProp::Unsupported {
                        reason,
                        source_location,
                    } => {
                        assert!(
                            reason.contains("cyclic monadic-bind binder"),
                            "expected reason to contain stable substring 'cyclic monadic-bind binder'; got: {reason}",
                        );
                        assert!(
                            reason.contains("'x'"),
                            "expected reason to mention binder name 'x'; got: {reason}",
                        );
                        assert!(
                            source_location
                                .as_ref()
                                .is_some_and(|s| s.starts_with("test_mod:")),
                            "expected source_location to be `test_mod:<offset>`; got: {source_location:?}",
                        );
                    }
                    other => panic!("expected first part Unsupported; got {other:?}"),
                }
                // Second part: body_prop. The body resolves `Var x`
                // through `local_values`, which still has the OUTER
                // binding (cycle guard skipped the inner shadow). So
                // `Var x` -> outer source `int_between(0, 10)` ->
                // SubGenerator(aiken/fuzz, int_between). The naive
                // scheme intentionally produces this incorrect-shadowing
                // result — the Unsupported marker is the audit log.
                match &parts[1] {
                    TransitionProp::SubGenerator { module, fn_name } => {
                        assert_eq!(module, "aiken/fuzz");
                        assert_eq!(fn_name, "int_between");
                    }
                    other => panic!(
                        "expected second part SubGenerator(aiken/fuzz, int_between) \
                         (resolved through OUTER binding because cycle guard skipped inner shadow); \
                         got {other:?}"
                    ),
                }
            }
            other => panic!("expected inner body And([Unsupported, body_prop]); got {other:?}"),
        }
    }

    /// Defensive control: a simple bind whose continuation uses
    /// `return(x)` short-circuits via `detect_return_call`, lowering the
    /// body via `typed_expr_to_shallow_ir` (which does NOT consult
    /// `local_values`). H3 must NOT change this established behaviour —
    /// the result remains `Exists { body: EqOutput(Var x) }`.
    #[test]
    fn translate_bind_no_threading_for_return_shortcut_unchanged() {
        let (function_index, constant_index, local_values, empty_data_types, mut visiting) =
            empty_transition_prop_context();

        let int_fuzzer = Type::fuzzer(Type::int());
        let source = make_typed_int_between_fuzzer("0", "10");

        let x = local_var("x", Type::int());
        let return_body = TypedExpr::Call {
            location: Span::empty(),
            tipo: int_fuzzer.clone(),
            fun: Box::new(module_fn_var(
                "constant",
                STDLIB_FUZZ_MODULE,
                Type::function(vec![Type::int()], int_fuzzer.clone()),
            )),
            args: vec![call_arg(x)],
        };

        let continuation =
            make_inline_bind_continuation("x", Type::int(), return_body, Type::int());

        let bind_call = make_typed_bind_call(source, continuation, Type::int());

        let prop = typed_expr_to_transition_prop(
            &bind_call,
            "test_mod",
            &function_index,
            &constant_index,
            &local_values,
            &empty_data_types,
            &mut visiting,
            &BTreeSet::new(),
            &mut BTreeSet::new(),
        );

        match prop {
            TransitionProp::Exists { binder, body, .. } => {
                assert_eq!(binder, "x");
                assert!(
                    matches!(
                        *body,
                        TransitionProp::EqOutput(ShallowIr::Var { ref name, .. })
                            if name == "x"
                    ),
                    "expected body = EqOutput(Var x), got {body:?}"
                );
            }
            other => panic!("expected Exists, got {other:?}"),
        }
    }

    /// H3 self-referential source guard — when the bind's source contains
    /// a free `Var` whose name collides with the binder, the naive
    /// substitution `local_values[binder] = source` is self-referential
    /// (e.g. `local_values["x"] = Var "x"`). Pre-fix the `Var`-lookup arm
    /// would recurse on itself indefinitely, stack-overflowing the
    /// lowering. Post-fix the bind site detects the self-reference,
    /// skips the threading, and emits a `TransitionProp::Unsupported`
    /// audit marker (with the distinct phrase
    /// `self-referential monadic-bind binder` so it can be filtered
    /// separately from the bind-on-bind cycle marker).
    ///
    /// Mirrors legal Aiken: `let g = some_fuzzer; and_then(g, fn(g) { g })`.
    /// The inner `g` is a fresh continuation parameter shadowing the outer
    /// `g`, but the bind's *source* expression is a bare `Var g`
    /// referring to the outer binding — so when `translate_bind` would
    /// extend `local_values["g"] -> Var "g"`, the `Var` arm has no
    /// terminating substitution.
    #[test]
    fn translate_bind_self_referential_source_emits_unsupported() {
        let (function_index, constant_index, local_values, empty_data_types, mut visiting) =
            empty_transition_prop_context();

        let int_type = Type::int();
        let int_fuzzer = Type::fuzzer(int_type.clone());

        // Source: `Var "x"` typed as `Fuzzer<Int>` — a free reference to
        // some outer `x` of type `Fuzzer<Int>`. This satisfies
        // `expression_has_fuzzer_type` (so the bind shape detector
        // accepts it) AND its name collides with the continuation
        // parameter `x` below — so the naive
        // `extended_locals.insert("x", Var "x")` is self-referential.
        let source = local_var("x", int_fuzzer.clone());

        // Continuation body is the bare local variable reference `x`.
        // Crucially this is NOT wrapped in `return(...)`, so the body
        // path goes through the `TypedExpr::Var` arm of
        // `typed_expr_to_transition_prop` (the Var-lookup site that
        // pre-fix loops on the self-referential entry).
        let cont_body = local_var("x", int_type.clone());
        let cont =
            make_inline_bind_continuation("x", int_type.clone(), cont_body, int_type.clone());

        let bind_call = make_typed_bind_call(source, cont, int_type.clone());

        let prop = typed_expr_to_transition_prop(
            &bind_call,
            "test_mod",
            &function_index,
            &constant_index,
            &local_values,
            &empty_data_types,
            &mut visiting,
            &BTreeSet::new(),
            &mut BTreeSet::new(),
        );

        // Outer bind: Exists { binder: x, body: And([Unsupported, body_prop]) }
        let body = match prop {
            TransitionProp::Exists { binder, body, .. } => {
                assert_eq!(binder, "x", "expected binder 'x', got '{binder}'");
                *body
            }
            other => panic!("expected Exists {{ binder: x, .. }}, got {other:?}"),
        };

        match body {
            TransitionProp::And(parts) => {
                assert_eq!(
                    parts.len(),
                    2,
                    "expected 2-part And([Unsupported, body_prop]); got {} parts: {parts:?}",
                    parts.len(),
                );
                // First part: Unsupported with self-referential reason.
                match &parts[0] {
                    TransitionProp::Unsupported {
                        reason,
                        source_location,
                    } => {
                        assert!(
                            reason.contains("self-referential monadic-bind binder"),
                            "expected reason to contain stable substring \
                             'self-referential monadic-bind binder' \
                             (distinct from 'cyclic monadic-bind binder' so future filtering \
                             can disambiguate); got: {reason}",
                        );
                        assert!(
                            reason.contains("'x'"),
                            "expected reason to mention binder name 'x'; got: {reason}",
                        );
                        assert!(
                            source_location
                                .as_ref()
                                .is_some_and(|s| s.starts_with("test_mod:")),
                            "expected source_location to be `test_mod:<offset>`; got: {source_location:?}",
                        );
                    }
                    other => panic!("expected first part Unsupported; got {other:?}"),
                }
                // Second part: body_prop. Without the threaded
                // substitution (skipped by the self-reference guard),
                // `Var x` lookup misses `local_values` and falls through
                // to the `Unsupported` arm in `typed_expr_to_transition_prop`
                // ("variable 'x' has no known transition content").
                // Since `Var "x"` was the body, body_prop is that
                // Unsupported message.
                match &parts[1] {
                    TransitionProp::Unsupported { reason, .. } => {
                        assert!(
                            reason.contains("variable 'x' has no known transition content"),
                            "expected body_prop = Unsupported(no known transition content); \
                             got reason: {reason}",
                        );
                    }
                    other => panic!(
                        "expected second part Unsupported (Var lookup misses without threading); \
                         got {other:?}"
                    ),
                }
            }
            other => panic!("expected body And([Unsupported, body_prop]); got {other:?}"),
        }
    }

    /// Indirect self-reference: the binder name appears in a *nested*
    /// position within the bind's source expression (here, as an
    /// argument to a `Call`). The free-vars walker must recurse through
    /// `Call::args` to detect this — a shallow check would miss it.
    /// Same expected outcome as the direct-reference case.
    #[test]
    fn translate_bind_self_referential_nested_source_emits_unsupported() {
        let (function_index, constant_index, local_values, empty_data_types, mut visiting) =
            empty_transition_prop_context();

        let int_type = Type::int();
        let int_fuzzer = Type::fuzzer(int_type.clone());

        // Source: `some_fn(Var "x")` typed as `Fuzzer<Int>`. The free
        // `Var "x"` is nested inside `Call::args`, not at the top level.
        let source = TypedExpr::Call {
            location: Span::empty(),
            tipo: int_fuzzer.clone(),
            fun: Box::new(module_fn_var(
                "wrap_int",
                "math",
                Type::function(vec![int_type.clone()], int_fuzzer.clone()),
            )),
            args: vec![call_arg(local_var("x", int_type.clone()))],
        };

        let cont_body = local_var("x", int_type.clone());
        let cont =
            make_inline_bind_continuation("x", int_type.clone(), cont_body, int_type.clone());

        let bind_call = make_typed_bind_call(source, cont, int_type.clone());

        let prop = typed_expr_to_transition_prop(
            &bind_call,
            "test_mod",
            &function_index,
            &constant_index,
            &local_values,
            &empty_data_types,
            &mut visiting,
            &BTreeSet::new(),
            &mut BTreeSet::new(),
        );

        let body = match prop {
            TransitionProp::Exists { binder, body, .. } => {
                assert_eq!(binder, "x");
                *body
            }
            other => panic!("expected Exists {{ binder: x, .. }}, got {other:?}"),
        };

        match body {
            TransitionProp::And(parts) => {
                assert_eq!(parts.len(), 2);
                match &parts[0] {
                    TransitionProp::Unsupported { reason, .. } => {
                        assert!(
                            reason.contains("self-referential monadic-bind binder"),
                            "expected nested-self-reference to also fire the \
                             self-referential guard; got: {reason}",
                        );
                        assert!(reason.contains("'x'"));
                    }
                    other => panic!("expected first part Unsupported; got {other:?}"),
                }
            }
            other => panic!("expected body And([Unsupported, body_prop]); got {other:?}"),
        }
    }

    /// Lookup-side visited-set guard — direct self-shadow regression.
    ///
    /// Aiken's `aiken check` accepts the legal pattern
    /// `let x = 1; let x = x; x`. The leading-`let` collector at the
    /// top of `typed_expr_to_transition_prop` walks the Sequence's
    /// non-terminal Assignments and overwrites
    /// `local_values["x"]` with `Var "x"` (the RHS of the second
    /// `let x = x`). Pre-fix, the `Var` arm at lookup time would
    /// recurse `Var "x" → local_values["x"] = Var "x" → Var "x" …`
    /// indefinitely, stack-overflowing the lowering and SIGABRTing
    /// `cargo test`.
    ///
    /// Post-fix, the `visiting_value_aliases` set inserted `"x"`
    /// before the recursive lookup and detects the cycle on the
    /// second visit, returning `TransitionProp::Unsupported` with the
    /// stable substring `local-alias cycle on 'x'`.
    #[test]
    fn var_lookup_breaks_direct_self_shadow_cycle() {
        let (function_index, constant_index, local_values, empty_data_types, mut visiting) =
            empty_transition_prop_context();

        let int_type = Type::int();

        // First assignment: `let x = 42` (non-Fuzzer Int — collected).
        let first_assignment = TypedExpr::Assignment {
            location: Span::empty(),
            tipo: int_type.clone(),
            value: Box::new(uint_lit("42")),
            pattern: TypedPattern::var("x"),
            kind: crate::ast::AssignmentKind::Let { backpassing: () },
            comment: None,
        };

        // Second assignment: `let x = x` — RHS is `Var "x"`. After
        // collection, `local_values["x"] = Var "x"` (overwriting the
        // first binding). This is the cyclic entry.
        let second_assignment = TypedExpr::Assignment {
            location: Span::empty(),
            tipo: int_type.clone(),
            value: Box::new(local_var("x", int_type.clone())),
            pattern: TypedPattern::var("x"),
            kind: crate::ast::AssignmentKind::Let { backpassing: () },
            comment: None,
        };

        // Terminal: bare `Var "x"`. Goes through the `TypedExpr::Var`
        // arm, which looks up `local_values["x"] = Var "x"` and would
        // recurse forever pre-fix.
        let terminal = local_var("x", int_type.clone());

        let sequence = TypedExpr::Sequence {
            location: Span::empty(),
            expressions: vec![first_assignment, second_assignment, terminal],
        };

        let prop = typed_expr_to_transition_prop(
            &sequence,
            "test_mod",
            &function_index,
            &constant_index,
            &local_values,
            &empty_data_types,
            &mut visiting,
            &BTreeSet::new(),
            &mut BTreeSet::new(),
        );

        match prop {
            TransitionProp::Unsupported { reason, .. } => {
                assert!(
                    reason.contains("local-alias cycle on 'x'"),
                    "expected reason to contain stable substring \
                     \"local-alias cycle on 'x'\"; got: {reason}",
                );
            }
            other => panic!("expected Unsupported(local-alias cycle on 'x'); got {other:?}"),
        }
    }

    /// Lookup-side visited-set guard — mutual-shadow chain regression.
    ///
    /// Legal Aiken: `let x = 0; let y = x; let x = y; x`. The
    /// leading-let collector installs:
    ///   - `local_values["x"] = 0`
    ///   - `local_values["y"] = Var "x"`
    ///   - `local_values["x"] = Var "y"` (overwrites `0`)
    ///     Terminal `Var "x"` looks up `Var "y"` → looks up `Var "x"` → …
    ///     Pre-fix: SIGABRT. Post-fix: visited-set guard catches the
    ///     second visit to `"x"` and returns the cycle marker.
    #[test]
    fn var_lookup_breaks_mutual_shadow_cycle() {
        let (function_index, constant_index, local_values, empty_data_types, mut visiting) =
            empty_transition_prop_context();

        let int_type = Type::int();

        // `let x = 0` — non-Fuzzer Int, collected.
        let assign_x0 = TypedExpr::Assignment {
            location: Span::empty(),
            tipo: int_type.clone(),
            value: Box::new(uint_lit("0")),
            pattern: TypedPattern::var("x"),
            kind: crate::ast::AssignmentKind::Let { backpassing: () },
            comment: None,
        };

        // `let y = x` — RHS is `Var "x"`. After collection,
        // `local_values["y"] = Var "x"`.
        let assign_y_eq_x = TypedExpr::Assignment {
            location: Span::empty(),
            tipo: int_type.clone(),
            value: Box::new(local_var("x", int_type.clone())),
            pattern: TypedPattern::var("y"),
            kind: crate::ast::AssignmentKind::Let { backpassing: () },
            comment: None,
        };

        // `let x = y` — RHS is `Var "y"`. After collection,
        // `local_values["x"] = Var "y"` (OVERWRITES the prior
        // `local_values["x"] = 0`). This is the cycle-completing
        // step: now `x → y → x → …`.
        let assign_x_eq_y = TypedExpr::Assignment {
            location: Span::empty(),
            tipo: int_type.clone(),
            value: Box::new(local_var("y", int_type.clone())),
            pattern: TypedPattern::var("x"),
            kind: crate::ast::AssignmentKind::Let { backpassing: () },
            comment: None,
        };

        // Terminal: `Var "x"`. Lookup recursion would loop forever
        // pre-fix.
        let terminal = local_var("x", int_type.clone());

        let sequence = TypedExpr::Sequence {
            location: Span::empty(),
            expressions: vec![assign_x0, assign_y_eq_x, assign_x_eq_y, terminal],
        };

        let prop = typed_expr_to_transition_prop(
            &sequence,
            "test_mod",
            &function_index,
            &constant_index,
            &local_values,
            &empty_data_types,
            &mut visiting,
            &BTreeSet::new(),
            &mut BTreeSet::new(),
        );

        // The visited-set guard catches the second visit to whichever
        // name is re-entered first. Because we start the lookup at
        // `Var "x"` (terminal), the recursion order is:
        //   visit "x" → resolve to Var "y"
        //     visit "y" → resolve to Var "x"
        //       visit "x" → ALREADY visited → emit cycle marker
        // So the guard fires on `"x"`. (If the terminal were `Var "y"`,
        // the guard would fire on `"y"`. The marker phrase reports
        // whichever name closes the loop.)
        match prop {
            TransitionProp::Unsupported { reason, .. } => {
                assert!(
                    reason.contains("local-alias cycle on 'x'")
                        || reason.contains("local-alias cycle on 'y'"),
                    "expected reason to contain stable substring \
                     \"local-alias cycle on 'x'\" or \"local-alias cycle on 'y'\" \
                     (whichever name closes the recursion first); got: {reason}",
                );
            }
            other => panic!("expected Unsupported(local-alias cycle on 'x' or 'y'); got {other:?}"),
        }
    }

    /// Lookup-side visited-set guard — negative control. A legal
    /// non-cyclic alias chain `let x = 1; let y = x; y` MUST resolve
    /// to whatever the terminal lowering produces, NOT a spurious
    /// cycle marker. After the leading-let collector installs
    /// `local_values["x"] = UInt 1`, `local_values["y"] = Var "x"`,
    /// the terminal `Var "y"` resolves through `Var "x"` to `UInt 1`.
    /// `UInt` is not a recognized transition shape so the result is
    /// `Unsupported` — but the reason text MUST mention the
    /// non-recognized shape, NOT the cycle phrase.
    #[test]
    fn var_lookup_does_not_falsely_trip_on_distinct_names() {
        let (function_index, constant_index, local_values, empty_data_types, mut visiting) =
            empty_transition_prop_context();

        let int_type = Type::int();

        // `let x = 1`
        let assign_x = TypedExpr::Assignment {
            location: Span::empty(),
            tipo: int_type.clone(),
            value: Box::new(uint_lit("1")),
            pattern: TypedPattern::var("x"),
            kind: crate::ast::AssignmentKind::Let { backpassing: () },
            comment: None,
        };

        // `let y = x` — RHS = Var "x".
        let assign_y = TypedExpr::Assignment {
            location: Span::empty(),
            tipo: int_type.clone(),
            value: Box::new(local_var("x", int_type.clone())),
            pattern: TypedPattern::var("y"),
            kind: crate::ast::AssignmentKind::Let { backpassing: () },
            comment: None,
        };

        // Terminal: `Var "y"`. Recursion: visit "y" → Var "x" →
        // visit "x" → UInt 1 (terminal, no further lookup).
        let terminal = local_var("y", int_type.clone());

        let sequence = TypedExpr::Sequence {
            location: Span::empty(),
            expressions: vec![assign_x, assign_y, terminal],
        };

        let prop = typed_expr_to_transition_prop(
            &sequence,
            "test_mod",
            &function_index,
            &constant_index,
            &local_values,
            &empty_data_types,
            &mut visiting,
            &BTreeSet::new(),
            &mut BTreeSet::new(),
        );

        // The result must NOT be the cycle marker. `UInt 1` falls
        // through to the catch-all Unsupported arm with phrase
        // "is not a recognized transition shape". The visited-set
        // guard MUST NOT misfire here: the alias chain x→y is
        // strictly forward and bottoms out at a non-Var leaf.
        match prop {
            TransitionProp::Unsupported { reason, .. } => {
                assert!(
                    !reason.contains("local-alias cycle"),
                    "FALSE POSITIVE: visited-set guard misfired on a strictly \
                     forward alias chain. The guard MUST only fire when the \
                     SAME name is re-entered. got reason: {reason}",
                );
                assert!(
                    reason.contains("is not a recognized transition shape"),
                    "expected the catch-all Unsupported (UInt 1 is not a \
                     transition shape); got: {reason}",
                );
            }
            other => panic!(
                "expected Unsupported(catch-all for UInt 1) — \
                 the alias chain should resolve cleanly to the integer literal; \
                 got {other:?}"
            ),
        }
    }

    #[test]
    fn transition_prop_is_trivially_unsupported_is_deep() {
        // All-Unsupported trees should be classified trivial; a tree with
        // any `EqOutput` leaf should not.
        let unsupp = || TransitionProp::Unsupported {
            reason: "leaf".to_string(),
            source_location: None,
        };

        // Or([Unsupported, Unsupported]) → trivial
        let all_unsupp_or = TransitionProp::Or(vec![unsupp(), unsupp()]);
        assert!(transition_prop_is_trivially_unsupported(&all_unsupp_or));

        // IfThenElse with at least one EqOutput leaf → not trivial
        let mixed = TransitionProp::IfThenElse {
            cond: ShallowIr::Const(ShallowConst::Bool(true)),
            t: Box::new(unsupp()),
            e: Box::new(TransitionProp::EqOutput(ShallowIr::Const(
                ShallowConst::Int("0".to_string()),
            ))),
        };
        assert!(!transition_prop_is_trivially_unsupported(&mixed));

        // Exists whose body is Unsupported → trivial (no extractable
        // content beyond the domain, which is a SOUND over-approximation
        // that a caller would otherwise represent as a bare `Unsupported`).
        let exists_empty = TransitionProp::Exists {
            binder: "x".to_string(),
            ty: ShallowIrType::Int,
            domain: Box::new(FuzzerSemantics::IntRange {
                min: None,
                max: None,
            }),
            body: Box::new(unsupp()),
        };
        assert!(transition_prop_is_trivially_unsupported(&exists_empty));
    }

    // ---------------------------------------------------------------
    // Constructor-tag resolution regression: prior to threading
    // `data_types` into `typed_expr_to_shallow_ir`, every `Construct`
    // node was emitted with `tag: 0`, so non-first constructors (e.g.
    // `Scenario.Step` at index 1) produced the wrong structural
    // equality in generated Lean predicates.
    // ---------------------------------------------------------------

    /// Build a `Scenario`-like data type with constructors:
    ///   index 0: Done           (zero-arity)
    ///   index 1: Step(Data)     (single-field)
    ///
    /// Returns the owned data-types map alongside the `Type::App` for
    /// `mod/Scenario`.
    fn scenario_like_data_types() -> (IndexMap<DataTypeKey, TypedDataType>, Rc<Type>) {
        let scenario_ty = Rc::new(Type::App {
            public: true,
            contains_opaque: false,
            module: "mod".to_string(),
            name: "Scenario".to_string(),
            args: vec![],
            alias: None,
        });

        let scenario_data_type = TypedDataType {
            decorators: vec![],
            constructors: vec![
                RecordConstructor {
                    decorators: vec![],
                    location: Span::empty(),
                    name: "Done".to_string(),
                    arguments: vec![],
                    doc: None,
                    sugar: false,
                },
                RecordConstructor {
                    decorators: vec![],
                    location: Span::empty(),
                    name: "Step".to_string(),
                    arguments: vec![RecordConstructorArg {
                        label: Some("payload".to_string()),
                        annotation: Annotation::Constructor {
                            location: Span::empty(),
                            module: None,
                            name: "Data".to_string(),
                            arguments: vec![],
                        },
                        location: Span::empty(),
                        tipo: Type::data(),
                        doc: None,
                    }],
                    doc: None,
                    sugar: false,
                },
            ],
            doc: None,
            location: Span::empty(),
            name: "Scenario".to_string(),
            opaque: false,
            parameters: vec![],
            public: true,
            typed_parameters: vec![],
        };

        let mut data_types = IndexMap::new();
        data_types.insert(
            DataTypeKey {
                module_name: "mod".to_string(),
                defined_type: "Scenario".to_string(),
            },
            scenario_data_type,
        );

        (data_types, scenario_ty)
    }

    #[test]
    fn resolve_constructor_tag_uses_declaration_order() {
        let (owned, scenario_ty) = scenario_like_data_types();
        let data_types: IndexMap<&DataTypeKey, &TypedDataType> = owned.iter().collect();

        // First constructor → Some(0).
        assert_eq!(
            resolve_constructor_tag(&scenario_ty, "Done", &data_types),
            Some(0)
        );

        // Second constructor → Some(1) (the bug emitted 0 here).
        assert_eq!(
            resolve_constructor_tag(&scenario_ty, "Step", &data_types),
            Some(1)
        );

        // Unknown constructor on a known type → None (was: silent fallback
        // to 0). Callers must route through `ShallowIr::Opaque` with the
        // S0002 marker so the verify pipeline raises a hard
        // `ConstructorTagUnresolved` error rather than emit a vacuously-
        // satisfiable equality.
        assert_eq!(
            resolve_constructor_tag(&scenario_ty, "DoesNotExist", &data_types),
            None
        );

        // Type not in the registry → None (was: silent fallback to 0).
        let unknown_ty = Rc::new(Type::App {
            public: true,
            contains_opaque: false,
            module: "other".to_string(),
            name: "Other".to_string(),
            args: vec![],
            alias: None,
        });
        assert_eq!(
            resolve_constructor_tag(&unknown_ty, "Step", &data_types),
            None,
        );
    }

    #[test]
    fn resolve_constructor_tag_walks_through_function_return_type() {
        // For an n-ary constructor, the `tipo` carried by
        // `ValueConstructorVariant::Record` (and `ModuleValueConstructor::
        // Record`) is `Type::Fn { args: [...field types], ret: ADT }`.
        // `resolve_constructor_tag` must walk through `Type::Fn::ret` so
        // n-ary constructor sites resolve to the correct tag, not 0.
        let (owned, scenario_ty) = scenario_like_data_types();
        let data_types: IndexMap<&DataTypeKey, &TypedDataType> = owned.iter().collect();

        // `Step(Data) -> Scenario` is the ctor's curried function type.
        let step_fn_ty = Type::function(vec![Type::data()], scenario_ty);

        assert_eq!(
            resolve_constructor_tag(&step_fn_ty, "Step", &data_types),
            Some(1),
            "n-ary constructor lookup must walk Type::Fn ret",
        );
    }

    #[test]
    fn typed_expr_to_shallow_ir_emits_correct_tag_for_step_constructor() {
        // Regression for the constructor-tag bug: a `Call(Step, [Data])`
        // expression must produce `ShallowIr::Construct { tag: 1, .. }`,
        // not `tag: 0`, because `Step` is the second constructor in the
        // `Scenario` ADT.
        let (owned, scenario_ty) = scenario_like_data_types();
        let data_types: IndexMap<&DataTypeKey, &TypedDataType> = owned.iter().collect();

        // Build `Step(payload)` as a `TypedExpr::Call` with a
        // `ValueConstructorVariant::Record` callee.
        let step_fn_ty = Type::function(vec![Type::data()], scenario_ty.clone());
        let step_ctor = TypedExpr::Var {
            location: Span::empty(),
            constructor: ValueConstructor::public(
                step_fn_ty,
                ValueConstructorVariant::Record {
                    name: "Step".to_string(),
                    arity: 1,
                    field_map: None,
                    location: Span::empty(),
                    module: "mod".to_string(),
                    constructors_count: 2,
                },
            ),
            name: "Step".to_string(),
        };
        let payload = TypedExpr::ByteArray {
            location: Span::empty(),
            tipo: Type::data(),
            bytes: vec![0xab, 0xcd],
            preferred_format: crate::ast::ByteArrayFormatPreference::HexadecimalString,
        };
        let step_call = TypedExpr::Call {
            location: Span::empty(),
            tipo: scenario_ty.clone(),
            fun: Box::new(step_ctor),
            args: vec![call_arg(payload)],
        };

        let ir = typed_expr_to_shallow_ir(&step_call, &data_types);
        match ir {
            ShallowIr::Construct {
                ref constructor,
                tag,
                ref fields,
                ..
            } => {
                assert_eq!(constructor, "Step");
                assert_eq!(tag, 1, "Step is the second constructor (index 1)");
                assert_eq!(fields.len(), 1);
            }
            other => panic!("expected ShallowIr::Construct, got {other:?}"),
        }

        // Done is zero-arity → `TypedExpr::Var` path. Tag must be 0.
        let done_var = TypedExpr::Var {
            location: Span::empty(),
            constructor: ValueConstructor::public(
                scenario_ty,
                ValueConstructorVariant::Record {
                    name: "Done".to_string(),
                    arity: 0,
                    field_map: None,
                    location: Span::empty(),
                    module: "mod".to_string(),
                    constructors_count: 2,
                },
            ),
            name: "Done".to_string(),
        };
        let ir = typed_expr_to_shallow_ir(&done_var, &data_types);
        match ir {
            ShallowIr::Construct {
                ref constructor,
                tag,
                ref fields,
                ..
            } => {
                assert_eq!(constructor, "Done");
                assert_eq!(tag, 0, "Done is the first constructor (index 0)");
                assert!(fields.is_empty());
            }
            other => panic!("expected ShallowIr::Construct, got {other:?}"),
        }
    }

    // ---------------------------------------------------------------
    // S0002 (`ConstructorTagUnresolved`) regression tests:
    // when `resolve_constructor_tag` returns `None`, every call site
    // must route the affected node through `ShallowIr::Opaque` carrying
    // a typed `OpaqueCode::ConstructorTagUnresolved` payload. This is
    // the contract that the verify-side dispatcher in
    // `aiken-project/src/verify.rs` consumes (via
    // `find_first_typed_opaque_in_shallow_ir`) to emit a hard,
    // non-skippable `S0002` error rather than a silent tag-0 fallback.
    //
    // Commit 18 retired the legacy `S0002_REASON_PREFIX` string-prefix
    // sniffing in favour of these typed assertions.
    // ---------------------------------------------------------------

    /// Helper: assert that `code` is the expected S0002 typed payload,
    /// independent of the (cosmetic) `reason` diagnostic string.
    fn assert_s0002_code(code: &Option<OpaqueCode>, expected_ctor: &str, expected_type: &str) {
        match code {
            Some(OpaqueCode::ConstructorTagUnresolved { ctor, type_name }) => {
                assert_eq!(
                    ctor, expected_ctor,
                    "ConstructorTagUnresolved.ctor mismatch"
                );
                assert_eq!(
                    type_name, expected_type,
                    "ConstructorTagUnresolved.type_name mismatch"
                );
            }
            other => panic!(
                "expected Some(OpaqueCode::ConstructorTagUnresolved {{ ctor: {expected_ctor:?}, \
                 type_name: {expected_type:?} }}); got {other:?}"
            ),
        }
    }

    /// Construct site (Var, zero-arity constructor `Done` referencing a
    /// `Scenario`-shaped type whose data-types map is empty): the
    /// `TypedExpr::Var` arm in `typed_expr_to_shallow_ir` must collapse
    /// to `Opaque` carrying the typed `ConstructorTagUnresolved` code
    /// rather than emit `Construct { tag: 0, .. }`.
    #[test]
    fn construct_site_unknown_constructor_emits_opaque_with_s0002() {
        // Empty registry — every constructor is unknown.
        let owned: IndexMap<DataTypeKey, TypedDataType> = IndexMap::new();
        let data_types: IndexMap<&DataTypeKey, &TypedDataType> = owned.iter().collect();

        let scenario_ty = Rc::new(Type::App {
            public: true,
            contains_opaque: false,
            module: "ghost".to_string(),
            name: "Phantom".to_string(),
            args: vec![],
            alias: None,
        });

        let var_expr = TypedExpr::Var {
            location: Span::empty(),
            constructor: ValueConstructor::public(
                scenario_ty,
                ValueConstructorVariant::Record {
                    name: "Ghost".to_string(),
                    arity: 0,
                    field_map: None,
                    location: Span::empty(),
                    module: "ghost".to_string(),
                    constructors_count: 1,
                },
            ),
            name: "Ghost".to_string(),
        };

        let ir = typed_expr_to_shallow_ir(&var_expr, &data_types);
        match ir {
            ShallowIr::Opaque { reason, code, .. } => {
                assert_s0002_code(&code, "Ghost", "ghost.Phantom");
                // The diagnostic message still mentions the relevant fields.
                assert!(reason.contains("Ghost"));
                assert!(reason.contains("ghost.Phantom"));
            }
            other => {
                panic!("expected ShallowIr::Opaque (constructor not in registry), got {other:?}")
            }
        }
    }

    /// Module-select site (qualified zero-arity constructor `M.Ghost`
    /// referenced via `TypedExpr::ModuleSelect`): same contract — the
    /// arm must collapse to `Opaque` carrying the typed S0002 code.
    #[test]
    fn module_select_unknown_constructor_emits_opaque_with_s0002() {
        let owned: IndexMap<DataTypeKey, TypedDataType> = IndexMap::new();
        let data_types: IndexMap<&DataTypeKey, &TypedDataType> = owned.iter().collect();

        let scenario_ty = Rc::new(Type::App {
            public: true,
            contains_opaque: false,
            module: "ghost".to_string(),
            name: "Phantom".to_string(),
            args: vec![],
            alias: None,
        });

        let mod_select = TypedExpr::ModuleSelect {
            location: Span::empty(),
            tipo: scenario_ty.clone(),
            label: "Ghost".to_string(),
            module_name: "ghost".to_string(),
            module_alias: "M".to_string(),
            constructor: ModuleValueConstructor::Record {
                name: "Ghost".to_string(),
                arity: 0,
                tipo: scenario_ty,
                field_map: None,
                location: Span::empty(),
            },
        };

        let ir = typed_expr_to_shallow_ir(&mod_select, &data_types);
        match ir {
            ShallowIr::Opaque { reason, code, .. } => {
                assert_s0002_code(&code, "Ghost", "ghost.Phantom");
                assert!(reason.contains("Ghost"));
                assert!(reason.contains("ghost.Phantom"));
            }
            other => {
                panic!("expected ShallowIr::Opaque (constructor not in registry), got {other:?}")
            }
        }
    }

    /// `translate_clause` directly: a constructor pattern referencing a
    /// constructor not in the registry must yield
    /// `Err(ClauseTranslationFailure { code: ConstructorTagUnresolved, .. })`,
    /// not silently produce a `ShallowIrArm` with `tag: Some(0)`.
    #[test]
    fn translate_clause_propagates_unknown_constructor_as_err() {
        let owned: IndexMap<DataTypeKey, TypedDataType> = IndexMap::new();
        let data_types: IndexMap<&DataTypeKey, &TypedDataType> = owned.iter().collect();

        let subject_ty = Rc::new(Type::App {
            public: true,
            contains_opaque: false,
            module: "ghost".to_string(),
            name: "Phantom".to_string(),
            args: vec![],
            alias: None,
        });

        let clause = TypedClause {
            location: Span::empty(),
            pattern: TypedPattern::constructor("Ghost", &[], subject_ty.clone(), Span::empty()),
            then: uint_lit("0"),
        };

        let result = translate_clause(&clause, &subject_ty, &data_types);
        match result {
            Err(failure) => {
                match failure.code {
                    OpaqueCode::ConstructorTagUnresolved { ctor, type_name } => {
                        assert_eq!(ctor, "Ghost");
                        assert_eq!(type_name, "ghost.Phantom");
                    }
                }
                assert!(failure.reason.contains("Ghost"));
                assert!(failure.reason.contains("ghost.Phantom"));
            }
            Ok(arm) => {
                panic!(
                    "expected Err(<S0002 typed code>) for unresolved Ghost ctor; got Ok({arm:?})"
                )
            }
        }
    }

    /// `When` arm dispatch: if a single `TypedClause` references an
    /// unresolved constructor, the entire `Match` collapses to a
    /// `ShallowIr::Opaque` carrying the typed S0002 code — no partial
    /// `Match` (which would carry an unsound tag-0 arm) is allowed to
    /// leak out.
    #[test]
    fn when_arm_unknown_constructor_bails_whole_match_to_opaque() {
        let owned: IndexMap<DataTypeKey, TypedDataType> = IndexMap::new();
        let data_types: IndexMap<&DataTypeKey, &TypedDataType> = owned.iter().collect();

        let subject_ty = Rc::new(Type::App {
            public: true,
            contains_opaque: false,
            module: "ghost".to_string(),
            name: "Phantom".to_string(),
            args: vec![],
            alias: None,
        });

        let when_expr = TypedExpr::When {
            location: Span::empty(),
            tipo: Type::int(),
            subject: Box::new(local_var("x", subject_ty.clone())),
            clauses: vec![TypedClause {
                location: Span::empty(),
                pattern: TypedPattern::constructor("Ghost", &[], subject_ty, Span::empty()),
                then: uint_lit("42"),
            }],
        };

        let ir = typed_expr_to_shallow_ir(&when_expr, &data_types);
        match ir {
            ShallowIr::Opaque { reason, code, .. } => {
                assert_s0002_code(&code, "Ghost", "ghost.Phantom");
                assert!(reason.contains("Ghost"));
                assert!(reason.contains("ghost.Phantom"));
            }
            other => panic!(
                "expected ShallowIr::Opaque (whole Match bails on unresolved arm), got {other:?}"
            ),
        }
    }

    /// `s0002_reason_message` produces a clean human-readable diagnostic
    /// (no machine-readable wire-format prefix). The typed payload —
    /// not the string — is what the verify-side dispatcher consumes.
    #[test]
    fn s0002_reason_message_is_human_readable() {
        let msg = s0002_reason_message("Ghost", "ghost.Phantom");
        assert!(
            !msg.starts_with("S0002:"),
            "post-commit-18: reason must NOT carry the legacy wire prefix; got {msg:?}"
        );
        assert!(msg.contains("constructor 'Ghost'"));
        assert!(msg.contains("type 'ghost.Phantom'"));
        assert!(msg.contains("(S0002)"));
    }

    /// `shallow_ir_is_vacuous` must classify an `Opaque` carrying a typed
    /// `OpaqueCode` as *non-vacuous* so the upstream filter in
    /// `state_machine_trace_from_test_arguments` does not swallow it
    /// before the verify-side dispatcher can emit a hard error.
    /// All other `Opaque` shapes must remain vacuous.
    #[test]
    fn shallow_ir_is_vacuous_carves_out_typed_opaque() {
        // Bare `Opaque` (no typed code): vacuous as before.
        let bare_opaque = ShallowIr::Opaque {
            ty: ShallowIrType::Data,
            reason: "translator did not recognise this shape".to_string(),
            code: None,
        };
        assert!(
            shallow_ir_is_vacuous(&bare_opaque),
            "bare Opaque must remain vacuous"
        );

        // Typed-code `Opaque`: carve-out returns false so the marker
        // can reach the verify-side dispatcher.
        let typed_opaque = ShallowIr::Opaque {
            ty: ShallowIrType::Data,
            reason: s0002_reason_message("Ghost", "ghost.Phantom"),
            code: Some(OpaqueCode::ConstructorTagUnresolved {
                ctor: "Ghost".to_string(),
                type_name: "ghost.Phantom".to_string(),
            }),
        };
        assert!(
            !shallow_ir_is_vacuous(&typed_opaque),
            "typed-code Opaque must NOT be vacuous (must reach the dispatcher)"
        );

        // Carve-out also fires when a typed-code Opaque is reached
        // through `Let` wrapping (the loop unwraps `Let` before classifying).
        let wrapped = ShallowIr::Let {
            name: "tmp".to_string(),
            value: Box::new(ShallowIr::Const(ShallowConst::Unit)),
            body: Box::new(typed_opaque.clone()),
        };
        assert!(
            !shallow_ir_is_vacuous(&wrapped),
            "Let wrapping a typed-code Opaque must remain non-vacuous"
        );
    }

    /// `find_first_typed_opaque_in_shallow_ir` must locate typed codes
    /// nested arbitrarily deep inside structural shapes — the verify-side
    /// dispatcher relies on this to dispatch S0002 even when the outer
    /// IR is `Construct { fields: [..., Opaque{S0002}, ...] }` (which is
    /// NOT classified as vacuous, so it can reach the dispatcher).
    #[test]
    fn find_first_typed_opaque_in_shallow_ir_walks_nested_construct() {
        let make_marker_ir = || ShallowIr::Opaque {
            ty: ShallowIrType::Data,
            reason: s0002_reason_message("Ghost", "ghost.Phantom"),
            code: Some(OpaqueCode::ConstructorTagUnresolved {
                ctor: "Ghost".to_string(),
                type_name: "ghost.Phantom".to_string(),
            }),
        };

        // Top-level marker: trivial case.
        let top_level = make_marker_ir();
        let found = find_first_typed_opaque_in_shallow_ir(&top_level);
        assert!(matches!(
            found,
            Some(OpaqueCode::ConstructorTagUnresolved { ctor, type_name })
                if ctor == "Ghost" && type_name == "ghost.Phantom"
        ));

        // Marker buried inside a Construct field: must still be found.
        let nested = ShallowIr::Construct {
            module: "test".to_string(),
            constructor: "Outer".to_string(),
            tag: 0,
            fields: vec![
                ShallowIr::Const(ShallowConst::Int("0".to_string())),
                make_marker_ir(),
            ],
        };
        let found = find_first_typed_opaque_in_shallow_ir(&nested);
        assert!(
            matches!(
                found,
                Some(OpaqueCode::ConstructorTagUnresolved { ctor, type_name })
                    if ctor == "Ghost" && type_name == "ghost.Phantom"
            ),
            "marker buried inside Construct field must be discovered; got {found:?}"
        );

        // Marker absent: returns None.
        let no_marker = ShallowIr::Construct {
            module: "test".to_string(),
            constructor: "Outer".to_string(),
            tag: 0,
            fields: vec![ShallowIr::Const(ShallowConst::Bool(true))],
        };
        assert_eq!(find_first_typed_opaque_in_shallow_ir(&no_marker), None);

        // Bare (non-typed) Opaque: returns None.
        let bare_opaque = ShallowIr::Opaque {
            ty: ShallowIrType::Data,
            reason: "unrelated reason".to_string(),
            code: None,
        };
        assert_eq!(find_first_typed_opaque_in_shallow_ir(&bare_opaque), None);
    }

    /// `find_first_typed_opaque_in_transition_prop` must walk the
    /// recursive `TransitionProp` shape and locate typed codes buried
    /// inside `EqOutput`, `Pure`, `And`, `Or`, `IfThenElse`, `Match`,
    /// and `Exists` arms. The verify-side dispatcher in
    /// `try_generate_two_phase_proof` relies on this to surface a hard
    /// `S0002` error even when the marker is several layers deep.
    #[test]
    fn find_first_typed_opaque_in_transition_prop_walks_recursive_shapes() {
        let make_marker_ir = || ShallowIr::Opaque {
            ty: ShallowIrType::Data,
            reason: s0002_reason_message("Ghost", "ghost.Phantom"),
            code: Some(OpaqueCode::ConstructorTagUnresolved {
                ctor: "Ghost".to_string(),
                type_name: "ghost.Phantom".to_string(),
            }),
        };

        let expected = OpaqueCode::ConstructorTagUnresolved {
            ctor: "Ghost".to_string(),
            type_name: "ghost.Phantom".to_string(),
        };

        // EqOutput at top level: trivial.
        let eq_top = TransitionProp::EqOutput(make_marker_ir());
        assert_eq!(
            find_first_typed_opaque_in_transition_prop(&eq_top),
            Some(expected.clone())
        );

        // EqOutput nested in And: walk descends.
        let and_eq = TransitionProp::And(vec![
            TransitionProp::Pure(ShallowIr::Const(ShallowConst::Bool(true))),
            TransitionProp::EqOutput(make_marker_ir()),
        ]);
        assert_eq!(
            find_first_typed_opaque_in_transition_prop(&and_eq),
            Some(expected.clone())
        );

        // EqOutput nested in Or → IfThenElse → EqOutput: walk descends.
        let nested = TransitionProp::Or(vec![TransitionProp::IfThenElse {
            cond: ShallowIr::Const(ShallowConst::Bool(true)),
            t: Box::new(TransitionProp::EqOutput(make_marker_ir())),
            e: Box::new(TransitionProp::EqOutput(ShallowIr::Const(
                ShallowConst::Int("0".to_string()),
            ))),
        }]);
        assert_eq!(
            find_first_typed_opaque_in_transition_prop(&nested),
            Some(expected.clone())
        );

        // Marker buried in Construct field, nested in EqOutput, in turn
        // nested in Exists: walk descends through every layer.
        let deep = TransitionProp::Exists {
            binder: "x".to_string(),
            ty: ShallowIrType::Data,
            domain: Box::new(FuzzerSemantics::Data),
            body: Box::new(TransitionProp::EqOutput(ShallowIr::Construct {
                module: "test".to_string(),
                constructor: "Wrapper".to_string(),
                tag: 0,
                fields: vec![make_marker_ir()],
            })),
        };
        assert_eq!(
            find_first_typed_opaque_in_transition_prop(&deep),
            Some(expected.clone())
        );

        // No marker present → None.
        let no_marker = TransitionProp::And(vec![
            TransitionProp::Pure(ShallowIr::Const(ShallowConst::Bool(true))),
            TransitionProp::EqOutput(ShallowIr::Const(ShallowConst::Int("42".to_string()))),
        ]);
        assert_eq!(find_first_typed_opaque_in_transition_prop(&no_marker), None);

        // SubGenerator and Unsupported leaves never carry markers.
        let sub_gen = TransitionProp::SubGenerator {
            module: "m".to_string(),
            fn_name: "f".to_string(),
        };
        assert_eq!(find_first_typed_opaque_in_transition_prop(&sub_gen), None);
        let unsupported = TransitionProp::Unsupported {
            reason: "test".to_string(),
            source_location: None,
        };
        assert_eq!(
            find_first_typed_opaque_in_transition_prop(&unsupported),
            None
        );
    }

    /// End-to-end: a synthetic step function whose body is a `Construct`
    /// referencing an unknown constructor must (a) flow through
    /// `typed_expr_to_shallow_ir` to produce an `Opaque` carrying the
    /// typed S0002 code, (b) survive the upstream `shallow_ir_is_vacuous`
    /// filter via the typed carve-out, and (c) be discoverable by the
    /// verify-side dispatcher via `find_first_typed_opaque_in_shallow_ir`.
    /// This is the production path that Oracle A flagged as broken before
    /// the carve-out: without it, the marker was swallowed by the filter
    /// and the user got a generic skippable `FallbackRequired` instead of
    /// the promised hard `S0002` error.
    #[test]
    fn s0002_marker_survives_shallow_ir_filter_pipeline() {
        // Empty data-type registry — every constructor is unknown.
        let owned: IndexMap<DataTypeKey, TypedDataType> = IndexMap::new();
        let data_types: IndexMap<&DataTypeKey, &TypedDataType> = owned.iter().collect();

        let phantom_ty = Rc::new(Type::App {
            public: true,
            contains_opaque: false,
            module: "ghost".to_string(),
            name: "Phantom".to_string(),
            args: vec![],
            alias: None,
        });

        // Step body: `Ghost` (a zero-arity constructor on `ghost.Phantom`,
        // which is NOT in the registry — `resolve_constructor_tag` returns
        // None and the translator must emit `Opaque{S0002}`).
        let body = TypedExpr::Var {
            location: Span::empty(),
            constructor: ValueConstructor::public(
                phantom_ty,
                ValueConstructorVariant::Record {
                    name: "Ghost".to_string(),
                    arity: 0,
                    field_map: None,
                    location: Span::empty(),
                    module: "ghost".to_string(),
                    constructors_count: 1,
                },
            ),
            name: "Ghost".to_string(),
        };

        // (a) `typed_expr_to_shallow_ir` produces typed `Opaque{S0002}`.
        let ir = typed_expr_to_shallow_ir(&body, &data_types);
        match &ir {
            ShallowIr::Opaque { code, .. } => {
                assert_s0002_code(code, "Ghost", "ghost.Phantom");
            }
            other => panic!("expected Opaque{{S0002}} from translator; got {other:?}"),
        }

        // (b) The filter in `state_machine_trace_from_test_arguments`
        // is `.filter(|ir| !shallow_ir_is_vacuous(ir))`.
        // Before the carve-out, this DROPPED the IR; after the carve-out,
        // the IR survives.
        assert!(
            !shallow_ir_is_vacuous(&ir),
            "typed-code Opaque must survive the upstream `shallow_ir_is_vacuous` filter; \
             this is the bug Oracle A flagged as Issue 1"
        );

        // (c) The verify-side dispatcher recovers the typed code — the
        // walker locates it whether at the top level or buried inside a
        // structural wrapper. Here the marker IS at the top level, but
        // confirming the walker finds it pins the contract.
        let found = find_first_typed_opaque_in_shallow_ir(&ir);
        assert!(
            matches!(
                found,
                Some(OpaqueCode::ConstructorTagUnresolved { ctor, type_name })
                    if ctor == "Ghost" && type_name == "ghost.Phantom"
            ),
            "verify-side dispatcher must find the S0002 typed code via the walker; got {found:?}"
        );
    }

    /// Structural-vacuity unit tests (M3).  Pin the verdict on every
    /// `TransitionProp` variant individually plus a few representative
    /// composites.  Together with the drift sentinel on the verify side,
    /// this gives us full coverage of the predicate.
    mod transition_prop_is_vacuous {
        use super::super::*;

        fn unsupported_leaf() -> TransitionProp {
            TransitionProp::Unsupported {
                reason: "test".to_string(),
                source_location: None,
            }
        }

        fn pure_true() -> TransitionProp {
            TransitionProp::Pure(ShallowIr::Const(ShallowConst::Bool(true)))
        }

        fn eq_output_var(name: &str) -> TransitionProp {
            TransitionProp::EqOutput(ShallowIr::Var {
                name: name.to_string(),
                ty: ShallowIrType::Data,
            })
        }

        fn eq_output_construct() -> TransitionProp {
            TransitionProp::EqOutput(ShallowIr::Construct {
                module: "m".to_string(),
                constructor: "Step".to_string(),
                tag: 1,
                fields: vec![],
            })
        }

        fn eq_output_typed_opaque() -> TransitionProp {
            TransitionProp::EqOutput(ShallowIr::Opaque {
                ty: ShallowIrType::Data,
                reason: s0002_reason_message("Ghost", "ghost.Phantom"),
                code: Some(OpaqueCode::ConstructorTagUnresolved {
                    ctor: "Ghost".to_string(),
                    type_name: "ghost.Phantom".to_string(),
                }),
            })
        }

        #[test]
        fn pure_is_vacuous_until_emitter_lowers_it_precisely() {
            // `Pure(_)` is currently widened to `True` by the Lean emitter
            // (S4 audit log).  The structural verdict mirrors the widening.
            assert!(transition_prop_is_vacuous(&pure_true()));
        }

        #[test]
        fn unsupported_is_vacuous() {
            assert!(transition_prop_is_vacuous(&unsupported_leaf()));
        }

        #[test]
        fn eq_output_with_vacuous_rhs_is_vacuous() {
            assert!(transition_prop_is_vacuous(&eq_output_var("transition")));
        }

        #[test]
        fn eq_output_with_structural_rhs_is_not_vacuous() {
            assert!(!transition_prop_is_vacuous(&eq_output_construct()));
        }

        #[test]
        fn eq_output_with_typed_opaque_rhs_preserves_s0002_marker() {
            assert!(!transition_prop_is_vacuous(&eq_output_typed_opaque()));
        }

        #[test]
        fn sub_generator_is_not_vacuous() {
            let sg = TransitionProp::SubGenerator {
                module: "m".to_string(),
                fn_name: "f".to_string(),
            };
            assert!(!transition_prop_is_vacuous(&sg));
        }

        #[test]
        fn empty_and_is_vacuous() {
            // The emitter renders `And([])` as literal `True`.
            assert!(transition_prop_is_vacuous(&TransitionProp::And(vec![])));
        }

        #[test]
        fn empty_or_is_vacuous() {
            // The emitter widens `Or([])` to literal `True` (logged) to
            // avoid an unsatisfiable precondition.
            assert!(transition_prop_is_vacuous(&TransitionProp::Or(vec![])));
        }

        #[test]
        fn empty_match_is_vacuous() {
            assert!(transition_prop_is_vacuous(&TransitionProp::Match {
                scrutinee: ShallowIr::Var {
                    name: "x".to_string(),
                    ty: ShallowIrType::Data,
                },
                arms: vec![],
            }));
        }

        #[test]
        fn and_of_unsupported_is_vacuous() {
            // Refinement over the text predicate (which gives up on `∧`).
            let prop = TransitionProp::And(vec![unsupported_leaf(), unsupported_leaf()]);
            assert!(transition_prop_is_vacuous(&prop));
        }

        #[test]
        fn and_with_one_real_constraint_is_not_vacuous() {
            let prop = TransitionProp::And(vec![unsupported_leaf(), eq_output_construct()]);
            assert!(!transition_prop_is_vacuous(&prop));
        }

        #[test]
        fn or_of_unsupported_is_vacuous() {
            let prop = TransitionProp::Or(vec![unsupported_leaf(), unsupported_leaf()]);
            assert!(transition_prop_is_vacuous(&prop));
        }

        #[test]
        fn or_with_one_real_constraint_is_not_vacuous() {
            let prop = TransitionProp::Or(vec![unsupported_leaf(), eq_output_construct()]);
            assert!(!transition_prop_is_vacuous(&prop));
        }

        #[test]
        fn exists_recurses_on_body_except_domain_constrained_bound_output() {
            let vacuous_body = TransitionProp::Exists {
                binder: "x".to_string(),
                ty: ShallowIrType::Data,
                domain: Box::new(FuzzerSemantics::Data),
                body: Box::new(unsupported_leaf()),
            };
            assert!(transition_prop_is_vacuous(&vacuous_body));

            let constrained_bound_output = TransitionProp::Exists {
                binder: "x".to_string(),
                ty: ShallowIrType::Data,
                domain: Box::new(FuzzerSemantics::Constructors { tags: vec![0, 1] }),
                body: Box::new(TransitionProp::EqOutput(ShallowIr::Var {
                    name: "x".to_string(),
                    ty: ShallowIrType::Data,
                })),
            };
            assert!(!transition_prop_is_vacuous(&constrained_bound_output));

            let unconstrained_bound_output = TransitionProp::Exists {
                binder: "x".to_string(),
                ty: ShallowIrType::Data,
                domain: Box::new(FuzzerSemantics::Data),
                body: Box::new(TransitionProp::EqOutput(ShallowIr::Var {
                    name: "x".to_string(),
                    ty: ShallowIrType::Data,
                })),
            };
            assert!(transition_prop_is_vacuous(&unconstrained_bound_output));

            let real_body = TransitionProp::Exists {
                binder: "x".to_string(),
                ty: ShallowIrType::Data,
                domain: Box::new(FuzzerSemantics::Data),
                body: Box::new(eq_output_construct()),
            };
            assert!(!transition_prop_is_vacuous(&real_body));
        }

        #[test]
        fn if_then_else_recurses_on_both_branches() {
            // Both branches vacuous — refinement over text predicate.
            let prop = TransitionProp::IfThenElse {
                cond: ShallowIr::Const(ShallowConst::Bool(true)),
                t: Box::new(unsupported_leaf()),
                e: Box::new(unsupported_leaf()),
            };
            assert!(transition_prop_is_vacuous(&prop));

            // One branch with a real constraint — not vacuous.
            let prop_partial = TransitionProp::IfThenElse {
                cond: ShallowIr::Const(ShallowConst::Bool(true)),
                t: Box::new(unsupported_leaf()),
                e: Box::new(eq_output_construct()),
            };
            assert!(!transition_prop_is_vacuous(&prop_partial));
        }

        #[test]
        fn match_recurses_on_arm_bodies() {
            let arms_all_vacuous = vec![
                TransitionPropArm {
                    tag: Some(0),
                    bindings: vec![],
                    body: unsupported_leaf(),
                },
                TransitionPropArm {
                    tag: Some(1),
                    bindings: vec![],
                    body: unsupported_leaf(),
                },
            ];
            let prop_vacuous = TransitionProp::Match {
                scrutinee: ShallowIr::Var {
                    name: "x".to_string(),
                    ty: ShallowIrType::Data,
                },
                arms: arms_all_vacuous,
            };
            assert!(transition_prop_is_vacuous(&prop_vacuous));

            let arms_mixed = vec![
                TransitionPropArm {
                    tag: Some(0),
                    bindings: vec![],
                    body: unsupported_leaf(),
                },
                TransitionPropArm {
                    tag: Some(1),
                    bindings: vec![],
                    body: eq_output_construct(),
                },
            ];
            let prop_real = TransitionProp::Match {
                scrutinee: ShallowIr::Var {
                    name: "x".to_string(),
                    ty: ShallowIrType::Data,
                },
                arms: arms_mixed,
            };
            assert!(!transition_prop_is_vacuous(&prop_real));
        }
    }
}
