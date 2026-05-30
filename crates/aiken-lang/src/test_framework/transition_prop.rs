use super::analysis_support::*;
use super::fuzzer_analysis::*;
use super::shallow_ir::*;
use super::*;

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
pub(super) enum ProjectionPathItem {
    ConstructorField {
        index: u64,
        label: String,
        ty: ShallowIrType,
    },
    ListElement {
        index: u64,
        label: String,
        ty: ShallowIrType,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub(super) enum LocalBinding {
    PureExpr(TypedExpr),
    ExactIr {
        ir: ShallowIr,
        ty: ShallowIrType,
    },
    DrawnValue {
        lean_name: String,
        ty: ShallowIrType,
        domain: FuzzerSemantics,
    },
    Projection {
        source_lean_name: String,
        source_ty: ShallowIrType,
        path: Vec<ProjectionPathItem>,
        ty: ShallowIrType,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum SubGeneratorBlocker {
    KnownPrimitiveCombinator,
    UnsupportedButInspectableShape,
    RecursiveHelper,
    ExternalOrUninspectableHelper,
    UserDefinedLookalikeCombinator,
}

pub(super) struct TransitionLoweringContext<'a> {
    pub(super) current_module: String,
    pub(super) function_index: &'a FunctionIndex<'a>,
    pub(super) constant_index: &'a ConstantIndex<'a>,
    pub(super) data_types: &'a IndexMap<&'a DataTypeKey, &'a TypedDataType>,
    pub(super) locals: BTreeMap<String, LocalBinding>,
    pub(super) visiting_functions: BTreeSet<(String, String)>,
    pub(super) visiting_locals: BTreeSet<String>,
    pub(super) visiting_value_aliases: BTreeSet<String>,
    pub(super) next_synthetic_binder: usize,
}

impl<'a> TransitionLoweringContext<'a> {
    fn expr_locals(&self) -> BTreeMap<String, TypedExpr> {
        self.locals
            .iter()
            .filter_map(|(name, binding)| match binding {
                LocalBinding::PureExpr(expr) => Some((name.clone(), expr.clone())),
                LocalBinding::ExactIr { .. }
                | LocalBinding::DrawnValue { .. }
                | LocalBinding::Projection { .. } => None,
            })
            .collect()
    }

    fn fresh_synthetic_binder(&mut self, prefix: &str) -> String {
        let name = format!("{prefix}_{}", self.next_synthetic_binder);
        self.next_synthetic_binder += 1;
        name
    }
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
pub(super) fn transition_prop_from_step_function(
    step_function: &TypedExpr,
    initial_state: Option<&TypedExpr>,
    function_index: &FunctionIndex<'_>,
    constant_index: &ConstantIndex<'_>,
    data_types: &IndexMap<&DataTypeKey, &TypedDataType>,
    visiting_functions: &mut BTreeSet<(String, String)>,
) -> Option<TransitionProp> {
    // Resolve `step` to its body when it's a module-level function reference,
    // mirroring the `step_function_ir` path above. For inline `TypedExpr::Fn`
    // step functions we descend into the body directly.
    let (body_expr, current_module, step_arguments): (&TypedExpr, String, &[TypedArg]) =
        match step_function {
            TypedExpr::Var { constructor, .. } => {
                if let ValueConstructorVariant::ModuleFn { module, name, .. } = &constructor.variant
                {
                    let key = (module.clone(), name.clone());
                    if visiting_functions.contains(&key) {
                        return None;
                    }
                    match find_function(function_index, module, name) {
                        Some(function) => (
                            &function.body,
                            module.clone(),
                            function.arguments.as_slice(),
                        ),
                        None => return None,
                    }
                } else {
                    return None;
                }
            }
            TypedExpr::Fn { args, body, .. } => (body.as_ref(), String::new(), args.as_slice()),
            _ => return None,
        };

    if step_arguments.len() > 1 {
        return None;
    }

    let mut locals: BTreeMap<String, LocalBinding> = BTreeMap::new();
    if let Some(state_arg) = step_arguments.first() {
        let binding = if let Some(expr) = initial_state {
            let ir = typed_expr_to_shallow_ir(expr, data_types);
            let ir = if shallow_ir_is_vacuous(&ir) {
                let empty_locals: BTreeMap<String, TypedExpr> = BTreeMap::new();
                resolve_function_with_applied_args(expr, "", function_index, &empty_locals)
                    .map(|(resolved, resolved_locals, _)| {
                        let resolved_locals = resolved_locals
                            .into_iter()
                            .map(|(name, expr)| (name, LocalBinding::PureExpr(expr)))
                            .collect();
                        let mut visiting = BTreeSet::new();
                        typed_expr_to_shallow_ir_with_locals(
                            &resolved.function.body,
                            data_types,
                            &resolved_locals,
                            &mut visiting,
                        )
                    })
                    .unwrap_or(ir)
            } else {
                ir
            };

            if shallow_ir_is_vacuous(&ir) {
                LocalBinding::PureExpr(expr.clone())
            } else {
                LocalBinding::ExactIr {
                    ty: shallow_ir_type(&state_arg.tipo),
                    ir,
                }
            }
        } else {
            LocalBinding::DrawnValue {
                lean_name: "state".to_string(),
                ty: shallow_ir_type(&state_arg.tipo),
                domain: FuzzerSemantics::Data,
            }
        };
        locals.insert(state_arg.get_name(), binding);
    }
    let visiting_locals: BTreeSet<String> = BTreeSet::new();
    let visiting_value_aliases: BTreeSet<String> = BTreeSet::new();

    let mut locals = locals;
    inject_module_constants(&mut locals, &current_module, constant_index);
    let mut ctx = TransitionLoweringContext {
        current_module: current_module.to_string(),
        function_index,
        constant_index,
        data_types,
        locals,
        visiting_functions: visiting_functions.clone(),
        visiting_locals: visiting_locals.clone(),
        visiting_value_aliases,
        next_synthetic_binder: 0,
    };
    let prop = typed_expr_to_transition_prop_ctx(body_expr, &mut ctx);
    *visiting_functions = ctx.visiting_functions;

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
        NormalizedFuzzer::Empty { reason, .. } => TransitionProp::Unsupported {
            reason: format!("fuzzer support is empty: {reason}"),
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
        NormalizedFuzzer::MapN { sources, .. } => TransitionProp::And(
            sources
                .iter()
                .enumerate()
                .map(|(index, source)| TransitionProp::Exists {
                    binder: format!("_map{index}"),
                    ty: ShallowIrType::Data,
                    domain: Box::new(fuzzer_semantics_of_normalized(source)),
                    body: Box::new(TransitionProp::Unsupported {
                        reason: "mapN mapper body is not yet lowered to a proposition".to_string(),
                        source_location: None,
                    }),
                })
                .collect(),
        ),
        NormalizedFuzzer::Product { .. } => TransitionProp::Unsupported {
            reason: "product-shaped fuzzer is not a valid step-function body".to_string(),
            source_location: None,
        },
        NormalizedFuzzer::List { .. } => TransitionProp::Unsupported {
            reason: "list-shaped fuzzer is not a valid step-function body".to_string(),
            source_location: None,
        },
        NormalizedFuzzer::Choice { branches, .. } => {
            if branches.is_empty() {
                TransitionProp::Unsupported {
                    reason: "choice-shaped fuzzer has no branches".to_string(),
                    source_location: None,
                }
            } else {
                TransitionProp::Or(
                    branches
                        .iter()
                        .map(normalized_fuzzer_to_transition_prop)
                        .collect(),
                )
            }
        }
        NormalizedFuzzer::Filter {
            source,
            predicate_summary,
            ..
        } => TransitionProp::Exists {
            binder: "_filter".to_string(),
            ty: ShallowIrType::Data,
            domain: Box::new(fuzzer_semantics_of_normalized(source)),
            body: Box::new(TransitionProp::Unsupported {
                reason: format!(
                    "such_that predicate is not yet lowered to a proposition: {predicate_summary}",
                ),
                source_location: None,
            }),
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
pub(super) fn fuzzer_semantics_of_normalized(normalized: &NormalizedFuzzer) -> FuzzerSemantics {
    match normalized {
        NormalizedFuzzer::Opaque { reason, .. } => FuzzerSemantics::Opaque {
            reason: reason.clone(),
        },
        NormalizedFuzzer::Empty { output_type, .. } => {
            if output_type.is_bool() {
                FuzzerSemantics::Bool
            } else if output_type.is_int() {
                FuzzerSemantics::IntRange {
                    min: None,
                    max: None,
                }
            } else if output_type.is_bytearray() {
                FuzzerSemantics::ByteArrayRange {
                    min_len: None,
                    max_len: None,
                }
            } else if output_type.is_string() {
                FuzzerSemantics::String
            } else {
                FuzzerSemantics::Data
            }
        }
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
        NormalizedFuzzer::Map { output_type, .. } => {
            if output_type.is_bool() {
                FuzzerSemantics::Bool
            } else if output_type.is_int() {
                FuzzerSemantics::IntRange {
                    min: None,
                    max: None,
                }
            } else if output_type.is_bytearray() {
                FuzzerSemantics::ByteArrayRange {
                    min_len: None,
                    max_len: None,
                }
            } else if output_type.is_string() {
                FuzzerSemantics::String
            } else if output_type.is_list() {
                FuzzerSemantics::List {
                    element: Box::new(FuzzerSemantics::Data),
                    min_len: None,
                    max_len: None,
                }
            } else if output_type.is_tuple() || output_type.is_pair() {
                FuzzerSemantics::Product(
                    output_type
                        .get_inner_types()
                        .iter()
                        .map(|_| FuzzerSemantics::Data)
                        .collect(),
                )
            } else {
                FuzzerSemantics::Data
            }
        }
        NormalizedFuzzer::MapN { output_type, .. } => {
            if output_type.is_bool() {
                FuzzerSemantics::Bool
            } else if output_type.is_int() {
                FuzzerSemantics::IntRange {
                    min: None,
                    max: None,
                }
            } else if output_type.is_bytearray() {
                FuzzerSemantics::ByteArrayRange {
                    min_len: None,
                    max_len: None,
                }
            } else if output_type.is_string() {
                FuzzerSemantics::String
            } else if output_type.is_list() {
                FuzzerSemantics::List {
                    element: Box::new(FuzzerSemantics::Data),
                    min_len: None,
                    max_len: None,
                }
            } else if output_type.is_tuple() || output_type.is_pair() {
                FuzzerSemantics::Product(
                    output_type
                        .get_inner_types()
                        .iter()
                        .map(|_| FuzzerSemantics::Data)
                        .collect(),
                )
            } else {
                FuzzerSemantics::Data
            }
        }
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
            ..
        } => FuzzerSemantics::List {
            element: Box::new(fuzzer_semantics_of_normalized(element)),
            min_len: *min_len,
            max_len: *max_len,
        },
        NormalizedFuzzer::Choice {
            output_type,
            branches,
            may_fail,
            non_empty_required,
        } => {
            if branches.is_empty() && *non_empty_required {
                FuzzerSemantics::Opaque {
                    reason: "choice combinator has no branches and therefore always fails"
                        .to_string(),
                }
            } else {
                let branch_semantics = branches
                    .iter()
                    .map(fuzzer_semantics_of_normalized)
                    .collect::<Vec<_>>();
                merge_choice_semantics_lightweight(output_type.as_ref(), &branch_semantics)
                    .unwrap_or_else(|| {
                        if *may_fail {
                            FuzzerSemantics::Opaque {
                                reason: "choice combinator may fail and the current lightweight semantics fallback is conservative".to_string(),
                            }
                        } else {
                            FuzzerSemantics::Data
                        }
                    })
            }
        }
        NormalizedFuzzer::Filter {
            source, impossible, ..
        } => {
            if *impossible {
                FuzzerSemantics::Opaque {
                    reason: "such_that predicate is impossible for all generated values"
                        .to_string(),
                }
            } else {
                fuzzer_semantics_of_normalized(source)
            }
        }
        NormalizedFuzzer::StateMachineTrace { .. } => FuzzerSemantics::Opaque {
            reason: "nested state-machine trace cannot appear as a sub-fuzzer domain".to_string(),
        },
    }
}

/// Lower a `FuzzerExactValue` to a `ShallowIr` constant for embedding in
/// `TransitionProp::EqOutput`.
pub(super) fn shallow_ir_from_fuzzer_exact_value(exact: &FuzzerExactValue) -> ShallowIr {
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
/// step in `verify::emit_transition_prop_as_lean` can still make several
/// variants vacuous or universally satisfiable:
///   * `Pure(_)`   — treated conservatively here until the structural vacuity
///     model learns the verifier's supported Bool subset precisely.
///   * `Unsupported { .. }` — widened by definition. The soundness rule is
///     that we never silently drop an unlowerable constraint without a
///     `True` over-approximation.
///   * Empty `And([])` / `Or([])` / `Match { arms: [] }` — emitted as
///     literal `True` (an empty conjunction is `True` and an empty
///     disjunction was deliberately widened to `True` in the emitter
///     to avoid an unsatisfiable precondition).

/// Mirroring those semantics conservatively here keeps the structural verdict
/// in step with what actually lands in the generated `.lean` file.
///
/// **Exhaustive match (no wildcard).**  Any future `TransitionProp`
/// variant must be classified explicitly.  Default-falsing a new variant
/// would silently widen it past the vacuity guard, which is the exact
/// soundness regression this predicate exists to prevent.
pub(super) fn exists_bound_output_domain_constrains_transition(
    binder: &str,
    domain: &FuzzerSemantics,
    body: &TransitionProp,
) -> bool {
    matches!(
        body,
        TransitionProp::EqOutput(ShallowIr::Var { name, .. })
            | TransitionProp::EqOutput(ShallowIr::BoundVar { name, .. })
            if name == binder
                && !matches!(domain, FuzzerSemantics::Data | FuzzerSemantics::Opaque { .. })
    )
}

pub fn transition_prop_is_vacuous(tp: &TransitionProp) -> bool {
    match tp {
        TransitionProp::Pure(ir) => !shallow_ir_is_exact_bool_expr(ir),
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
pub(super) fn transition_prop_is_trivially_unsupported(prop: &TransitionProp) -> bool {
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

pub(super) fn transition_prop_contains_unsupported(prop: &TransitionProp) -> bool {
    match prop {
        TransitionProp::Unsupported { .. } => true,
        TransitionProp::SubGenerator { .. }
        | TransitionProp::EqOutput(_)
        | TransitionProp::Pure(_) => false,
        TransitionProp::Exists { body, .. } => transition_prop_contains_unsupported(body),
        TransitionProp::And(children) | TransitionProp::Or(children) => {
            children.iter().any(transition_prop_contains_unsupported)
        }
        TransitionProp::IfThenElse { t, e, .. } => {
            transition_prop_contains_unsupported(t) || transition_prop_contains_unsupported(e)
        }
        TransitionProp::Match { arms, .. } => arms
            .iter()
            .any(|arm| transition_prop_contains_unsupported(&arm.body)),
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

pub(super) fn collect_sub_generators_inner(prop: &TransitionProp, acc: &mut Vec<(String, String)>) {
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
            | ShallowIr::BoundVar { .. }
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
            ShallowIr::RecordUpdate {
                record, updates, ..
            } => visit(record).or_else(|| {
                for update in updates {
                    if let Some(found) = visit(&update.value) {
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
                if let ShallowIr::ListLit {
                    tail: Some(tail), ..
                } = ir
                {
                    return visit(tail);
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
#[cfg(test)]
#[allow(clippy::too_many_arguments)]
pub(super) fn typed_expr_to_transition_prop(
    expr: &TypedExpr,
    current_module: &str,
    function_index: &FunctionIndex<'_>,
    constant_index: &ConstantIndex<'_>,
    local_values: &BTreeMap<String, TypedExpr>,
    data_types: &IndexMap<&DataTypeKey, &TypedDataType>,
    visiting_functions: &mut BTreeSet<(String, String)>,
    visiting_locals: &BTreeSet<String>,
    visiting_value_aliases: &mut BTreeSet<String>,
) -> TransitionProp {
    let mut locals: BTreeMap<String, LocalBinding> = local_values
        .iter()
        .map(|(name, expr)| (name.clone(), LocalBinding::PureExpr(expr.clone())))
        .collect();
    inject_module_constants(&mut locals, current_module, constant_index);
    let mut ctx = TransitionLoweringContext {
        current_module: current_module.to_string(),
        function_index,
        constant_index,
        data_types,
        locals,
        visiting_functions: visiting_functions.clone(),
        visiting_locals: visiting_locals.clone(),
        visiting_value_aliases: visiting_value_aliases.clone(),
        next_synthetic_binder: 0,
    };
    let prop = typed_expr_to_transition_prop_ctx(expr, &mut ctx);
    *visiting_functions = ctx.visiting_functions;
    *visiting_value_aliases = ctx.visiting_value_aliases;
    prop
}
pub(super) fn module_name_candidates(module_name: &str) -> Vec<String> {
    let mut candidates = vec![module_name.to_string()];
    let dotted = module_name.replace('/', ".");
    if !candidates.contains(&dotted) {
        candidates.push(dotted);
    }
    let slashed = module_name.replace('.', "/");
    if !candidates.contains(&slashed) {
        candidates.push(slashed);
    }
    candidates
}

pub(super) fn inject_module_constants(
    locals: &mut BTreeMap<String, LocalBinding>,
    current_module: &str,
    constant_index: &ConstantIndex<'_>,
) {
    for module_name in module_name_candidates(current_module) {
        if let Some(constants) = constant_index.get(&module_name) {
            for (name, expr) in constants {
                locals
                    .entry(name.clone())
                    .or_insert_with(|| LocalBinding::PureExpr((*expr).clone()));
            }
        }
    }
}

pub(super) fn with_scoped_locals<R>(
    ctx: &mut TransitionLoweringContext<'_>,
    locals: BTreeMap<String, LocalBinding>,
    f: impl FnOnce(&mut TransitionLoweringContext<'_>) -> R,
) -> R {
    let mut locals = locals;
    inject_module_constants(&mut locals, &ctx.current_module, ctx.constant_index);
    let saved = std::mem::replace(&mut ctx.locals, locals);
    let result = f(ctx);
    ctx.locals = saved;
    result
}

pub(super) fn with_current_module<R>(
    ctx: &mut TransitionLoweringContext<'_>,
    module: String,
    f: impl FnOnce(&mut TransitionLoweringContext<'_>) -> R,
) -> R {
    let saved_module = std::mem::replace(&mut ctx.current_module, module.clone());
    let saved_locals = ctx.locals.clone();
    inject_module_constants(&mut ctx.locals, &module, ctx.constant_index);
    let result = f(ctx);
    ctx.locals = saved_locals;
    ctx.current_module = saved_module;
    result
}

pub(super) fn binding_from_expr_with_locals(
    expr: &TypedExpr,
    locals: &BTreeMap<String, LocalBinding>,
    visiting: &mut BTreeSet<String>,
) -> LocalBinding {
    match terminal_expression(expr) {
        TypedExpr::Var { name, .. } if locals.contains_key(name) => {
            let Some(binding) = locals.get(name).cloned() else {
                return LocalBinding::PureExpr(expr.clone());
            };
            if !visiting.insert(name.clone()) {
                return LocalBinding::PureExpr(expr.clone());
            }
            let resolved = match binding {
                LocalBinding::PureExpr(bound_expr) => {
                    binding_from_expr_with_locals(&bound_expr, locals, visiting)
                }
                other => other,
            };
            visiting.remove(name);
            resolved
        }
        _ => LocalBinding::PureExpr(expr.clone()),
    }
}

pub(super) fn binding_from_expr(
    expr: &TypedExpr,
    ctx: &TransitionLoweringContext<'_>,
) -> LocalBinding {
    let mut visiting = BTreeSet::new();
    binding_from_expr_with_locals(expr, &ctx.locals, &mut visiting)
}

pub(super) fn extend_locals_with_leading_assignments(
    expr: &TypedExpr,
    ctx: &TransitionLoweringContext<'_>,
) -> Option<BTreeMap<String, LocalBinding>> {
    let expressions = match expr {
        TypedExpr::Sequence { expressions, .. } | TypedExpr::Pipeline { expressions, .. } => {
            expressions
        }
        _ => return None,
    };
    if expressions.len() <= 1 {
        return None;
    }
    let mut scoped = ctx.locals.clone();
    for e in expressions.iter().take(expressions.len().saturating_sub(1)) {
        if let TypedExpr::Assignment {
            pattern,
            value,
            kind,
            ..
        } = e
        {
            if !kind.is_let() {
                return None;
            }
            if extract_fuzzer_payload_type(value.tipo().as_ref()).is_some() {
                return None;
            }
            let mut visiting = BTreeSet::new();
            let binding = binding_from_expr_with_locals(value, &scoped, &mut visiting);
            bind_pattern_to_locals(pattern, &binding, ctx.data_types, &mut scoped);
        }
    }
    Some(scoped)
}

pub(super) fn transition_output_binding() -> LocalBinding {
    LocalBinding::DrawnValue {
        lean_name: "transition".to_string(),
        ty: ShallowIrType::Data,
        domain: FuzzerSemantics::Data,
    }
}

pub(super) fn binding_is_transition_output(binding: &LocalBinding) -> bool {
    matches!(
        binding,
        LocalBinding::DrawnValue { lean_name, ty, .. }
            if lean_name == "transition" && matches!(ty, ShallowIrType::Data)
    )
}

pub(super) fn shallow_ir_root_kind(ir: &ShallowIr) -> &'static str {
    match ir {
        ShallowIr::Const(_) => "Const",
        ShallowIr::Var { .. } => "Var",
        ShallowIr::BoundVar { .. } => "BoundVar",
        ShallowIr::Let { .. } => "Let",
        ShallowIr::If { .. } => "If",
        ShallowIr::Match { .. } => "Match",
        ShallowIr::Construct { .. } => "Construct",
        ShallowIr::FieldAccess { .. } => "FieldAccess",
        ShallowIr::RecordUpdate { .. } => "RecordUpdate",
        ShallowIr::BinOp { .. } => "BinOp",
        ShallowIr::Tuple(_) => "Tuple",
        ShallowIr::ListLit { .. } => "ListLit",
        ShallowIr::FuzzExistential { .. } => "FuzzExistential",
        ShallowIr::Opaque { .. } => "Opaque",
    }
}

pub(super) fn transition_prop_is_noop(prop: &TransitionProp) -> bool {
    matches!(prop, TransitionProp::And(parts) if parts.is_empty())
}

pub(super) fn transition_prop_from_parts(parts: Vec<TransitionProp>) -> TransitionProp {
    let mut kept = Vec::new();
    for part in parts {
        if !transition_prop_is_noop(&part) {
            kept.push(part);
        }
    }

    match kept.len() {
        0 => TransitionProp::And(vec![]),
        1 => kept.into_iter().next().expect("one kept element"),
        _ => TransitionProp::And(kept),
    }
}

pub(super) fn lower_relation_with_bound_domain(
    source: &TypedExpr,
    output_binding: &LocalBinding,
    ctx: &mut TransitionLoweringContext<'_>,
) -> TransitionProp {
    let relation = lower_fuzzer_relation_with_locals(source, output_binding, ctx);
    if !binding_is_transition_output(output_binding)
        && transition_prop_is_trivially_unsupported(&relation)
        && matches!(output_binding, LocalBinding::DrawnValue { domain, .. } if !matches!(domain, FuzzerSemantics::Data | FuzzerSemantics::Opaque { .. }))
    {
        TransitionProp::And(vec![])
    } else {
        relation
    }
}

pub(super) fn output_match_prop(
    output_binding: &LocalBinding,
    rhs: ShallowIr,
    ctx: &TransitionLoweringContext<'_>,
) -> TransitionProp {
    if binding_is_transition_output(output_binding) {
        if shallow_ir_is_exact_data_expr(&rhs) {
            TransitionProp::EqOutput(rhs)
        } else {
            TransitionProp::Unsupported {
                reason: format!(
                    "counted output equality rejected non-exact rhs root '{}'",
                    shallow_ir_root_kind(&rhs)
                ),
                source_location: None,
            }
        }
    } else {
        let lhs = output_binding_ir(output_binding, ctx);
        if shallow_ir_is_exact_data_expr(&lhs) && shallow_ir_is_exact_data_expr(&rhs) {
            equality_prop(lhs, rhs)
        } else {
            TransitionProp::Unsupported {
                reason: format!(
                    "counted output equality rejected non-exact operands (lhs='{}', rhs='{}')",
                    shallow_ir_root_kind(&lhs),
                    shallow_ir_root_kind(&rhs)
                ),
                source_location: None,
            }
        }
    }
}

pub(super) fn pattern_source_location(
    pattern: &TypedPattern,
    ctx: &TransitionLoweringContext<'_>,
) -> Option<String> {
    Some(format!(
        "{}:{}",
        ctx.current_module,
        pattern.location().start
    ))
}

pub(super) fn unsupported_when_pattern(
    pattern: &TypedPattern,
    ctx: &TransitionLoweringContext<'_>,
    reason: impl Into<String>,
) -> TransitionProp {
    TransitionProp::Unsupported {
        reason: reason.into(),
        source_location: pattern_source_location(pattern, ctx),
    }
}

pub(super) fn pattern_value_from_binding(
    pattern: &TypedPattern,
    binding: &LocalBinding,
    ctx: &TransitionLoweringContext<'_>,
) -> Result<ShallowIr, TransitionProp> {
    match pattern {
        TypedPattern::Var { .. } | TypedPattern::Discard { .. } => Ok(local_binding_to_shallow_ir(
            binding,
            ctx.data_types,
            &ctx.locals,
            &mut BTreeSet::new(),
        )),
        TypedPattern::Assign { pattern, .. } => pattern_value_from_binding(pattern, binding, ctx),
        TypedPattern::Int { value, .. } => Ok(ShallowIr::Const(ShallowConst::Int(value.clone()))),
        TypedPattern::ByteArray { value, .. } => Ok(ShallowIr::Const(ShallowConst::ByteArray(
            hex::encode(value),
        ))),
        TypedPattern::Pair { fst, snd, .. } => {
            let (fst_ty, snd_ty) = match binding_shallow_ir_type(binding) {
                ShallowIrType::Pair(fst_ty, snd_ty) => ((*fst_ty).clone(), (*snd_ty).clone()),
                _ => (ShallowIrType::Data, ShallowIrType::Data),
            };
            let Some(fst_binding) =
                projected_binding(binding, 0, "0", fst_ty, ShallowFieldAccessKind::ListElement)
            else {
                return Err(unsupported_when_pattern(
                    pattern,
                    ctx,
                    "pair-pattern subject cannot be projected faithfully",
                ));
            };
            let Some(snd_binding) =
                projected_binding(binding, 1, "1", snd_ty, ShallowFieldAccessKind::ListElement)
            else {
                return Err(unsupported_when_pattern(
                    pattern,
                    ctx,
                    "pair-pattern subject cannot be projected faithfully",
                ));
            };
            Ok(ShallowIr::Tuple(vec![
                pattern_value_from_binding(fst, &fst_binding, ctx)?,
                pattern_value_from_binding(snd, &snd_binding, ctx)?,
            ]))
        }
        TypedPattern::Tuple { elems, .. } => {
            let pair_types = match binding_shallow_ir_type(binding) {
                ShallowIrType::Pair(fst_ty, snd_ty) if elems.len() == 2 => {
                    Some([(*fst_ty).clone(), (*snd_ty).clone()])
                }
                _ => None,
            };
            let mut fields = Vec::with_capacity(elems.len());
            for (index, elem) in elems.iter().enumerate() {
                let ty = pair_types
                    .as_ref()
                    .and_then(|tys| tys.get(index))
                    .cloned()
                    .unwrap_or(ShallowIrType::Data);
                let Some(projected) = projected_binding(
                    binding,
                    index as u64,
                    index.to_string(),
                    ty,
                    ShallowFieldAccessKind::ListElement,
                ) else {
                    return Err(unsupported_when_pattern(
                        pattern,
                        ctx,
                        "tuple-pattern subject cannot be projected faithfully",
                    ));
                };
                fields.push(pattern_value_from_binding(elem, &projected, ctx)?);
            }
            Ok(ShallowIr::Tuple(fields))
        }
        TypedPattern::Constructor {
            name: ctor_name,
            arguments,
            tipo,
            ..
        } => {
            let Some(tag) = resolve_constructor_tag(tipo, ctor_name, ctx.data_types) else {
                return Err(unsupported_when_pattern(
                    pattern,
                    ctx,
                    s0002_reason_message(ctor_name, &describe_tipo(tipo)),
                ));
            };
            let field_types = constructor_pattern_argument_types(pattern, ctx.data_types);
            if let LocalBinding::PureExpr(expr) = binding {
                if let Some(expr_args) = pure_expr_constructor_fields(expr) {
                    let fields = if expr_args.len() == arguments.len() {
                        arguments
                            .iter()
                            .zip(expr_args.iter())
                            .map(|(arg, expr_arg)| {
                                pattern_value_from_binding(
                                    &arg.value,
                                    &LocalBinding::PureExpr(expr_arg.clone()),
                                    ctx,
                                )
                            })
                            .collect::<Result<Vec<_>, _>>()?
                    } else {
                        vec![ShallowIr::Const(ShallowConst::Unit); arguments.len()]
                    };
                    return Ok(ShallowIr::Construct {
                        module: "".to_string(),
                        constructor: ctor_name.clone(),
                        tag,
                        fields,
                    });
                }
            }

            let mut fields = Vec::with_capacity(arguments.len());
            for (index, arg) in arguments.iter().enumerate() {
                let field_ty = field_types
                    .get(index)
                    .cloned()
                    .unwrap_or(ShallowIrType::Data);
                let label = arg.label.clone().unwrap_or_else(|| index.to_string());
                let Some(projected) = projected_binding(
                    binding,
                    index as u64,
                    label,
                    field_ty,
                    ShallowFieldAccessKind::ConstructorField,
                ) else {
                    return Err(unsupported_when_pattern(
                        pattern,
                        ctx,
                        "constructor-pattern subject cannot be projected faithfully",
                    ));
                };
                fields.push(pattern_value_from_binding(&arg.value, &projected, ctx)?);
            }
            Ok(ShallowIr::Construct {
                module: "".to_string(),
                constructor: ctor_name.clone(),
                tag,
                fields,
            })
        }
        TypedPattern::List { .. } => Err(unsupported_when_pattern(
            pattern,
            ctx,
            "list-pattern when clauses are not lowered faithfully yet",
        )),
    }
}

pub(super) fn when_clause_condition(
    pattern: &TypedPattern,
    subject_binding: &LocalBinding,
    ctx: &TransitionLoweringContext<'_>,
) -> Result<Option<ShallowIr>, TransitionProp> {
    match pattern {
        TypedPattern::Var { .. } | TypedPattern::Discard { .. } => Ok(None),
        TypedPattern::Assign { pattern, .. } => {
            when_clause_condition(pattern, subject_binding, ctx)
        }
        _ => {
            let mut visiting = BTreeSet::new();
            let subject_ir = local_binding_to_shallow_ir(
                subject_binding,
                ctx.data_types,
                &ctx.locals,
                &mut visiting,
            );
            let pattern_ir = pattern_value_from_binding(pattern, subject_binding, ctx)?;
            Ok(Some(ShallowIr::BinOp {
                op: ShallowBinOp::Eq,
                left: Box::new(subject_ir),
                right: Box::new(pattern_ir),
            }))
        }
    }
}

pub(super) fn lower_when_clause_body(
    clause: &TypedClause,
    subject_binding: &LocalBinding,
    ctx: &mut TransitionLoweringContext<'_>,
) -> TransitionProp {
    if let Some(reason) = unsupported_pattern_binding_reason(&clause.pattern) {
        return unsupported_when_pattern(&clause.pattern, ctx, reason);
    }

    let mut clause_locals = ctx.locals.clone();
    bind_pattern_to_locals(
        &clause.pattern,
        subject_binding,
        ctx.data_types,
        &mut clause_locals,
    );
    with_scoped_locals(ctx, clause_locals, |ctx| {
        typed_expr_to_transition_prop_ctx(&clause.then, ctx)
    })
}

pub(super) fn data_type_for_shallow_adt_name<'a>(
    name: &str,
    data_types: &'a IndexMap<&DataTypeKey, &TypedDataType>,
) -> Option<&'a TypedDataType> {
    data_types.iter().find_map(|(key, data_type)| {
        let slash = format!("{}/{}", key.module_name, key.defined_type);
        let dot = format!("{}.{}", key.module_name, key.defined_type);
        (name == slash || name == dot).then_some(*data_type)
    })
}

pub(super) fn exact_field_ir_from_shallow_ir(
    record: &ShallowIr,
    index: u64,
    kind: ShallowFieldAccessKind,
) -> Option<ShallowIr> {
    match (record, kind) {
        (ShallowIr::Construct { fields, .. }, ShallowFieldAccessKind::ConstructorField) => {
            fields.get(index as usize).cloned()
        }
        (ShallowIr::Tuple(elems), ShallowFieldAccessKind::ListElement) => {
            elems.get(index as usize).cloned()
        }
        (ShallowIr::ListLit { elements, tail, .. }, ShallowFieldAccessKind::ListElement) => {
            if tail.is_none() {
                elements.get(index as usize).cloned()
            } else {
                None
            }
        }
        (
            ShallowIr::RecordUpdate {
                record, updates, ..
            },
            ShallowFieldAccessKind::ConstructorField,
        ) => {
            if let Some(update) = updates.iter().find(|update| update.index == index as usize) {
                Some(update.value.clone())
            } else {
                exact_field_ir_from_shallow_ir(record, index, kind)
            }
        }
        (ShallowIr::Let { body, .. }, _) => exact_field_ir_from_shallow_ir(body, index, kind),
        (
            ShallowIr::If {
                cond,
                then_branch,
                else_branch,
            },
            _,
        ) => Some(ShallowIr::If {
            cond: cond.clone(),
            then_branch: Box::new(exact_field_ir_from_shallow_ir(then_branch, index, kind)?),
            else_branch: Box::new(exact_field_ir_from_shallow_ir(else_branch, index, kind)?),
        }),
        (ShallowIr::Match { subject, arms }, _) => Some(ShallowIr::Match {
            subject: subject.clone(),
            arms: arms
                .iter()
                .map(|arm| {
                    Some(ShallowIrArm {
                        tag: arm.tag,
                        bindings: arm.bindings.clone(),
                        guard: arm.guard.clone(),
                        body: exact_field_ir_from_shallow_ir(&arm.body, index, kind)?,
                    })
                })
                .collect::<Option<Vec<_>>>()?,
        }),
        _ => None,
    }
}

pub(super) fn exact_constructor_tags_from_ir(
    ir: &ShallowIr,
    data_types: &IndexMap<&DataTypeKey, &TypedDataType>,
) -> Option<BTreeSet<u64>> {
    match ir {
        ShallowIr::Construct { tag, .. } | ShallowIr::RecordUpdate { tag, .. } => {
            Some(std::iter::once(*tag).collect())
        }
        ShallowIr::FieldAccess {
            record,
            index,
            kind,
            ..
        } => {
            let field = exact_field_ir_from_shallow_ir(record, *index, *kind)?;
            exact_constructor_tags_from_ir(&field, data_types)
        }
        ShallowIr::Let { body, .. } => exact_constructor_tags_from_ir(body, data_types),
        ShallowIr::If {
            then_branch,
            else_branch,
            ..
        } => {
            let mut tags = exact_constructor_tags_from_ir(then_branch, data_types)?;
            tags.extend(exact_constructor_tags_from_ir(else_branch, data_types)?);
            Some(tags)
        }
        ShallowIr::Match { arms, .. } => {
            let mut tags = BTreeSet::new();
            for arm in arms {
                tags.extend(exact_constructor_tags_from_ir(&arm.body, data_types)?);
            }
            Some(tags)
        }
        ShallowIr::BoundVar {
            ty: ShallowIrType::Adt(type_name),
            ..
        } => {
            let data_type = data_type_for_shallow_adt_name(type_name, data_types)?;
            Some(
                data_type
                    .constructors
                    .iter()
                    .filter_map(|constructor| {
                        get_constr_index_variant(data_type, &constructor.name)
                            .map(|(index, _)| index as u64)
                    })
                    .collect(),
            )
        }
        _ => None,
    }
}

pub(super) fn binding_possible_constructor_tags(
    binding: &LocalBinding,
    data_types: &IndexMap<&DataTypeKey, &TypedDataType>,
) -> Option<BTreeSet<u64>> {
    if let LocalBinding::DrawnValue {
        domain: FuzzerSemantics::Constructors { tags },
        ..
    } = binding
    {
        return Some(tags.iter().copied().collect());
    }

    if let LocalBinding::ExactIr { ir, .. } = binding
        && let Some(tags) = exact_constructor_tags_from_ir(ir, data_types)
    {
        return Some(tags);
    }

    let ShallowIrType::Adt(type_name) = binding_shallow_ir_type(binding) else {
        return None;
    };
    let data_type = data_type_for_shallow_adt_name(&type_name, data_types)?;
    Some(
        data_type
            .constructors
            .iter()
            .filter_map(|constructor| {
                get_constr_index_variant(data_type, &constructor.name)
                    .map(|(index, _)| index as u64)
            })
            .collect(),
    )
}

pub(super) fn clause_pattern_tag(
    pattern: &TypedPattern,
    data_types: &IndexMap<&DataTypeKey, &TypedDataType>,
) -> Option<u64> {
    match pattern {
        TypedPattern::Assign { pattern, .. } => clause_pattern_tag(pattern, data_types),
        TypedPattern::Constructor { name, tipo, .. } => {
            resolve_constructor_tag(tipo, name, data_types)
        }
        _ => None,
    }
}

pub(super) fn clause_pattern_is_catchall(pattern: &TypedPattern) -> bool {
    match pattern {
        TypedPattern::Var { .. } | TypedPattern::Discard { .. } => true,
        TypedPattern::Assign { pattern, .. } => clause_pattern_is_catchall(pattern),
        _ => false,
    }
}

pub(super) fn filter_reachable_when_clauses<'a>(
    clauses: &'a [TypedClause],
    subject_binding: &LocalBinding,
    ctx: &TransitionLoweringContext<'_>,
) -> Vec<&'a TypedClause> {
    let Some(possible_tags) = binding_possible_constructor_tags(subject_binding, ctx.data_types)
    else {
        return clauses.iter().collect();
    };
    clauses
        .iter()
        .filter(|clause| {
            clause_pattern_tag(&clause.pattern, ctx.data_types)
                .map(|tag| possible_tags.contains(&tag))
                .unwrap_or(true)
        })
        .collect()
}

pub(super) fn clause_constructor_subpatterns_are_catchall(pattern: &TypedPattern) -> bool {
    match pattern {
        TypedPattern::Assign { pattern, .. } => {
            clause_constructor_subpatterns_are_catchall(pattern)
        }
        TypedPattern::Constructor { arguments, .. } => arguments.iter().all(|arg| {
            matches!(
                arg.value,
                TypedPattern::Var { .. } | TypedPattern::Discard { .. }
            )
        }),
        _ => false,
    }
}

pub(super) fn when_clauses_are_exhaustive(
    clauses: &[&TypedClause],
    subject_binding: &LocalBinding,
    ctx: &TransitionLoweringContext<'_>,
) -> bool {
    if clauses
        .iter()
        .any(|clause| clause_pattern_is_catchall(&clause.pattern))
    {
        return true;
    }
    let Some(possible_tags) = binding_possible_constructor_tags(subject_binding, ctx.data_types)
    else {
        return false;
    };
    let mut covered = BTreeSet::new();
    for clause in clauses {
        let Some(tag) = clause_pattern_tag(&clause.pattern, ctx.data_types) else {
            return false;
        };
        if !clause_constructor_subpatterns_are_catchall(&clause.pattern) {
            return false;
        }
        covered.insert(tag);
    }
    !possible_tags.is_empty() && possible_tags.is_subset(&covered)
}

pub(super) fn lower_when_clause_relation(
    clause: &TypedClause,
    subject_binding: &LocalBinding,
    output_binding: &LocalBinding,
    ctx: &mut TransitionLoweringContext<'_>,
) -> TransitionProp {
    if let Some(reason) = unsupported_pattern_binding_reason(&clause.pattern) {
        return unsupported_when_pattern(&clause.pattern, ctx, reason);
    }

    let mut clause_locals = ctx.locals.clone();
    bind_pattern_to_locals(
        &clause.pattern,
        subject_binding,
        ctx.data_types,
        &mut clause_locals,
    );
    with_scoped_locals(ctx, clause_locals, |ctx| {
        lower_fuzzer_relation_with_locals(&clause.then, output_binding, ctx)
    })
}

pub(super) fn typed_expr_to_transition_prop_ctx(
    expr: &TypedExpr,
    ctx: &mut TransitionLoweringContext<'_>,
) -> TransitionProp {
    if let TypedExpr::Sequence { expressions, .. } | TypedExpr::Pipeline { expressions, .. } = expr
    {
        for expression in expressions.iter().take(expressions.len().saturating_sub(1)) {
            if let TypedExpr::Assignment { pattern, kind, .. } = expression {
                if !kind.is_let() {
                    return TransitionProp::Unsupported {
                        reason: "refutable assignments are not lowered faithfully yet".to_string(),
                        source_location: pattern_source_location(pattern, ctx),
                    };
                }
                if let Some(reason) = unsupported_assignment_pattern_reason(pattern) {
                    return TransitionProp::Unsupported {
                        reason: reason.to_string(),
                        source_location: pattern_source_location(pattern, ctx),
                    };
                }
            }
        }
        if expressions
            .iter()
            .take(expressions.len().saturating_sub(1))
            .any(|expression| {
                matches!(
                    expression,
                    TypedExpr::Assignment { value, .. }
                        if extract_fuzzer_payload_type(value.tipo().as_ref()).is_some()
                )
            })
        {
            return lower_sequence_relation_with_locals(
                expressions,
                &transition_output_binding(),
                ctx,
            );
        }
    }

    if let Some(scoped) = extend_locals_with_leading_assignments(expr, ctx) {
        return with_scoped_locals(ctx, scoped, |ctx| {
            typed_expr_to_transition_prop_ctx(terminal_expression(expr), ctx)
        });
    }

    match terminal_expression(expr) {
        TypedExpr::If {
            branches,
            final_else,
            ..
        } => {
            let mut result = typed_expr_to_transition_prop_ctx(final_else.as_ref(), ctx);
            for branch in branches.iter().rev() {
                let mut visiting = BTreeSet::new();
                let cond_ir = typed_expr_to_shallow_ir_with_locals(
                    &branch.condition,
                    ctx.data_types,
                    &ctx.locals,
                    &mut visiting,
                );
                let then_prop = typed_expr_to_transition_prop_ctx(&branch.body, ctx);
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
            if is_known_bind_call(fun, ctx) {
                if let Some((source, continuation)) = resolve_bind_call(args, ctx) {
                    return translate_resolved_bind_ctx(source, continuation, ctx);
                }
            }

            if let Some(inner) = detect_audited_return_call(fun, args, tipo.as_ref(), ctx) {
                let mut visiting = BTreeSet::new();
                return output_match_prop(
                    &transition_output_binding(),
                    typed_expr_to_shallow_ir_with_locals(
                        inner,
                        ctx.data_types,
                        &ctx.locals,
                        &mut visiting,
                    ),
                    ctx,
                );
            }

            if extract_fuzzer_payload_type(tipo.as_ref()).is_some() {
                return lower_fuzzer_relation_with_locals(expr, &transition_output_binding(), ctx);
            }

            TransitionProp::Unsupported {
                reason: format!(
                    "step-function call '{}' is not a recognized transition relation",
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
            if !ctx.visiting_value_aliases.insert(name.clone()) {
                return TransitionProp::Unsupported {
                    reason: format!("local-alias cycle on '{name}' detected"),
                    source_location: None,
                };
            }
            let result = match ctx.locals.get(name).cloned() {
                Some(LocalBinding::PureExpr(expr)) => typed_expr_to_transition_prop_ctx(&expr, ctx),
                Some(LocalBinding::ExactIr { .. })
                | Some(LocalBinding::DrawnValue { .. })
                | Some(LocalBinding::Projection { .. }) => TransitionProp::Unsupported {
                    reason: format!(
                        "step-function variable '{name}' is a value binding, not a transition predicate"
                    ),
                    source_location: None,
                },
                None => TransitionProp::Unsupported {
                    reason: format!(
                        "step-function variable '{name}' has no known transition content",
                    ),
                    source_location: None,
                },
            };
            ctx.visiting_value_aliases.remove(name);
            result
        }

        TypedExpr::Fn { body, .. } => typed_expr_to_transition_prop_ctx(body.as_ref(), ctx),
        TypedExpr::Trace { then, .. } => typed_expr_to_transition_prop_ctx(then.as_ref(), ctx),

        TypedExpr::When {
            subject, clauses, ..
        } => {
            if clauses.is_empty() {
                return TransitionProp::Unsupported {
                    reason: "When expression with no clauses".to_string(),
                    source_location: None,
                };
            }

            let subject_binding = binding_from_expr(subject, ctx);
            let reachable_clauses = filter_reachable_when_clauses(clauses, &subject_binding, ctx);
            if reachable_clauses.is_empty() {
                return TransitionProp::Unsupported {
                    reason: "when-expression has no reachable clauses under the current domain"
                        .to_string(),
                    source_location: None,
                };
            }

            let exhaustive = when_clauses_are_exhaustive(&reachable_clauses, &subject_binding, ctx);
            let mut clauses_rev = reachable_clauses.iter().rev();
            let Some(last_clause) = clauses_rev.next() else {
                return TransitionProp::Unsupported {
                    reason: "When expression with no clauses".to_string(),
                    source_location: None,
                };
            };
            let mut result = lower_when_clause_body(last_clause, &subject_binding, ctx);
            if !exhaustive {
                let fallback = TransitionProp::Unsupported {
                    reason: "when-expression fell through all clauses".to_string(),
                    source_location: pattern_source_location(&last_clause.pattern, ctx),
                };
                result = match when_clause_condition(&last_clause.pattern, &subject_binding, ctx) {
                    Ok(None) => lower_when_clause_body(last_clause, &subject_binding, ctx),
                    Ok(Some(cond_ir)) => TransitionProp::IfThenElse {
                        cond: cond_ir,
                        t: Box::new(lower_when_clause_body(last_clause, &subject_binding, ctx)),
                        e: Box::new(fallback),
                    },
                    Err(unsupported) => guarded_when_clause_fallback(
                        unsupported,
                        lower_when_clause_body(last_clause, &subject_binding, ctx),
                        fallback,
                    ),
                };
            }

            for clause in clauses_rev {
                let body = lower_when_clause_body(clause, &subject_binding, ctx);
                result = match when_clause_condition(&clause.pattern, &subject_binding, ctx) {
                    Ok(None) => body,
                    Ok(Some(cond_ir)) => TransitionProp::IfThenElse {
                        cond: cond_ir,
                        t: Box::new(body),
                        e: Box::new(result),
                    },
                    Err(unsupported) => guarded_when_clause_fallback(unsupported, body, result),
                };
            }

            result
        }

        _ => TransitionProp::Unsupported {
            reason: format!(
                "step-function expression '{}' is not a recognized transition shape",
                describe_expr(terminal_expression(expr))
            ),
            source_location: None,
        },
    }
}
pub(super) fn is_known_bind_call(fun: &TypedExpr, ctx: &TransitionLoweringContext<'_>) -> bool {
    let mut local_aliases = BTreeSet::new();
    resolve_function_from_expr(
        fun,
        &ctx.current_module,
        ctx.function_index,
        &ctx.expr_locals(),
        &mut local_aliases,
    )
    .is_some_and(|resolved| {
        resolved.module_name == STDLIB_FUZZ_MODULE && resolved.function_name == "and_then"
    }) || extract_module_fn_identity(fun)
        .is_some_and(|(module, name)| module == STDLIB_FUZZ_MODULE && name == "and_then")
}

pub(super) fn known_fork_call_name(
    fun: &TypedExpr,
    ctx: &TransitionLoweringContext<'_>,
) -> Option<String> {
    let mut local_aliases = BTreeSet::new();
    resolve_function_from_expr(
        fun,
        &ctx.current_module,
        ctx.function_index,
        &ctx.expr_locals(),
        &mut local_aliases,
    )
    .and_then(|resolved| {
        (resolved.module_name == STDLIB_FUZZ_SCENARIO_MODULE
            && matches!(
                resolved.function_name.as_str(),
                "fork_and_then" | "fork2_and_then" | "fork_if_and_then"
            ))
        .then_some(resolved.function_name)
    })
    .or_else(|| {
        extract_module_fn_identity(fun).and_then(|(module, name)| {
            (module == STDLIB_FUZZ_SCENARIO_MODULE
                && matches!(
                    name.as_str(),
                    "fork_and_then" | "fork2_and_then" | "fork_if_and_then"
                ))
            .then_some(name)
        })
    })
}

pub(super) fn is_known_relation_lookalike_name(name: &str) -> bool {
    matches!(
        name,
        "and_then"
            | "map"
            | "such_that"
            | "one_of"
            | "either"
            | "fork_and_then"
            | "fork2_and_then"
            | "fork_if_and_then"
    )
}

pub(super) fn extract_module_fn_identity_with_local_aliases(
    fun: &TypedExpr,
    locals: &BTreeMap<String, TypedExpr>,
    visiting_local_aliases: &mut BTreeSet<String>,
) -> Option<(String, String)> {
    let fun = terminal_expression(fun);
    match fun {
        TypedExpr::Var {
            name, constructor, ..
        } if matches!(
            constructor.variant,
            ValueConstructorVariant::LocalVariable { .. }
        ) =>
        {
            let bound_expr = locals.get(name)?;
            if !visiting_local_aliases.insert(name.clone()) {
                return None;
            }
            let result = extract_module_fn_identity_with_local_aliases(
                bound_expr,
                locals,
                visiting_local_aliases,
            );
            visiting_local_aliases.remove(name);
            result
        }
        _ => extract_module_fn_identity(fun),
    }
}

pub(super) fn is_known_fuzz_call(
    fun: &TypedExpr,
    ctx: &TransitionLoweringContext<'_>,
    fn_name: &str,
) -> bool {
    let mut local_aliases = BTreeSet::new();
    resolve_function_from_expr(
        fun,
        &ctx.current_module,
        ctx.function_index,
        &ctx.expr_locals(),
        &mut local_aliases,
    )
    .is_some_and(|resolved| {
        resolved.module_name == STDLIB_FUZZ_MODULE && resolved.function_name == fn_name
    }) || extract_module_fn_identity_with_local_aliases(
        fun,
        &ctx.expr_locals(),
        &mut BTreeSet::new(),
    )
    .is_some_and(|(module, name)| module == STDLIB_FUZZ_MODULE && name == fn_name)
}

pub(super) fn resolve_relation_function_identity(
    fun: &TypedExpr,
    ctx: &TransitionLoweringContext<'_>,
) -> Option<(String, String)> {
    let mut local_aliases = BTreeSet::new();
    resolve_function_from_expr(
        fun,
        &ctx.current_module,
        ctx.function_index,
        &ctx.expr_locals(),
        &mut local_aliases,
    )
    .map(|resolved| (resolved.module_name, resolved.function_name))
    .or_else(|| extract_module_fn_identity(fun))
}

pub(super) fn classify_sub_generator_blocker(
    fun: &TypedExpr,
    ctx: &TransitionLoweringContext<'_>,
) -> SubGeneratorBlocker {
    let Some((module, name)) = resolve_relation_function_identity(fun, ctx) else {
        return SubGeneratorBlocker::ExternalOrUninspectableHelper;
    };

    if module == STDLIB_FUZZ_MODULE || module == STDLIB_FUZZ_SCENARIO_MODULE {
        return SubGeneratorBlocker::KnownPrimitiveCombinator;
    }

    if is_known_relation_lookalike_name(&name) {
        return SubGeneratorBlocker::UserDefinedLookalikeCombinator;
    }

    if ctx
        .visiting_functions
        .contains(&(module.clone(), name.clone()))
    {
        SubGeneratorBlocker::RecursiveHelper
    } else {
        SubGeneratorBlocker::UnsupportedButInspectableShape
    }
}

pub(super) fn relation_fallback_for_call(
    fun: &TypedExpr,
    ctx: &TransitionLoweringContext<'_>,
    reason: impl Into<String>,
) -> TransitionProp {
    let reason = reason.into();
    match classify_sub_generator_blocker(fun, ctx) {
        SubGeneratorBlocker::KnownPrimitiveCombinator => TransitionProp::Unsupported {
            reason,
            source_location: None,
        },
        SubGeneratorBlocker::UnsupportedButInspectableShape
        | SubGeneratorBlocker::RecursiveHelper
        | SubGeneratorBlocker::ExternalOrUninspectableHelper
        | SubGeneratorBlocker::UserDefinedLookalikeCombinator => {
            match resolve_relation_function_identity(fun, ctx) {
                Some((module, fn_name)) => TransitionProp::SubGenerator { module, fn_name },
                None => TransitionProp::Unsupported {
                    reason,
                    source_location: None,
                },
            }
        }
    }
}

pub(super) fn lower_known_fork_call_with_locals(
    fun: &TypedExpr,
    args: &[CallArg<TypedExpr>],
    output_binding: &LocalBinding,
    ctx: &mut TransitionLoweringContext<'_>,
) -> Option<TransitionProp> {
    let fork_name = known_fork_call_name(fun, ctx)?;
    let thunk_bodies = detect_fork_call(args)?;
    let continuation = match args
        .iter()
        .find(|arg| expression_is_bind_continuation(&arg.value))
    {
        Some(arg) => match resolve_relation_continuation(&arg.value, ctx) {
            Some(continuation) => Some(continuation),
            None => {
                return Some(TransitionProp::Unsupported {
                    reason: "fork continuation is not a visible unary function".to_string(),
                    source_location: None,
                });
            }
        },
        None => None,
    };
    let cond_ir = if fork_name == "fork_if_and_then" {
        let cond_expr = args.iter().find_map(|arg| {
            let value = &arg.value;
            (!expression_has_fuzzer_type(value)
                && zero_arg_fuzzer_thunk_body(value).is_none()
                && !expression_is_bind_continuation(value))
            .then_some(value)
        })?;
        let mut visiting = BTreeSet::new();
        Some(typed_expr_to_shallow_ir_with_locals(
            cond_expr,
            ctx.data_types,
            &ctx.locals,
            &mut visiting,
        ))
    } else {
        None
    };

    let branch_relation = |ctx: &mut TransitionLoweringContext<'_>,
                           output_binding: &LocalBinding,
                           cond_ir: Option<ShallowIr>| {
        if let Some(cond_ir) = cond_ir {
            if thunk_bodies.len() != 2 {
                return TransitionProp::Unsupported {
                    reason: format!(
                        "fork_if_and_then expects exactly 2 branch thunks, found {}",
                        thunk_bodies.len()
                    ),
                    source_location: None,
                };
            }
            TransitionProp::IfThenElse {
                cond: cond_ir,
                t: Box::new(lower_fuzzer_relation_with_locals(
                    thunk_bodies[0],
                    output_binding,
                    ctx,
                )),
                e: Box::new(lower_fuzzer_relation_with_locals(
                    thunk_bodies[1],
                    output_binding,
                    ctx,
                )),
            }
        } else {
            let mut branches = Vec::with_capacity(thunk_bodies.len());
            for body in &thunk_bodies {
                branches.push(lower_fuzzer_relation_with_locals(body, output_binding, ctx));
            }
            TransitionProp::Or(branches)
        }
    };

    Some(
        if let Some(ResolvedRelationContinuation {
            binder,
            binder_ty,
            body: cont_body,
            module,
            locals: cont_locals,
        }) = continuation
        {
            let exact_branch_bindings = thunk_bodies
                .iter()
                .map(|body| exact_output_binding_from_fuzzer_expr(body, ctx))
                .collect::<Option<Vec<_>>>();

            if let Some(exact_branch_bindings) = exact_branch_bindings {
                let lower_exact_branch =
                    |ctx: &mut TransitionLoweringContext<'_>,
                     body: &TypedExpr,
                     binding: LocalBinding| {
                        let source_relation = lower_relation_with_bound_domain(body, &binding, ctx);
                        let mut scoped = cont_locals.clone();
                        scoped.insert(binder.clone(), binding);
                        let continuation = with_current_module(ctx, module.clone(), |ctx| {
                            with_scoped_locals(ctx, scoped, |ctx| {
                                lower_fuzzer_relation_with_locals(&cont_body, output_binding, ctx)
                            })
                        });
                        transition_prop_from_parts(vec![source_relation, continuation])
                    };

                if let Some(cond_ir) = cond_ir {
                    if thunk_bodies.len() != 2 || exact_branch_bindings.len() != 2 {
                        TransitionProp::Unsupported {
                            reason: format!(
                                "fork_if_and_then expects exactly 2 branch thunks, found {}",
                                thunk_bodies.len()
                            ),
                            source_location: None,
                        }
                    } else {
                        TransitionProp::IfThenElse {
                            cond: cond_ir,
                            t: Box::new(lower_exact_branch(
                                ctx,
                                thunk_bodies[0],
                                exact_branch_bindings[0].clone(),
                            )),
                            e: Box::new(lower_exact_branch(
                                ctx,
                                thunk_bodies[1],
                                exact_branch_bindings[1].clone(),
                            )),
                        }
                    }
                } else {
                    TransitionProp::Or(
                        thunk_bodies
                            .iter()
                            .zip(exact_branch_bindings.into_iter())
                            .map(|(body, binding)| lower_exact_branch(ctx, body, binding))
                            .collect(),
                    )
                }
            } else {
                let binding = LocalBinding::DrawnValue {
                    lean_name: binder.clone(),
                    ty: binder_ty.clone(),
                    domain: FuzzerSemantics::Data,
                };
                let branches = branch_relation(ctx, &binding, cond_ir);
                let mut scoped = cont_locals;
                scoped.insert(binder.clone(), binding);
                let continuation = with_current_module(ctx, module, |ctx| {
                    with_scoped_locals(ctx, scoped, |ctx| {
                        lower_fuzzer_relation_with_locals(&cont_body, output_binding, ctx)
                    })
                });
                TransitionProp::Exists {
                    binder,
                    ty: binder_ty,
                    domain: Box::new(FuzzerSemantics::Data),
                    body: Box::new(TransitionProp::And(vec![branches, continuation])),
                }
            }
        } else {
            branch_relation(ctx, output_binding, cond_ir)
        },
    )
}
pub(super) struct ResolvedUnaryCallbackBody {
    binder: String,
    body: TypedExpr,
    module: String,
    locals: BTreeMap<String, LocalBinding>,
}

pub(super) fn resolve_unary_callback_body(
    callback: &TypedExpr,
    ctx: &TransitionLoweringContext<'_>,
) -> Option<ResolvedUnaryCallbackBody> {
    let mut callback_expr = terminal_expression(callback).clone();
    let mut callback_module = ctx.current_module.clone();
    let mut callback_expr_locals = ctx.expr_locals();
    let mut callback_locals = ctx.locals.clone();
    let mut visiting_functions = BTreeSet::new();

    loop {
        match terminal_expression(&callback_expr) {
            TypedExpr::Fn { args, body, .. } if args.len() == 1 => {
                let binder = args[0]
                    .get_variable_name()
                    .unwrap_or("_callback")
                    .to_string();
                return Some(ResolvedUnaryCallbackBody {
                    binder,
                    body: body.as_ref().clone(),
                    module: callback_module.clone(),

                    locals: callback_locals.clone(),
                });
            }
            TypedExpr::Trace { then, .. } => {
                callback_expr = then.as_ref().clone();
            }
            _ => {
                let (resolved, resolved_expr_locals, applied_arg_count) =
                    resolve_function_with_applied_args(
                        &callback_expr,
                        &callback_module,
                        ctx.function_index,
                        &callback_expr_locals,
                    )?;
                let key = (resolved.module_name.clone(), resolved.function_name.clone());
                if !visiting_functions.insert(key) {
                    return None;
                }

                let resolved_locals: BTreeMap<String, LocalBinding> = resolved_expr_locals
                    .iter()
                    .map(|(name, expr)| {
                        let mut visiting = BTreeSet::new();
                        (
                            name.clone(),
                            binding_from_expr_with_locals(expr, &callback_locals, &mut visiting),
                        )
                    })
                    .collect();

                let remaining_args = resolved
                    .function
                    .arguments
                    .len()
                    .saturating_sub(applied_arg_count);

                if remaining_args == 1 {
                    let binder = resolved.function.arguments[applied_arg_count]
                        .get_variable_name()
                        .unwrap_or("_callback")
                        .to_string();
                    return Some(ResolvedUnaryCallbackBody {
                        binder,
                        body: resolved.function.body.clone(),
                        module: resolved.module_name,
                        locals: resolved_locals,
                    });
                }

                if remaining_args == 0 {
                    callback_expr = resolved.function.body.clone();
                    callback_module = resolved.module_name;
                    callback_expr_locals = resolved_expr_locals;
                    callback_locals = resolved_locals;
                    continue;
                }

                return None;
            }
        }
    }
}

pub(super) fn lower_bool_predicate_with_locals(
    predicate: &TypedExpr,
    output_binding: &LocalBinding,
    ctx: &mut TransitionLoweringContext<'_>,
) -> TransitionProp {
    let Some(ResolvedUnaryCallbackBody {
        binder,
        body,
        module,
        locals,
    }) = resolve_unary_callback_body(predicate, ctx)
    else {
        return TransitionProp::Unsupported {
            reason: "predicate callback is not a visible unary function".to_string(),
            source_location: None,
        };
    };

    let mut scoped = locals;
    scoped.insert(binder, output_binding.clone());
    let bool_ir = with_current_module(ctx, module, |ctx| {
        with_scoped_locals(ctx, scoped, |ctx| {
            let mut visiting = BTreeSet::new();
            typed_expr_to_shallow_ir_with_locals(&body, ctx.data_types, &ctx.locals, &mut visiting)
        })
    });

    if shallow_ir_is_exact_bool_expr(&bool_ir) {
        TransitionProp::Pure(bool_ir)
    } else {
        TransitionProp::Unsupported {
            reason: format!(
                "predicate body is not emitted exactly on counted path (root='{}')",
                shallow_ir_root_kind(&bool_ir)
            ),
            source_location: None,
        }
    }
}

pub(super) fn equality_prop(left: ShallowIr, right: ShallowIr) -> TransitionProp {
    TransitionProp::Pure(ShallowIr::BinOp {
        op: ShallowBinOp::Eq,
        left: Box::new(left),
        right: Box::new(right),
    })
}

pub(super) fn exact_output_binding_from_fuzzer_expr(
    source: &TypedExpr,
    ctx: &TransitionLoweringContext<'_>,
) -> Option<LocalBinding> {
    let payload_ty =
        extract_fuzzer_payload_type(source.tipo().as_ref()).map(|ty| shallow_ir_type(&ty))?;

    if let Some((resolved_head, applied_args)) =
        flatten_call_head_and_args(source, &[], &ctx.expr_locals())
    {
        let mut visiting_local_aliases = BTreeSet::new();
        if let Some(resolved) = resolve_function_from_expr(
            &resolved_head,
            &ctx.current_module,
            ctx.function_index,
            &ctx.expr_locals(),
            &mut visiting_local_aliases,
        ) {
            if applied_args.len() == resolved.function.arguments.len() {
                let mut locals = BTreeMap::new();
                for (param, arg) in resolved.function.arguments.iter().zip(applied_args.iter()) {
                    if let Some(name) = param.get_variable_name() {
                        let mut visiting_bindings = BTreeSet::new();
                        locals.insert(
                            name.to_string(),
                            binding_from_expr_with_locals(arg, &ctx.locals, &mut visiting_bindings),
                        );
                    }
                }
                inject_module_constants(&mut locals, &resolved.module_name, ctx.constant_index);

                let mut visiting = BTreeSet::new();
                let ir = typed_expr_to_shallow_ir_with_locals(
                    &resolved.function.body,
                    ctx.data_types,
                    &locals,
                    &mut visiting,
                );
                if !shallow_ir_is_vacuous(&ir) {
                    return Some(LocalBinding::ExactIr { ty: payload_ty, ir });
                }
            }
        }
    }

    let mut visiting = BTreeSet::new();
    let ir =
        typed_expr_to_shallow_ir_with_locals(source, ctx.data_types, &ctx.locals, &mut visiting);
    (!shallow_ir_is_vacuous(&ir)).then_some(LocalBinding::ExactIr { ty: payload_ty, ir })
}

pub(super) fn output_binding_ir(
    output_binding: &LocalBinding,
    ctx: &TransitionLoweringContext<'_>,
) -> ShallowIr {
    let mut visiting = BTreeSet::new();
    local_binding_to_shallow_ir(output_binding, ctx.data_types, &ctx.locals, &mut visiting)
}

pub(super) fn constructor_choice_domain_from_expr(
    expr: &TypedExpr,
    data_types: &IndexMap<&DataTypeKey, &TypedDataType>,
) -> Option<FuzzerSemantics> {
    let TypedExpr::Call { fun, args, .. } = terminal_expression(expr) else {
        return None;
    };
    let Some((module, fn_name)) = extract_module_fn_identity(fun.as_ref()) else {
        return None;
    };
    if module != STDLIB_FUZZ_MODULE || fn_name != "one_of" {
        return None;
    }
    let [values] = args.as_slice() else {
        return None;
    };
    let TypedExpr::List { elements, .. } = terminal_expression(&values.value) else {
        return None;
    };
    let mut tags = Vec::new();
    for element in elements {
        tags.push(typed_expr_constructor_tag(element, data_types)?);
    }
    tags.sort_unstable();
    tags.dedup();
    Some(FuzzerSemantics::Constructors { tags })
}

pub(super) fn normalize_source_domain(
    source: &TypedExpr,
    ctx: &mut TransitionLoweringContext<'_>,
) -> FuzzerSemantics {
    if let Some(domain) = constructor_choice_domain_from_expr(source, ctx.data_types) {
        return domain;
    }

    if let TypedExpr::Call { fun, args, .. } = terminal_expression(source) {
        if is_known_fuzz_call(fun, ctx, "and_then") || is_known_fuzz_call(fun, ctx, "map") {
            if let Some(source_arg) = args.first() {
                return normalize_source_domain(&source_arg.value, ctx);
            }
        }
        if is_known_fuzz_call(fun, ctx, "such_that") {
            if let [source_arg, _] = args.as_slice() {
                return normalize_source_domain(&source_arg.value, ctx);
            }
        }
    }
    let payload_type = extract_fuzzer_payload_type(source.tipo().as_ref());
    let normalized = normalize_fuzzer_from_expr(
        source,
        &ctx.current_module,
        ctx.function_index,
        ctx.constant_index,
        &ctx.expr_locals(),
        &mut ctx.visiting_functions,
    );
    let semantics = if let Some(payload_type) = payload_type.as_ref() {
        normalized_fuzzer_semantics(
            &normalized,
            &ctx.current_module,
            ctx.function_index,
            ctx.constant_index,
            ctx.data_types,
            payload_type.as_ref(),
            &ctx.expr_locals(),
            &mut ctx.visiting_functions,
        )
    } else {
        fuzzer_semantics_of_normalized(&normalized)
    };
    if matches!(semantics, FuzzerSemantics::Opaque { .. }) {
        if let Some(payload_type) = payload_type {
            return default_semantics_for_type(payload_type.as_ref(), ctx.data_types);
        }
    }
    semantics
}

pub(super) fn lower_sequence_relation_with_locals(
    expressions: &[TypedExpr],
    output_binding: &LocalBinding,
    ctx: &mut TransitionLoweringContext<'_>,
) -> TransitionProp {
    let Some((first, rest)) = expressions.split_first() else {
        return TransitionProp::And(vec![]);
    };
    match first {
        TypedExpr::Assignment {
            pattern,
            value,
            kind,
            ..
        } => {
            if !kind.is_let() {
                return TransitionProp::Unsupported {
                    reason: "refutable assignments are not lowered faithfully yet".to_string(),
                    source_location: pattern_source_location(pattern, ctx),
                };
            }
            if let Some(reason) = unsupported_assignment_pattern_reason(pattern) {
                return TransitionProp::Unsupported {
                    reason: reason.to_string(),
                    source_location: pattern_source_location(pattern, ctx),
                };
            }

            if extract_fuzzer_payload_type(value.tipo().as_ref()).is_some() {
                let binder = pattern_var_name(pattern)
                    .map(ToString::to_string)
                    .unwrap_or_else(|| ctx.fresh_synthetic_binder("_backpass"));
                let domain = normalize_source_domain(value, ctx);
                let mut binder_ty = extract_fuzzer_payload_type(value.tipo().as_ref())
                    .map(|ty| shallow_ir_type(&ty))
                    .unwrap_or(ShallowIrType::Unknown);
                if matches!(binder_ty, ShallowIrType::Unknown | ShallowIrType::Data) {
                    binder_ty = shallow_ir_type_from_semantics(&domain);
                }
                let binding = LocalBinding::DrawnValue {
                    lean_name: binder.clone(),
                    ty: binder_ty.clone(),
                    domain: domain.clone(),
                };
                let relation_binding = exact_output_binding_from_fuzzer_expr(value, ctx)
                    .unwrap_or_else(|| binding.clone());
                let source_relation =
                    lower_relation_with_bound_domain(value, &relation_binding, ctx);
                let mut scoped = ctx.locals.clone();
                bind_pattern_to_locals(pattern, &relation_binding, ctx.data_types, &mut scoped);
                let continuation = with_scoped_locals(ctx, scoped, |ctx| {
                    lower_sequence_relation_with_locals(rest, output_binding, ctx)
                });
                TransitionProp::Exists {
                    binder,
                    ty: binder_ty,
                    domain: Box::new(domain),
                    body: Box::new(transition_prop_from_parts(vec![
                        source_relation,
                        continuation,
                    ])),
                }
            } else {
                let binding = binding_from_expr(value, ctx);
                let mut scoped = ctx.locals.clone();
                bind_pattern_to_locals(pattern, &binding, ctx.data_types, &mut scoped);
                with_scoped_locals(ctx, scoped, |ctx| {
                    lower_sequence_relation_with_locals(rest, output_binding, ctx)
                })
            }
        }
        TypedExpr::Trace { then, .. } if rest.is_empty() => {
            lower_fuzzer_relation_with_locals(then, output_binding, ctx)
        }
        _ if rest.is_empty() => lower_fuzzer_relation_with_locals(first, output_binding, ctx),
        _ => lower_sequence_relation_with_locals(rest, output_binding, ctx),
    }
}

pub(super) fn lower_fuzzer_relation_with_locals(
    source: &TypedExpr,
    output_binding: &LocalBinding,
    ctx: &mut TransitionLoweringContext<'_>,
) -> TransitionProp {
    match terminal_expression(source) {
        TypedExpr::Trace { then, .. } => {
            lower_fuzzer_relation_with_locals(then, output_binding, ctx)
        }
        TypedExpr::Sequence { expressions, .. } | TypedExpr::Pipeline { expressions, .. } => {
            lower_sequence_relation_with_locals(expressions, output_binding, ctx)
        }
        TypedExpr::If {
            branches,
            final_else,
            ..
        } => {
            let mut result =
                lower_fuzzer_relation_with_locals(final_else.as_ref(), output_binding, ctx);
            for branch in branches.iter().rev() {
                let mut visiting = BTreeSet::new();
                let cond_ir = typed_expr_to_shallow_ir_with_locals(
                    &branch.condition,
                    ctx.data_types,
                    &ctx.locals,
                    &mut visiting,
                );
                let then_prop =
                    lower_fuzzer_relation_with_locals(&branch.body, output_binding, ctx);
                result = TransitionProp::IfThenElse {
                    cond: cond_ir,
                    t: Box::new(then_prop),
                    e: Box::new(result),
                };
            }
            result
        }
        TypedExpr::When {
            subject, clauses, ..
        } => {
            if clauses.is_empty() {
                return TransitionProp::Unsupported {
                    reason: "When expression with no clauses".to_string(),
                    source_location: None,
                };
            }

            let subject_binding = binding_from_expr(subject, ctx);
            let reachable_clauses = filter_reachable_when_clauses(clauses, &subject_binding, ctx);
            if reachable_clauses.is_empty() {
                return TransitionProp::Unsupported {
                    reason: "when-expression has no reachable clauses under the current domain"
                        .to_string(),
                    source_location: None,
                };
            }

            let exhaustive = when_clauses_are_exhaustive(&reachable_clauses, &subject_binding, ctx);
            let mut clauses_rev = reachable_clauses.iter().rev();
            let Some(last_clause) = clauses_rev.next() else {
                return TransitionProp::Unsupported {
                    reason: "When expression with no clauses".to_string(),
                    source_location: None,
                };
            };
            let mut result =
                lower_when_clause_relation(last_clause, &subject_binding, output_binding, ctx);
            if !exhaustive {
                let fallback = TransitionProp::Unsupported {
                    reason: "when-expression fell through all clauses".to_string(),
                    source_location: pattern_source_location(&last_clause.pattern, ctx),
                };
                result = match when_clause_condition(&last_clause.pattern, &subject_binding, ctx) {
                    Ok(None) => lower_when_clause_relation(
                        last_clause,
                        &subject_binding,
                        output_binding,
                        ctx,
                    ),
                    Ok(Some(cond_ir)) => TransitionProp::IfThenElse {
                        cond: cond_ir,
                        t: Box::new(lower_when_clause_relation(
                            last_clause,
                            &subject_binding,
                            output_binding,
                            ctx,
                        )),
                        e: Box::new(fallback),
                    },
                    Err(unsupported) => guarded_when_clause_fallback(
                        unsupported,
                        lower_when_clause_relation(
                            last_clause,
                            &subject_binding,
                            output_binding,
                            ctx,
                        ),
                        fallback,
                    ),
                };
            }

            for clause in clauses_rev {
                let body =
                    lower_when_clause_relation(clause, &subject_binding, output_binding, ctx);
                result = match when_clause_condition(&clause.pattern, &subject_binding, ctx) {
                    Ok(None) => body,
                    Ok(Some(cond_ir)) => TransitionProp::IfThenElse {
                        cond: cond_ir,
                        t: Box::new(body),
                        e: Box::new(result),
                    },
                    Err(unsupported) => guarded_when_clause_fallback(unsupported, body, result),
                };
            }

            result
        }
        TypedExpr::Call {
            fun, args, tipo, ..
        } => {
            if let Some(inner) = detect_audited_return_call(fun, args, tipo.as_ref(), ctx) {
                let mut visiting = BTreeSet::new();
                return output_match_prop(
                    output_binding,
                    typed_expr_to_shallow_ir_with_locals(
                        inner,
                        ctx.data_types,
                        &ctx.locals,
                        &mut visiting,
                    ),
                    ctx,
                );
            }
            if is_known_fuzz_call(fun, ctx, "and_then") && args.len() == 1 {
                return relation_fallback_for_call(
                    fun,
                    ctx,
                    "relation-bearing and_then call is missing a recognized continuation",
                );
            }
            if let Some(fork_relation) =
                lower_known_fork_call_with_locals(fun, args, output_binding, ctx)
            {
                return fork_relation;
            }
            if is_known_fuzz_call(fun, ctx, "one_of") {
                return match args.as_slice() {
                    [values] => match terminal_expression(&values.value) {
                        TypedExpr::List { elements, .. } if elements.is_empty() => {
                            TransitionProp::Unsupported {
                                reason: "one_of requires a non-empty literal list".to_string(),
                                source_location: None,
                            }
                        }
                        TypedExpr::List { elements, .. }
                            if elements.len() <= MAX_FINITE_DOMAIN_CASES =>
                        {
                            let mut branches = Vec::with_capacity(elements.len());
                            for elem in elements {
                                let mut visiting = BTreeSet::new();
                                let branch = output_match_prop(
                                    output_binding,
                                    typed_expr_to_shallow_ir_with_locals(
                                        elem,
                                        ctx.data_types,
                                        &ctx.locals,
                                        &mut visiting,
                                    ),
                                    ctx,
                                );
                                if matches!(branch, TransitionProp::Unsupported { .. }) {
                                    return TransitionProp::Unsupported {
                                        reason: "one_of literal element is not emitted exactly on counted path"
                                            .to_string(),
                                        source_location: None,
                                    };
                                }
                                branches.push(branch);
                            }
                            TransitionProp::Or(branches)
                        }
                        TypedExpr::List { .. } => TransitionProp::Unsupported {
                            reason: format!(
                                "one_of literal list exceeds exact support cap of {} elements",
                                MAX_FINITE_DOMAIN_CASES
                            ),
                            source_location: None,
                        },
                        _ => TransitionProp::Unsupported {
                            reason: "one_of requires a literal list domain".to_string(),
                            source_location: None,
                        },
                    },
                    _ => TransitionProp::Unsupported {
                        reason: "one_of requires exactly one list argument".to_string(),
                        source_location: None,
                    },
                };
            }
            if is_known_fuzz_call(fun, ctx, "either") {
                if let [left, right] = args.as_slice() {
                    let left_prop =
                        lower_fuzzer_relation_with_locals(&left.value, output_binding, ctx);
                    let right_prop =
                        lower_fuzzer_relation_with_locals(&right.value, output_binding, ctx);
                    if transition_prop_contains_unsupported(&left_prop)
                        || transition_prop_contains_unsupported(&right_prop)
                    {
                        return relation_fallback_for_call(
                            fun,
                            ctx,
                            "either branch is not lowered faithfully yet",
                        );
                    }
                    return TransitionProp::Or(vec![left_prop, right_prop]);
                }
            }
            if is_known_fuzz_call(fun, ctx, "such_that") {
                if let [source_arg, predicate_arg] = args.as_slice() {
                    let source_relation =
                        lower_relation_with_bound_domain(&source_arg.value, output_binding, ctx);
                    let predicate_relation =
                        lower_bool_predicate_with_locals(&predicate_arg.value, output_binding, ctx);
                    return transition_prop_from_parts(vec![source_relation, predicate_relation]);
                }
            }
            if is_known_fuzz_call(fun, ctx, "map") {
                match args.as_slice() {
                    [source_arg] => {
                        return lower_relation_with_bound_domain(
                            &source_arg.value,
                            output_binding,
                            ctx,
                        );
                    }
                    [source_arg, mapper_arg] => {
                        let source_binder = "_map_source".to_string();
                        let source_domain = normalize_source_domain(&source_arg.value, ctx);
                        let mut source_ty =
                            extract_fuzzer_payload_type(source_arg.value.tipo().as_ref())
                                .map(|ty| shallow_ir_type(&ty))
                                .unwrap_or(ShallowIrType::Unknown);
                        if matches!(source_ty, ShallowIrType::Unknown | ShallowIrType::Data) {
                            source_ty = shallow_ir_type_from_semantics(&source_domain);
                        }
                        let source_binding = LocalBinding::DrawnValue {
                            lean_name: source_binder.clone(),
                            ty: source_ty.clone(),
                            domain: source_domain.clone(),
                        };
                        let source_relation = lower_relation_with_bound_domain(
                            &source_arg.value,
                            &source_binding,
                            ctx,
                        );
                        if let Some(ResolvedUnaryCallbackBody {
                            binder,
                            body,
                            module,
                            locals,
                        }) = resolve_unary_callback_body(&mapper_arg.value, ctx)
                        {
                            let mut scoped = locals;
                            scoped.insert(binder, source_binding.clone());
                            let mapped = with_current_module(ctx, module, |ctx| {
                                with_scoped_locals(ctx, scoped, |ctx| {
                                    let mut visiting = BTreeSet::new();
                                    typed_expr_to_shallow_ir_with_locals(
                                        &body,
                                        ctx.data_types,
                                        &ctx.locals,
                                        &mut visiting,
                                    )
                                })
                            });
                            let eq = output_match_prop(output_binding, mapped, ctx);
                            return TransitionProp::Exists {
                                binder: source_binder,
                                ty: source_ty,
                                domain: Box::new(source_domain),
                                body: Box::new(transition_prop_from_parts(vec![
                                    source_relation,
                                    eq,
                                ])),
                            };
                        }
                    }
                    _ => {}
                }
            }
            if is_known_bind_call(fun, ctx) {
                if let Some((bind_source, continuation)) = resolve_bind_call(args, ctx) {
                    let ResolvedRelationContinuation {
                        binder,
                        binder_ty: _,
                        body: cont_body,
                        module,
                        locals: cont_locals,
                    } = continuation;
                    let source_domain = normalize_source_domain(bind_source, ctx);
                    let mut source_ty = extract_fuzzer_payload_type(bind_source.tipo().as_ref())
                        .map(|ty| shallow_ir_type(&ty))
                        .unwrap_or(ShallowIrType::Unknown);
                    if matches!(source_ty, ShallowIrType::Unknown | ShallowIrType::Data) {
                        source_ty = shallow_ir_type_from_semantics(&source_domain);
                    }
                    let source_binding = LocalBinding::DrawnValue {
                        lean_name: binder.clone(),
                        ty: source_ty.clone(),
                        domain: source_domain.clone(),
                    };
                    let source_relation =
                        lower_relation_with_bound_domain(bind_source, &source_binding, ctx);
                    let mut scoped = cont_locals;
                    scoped.insert(binder.clone(), source_binding);
                    let continuation = with_current_module(ctx, module, |ctx| {
                        with_scoped_locals(ctx, scoped, |ctx| {
                            lower_fuzzer_relation_with_locals(&cont_body, output_binding, ctx)
                        })
                    });
                    return TransitionProp::Exists {
                        binder,
                        ty: source_ty,
                        domain: Box::new(source_domain),
                        body: Box::new(transition_prop_from_parts(vec![
                            source_relation,
                            continuation,
                        ])),
                    };
                }
            }
            if let Some(inlined) = inline_resolved_fuzzer_relation_call(source, output_binding, ctx)
            {
                return inlined;
            }
            relation_fallback_for_call(
                fun,
                ctx,
                format!(
                    "relation-bearing call '{}' is not lowered faithfully yet",
                    describe_expr(fun)
                ),
            )
        }
        _ => {
            let mut visiting = BTreeSet::new();
            output_match_prop(
                output_binding,
                typed_expr_to_shallow_ir_with_locals(
                    source,
                    ctx.data_types,
                    &ctx.locals,
                    &mut visiting,
                ),
                ctx,
            )
        }
    }
}

pub(super) fn translate_resolved_bind_ctx(
    source: &TypedExpr,
    continuation: ResolvedRelationContinuation,
    ctx: &mut TransitionLoweringContext<'_>,
) -> TransitionProp {
    let ResolvedRelationContinuation {
        binder,
        binder_ty: _,
        body: cont_body,
        module,
        locals: cont_locals,
    } = continuation;
    let domain = normalize_source_domain(source, ctx);
    let mut binder_ty = extract_fuzzer_payload_type(source.tipo().as_ref())
        .map(|ty| shallow_ir_type(&ty))
        .unwrap_or(ShallowIrType::Unknown);
    if matches!(binder_ty, ShallowIrType::Unknown | ShallowIrType::Data) {
        binder_ty = shallow_ir_type_from_semantics(&domain);
    }
    let cyclic = ctx.visiting_locals.contains(&binder);
    let self_referential = !cyclic && free_vars_in_typed_expr(source).contains(&binder);
    let binding = LocalBinding::DrawnValue {
        lean_name: binder.clone(),
        ty: binder_ty.clone(),
        domain: domain.clone(),
    };
    let source_relation = lower_relation_with_bound_domain(source, &binding, ctx);
    let mut scoped = cont_locals;
    if !cyclic && !self_referential {
        scoped.insert(binder.clone(), binding);
    }
    let saved_visiting = ctx.visiting_locals.clone();
    ctx.visiting_locals.insert(binder.clone());
    let body_prop = with_current_module(ctx, module, |ctx| {
        with_scoped_locals(ctx, scoped, |ctx| {
            typed_expr_to_transition_prop_ctx(&cont_body, ctx)
        })
    });
    ctx.visiting_locals = saved_visiting;
    let body = if self_referential || cyclic {
        transition_prop_from_parts(vec![
            TransitionProp::Unsupported {
                reason: if self_referential {
                    format!("self-referential monadic-bind binder '{binder}' detected")
                } else {
                    format!("cyclic monadic-bind binder '{binder}' detected")
                },
                source_location: None,
            },
            source_relation,
            body_prop,
        ])
    } else {
        transition_prop_from_parts(vec![source_relation, body_prop])
    };
    TransitionProp::Exists {
        binder,
        ty: binder_ty,
        domain: Box::new(domain),
        body: Box::new(body),
    }
}

pub(super) fn inline_resolved_fuzzer_relation_call(
    expr: &TypedExpr,
    output_binding: &LocalBinding,
    ctx: &mut TransitionLoweringContext<'_>,
) -> Option<TransitionProp> {
    let expr_locals = ctx.expr_locals();
    let (resolved_head, applied_args) = flatten_call_head_and_args(expr, &[], &expr_locals)
        .unwrap_or_else(|| (terminal_expression(expr).clone(), Vec::new()));
    let mut local_aliases = BTreeSet::new();
    let resolved = resolve_function_from_expr(
        &resolved_head,
        &ctx.current_module,
        ctx.function_index,
        &expr_locals,
        &mut local_aliases,
    )?;
    if resolved.module_name == STDLIB_FUZZ_MODULE
        || applied_args.len() > resolved.function.arguments.len()
    {
        return None;
    }
    let key = (resolved.module_name.clone(), resolved.function_name.clone());
    if !ctx.visiting_functions.insert(key.clone()) {
        return Some(TransitionProp::Unsupported {
            reason: format!(
                "recursive helper relation detected at {}.{}",
                resolved.module_name, resolved.function_name
            ),
            source_location: None,
        });
    }
    let mut callee_locals = BTreeMap::new();
    for (param, arg) in resolved.function.arguments.iter().zip(applied_args.iter()) {
        if let Some(name) = param.get_variable_name() {
            callee_locals.insert(name.to_string(), binding_from_expr(arg, ctx));
        }
    }
    let inlined = with_current_module(ctx, resolved.module_name.clone(), |ctx| {
        with_scoped_locals(ctx, callee_locals, |ctx| {
            lower_fuzzer_relation_with_locals(&resolved.function.body, output_binding, ctx)
        })
    });
    ctx.visiting_functions.remove(&key);
    Some(inlined)
}

#[derive(Debug, Clone)]
pub(super) struct ResolvedRelationContinuation {
    binder: String,
    binder_ty: ShallowIrType,
    body: TypedExpr,
    module: String,
    locals: BTreeMap<String, LocalBinding>,
}

pub(super) type BindCall<'a> = (&'a TypedExpr, String, &'a TypedExpr);
pub(super) type ResolvedBindCall<'a> = (&'a TypedExpr, ResolvedRelationContinuation);
pub(super) type ForkCall<'a> = Vec<&'a TypedExpr>;

pub(super) fn resolve_bind_call<'a>(
    args: &'a [CallArg<TypedExpr>],
    ctx: &TransitionLoweringContext<'_>,
) -> Option<ResolvedBindCall<'a>> {
    if args.len() != 2 {
        return None;
    }

    let source = &args[0].value;
    if !expression_has_fuzzer_type(source) {
        return None;
    }

    let continuation = resolve_relation_continuation(&args[1].value, ctx)?;
    Some((source, continuation))
}

/// Detect `and_then`/bind-style calls by *shape*:
///   - exactly two arguments,
///   - the first is a `Fuzzer<T>`,
///   - the second is a continuation `T -> Fuzzer<U>`, which we recognize
///     through `expression_is_bind_continuation`.
///
/// Returns `Some((source, binder_name, continuation_body))` when the call
/// matches and the continuation is a `Fn` literal we can peel.
pub(super) fn detect_bind_call(args: &[CallArg<TypedExpr>]) -> Option<BindCall<'_>> {
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

pub(super) fn resolve_relation_continuation(
    continuation: &TypedExpr,
    ctx: &TransitionLoweringContext<'_>,
) -> Option<ResolvedRelationContinuation> {
    if !expression_is_bind_continuation(continuation) {
        return None;
    }

    if let TypedExpr::Fn { args, body, .. } = terminal_expression(continuation) {
        if args.len() == 1 {
            return Some(ResolvedRelationContinuation {
                binder: args[0]
                    .get_variable_name()
                    .unwrap_or("_fork_cont")
                    .to_string(),
                binder_ty: shallow_ir_type(&args[0].tipo),
                body: body.as_ref().clone(),
                module: ctx.current_module.clone(),
                locals: ctx.locals.clone(),
            });
        }
    }

    let (resolved, resolved_locals, applied_arg_count) = resolve_function_with_applied_args(
        continuation,
        &ctx.current_module,
        ctx.function_index,
        &ctx.expr_locals(),
    )?;
    let remaining_args = resolved
        .function
        .arguments
        .len()
        .checked_sub(applied_arg_count)?;
    if remaining_args != 1 {
        return None;
    }

    let binder_arg = &resolved.function.arguments[applied_arg_count];
    Some(ResolvedRelationContinuation {
        binder: binder_arg
            .get_variable_name()
            .unwrap_or("_fork_cont")
            .to_string(),
        binder_ty: shallow_ir_type(&binder_arg.tipo),
        body: resolved.function.body.clone(),
        module: resolved.module_name,
        locals: resolved_locals
            .into_iter()
            .map(|(name, expr)| (name, LocalBinding::PureExpr(expr)))
            .collect(),
    })
}

/// Detect `fork*_and_then`-style calls by *shape*:
///   - at least two arguments that are zero-arg `fn() -> Fuzzer<T>` thunks
///     (the branches).
///
/// Returns the peeled thunk bodies when the call matches. Continuations are
/// resolved separately so named/aliased visible functions can reuse the normal
/// resolver pipeline instead of being silently dropped when they are not inline
/// `fn` literals.
pub(super) fn detect_fork_call(args: &[CallArg<TypedExpr>]) -> Option<ForkCall<'_>> {
    let thunk_bodies: Vec<&TypedExpr> = args
        .iter()
        .filter_map(|a| zero_arg_fuzzer_thunk_body(&a.value))
        .collect();
    (thunk_bodies.len() >= 2).then_some(thunk_bodies)
}

/// If `expr` is an inline `fn() -> Fuzzer<T>` zero-arg thunk, return its
/// body. Otherwise `None`.
pub(super) fn zero_arg_fuzzer_thunk_body(expr: &TypedExpr) -> Option<&TypedExpr> {
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
pub(super) fn types_semantically_equal(a: &Type, b: &Type) -> bool {
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

pub(super) fn detect_audited_return_call<'a>(
    fun: &'a TypedExpr,
    args: &'a [CallArg<TypedExpr>],
    call_tipo: &Type,
    ctx: &TransitionLoweringContext<'_>,
) -> Option<&'a TypedExpr> {
    if !is_known_fuzz_call(fun, ctx, "constant") {
        return None;
    }
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
