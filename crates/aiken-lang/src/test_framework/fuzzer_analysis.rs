use super::analysis_support::*;
use super::shallow_ir::*;
use super::state_machine::*;
use super::transition_prop::*;
use super::*;

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
    pub source_span: Span,
}

/// A property test plus the verifier/export metadata derived during compilation.
#[derive(Debug, Clone)]
pub struct AnalyzedPropertyTest {
    pub test: PropertyTest,
    pub analysis: PropertyTestAnalysis,
}

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
    /// Empty support: the fuzzer never produces a value; lowers to `False` in Lean predicates.
    Empty { reason: String },
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
    IntAffine {
        scale: i8,
        offset: String,
    },
    ConstructorMap(BTreeMap<String, String>),
    ConstructorWrap {
        constructor: String,
        type_name: Option<String>,
    },
    Unknown,
}

#[non_exhaustive]
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum NaryMapperShape {
    ConstructorApply {
        constructor: String,
        type_name: Option<String>,
        arg_order: Vec<usize>,
    },
    Unknown,
}

#[non_exhaustive]
#[derive(Debug, Clone, PartialEq)]
pub enum NormalizedFuzzer {
    Opaque {
        expr: Box<TypedExpr>,
        reason: String,
    },
    Empty {
        output_type: Rc<Type>,
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
    MapN {
        sources: Vec<NormalizedFuzzer>,
        output_type: Rc<Type>,
        mapper_shape: NaryMapperShape,
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
        unique: bool,
        retry_limit: Option<usize>,
    },
    Choice {
        output_type: Rc<Type>,
        branches: Vec<NormalizedFuzzer>,
        may_fail: bool,
        non_empty_required: bool,
    },
    Filter {
        output_type: Rc<Type>,
        source: Box<NormalizedFuzzer>,
        predicate_summary: String,
        predicate_ir: Option<ShallowIr>,
        max_tries: Option<usize>,
        impossible: bool,
    },
    StateMachineTrace {
        acceptance: StateMachineAcceptance,
        output_type: Rc<Type>,
        initial_state: Box<TypedExpr>,
        step_function: Box<TypedExpr>,
    },
}

#[cfg(test)]
pub(super) fn normalize_fuzzer_from_via(
    via: &TypedExpr,
    current_module: &str,
    known_functions: &IndexMap<&FunctionAccessKey, &TypedFunction>,
) -> NormalizedFuzzer {
    normalize_fuzzer_from_via_with_constants(via, current_module, known_functions, &IndexMap::new())
}

pub(super) fn normalize_fuzzer_from_via_with_constants(
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

pub(super) fn opaque_normalized_fuzzer(
    expr: &TypedExpr,
    reason: impl Into<String>,
) -> NormalizedFuzzer {
    NormalizedFuzzer::Opaque {
        expr: Box::new(terminal_expression(expr).clone()),
        reason: reason.into(),
    }
}

pub(super) fn normalize_fuzzer_from_expr(
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

    if let TypedExpr::Fn { args, body, .. } = expr {
        if args.is_empty() && expression_has_fuzzer_type(body.as_ref()) {
            return normalize_fuzzer_from_expr(
                body.as_ref(),
                current_module,
                function_index,
                constant_index,
                local_values,
                visiting_functions,
            );
        }
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

fn normalized_branch_is_empty_support(branch: &NormalizedFuzzer) -> bool {
    matches!(branch, NormalizedFuzzer::Empty { .. })
        || matches!(
            branch,
            NormalizedFuzzer::Opaque { expr, .. }
                if matches!(expr.as_ref(), TypedExpr::ErrorTerm { tipo, .. } if extract_fuzzer_payload_type(tipo.as_ref()).is_some())
        )
}

/// Combine per-branch normalizations from an `if`/`when` scrutinee into a
/// single normalization for the whole expression.
///
/// Failing branches contribute empty support rather than widening the domain.
/// If every branch is empty, the whole control-flow expression has empty
/// support and lowers to `False` in downstream predicates.
pub(super) fn merge_branch_normalizations(
    branches: Vec<NormalizedFuzzer>,
    expr_type: &Type,
) -> NormalizedFuzzer {
    let producing_branches: Vec<NormalizedFuzzer> = branches
        .iter()
        .filter(|branch| !normalized_branch_is_empty_support(branch))
        .cloned()
        .collect();

    if producing_branches.is_empty() {
        let Some(output_type) = extract_fuzzer_payload_type(expr_type) else {
            return opaque_normalized_fuzzer(
                &TypedExpr::ErrorTerm {
                    location: Span::empty(),
                    tipo: Rc::new(expr_type.clone()),
                },
                "control-flow expression does not have Fuzzer type",
            );
        };

        return NormalizedFuzzer::Empty {
            output_type,
            reason: "control-flow fuzzer has no producing branches".to_string(),
        };
    }

    if let Some(first) = producing_branches.first() {
        if producing_branches.iter().all(|branch| branch == first) {
            return first.clone();
        }
    }

    let Some(output_type) = extract_fuzzer_payload_type(expr_type) else {
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
pub(super) fn primitive_from_fuzzer_expr(expr: &TypedExpr) -> NormalizedFuzzer {
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

pub(super) fn normalize_fuzzer_from_sequence(
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

pub(super) fn normalize_state_machine_trace_from_expr(
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

pub(super) fn normalize_state_machine_trace_from_call(
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
pub(super) fn normalize_fuzzer_from_call(
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
                        let (unique, min_len, max_len, retry_limit) =
                            stdlib_collection_info(expr, args, constant_index, local_values)
                                .unwrap_or((false, None, None, None));
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
                            unique,
                            retry_limit,
                        };
                    }
                }
            }
        }
    }

    // STDLIB FILTER SHORTCUT: `fuzz.such_that(source, predicate)` keeps the
    // source generator's support but adds bounded retry behavior. Preserve it
    // as a dedicated filter node so downstream verification can classify it
    // conservatively instead of silently promoting the base domain to exactness.
    if let Some((module, fn_name)) = extract_module_fn_identity(fun)
        && module == STDLIB_FUZZ_MODULE
        && fn_name == "such_that"
        && let [source_arg, predicate_arg] = args
        && expression_has_fuzzer_type(&source_arg.value)
        && let Some(output_type) = extract_fuzzer_payload_type(expr.tipo().as_ref())
    {
        let source = normalize_fuzzer_from_expr(
            &source_arg.value,
            current_module,
            function_index,
            constant_index,
            local_values,
            visiting_functions,
        );
        let (predicate_summary, impossible) = summarize_filter_predicate(
            &predicate_arg.value,
            source_arg.value.tipo().as_ref(),
            current_module,
            function_index,
            local_values,
        );
        let predicate_ir = lower_filter_predicate_ir(
            &predicate_arg.value,
            source_arg.value.tipo().as_ref(),
            current_module,
            function_index,
            local_values,
        );
        return NormalizedFuzzer::Filter {
            output_type,
            source: Box::new(source),
            predicate_summary,
            predicate_ir,
            max_tries: Some(100),
            impossible,
        };
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
pub(super) fn helper_body_is_control_flow_shaped(
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

pub(super) fn trusted_stdlib_int_range_constraint() -> FuzzerConstraint {
    FuzzerConstraint::IntRange {
        min: (-255).to_string(),
        max: 16_383.to_string(),
    }
}

pub(super) fn trusted_stdlib_byte_range_constraint() -> FuzzerConstraint {
    FuzzerConstraint::IntRange {
        min: 0.to_string(),
        max: 255.to_string(),
    }
}

pub(super) fn trusted_stdlib_int_at_least_constraint(min: BigInt) -> FuzzerConstraint {
    let max_rand = BigInt::from(255);
    let abs = if min < BigInt::from(0) {
        -min.clone()
    } else {
        min.clone()
    };
    let max = if abs <= max_rand {
        max_rand
    } else {
        &min + BigInt::from(5) * abs
    };

    FuzzerConstraint::IntRange {
        min: min.to_string(),
        max: max.to_string(),
    }
}

pub(super) fn trusted_stdlib_int_at_most_constraint(max: BigInt) -> FuzzerConstraint {
    let max_rand = BigInt::from(255);
    let abs = if max < BigInt::from(0) {
        -max.clone()
    } else {
        max.clone()
    };
    let min = if abs <= max_rand {
        -max_rand
    } else {
        &max - BigInt::from(5) * abs
    };

    FuzzerConstraint::IntRange {
        min: min.to_string(),
        max: max.to_string(),
    }
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
pub(super) fn try_extract_primitive_constraint_structurally(
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
        ("bool", []) => Some(FuzzerConstraint::OneOf(vec![
            FuzzerExactValue::Bool(false),
            FuzzerExactValue::Bool(true),
        ])),
        ("byte", []) => Some(trusted_stdlib_byte_range_constraint()),
        ("int", []) => Some(trusted_stdlib_int_range_constraint()),
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
            Some(trusted_stdlib_int_at_least_constraint(min))
        }
        ("int_at_most", [max_arg]) => {
            let max = try_extract_int_literal(&max_arg.value, constant_index, local_values)?;
            Some(trusted_stdlib_int_at_most_constraint(max))
        }
        ("constant", [value_arg]) => {
            if let Some(value) = try_extract_exact_scalar(&value_arg.value) {
                return Some(FuzzerConstraint::Exact(value));
            }

            if value_arg.value.tipo().is_int() {
                let v = try_extract_int_literal(&value_arg.value, constant_index, local_values)?;
                return Some(FuzzerConstraint::IntRange {
                    min: v.to_string(),
                    max: v.to_string(),
                });
            }

            None
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
        ("bytearray_fixed", [len_arg]) => {
            let len = extract_bytearray_len(&len_arg.value, constant_index, local_values)?;
            Some(FuzzerConstraint::ByteStringLenRange {
                min_len: len,
                max_len: len,
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
pub(super) fn extract_bytearray_len(
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
pub(super) fn extract_module_fn_identity(fun: &TypedExpr) -> Option<(String, String)> {
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
pub(super) fn try_extract_int_literal(
    expr: &TypedExpr,
    constant_index: &ConstantIndex<'_>,
    local_values: &BTreeMap<String, TypedExpr>,
) -> Option<BigInt> {
    try_extract_int_literal_inner(expr, constant_index, local_values, 0)
}

pub(super) const INT_LITERAL_MAX_DEPTH: u8 = 16;

pub(super) fn try_extract_int_literal_inner(
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
pub(super) fn try_extract_exact_scalar(expr: &TypedExpr) -> Option<FuzzerExactValue> {
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
pub(super) enum IntBoundSide {
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
pub(super) fn unbounded_int_sentinel_to_none(bound: &str, side: IntBoundSide) -> Option<String> {
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
pub(super) fn semantics_from_known_constraint(
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

/// Try to extract collection length bounds structurally for stdlib list/set fuzzers.
pub(super) fn try_extract_list_length_bounds(
    expr: &TypedExpr,
    args: &[CallArg<TypedExpr>],
    constant_index: &ConstantIndex<'_>,
    local_values: &BTreeMap<String, TypedExpr>,
) -> (Option<usize>, Option<usize>) {
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
        extract_bytearray_len(&arg.value, constant_index, local_values)
    };

    match (fn_name.as_str(), args) {
        ("list", [_elem]) | ("set", [_elem]) => (Some(0), Some(20)),
        ("list_between", [_elem, min_arg, max_arg])
        | ("set_between", [_elem, min_arg, max_arg]) => {
            let min = extract_len(min_arg);
            let max = extract_len(max_arg);
            match (min, max) {
                (Some(a), Some(b)) if a > b => (Some(b), Some(a)),
                _ => (min, max),
            }
        }
        ("list_at_least", [_elem, min_arg]) | ("set_at_least", [_elem, min_arg]) => {
            let min = extract_len(min_arg);
            let max = min.and_then(|min| min.checked_add(20));
            (min, max)
        }
        ("list_at_most", [_elem, max_arg]) | ("set_at_most", [_elem, max_arg]) => {
            (Some(0), extract_len(max_arg))
        }
        _ => (None, None),
    }
}

pub(super) fn stdlib_collection_info(
    expr: &TypedExpr,
    args: &[CallArg<TypedExpr>],
    constant_index: &ConstantIndex<'_>,
    local_values: &BTreeMap<String, TypedExpr>,
) -> Option<(bool, Option<usize>, Option<usize>, Option<usize>)> {
    let TypedExpr::Call { fun, .. } = expr else {
        return None;
    };
    let (module, fn_name) = extract_module_fn_identity(fun)?;
    if module != STDLIB_FUZZ_MODULE {
        return None;
    }

    let (min_len, max_len) =
        try_extract_list_length_bounds(expr, args, constant_index, local_values);
    match fn_name.as_str() {
        "list" | "list_between" | "list_at_least" | "list_at_most" => {
            Some((false, min_len, max_len, None))
        }
        "set" | "set_between" | "set_at_least" | "set_at_most" => {
            Some((true, min_len, max_len, Some(100)))
        }
        _ => None,
    }
}

pub(super) fn normalize_choice_branches(
    args: &[CallArg<TypedExpr>],
    current_module: &str,
    function_index: &FunctionIndex<'_>,
    constant_index: &ConstantIndex<'_>,
    local_values: &BTreeMap<String, TypedExpr>,
    visiting_functions: &mut BTreeSet<(String, String)>,
) -> Vec<NormalizedFuzzer> {
    args.iter()
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
        .collect()
}

pub(super) fn choice_exact_scalar_branch(
    output_type: &Type,
    expr: &TypedExpr,
) -> Option<NormalizedFuzzer> {
    let exact = try_extract_exact_scalar(expr)?;
    Some(NormalizedFuzzer::Primitive {
        output_type: Rc::new(output_type.clone()),
        known_constraint: Some(FuzzerConstraint::Exact(exact)),
    })
}

pub(super) fn summarize_filter_predicate(
    predicate: &TypedExpr,
    source_output_type: &Type,
    current_module: &str,
    function_index: &FunctionIndex<'_>,
    local_values: &BTreeMap<String, TypedExpr>,
) -> (String, bool) {
    let output_type = extract_fuzzer_payload_type(source_output_type)
        .unwrap_or_else(|| Rc::new(source_output_type.clone()));
    match summarize_unary_mapper_shape(predicate, current_module, function_index, local_values) {
        UnaryMapperShape::ConstBool(true) => return ("always_true".to_string(), false),
        UnaryMapperShape::ConstBool(false) => return ("always_false".to_string(), true),
        UnaryMapperShape::Identity => {
            return (
                format!(
                    "predicate preserves {} truthiness",
                    pretty_print_type(output_type.as_ref())
                ),
                false,
            );
        }
        _ => {}
    }

    (
        "predicate not lowered; relation records only the base domain".to_string(),
        false,
    )
}

fn lower_filter_predicate_ir(
    predicate: &TypedExpr,
    source_output_type: &Type,
    current_module: &str,
    function_index: &FunctionIndex<'_>,
    local_values: &BTreeMap<String, TypedExpr>,
) -> Option<ShallowIr> {
    let output_type = extract_fuzzer_payload_type(source_output_type)
        .unwrap_or_else(|| Rc::new(source_output_type.clone()));
    if !output_type.is_bool() {
        return None;
    }

    match summarize_unary_mapper_shape(predicate, current_module, function_index, local_values) {
        UnaryMapperShape::Identity => Some(ShallowIr::BoundVar {
            name: "_filter_value".to_string(),
            ty: ShallowIrType::Bool,
        }),
        UnaryMapperShape::ConstBool(value) => Some(ShallowIr::Const(ShallowConst::Bool(value))),
        _ => None,
    }
}

pub(super) fn normalize_structural_fuzzer_call(
    expr: &TypedExpr,
    args: &[CallArg<TypedExpr>],
    current_module: &str,
    function_index: &FunctionIndex<'_>,
    constant_index: &ConstantIndex<'_>,
    local_values: &BTreeMap<String, TypedExpr>,
    visiting_functions: &mut BTreeSet<(String, String)>,
) -> Option<NormalizedFuzzer> {
    let stdlib_call = match expr {
        TypedExpr::Call { fun, .. } => extract_module_fn_identity(fun.as_ref()),
        _ => None,
    };

    if let Some((module, fn_name)) = stdlib_call.as_ref()
        && module == STDLIB_FUZZ_MODULE
    {
        match fn_name.as_str() {
            "such_that" if args.len() == 2 => {
                let source_arg = &args[0];
                let predicate_arg = &args[1];
                if expression_has_fuzzer_type(&source_arg.value) {
                    let source = normalize_fuzzer_from_expr(
                        &source_arg.value,
                        current_module,
                        function_index,
                        constant_index,
                        local_values,
                        visiting_functions,
                    );
                    let output_type = extract_fuzzer_payload_type(expr.tipo().as_ref())?;
                    let (predicate_summary, impossible) = summarize_filter_predicate(
                        &predicate_arg.value,
                        source_arg.value.tipo().as_ref(),
                        current_module,
                        function_index,
                        local_values,
                    );
                    let predicate_ir = lower_filter_predicate_ir(
                        &predicate_arg.value,
                        source_arg.value.tipo().as_ref(),
                        current_module,
                        function_index,
                        local_values,
                    );
                    return Some(NormalizedFuzzer::Filter {
                        output_type,
                        source: Box::new(source),
                        predicate_summary,
                        predicate_ir,
                        max_tries: Some(100),
                        impossible,
                    });
                }
            }
            "option" if args.len() == 1 => {
                let source_arg = &args[0];
                if expression_has_fuzzer_type(&source_arg.value) {
                    let source_output_type =
                        extract_fuzzer_payload_type(source_arg.value.tipo().as_ref())?;
                    let output_type = extract_fuzzer_payload_type(expr.tipo().as_ref())?;
                    let some_branch = NormalizedFuzzer::Map {
                        source: Box::new(normalize_fuzzer_from_expr(
                            &source_arg.value,
                            current_module,
                            function_index,
                            constant_index,
                            local_values,
                            visiting_functions,
                        )),
                        source_output_type,
                        output_type: output_type.clone(),
                        mapper_shape: UnaryMapperShape::ConstructorWrap {
                            constructor: "Some".to_string(),
                            type_name: data_with_schema_type_name(output_type.as_ref()),
                        },
                    };
                    let none_branch = NormalizedFuzzer::Primitive {
                        output_type: output_type.clone(),
                        known_constraint: None,
                    };
                    return Some(NormalizedFuzzer::Choice {
                        output_type,
                        branches: vec![some_branch, none_branch],
                        may_fail: false,
                        non_empty_required: false,
                    });
                }
            }
            "either" | "either3" | "either4" | "either5" | "either6" | "either7" | "either8"
            | "either9"
                if args
                    .iter()
                    .all(|arg| expression_has_fuzzer_type(&arg.value)) =>
            {
                let output_type = extract_fuzzer_payload_type(expr.tipo().as_ref())?;
                return Some(NormalizedFuzzer::Choice {
                    output_type,
                    branches: normalize_choice_branches(
                        args,
                        current_module,
                        function_index,
                        constant_index,
                        local_values,
                        visiting_functions,
                    ),
                    may_fail: false,
                    non_empty_required: false,
                });
            }
            "one_of" if args.len() == 1 => {
                let output_type = extract_fuzzer_payload_type(expr.tipo().as_ref())?;
                if let TypedExpr::List { elements, .. } = terminal_expression(&args[0].value) {
                    if elements.is_empty() {
                        return Some(NormalizedFuzzer::Choice {
                            output_type,
                            branches: Vec::new(),
                            may_fail: true,
                            non_empty_required: true,
                        });
                    }

                    let mut branches = Vec::new();
                    let mut all_exact = true;
                    for element in elements {
                        if let Some(branch) =
                            choice_exact_scalar_branch(output_type.as_ref(), element)
                        {
                            branches.push(branch);
                        } else {
                            all_exact = false;
                            break;
                        }
                    }

                    if all_exact {
                        return Some(NormalizedFuzzer::Choice {
                            output_type,
                            branches,
                            may_fail: false,
                            non_empty_required: true,
                        });
                    }
                }
            }
            _ => {}
        }
    }

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
                let source = normalize_fuzzer_from_expr(
                    &source.value,
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

    if args.len() >= 3 {
        let arity = args.len() - 1;
        let sources = &args[..arity];
        let mapper = &args[arity].value;

        if let Some((module, fn_name)) = stdlib_call.as_ref()
            && module == STDLIB_FUZZ_MODULE
            && matches!(
                fn_name.as_str(),
                "map2" | "map3" | "map4" | "map5" | "map6" | "map7" | "map8" | "map9"
            )
            && sources
                .iter()
                .all(|arg| expression_has_fuzzer_type(&arg.value))
            && let Some(mapper_shape) = summarize_nary_mapper_shape(
                mapper,
                arity,
                current_module,
                function_index,
                local_values,
            )
        {
            let output_type = extract_fuzzer_payload_type(expr.tipo().as_ref())?;
            return Some(NormalizedFuzzer::MapN {
                sources: normalize_choice_branches(
                    sources,
                    current_module,
                    function_index,
                    constant_index,
                    local_values,
                    visiting_functions,
                ),
                output_type,
                mapper_shape,
            });
        }
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
                    && types_semantically_equal(
                        source_output_type.as_ref(),
                        inner_types[0].as_ref(),
                    )
                {
                    let (unique, min_len, max_len, retry_limit) =
                        stdlib_collection_info(expr, args, constant_index, local_values)
                            .unwrap_or((false, None, None, None));
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
                        unique,
                        retry_limit,
                    });
                }
            }
        }
    }

    None
}

pub(super) fn normalize_fuzzer_from_continuation(
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

            let normalized_body =
                peel_bound_fuzzer_lambda_body(&resolved.function.body, &resolved_locals);
            let normalized_body = match terminal_expression(normalized_body) {
                TypedExpr::Fn { args, body, .. } if args.len() == 1 => body.as_ref(),
                _ => normalized_body,
            };
            let result = normalize_fuzzer_from_expr(
                normalized_body,
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

pub(super) fn peel_bound_fuzzer_lambda_body<'a>(
    body: &'a TypedExpr,
    local_values: &BTreeMap<String, TypedExpr>,
) -> &'a TypedExpr {
    let mut current = body;
    loop {
        let TypedExpr::Fn { args, body, .. } = terminal_expression(current) else {
            return current;
        };
        if args.is_empty()
            || !args.iter().all(|arg| {
                arg.get_variable_name()
                    .is_some_and(|name| local_values.contains_key(name))
            })
        {
            return current;
        }
        current = body.as_ref();
    }
}
pub(super) fn normalize_fuzzer_from_helper_call(
    fun: &TypedExpr,
    args: &[CallArg<TypedExpr>],
    current_module: &str,
    function_index: &FunctionIndex<'_>,
    constant_index: &ConstantIndex<'_>,
    local_values: &BTreeMap<String, TypedExpr>,
    visiting_functions: &mut BTreeSet<(String, String)>,
) -> Option<NormalizedFuzzer> {
    let (resolved, mut helper_locals, applied_arg_count) =
        resolve_function_with_applied_args(fun, current_module, function_index, local_values)?;

    if applied_arg_count + args.len() > resolved.function.arguments.len() {
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

    for (param, arg) in resolved.function.arguments[applied_arg_count..]
        .iter()
        .zip(args.iter())
    {
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

    let normalized_body = peel_bound_fuzzer_lambda_body(&resolved.function.body, &helper_locals);
    let result = normalize_fuzzer_from_expr(
        normalized_body,
        &resolved.module_name,
        function_index,
        constant_index,
        &helper_locals,
        visiting_functions,
    );
    visiting_functions.remove(&key);
    Some(result)
}

pub(super) fn normalize_fuzzer_from_resolved_function(
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

    let normalized_body = peel_bound_fuzzer_lambda_body(&resolved.function.body, &resolved_locals);
    let result = normalize_fuzzer_from_expr(
        normalized_body,
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
pub(super) fn try_beta_reduce_fuzzer_call(
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

pub(super) fn expression_has_fuzzer_type(expr: &TypedExpr) -> bool {
    extract_fuzzer_payload_type(expr.tipo().as_ref()).is_some()
}

pub(super) fn expression_is_pure_mapper(expr: &TypedExpr) -> bool {
    !expression_has_fuzzer_type(expr)
        && function_return_type(expr).is_some_and(|(args, ret)| {
            args.len() == 1 && extract_fuzzer_payload_type(ret.as_ref()).is_none()
        })
}

pub(super) fn summarize_unary_mapper_shape(
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

pub(super) fn summarize_unary_mapper_body(
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
    if let Some((constructor, type_name)) =
        resolve_unary_constructor_wrap(body, arg_name, local_values, &mut visiting_local_aliases)
    {
        return UnaryMapperShape::ConstructorWrap {
            constructor,
            type_name,
        };
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

pub(super) fn resolve_identity_mapper(
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

pub(super) fn resolve_exact_constant_mapper(
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

pub(super) fn resolve_unary_constructor_wrap(
    expr: &TypedExpr,
    arg_name: &str,
    local_values: &BTreeMap<String, TypedExpr>,
    visiting_local_aliases: &mut BTreeSet<String>,
) -> Option<(String, Option<String>)> {
    let TypedExpr::Call { fun, args, .. } = terminal_expression(expr) else {
        return None;
    };
    let [arg] = args.as_slice() else {
        return None;
    };
    if !expression_resolves_to_local_name(
        &arg.value,
        arg_name,
        local_values,
        visiting_local_aliases,
    ) {
        return None;
    }

    let constructor = match terminal_expression(fun.as_ref()) {
        TypedExpr::Var {
            name, constructor, ..
        } => match &constructor.variant {
            ValueConstructorVariant::Record { arity, .. } if *arity == 1 => Some(name.clone()),
            _ => None,
        },
        TypedExpr::ModuleSelect {
            label, constructor, ..
        } => match constructor {
            ModuleValueConstructor::Record { arity, .. } if *arity == 1 => Some(label.clone()),
            _ => None,
        },
        _ => None,
    }?;

    Some((
        constructor,
        data_with_schema_type_name(expr.tipo().as_ref()),
    ))
}

pub(super) fn resolve_tautological_bool_mapper(
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

pub(super) fn resolve_nullary_constructor_mapper(
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

pub(super) fn expression_resolves_to_local_name(
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

pub(super) fn nullary_constructor_pattern_name(pattern: &TypedPattern) -> Option<String> {
    match pattern {
        TypedPattern::Assign { pattern, .. } => nullary_constructor_pattern_name(pattern.as_ref()),
        TypedPattern::Constructor {
            name, arguments, ..
        } if arguments.is_empty() => Some(name.clone()),
        _ => None,
    }
}

pub(super) fn resolve_nullary_constructor_value_name(
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

pub(super) fn resolve_int_affine_mapper(
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
pub(super) enum FiniteEvalValue {
    Int(BigInt),
    Bool(bool),
    ByteArray(Vec<u8>),
    String(String),
}

pub(super) fn normalized_int_range(normalized: &NormalizedFuzzer) -> Option<(String, String)> {
    match normalized {
        NormalizedFuzzer::Primitive {
            known_constraint: Some(FuzzerConstraint::IntRange { min, max }),
            ..
        } => Some((min.clone(), max.clone())),
        _ => None,
    }
}

pub(super) fn enumerate_capped_int_range(min: &str, max: &str, cap: usize) -> Option<Vec<BigInt>> {
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

pub(super) fn summarize_finite_scalar_mapper_shape(
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

pub(super) fn evaluate_finite_scalar_mapper_body(
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

pub(super) fn eval_finite_mapper_expr(
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

pub(super) fn eval_finite_mapper_sequence(
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

pub(super) fn eval_finite_mapper_bin_op(
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

pub(super) fn finite_eval_value_matches_pattern(
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

pub(super) fn parse_uint_bigint(value: &str, base: &Base) -> Option<BigInt> {
    let digits = value.replace('_', "");
    let radix = match base {
        Base::Decimal { .. } => 10,
        Base::Hexadecimal => 16,
    };

    BigInt::parse_bytes(digits.as_bytes(), radix)
}

pub(super) fn expression_is_bind_continuation(expr: &TypedExpr) -> bool {
    !expression_has_fuzzer_type(expr)
        && function_return_type(expr).is_some_and(|(args, ret)| {
            args.len() == 1 && extract_fuzzer_payload_type(ret.as_ref()).is_some()
        })
}

pub(super) fn function_return_type(expr: &TypedExpr) -> Option<(Vec<Rc<Type>>, Rc<Type>)> {
    function_signature(expr.tipo().as_ref())
}

pub(super) fn function_signature(tipo: &Type) -> Option<(Vec<Rc<Type>>, Rc<Type>)> {
    match tipo {
        Type::Fn { args, ret, .. } => Some((args.clone(), ret.clone())),
        Type::Var { tipo, .. } => match tipo.as_ref().borrow().deref() {
            TypeVar::Link { tipo } => function_signature(tipo.as_ref()),
            _ => None,
        },
        _ => None,
    }
}

pub(super) fn nullary_constructor_tags_for_type(
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

pub(super) fn pushforward_nullary_constructor_tags(
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

pub(super) fn parse_decimal_bigint(value: &str) -> Option<BigInt> {
    value.parse::<BigInt>().ok()
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(super) enum FiniteDomainError {
    Empty,
    Heterogeneous,
    OutputTypeMismatch,
    TooLarge,
}

#[derive(Debug, Clone, PartialEq)]
pub(super) enum CanonicalFiniteScalarDomain {
    Exact(FuzzerExactValue),
    OneOf(Vec<FuzzerExactValue>),
}

pub(super) fn exact_value_matches_output_type(
    output_type: &Type,
    value: &FuzzerExactValue,
) -> bool {
    match value {
        FuzzerExactValue::Bool(_) => output_type.is_bool(),
        FuzzerExactValue::ByteArray(_) => output_type.is_bytearray(),
        FuzzerExactValue::String(_) => output_type.is_string(),
    }
}

pub(super) fn exact_value_kind(value: &FuzzerExactValue) -> u8 {
    match value {
        FuzzerExactValue::Bool(_) => 0,
        FuzzerExactValue::ByteArray(_) => 1,
        FuzzerExactValue::String(_) => 2,
    }
}

pub(super) fn compare_exact_values(a: &FuzzerExactValue, b: &FuzzerExactValue) -> Ordering {
    match (a, b) {
        (FuzzerExactValue::Bool(a), FuzzerExactValue::Bool(b)) => a.cmp(b),
        (FuzzerExactValue::ByteArray(a), FuzzerExactValue::ByteArray(b)) => a.cmp(b),
        (FuzzerExactValue::String(a), FuzzerExactValue::String(b)) => {
            a.as_bytes().cmp(b.as_bytes())
        }
        _ => exact_value_kind(a).cmp(&exact_value_kind(b)),
    }
}

pub(super) fn canonicalize_finite_scalar_domain(
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

pub(super) fn merge_choice_constraints(
    output_type: &Type,
    constraints: &[FuzzerConstraint],
) -> Option<FuzzerConstraint> {
    if constraints.is_empty() {
        return None;
    }

    let mut scalar_values = Vec::new();
    let mut all_scalar = true;
    for constraint in constraints {
        match constraint {
            FuzzerConstraint::Exact(value) => scalar_values.push(value.clone()),
            FuzzerConstraint::OneOf(values) => scalar_values.extend(values.clone()),
            _ => {
                all_scalar = false;
                break;
            }
        }
    }
    if all_scalar {
        return match canonicalize_finite_scalar_domain(output_type, scalar_values).ok()? {
            CanonicalFiniteScalarDomain::Exact(value) => Some(FuzzerConstraint::Exact(value)),
            CanonicalFiniteScalarDomain::OneOf(values) => Some(FuzzerConstraint::OneOf(values)),
        };
    }

    let mut constructor_tags = Vec::new();
    let mut all_constructors = true;
    for constraint in constraints {
        match constraint {
            FuzzerConstraint::DataConstructorTags { tags } => {
                constructor_tags.extend(tags.iter().copied())
            }
            _ => {
                all_constructors = false;
                break;
            }
        }
    }
    if all_constructors {
        constructor_tags.sort_unstable();
        constructor_tags.dedup();
        return Some(FuzzerConstraint::DataConstructorTags {
            tags: constructor_tags,
        });
    }

    None
}

pub(super) fn merge_choice_semantics(
    output_type: &Type,
    semantics: &[FuzzerSemantics],
    data_types: &IndexMap<&DataTypeKey, &TypedDataType>,
) -> Option<FuzzerSemantics> {
    if semantics.is_empty() {
        return None;
    }

    let mut scalar_values = Vec::new();
    let mut all_scalar = true;
    for semantic in semantics {
        match semantic {
            FuzzerSemantics::Exact(value) => scalar_values.push(value.clone()),
            FuzzerSemantics::OneOf(values) => scalar_values.extend(values.clone()),
            _ => {
                all_scalar = false;
                break;
            }
        }
    }
    if all_scalar {
        return match canonicalize_finite_scalar_domain(output_type, scalar_values).ok()? {
            CanonicalFiniteScalarDomain::Exact(value) => Some(FuzzerSemantics::Exact(value)),
            CanonicalFiniteScalarDomain::OneOf(values) => Some(FuzzerSemantics::OneOf(values)),
        };
    }

    let mut constructor_tags = Vec::new();
    let mut all_constructors = true;
    for semantic in semantics {
        match semantic {
            FuzzerSemantics::Constructors { tags } => constructor_tags.extend(tags.iter().copied()),
            _ => {
                all_constructors = false;
                break;
            }
        }
    }
    if all_constructors {
        constructor_tags.sort_unstable();
        constructor_tags.dedup();
        return Some(FuzzerSemantics::Constructors {
            tags: constructor_tags,
        });
    }

    if output_type.is_bool() {
        return Some(FuzzerSemantics::Bool);
    }
    if output_type.is_int() {
        return Some(FuzzerSemantics::IntRange {
            min: None,
            max: None,
        });
    }
    if output_type.is_bytearray() {
        return Some(FuzzerSemantics::ByteArrayRange {
            min_len: None,
            max_len: None,
        });
    }
    if output_type.is_string() {
        return Some(FuzzerSemantics::String);
    }
    if output_type.is_list()
        || output_type.is_tuple()
        || output_type.is_pair()
        || output_type.is_data()
    {
        return Some(default_semantics_for_type(output_type, data_types));
    }
    data_with_schema_type_name(output_type)
        .map(|type_name| FuzzerSemantics::DataWithSchema { type_name })
}

pub(super) fn merge_choice_semantics_lightweight(
    output_type: &Type,
    semantics: &[FuzzerSemantics],
) -> Option<FuzzerSemantics> {
    if semantics.is_empty() {
        return None;
    }

    let mut scalar_values = Vec::new();
    let mut all_scalar = true;
    for semantic in semantics {
        match semantic {
            FuzzerSemantics::Exact(value) => scalar_values.push(value.clone()),
            FuzzerSemantics::OneOf(values) => scalar_values.extend(values.clone()),
            _ => {
                all_scalar = false;
                break;
            }
        }
    }
    if all_scalar {
        return match canonicalize_finite_scalar_domain(output_type, scalar_values).ok()? {
            CanonicalFiniteScalarDomain::Exact(value) => Some(FuzzerSemantics::Exact(value)),
            CanonicalFiniteScalarDomain::OneOf(values) => Some(FuzzerSemantics::OneOf(values)),
        };
    }

    let mut constructor_tags = Vec::new();
    let mut all_constructors = true;
    for semantic in semantics {
        match semantic {
            FuzzerSemantics::Constructors { tags } => constructor_tags.extend(tags.iter().copied()),
            _ => {
                all_constructors = false;
                break;
            }
        }
    }
    if all_constructors {
        constructor_tags.sort_unstable();
        constructor_tags.dedup();
        return Some(FuzzerSemantics::Constructors {
            tags: constructor_tags,
        });
    }

    None
}

pub(super) fn apply_unary_map_constraint_precision(
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
        UnaryMapperShape::ConstructorWrap { .. } | UnaryMapperShape::Unknown => {
            FuzzerConstraint::Map(Box::new(source_constraint))
        }
    }
}

pub(super) fn apply_int_affine_constraint(
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

pub(super) fn apply_unary_map_semantics_precision(
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

            default_semantics_for_type(output_type, data_types)
        }
        UnaryMapperShape::ConstructorWrap { .. } | UnaryMapperShape::Unknown => {
            default_semantics_for_type(output_type, data_types)
        }
    }
}

pub(super) fn apply_int_affine_semantics(
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

pub(super) fn apply_int_affine_bound(
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
pub(super) fn normalized_fuzzer_constraint(
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
        NormalizedFuzzer::Empty { reason, .. } => FuzzerConstraint::Empty {
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
        NormalizedFuzzer::MapN { .. } => FuzzerConstraint::Any,
        NormalizedFuzzer::Bind { source: _, result } => normalized_fuzzer_constraint(
            result,
            current_module,
            function_index,
            constant_index,
            data_types,
            local_values,
            visiting_functions,
        ),
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
            ..
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
        NormalizedFuzzer::Choice {
            output_type,
            branches,
            may_fail,
            non_empty_required,
        } => {
            if branches.is_empty() && *non_empty_required {
                FuzzerConstraint::Unsupported {
                    reason: "choice combinator has no branches and therefore always fails"
                        .to_string(),
                }
            } else {
                let constraints = branches
                    .iter()
                    .map(|branch| {
                        normalized_fuzzer_constraint(
                            branch,
                            current_module,
                            function_index,
                            constant_index,
                            data_types,
                            local_values,
                            visiting_functions,
                        )
                    })
                    .collect::<Vec<_>>();
                merge_choice_constraints(output_type.as_ref(), &constraints).unwrap_or_else(|| {
                    if *may_fail {
                        FuzzerConstraint::Unsupported {
                            reason: "choice combinator may fail and the current constraint IR cannot model its exact support".to_string(),
                        }
                    } else {
                        FuzzerConstraint::Any
                    }
                })
            }
        }
        NormalizedFuzzer::Filter {
            source, impossible, ..
        } => {
            if *impossible {
                FuzzerConstraint::Unsupported {
                    reason: "such_that predicate is impossible for all generated values"
                        .to_string(),
                }
            } else {
                normalized_fuzzer_constraint(
                    source,
                    current_module,
                    function_index,
                    constant_index,
                    data_types,
                    local_values,
                    visiting_functions,
                )
            }
        }
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
            constant_index,
            visiting_functions,
        ) {
            Some(FuzzerSemantics::StateMachineTrace {
                output_semantics, ..
            }) => semantics_to_constraint(output_semantics.as_ref()),
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
pub(super) fn typed_expr_constructor_tag(
    expr: &TypedExpr,
    data_types: &IndexMap<&DataTypeKey, &TypedDataType>,
) -> Option<u64> {
    let container_tipo = expr.tipo();
    match terminal_expression(expr) {
        TypedExpr::Var { constructor, .. } => match &constructor.variant {
            ValueConstructorVariant::Record { name, .. } => {
                resolve_constructor_tag(&container_tipo, name, data_types)
            }
            _ => None,
        },
        TypedExpr::ModuleSelect { constructor, .. } => match constructor {
            ModuleValueConstructor::Record { name, .. } => {
                resolve_constructor_tag(&container_tipo, name, data_types)
            }
            _ => None,
        },
        TypedExpr::Call { fun, .. } => match terminal_expression(fun.as_ref()) {
            TypedExpr::Var { constructor, .. } => match &constructor.variant {
                ValueConstructorVariant::Record { name, .. } => {
                    resolve_constructor_tag(&container_tipo, name, data_types)
                }
                _ => None,
            },
            TypedExpr::ModuleSelect { constructor, .. } => match constructor {
                ModuleValueConstructor::Record { name, .. } => {
                    resolve_constructor_tag(&container_tipo, name, data_types)
                }
                _ => None,
            },
            _ => None,
        },
        _ => None,
    }
}

pub(super) fn recover_constructor_choice_semantics(
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

pub(super) fn normalized_fuzzer_semantics(
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
        NormalizedFuzzer::Opaque { expr, reason } => {
            { recover_constructor_choice_semantics(expr.as_ref(), data_types) }
                .unwrap_or_else(|| opaque_semantics(reason.clone()))
        }
        NormalizedFuzzer::Empty { output_type, .. } => {
            default_semantics_for_type(output_type.as_ref(), data_types)
        }
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
        NormalizedFuzzer::MapN { output_type, .. } => {
            default_semantics_for_type(output_type.as_ref(), data_types)
        }
        NormalizedFuzzer::Bind { source: _, result } => normalized_fuzzer_semantics(
            result,
            current_module,
            function_index,
            constant_index,
            data_types,
            output_type,
            local_values,
            visiting_functions,
        ),
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
                        let semantics = normalized_fuzzer_semantics(
                            element,
                            current_module,
                            function_index,
                            constant_index,
                            data_types,
                            inner_type.as_ref(),
                            local_values,
                            visiting_functions,
                        );
                        if matches!(semantics, FuzzerSemantics::Opaque { .. }) {
                            default_semantics_for_type(inner_type.as_ref(), data_types)
                        } else {
                            semantics
                        }
                    })
                    .collect(),
            )
        }
        NormalizedFuzzer::List {
            element,
            min_len,
            max_len,
            ..
        } => {
            let inner_types = output_type.get_inner_types();
            if !(output_type.is_list() && inner_types.len() == 1) {
                return opaque_semantics(format!(
                    "list normalization does not match output type '{}'",
                    pretty_print_type(output_type)
                ));
            }

            let element_semantics = normalized_fuzzer_semantics(
                element,
                current_module,
                function_index,
                constant_index,
                data_types,
                inner_types[0].as_ref(),
                local_values,
                visiting_functions,
            );

            FuzzerSemantics::List {
                element: Box::new(
                    if matches!(element_semantics, FuzzerSemantics::Opaque { .. }) {
                        default_semantics_for_type(inner_types[0].as_ref(), data_types)
                    } else {
                        element_semantics
                    },
                ),
                min_len: *min_len,
                max_len: *max_len,
            }
        }
        NormalizedFuzzer::Choice {
            output_type,
            branches,
            may_fail,
            non_empty_required,
        } => {
            if branches.is_empty() && *non_empty_required {
                opaque_semantics("choice combinator has no branches and therefore always fails")
            } else {
                let branch_semantics = branches
                    .iter()
                    .map(|branch| {
                        normalized_fuzzer_semantics(
                            branch,
                            current_module,
                            function_index,
                            constant_index,
                            data_types,
                            output_type.as_ref(),
                            local_values,
                            visiting_functions,
                        )
                    })
                    .collect::<Vec<_>>();
                merge_choice_semantics(output_type.as_ref(), &branch_semantics, data_types)
                    .unwrap_or_else(|| {
                        if *may_fail {
                            opaque_semantics(
                                "choice combinator may fail and the current semantic export is conservative",
                            )
                        } else {
                            default_semantics_for_type(output_type.as_ref(), data_types)
                        }
                    })
            }
        }
        NormalizedFuzzer::Filter {
            output_type,
            source,
            impossible,
            ..
        } => {
            if *impossible {
                opaque_semantics("such_that predicate is impossible for all generated values")
            } else {
                normalized_fuzzer_semantics(
                    source,
                    current_module,
                    function_index,
                    constant_index,
                    data_types,
                    output_type.as_ref(),
                    local_values,
                    visiting_functions,
                )
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
            constant_index,
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
pub(super) fn extract_constraint_from_via(
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
pub(super) fn extract_constraint_from_via_with_data_types(
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
pub(super) fn extract_constraint_from_via_with_constants(
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

pub(super) fn extract_constraint_from_via_with_constants_and_data_types(
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

#[cfg(test)]
pub(super) fn int_bounds_are_in_order(lo: &str, hi: &str) -> Option<bool> {
    Some(parse_decimal_bigint(lo)? <= parse_decimal_bigint(hi)?)
}

/// Pick the larger of two int bound strings (for lower bound intersection).
#[cfg(test)]
pub(super) fn intersect_int_bound_max(a: Option<&str>, b: Option<&str>) -> Option<String> {
    match (a, b) {
        (Some(a), Some(b)) => {
            let va = parse_decimal_bigint(a)?;
            let vb = parse_decimal_bigint(b)?;
            Some(va.max(vb).to_string())
        }
        (Some(a), None) => Some(a.to_string()),
        (None, Some(b)) => Some(b.to_string()),
        (None, None) => None,
    }
}

/// Pick the smaller of two int bound strings (for upper bound intersection).
#[cfg(test)]
pub(super) fn intersect_int_bound_min(a: Option<&str>, b: Option<&str>) -> Option<String> {
    match (a, b) {
        (Some(a), Some(b)) => {
            let va = parse_decimal_bigint(a)?;
            let vb = parse_decimal_bigint(b)?;
            Some(va.min(vb).to_string())
        }
        (Some(a), None) => Some(a.to_string()),
        (None, Some(b)) => Some(b.to_string()),
        (None, None) => None,
    }
}

pub(super) fn semantics_to_constraint(semantics: &FuzzerSemantics) -> FuzzerConstraint {
    match semantics {
        FuzzerSemantics::IntRange {
            min: Some(min),
            max: Some(max),
        } => FuzzerConstraint::IntRange {
            min: min.clone(),
            max: max.clone(),
        },
        FuzzerSemantics::ByteArrayRange {
            min_len: Some(min_len),
            max_len: Some(max_len),
        } => FuzzerConstraint::ByteStringLenRange {
            min_len: *min_len,
            max_len: *max_len,
        },
        FuzzerSemantics::Exact(value) => FuzzerConstraint::Exact(value.clone()),
        FuzzerSemantics::OneOf(values) => FuzzerConstraint::OneOf(values.clone()),
        FuzzerSemantics::Product(items) => {
            FuzzerConstraint::Tuple(items.iter().map(semantics_to_constraint).collect())
        }
        FuzzerSemantics::List {
            element,
            min_len,
            max_len,
        } => FuzzerConstraint::List {
            elem: Box::new(semantics_to_constraint(element.as_ref())),
            min_len: *min_len,
            max_len: *max_len,
        },
        FuzzerSemantics::Constructors { tags } => {
            FuzzerConstraint::DataConstructorTags { tags: tags.clone() }
        }
        FuzzerSemantics::Opaque { reason } => FuzzerConstraint::Unsupported {
            reason: reason.clone(),
        },
        FuzzerSemantics::DataWithSchema { .. }
        | FuzzerSemantics::Data
        | FuzzerSemantics::Bool
        | FuzzerSemantics::String
        | FuzzerSemantics::IntRange { .. }
        | FuzzerSemantics::ByteArrayRange { .. }
        | FuzzerSemantics::StateMachineTrace { .. } => FuzzerConstraint::Any,
    }
}

pub(super) fn opaque_semantics(reason: impl Into<String>) -> FuzzerSemantics {
    FuzzerSemantics::Opaque {
        reason: reason.into(),
    }
}

pub(super) fn default_semantics_for_type(
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

pub(super) fn pretty_print_type(tipo: &Type) -> String {
    let mut printer = Printer::new();
    printer.print(tipo).to_pretty_string(80)
}

pub(super) fn semantic_type_name(tipo: &Type) -> String {
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

pub(super) fn semantic_type_from_type(tipo: &Type) -> SemanticType {
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

pub(super) fn state_machine_output_semantics_matches_acceptance(
    acceptance: &StateMachineAcceptance,
    output_semantics: &FuzzerSemantics,
) -> bool {
    matches!(
        (acceptance, output_semantics),
        (
            StateMachineAcceptance::AcceptsSuccess,
            FuzzerSemantics::List { .. }
        ) | (
            StateMachineAcceptance::AcceptsFailure,
            FuzzerSemantics::Product(_)
        )
    )
}

pub(super) fn state_machine_trace_output_semantics(
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

pub(super) fn is_prng_type(tipo: &Type) -> bool {
    match tipo {
        Type::App { module, name, .. } => name == "PRNG" && module.is_empty(),
        Type::Var { tipo, .. } => match tipo.as_ref().borrow().deref() {
            TypeVar::Link { tipo } => is_prng_type(tipo.as_ref()),
            _ => false,
        },
        _ => false,
    }
}

pub(super) fn extract_fuzzer_payload_type(tipo: &Type) -> Option<Rc<Type>> {
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

pub(super) fn extract_semantics_from_via_with_constants(
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
pub(super) fn extract_semantics_from_via(
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
pub(super) fn semantics_from_constraint(
    constraint: &FuzzerConstraint,
    output_type: &Type,
) -> FuzzerSemantics {
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
        FuzzerConstraint::Empty { .. } => default_semantics_for_type(output_type, &IndexMap::new()),
        FuzzerConstraint::Map(_) => opaque_semantics(
            "map input constraint cannot be reinterpreted as output semantics without a proven identity mapper",
        ),
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
                                Some(existing) => intersect_int_bound_max(Some(&existing), Some(m))
                                    .unwrap_or_else(|| existing.clone()),
                                None => m.clone(),
                            });
                        }
                        if let Some(m) = max {
                            result_max = Some(match result_max {
                                Some(existing) => intersect_int_bound_min(Some(&existing), Some(m))
                                    .unwrap_or_else(|| existing.clone()),
                                None => m.clone(),
                            });
                        }
                    }
                }
                // Guard against inverted (empty) ranges from disjoint
                // constraints, e.g. [10,20] AND [30,40] => min=30, max=20.
                if let (Some(lo), Some(hi)) = (&result_min, &result_max) {
                    if int_bounds_are_in_order(lo, hi) == Some(false) {
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
