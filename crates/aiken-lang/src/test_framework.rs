#[cfg(test)]
use crate::ast::{Annotation, RecordConstructor, RecordConstructorArg};
use crate::{
    ast::{
        BinOp, CallArg, DataTypeKey, FunctionAccessKey, IfBranch, OnTestFailure, Span, TypedArg,
        TypedDataType, TypedFunction, TypedPattern, TypedTest, UnOp,
    },
    expr::{TypedExpr, UntypedExpr},
    format::Formatter,
    gen_uplc::CodeGenerator,
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
#[cfg(test)]
const STDLIB_FUZZ_SCENARIO_MODULE: &str = "aiken/fuzz/scenario";

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
                    .expect("failed to convert assertion operaand to NamedDeBruijn")
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
        return_type: Rc<Type>,
        program: Program<Name>,
        fuzzer: Fuzzer<Name>,
    ) -> Test {
        Test::PropertyTest(PropertyTest {
            input_path,
            module,
            name,
            program,
            on_test_failure,
            return_type,
            fuzzer,
        })
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
        } else {
            let parameter = test.arguments.first().unwrap().to_owned();

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

            match kind {
                RunnableKind::Bench => Test::Benchmark(Benchmark {
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
                }),
                RunnableKind::Test => Self::property_test(
                    input_path,
                    module_name,
                    test.name,
                    test.on_test_failure,
                    test.return_type,
                    program,
                    Fuzzer {
                        program: generator_program,
                        stripped_type_info,
                        type_info,
                        normalized,
                        constraint,
                        semantics,
                    },
                ),
            }
        }
    }

    pub fn run(
        self,
        seed: u32,
        max_success: usize,
        plutus_version: &PlutusVersion,
    ) -> TestResult<(Constant, Rc<Type>), PlutusData> {
        match self {
            Test::UnitTest(unit_test) => TestResult::UnitTestResult(unit_test.run(plutus_version)),
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
    pub fn run(self, plutus_version: &PlutusVersion) -> UnitTestResult<(Constant, Rc<Type>)> {
        let eval_result = Program::<NamedDeBruijn>::try_from(self.program.clone())
            .unwrap()
            .eval_version(ExBudget::max(), &plutus_version.into());

        let success = !eval_result.failed(match self.on_test_failure {
            OnTestFailure::SucceedEventually | OnTestFailure::SucceedImmediately => true,
            OnTestFailure::FailImmediately => false,
        });

        let mut logs = Vec::new();
        if let Err(err) = eval_result.result() {
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
    pub return_type: Rc<Type>,
    pub program: Program<Name>,
    pub fuzzer: Fuzzer<Name>,
}

unsafe impl Send for PropertyTest {}

/// Typed constraint IR describing what a fuzzer is known to produce.
///
/// This is re-exported from the project crate as `FuzzerConstraint` in the
/// export manifest. It supports composable constraints for arbitrary fuzzer
/// output shapes (integers, tuples, lists, mapped values, etc.).
#[derive(Debug, Clone, PartialEq)]
pub enum FuzzerExactValue {
    Bool(bool),
    ByteArray(Vec<u8>),
    String(String),
}

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
    Exact(FuzzerExactValue),
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
    },
    Opaque {
        reason: String,
    },
}

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

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum UnaryMapperShape {
    Identity,
    ConstBool(bool),
    ConstByteArray(Vec<u8>),
    ConstString(String),
    ConstInt(String),
    IntAffine { scale: i8, offset: String },
    ConstructorMap(BTreeMap<String, String>),
    Unknown,
}

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

    /// Compiler-owned structural normalization of the fuzzer expression.
    pub normalized: NormalizedFuzzer,

    /// Constraint extracted from the fuzzer expression for formal verification.
    pub constraint: FuzzerConstraint,

    /// Compiler-owned semantic description of the fuzzer image.
    pub semantics: FuzzerSemantics,
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

            normalize_state_machine_trace_from_call(&resolved_fun, tipo.as_ref(), &resolved_args)
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

    if let Some(normalized) = normalize_fuzzer_from_helper_call(
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

    if args
        .iter()
        .all(|arg| !expression_has_fuzzer_type(&arg.value))
    {
        if let Some(output_type) = extract_fuzzer_payload_type(expr.tipo().as_ref()) {
            let known_constraint =
                try_extract_stdlib_primitive_constraint(fun, args, constant_index, local_values);
            return NormalizedFuzzer::Primitive {
                output_type,
                known_constraint,
            };
        }
    }

    opaque_normalized_fuzzer(
        expr,
        format!(
            "call '{}' is a Fuzzer but its structural shape is not recognized",
            describe_expr(fun)
        ),
    )
}

/// Check if a call target is a known stdlib fuzzer from `aiken/fuzz` and extract
/// the constraint from its literal arguments.
fn try_extract_stdlib_primitive_constraint(
    fun: &TypedExpr,
    args: &[CallArg<TypedExpr>],
    constant_index: &ConstantIndex<'_>,
    local_values: &BTreeMap<String, TypedExpr>,
) -> Option<FuzzerConstraint> {
    let (module, name) = extract_module_fn_identity(fun)?;
    if module != STDLIB_FUZZ_MODULE {
        return None;
    }

    match (name.as_str(), args.len()) {
        ("int_between", 2) => {
            let min = try_extract_int_literal(&args[0].value, constant_index, local_values)?;
            let max = try_extract_int_literal(&args[1].value, constant_index, local_values)?;
            Some(FuzzerConstraint::IntRange {
                min: min.to_string(),
                max: max.to_string(),
            })
        }
        ("int_at_least", 1) => {
            let min = try_extract_int_literal(&args[0].value, constant_index, local_values)?;
            Some(FuzzerConstraint::IntRange {
                min: min.to_string(),
                max: i128::MAX.to_string(),
            })
        }
        ("int_at_most", 1) => {
            let max = try_extract_int_literal(&args[0].value, constant_index, local_values)?;
            Some(FuzzerConstraint::IntRange {
                min: i128::MIN.to_string(),
                max: max.to_string(),
            })
        }
        ("constant", 1) => {
            if let Some(value) =
                try_extract_int_literal(&args[0].value, constant_index, local_values)
            {
                let s = value.to_string();
                Some(FuzzerConstraint::IntRange {
                    min: s.clone(),
                    max: s,
                })
            } else {
                try_extract_exact_scalar(&args[0].value).map(FuzzerConstraint::Exact)
            }
        }
        ("bytearray_between", 2) => {
            let min_len = try_extract_int_literal(&args[0].value, constant_index, local_values)?;
            let max_len = try_extract_int_literal(&args[1].value, constant_index, local_values)?;
            if min_len < 0 || max_len < 0 || min_len > max_len {
                return None;
            }
            Some(FuzzerConstraint::ByteStringLenRange {
                min_len: min_len as usize,
                max_len: max_len as usize,
            })
        }
        _ => None,
    }
}

/// Extract the module and function name from a callee expression.
fn extract_module_fn_identity(fun: &TypedExpr) -> Option<(String, String)> {
    let fun = terminal_expression(fun);
    match fun {
        TypedExpr::Var { constructor, .. } => match &constructor.variant {
            ValueConstructorVariant::ModuleFn { module, name, .. } => {
                Some((module.clone(), name.clone()))
            }
            _ => None,
        },
        _ => None,
    }
}

/// Try to extract an integer literal from a TypedExpr.
/// Handles UInt literals, negated UInt literals, local variable aliases, and module constants.
fn try_extract_int_literal(
    expr: &TypedExpr,
    constant_index: &ConstantIndex<'_>,
    local_values: &BTreeMap<String, TypedExpr>,
) -> Option<i128> {
    try_extract_int_literal_inner(expr, constant_index, local_values, 0)
}

const INT_LITERAL_MAX_DEPTH: u8 = 16;

fn try_extract_int_literal_inner(
    expr: &TypedExpr,
    constant_index: &ConstantIndex<'_>,
    local_values: &BTreeMap<String, TypedExpr>,
    depth: u8,
) -> Option<i128> {
    if depth > INT_LITERAL_MAX_DEPTH {
        return None;
    }
    let expr = terminal_expression(expr);
    match expr {
        TypedExpr::UInt { value, .. } => value.parse::<i128>().ok(),
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
        _ => None,
    }
}

/// Try to extract list length bounds from a known stdlib list-producing call.
fn try_extract_list_length_bounds(
    expr: &TypedExpr,
    args: &[CallArg<TypedExpr>],
    constant_index: &ConstantIndex<'_>,
    local_values: &BTreeMap<String, TypedExpr>,
) -> (Option<usize>, Option<usize>) {
    let fun = match expr {
        TypedExpr::Call { fun, .. } => fun.as_ref(),
        _ => return (None, None),
    };
    let Some((module, name)) = extract_module_fn_identity(fun) else {
        return (None, None);
    };
    if module != STDLIB_FUZZ_MODULE {
        return (None, None);
    }

    // Non-fuzzer arguments are the scalar (length bound) arguments.
    let scalar_args: Vec<&CallArg<TypedExpr>> = args
        .iter()
        .filter(|arg| !expression_has_fuzzer_type(&arg.value))
        .collect();

    match (name.as_str(), scalar_args.len()) {
        ("list_between", 2) => {
            let min = try_extract_int_literal(&scalar_args[0].value, constant_index, local_values);
            let max = try_extract_int_literal(&scalar_args[1].value, constant_index, local_values);
            match (min, max) {
                (Some(lo), Some(hi)) if lo >= 0 && hi >= 0 && lo <= hi => {
                    (Some(lo as usize), Some(hi as usize))
                }
                _ => (None, None),
            }
        }
        ("list_at_least", 1) => {
            let min = try_extract_int_literal(&scalar_args[0].value, constant_index, local_values);
            match min {
                Some(lo) if lo >= 0 => (Some(lo as usize), None),
                _ => (None, None),
            }
        }
        ("list_at_most", 1) => {
            let max = try_extract_int_literal(&scalar_args[0].value, constant_index, local_values);
            match max {
                Some(hi) if hi >= 0 => (None, Some(hi as usize)),
                _ => (None, None),
            }
        }
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
                let source = normalize_fuzzer_from_expr(
                    &source.value,
                    current_module,
                    function_index,
                    constant_index,
                    local_values,
                    visiting_functions,
                );
                let mapper_shape = summarize_unary_mapper_shape(
                    &mapper.value,
                    current_module,
                    function_index,
                    local_values,
                );

                if mapper_shape == UnaryMapperShape::Identity {
                    return Some(source);
                }

                // Distinguish filter from map: if the mapper returns Bool and the
                // source payload type equals the output payload type, this is a
                // filter predicate (like `such_that`), not a domain-transforming map.
                // Propagate the source's normalization unchanged — this is a sound
                // over-approximation since the filter only narrows the domain.
                let mapper_returns_bool =
                    function_return_type(&mapper.value).is_some_and(|(_, ret)| ret.is_bool());
                if mapper_returns_bool && source_output_type.as_ref() == output_type.as_ref() {
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
    if let Some(constructor_map) = resolve_nullary_constructor_mapper(
        body,
        arg_name,
        local_values,
        &mut visiting_local_aliases,
    ) {
        return UnaryMapperShape::ConstructorMap(constructor_map);
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
    let source_debug = format!("{source_semantics:?}");

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
        UnaryMapperShape::ConstInt(value) => FuzzerSemantics::IntRange {
            min: Some(value.clone()),
            max: Some(value.clone()),
        },
        UnaryMapperShape::IntAffine { scale, offset } => {
            if let Some(transformed) = apply_int_affine_semantics(&source_semantics, *scale, offset)
            {
                transformed
            } else {
                opaque_semantics(format!(
                    "semantic export for mapped generators is not implemented yet; source domain: {}",
                    source_debug
                ))
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

            opaque_semantics(format!(
                "semantic export for mapped generators is not implemented yet; source domain: {}",
                source_debug
            ))
        }
        UnaryMapperShape::Unknown => opaque_semantics(
            "semantic export for structurally mapped generators is not implemented yet",
        ),
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
                default_semantics_for_type(output_type)
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

fn default_semantics_for_type(tipo: &Type) -> FuzzerSemantics {
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
                element: Box::new(default_semantics_for_type(element_type.as_ref())),
                min_len: None,
                max_len: None,
            },
            _ => opaque_semantics("list type is missing its element type"),
        }
    } else if tipo.is_tuple() || tipo.is_pair() {
        FuzzerSemantics::Product(
            tipo.get_inner_types()
                .iter()
                .map(|inner| default_semantics_for_type(inner.as_ref()))
                .collect(),
        )
    } else if let Some((module, name)) = tipo.qualifier() {
        opaque_semantics(format!(
            "semantic export for type '{}.{}' is not implemented yet",
            if module.is_empty() {
                "<local>"
            } else {
                module.as_str()
            },
            name
        ))
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

fn default_semantics_for_semantic_type(tipo: &SemanticType) -> FuzzerSemantics {
    match tipo {
        SemanticType::Int => FuzzerSemantics::IntRange {
            min: None,
            max: None,
        },
        SemanticType::Bool => FuzzerSemantics::Bool,
        SemanticType::ByteArray => FuzzerSemantics::ByteArrayRange {
            min_len: None,
            max_len: None,
        },
        SemanticType::String => FuzzerSemantics::String,
        SemanticType::Data => FuzzerSemantics::Data,
        SemanticType::List(inner) => FuzzerSemantics::List {
            element: Box::new(default_semantics_for_semantic_type(inner.as_ref())),
            min_len: Some(0),
            max_len: None,
        },
        SemanticType::Tuple(elems) => FuzzerSemantics::Product(
            elems
                .iter()
                .map(default_semantics_for_semantic_type)
                .collect(),
        ),
        SemanticType::Pair(fst, snd) => FuzzerSemantics::Product(vec![
            default_semantics_for_semantic_type(fst.as_ref()),
            default_semantics_for_semantic_type(snd.as_ref()),
        ]),
        SemanticType::Unsupported(name) => FuzzerSemantics::Opaque {
            reason: format!(
                "semantic type '{}' requires structural schema for precise lowering",
                name
            ),
        },
    }
}

fn state_machine_trace_output_semantics(
    acceptance: StateMachineAcceptance,
    label_type: &SemanticType,
    event_type: &SemanticType,
) -> FuzzerSemantics {
    match acceptance {
        StateMachineAcceptance::AcceptsSuccess => FuzzerSemantics::List {
            element: Box::new(default_semantics_for_semantic_type(event_type)),
            min_len: Some(0),
            max_len: None,
        },
        StateMachineAcceptance::AcceptsFailure => {
            let mut labels = default_semantics_for_semantic_type(label_type);
            if let FuzzerSemantics::List { min_len, .. } = &mut labels {
                *min_len = Some(1);
            }

            FuzzerSemantics::Product(vec![
                labels,
                FuzzerSemantics::List {
                    element: Box::new(default_semantics_for_semantic_type(event_type)),
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
        _ => None,
    }
}

fn extract_state_machine_trace_fields(
    transition_type: &Rc<Type>,
    state_type: &Rc<Type>,
    data_types: &IndexMap<&DataTypeKey, &TypedDataType>,
) -> Result<(u64, u64, SemanticType, SemanticType), String> {
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

    let terminal_tag = zero_arity_ctors[0].0 as u64;
    let step_tag = step_constructors[0].0 as u64;

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

    Ok((
        terminal_tag,
        step_tag,
        semantic_type_from_type(label_type.as_ref()),
        semantic_type_from_type(event_type.as_ref()),
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
) -> Option<FuzzerSemantics> {
    let output_type = convert_opaque_type(&Rc::new(output_type.clone()), data_types, true);
    let args = make_synthetic_call_args(vec![initial_state.clone(), step_function.clone()]);

    extract_state_machine_trace_semantics_from_call(output_type.as_ref(), &args, data_types)
}

fn extract_state_machine_trace_semantics_from_call(
    output_type: &Type,
    args: &[CallArg<TypedExpr>],
    data_types: &IndexMap<&DataTypeKey, &TypedDataType>,
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

    let (terminal_tag, step_tag, label_type, event_type) =
        match extract_state_machine_trace_fields(&transition_type, &state_type, data_types) {
            Ok(fields) => fields,
            Err(reason) => {
                return Some(opaque_semantics(format!(
                    "state-machine trace lowering cannot infer transition shape: {reason}"
                )));
            }
        };

    let step_input_types: Vec<SemanticType> = step_args
        .iter()
        .skip(1)
        .map(|arg| {
            let stripped = convert_opaque_type(arg, data_types, true);
            semantic_type_from_type(stripped.as_ref())
        })
        .collect();
    let state_semantics = Box::new(default_semantics_for_semantic_type(
        &semantic_type_from_type(state_type.as_ref()),
    ));
    let step_input_semantics = step_input_types
        .iter()
        .map(default_semantics_for_semantic_type)
        .collect();
    let label_semantics = Box::new(default_semantics_for_semantic_type(&label_type));
    let event_semantics = Box::new(default_semantics_for_semantic_type(&event_type));
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
        &label_type,
        &event_type,
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
        "state-machine output semantics shape does not match acceptance mode: acceptance={:?}, semantics={:?}",
        acceptance,
        output_semantics
    );

    Some(FuzzerSemantics::StateMachineTrace {
        acceptance,
        state_type: semantic_type_from_type(state_type.as_ref()),
        step_input_types,
        label_type,
        event_type,
        transition_semantics,
        output_semantics,
    })
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
        FuzzerConstraint::Any => default_semantics_for_type(output_type),
        FuzzerConstraint::IntRange { min, max } => {
            if output_type.is_int() {
                FuzzerSemantics::IntRange {
                    min: Some(min.clone()),
                    max: Some(max.clone()),
                }
            } else {
                opaque_semantics(format!(
                    "integer-range constraint does not match output type '{:?}'",
                    output_type
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
                    "bytearray-length constraint does not match output type '{:?}'",
                    output_type
                ))
            }
        }
        FuzzerConstraint::Exact(value) => FuzzerSemantics::Exact(value.clone()),
        FuzzerConstraint::Tuple(elems) => {
            let inner_types = output_type.get_inner_types();
            if !(output_type.is_tuple() || output_type.is_pair()) {
                return opaque_semantics(format!(
                    "product constraint does not match output type '{:?}'",
                    output_type
                ));
            }
            if inner_types.len() != elems.len() {
                return opaque_semantics(format!(
                    "product constraint arity {} does not match output type '{:?}'",
                    elems.len(),
                    output_type
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
                "list constraint does not match output type '{:?}'",
                output_type
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
                        return default_semantics_for_type(output_type);
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
            default_semantics_for_type(output_type)
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

        let is_failure = result.failed(false);

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
                            let result = self.eval(&value, plutus_version);

                            let is_failure = result.failed(false);

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
                if *tag == 121 + Prng::SEEDED {
                    if let [
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
                }

                if *tag == 121 + Prng::REPLAYED {
                    if let [PlutusData::BigInt(..), PlutusData::BoundedBytes(choices)] = &fields[..]
                    {
                        return Prng::Replayed {
                            choices: choices.to_vec(),
                            uplc: cst.clone(),
                        };
                    }
                }
            }

            unreachable!("malformed Prng: {cst:#?}")
        }

        if let Term::Constant(rc) = &result {
            if let Constant::Data(PlutusData::Constr(Constr { tag, fields, .. })) = &rc.borrow() {
                if *tag == 121 + Prng::SOME {
                    if let [PlutusData::Array(elems)] = &fields[..] {
                        if let [new_seed, value] = &elems[..] {
                            return Some((as_prng(new_seed), value.clone()));
                        }
                    }
                }

                // May occurs when replaying a fuzzer from a shrinked sequence of
                // choices. If we run out of choices, or a choice end up being
                // invalid as per the expectation, the fuzzer can't go further and
                // fail.
                if *tag == 121 + Prng::NONE {
                    return None;
                }
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

    fn make_such_that_via(inner: TypedExpr) -> TypedExpr {
        let int_tipo = Type::int();
        let bool_tipo = Type::bool();

        let predicate = TypedExpr::Fn {
            location: Span::empty(),
            tipo: Type::function(vec![int_tipo.clone()], bool_tipo.clone()),
            is_capture: false,
            args: vec![TypedArg::new("x", int_tipo)],
            body: Box::new(local_var("True", bool_tipo)),
            return_annotation: None,
        };

        TypedExpr::Call {
            location: Span::empty(),
            tipo: Type::int(),
            fun: Box::new(fuzz_var(
                "such_that",
                Type::function(
                    vec![Type::int(), Type::function(vec![Type::int()], Type::bool())],
                    Type::int(),
                ),
            )),
            args: vec![call_arg(inner), call_arg(predicate)],
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

    fn make_constant_int_mapper(input_type: Rc<Type>, value: &str) -> TypedExpr {
        make_unary_mapper("x", input_type, Type::int(), uint_lit(value))
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
    fn extract_constraint_name_agnostic_map_preserves_map_domain_for_unknown_mapper() {
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

    /// Regression: `fuzz.int_at_least(5)` must produce a half-open
    /// `FuzzerSemantics::IntRange { min: Some("5"), max: None }`. Prior to the
    /// fix, `try_extract_stdlib_primitive_constraint` planted
    /// `i128::MAX.to_string()` as the upper-bound sentinel and
    /// `semantics_from_known_constraint` forwarded it verbatim, so downstream
    /// Lean emission narrowed the verified domain to [5, i128::MAX] and any
    /// counterexample with `x > i128::MAX` was missed.
    #[test]
    fn int_at_least_semantics_has_open_upper_bound() {
        let via = make_typed_int_at_least_fuzzer("5");
        let data_types: IndexMap<&DataTypeKey, &TypedDataType> = IndexMap::new();

        // Sanity-check the intermediate constraint still carries the sentinel
        // (so this test also pins the invariant that the fix lives in
        // `semantics_from_known_constraint`, not further upstream).
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

        // Defence-in-depth: assert the rendered semantics never leak the
        // i128 sentinel string (or any 39-digit numeric string) so the Lean
        // emitter can't receive it by accident if this code path is changed
        // later.
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

    /// Symmetric regression for `fuzz.int_at_most(10)`: lower bound must be
    /// open (`min: None`).
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

        assert!(matches!(semantics, FuzzerSemantics::Opaque { .. }));
    }

    #[test]
    fn extract_semantics_name_agnostic_constructor_map_unresolved_mapper_is_opaque() {
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

        assert!(matches!(semantics, FuzzerSemantics::Opaque { .. }));
    }

    #[test]
    fn extract_semantics_name_agnostic_constructor_map_recursive_mapper_cycle_is_opaque() {
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

        assert!(matches!(semantics, FuzzerSemantics::Opaque { .. }));
    }

    #[test]
    fn extract_semantics_name_agnostic_bind_uses_continuation_shape() {
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
    fn extract_semantics_name_agnostic_map_stays_conservative_for_unknown_mapper() {
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
        assert!(matches!(semantics, FuzzerSemantics::Opaque { .. }));
    }

    #[test]
    fn extract_semantics_name_agnostic_identity_map_preserves_source_domain() {
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

        let expected: Vec<FuzzerConstraint> = (0..10)
            .rev()
            .map(|i| FuzzerConstraint::IntRange {
                min: (i * 10).to_string(),
                max: (i * 10 + 9).to_string(),
            })
            .collect();

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
        // A filter (such_that) should propagate the source's constraint through unchanged.
        // such_that(int_between(1, 50), fn(x) -> Bool) => source constraint (Any, since
        // int_between is a Primitive with no extracted bounds yet)
        let source = make_typed_int_between_fuzzer("1", "50");
        let predicate = make_bool_predicate("x", Type::int());
        let via = make_typed_filter_call(source, predicate);
        let functions = empty_known_functions();
        let constraint = extract_constraint_from_via(&via, "math", &functions);

        // The source is int_between(1, 50), which now extracts IntRange bounds.
        // The filter should propagate through, giving us the source's constraint.
        assert_eq!(
            constraint,
            FuzzerConstraint::IntRange {
                min: "1".to_string(),
                max: "50".to_string(),
            }
        );
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

    #[test]
    fn extract_constraint_bytearray_between_with_bounds() {
        let via = TypedExpr::Call {
            location: Span::empty(),
            tipo: Type::byte_array(),
            fun: Box::new(fuzz_var(
                "bytearray_between",
                Type::function(vec![Type::int(), Type::int()], Type::byte_array()),
            )),
            args: vec![call_arg(uint_lit("2")), call_arg(uint_lit("5"))],
        };
        let functions = empty_known_functions();
        assert!(matches!(
            extract_constraint_from_via(&via, "math", &functions),
            FuzzerConstraint::Unsupported { .. }
        ));
    }

    #[test]
    fn extract_constraint_bytearray_between_inconsistent_bounds_is_unsupported() {
        let via = TypedExpr::Call {
            location: Span::empty(),
            tipo: Type::byte_array(),
            fun: Box::new(fuzz_var(
                "bytearray_between",
                Type::function(vec![Type::int(), Type::int()], Type::byte_array()),
            )),
            args: vec![call_arg(uint_lit("7")), call_arg(uint_lit("3"))],
        };
        let functions = empty_known_functions();
        let constraint = extract_constraint_from_via(&via, "math", &functions);
        assert!(matches!(constraint, FuzzerConstraint::Unsupported { .. }));
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
            state_semantics: Box::new(FuzzerSemantics::Opaque {
                reason: "semantic type 'permissions.State' requires structural schema for precise lowering".to_string(),
            }),
            step_input_semantics: vec![FuzzerSemantics::List {
                element: Box::new(FuzzerSemantics::Opaque {
                    reason: "semantic type 'cardano/transaction.Input' requires structural schema for precise lowering".to_string(),
                }),
                min_len: Some(0),
                max_len: None,
            }],
            label_semantics: Box::new(FuzzerSemantics::List {
                element: Box::new(FuzzerSemantics::String),
                min_len: Some(0),
                max_len: None,
            }),
            event_semantics: Box::new(FuzzerSemantics::Opaque {
                reason: "semantic type 'cardano/transaction.Transaction' requires structural schema for precise lowering".to_string(),
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
                    element: Box::new(FuzzerSemantics::Opaque {
                        reason: "semantic type 'cardano/transaction.Transaction' requires structural schema for precise lowering".to_string(),
                    }),
                    min_len: Some(0),
                    max_len: None,
                }),
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
                    element: Box::new(FuzzerSemantics::Opaque {
                        reason: "semantic type 'cardano/transaction.Transaction' requires structural schema for precise lowering".to_string(),
                    }),
                    min_len: Some(0),
                    max_len: None,
                }),
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
                        element: Box::new(FuzzerSemantics::Opaque {
                            reason: "semantic type 'cardano/transaction.Transaction' requires structural schema for precise lowering".to_string(),
                        }),
                        min_len: Some(1),
                        max_len: Some(1),
                    },
                ])),
            }
        );
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
    fn derive_semantics_for_list_of_unsupported_type_keeps_opaque_leaf() {
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
                    FuzzerSemantics::Opaque { reason }
                    if reason.contains("cardano/transaction.Transaction")
                )
            ),
            "expected unsupported element type to remain explicit in semantic IR, got {semantics:?}"
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
            Some(7),
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
            Some(-7),
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
            Some(99),
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
                // Innermost: must be a Primitive with a concrete IntRange.
                match n {
                    NormalizedFuzzer::Primitive {
                        known_constraint, ..
                    } => {
                        assert!(
                            matches!(known_constraint, Some(FuzzerConstraint::IntRange { .. })),
                            "innermost fuzzer must carry its IntRange constraint"
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
    fn normalize_fuzzer_map_of_map_depth_four_yields_intrange_constraint() {
        // Same Map(Map(Map(Map(...)))) shape, but checked through the
        // constraint extractor which is the actual proof-pipeline entry
        // point. The affine mappers compose: ((seed + 1) + 2) + 3 + 4 = +10.
        // With an unconstrained `seed` (Any), the composed range remains
        // unconstrained, but the extracted constraint must still walk the
        // full chain without opaquing.
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

        // [0,5] shifted by +1, +2, +3, +4 = [10, 15].
        assert_eq!(
            constraint,
            FuzzerConstraint::IntRange {
                min: "10".to_string(),
                max: "15".to_string(),
            },
            "Map(Map(Map(Map(...)))) must compose affine shifts through all four layers"
        );
    }

    #[test]
    fn normalize_fuzzer_nested_bind_continuation_returns_int_range() {
        // Build Bind(seed, \x0. Bind(p1, \x1. Bind(p2, \x2. int_between(3,7)))).
        // The outer Bind's constraint should be the innermost fuzzer's range
        // (Bind propagates the continuation's constraint).
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
            "nested Bind must propagate the innermost continuation's range"
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
}
