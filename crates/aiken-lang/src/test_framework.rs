use crate::{
    ast::{
        BinOp, CallArg, DataTypeKey, FunctionAccessKey, IfBranch, OnTestFailure, Span, TypedArg,
        TypedDataType, TypedFunction, TypedPattern, TypedTest, UnOp,
    },
    expr::{TypedExpr, UntypedExpr},
    format::Formatter,
    gen_uplc::CodeGenerator,
    plutus_version::PlutusVersion,
    tipo::{ModuleValueConstructor, Type, ValueConstructorVariant, convert_opaque_type},
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
            let constraint = extract_constraint_from_via_with_constants(
                &via,
                module_name.as_str(),
                generator.functions(),
                generator.constants(),
            );

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
                        constraint,
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
pub enum FuzzerConstraint {
    /// No constraint known; the fuzzer may produce any value of the given type.
    Any,
    /// Integer in a closed range [min, max].
    IntRange { min: String, max: String },
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
    /// A mapped constraint: the underlying constraint describes the input domain.
    Map(Box<FuzzerConstraint>),
    /// Conjunction of constraints (all must hold).
    And(Vec<FuzzerConstraint>),
    /// Constraint could not be extracted; includes a human-readable reason.
    Unsupported { reason: String },
}

#[derive(Debug, Clone)]
pub struct Fuzzer<T> {
    pub program: Program<T>,

    pub type_info: Rc<Type>,

    /// A version of the Fuzzer's type that has gotten rid of
    /// all erasable opaque type. This is needed in order to
    /// generate Plutus data with the appropriate shape.
    pub stripped_type_info: Rc<Type>,

    /// Constraint extracted from the fuzzer expression for formal verification.
    pub constraint: FuzzerConstraint,
}

fn extract_constraint_from_via(
    via: &TypedExpr,
    current_module: &str,
    known_functions: &IndexMap<&FunctionAccessKey, &TypedFunction>,
) -> FuzzerConstraint {
    extract_constraint_from_via_with_constants(
        via,
        current_module,
        known_functions,
        &IndexMap::new(),
    )
}

fn extract_constraint_from_via_with_constants(
    via: &TypedExpr,
    current_module: &str,
    known_functions: &IndexMap<&FunctionAccessKey, &TypedFunction>,
    known_constants: &IndexMap<&FunctionAccessKey, &TypedExpr>,
) -> FuzzerConstraint {
    let function_index = index_known_functions(known_functions);
    let constant_index = index_known_constants(known_constants);
    let mut visiting_functions = BTreeSet::new();

    extract_constraint_from_expr(
        via,
        current_module,
        &function_index,
        &constant_index,
        &BTreeMap::new(),
        &mut visiting_functions,
    )
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

fn find_constant<'a>(
    constant_index: &'a ConstantIndex<'a>,
    module_name: &str,
    constant_name: &str,
) -> Option<&'a TypedExpr> {
    constant_index.get(module_name)?.get(constant_name).copied()
}

fn extract_constraint_from_expr(
    expr: &TypedExpr,
    current_module: &str,
    function_index: &FunctionIndex<'_>,
    constant_index: &ConstantIndex<'_>,
    local_values: &BTreeMap<String, TypedExpr>,
    visiting_functions: &mut BTreeSet<(String, String)>,
) -> FuzzerConstraint {
    match expr {
        TypedExpr::Call { fun, args, .. } => {
            let fn_name =
                fuzz_builtin_name(fun.as_ref(), current_module, function_index, local_values);

            match fn_name.as_deref() {
                Some("int_between") if args.len() == 2 => {
                    let min = extract_int_value(
                        &args[0].value,
                        current_module,
                        constant_index,
                        local_values,
                    );
                    let max = extract_int_value(
                        &args[1].value,
                        current_module,
                        constant_index,
                        local_values,
                    );
                    match (min, max) {
                        (Some(min), Some(max)) => normalize_int_range(min, max),
                        _ => FuzzerConstraint::Unsupported {
                            reason: "int_between: could not extract literal bounds".to_string(),
                        },
                    }
                }
                Some("int") | Some("any_int") if args.is_empty() => {
                    // aiken/fuzz.int() produces values in [-255, 16383].
                    // These are the documented bounds of the fuzz library implementation.
                    FuzzerConstraint::IntRange {
                        min: "-255".to_string(),
                        max: "16383".to_string(),
                    }
                }
                Some("int_at_least") if args.len() == 1 => {
                    if let Some(min_str) = extract_int_value(
                        &args[0].value,
                        current_module,
                        constant_index,
                        local_values,
                    ) {
                        int_at_least_constraint(min_str)
                    } else {
                        FuzzerConstraint::Unsupported {
                            reason: "int_at_least: could not extract literal bound".to_string(),
                        }
                    }
                }
                Some("int_at_most") if args.len() == 1 => {
                    if let Some(max_str) = extract_int_value(
                        &args[0].value,
                        current_module,
                        constant_index,
                        local_values,
                    ) {
                        int_at_most_constraint(max_str)
                    } else {
                        FuzzerConstraint::Unsupported {
                            reason: "int_at_most: could not extract literal bound".to_string(),
                        }
                    }
                }
                // map(fuzzer, transform_fn): the underlying constraint describes
                // the input fuzzer's domain, wrapped in Map to indicate the output
                // type may differ.
                Some("map") if args.len() == 2 => {
                    let inner = extract_constraint_from_expr(
                        &args[0].value,
                        current_module,
                        function_index,
                        constant_index,
                        local_values,
                        visiting_functions,
                    );
                    // When the mapper is an obvious unary integer transform,
                    // preserve output-domain bounds instead of leaving a generic
                    // Map wrapper.
                    map_int_constraint_through_mapper(
                        &inner,
                        &args[1].value,
                        current_module,
                        function_index,
                        local_values,
                    )
                    .unwrap_or_else(|| FuzzerConstraint::Map(Box::new(inner)))
                }
                // and_then(fuzzer, continuation_fn): compose the input-domain and
                // continuation-domain constraints when both are analyzable.
                Some("and_then") | Some("then") if args.len() == 2 => {
                    let left = extract_constraint_from_expr(
                        &args[0].value,
                        current_module,
                        function_index,
                        constant_index,
                        local_values,
                        visiting_functions,
                    );
                    let right = extract_constraint_from_continuation(
                        &args[1].value,
                        current_module,
                        function_index,
                        constant_index,
                        local_values,
                        visiting_functions,
                    );
                    combine_constraints(vec![left, right])
                }
                // both(fuzzer_a, fuzzer_b): tuple constraint from both components.
                Some("both") if args.len() >= 2 => {
                    let left = extract_constraint_from_expr(
                        &args[0].value,
                        current_module,
                        function_index,
                        constant_index,
                        local_values,
                        visiting_functions,
                    );
                    let right = extract_constraint_from_expr(
                        &args[1].value,
                        current_module,
                        function_index,
                        constant_index,
                        local_values,
                        visiting_functions,
                    );
                    FuzzerConstraint::Tuple(vec![left, right])
                }
                // tuple/tuple3/tuple4/... : preserve per-component constraints.
                Some(name)
                    if tuple_builtin_arity(name).is_some_and(|arity| args.len() == arity) =>
                {
                    FuzzerConstraint::Tuple(
                        args.iter()
                            .map(|arg| {
                                extract_constraint_from_expr(
                                    &arg.value,
                                    current_module,
                                    function_index,
                                    constant_index,
                                    local_values,
                                    visiting_functions,
                                )
                            })
                            .collect(),
                    )
                }
                // map2(fuzzer_a, fuzzer_b, mapper): preserve/reorder constraints
                // only when the mapper is known to return a direct tuple of
                // its two inputs (possibly swapped).
                //
                // NOTE (over-approximation): This decomposition extracts
                // per-element constraints independently. Any relational
                // correlations introduced by the mapper (e.g., a < b) are
                // not captured in the constraint IR. The generated proof
                // domain will therefore be a Cartesian product of the
                // individual element domains, which is an over-approximation
                // of the actual fuzzer sample space. Properties that depend
                // on inter-element invariants enforced only by the mapper
                // may fail formal verification even when property testing
                // passes.
                Some("map2") if args.len() >= 3 => {
                    let Some(mapper_arg_order) = map2_mapper_arg_order(
                        &args[2].value,
                        current_module,
                        function_index,
                        local_values,
                    ) else {
                        return FuzzerConstraint::Unsupported {
                            reason: "map2: mapper is not a simple tuple constructor".to_string(),
                        };
                    };

                    let left = extract_constraint_from_expr(
                        &args[0].value,
                        current_module,
                        function_index,
                        constant_index,
                        local_values,
                        visiting_functions,
                    );
                    let right = extract_constraint_from_expr(
                        &args[1].value,
                        current_module,
                        function_index,
                        constant_index,
                        local_values,
                        visiting_functions,
                    );

                    let ordered = match mapper_arg_order {
                        [0, 1] => vec![left, right],
                        [1, 0] => vec![right, left],
                        _ => {
                            return FuzzerConstraint::Unsupported {
                                reason: "map2: unexpected mapper argument order".to_string(),
                            };
                        }
                    };

                    FuzzerConstraint::Tuple(ordered)
                }
                // constant(value): always produces the same value.
                Some("constant") if args.len() == 1 => {
                    if let Some(val) = extract_int_value(
                        &args[0].value,
                        current_module,
                        constant_index,
                        local_values,
                    ) {
                        FuzzerConstraint::IntRange {
                            min: val.clone(),
                            max: val,
                        }
                    } else if let Some(value) = extract_exact_scalar_value(
                        &args[0].value,
                        current_module,
                        constant_index,
                        local_values,
                    ) {
                        FuzzerConstraint::Exact(value)
                    } else {
                        FuzzerConstraint::Unsupported {
                            reason:
                                "constant: could not extract supported literal (Int/Bool/String/ByteArray)"
                                    .to_string(),
                        }
                    }
                }
                // list(elem_fuzzer): list with element constraint, no length bounds known.
                Some("list") if args.len() == 1 => {
                    let elem = extract_constraint_from_expr(
                        &args[0].value,
                        current_module,
                        function_index,
                        constant_index,
                        local_values,
                        visiting_functions,
                    );
                    FuzzerConstraint::List {
                        elem: Box::new(elem),
                        min_len: None,
                        max_len: None,
                    }
                }
                // list_between(elem_fuzzer, min_len, max_len): list with length bounds.
                Some("list_between") if args.len() == 3 => {
                    let elem = extract_constraint_from_expr(
                        &args[0].value,
                        current_module,
                        function_index,
                        constant_index,
                        local_values,
                        visiting_functions,
                    );
                    let min_len = match extract_list_length_bound(
                        "list_between",
                        "min_len",
                        &args[1].value,
                        current_module,
                        constant_index,
                        local_values,
                    ) {
                        Ok(v) => v,
                        Err(reason) => {
                            return FuzzerConstraint::Unsupported { reason };
                        }
                    };
                    let max_len = match extract_list_length_bound(
                        "list_between",
                        "max_len",
                        &args[2].value,
                        current_module,
                        constant_index,
                        local_values,
                    ) {
                        Ok(v) => v,
                        Err(reason) => {
                            return FuzzerConstraint::Unsupported { reason };
                        }
                    };

                    if min_len > max_len {
                        return FuzzerConstraint::Unsupported {
                            reason: format!(
                                "list_between: inconsistent length bounds min_len={} > max_len={}",
                                min_len, max_len
                            ),
                        };
                    }

                    FuzzerConstraint::List {
                        elem: Box::new(elem),
                        min_len: Some(min_len),
                        max_len: Some(max_len),
                    }
                }
                Some("list_at_least") if args.len() == 2 => {
                    let elem = extract_constraint_from_expr(
                        &args[0].value,
                        current_module,
                        function_index,
                        constant_index,
                        local_values,
                        visiting_functions,
                    );
                    let min_len = match extract_list_length_bound(
                        "list_at_least",
                        "min_len",
                        &args[1].value,
                        current_module,
                        constant_index,
                        local_values,
                    ) {
                        Ok(v) => v,
                        Err(reason) => {
                            return FuzzerConstraint::Unsupported { reason };
                        }
                    };

                    FuzzerConstraint::List {
                        elem: Box::new(elem),
                        min_len: Some(min_len),
                        max_len: None,
                    }
                }
                Some("list_at_most") if args.len() == 2 => {
                    let elem = extract_constraint_from_expr(
                        &args[0].value,
                        current_module,
                        function_index,
                        constant_index,
                        local_values,
                        visiting_functions,
                    );
                    let max_len = match extract_list_length_bound(
                        "list_at_most",
                        "max_len",
                        &args[1].value,
                        current_module,
                        constant_index,
                        local_values,
                    ) {
                        Ok(v) => v,
                        Err(reason) => {
                            return FuzzerConstraint::Unsupported { reason };
                        }
                    };

                    FuzzerConstraint::List {
                        elem: Box::new(elem),
                        min_len: None,
                        max_len: Some(max_len),
                    }
                }
                // option(inner): output domain differs from input domain.
                Some("option") if args.len() == 1 => {
                    let inner = extract_constraint_from_expr(
                        &args[0].value,
                        current_module,
                        function_index,
                        constant_index,
                        local_values,
                        visiting_functions,
                    );
                    FuzzerConstraint::Map(Box::new(inner))
                }
                Some("bytearray_between") if args.len() == 2 => FuzzerConstraint::Unsupported {
                    reason: "bytearray_between: bytearray length-domain constraints are not yet represented".to_string(),
                },
                _ => {
                    if let Some(name) = scenario_builtin_name(
                        fun.as_ref(),
                        current_module,
                        function_index,
                        local_values,
                    ) {
                        extract_constraint_from_scenario_call(name.as_str(), args)
                    } else if let Some(helper) = extract_constraint_from_helper_call(
                        fun.as_ref(),
                        args,
                        current_module,
                        function_index,
                        constant_index,
                        local_values,
                        visiting_functions,
                    ) {
                        helper
                    } else {
                        FuzzerConstraint::Unsupported {
                            reason: format!(
                                "unsupported fuzzer call shape: {}",
                                describe_expr(fun.as_ref())
                            ),
                        }
                    }
                }
            }
        }
        TypedExpr::Var {
            name, constructor, ..
        } => {
            if let ValueConstructorVariant::LocalVariable { .. } = &constructor.variant {
                if let Some(bound_expr) = local_values.get(name) {
                    extract_constraint_from_expr(
                        bound_expr,
                        current_module,
                        function_index,
                        constant_index,
                        local_values,
                        visiting_functions,
                    )
                } else {
                    FuzzerConstraint::Unsupported {
                        reason: format!(
                            "local fuzzer '{}' is not bound to a resolvable expression",
                            name
                        ),
                    }
                }
            } else {
                FuzzerConstraint::Unsupported {
                    reason: format!("unsupported fuzzer expression: {}", describe_expr(expr)),
                }
            }
        }
        // Pipelines and sequences are desugared into a list of expressions.
        // Track intermediate let-bound aliases before extracting from the tail.
        TypedExpr::Pipeline { expressions, .. } | TypedExpr::Sequence { expressions, .. } => {
            extract_constraint_from_sequence(
                expressions,
                current_module,
                function_index,
                constant_index,
                local_values,
                visiting_functions,
            )
        }
        _ => FuzzerConstraint::Unsupported {
            reason: format!("unsupported fuzzer expression: {}", describe_expr(expr)),
        },
    }
}

fn extract_constraint_from_sequence(
    expressions: &[TypedExpr],
    current_module: &str,
    function_index: &FunctionIndex<'_>,
    constant_index: &ConstantIndex<'_>,
    local_values: &BTreeMap<String, TypedExpr>,
    visiting_functions: &mut BTreeSet<(String, String)>,
) -> FuzzerConstraint {
    let Some(last) = expressions.last() else {
        return FuzzerConstraint::Unsupported {
            reason: "empty pipeline/sequence fuzzer expression".to_string(),
        };
    };

    let mut scoped_values = local_values.clone();

    for expr in expressions.iter().take(expressions.len().saturating_sub(1)) {
        if let TypedExpr::Assignment { pattern, value, .. } = expr {
            if let Some(name) = pattern_var_name(pattern) {
                scoped_values.insert(name.to_string(), value.as_ref().clone());
            }
        }
    }

    extract_constraint_from_expr(
        last,
        current_module,
        function_index,
        constant_index,
        &scoped_values,
        visiting_functions,
    )
}

fn pattern_var_name(pattern: &TypedPattern) -> Option<&str> {
    match pattern {
        TypedPattern::Var { name, .. } | TypedPattern::Assign { name, .. } => Some(name.as_str()),
        _ => None,
    }
}

fn extract_list_length_bound(
    builtin_name: &str,
    bound_name: &str,
    expr: &TypedExpr,
    current_module: &str,
    constant_index: &ConstantIndex<'_>,
    local_values: &BTreeMap<String, TypedExpr>,
) -> Result<usize, String> {
    let Some(raw) = extract_int_value(expr, current_module, constant_index, local_values) else {
        return Err(format!(
            "{builtin_name}: could not extract {bound_name} as an integer literal"
        ));
    };

    let parsed = raw.parse::<usize>().map_err(|_| {
        format!("{builtin_name}: {bound_name} must be a non-negative usize literal, got '{raw}'")
    })?;

    Ok(parsed)
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

fn combine_constraints(constraints: Vec<FuzzerConstraint>) -> FuzzerConstraint {
    fn flatten_into(
        constraint: FuzzerConstraint,
        out: &mut Vec<FuzzerConstraint>,
        unsupported: &mut Vec<String>,
    ) {
        match constraint {
            FuzzerConstraint::Any => {}
            FuzzerConstraint::And(parts) => {
                for part in parts {
                    flatten_into(part, out, unsupported);
                }
            }
            FuzzerConstraint::Unsupported { reason } => unsupported.push(reason),
            other => out.push(other),
        }
    }

    let mut normalized = Vec::new();
    let mut unsupported = Vec::new();
    for constraint in constraints {
        flatten_into(constraint, &mut normalized, &mut unsupported);
    }

    if !unsupported.is_empty() {
        return FuzzerConstraint::Unsupported {
            reason: unsupported.join("; "),
        };
    }

    if normalized.is_empty() {
        FuzzerConstraint::Any
    } else if normalized.len() == 1 {
        normalized.pop().expect("len checked")
    } else {
        FuzzerConstraint::And(normalized)
    }
}

fn extract_constraint_from_continuation(
    continuation: &TypedExpr,
    current_module: &str,
    function_index: &FunctionIndex<'_>,
    constant_index: &ConstantIndex<'_>,
    local_values: &BTreeMap<String, TypedExpr>,
    visiting_functions: &mut BTreeSet<(String, String)>,
) -> FuzzerConstraint {
    let continuation = terminal_expression(continuation);
    match continuation {
        TypedExpr::Fn { body, .. } => extract_constraint_from_expr(
            body,
            current_module,
            function_index,
            constant_index,
            local_values,
            visiting_functions,
        ),
        _ => {
            let mut visiting_local_aliases = BTreeSet::new();
            let Some(resolved) = resolve_function_from_expr(
                continuation,
                current_module,
                function_index,
                local_values,
                &mut visiting_local_aliases,
            ) else {
                return FuzzerConstraint::Unsupported {
                    reason: "and_then/then: continuation is not a resolvable function".to_string(),
                };
            };

            let key = (resolved.module_name.clone(), resolved.function_name.clone());
            if !visiting_functions.insert(key.clone()) {
                return FuzzerConstraint::Unsupported {
                    reason: format!(
                        "and_then/then: recursive continuation detected at {}.{}",
                        resolved.module_name, resolved.function_name
                    ),
                };
            }

            let result = extract_constraint_from_expr(
                &resolved.function.body,
                &resolved.module_name,
                function_index,
                constant_index,
                &BTreeMap::new(),
                visiting_functions,
            );
            visiting_functions.remove(&key);
            result
        }
    }
}

fn extract_constraint_from_scenario_call(
    name: &str,
    args: &[CallArg<TypedExpr>],
) -> FuzzerConstraint {
    let unbounded_list = || FuzzerConstraint::List {
        elem: Box::new(FuzzerConstraint::Any),
        min_len: None,
        max_len: None,
    };

    match name {
        // scenario.ok(initial_state, step) : Fuzzer<List<Transaction>>
        "ok" if args.len() == 2 => unbounded_list(),
        // scenario.ko(initial_state, step) : Fuzzer<(List<Label>, List<Transaction>)>
        "ko" if args.len() == 2 => {
            FuzzerConstraint::Tuple(vec![unbounded_list(), unbounded_list()])
        }
        // scenario.report_coverage(initial_state, step) : Fuzzer<Outcome>
        "report_coverage" if args.len() == 2 => FuzzerConstraint::Unsupported {
            reason: "scenario.report_coverage: output-domain extraction not yet supported"
                .to_string(),
        },
        _ => FuzzerConstraint::Unsupported {
            reason: format!("scenario.{name}: unsupported call shape"),
        },
    }
}

/// Return the tuple arity for fuzz tuple constructors.
/// Supports `tuple` (=2), `tuple3`, `tuple4`, ...
fn tuple_builtin_arity(name: &str) -> Option<usize> {
    if name == "tuple" {
        Some(2)
    } else {
        let suffix = name.strip_prefix("tuple")?;
        let arity = suffix.parse::<usize>().ok()?;
        (arity >= 2).then_some(arity)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum UnaryIntTransform {
    Identity,
    Negate,
}

/// Preserve output-domain bounds for simple integer `fuzz.map` mappers.
fn map_int_constraint_through_mapper(
    inner: &FuzzerConstraint,
    mapper: &TypedExpr,
    current_module: &str,
    function_index: &FunctionIndex<'_>,
    local_values: &BTreeMap<String, TypedExpr>,
) -> Option<FuzzerConstraint> {
    let (min, max) = extract_int_range_from_constraint(inner)?;
    let transform =
        unary_int_mapper_transform(mapper, current_module, function_index, local_values)?;

    let (min, max) = match transform {
        UnaryIntTransform::Identity => (min, max),
        UnaryIntTransform::Negate => {
            let min_val = parse_bigint_literal(&min)?;
            let max_val = parse_bigint_literal(&max)?;
            ((-max_val).to_string(), (-min_val).to_string())
        }
    };

    Some(FuzzerConstraint::IntRange { min, max })
}

/// Extract an IntRange from a constraint, recursively unwrapping Map layers.
fn extract_int_range_from_constraint(constraint: &FuzzerConstraint) -> Option<(String, String)> {
    match constraint {
        FuzzerConstraint::IntRange { min, max } => Some((min.clone(), max.clone())),
        FuzzerConstraint::Map(inner) => extract_int_range_from_constraint(inner),
        _ => None,
    }
}

fn unary_int_mapper_transform(
    mapper: &TypedExpr,
    current_module: &str,
    function_index: &FunctionIndex<'_>,
    local_values: &BTreeMap<String, TypedExpr>,
) -> Option<UnaryIntTransform> {
    let mapper = terminal_expression(mapper);

    match mapper {
        TypedExpr::Fn { args, body, .. } => unary_int_transform_from_fn(args, body),
        _ => {
            let mut visiting_local_aliases = BTreeSet::new();
            let resolved = resolve_function_from_expr(
                mapper,
                current_module,
                function_index,
                local_values,
                &mut visiting_local_aliases,
            )?;
            unary_int_transform_from_fn_in_module(&resolved.module_name, resolved.function)
        }
    }
}

fn unary_int_transform_from_fn(args: &[TypedArg], body: &TypedExpr) -> Option<UnaryIntTransform> {
    let [arg] = args else {
        return None;
    };
    let arg_name = arg.get_variable_name()?;
    let body = terminal_expression(body);

    match body {
        TypedExpr::Var { name, .. } if name == arg_name => Some(UnaryIntTransform::Identity),
        TypedExpr::UnOp {
            op: UnOp::Negate,
            value,
            ..
        } => {
            let value = terminal_expression(value.as_ref());
            if let TypedExpr::Var { name, .. } = value {
                (name == arg_name).then_some(UnaryIntTransform::Negate)
            } else {
                None
            }
        }
        _ => None,
    }
}

fn unary_int_transform_from_fn_in_module(
    _module_name: &str,
    function: &TypedFunction,
) -> Option<UnaryIntTransform> {
    unary_int_transform_from_fn(&function.arguments, &function.body)
}

fn extract_constraint_from_helper_call(
    fun: &TypedExpr,
    args: &[CallArg<TypedExpr>],
    current_module: &str,
    function_index: &FunctionIndex<'_>,
    constant_index: &ConstantIndex<'_>,
    local_values: &BTreeMap<String, TypedExpr>,
    visiting_functions: &mut BTreeSet<(String, String)>,
) -> Option<FuzzerConstraint> {
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
        return Some(FuzzerConstraint::Unsupported {
            reason: format!(
                "recursive helper fuzzer detected at {}.{}",
                resolved.module_name, resolved.function_name
            ),
        });
    }

    // Helper body extraction should see both caller locals and helper parameter bindings.
    // Materialize local-alias arguments against caller scope to avoid self-referential
    // bindings when parameter names overlap with caller aliases.
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

    let result = extract_constraint_from_expr(
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
        TypedExpr::ModuleSelect { constructor, .. } => match constructor {
            ModuleValueConstructor::Fn { module, name, .. } => {
                let function = find_function(function_index, module, name)?;
                Some(ResolvedFunction {
                    module_name: module.clone(),
                    function_name: name.clone(),
                    function,
                })
            }
            _ => None,
        },
        _ => None,
    }
}

fn fuzz_builtin_name(
    fun: &TypedExpr,
    current_module: &str,
    function_index: &FunctionIndex<'_>,
    local_values: &BTreeMap<String, TypedExpr>,
) -> Option<String> {
    const FUZZ_MODULE: &str = "aiken/fuzz";

    match fun {
        TypedExpr::Var {
            name, constructor, ..
        } => match &constructor.variant {
            ValueConstructorVariant::ModuleFn { module, name, .. } if module == FUZZ_MODULE => {
                Some(name.clone())
            }
            ValueConstructorVariant::LocalVariable { .. } => {
                let bound_expr = local_values.get(name)?;
                fuzz_builtin_name(bound_expr, current_module, function_index, local_values)
            }
            _ => None,
        },
        TypedExpr::ModuleSelect { constructor, .. } => match constructor {
            ModuleValueConstructor::Fn { module, name, .. } if module == FUZZ_MODULE => {
                Some(name.clone())
            }
            _ => None,
        },
        _ => {
            let mut visiting_local_aliases = BTreeSet::new();
            let resolved = resolve_function_from_expr(
                fun,
                current_module,
                function_index,
                local_values,
                &mut visiting_local_aliases,
            )?;
            (resolved.module_name == FUZZ_MODULE).then_some(resolved.function_name)
        }
    }
}

fn scenario_builtin_name(
    fun: &TypedExpr,
    current_module: &str,
    function_index: &FunctionIndex<'_>,
    local_values: &BTreeMap<String, TypedExpr>,
) -> Option<String> {
    const SCENARIO_MODULE: &str = "aiken/fuzz/scenario";

    match fun {
        TypedExpr::Var {
            name, constructor, ..
        } => match &constructor.variant {
            ValueConstructorVariant::ModuleFn { module, name, .. } if module == SCENARIO_MODULE => {
                Some(name.clone())
            }
            ValueConstructorVariant::LocalVariable { .. } => {
                let bound_expr = local_values.get(name)?;
                scenario_builtin_name(bound_expr, current_module, function_index, local_values)
            }
            _ => None,
        },
        TypedExpr::ModuleSelect { constructor, .. } => match constructor {
            ModuleValueConstructor::Fn { module, name, .. } if module == SCENARIO_MODULE => {
                Some(name.clone())
            }
            _ => None,
        },
        _ => {
            let mut visiting_local_aliases = BTreeSet::new();
            let resolved = resolve_function_from_expr(
                fun,
                current_module,
                function_index,
                local_values,
                &mut visiting_local_aliases,
            )?;
            (resolved.module_name == SCENARIO_MODULE).then_some(resolved.function_name)
        }
    }
}

fn parse_bigint_literal(value: &str) -> Option<BigInt> {
    value.parse::<BigInt>().ok()
}

fn extract_exact_scalar_value(
    expr: &TypedExpr,
    current_module: &str,
    constant_index: &ConstantIndex<'_>,
    local_values: &BTreeMap<String, TypedExpr>,
) -> Option<FuzzerExactValue> {
    let mut visiting_constants = BTreeSet::new();
    let mut visiting_locals = BTreeSet::new();
    extract_exact_scalar_value_with_constants(
        expr,
        current_module,
        constant_index,
        local_values,
        &mut visiting_constants,
        &mut visiting_locals,
    )
}

fn extract_exact_scalar_value_with_constants(
    expr: &TypedExpr,
    current_module: &str,
    constant_index: &ConstantIndex<'_>,
    local_values: &BTreeMap<String, TypedExpr>,
    visiting_constants: &mut BTreeSet<(String, String)>,
    visiting_locals: &mut BTreeSet<String>,
) -> Option<FuzzerExactValue> {
    let expr = terminal_expression(expr);

    match expr {
        TypedExpr::Var { name, .. } if name == "True" => Some(FuzzerExactValue::Bool(true)),
        TypedExpr::Var { name, .. } if name == "False" => Some(FuzzerExactValue::Bool(false)),
        TypedExpr::String { value, .. } => Some(FuzzerExactValue::String(value.clone())),
        TypedExpr::ByteArray { bytes, .. } => Some(FuzzerExactValue::ByteArray(bytes.clone())),
        TypedExpr::Var {
            name, constructor, ..
        } => match &constructor.variant {
            ValueConstructorVariant::ModuleConstant { module, name, .. } => {
                resolve_exact_scalar_constant(
                    module,
                    name,
                    constant_index,
                    local_values,
                    visiting_constants,
                    visiting_locals,
                )
            }
            ValueConstructorVariant::LocalVariable { .. } => {
                let bound_expr = local_values.get(name)?;
                if !visiting_locals.insert(name.clone()) {
                    return None;
                }
                let result = extract_exact_scalar_value_with_constants(
                    bound_expr,
                    current_module,
                    constant_index,
                    local_values,
                    visiting_constants,
                    visiting_locals,
                );
                visiting_locals.remove(name);
                result
            }
            _ => None,
        },
        TypedExpr::ModuleSelect { constructor, .. } => match constructor {
            ModuleValueConstructor::Constant { module, name, .. } => resolve_exact_scalar_constant(
                module,
                name,
                constant_index,
                local_values,
                visiting_constants,
                visiting_locals,
            ),
            _ => None,
        },
        _ => None,
    }
}

fn resolve_exact_scalar_constant(
    module_name: &str,
    constant_name: &str,
    constant_index: &ConstantIndex<'_>,
    local_values: &BTreeMap<String, TypedExpr>,
    visiting_constants: &mut BTreeSet<(String, String)>,
    visiting_locals: &mut BTreeSet<String>,
) -> Option<FuzzerExactValue> {
    let key = (module_name.to_string(), constant_name.to_string());
    if !visiting_constants.insert(key.clone()) {
        return None;
    }

    let value_expr = match find_constant(constant_index, module_name, constant_name) {
        Some(expr) => expr,
        None => {
            visiting_constants.remove(&key);
            return None;
        }
    };
    let result = extract_exact_scalar_value_with_constants(
        value_expr,
        module_name,
        constant_index,
        local_values,
        visiting_constants,
        visiting_locals,
    );
    visiting_constants.remove(&key);
    result
}

fn bigint_abs(value: &BigInt) -> BigInt {
    if value < &BigInt::from(0) {
        -value
    } else {
        value.clone()
    }
}

fn normalize_int_range(min: String, max: String) -> FuzzerConstraint {
    let (min_val, max_val) = match (parse_bigint_literal(&min), parse_bigint_literal(&max)) {
        (Some(min_val), Some(max_val)) => (min_val, max_val),
        _ => {
            return FuzzerConstraint::Unsupported {
                reason: format!("could not parse integer bounds: min={min}, max={max}"),
            };
        }
    };

    if min_val <= max_val {
        FuzzerConstraint::IntRange { min, max }
    } else {
        FuzzerConstraint::IntRange { min: max, max: min }
    }
}

/// Heuristic parameters for `int_at_least` / `int_at_most` bound estimation.
///
/// The aiken/fuzz library generates random integers using a scheme that, for
/// small absolute values (at or below `SMALL_INT_THRESHOLD`), stays within
/// `[-SMALL_INT_THRESHOLD, SMALL_INT_THRESHOLD]`. For larger values, the range
/// expands proportionally by `EXPANSION_FACTOR * abs(anchor)`.
///
/// These are derived from the current aiken/fuzz implementation and should be
/// updated if the library semantics change.
const SMALL_INT_THRESHOLD: i64 = 255;
const EXPANSION_FACTOR: i64 = 5;

fn int_at_least_constraint(min: String) -> FuzzerConstraint {
    let Some(min_val) = parse_bigint_literal(&min) else {
        return FuzzerConstraint::Unsupported {
            reason: format!("int_at_least: could not parse bound '{min}'"),
        };
    };
    let abs_min = bigint_abs(&min_val);
    let threshold = BigInt::from(SMALL_INT_THRESHOLD);
    let max_val = if abs_min <= threshold {
        threshold
    } else {
        &min_val + BigInt::from(EXPANSION_FACTOR) * &abs_min
    };

    FuzzerConstraint::IntRange {
        min,
        max: max_val.to_string(),
    }
}

fn int_at_most_constraint(max: String) -> FuzzerConstraint {
    let Some(max_val) = parse_bigint_literal(&max) else {
        return FuzzerConstraint::Unsupported {
            reason: format!("int_at_most: could not parse bound '{max}'"),
        };
    };
    let abs_max = bigint_abs(&max_val);
    let threshold = BigInt::from(SMALL_INT_THRESHOLD);
    let min_val = if abs_max <= threshold {
        -threshold
    } else {
        &max_val - BigInt::from(EXPANSION_FACTOR) * &abs_max
    };

    FuzzerConstraint::IntRange {
        min: min_val.to_string(),
        max,
    }
}

fn map2_mapper_arg_order(
    mapper: &TypedExpr,
    current_module: &str,
    function_index: &FunctionIndex<'_>,
    local_values: &BTreeMap<String, TypedExpr>,
) -> Option<[usize; 2]> {
    let mapper = terminal_expression(mapper);

    match mapper {
        TypedExpr::Fn { args, body, .. } => map2_tuple_arg_order(args, body),
        _ => {
            let mut visiting_local_aliases = BTreeSet::new();
            let resolved = resolve_function_from_expr(
                mapper,
                current_module,
                function_index,
                local_values,
                &mut visiting_local_aliases,
            )?;
            map2_tuple_arg_order(&resolved.function.arguments, &resolved.function.body)
        }
    }
}

fn map2_tuple_arg_order(args: &[TypedArg], body: &TypedExpr) -> Option<[usize; 2]> {
    let [first_arg, second_arg] = args else {
        return None;
    };

    let first_name = first_arg.get_variable_name()?;
    let second_name = second_arg.get_variable_name()?;

    let body = terminal_expression(body);
    let TypedExpr::Tuple { elems, .. } = body else {
        return None;
    };

    let [first_elem, second_elem] = elems.as_slice() else {
        return None;
    };

    let first_ix = tuple_elem_arg_index(first_elem, first_name, second_name)?;
    let second_ix = tuple_elem_arg_index(second_elem, first_name, second_name)?;

    if first_ix == second_ix {
        None
    } else {
        Some([first_ix, second_ix])
    }
}

fn tuple_elem_arg_index(elem: &TypedExpr, first_name: &str, second_name: &str) -> Option<usize> {
    let elem = terminal_expression(elem);
    let TypedExpr::Var { name, .. } = elem else {
        return None;
    };

    if name == first_name {
        Some(0)
    } else if name == second_name {
        Some(1)
    } else {
        None
    }
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

fn extract_int_value(
    expr: &TypedExpr,
    current_module: &str,
    constant_index: &ConstantIndex<'_>,
    local_values: &BTreeMap<String, TypedExpr>,
) -> Option<String> {
    let mut visiting_constants = BTreeSet::new();
    let mut visiting_locals = BTreeSet::new();
    extract_int_value_with_constants(
        expr,
        current_module,
        constant_index,
        local_values,
        &mut visiting_constants,
        &mut visiting_locals,
    )
}

fn extract_int_value_with_constants(
    expr: &TypedExpr,
    current_module: &str,
    constant_index: &ConstantIndex<'_>,
    local_values: &BTreeMap<String, TypedExpr>,
    visiting_constants: &mut BTreeSet<(String, String)>,
    visiting_locals: &mut BTreeSet<String>,
) -> Option<String> {
    let expr = terminal_expression(expr);

    match expr {
        TypedExpr::UInt { value, .. } => Some(value.clone()),
        TypedExpr::UnOp {
            op: UnOp::Negate,
            value,
            ..
        } => {
            let value = terminal_expression(value.as_ref());
            if let TypedExpr::UInt { value, .. } = value {
                Some(format!("-{}", value))
            } else {
                let inner = extract_int_value_with_constants(
                    value,
                    current_module,
                    constant_index,
                    local_values,
                    visiting_constants,
                    visiting_locals,
                )?;
                let parsed = parse_bigint_literal(&inner)?;
                Some((-parsed).to_string())
            }
        }
        TypedExpr::BinOp {
            name, left, right, ..
        } => {
            let left = extract_int_value_with_constants(
                left,
                current_module,
                constant_index,
                local_values,
                visiting_constants,
                visiting_locals,
            )?;
            let right = extract_int_value_with_constants(
                right,
                current_module,
                constant_index,
                local_values,
                visiting_constants,
                visiting_locals,
            )?;
            let left = parse_bigint_literal(&left)?;
            let right = parse_bigint_literal(&right)?;

            match name {
                BinOp::AddInt => Some((left + right).to_string()),
                BinOp::SubInt => Some((left - right).to_string()),
                BinOp::MultInt => Some((left * right).to_string()),
                BinOp::DivInt => {
                    plutus_divide_integer(&left, &right).map(|value| value.to_string())
                }
                BinOp::ModInt => plutus_mod_integer(&left, &right).map(|value| value.to_string()),
                _ => None,
            }
        }
        TypedExpr::Var {
            name, constructor, ..
        } => match &constructor.variant {
            ValueConstructorVariant::ModuleConstant { module, name, .. } => resolve_int_constant(
                module,
                name,
                constant_index,
                local_values,
                visiting_constants,
                visiting_locals,
            ),
            ValueConstructorVariant::LocalVariable { .. } => {
                let bound_expr = local_values.get(name)?;
                if !visiting_locals.insert(name.clone()) {
                    return None;
                }
                let result = extract_int_value_with_constants(
                    bound_expr,
                    current_module,
                    constant_index,
                    local_values,
                    visiting_constants,
                    visiting_locals,
                );
                visiting_locals.remove(name);
                result
            }
            _ => None,
        },
        TypedExpr::ModuleSelect { constructor, .. } => match constructor {
            ModuleValueConstructor::Constant { module, name, .. } => resolve_int_constant(
                module,
                name,
                constant_index,
                local_values,
                visiting_constants,
                visiting_locals,
            ),
            _ => None,
        },
        _ => None,
    }
}

/// Plutus DivideInteger semantics:
/// quotient = floor(lhs / rhs) over mathematical integers.
fn plutus_divide_integer(lhs: &BigInt, rhs: &BigInt) -> Option<BigInt> {
    let (quotient, _) = plutus_div_mod(lhs, rhs)?;
    Some(quotient)
}

/// Plutus ModInteger semantics:
/// remainder uses divisor-sign convention (same sign as rhs, unless zero).
fn plutus_mod_integer(lhs: &BigInt, rhs: &BigInt) -> Option<BigInt> {
    let (_, remainder) = plutus_div_mod(lhs, rhs)?;
    Some(remainder)
}

fn plutus_div_mod(lhs: &BigInt, rhs: &BigInt) -> Option<(BigInt, BigInt)> {
    let zero = BigInt::from(0);
    if rhs == &zero {
        return None;
    }

    // BigInt / % use truncating-division semantics; adjust when remainder and
    // divisor signs differ to recover Plutus floor-division/modulo behavior.
    let quotient = lhs / rhs;
    let remainder = lhs % rhs;
    let signs_differ = (remainder > zero && rhs < &zero) || (remainder < zero && rhs > &zero);

    if remainder != zero && signs_differ {
        Some((quotient - BigInt::from(1), remainder + rhs))
    } else {
        Some((quotient, remainder))
    }
}

fn resolve_int_constant(
    module_name: &str,
    constant_name: &str,
    constant_index: &ConstantIndex<'_>,
    local_values: &BTreeMap<String, TypedExpr>,
    visiting_constants: &mut BTreeSet<(String, String)>,
    visiting_locals: &mut BTreeSet<String>,
) -> Option<String> {
    let key = (module_name.to_string(), constant_name.to_string());
    if !visiting_constants.insert(key.clone()) {
        return None;
    }

    let value_expr = match find_constant(constant_index, module_name, constant_name) {
        Some(expr) => expr,
        None => {
            visiting_constants.remove(&key);
            return None;
        }
    };
    let result = extract_int_value_with_constants(
        value_expr,
        module_name,
        constant_index,
        local_values,
        visiting_constants,
        visiting_locals,
    );
    visiting_constants.remove(&key);
    result
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
    use crate::ast::CallArg;
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
        module_fn_var(name, "aiken/fuzz", tipo)
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

    fn empty_known_functions<'a>() -> IndexMap<&'a FunctionAccessKey, &'a TypedFunction> {
        IndexMap::new()
    }

    fn empty_known_constants<'a>() -> IndexMap<&'a FunctionAccessKey, &'a TypedExpr> {
        IndexMap::new()
    }

    #[test]
    fn normalize_int_range_swaps_reversed_bounds() {
        let constraint = normalize_int_range("10".to_string(), "0".to_string());

        match constraint {
            FuzzerConstraint::IntRange { min, max } => {
                assert_eq!(min, "0");
                assert_eq!(max, "10");
            }
            other => panic!("expected IntRange, got {other:?}"),
        }
    }

    #[test]
    fn int_at_least_handles_big_literal_without_fallback() {
        let min = "170141183460469231731687303715884105728".to_string(); // i128::MAX + 1
        let expected_max = "1020847100762815390390123822295304634368".to_string(); // min * 6

        let constraint = int_at_least_constraint(min.clone());

        match constraint {
            FuzzerConstraint::IntRange { min: got_min, max } => {
                assert_eq!(got_min, min);
                assert_eq!(max, expected_max);
            }
            other => panic!("expected IntRange, got {other:?}"),
        }
    }

    #[test]
    fn int_at_most_handles_i128_min_without_overflow() {
        let max = "-170141183460469231731687303715884105728".to_string(); // i128::MIN
        let expected_min = "-1020847100762815390390123822295304634368".to_string(); // max * 6

        let constraint = int_at_most_constraint(max.clone());

        match constraint {
            FuzzerConstraint::IntRange { min, max: got_max } => {
                assert_eq!(min, expected_min);
                assert_eq!(got_max, max);
            }
            other => panic!("expected IntRange, got {other:?}"),
        }
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
    fn extract_constraint_map2_identity_mapper_yields_tuple() {
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
    fn extract_constraint_map2_swapped_mapper_reorders() {
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
        assert_eq!(
            constraint,
            FuzzerConstraint::Tuple(vec![
                FuzzerConstraint::IntRange {
                    min: "20".to_string(),
                    max: "30".to_string(),
                },
                FuzzerConstraint::IntRange {
                    min: "0".to_string(),
                    max: "10".to_string(),
                },
            ])
        );
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
        assert_eq!(
            constraint,
            FuzzerConstraint::Tuple(vec![
                FuzzerConstraint::IntRange {
                    min: "20".to_string(),
                    max: "30".to_string(),
                },
                FuzzerConstraint::IntRange {
                    min: "0".to_string(),
                    max: "10".to_string(),
                },
            ])
        );
    }

    #[test]
    fn extract_constraint_int_between_basic() {
        let via = make_int_between_via("5", "100");
        let functions = empty_known_functions();
        assert_eq!(
            extract_constraint_from_via(&via, "math", &functions),
            FuzzerConstraint::IntRange {
                min: "5".to_string(),
                max: "100".to_string(),
            }
        );
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
        assert_eq!(
            extract_constraint_from_via(&via, "math", &functions),
            FuzzerConstraint::IntRange {
                min: "-3".to_string(),
                max: "1".to_string(),
            }
        );
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
        assert_eq!(
            extract_constraint_from_via(&via, "math", &functions),
            FuzzerConstraint::IntRange {
                min: "-3".to_string(),
                max: "-1".to_string(),
            }
        );
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

        assert_eq!(
            extract_constraint_from_via_with_constants(
                &via,
                "permissions_examples",
                &functions,
                &constants,
            ),
            FuzzerConstraint::IntRange {
                min: "0".to_string(),
                max: "0".to_string(),
            }
        );
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

        assert_eq!(
            extract_constraint_from_via_with_constants(
                &via,
                "permissions_examples",
                &functions,
                &constants,
            ),
            FuzzerConstraint::IntRange {
                min: "0".to_string(),
                max: "0".to_string(),
            }
        );
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
        assert_eq!(
            extract_constraint_from_via(&via, "math", &functions),
            FuzzerConstraint::IntRange {
                min: "-255".to_string(),
                max: "16383".to_string(),
            }
        );
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
        assert_eq!(
            extract_constraint_from_via(&via, "math", &functions),
            FuzzerConstraint::IntRange {
                min: "10".to_string(),
                max: "255".to_string(),
            }
        );
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
        assert_eq!(
            extract_constraint_from_via(&via, "math", &functions),
            FuzzerConstraint::IntRange {
                min: "-255".to_string(),
                max: "10".to_string(),
            }
        );
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
        assert_eq!(
            extract_constraint_from_via(&via, "math", &functions),
            FuzzerConstraint::IntRange {
                min: "42".to_string(),
                max: "42".to_string(),
            }
        );
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
        assert_eq!(
            constraint,
            FuzzerConstraint::Map(Box::new(FuzzerConstraint::IntRange {
                min: "0".to_string(),
                max: "10".to_string(),
            }))
        );
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
        assert_eq!(
            constraint,
            FuzzerConstraint::IntRange {
                min: "-50".to_string(),
                max: "-1".to_string(),
            }
        );
    }

    #[test]
    fn extract_constraint_zero_arg_helper_call_is_unwrapped() {
        let (helper_key, helper_fn) =
            make_zero_arg_function("helper_fuzzer", Type::int(), make_int_between_via("3", "7"));
        let mut functions = empty_known_functions();
        functions.insert(&helper_key, &helper_fn);

        let via = make_zero_arg_call("helper_fuzzer", Type::int());
        let constraint = extract_constraint_from_via(&via, "math", &functions);

        assert_eq!(
            constraint,
            FuzzerConstraint::IntRange {
                min: "3".to_string(),
                max: "7".to_string(),
            }
        );
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

        assert_eq!(
            constraint,
            FuzzerConstraint::IntRange {
                min: "-50".to_string(),
                max: "-1".to_string(),
            }
        );
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

        assert_eq!(
            extract_constraint_from_via(&via, "math", &functions),
            FuzzerConstraint::IntRange {
                min: "7".to_string(),
                max: "12".to_string(),
            }
        );
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

        assert_eq!(
            extract_constraint_from_via(&via, "math", &functions),
            FuzzerConstraint::IntRange {
                min: "7".to_string(),
                max: "12".to_string(),
            }
        );
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
        assert_eq!(
            extract_constraint_from_via(&sequence, "math", &functions),
            FuzzerConstraint::IntRange {
                min: "2".to_string(),
                max: "9".to_string(),
            }
        );
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

        assert_eq!(
            constraint,
            FuzzerConstraint::Tuple(vec![
                FuzzerConstraint::IntRange {
                    min: "0".to_string(),
                    max: "5".to_string(),
                },
                FuzzerConstraint::IntRange {
                    min: "10".to_string(),
                    max: "15".to_string(),
                },
                FuzzerConstraint::IntRange {
                    min: "20".to_string(),
                    max: "25".to_string(),
                },
                FuzzerConstraint::IntRange {
                    min: "30".to_string(),
                    max: "35".to_string(),
                },
            ])
        );
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
        assert_eq!(
            extract_constraint_from_via(&via, "math", &functions),
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
        assert_eq!(
            extract_constraint_from_via(&via, "math", &functions),
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
        assert_eq!(
            extract_constraint_from_via(&via, "math", &functions),
            FuzzerConstraint::List {
                elem: Box::new(FuzzerConstraint::IntRange {
                    min: "0".to_string(),
                    max: "10".to_string(),
                }),
                min_len: Some(2),
                max_len: Some(5),
            }
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
        assert_eq!(
            extract_constraint_from_via(&via, "math", &functions),
            FuzzerConstraint::IntRange {
                min: "3".to_string(),
                max: "7".to_string(),
            }
        );
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

    #[test]
    fn extract_constraint_scenario_ok_is_list_domain() {
        let via = TypedExpr::Call {
            location: Span::empty(),
            tipo: Type::int(),
            fun: Box::new(module_fn_var(
                "ok",
                "aiken/fuzz/scenario",
                Type::function(vec![Type::int(), Type::int()], Type::int()),
            )),
            args: vec![
                call_arg(make_int_between_via("0", "10")),
                call_arg(uint_lit("0")),
            ],
        };
        let functions = empty_known_functions();
        assert_eq!(
            extract_constraint_from_via(&via, "math", &functions),
            FuzzerConstraint::List {
                elem: Box::new(FuzzerConstraint::Any),
                min_len: None,
                max_len: None,
            }
        );
    }

    #[test]
    fn extract_constraint_scenario_ko_is_tuple_of_lists() {
        let via = TypedExpr::Call {
            location: Span::empty(),
            tipo: Type::int(),
            fun: Box::new(module_fn_var(
                "ko",
                "aiken/fuzz/scenario",
                Type::function(vec![Type::int(), Type::int()], Type::int()),
            )),
            args: vec![
                call_arg(make_int_between_via("0", "10")),
                call_arg(uint_lit("0")),
            ],
        };
        let functions = empty_known_functions();
        assert_eq!(
            extract_constraint_from_via(&via, "math", &functions),
            FuzzerConstraint::Tuple(vec![
                FuzzerConstraint::List {
                    elem: Box::new(FuzzerConstraint::Any),
                    min_len: None,
                    max_len: None,
                },
                FuzzerConstraint::List {
                    elem: Box::new(FuzzerConstraint::Any),
                    min_len: None,
                    max_len: None,
                },
            ])
        );
    }

    #[test]
    fn extract_constraint_scenario_report_coverage_is_unsupported() {
        let via = TypedExpr::Call {
            location: Span::empty(),
            tipo: Type::int(),
            fun: Box::new(module_fn_var(
                "report_coverage",
                "aiken/fuzz/scenario",
                Type::function(vec![Type::int(), Type::int()], Type::int()),
            )),
            args: vec![
                call_arg(make_int_between_via("0", "10")),
                call_arg(uint_lit("0")),
            ],
        };
        let functions = empty_known_functions();
        assert!(matches!(
            extract_constraint_from_via(&via, "math", &functions),
            FuzzerConstraint::Unsupported { .. }
        ));
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
        assert_eq!(
            extract_constraint_from_via(&via, "math", &functions),
            FuzzerConstraint::Tuple(vec![
                FuzzerConstraint::Map(Box::new(FuzzerConstraint::IntRange {
                    min: "0".to_string(),
                    max: "10".to_string(),
                })),
                FuzzerConstraint::IntRange {
                    min: "20".to_string(),
                    max: "30".to_string(),
                },
            ])
        );
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
