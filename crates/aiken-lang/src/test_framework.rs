use crate::{
    ast::{BinOp, DataTypeKey, IfBranch, OnTestFailure, Span, TypedArg, TypedDataType, TypedTest},
    expr::{TypedExpr, UntypedExpr},
    format::Formatter,
    gen_uplc::CodeGenerator,
    plutus_version::PlutusVersion,
    tipo::{convert_opaque_type, Type},
};
use cryptoxide::{blake2b::Blake2b, digest::Digest};
use indexmap::IndexMap;
use itertools::Itertools;
use owo_colors::{OwoColorize, Stream, Stream::Stderr};
use pallas_primitives::alonzo::{Constr, PlutusData};
use patricia_tree::PatriciaMap;
use std::{
    borrow::Borrow,
    collections::BTreeMap,
    convert::TryFrom,
    fmt::{Debug, Display},
    ops::Deref,
    path::PathBuf,
    rc::Rc,
    time::Duration,
};
use uplc::{
    ast::{Constant, Data, Name, NamedDeBruijn, Program, Term},
    machine::{cost_model::ExBudget, eval_result::EvalResult},
};
use vec1::{vec1, Vec1};

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
        program: Program<Name>,
        fuzzer: Fuzzer<Name>,
    ) -> Test {
        Test::PropertyTest(PropertyTest {
            input_path,
            module,
            name,
            program,
            on_test_failure,
            fuzzer,
        })
    }

    pub fn from_test_definition(
        generator: &mut CodeGenerator<'_>,
        test: TypedTest,
        module_name: String,
        input_path: PathBuf,
        is_benchmark: bool,
    ) -> Test {
        if test.arguments.is_empty() {
            if is_benchmark {
                unreachable!("benchmark must have at least one argument");
            } else {
                Self::unit_test(generator, test, module_name, input_path)
            }
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

            if is_benchmark {
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
            } else {
                Self::property_test(
                    input_path,
                    module_name,
                    test.name,
                    test.on_test_failure,
                    program,
                    Fuzzer {
                        program: generator_program,
                        stripped_type_info,
                        type_info,
                    },
                )
            }
        }
    }

    pub fn from_benchmark_definition(
        generator: &mut CodeGenerator<'_>,
        test: TypedTest,
        module_name: String,
        input_path: PathBuf,
    ) -> Test {
        Self::from_test_definition(generator, test, module_name, input_path, true)
    }

    pub fn from_function_definition(
        generator: &mut CodeGenerator<'_>,
        test: TypedTest,
        module_name: String,
        input_path: PathBuf,
    ) -> Test {
        Self::from_test_definition(generator, test, module_name, input_path, false)
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
    pub fn run<T>(self, plutus_version: &PlutusVersion) -> TestResult<(Constant, Rc<Type>), T> {
        let mut eval_result = Program::<NamedDeBruijn>::try_from(self.program.clone())
            .unwrap()
            .eval_version(ExBudget::max(), &plutus_version.into());

        let success = !eval_result.failed(match self.on_test_failure {
            OnTestFailure::SucceedEventually | OnTestFailure::SucceedImmediately => true,
            OnTestFailure::FailImmediately => false,
        });

        let mut traces = Vec::new();
        if let Err(err) = eval_result.result() {
            traces.push(format!("{err}"))
        }
        traces.extend(eval_result.logs());

        TestResult::UnitTestResult(UnitTestResult {
            success,
            test: self.to_owned(),
            spent_budget: eval_result.cost(),
            traces,
            assertion: self.assertion,
        })
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

unsafe impl Send for PropertyTest {}

#[derive(Debug, Clone)]
pub struct Fuzzer<T> {
    pub program: Program<T>,

    pub type_info: Rc<Type>,

    /// A version of the Fuzzer's type that has gotten rid of
    /// all erasable opaque type. This is needed in order to
    /// generate Plutus data with the appropriate shape.
    pub stripped_type_info: Rc<Type>,
}

#[derive(Debug, Clone, thiserror::Error, miette::Diagnostic)]
#[error("Fuzzer exited unexpectedly: {uplc_error}")]
pub struct FuzzerError {
    traces: Vec<String>,
    uplc_error: uplc::machine::Error,
}

#[derive(Debug, Clone)]
pub enum Event {
    Simplifying { choices: usize },
    Simplified { duration: Duration, steps: usize },
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
    pub fn run<U>(
        self,
        seed: u32,
        n: usize,
        plutus_version: &PlutusVersion,
    ) -> TestResult<U, PlutusData> {
        let mut labels = BTreeMap::new();
        let mut remaining = n;

        let (traces, counterexample, iterations) = match self.run_n_times(
            &mut remaining,
            Prng::from_seed(seed),
            &mut labels,
            plutus_version,
        ) {
            Ok(None) => (Vec::new(), Ok(None), n),
            Ok(Some(counterexample)) => (
                self.eval(&counterexample.value, plutus_version)
                    .logs()
                    .into_iter()
                    .filter(|s| PropertyTest::extract_label(s).is_none())
                    .collect(),
                Ok(Some(counterexample.value)),
                n - remaining,
            ),
            Err(FuzzerError { traces, uplc_error }) => (
                traces
                    .into_iter()
                    .filter(|s| PropertyTest::extract_label(s).is_none())
                    .collect(),
                Err(uplc_error),
                n - remaining + 1,
            ),
        };

        TestResult::PropertyTestResult(PropertyTestResult {
            test: self,
            counterexample,
            iterations,
            labels,
            traces,
        })
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
            let (next_prng, cex) = self.run_once(prng, labels, plutus_version)?;
            prng = next_prng;
            counterexample = cex;
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

        let mut result = self.eval(&value, plutus_version);

        for s in result.logs() {
            // NOTE: There may be other log outputs that interefere with labels. So *by
            // convention*, we treat as label strings that starts with a NUL byte, which
            // should be a guard sufficient to prevent inadvertent clashes.
            if let Some(label) = PropertyTest::extract_label(&s) {
                labels
                    .entry(label)
                    .and_modify(|count| *count += 1)
                    .or_insert(1);
            }
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

    fn extract_label(s: &str) -> Option<String> {
        if s.starts_with('\0') {
            Some(s.split_at(1).1.to_string())
        } else {
            None
        }
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
    pub fn benchmark(
        self,
        seed: u32,
        max_iterations: usize,
        plutus_version: &PlutusVersion,
    ) -> Vec<BenchmarkResult> {
        let mut results = Vec::with_capacity(max_iterations);
        let mut iteration = 0;
        let mut prng = Prng::from_seed(seed);

        while max_iterations > iteration {
            let fuzzer = self
                .sampler
                .program
                .apply_data(Data::integer(num_bigint::BigInt::from(iteration as i64)));
            match prng.sample(&fuzzer) {
                Ok(Some((new_prng, value))) => {
                    prng = new_prng;
                    let mut eval_result = self.eval(&value, plutus_version);
                    results.push(BenchmarkResult {
                        test: self.clone(),
                        cost: eval_result.cost(),
                        success: true,
                        traces: eval_result.logs().to_vec(),
                    });
                }

                Ok(None) => {
                    break;
                }
                Err(e) => {
                    results.push(BenchmarkResult {
                        test: self.clone(),
                        cost: ExBudget::default(),
                        success: false,
                        traces: vec![format!("Fuzzer error: {}", e)],
                    });
                    break;
                }
            }
            iteration += 1;
        }

        results
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
        // iteration: usize,
    ) -> Result<Option<(Prng, PlutusData)>, FuzzerError> {
        let program = Program::<NamedDeBruijn>::try_from(fuzzer.apply_data(self.uplc())).unwrap();
        let mut result = program.eval(ExBudget::max());
        result
            .result()
            .map_err(|uplc_error| FuzzerError {
                traces: result.logs(),
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
                    if let [PlutusData::BoundedBytes(bytes), PlutusData::BoundedBytes(choices)] =
                        &fields[..]
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
                duration: now.elapsed(),
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
    Benchmark(BenchmarkResult),
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
            TestResult::Benchmark(result) => TestResult::Benchmark(result),
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
            TestResult::Benchmark(BenchmarkResult { success, .. }) => *success,
        }
    }

    pub fn module(&self) -> &str {
        match self {
            TestResult::UnitTestResult(UnitTestResult { ref test, .. }) => test.module.as_str(),
            TestResult::PropertyTestResult(PropertyTestResult { ref test, .. }) => {
                test.module.as_str()
            }
            TestResult::Benchmark(BenchmarkResult { ref test, .. }) => test.module.as_str(),
        }
    }

    pub fn title(&self) -> &str {
        match self {
            TestResult::UnitTestResult(UnitTestResult { ref test, .. }) => test.name.as_str(),
            TestResult::PropertyTestResult(PropertyTestResult { ref test, .. }) => {
                test.name.as_str()
            }
            TestResult::Benchmark(BenchmarkResult { ref test, .. }) => test.name.as_str(),
        }
    }

    pub fn traces(&self) -> &[String] {
        match self {
            TestResult::UnitTestResult(UnitTestResult { traces, .. })
            | TestResult::PropertyTestResult(PropertyTestResult { traces, .. }) => traces,
            TestResult::Benchmark(BenchmarkResult { traces, .. }) => traces,
        }
    }
}

#[derive(Debug, Clone)]
pub struct UnitTestResult<T> {
    pub success: bool,
    pub spent_budget: ExBudget,
    pub traces: Vec<String>,
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
            traces: self.traces,
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
                        UntypedExpr::reify_constant(data_types, cst, &tipo)
                            .expect("failed to reify assertion operand?")
                    }),
                    tail: assertion.tail.map(|xs| {
                        xs.mapped(|(cst, tipo)| {
                            UntypedExpr::reify_constant(data_types, cst, &tipo)
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
    pub traces: Vec<String>,
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
                    UntypedExpr::reify_data(data_types, counterexample, &self.test.fuzzer.type_info)
                        .expect("failed to reify counterexample?")
                })
            }),
            iterations: self.iterations,
            test: self.test,
            labels: self.labels,
            traces: self.traces,
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
                if let [IfBranch {
                    condition, body, ..
                }] = &branches[..]
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
    pub test: Benchmark,
    pub cost: ExBudget,
    pub success: bool,
    pub traces: Vec<String>,
}

unsafe impl Send for BenchmarkResult {}
unsafe impl Sync for BenchmarkResult {}

#[cfg(test)]
mod test {
    use super::*;

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
