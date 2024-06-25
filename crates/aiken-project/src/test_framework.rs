use aiken_lang::ast::OnTestFailure;
pub(crate) use aiken_lang::{
    ast::{BinOp, DataTypeKey, IfBranch, Span, TypedArg, TypedDataType, TypedTest},
    builtins::bool,
    expr::{TypedExpr, UntypedExpr},
    format::Formatter,
    gen_uplc::CodeGenerator,
    plutus_version::PlutusVersion,
    tipo::{convert_opaque_type, Type},
};
use cryptoxide::{blake2b::Blake2b, digest::Digest};
use indexmap::IndexMap;
use itertools::Itertools;
use owo_colors::{OwoColorize, Stream};
use pallas::ledger::primitives::alonzo::{Constr, PlutusData};
use patricia_tree::PatriciaMap;
use std::{
    borrow::Borrow, collections::BTreeMap, convert::TryFrom, ops::Deref, path::PathBuf, rc::Rc,
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

    pub fn from_function_definition(
        generator: &mut CodeGenerator<'_>,
        test: TypedTest,
        module_name: String,
        input_path: PathBuf,
    ) -> Test {
        if test.arguments.is_empty() {
            Self::unit_test(generator, test, module_name, input_path)
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

            // NOTE: We need not to pass any parameter to the fuzzer here because the fuzzer
            // argument is a Data constructor which needs not any conversion. So we can just safely
            // apply onto it later.
            let fuzzer = generator.clone().generate_raw(&via, &[], &module_name);

            Self::property_test(
                input_path,
                module_name,
                test.name,
                test.on_test_failure,
                program,
                Fuzzer {
                    program: fuzzer,
                    stripped_type_info,
                    type_info,
                },
            )
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
    pub fn run<T>(self, plutus_version: &PlutusVersion) -> TestResult<(Constant, Rc<Type>), T> {
        let mut eval_result = Program::<NamedDeBruijn>::try_from(self.program.clone())
            .unwrap()
            .eval_version(ExBudget::max(), &plutus_version.into());

        let success = !eval_result.failed(match self.on_test_failure {
            OnTestFailure::SucceedEventually | OnTestFailure::SucceedImmediately => true,
            OnTestFailure::FailImmediately => false,
        });

        TestResult::UnitTestResult(UnitTestResult {
            success,
            test: self.to_owned(),
            spent_budget: eval_result.cost(),
            traces: eval_result.logs(),
            assertion: self.assertion,
        })
    }
}

/// ----- PropertyTest -----------------------------------------------------------------
///
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

    fn run_n_times<'a>(
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
    ///   type Fuzzer<a> = fn(Prng) -> Option<(Prng, a)>
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
                            uplc: PlutusData::Constr(Constr {
                                tag: 121 + Prng::SEEDED,
                                fields: vec![
                                    PlutusData::BoundedBytes(bytes.to_owned()),
                                    // Clear choices between seeded runs, to not
                                    // accumulate ALL choices ever made.
                                    PlutusData::BoundedBytes(vec![].into()),
                                ],
                                any_constructor: None,
                            }),
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

impl<'a> Counterexample<'a> {
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
    fn simplify(&mut self) {
        let mut prev;

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
                    self.binary_search_replace(0, self.choices[i], |v| vec![(i, v)]);
                    (i, underflow) = i.overflowing_sub(1);
                }

                // Sort out of orders chunks in ascending order
                let mut k = 8;
                while k > 1 {
                    let mut i = self.choices.len() - 1;
                    while i >= k {
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

#[derive(Debug)]
pub enum TestResult<U, T> {
    UnitTestResult(UnitTestResult<U>),
    PropertyTestResult(PropertyTestResult<T>),
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
        }
    }

    pub fn module(&self) -> &str {
        match self {
            TestResult::UnitTestResult(UnitTestResult { ref test, .. }) => test.module.as_str(),
            TestResult::PropertyTestResult(PropertyTestResult { ref test, .. }) => {
                test.module.as_str()
            }
        }
    }

    pub fn title(&self) -> &str {
        match self {
            TestResult::UnitTestResult(UnitTestResult { ref test, .. }) => test.name.as_str(),
            TestResult::PropertyTestResult(PropertyTestResult { ref test, .. }) => {
                test.name.as_str()
            }
        }
    }

    pub fn traces(&self) -> &[String] {
        match self {
            TestResult::UnitTestResult(UnitTestResult { ref traces, .. })
            | TestResult::PropertyTestResult(PropertyTestResult { ref traces, .. }) => {
                traces.as_slice()
            }
        }
    }

    pub fn into_error(&self, verbose: bool) -> crate::Error {
        let (name, path, src) = match self {
            TestResult::UnitTestResult(UnitTestResult { test, .. }) => (
                test.name.to_string(),
                test.input_path.to_path_buf(),
                test.program.to_pretty(),
            ),
            TestResult::PropertyTestResult(PropertyTestResult { test, .. }) => (
                test.name.to_string(),
                test.input_path.to_path_buf(),
                test.program.to_pretty(),
            ),
        };
        crate::Error::TestFailure {
            name,
            path,
            src,
            verbose,
        }
    }
}

#[derive(Debug)]
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

#[derive(Debug)]
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
                        .expect("Failed to reify counterexample?")
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
            } if tipo == bool() => {
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
                        } => name == "True" && constructor.tipo == bool(),
                        _ => false,
                    };

                    let else_is_wrapped_false = match *final_else {
                        TypedExpr::Trace { then, .. } => match *then {
                            TypedExpr::Var {
                                name, constructor, ..
                            } => name == "False" && constructor.tipo == bool(),
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

impl Assertion<UntypedExpr> {
    #[allow(clippy::just_underscores_and_digits)]
    pub fn to_string(&self, stream: Stream, expect_failure: bool) -> String {
        let red = |s: &str| {
            format!("× {s}")
                .if_supports_color(stream, |s| s.red())
                .if_supports_color(stream, |s| s.bold())
                .to_string()
        };

        // head did not map to a constant
        if self.head.is_err() {
            return red("program failed");
        }

        // any value in tail did not map to a constant
        if self.tail.is_err() {
            return red("program failed");
        }

        fn fmt_side(side: &UntypedExpr, stream: Stream) -> String {
            let __ = "│".if_supports_color(stream, |s| s.red());

            Formatter::new()
                .expr(side, false)
                .to_pretty_string(60)
                .lines()
                .map(|line| format!("{__} {line}"))
                .collect::<Vec<String>>()
                .join("\n")
        }

        let left = fmt_side(self.head.as_ref().unwrap(), stream);

        let tail = self.tail.as_ref().unwrap();

        let right = fmt_side(tail.first(), stream);

        format!(
            "{}{}{}",
            red("expected"),
            if expect_failure && self.bin_op == BinOp::Or {
                " neither\n"
                    .if_supports_color(stream, |s| s.red())
                    .if_supports_color(stream, |s| s.bold())
                    .to_string()
            } else {
                "\n".to_string()
            },
            if expect_failure {
                match self.bin_op {
                    BinOp::And => [
                        left,
                        red("and"),
                        [
                            tail.mapped_ref(|s| fmt_side(s, stream))
                                .join(format!("\n{}\n", red("and")).as_str()),
                            if tail.len() > 1 {
                                red("to not all be true")
                            } else {
                                red("to not both be true")
                            },
                        ]
                        .join("\n"),
                    ],
                    BinOp::Or => [
                        left,
                        red("nor"),
                        [
                            tail.mapped_ref(|s| fmt_side(s, stream))
                                .join(format!("\n{}\n", red("nor")).as_str()),
                            red("to be true"),
                        ]
                        .join("\n"),
                    ],
                    BinOp::Eq => [left, red("to not equal"), right],
                    BinOp::NotEq => [left, red("to not be different"), right],
                    BinOp::LtInt => [left, red("to not be lower than"), right],
                    BinOp::LtEqInt => [left, red("to not be lower than or equal to"), right],
                    BinOp::GtInt => [left, red("to not be greater than"), right],
                    BinOp::GtEqInt => [left, red("to not be greater than or equal to"), right],
                    _ => unreachable!("unexpected non-boolean binary operator in assertion?"),
                }
                .join("\n")
            } else {
                match self.bin_op {
                    BinOp::And => [
                        left,
                        red("and"),
                        [
                            tail.mapped_ref(|s| fmt_side(s, stream))
                                .join(format!("\n{}\n", red("and")).as_str()),
                            if tail.len() > 1 {
                                red("to all be true")
                            } else {
                                red("to both be true")
                            },
                        ]
                        .join("\n"),
                    ],
                    BinOp::Or => [
                        left,
                        red("or"),
                        [
                            tail.mapped_ref(|s| fmt_side(s, stream))
                                .join(format!("\n{}\n", red("or")).as_str()),
                            red("to be true"),
                        ]
                        .join("\n"),
                    ],
                    BinOp::Eq => [left, red("to equal"), right],
                    BinOp::NotEq => [left, red("to not equal"), right],
                    BinOp::LtInt => [left, red("to be lower than"), right],
                    BinOp::LtEqInt => [left, red("to be lower than or equal to"), right],
                    BinOp::GtInt => [left, red("to be greater than"), right],
                    BinOp::GtEqInt => [left, red("to be greater than or equal to"), right],
                    _ => unreachable!("unexpected non-boolean binary operator in assertion?"),
                }
                .join("\n")
            }
        )
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::{
        module::{CheckedModule, CheckedModules},
        utils,
    };
    use aiken_lang::{
        ast::{Definition, ModuleKind, TraceLevel, Tracing},
        builtins,
        format::Formatter,
        line_numbers::LineNumbers,
        parser::{self, extra::ModuleExtra},
        plutus_version::PlutusVersion,
        IdGenerator,
    };
    use indoc::indoc;
    use std::collections::HashMap;

    const TEST_KIND: ModuleKind = ModuleKind::Lib;

    impl Test {
        pub fn from_source(src: &str) -> (Self, IndexMap<DataTypeKey, TypedDataType>) {
            let id_gen = IdGenerator::new();

            let module_name = "";

            let mut module_types = HashMap::new();
            module_types.insert("aiken".to_string(), builtins::prelude(&id_gen));
            module_types.insert("aiken/builtin".to_string(), builtins::plutus(&id_gen));

            let mut warnings = vec![];
            let (ast, _) = parser::module(src, TEST_KIND).expect("Failed to parse module");
            let ast = ast
                .infer(
                    &id_gen,
                    TEST_KIND,
                    module_name,
                    &module_types,
                    Tracing::All(TraceLevel::Verbose),
                    &mut warnings,
                )
                .expect("Failed to type-check module.");

            module_types.insert(module_name.to_string(), ast.type_info.clone());

            let test = ast
                .definitions()
                .filter_map(|def| match def {
                    Definition::Test(test) => Some(test.clone()),
                    _ => None,
                })
                .last()
                .expect("No test found in declared src?");

            let mut functions = builtins::prelude_functions(&id_gen);
            let mut data_types = builtins::prelude_data_types(&id_gen);
            ast.register_definitions(&mut functions, &mut data_types);

            let mut module_sources = HashMap::new();
            module_sources.insert(
                module_name.to_string(),
                (src.to_string(), LineNumbers::new(src)),
            );

            let mut modules = CheckedModules::default();
            modules.insert(
                module_name.to_string(),
                CheckedModule {
                    kind: TEST_KIND,
                    extra: ModuleExtra::default(),
                    name: module_name.to_string(),
                    code: src.to_string(),
                    ast,
                    package: String::new(),
                    input_path: PathBuf::new(),
                },
            );

            let mut generator = CodeGenerator::new(
                PlutusVersion::default(),
                utils::indexmap::as_ref_values(&functions),
                utils::indexmap::as_ref_values(&data_types),
                utils::indexmap::as_str_ref_values(&module_types),
                utils::indexmap::as_str_ref_values(&module_sources),
                Tracing::All(TraceLevel::Verbose),
            );

            (
                Self::from_function_definition(
                    &mut generator,
                    test.to_owned(),
                    module_name.to_string(),
                    PathBuf::new(),
                ),
                data_types,
            )
        }
    }

    fn property(src: &str) -> (PropertyTest, impl Fn(PlutusData) -> String) {
        let prelude = indoc! { r#"
            use aiken/builtin

            const max_int: Int = 255

            pub fn int() -> Fuzzer<Int> {
              fn(prng: PRNG) -> Option<(PRNG, Int)> {
                when prng is {
                  Seeded { seed, choices } -> {
                     let choice =
                       seed
                         |> builtin.index_bytearray(0)

                     Some((
                       Seeded {
                         seed: builtin.blake2b_256(seed),
                         choices: builtin.cons_bytearray(choice, choices)
                       },
                       choice
                     ))
                  }

                  Replayed { cursor, choices } -> {
                    if cursor >= 1 {
                        let cursor = cursor - 1
                        Some((
                          Replayed { choices, cursor },
                          builtin.index_bytearray(choices, cursor)
                        ))
                    } else {
                        None
                    }
                  }
                }
              }
            }

            fn bool() -> Fuzzer<Bool> {
              int() |> map(fn(n) { n % 2 == 0 })
            }

            fn bytearray() -> Fuzzer<ByteArray> {
              int()
                |> map(
                     fn(n) {
                       n
                         |> builtin.integer_to_bytearray(True, 32, _)
                         |> builtin.blake2b_256()
                         |> builtin.slice_bytearray(8, 4, _)
                     },
                   )
            }

            pub fn constant(a: a) -> Fuzzer<a> {
              fn(s0) { Some((s0, a)) }
            }

            pub fn and_then(fuzz_a: Fuzzer<a>, f: fn(a) -> Fuzzer<b>) -> Fuzzer<b> {
              fn(s0) {
                when fuzz_a(s0) is {
                  Some((s1, a)) -> f(a)(s1)
                  None -> None
                }
              }
            }

            pub fn map(fuzz_a: Fuzzer<a>, f: fn(a) -> b) -> Fuzzer<b> {
              fn(s0) {
                when fuzz_a(s0) is {
                  Some((s1, a)) -> Some((s1, f(a)))
                  None -> None
                }
              }
            }

            pub fn map2(fuzz_a: Fuzzer<a>, fuzz_b: Fuzzer<b>, f: fn(a, b) -> c) -> Fuzzer<c> {
              fn(s0) {
                when fuzz_a(s0) is {
                  Some((s1, a)) ->
                    when fuzz_b(s1) is {
                      Some((s2, b)) -> Some((s2, f(a, b)))
                      None -> None
                    }
                  None -> None
                }
              }
            }
        "#};

        let src = format!("{prelude}\n{src}");

        match Test::from_source(&src) {
            (Test::PropertyTest(test), data_types) => {
                let type_info = test.fuzzer.type_info.clone();

                let reify = move |counterexample| {
                    let data_type_refs = utils::indexmap::as_ref_values(&data_types);
                    let expr = UntypedExpr::reify_data(&data_type_refs, counterexample, &type_info)
                        .expect("Failed to reify value.");
                    Formatter::new().expr(&expr, false).to_pretty_string(70)
                };

                (test, reify)
            }
            (Test::UnitTest(..), _) => {
                panic!("Expected to yield a PropertyTest but found a UnitTest")
            }
        }
    }

    impl PropertyTest {
        fn expect_failure<'a>(&'a self, plutus_version: &'a PlutusVersion) -> Counterexample<'a> {
            let mut labels = BTreeMap::new();
            let mut remaining = PropertyTest::DEFAULT_MAX_SUCCESS;
            match self.run_n_times(
                &mut remaining,
                Prng::from_seed(42),
                &mut labels,
                plutus_version,
            ) {
                Ok(Some(counterexample)) => counterexample,
                _ => panic!("expected property to fail but it didn't."),
            }
        }
    }

    #[test]
    fn test_prop_basic() {
        let (prop, _) = property(indoc! { r#"
            test foo(n: Int via int()) {
                n >= 0
            }
        "#});

        assert!(prop
            .run::<()>(
                42,
                PropertyTest::DEFAULT_MAX_SUCCESS,
                &PlutusVersion::default()
            )
            .is_success());
    }

    #[test]
    fn test_prop_labels() {
        let (prop, _) = property(indoc! { r#"
            fn label(str: String) -> Void {
              str
                |> builtin.append_string(@"\0", _)
                |> builtin.debug(Void)
            }

            test foo(head_or_tail via bool()) {
                if head_or_tail {
                    label(@"head")
                } else {
                    label(@"tail")
                }
                True
            }
        "#});

        match prop.run::<()>(
            42,
            PropertyTest::DEFAULT_MAX_SUCCESS,
            &PlutusVersion::default(),
        ) {
            TestResult::UnitTestResult(..) => unreachable!("property returned unit-test result ?!"),
            TestResult::PropertyTestResult(result) => {
                assert!(
                    result
                        .labels
                        .iter()
                        .eq(vec![(&"head".to_string(), &53), (&"tail".to_string(), &47)]),
                    "labels: {:#?}",
                    result.labels
                )
            }
        }
    }

    #[test]
    fn test_prop_always_odd() {
        let (prop, reify) = property(indoc! { r#"
            test foo(n: Int via int()) {
                n % 2 == 0
            }
        "#});

        let plutus_version = PlutusVersion::default();
        let mut counterexample = prop.expect_failure(&plutus_version);

        counterexample.simplify();

        assert_eq!(counterexample.choices, vec![1]);
        assert_eq!(reify(counterexample.value), "1");
    }

    #[test]
    fn test_prop_combine() {
        let (prop, reify) = property(indoc! { r#"
            fn pair(fuzz_a: Fuzzer<a>, fuzz_b: Fuzzer<b>) -> Fuzzer<(a, b)> {
                fuzz_a
                    |> and_then(fn(a) {
                        fuzz_b
                            |> map(fn(b) {
                                (a, b)
                            })
                    })
            }


            test foo(t: (Int, Int) via pair(int(), int())) {
                t.1st + t.2nd <= 400
            }
        "#});

        let plutus_version = PlutusVersion::default();
        let mut counterexample = prop.expect_failure(&plutus_version);

        counterexample.simplify();

        assert_eq!(counterexample.choices, vec![149, 252]);
        assert_eq!(reify(counterexample.value), "(149, 252)");
    }

    #[test]
    fn test_prop_enum_bool() {
        let (prop, reify) = property(indoc! { r#"
            test foo(predicate via bool()) {
                predicate
            }
        "#});

        let plutus_version = PlutusVersion::default();
        let mut counterexample = prop.expect_failure(&plutus_version);

        counterexample.simplify();

        assert_eq!(counterexample.choices, vec![1]);
        assert_eq!(reify(counterexample.value), "False");
    }

    #[test]
    fn test_prop_enum_custom() {
        let (prop, reify) = property(indoc! { r#"
            type Temperature {
                Hot
                Cold
            }

            fn temperature() -> Fuzzer<Temperature> {
                bool() |> map(fn(is_cold) {
                    if is_cold { Cold } else { Hot }
                })
            }

            test foo(t via temperature()) {
                t == Hot
            }
        "#});

        let plutus_version = PlutusVersion::default();
        let mut counterexample = prop.expect_failure(&plutus_version);

        counterexample.simplify();

        assert_eq!(counterexample.choices, vec![0]);
        assert_eq!(reify(counterexample.value), "Cold");
    }

    #[test]
    fn test_prop_opaque() {
        let (prop, reify) = property(indoc! { r#"
            opaque type Temperature {
                Hot
                Cold
            }

            fn temperature() -> Fuzzer<Temperature> {
                bool() |> map(fn(is_cold) {
                    if is_cold { Cold } else { Hot }
                })
            }

            test foo(t via temperature()) {
                t == Hot
            }
        "#});

        let plutus_version = PlutusVersion::default();
        let mut counterexample = prop.expect_failure(&plutus_version);

        counterexample.simplify();

        assert_eq!(counterexample.choices, vec![0]);
        assert_eq!(reify(counterexample.value), "Cold");
    }

    #[test]
    fn test_prop_private_enum() {
        let (prop, reify) = property(indoc! { r#"
            type Vehicle {
                Car { wheels: Int }
                Bike { wheels: Int }
            }

            fn vehicle() -> Fuzzer<Vehicle> {
                bool() |> map(fn(is_car) {
                    if is_car { Car(4) } else { Bike(2) }
                })
            }

            test foo(v via vehicle()) {
                when v is {
                    Car { wheels } -> wheels
                    Bike { wheels } -> wheels
                } == 4
            }
        "#});

        let plutus_version = PlutusVersion::default();
        let mut counterexample = prop.expect_failure(&plutus_version);

        counterexample.simplify();

        assert_eq!(counterexample.choices, vec![1]);
        assert_eq!(reify(counterexample.value), "Bike { wheels: 2 }");
    }

    #[test]
    fn test_prop_list() {
        let (prop, reify) = property(indoc! { r#"
            fn list(elem: Fuzzer<a>) -> Fuzzer<List<a>> {
              bool()
                |> and_then(fn(continue) {
                    if continue {
                      map2(elem, list(elem), fn(head, tail) { [head, ..tail] })
                    } else {
                      constant([])
                    }
                })
            }

            fn length(es: List<a>) -> Int {
              when es is {
                [] -> 0
                [_, ..tail] -> 1 + length(tail)
              }
            }

            test foo(es: List<Int> via list(int())) {
              length(es) < 3
            }
        "#});

        let plutus_version = PlutusVersion::default();

        let mut counterexample = prop.expect_failure(&plutus_version);

        counterexample.simplify();

        assert_eq!(counterexample.choices, vec![0, 0, 0, 0, 0, 0, 1]);
        assert_eq!(reify(counterexample.value), "[0, 0, 0]");
    }

    #[test]
    fn test_prop_opaque_dict() {
        let (prop, reify) = property(indoc! { r#"
            pub opaque type Dict<a> {
              inner: List<(ByteArray, a)>,
            }

            fn dict(elem: Fuzzer<a>) -> Fuzzer<Dict<a>> {
              bool()
                |> and_then(
                     fn(continue) {
                       if continue {
                         let kv = map2(bytearray(), elem, fn(k, v) { (k, v) })
                         map2(kv, dict(elem), fn(head, tail) { Dict([head, ..tail.inner]) })
                       } else {
                         constant(Dict([]))
                       }
                     },
                   )
            }

            test foo(d via dict(bool())) {
              d == Dict([])
            }
        "#});

        let plutus_version = PlutusVersion::default();
        let mut counterexample = prop.expect_failure(&plutus_version);

        counterexample.simplify();

        assert_eq!(counterexample.choices, vec![0, 0, 0, 1]);
        assert_eq!(reify(counterexample.value), "Dict([(#\"2cd15ed0\", True)])");
    }

    #[test]
    fn test_prop_opaque_nested_dict() {
        let (prop, reify) = property(indoc! { r#"
            pub opaque type Dict<a> {
              inner: List<(ByteArray, a)>,
            }

            fn dict(elem: Fuzzer<a>) -> Fuzzer<Dict<a>> {
              bool()
                |> and_then(
                     fn(continue) {
                       if continue {
                         let kv = map2(bytearray(), elem, fn(k, v) { (k, v) })
                         map2(kv, dict(elem), fn(head, tail) { Dict([head, ..tail.inner]) })
                       } else {
                         constant(Dict([]))
                       }
                     },
                   )
            }

            test foo(d via dict(dict(int()))) {
              d == Dict([])
            }
        "#});

        let plutus_version = PlutusVersion::default();
        let mut counterexample = prop.expect_failure(&plutus_version);

        counterexample.simplify();

        assert_eq!(counterexample.choices, vec![0, 0, 1, 1]);
        assert_eq!(
            reify(counterexample.value),
            "Dict([(#\"2cd15ed0\", Dict([]))])"
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
