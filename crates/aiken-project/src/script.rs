use crate::{pretty, ExBudget};
use aiken_lang::{
    ast::BinOp,
    expr::UntypedExpr,
    gen_uplc::builder::convert_data_to_type,
    tipo::{Type, TypeInfo},
};
use pallas::{
    codec::utils::Int,
    ledger::primitives::alonzo::{BigInt, Constr, PlutusData},
};
use std::{
    borrow::Borrow,
    collections::HashMap,
    fmt::{self, Display},
    path::PathBuf,
    rc::Rc,
};
use uplc::{
    ast::{Constant, Data, NamedDeBruijn, Program, Term},
    machine::eval_result::EvalResult,
};

/// ----------------------------------------------------------------------------
///
/// Test
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
/// ----------------------------------------------------------------------------
#[derive(Debug, Clone)]
pub enum Test {
    UnitTest(UnitTest),
    PropertyTest(PropertyTest),
}

unsafe impl Send for Test {}

impl Test {
    pub fn unit_test(
        input_path: PathBuf,
        module: String,
        name: String,
        can_error: bool,
        program: Program<NamedDeBruijn>,
        assertion: Option<Assertion>,
    ) -> Test {
        Test::UnitTest(UnitTest {
            input_path,
            module,
            name,
            program,
            can_error,
            assertion,
        })
    }

    pub fn property_test(
        input_path: PathBuf,
        module: String,
        name: String,
        can_error: bool,
        program: Program<NamedDeBruijn>,
        fuzzer: Fuzzer<NamedDeBruijn>,
    ) -> Test {
        Test::PropertyTest(PropertyTest {
            input_path,
            module,
            name,
            program,
            can_error,
            fuzzer,
        })
    }
}

// ----------------------------------------------------------------------------
//
// UnitTest
//
// ----------------------------------------------------------------------------

#[derive(Debug, Clone)]
pub struct UnitTest {
    pub input_path: PathBuf,
    pub module: String,
    pub name: String,
    pub can_error: bool,
    pub program: Program<NamedDeBruijn>,
    pub assertion: Option<Assertion>,
}

unsafe impl Send for UnitTest {}

impl UnitTest {
    pub fn run<T>(self) -> TestResult<T> {
        let mut eval_result = self.program.clone().eval(ExBudget::max());
        TestResult::UnitTestResult(UnitTestResult {
            test: self.to_owned(),
            success: !eval_result.failed(self.can_error),
            spent_budget: eval_result.cost(),
            logs: eval_result.logs(),
            output: eval_result.result().ok(),
        })
    }
}

// ----------------------------------------------------------------------------
//
// PropertyTest
//
// ----------------------------------------------------------------------------

#[derive(Debug, Clone)]
pub struct PropertyTest {
    pub input_path: PathBuf,
    pub module: String,
    pub name: String,
    pub can_error: bool,
    pub program: Program<NamedDeBruijn>,
    pub fuzzer: Fuzzer<NamedDeBruijn>,
}

unsafe impl Send for PropertyTest {}

#[derive(Debug, Clone)]
pub struct Fuzzer<T> {
    pub program: Program<T>,
    pub type_info: Rc<Type>,
}

impl PropertyTest {
    const MAX_TEST_RUN: usize = 100;

    /// Run a property test from a given seed. The property is run at most MAX_TEST_RUN times. It
    /// may stops earlier on failure; in which case a 'counterexample' is returned.
    pub fn run(self, seed: u32) -> TestResult<PlutusData> {
        let n = PropertyTest::MAX_TEST_RUN;

        let (counterexample, iterations) = match self.run_n_times(n, seed, None) {
            None => (None, n),
            Some((remaining, counterexample)) => (Some(counterexample), n - remaining + 1),
        };

        TestResult::PropertyTestResult(PropertyTestResult {
            test: self,
            counterexample,
            iterations,
        })
    }

    fn run_n_times(
        &self,
        remaining: usize,
        seed: u32,
        counterexample: Option<(usize, PlutusData)>,
    ) -> Option<(usize, PlutusData)> {
        // We short-circuit failures in case we have any. The counterexample is already simplified
        // at this point.
        if remaining > 0 && counterexample.is_none() {
            let (next_seed, counterexample) = self.run_once(seed);
            self.run_n_times(
                remaining - 1,
                next_seed,
                counterexample.map(|c| (remaining, c)),
            )
        } else {
            counterexample
        }
    }

    fn run_once(&self, seed: u32) -> (u32, Option<PlutusData>) {
        let (next_prng, value) = Prng::from_seed(seed)
            .sample(&self.fuzzer.program)
            .expect("running seeded Prng cannot fail.");

        let result = self.eval(&value);

        if let Prng::Seeded {
            seed: next_seed, ..
        } = next_prng
        {
            if result.failed(self.can_error) {
                let mut counterexample = Counterexample {
                    value,
                    choices: next_prng.choices(),
                    property: self,
                };

                if !counterexample.choices.is_empty() {
                    counterexample.simplify();
                }

                (next_seed, Some(counterexample.value))
            } else {
                (next_seed, None)
            }
        } else {
            unreachable!("Prng constructed from a seed necessarily yield a seed.");
        }
    }

    pub fn eval(&self, value: &PlutusData) -> EvalResult {
        let term = convert_data_to_type(Term::data(value.clone()), &self.fuzzer.type_info)
            .try_into()
            .expect("safe conversion from Name -> NamedDeBruijn");
        self.program.apply_term(&term).eval(ExBudget::max())
    }
}

// ----------------------------------------------------------------------------
//
// Prng
//
// ----------------------------------------------------------------------------

#[derive(Debug)]
pub enum Prng {
    Seeded {
        seed: u32,
        choices: Vec<u32>,
        uplc: PlutusData,
    },
    Replayed {
        choices: Vec<u32>,
        uplc: PlutusData,
    },
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

    pub fn choices(&self) -> Vec<u32> {
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
        Prng::Seeded {
            seed,
            choices: vec![],
            uplc: Data::constr(
                Prng::SEEDED,
                vec![
                    Data::integer(seed.into()), // Prng's seed
                    Data::list(vec![]),         // Random choices
                ],
            ),
        }
    }

    /// Construct a Pseudo-random number generator from a pre-defined list of choices.
    pub fn from_choices(choices: &[u32]) -> Prng {
        Prng::Replayed {
            choices: choices.to_vec(),
            uplc: Data::constr(
                Prng::REPLAYED,
                vec![Data::list(
                    choices.iter().map(|i| Data::integer((*i).into())).collect(),
                )],
            ),
        }
    }

    /// Generate a pseudo-random value from a fuzzer using the given PRNG.
    pub fn sample(&self, fuzzer: &Program<NamedDeBruijn>) -> Option<(Prng, PlutusData)> {
        let result = fuzzer
            .apply_data(self.uplc())
            .eval(ExBudget::max())
            .result()
            .expect("Fuzzer crashed?");

        Prng::from_result(result)
    }

    /// Obtain a Prng back from a fuzzer execution. As a reminder, fuzzers have the following
    /// signature:
    ///
    ///     type Fuzzer<a> = fn(Prng) -> Option<(Prng, a)>
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
                    if let [seed, PlutusData::Array(choices)] = &fields[..] {
                        return Prng::Seeded {
                            seed: as_u32(seed),
                            choices: choices.iter().map(as_u32).collect(),
                            uplc: cst.clone(),
                        };
                    }
                }

                if *tag == 121 + Prng::REPLAYED {
                    if let [PlutusData::Array(choices)] = &fields[..] {
                        return Prng::Replayed {
                            choices: choices.iter().map(as_u32).collect(),
                            uplc: cst.clone(),
                        };
                    }
                }
            }

            panic!("Malformed Prng: {cst:#?}")
        }

        fn as_u32(field: &PlutusData) -> u32 {
            if let PlutusData::BigInt(BigInt::Int(Int(i))) = field {
                return u32::try_from(*i).expect("Choice doesn't fit in u32?");
            }

            panic!("Malformed choice's value: {field:#?}")
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

        // In principle, this cannot happen provided that the 'result' was produced from a
        // type-checked fuzzer. The type-checker enforces that fuzzers are of the right shape
        // describe above.
        unreachable!("Fuzzer yielded a malformed result? {result:#?}")
    }
}

// ----------------------------------------------------------------------------
//
// Counterexample
//
// A counterexample is constructed on test failures.
//
// ----------------------------------------------------------------------------

#[derive(Debug)]
pub struct Counterexample<'a> {
    pub value: PlutusData,
    pub choices: Vec<u32>,
    pub property: &'a PropertyTest,
}

impl<'a> Counterexample<'a> {
    fn consider(&mut self, choices: &[u32]) -> bool {
        if choices == self.choices {
            return true;
        }

        // TODO: Memoize test cases & choices in a cache. Due to the nature of
        // our integrated shrinking approach, we may end up re-executing the same
        // test cases many times. Given that tests are fully deterministic, we can
        // memoize the already seen choices to avoid re-running the generators and
        // the test (which can be quite expensive).
        match Prng::from_choices(choices).sample(&self.property.fuzzer.program) {
            // Shrinked choices led to an impossible generation.
            None => false,

            // Shrinked choices let to a new valid generated value, now, is it better?
            Some((_, value)) => {
                let result = self.property.eval(&value);

                // If the test no longer fails, it isn't better as we're only
                // interested in counterexamples.
                if !result.failed(self.property.can_error) {
                    return false;
                }

                // If these new choices are shorter or smaller, then we pick them
                // as new choices and inform that it's been an improvement.
                if choices.len() <= self.choices.len() || choices < &self.choices {
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
    /// - Decrementing chunks of values
    /// - Replacing chunks of values
    /// - Sorting chunks
    /// - Redistribute values between nearby pairs
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
                if k > self.choices.len() {
                    break;
                }

                for (i, j) in (0..=self.choices.len() - k).map(|i| (i, i + k)).rev() {
                    let mut choices = [
                        &self.choices[..i],
                        if j < self.choices.len() {
                            &self.choices[j..]
                        } else {
                            &[]
                        },
                    ]
                    .concat();

                    if self.consider(&choices) {
                        break;
                    }

                    // Perform an extra reduction step that decrease the size of choices near
                    // the end, to cope with dependencies between choices, e.g. drawing a
                    // number as a list length, and then drawing that many elements.
                    //
                    // This isn't perfect, but allows to make progresses in many cases.
                    if i > 0 && choices[i - 1] > 0 {
                        choices[i - 1] -= 1;
                        if self.consider(&choices) {
                            break;
                        };
                    }
                }

                k /= 2
            }

            // Now we try replacing region of choices with zeroes. Note that unlike the above we
            // skip k = 1 because we handle that in the next step. Often (but not always) a block
            // of all zeroes is the smallest value that a region can be.
            let mut k: isize = 8;
            while k > 1 {
                let mut i: isize = self.choices.len() as isize - k;
                while i >= 0 {
                    i -= if self.zeroes(i, k) { k } else { 1 }
                }
                k /= 2
            }

            // TODO: Remaining shrinking strategies...
            //
            // - Swaps
            // - Sorting
            // - Pair adjustments

            // If we've reached a fixed point, then we cannot shrink further. We've reached a
            // (local) minimum, which is as good as a counterexample we'll get with this approach.
            if prev.as_slice() == self.choices.as_slice() {
                break;
            }
        }
    }

    // Replace a block between indices 'i' and 'k' by zeroes.
    fn zeroes(&mut self, i: isize, k: isize) -> bool {
        let mut choices = self.choices.clone();

        for j in i..(i + k) {
            if j >= self.choices.len() as isize {
                return false;
            }
            choices[j as usize] = 0;
        }

        self.consider(&choices)
    }
}

// ----------------------------------------------------------------------------
//
// TestResult
//
// ----------------------------------------------------------------------------

#[derive(Debug)]
pub enum TestResult<T> {
    UnitTestResult(UnitTestResult),
    PropertyTestResult(PropertyTestResult<T>),
}

unsafe impl<T> Send for TestResult<T> {}

impl TestResult<PlutusData> {
    pub fn reify(self, data_types: &HashMap<String, TypeInfo>) -> TestResult<UntypedExpr> {
        match self {
            TestResult::UnitTestResult(test) => TestResult::UnitTestResult(test),
            TestResult::PropertyTestResult(test) => {
                TestResult::PropertyTestResult(test.reify(data_types))
            }
        }
    }
}

impl<T> TestResult<T> {
    pub fn is_success(&self) -> bool {
        match self {
            TestResult::UnitTestResult(UnitTestResult { success, .. }) => *success,
            TestResult::PropertyTestResult(PropertyTestResult {
                counterexample,
                test,
                ..
            }) => {
                if test.can_error {
                    counterexample.is_some()
                } else {
                    counterexample.is_none()
                }
            }
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

    pub fn logs(&self) -> &[String] {
        match self {
            TestResult::UnitTestResult(UnitTestResult { ref logs, .. }) => logs.as_slice(),
            TestResult::PropertyTestResult(..) => &[],
        }
    }

    pub fn into_error(&self, verbose: bool) -> crate::Error {
        let (name, path, assertion, src) = match self {
            TestResult::UnitTestResult(UnitTestResult { test, .. }) => (
                test.name.to_string(),
                test.input_path.to_path_buf(),
                test.assertion.as_ref().map(|hint| hint.to_string()),
                test.program.to_pretty(),
            ),
            TestResult::PropertyTestResult(PropertyTestResult { test, .. }) => (
                test.name.to_string(),
                test.input_path.to_path_buf(),
                None,
                test.program.to_pretty(),
            ),
        };
        crate::Error::TestFailure {
            name,
            path,
            assertion,
            src,
            verbose,
        }
    }
}

#[derive(Debug)]
pub struct UnitTestResult {
    pub success: bool,
    pub spent_budget: ExBudget,
    pub output: Option<Term<NamedDeBruijn>>,
    pub logs: Vec<String>,
    pub test: UnitTest,
}

unsafe impl Send for UnitTestResult {}

#[derive(Debug)]
pub struct PropertyTestResult<T> {
    pub test: PropertyTest,
    pub counterexample: Option<T>,
    pub iterations: usize,
}

unsafe impl<T> Send for PropertyTestResult<T> {}

impl PropertyTestResult<PlutusData> {
    pub fn reify(self, data_types: &HashMap<String, TypeInfo>) -> PropertyTestResult<UntypedExpr> {
        PropertyTestResult {
            counterexample: match self.counterexample {
                None => None,
                Some(counterexample) => Some(
                    UntypedExpr::reify(data_types, counterexample, &self.test.fuzzer.type_info)
                        .expect("Failed to reify counterexample?"),
                ),
            },
            iterations: self.iterations,
            test: self.test,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Assertion {
    pub bin_op: BinOp,
    pub left: Program<NamedDeBruijn>,
    pub right: Program<NamedDeBruijn>,
    pub can_error: bool,
}

impl Display for Assertion {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let unlimited_budget = ExBudget {
            mem: i64::MAX,
            cpu: i64::MAX,
        };

        let left = pretty::boxed(
            "left",
            &match self.left.clone().eval(unlimited_budget).result() {
                Ok(term) => format!("{term}"),
                Err(err) => format!("{err}"),
            },
        );
        let right = pretty::boxed(
            "right",
            &match self.right.clone().eval(unlimited_budget).result() {
                Ok(term) => format!("{term}"),
                Err(err) => format!("{err}"),
            },
        );
        let msg = if self.can_error {
            match self.bin_op {
                BinOp::And => Some(format!(
                    "{left}\n\nand\n\n{right}\n\nare both true but shouldn't."
                )),
                BinOp::Or => Some(format!(
                    "neither\n\n{left}\n\nnor\n\n{right}\n\nshould be true."
                )),
                BinOp::Eq => Some(format!("{left}\n\nshould not be equal to\n\n{right}")),
                BinOp::NotEq => Some(format!("{left}\n\nshould be equal to\n\n{right}")),
                BinOp::LtInt => Some(format!(
                    "{left}\n\nshould be greater than or equal to\n\n{right}"
                )),
                BinOp::LtEqInt => Some(format!("{left}\n\nshould be greater than\n\n{right}")),
                BinOp::GtEqInt => Some(format!(
                    "{left}\n\nshould be lower than or equal\n\n{right}"
                )),
                BinOp::GtInt => Some(format!("{left}\n\nshould be lower than\n\n{right}")),
                _ => None,
            }
        } else {
            match self.bin_op {
                BinOp::And => Some(format!("{left}\n\nand\n\n{right}\n\nshould both be true.")),
                BinOp::Or => Some(format!("{left}\n\nor\n\n{right}\n\nshould be true.")),
                BinOp::Eq => Some(format!("{left}\n\nshould be equal to\n\n{right}")),
                BinOp::NotEq => Some(format!("{left}\n\nshould not be equal to\n\n{right}")),
                BinOp::LtInt => Some(format!("{left}\n\nshould be lower than\n\n{right}")),
                BinOp::LtEqInt => Some(format!(
                    "{left}\n\nshould be lower than or equal to\n\n{right}"
                )),
                BinOp::GtEqInt => Some(format!("{left}\n\nshould be greater than\n\n{right}")),
                BinOp::GtInt => Some(format!(
                    "{left}\n\nshould be greater than or equal to\n\n{right}"
                )),
                _ => None,
            }
        }
        .ok_or(fmt::Error)?;

        f.write_str(&msg)
    }
}
