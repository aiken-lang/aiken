use crate::{pretty, ExBudget};
use aiken_lang::ast::BinOp;
use std::{
    borrow::Borrow,
    fmt::{self, Display},
    path::{Path, PathBuf},
    rc::Rc,
};
use uplc::{
    ast::{Constant, Data, NamedDeBruijn, Program, Term},
    machine::eval_result::EvalResult,
};

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
        evaluation_hint: Option<EvalHint>,
    ) -> Test {
        Test::UnitTest(UnitTest {
            input_path,
            module,
            name,
            program,
            can_error,
            evaluation_hint,
        })
    }

    pub fn property_test(
        input_path: PathBuf,
        module: String,
        name: String,
        can_error: bool,
        program: Program<NamedDeBruijn>,
        fuzzer: Program<NamedDeBruijn>,
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

    pub fn name(&self) -> &str {
        match self {
            Test::UnitTest(test) => &test.name,
            Test::PropertyTest(test) => &test.name,
        }
    }

    pub fn module(&self) -> &str {
        match self {
            Test::UnitTest(test) => &test.module,
            Test::PropertyTest(test) => &test.module,
        }
    }

    pub fn input_path(&self) -> &Path {
        match self {
            Test::UnitTest(test) => &test.input_path,
            Test::PropertyTest(test) => &test.input_path,
        }
    }

    pub fn program(&self) -> &Program<NamedDeBruijn> {
        match self {
            Test::UnitTest(test) => &test.program,
            Test::PropertyTest(test) => &test.program,
        }
    }

    pub fn evaluation_hint(&self) -> Option<&EvalHint> {
        match self {
            Test::UnitTest(test) => test.evaluation_hint.as_ref(),
            Test::PropertyTest(_) => None,
        }
    }

    pub fn report<'a>(&'a self, eval_result: &mut EvalResult) -> EvalInfo<'a> {
        let can_error = match self {
            Test::UnitTest(test) => test.can_error,
            Test::PropertyTest(test) => test.can_error,
        };

        EvalInfo {
            test: self,
            success: !eval_result.failed(can_error),
            spent_budget: eval_result.cost(),
            logs: eval_result.logs(),
            output: eval_result.result().ok(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct UnitTest {
    pub input_path: PathBuf,
    pub module: String,
    pub name: String,
    pub can_error: bool,
    pub program: Program<NamedDeBruijn>,
    pub evaluation_hint: Option<EvalHint>,
}

unsafe impl Send for UnitTest {}

impl UnitTest {
    pub fn run(&self) -> EvalResult {
        self.program.clone().eval(ExBudget::max())
    }
}

#[derive(Debug, Clone)]
pub struct PropertyTest {
    pub input_path: PathBuf,
    pub module: String,
    pub name: String,
    pub can_error: bool,
    pub program: Program<NamedDeBruijn>,
    pub fuzzer: Program<NamedDeBruijn>,
}

unsafe impl Send for PropertyTest {}

impl PropertyTest {
    pub fn new_seed(seed: u32) -> Term<NamedDeBruijn> {
        Term::Constant(Rc::new(Constant::Data(Data::constr(
            0,
            vec![
                Data::integer(seed.into()),
                Data::integer(0.into()), // Size
            ],
        ))))
    }

    pub fn sample(&self, seed: Term<NamedDeBruijn>) -> (Term<NamedDeBruijn>, Term<NamedDeBruijn>) {
        let term = self.fuzzer.apply_term(&seed).eval(ExBudget::max()).result();

        if let Ok(Term::Constant(rc)) = term {
            match &rc.borrow() {
                Constant::ProtoPair(_, _, new_seed, value) => (
                    Term::Constant(new_seed.clone()),
                    Term::Constant(value.clone()),
                ),
                _ => todo!("Fuzzer yielded a new seed that isn't an integer?"),
            }
        } else {
            todo!("Fuzzer yielded something else than a pair? {:#?}", term)
        }
    }

    pub fn run(&self, sample: &Term<NamedDeBruijn>) -> EvalResult {
        self.program.apply_term(sample).eval(ExBudget::max())
    }
}

#[derive(Debug, Clone)]
pub struct EvalHint {
    pub bin_op: BinOp,
    pub left: Program<NamedDeBruijn>,
    pub right: Program<NamedDeBruijn>,
    pub can_error: bool,
}

impl Display for EvalHint {
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

#[derive(Debug)]
pub struct EvalInfo<'a> {
    pub success: bool,
    pub spent_budget: ExBudget,
    pub output: Option<Term<NamedDeBruijn>>,
    pub logs: Vec<String>,
    pub test: &'a Test,
}
