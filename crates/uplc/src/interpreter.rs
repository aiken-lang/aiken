use flat_rs::de;

use crate::{
    ast::{Constant, NamedDeBruijn, Term},
    builtins::DefaultFunction,
};

#[repr(u8)]
#[derive(Debug, Clone, PartialEq, Copy)]
enum StepKind {
    BConst = 0,
    BVar = 1,
    BLamAbs = 2,
    BApply = 3,
    BDelay = 4,
    BForce = 5,
    BBuiltin = 6,
}
impl TryFrom<u8> for StepKind {
    type Error = de::Error;

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        match value {
            0 => Ok(StepKind::BConst),
            1 => Ok(StepKind::BVar),
            2 => Ok(StepKind::BLamAbs),
            3 => Ok(StepKind::BApply),
            4 => Ok(StepKind::BDelay),
            5 => Ok(StepKind::BForce),
            6 => Ok(StepKind::BBuiltin),
            v => Err(de::Error::Message(format!(
                "Default Function not found - {}",
                v
            ))),
        }
    }
}

enum ExBudgetCategory {
    BStep(StepKind),
    BBuiltinApp(DefaultFunction),
    BStartup,
}
/// Can be negative
#[derive(Debug, Clone, PartialEq)]
struct ExBudget {
    mem: i32,
    cpu: i32,
}

impl std::ops::Mul<i32> for ExBudget {
    type Output = ExBudget;

    fn mul(self, rhs: i32) -> ExBudget {
        ExBudget {
            mem: self.mem * rhs,
            cpu: self.cpu * rhs,
        }
    }
}

enum CekValue {
    VCon(Constant),
    VDelay(Term<NamedDeBruijn>, Vec<CekValue>),
    VLamAbs(NamedDeBruijn, Term<NamedDeBruijn>, Vec<CekValue>),
    VBuiltin(
        DefaultFunction,
        Term<NamedDeBruijn>,
        // Need to figure out run time stuff
        // BuiltinRuntime (CekValue uni fun)
    ),
}

enum Context {
    FrameApplyFun(Term<NamedDeBruijn>, Term<NamedDeBruijn>),
    FrameApplyArg(Vec<CekValue>, Term<NamedDeBruijn>, Box<Context>),
    FrameForce(Box<Context>),
    NoFrame,
}

// For now let's just start with running cek on term with generic params
fn run_cek_debruijn(term: Term<NamedDeBruijn>) -> (Term<NamedDeBruijn>, usize, Vec<String>) {
    //paramerterize this
    let mut initial_budget = ExBudget {
        mem: 1000,
        cpu: 1000,
    };
    let startup_budget = ExBudget { mem: 10, cpu: 10 };
    spend_budget_cek(
        &mut initial_budget,
        ExBudgetCategory::BStartup,
        startup_budget,
    );
    let evaluation = enter_compute_cek(Context::NoFrame, Vec::new(), term, &mut initial_budget);
    todo!()
}
fn enter_compute_cek(
    frame: Context,
    env: Vec<CekValue>,
    term: Term<NamedDeBruijn>,
    current_budget: &mut ExBudget,
) -> (Term<NamedDeBruijn>, usize, Vec<u32>) {
    //enter compute
    compute_cek(&mut Vec::with_capacity(8), frame, env, term, current_budget)
}

fn compute_cek(
    unbudgeted_steps: &mut Vec<u32>,
    frame: Context,
    env: Vec<CekValue>,
    term: Term<NamedDeBruijn>,
    current_budget: &mut ExBudget,
) -> (Term<NamedDeBruijn>, usize, Vec<u32>) {
    //TODO: parameterize slippage
    let slippage = 200;
    match (term) {
        a @ Term::Var(_) => {
            unbudgeted_steps[2] += 1;
            unbudgeted_steps[7] += 1;
            if unbudgeted_steps[7] >= slippage {
                spend_accumulated_budget_cek(unbudgeted_steps, current_budget);
            }
            todo!()
        }
        Term::Delay(_) => todo!(),
        Term::Lambda {
            parameter_name,
            body,
        } => todo!(),
        Term::Apply { function, argument } => todo!(),
        Term::Constant(_) => todo!(),
        Term::Force(_) => todo!(),
        Term::Error => todo!(),
        Term::Builtin(_) => todo!(),
    }
}

fn spend_accumulated_budget_cek(unbudgeted_steps: &mut Vec<u32>, current_budget: &mut ExBudget) {
    //only spend each step kind and not total which is index
    for i in 0..unbudgeted_steps.len() - 1 {
        let step = StepKind::try_from(i as u8).unwrap();
        spend_budget_cek(
            current_budget,
            ExBudgetCategory::BStep(step),
            get_cost_by_step(step) * unbudgeted_steps[i] as i32,
        );
    }
}

fn spend_budget_cek(
    current_budget: &mut ExBudget,
    category: ExBudgetCategory,
    spend_budget: ExBudget,
) {
    current_budget.mem -= spend_budget.mem;
    current_budget.cpu -= spend_budget.cpu;
    if current_budget.mem < 0 || current_budget.cpu < 0 {
        panic!("Budget exhausted {:?}", current_budget);
    }
}

fn get_cost_by_step(step: StepKind) -> ExBudget {
    match (step) {
        StepKind::BConst => ExBudget { mem: 100, cpu: 100 },
        StepKind::BVar => ExBudget { mem: 100, cpu: 100 },
        StepKind::BLamAbs => ExBudget { mem: 100, cpu: 100 },
        StepKind::BApply => ExBudget { mem: 100, cpu: 100 },
        StepKind::BDelay => ExBudget { mem: 100, cpu: 100 },
        StepKind::BForce => ExBudget { mem: 100, cpu: 100 },
        StepKind::BBuiltin => ExBudget { mem: 100, cpu: 100 },
    }
}
