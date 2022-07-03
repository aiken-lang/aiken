use crate::{
    ast::{Constant, NamedDeBruijn, Term},
    builtins::DefaultFunction,
};

enum StepKind {
    BConst = 0,
    BVar = 1,
    BLamAbs = 2,
    BApply = 3,
    BDelay = 4,
    BForce = 5,
    BBuiltin = 6,
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
    let initial_budget = ExBudget {
        mem: 1000,
        cpu: 1000,
    };

    let evaluation = enter_compute_cek(Context::NoFrame, Vec::new(), term);
    todo!()
}

fn spend_budget_cek(
    current_budget: ExBudget,
    category: ExBudgetCategory,
    spend_budget: ExBudget,
) -> ExBudget {
    let new_budget = ExBudget {
        mem: current_budget.mem - spend_budget.mem,
        cpu: current_budget.cpu - spend_budget.cpu,
    };
    if new_budget.mem < 0 || new_budget.cpu < 0 {
        panic!("Budget exhausted {:?}", new_budget);
    }
    new_budget
}

fn enter_compute_cek(
    frame: Context,
    env: Vec<CekValue>,
    term: Term<NamedDeBruijn>,
) -> (Term<NamedDeBruijn>, usize, Vec<u32>) {
    //enter compute
    compute_cek(Vec::with_capacity(8), frame, env, term)
}

fn compute_cek(
    mut unbudgeted_steps: Vec<u32>,
    frame: Context,
    env: Vec<CekValue>,
    term: Term<NamedDeBruijn>,
) -> (Term<NamedDeBruijn>, usize, Vec<u32>) {
    //TODO: parameterize slippage
    let slippage = 200;
    match (term) {
        a @ Term::Var(_) => {
            unbudgeted_steps[2] += 1;
            unbudgeted_steps[7] += 1;
            //TODO spend budget one slippage is met
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

