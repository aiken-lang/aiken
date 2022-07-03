use crate::{
    ast::{Constant, DeBruijn, Term},
    builtins::DefaultFunction,
};

mod error;

pub use error::Error;

pub struct Machine {
    costs: Costs,
    ex_budget: ExBudget,
    frames: Vec<Context>,
    slippage: u32,
    env: Vec<Value>,
}

impl Machine {
    pub fn new(costs: Costs, initial_budget: ExBudget, slippage: u32) -> Machine {
        Machine {
            costs,
            ex_budget: initial_budget,
            slippage,
            frames: vec![],
            env: vec![],
        }
    }

    pub fn run(
        &mut self,
        term: &Term<DeBruijn>,
    ) -> Result<(Term<DeBruijn>, usize, Vec<String>), Error> {
        let startup_budget = self.costs.get(StepKind::StartUp);

        self.spend_budget(startup_budget)?;

        self.push_frame(Context::NoFrame);

        self.enter_compute(term)
    }

    fn enter_compute(&mut self, term: &Term<DeBruijn>) {}

    fn spend_budget(&mut self, spend_budget: ExBudget) -> Result<(), Error> {
        self.ex_budget.mem -= spend_budget.mem;
        self.ex_budget.cpu -= spend_budget.cpu;

        if self.ex_budget.mem < 0 || self.ex_budget.cpu < 0 {
            Err(Error::OutOfExError(self.ex_budget))
        } else {
            Ok(())
        }
    }

    fn push_frame(&mut self, frame: Context) {
        self.frames.push(frame);
    }
}

enum Context {
    FrameApplyFun(Term<DeBruijn>, Term<DeBruijn>),
    FrameApplyArg(Vec<Value>, Term<DeBruijn>, Box<Context>),
    FrameForce(Box<Context>),
    NoFrame,
}

enum Value {
    Con(Constant),
    Delay(Term<DeBruijn>, Vec<Value>),
    Lambda(DeBruijn, Term<DeBruijn>, Vec<Value>),
    Builtin(
        DefaultFunction,
        Term<DeBruijn>,
        // Need to figure out run time stuff
        // BuiltinRuntime (CekValue uni fun)
    ),
}

/// Can be negative
#[derive(Debug, Clone, PartialEq)]
struct ExBudget {
    mem: i32,
    cpu: i32,
}

impl ExBudget {
    pub fn occurence(&mut self, n: i32) {
        self.mem *= n;
        self.cpu *= n;
    }
}

enum StepKind {
    Constant,
    Var,
    Lambda,
    Apply,
    Delay,
    Force,
    Builtin,
    StartUp,
}

/// There's no entry for Error since we'll be exiting anyway; also, what would
/// happen if calling 'Error' caused the budget to be exceeded?
struct Costs {
    startup: ExBudget,
    var: ExBudget,
    constant: ExBudget,
    lambda: ExBudget,
    delay: ExBudget,
    force: ExBudget,
    apply: ExBudget,
    /// Just the cost of evaluating a Builtin node, not the builtin itself.
    builtin: ExBudget,
}

impl Costs {
    /// Get the cost of a step
    pub fn get(&self, step: StepKind) -> ExBudget {
        match step {
            StepKind::Constant => self.constant,
            StepKind::Var => self.var,
            StepKind::Lambda => self.lambda,
            StepKind::Apply => self.apply,
            StepKind::Delay => self.delay,
            StepKind::Force => self.force,
            StepKind::Builtin => self.builtin,
            StepKind::StartUp => self.startup,
        }
    }
}
