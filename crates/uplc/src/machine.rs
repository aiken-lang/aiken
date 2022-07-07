use crate::{
    ast::{Constant, NamedDeBruijn, Term},
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
    unbudgeted_steps: Vec<u32>,
}

impl Machine {
    pub fn new(costs: Costs, initial_budget: ExBudget, slippage: u32) -> Machine {
        Machine {
            costs,
            ex_budget: initial_budget,
            slippage,
            frames: vec![Context::NoFrame],
            env: vec![],
            unbudgeted_steps: vec![0; 8],
        }
    }

    pub fn run(
        &mut self,
        term: &Term<NamedDeBruijn>,
    ) -> Result<(Term<NamedDeBruijn>, usize, Vec<String>), Error> {
        let startup_budget = self.costs.get(StepKind::StartUp);

        self.spend_budget(startup_budget)?;

        let res = self.compute(term)?;

        Ok((res, 0, vec![]))
    }

    fn compute(&mut self, term: &Term<NamedDeBruijn>) -> Result<Term<NamedDeBruijn>, Error> {
        match term {
            Term::Var(name) => {
                self.step_and_maybe_spend(StepKind::Var)?;

                let val = self.lookup_var(name.clone())?;

                self.return_compute(val)
            }
            Term::Delay(body) => {
                self.step_and_maybe_spend(StepKind::Delay)?;

                self.return_compute(Value::Delay(*body.clone()))
            }
            Term::Lambda {
                parameter_name,
                body,
            } => {
                self.step_and_maybe_spend(StepKind::Lambda)?;

                self.return_compute(Value::Lambda {
                    parameter_name: parameter_name.clone(),
                    body: *body.clone(),
                })
            }
            Term::Apply { function, argument } => {
                self.step_and_maybe_spend(StepKind::Apply)?;

                self.push_frame(Context::FrameApplyArg(*argument.clone()));

                self.compute(function)
            }
            Term::Constant(x) => {
                self.step_and_maybe_spend(StepKind::Constant)?;

                self.return_compute(Value::Con(x.clone()))
            }
            Term::Force(body) => {
                self.step_and_maybe_spend(StepKind::Force)?;

                self.push_frame(Context::FrameForce);

                self.compute(body)
            }
            Term::Error => Err(Error::EvaluationFailure),
            Term::Builtin(_) => todo!(),
        }
    }

    fn return_compute(&mut self, value: Value) -> Result<Term<NamedDeBruijn>, Error> {
        // TODO: avoid unwrap and possible just return an err when None
        // but honestly it should never be empty anyways because Machine
        // is initialized with `Context::NoFrame`.
        let frame = self.frames.last().cloned().unwrap();

        match frame {
            Context::FrameApplyFun(function) => {
                self.pop_frame();

                self.apply_evaluate(function, value)
            }
            Context::FrameApplyArg(arg) => {
                self.pop_frame();

                self.push_frame(Context::FrameApplyFun(value));

                self.compute(&arg)
            }
            Context::FrameForce => {
                self.pop_frame();

                self.force_evaluate(value)
            }
            Context::NoFrame => {
                if self.unbudgeted_steps[7] > 0 {
                    self.spend_unbudgeted_steps()?;
                }

                let term = self.discharge_value(value);

                Ok(term)
            }
        }
    }

    fn discharge_value(&mut self, value: Value) -> Term<NamedDeBruijn> {
        match value {
            Value::Con(x) => Term::Constant(x),
            Value::Builtin(_, t) => t,
            Value::Delay(body) => self.discharge_value_env(Term::Delay(Box::new(body))),
            Value::Lambda {
                parameter_name,
                body,
            } => self.discharge_value_env(Term::Lambda {
                parameter_name: NamedDeBruijn {
                    text: parameter_name.text,
                    index: 0.into(),
                },
                body: Box::new(body),
            }),
        }
    }

    fn discharge_value_env(&mut self, term: Term<NamedDeBruijn>) -> Term<NamedDeBruijn> {
        fn rec(lam_cnt: usize, t: Term<NamedDeBruijn>, this: &mut Machine) -> Term<NamedDeBruijn> {
            match t {
                Term::Var(name) => {
                    let index: usize = name.index.into();
                    if lam_cnt >= index {
                        Term::Var(name)
                    } else {
                        this.env
                            .get::<usize>(index - lam_cnt)
                            .cloned()
                            .map_or(Term::Var(name), |v| this.discharge_value(v))
                    }
                }
                Term::Lambda {
                    parameter_name,
                    body,
                } => Term::Lambda {
                    parameter_name,
                    body: Box::new(rec(lam_cnt + 1, *body, this)),
                },
                Term::Apply { function, argument } => Term::Apply {
                    function: Box::new(rec(lam_cnt, *function, this)),
                    argument: Box::new(rec(lam_cnt, *argument, this)),
                },

                Term::Delay(x) => Term::Delay(Box::new(rec(lam_cnt, *x, this))),
                Term::Force(x) => Term::Force(Box::new(rec(lam_cnt, *x, this))),
                rest => rest,
            }
        }
        rec(0, term, self)
    }

    fn force_evaluate(&mut self, value: Value) -> Result<Term<NamedDeBruijn>, Error> {
        match value {
            Value::Delay(body) => self.compute(&body),
            Value::Builtin(_, _) => todo!(),
            rest => Err(Error::NonPolymorphicInstantiation(rest)),
        }
    }

    fn apply_evaluate(
        &mut self,
        function: Value,
        argument: Value,
    ) -> Result<Term<NamedDeBruijn>, Error> {
        match function {
            Value::Lambda { body, .. } => {
                self.env.push(argument);
                let term = self.compute(&body)?;
                self.env.pop();
                Ok(term)
            }
            Value::Builtin(_, _) => todo!(),
            rest => Err(Error::NonFunctionalApplication(rest)),
        }
    }

    fn push_frame(&mut self, frame: Context) {
        self.frames.push(frame);
    }

    fn pop_frame(&mut self) {
        self.frames.pop();
    }

    fn lookup_var(&mut self, name: NamedDeBruijn) -> Result<Value, Error> {
        self.env
            .get::<usize>(name.index.into())
            .cloned()
            .ok_or(Error::OpenTermEvaluated(Term::Var(name)))
    }

    fn step_and_maybe_spend(&mut self, step: StepKind) -> Result<(), Error> {
        let index = step as u8;
        self.unbudgeted_steps[index as usize] += 1;
        self.unbudgeted_steps[7] += 1;

        if self.unbudgeted_steps[7] >= self.slippage {
            self.spend_unbudgeted_steps()?;
        }

        Ok(())
    }

    fn spend_unbudgeted_steps(&mut self) -> Result<(), Error> {
        for i in 0..self.unbudgeted_steps.len() - 1 {
            let mut unspent_step_budget = self.costs.get(StepKind::try_from(i as u8)?);

            unspent_step_budget.occurences(self.unbudgeted_steps[i] as i32);

            self.spend_budget(unspent_step_budget)?;
        }

        self.unbudgeted_steps = vec![0; 8];

        Ok(())
    }

    fn spend_budget(&mut self, spend_budget: ExBudget) -> Result<(), Error> {
        self.ex_budget.mem -= spend_budget.mem;
        self.ex_budget.cpu -= spend_budget.cpu;

        if self.ex_budget.mem < 0 || self.ex_budget.cpu < 0 {
            Err(Error::OutOfExError(self.ex_budget))
        } else {
            Ok(())
        }
    }
}

#[derive(Clone)]
enum Context {
    FrameApplyFun(Value),
    FrameApplyArg(Term<NamedDeBruijn>),
    FrameForce,
    NoFrame,
}

#[derive(Clone, Debug)]
pub enum Value {
    Con(Constant),
    Delay(Term<NamedDeBruijn>),
    Lambda {
        parameter_name: NamedDeBruijn,
        body: Term<NamedDeBruijn>,
    },
    Builtin(
        DefaultFunction,
        Term<NamedDeBruijn>,
        // Need to figure out run time stuff
        // BuiltinRuntime (CekValue uni fun)
    ),
}

/// Can be negative
#[derive(Debug, Clone, PartialEq, Copy, Default)]
pub struct ExBudget {
    mem: i32,
    cpu: i32,
}

impl ExBudget {
    pub fn occurences(&mut self, n: i32) {
        self.mem *= n;
        self.cpu *= n;
    }
}

#[repr(u8)]
pub enum StepKind {
    Constant = 0,
    Var = 1,
    Lambda = 2,
    Apply = 3,
    Delay = 4,
    Force = 5,
    Builtin = 6,
    // DO NOT USE THIS IN `step_and_maybe_spend`
    StartUp = 7,
}

impl TryFrom<u8> for StepKind {
    type Error = error::Error;

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        match value {
            0 => Ok(StepKind::Constant),
            1 => Ok(StepKind::Var),
            2 => Ok(StepKind::Lambda),
            3 => Ok(StepKind::Apply),
            4 => Ok(StepKind::Delay),
            5 => Ok(StepKind::Force),
            6 => Ok(StepKind::Builtin),
            v => Err(error::Error::InvalidStepKind(v)),
        }
    }
}

/// There's no entry for Error since we'll be exiting anyway; also, what would
/// happen if calling 'Error' caused the budget to be exceeded?
#[derive(Default)]
pub struct Costs {
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
