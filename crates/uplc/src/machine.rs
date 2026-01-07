use std::{fmt::Display, rc::Rc};

use crate::ast::{Constant, NamedDeBruijn, Term, Type};

pub mod cost_model;
pub mod discharge;
mod error;
pub mod eval_result;
pub mod runtime;
pub mod value;

use cost_model::{ExBudget, StepKind};
pub use error::Error;
use pallas_primitives::conway::Language;

use self::{cost_model::CostModel, runtime::BuiltinRuntime, value::Value};

/// Environment for the CEK machine with named values for debugging.
/// The `values` field provides name-value pairs for variable inspection.
/// Generic over context type C (e.g., u64 for source map indices).
#[derive(Clone, Debug)]
pub struct Env<C> {
    pub values: Rc<Vec<(NamedDeBruijn, Value<C>)>>,
}

impl<C: Clone> Env<C> {
    pub fn new() -> Self {
        Env {
            values: Rc::new(vec![]),
        }
    }

    fn push(&mut self, name: NamedDeBruijn, value: Value<C>) {
        Rc::make_mut(&mut self.values).push((name, value));
    }

    fn get(&self, index: usize) -> Option<&Value<C>> {
        let len = self.values.len();
        if index <= len {
            self.values.get(len - index).map(|(_, v)| v)
        } else {
            None
        }
    }

    pub fn len(&self) -> usize {
        self.values.len()
    }
}

impl<C: Clone> Default for Env<C> {
    fn default() -> Self {
        Self::new()
    }
}

/// Machine state for step-by-step execution with source map support.
/// Generic over context type C which is preserved through evaluation.
#[derive(Clone, Debug)]
pub enum MachineState<C> {
    Return(Context<C>, Value<C>),
    Compute(Context<C>, Env<C>, Term<NamedDeBruijn, C>),
    Done(Term<NamedDeBruijn, C>),
}

/// Evaluation context (continuation) for the stepping interface.
/// Generic over context type C.
#[derive(Clone, Debug)]
pub enum Context<C> {
    FrameAwaitArg(Value<C>, Box<Context<C>>),
    FrameAwaitFunTerm(Env<C>, Term<NamedDeBruijn, C>, Box<Context<C>>),
    FrameAwaitFunValue(Value<C>, Box<Context<C>>),
    FrameForce(Box<Context<C>>),
    FrameConstr(
        Env<C>,
        usize,
        Vec<Term<NamedDeBruijn, C>>,
        Vec<Value<C>>,
        Box<Context<C>>,
    ),
    FrameCases(Env<C>, Vec<Term<NamedDeBruijn, C>>, Box<Context<C>>),
    NoFrame,
}

/// Internal machine state using regular Terms (for the non-stepping interface).
/// Uses () as context for efficiency.
enum InternalState {
    Return(InternalContext, value::Env<()>, Value<()>),
    Compute(InternalContext, value::Env<()>, Term<NamedDeBruijn, ()>),
    Done(Term<NamedDeBruijn, ()>),
}

/// Internal context using regular Terms.
/// Uses () as context for efficiency.
#[derive(Clone)]
enum InternalContext {
    FrameAwaitArg(Value<()>, Box<InternalContext>),
    FrameAwaitFunTerm(
        value::Env<()>,
        Term<NamedDeBruijn, ()>,
        Box<InternalContext>,
    ),
    FrameAwaitFunValue(Value<()>, Box<InternalContext>),
    FrameForce(Box<InternalContext>),
    FrameConstr(
        value::Env<()>,
        usize,
        Vec<Term<NamedDeBruijn, ()>>,
        Vec<Value<()>>,
        Box<InternalContext>,
    ),
    FrameCases(
        value::Env<()>,
        Vec<Term<NamedDeBruijn, ()>>,
        Box<InternalContext>,
    ),
    NoFrame,
}

pub const TERM_COUNT: usize = 9;
pub const BUILTIN_COUNT: usize = 87;

#[derive(Debug, Clone)]
pub enum Trace {
    Log(String),
    Label(String),
}

impl Display for Trace {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Trace::Log(log) => f.write_str(log),
            Trace::Label(label) => f.write_str(label),
        }
    }
}

impl Trace {
    pub fn unwrap_log(self) -> Option<String> {
        match self {
            Trace::Log(log) => Some(log),
            _ => None,
        }
    }

    pub fn unwrap_label(self) -> Option<String> {
        match self {
            Trace::Label(label) => Some(label),
            _ => None,
        }
    }
}

pub struct Machine {
    costs: CostModel,
    pub ex_budget: ExBudget,
    slippage: u32,
    unbudgeted_steps: [u32; 10],
    pub traces: Vec<Trace>,
    pub spend_counter: Option<[i64; (TERM_COUNT + BUILTIN_COUNT) * 2]>,
    version: Language,
}

impl Machine {
    pub fn new(
        version: Language,
        costs: CostModel,
        initial_budget: ExBudget,
        slippage: u32,
    ) -> Machine {
        Machine {
            costs,
            ex_budget: initial_budget,
            slippage,
            unbudgeted_steps: [0; 10],
            traces: vec![],
            spend_counter: None,
            version,
        }
    }

    pub fn new_debug(
        version: Language,
        costs: CostModel,
        initial_budget: ExBudget,
        slippage: u32,
    ) -> Machine {
        Machine {
            costs,
            ex_budget: initial_budget,
            slippage,
            unbudgeted_steps: [0; 10],
            traces: vec![],
            spend_counter: Some([0; (TERM_COUNT + BUILTIN_COUNT) * 2]),
            version,
        }
    }

    /// Run the machine to completion (non-stepping interface).
    /// Uses () as context for efficiency.
    pub fn run(&mut self, term: Term<NamedDeBruijn, ()>) -> Result<Term<NamedDeBruijn, ()>, Error> {
        let startup_budget = self.costs.machine_costs.get(StepKind::StartUp);
        self.spend_budget(startup_budget)?;

        let mut state = InternalState::Compute(InternalContext::NoFrame, Rc::new(vec![]), term);

        loop {
            state = match state {
                InternalState::Compute(context, env, t) => {
                    self.internal_compute(context, env, t)?
                }
                InternalState::Return(context, env, value) => {
                    self.internal_return(context, env, value)?
                }
                InternalState::Done(t) => {
                    return Ok(t);
                }
            };
        }
    }

    /// Get initial machine state for stepping.
    /// Generic over context type C which is preserved through evaluation.
    pub fn get_initial_machine_state<C: Clone + Default>(
        &mut self,
        term: Term<NamedDeBruijn, C>,
    ) -> Result<MachineState<C>, Error> {
        let startup_budget = self.costs.machine_costs.get(StepKind::StartUp);
        self.spend_budget(startup_budget)?;

        Ok(MachineState::Compute(Context::NoFrame, Env::new(), term))
    }

    /// Step the machine one step forward.
    /// Generic over context type C which is preserved through evaluation.
    pub fn step<C: Clone + Default>(
        &mut self,
        state: MachineState<C>,
    ) -> Result<MachineState<C>, Error> {
        match state {
            MachineState::Compute(context, env, term) => self.step_compute(context, env, term),
            MachineState::Return(context, value) => self.step_return(context, value),
            MachineState::Done(t) => Ok(MachineState::Done(t)),
        }
    }

    fn step_compute<C: Clone + Default>(
        &mut self,
        context: Context<C>,
        env: Env<C>,
        term: Term<NamedDeBruijn, C>,
    ) -> Result<MachineState<C>, Error> {
        match term {
            Term::Var { name, .. } => {
                self.step_and_maybe_spend(StepKind::Var)?;
                let val = self.lookup_var_env(name.as_ref(), &env)?;
                Ok(MachineState::Return(context, val))
            }
            Term::Delay {
                term: body,
                context: _,
            } => {
                self.step_and_maybe_spend(StepKind::Delay)?;
                Ok(MachineState::Return(
                    context,
                    Value::Delay(body.clone(), env_to_value_env(&env)),
                ))
            }
            Term::Lambda {
                parameter_name,
                body,
                context: _,
            } => {
                self.step_and_maybe_spend(StepKind::Lambda)?;
                Ok(MachineState::Return(
                    context,
                    Value::Lambda {
                        parameter_name,
                        body: body.clone(),
                        env: env_to_value_env(&env),
                    },
                ))
            }
            Term::Apply {
                function,
                argument,
                context: _,
            } => {
                self.step_and_maybe_spend(StepKind::Apply)?;
                Ok(MachineState::Compute(
                    Context::FrameAwaitFunTerm(
                        env.clone(),
                        argument.as_ref().clone(),
                        context.into(),
                    ),
                    env,
                    function.as_ref().clone(),
                ))
            }
            Term::Constant { value: x, .. } => {
                self.step_and_maybe_spend(StepKind::Constant)?;
                Ok(MachineState::Return(context, Value::Con(x)))
            }
            Term::Force {
                term: body,
                context: _,
            } => {
                self.step_and_maybe_spend(StepKind::Force)?;
                Ok(MachineState::Compute(
                    Context::FrameForce(context.into()),
                    env,
                    body.as_ref().clone(),
                ))
            }
            Term::Error { .. } => Err(Error::EvaluationFailure),
            Term::Builtin { func: fun, .. } => {
                self.step_and_maybe_spend(StepKind::Builtin)?;
                let runtime: BuiltinRuntime<C> = fun.into();
                Ok(MachineState::Return(
                    context,
                    Value::Builtin { fun, runtime },
                ))
            }
            Term::Constr {
                tag,
                mut fields,
                context: _,
            } => {
                self.step_and_maybe_spend(StepKind::Constr)?;
                fields.reverse();
                if !fields.is_empty() {
                    let popped_field = fields.pop().unwrap();
                    Ok(MachineState::Compute(
                        Context::FrameConstr(env.clone(), tag, fields, vec![], context.into()),
                        env,
                        popped_field,
                    ))
                } else {
                    Ok(MachineState::Return(
                        context,
                        Value::Constr {
                            tag,
                            fields: vec![],
                        },
                    ))
                }
            }
            Term::Case {
                constr,
                branches,
                context: _,
            } => {
                self.step_and_maybe_spend(StepKind::Case)?;
                Ok(MachineState::Compute(
                    Context::FrameCases(env.clone(), branches, context.into()),
                    env,
                    constr.as_ref().clone(),
                ))
            }
        }
    }

    fn step_return<C: Clone + Default>(
        &mut self,
        context: Context<C>,
        value: Value<C>,
    ) -> Result<MachineState<C>, Error> {
        match context {
            Context::NoFrame => {
                if self.unbudgeted_steps[9] > 0 {
                    self.spend_unbudgeted_steps()?;
                }
                let term = discharge::value_as_term(value);
                Ok(MachineState::Done(term))
            }
            Context::FrameForce(ctx) => self.step_force(*ctx, value),
            Context::FrameAwaitFunTerm(arg_env, arg, ctx) => Ok(MachineState::Compute(
                Context::FrameAwaitArg(value, ctx),
                arg_env,
                arg,
            )),
            Context::FrameAwaitArg(fun, ctx) => self.step_apply(*ctx, fun, value),
            Context::FrameAwaitFunValue(arg, ctx) => self.step_apply(*ctx, value, arg),
            Context::FrameConstr(env, tag, mut fields, mut resolved_fields, ctx) => {
                resolved_fields.push(value);
                if !fields.is_empty() {
                    let popped_field = fields.pop().unwrap();
                    Ok(MachineState::Compute(
                        Context::FrameConstr(env.clone(), tag, fields, resolved_fields, ctx),
                        env,
                        popped_field,
                    ))
                } else {
                    Ok(MachineState::Return(
                        *ctx,
                        Value::Constr {
                            tag,
                            fields: resolved_fields,
                        },
                    ))
                }
            }
            Context::FrameCases(env, branches, ctx) => match value {
                Value::Constr { tag, fields } => match branches.get(tag) {
                    Some(t) => Ok(MachineState::Compute(
                        step_transfer_arg_stack(fields, *ctx),
                        env,
                        t.clone(),
                    )),
                    None => {
                        let term_branches: Vec<Term<NamedDeBruijn, ()>> = branches
                            .into_iter()
                            .map(|t| t.map_context(|_| ()))
                            .collect();
                        Err(Error::MissingCaseBranch(
                            term_branches,
                            Value::Constr { tag, fields }.erase_context(),
                        ))
                    }
                },
                v => Err(Error::NonConstrScrutinized(v.erase_context())),
            },
        }
    }

    fn step_force<C: Clone + Default>(
        &mut self,
        context: Context<C>,
        value: Value<C>,
    ) -> Result<MachineState<C>, Error> {
        match value {
            Value::Delay(body, value_env) => {
                let env = value_env_to_env(&value_env);
                Ok(MachineState::Compute(context, env, body.as_ref().clone()))
            }
            Value::Builtin { fun, mut runtime } => {
                if runtime.needs_force() {
                    runtime.consume_force();
                    let res = if runtime.is_ready() {
                        self.eval_builtin_app(runtime)?
                    } else {
                        Value::Builtin { fun, runtime }
                    };
                    Ok(MachineState::Return(context, res))
                } else {
                    let term =
                        discharge::value_as_term(Value::Builtin { fun, runtime }.erase_context());
                    Err(Error::BuiltinTermArgumentExpected(term))
                }
            }
            rest => Err(Error::NonPolymorphicInstantiation(rest.erase_context())),
        }
    }

    fn step_apply<C: Clone + Default>(
        &mut self,
        context: Context<C>,
        function: Value<C>,
        argument: Value<C>,
    ) -> Result<MachineState<C>, Error> {
        match function {
            Value::Lambda {
                parameter_name,
                body,
                env: value_env,
            } => {
                let mut env = value_env_to_env(&value_env);
                env.push((*parameter_name).clone(), argument);
                Ok(MachineState::Compute(context, env, body.as_ref().clone()))
            }
            Value::Builtin { fun, runtime } => {
                if runtime.is_arrow() && !runtime.needs_force() {
                    let mut runtime = runtime;
                    runtime.push(argument)?;
                    let res = if runtime.is_ready() {
                        self.eval_builtin_app(runtime)?
                    } else {
                        Value::Builtin { fun, runtime }
                    };
                    Ok(MachineState::Return(context, res))
                } else {
                    let term =
                        discharge::value_as_term(Value::Builtin { fun, runtime }.erase_context());
                    Err(Error::UnexpectedBuiltinTermArgument(term))
                }
            }
            rest => Err(Error::NonFunctionalApplication(
                rest.erase_context(),
                argument.erase_context(),
            )),
        }
    }

    // ===== Internal compute/return for the non-stepping interface =====

    fn internal_compute(
        &mut self,
        context: InternalContext,
        env: value::Env<()>,
        term: Term<NamedDeBruijn, ()>,
    ) -> Result<InternalState, Error> {
        match term {
            Term::Var { name, .. } => {
                self.step_and_maybe_spend(StepKind::Var)?;
                let val = self.lookup_var(name.as_ref(), &env)?;
                Ok(InternalState::Return(context, env, val))
            }
            Term::Delay { term: body, .. } => {
                self.step_and_maybe_spend(StepKind::Delay)?;
                Ok(InternalState::Return(
                    context,
                    env.clone(),
                    Value::Delay(body, env),
                ))
            }
            Term::Lambda {
                parameter_name,
                body,
                ..
            } => {
                self.step_and_maybe_spend(StepKind::Lambda)?;
                Ok(InternalState::Return(
                    context,
                    env.clone(),
                    Value::Lambda {
                        parameter_name,
                        body,
                        env,
                    },
                ))
            }
            Term::Apply {
                function, argument, ..
            } => {
                self.step_and_maybe_spend(StepKind::Apply)?;
                Ok(InternalState::Compute(
                    InternalContext::FrameAwaitFunTerm(
                        env.clone(),
                        argument.as_ref().clone(),
                        context.into(),
                    ),
                    env,
                    function.as_ref().clone(),
                ))
            }
            Term::Constant { value: x, .. } => {
                self.step_and_maybe_spend(StepKind::Constant)?;
                Ok(InternalState::Return(context, env, Value::Con(x)))
            }
            Term::Force { term: body, .. } => {
                self.step_and_maybe_spend(StepKind::Force)?;
                Ok(InternalState::Compute(
                    InternalContext::FrameForce(context.into()),
                    env,
                    body.as_ref().clone(),
                ))
            }
            Term::Error { .. } => Err(Error::EvaluationFailure),
            Term::Builtin { func: fun, .. } => {
                self.step_and_maybe_spend(StepKind::Builtin)?;
                let runtime: BuiltinRuntime<()> = fun.into();
                Ok(InternalState::Return(
                    context,
                    env,
                    Value::Builtin { fun, runtime },
                ))
            }
            Term::Constr {
                tag, mut fields, ..
            } => {
                self.step_and_maybe_spend(StepKind::Constr)?;
                fields.reverse();
                if !fields.is_empty() {
                    let popped_field = fields.pop().unwrap();
                    Ok(InternalState::Compute(
                        InternalContext::FrameConstr(
                            env.clone(),
                            tag,
                            fields,
                            vec![],
                            context.into(),
                        ),
                        env,
                        popped_field,
                    ))
                } else {
                    Ok(InternalState::Return(
                        context,
                        env,
                        Value::Constr {
                            tag,
                            fields: vec![],
                        },
                    ))
                }
            }
            Term::Case {
                constr, branches, ..
            } => {
                self.step_and_maybe_spend(StepKind::Case)?;
                Ok(InternalState::Compute(
                    InternalContext::FrameCases(env.clone(), branches, context.into()),
                    env,
                    constr.as_ref().clone(),
                ))
            }
        }
    }

    fn internal_return(
        &mut self,
        context: InternalContext,
        env: value::Env<()>,
        value: Value<()>,
    ) -> Result<InternalState, Error> {
        match context {
            InternalContext::NoFrame => {
                if self.unbudgeted_steps[9] > 0 {
                    self.spend_unbudgeted_steps()?;
                }
                let term = discharge::value_as_term(value);
                Ok(InternalState::Done(term))
            }
            InternalContext::FrameForce(ctx) => self.internal_force(*ctx, env, value),
            InternalContext::FrameAwaitFunTerm(arg_env, arg, ctx) => Ok(InternalState::Compute(
                InternalContext::FrameAwaitArg(value, ctx),
                arg_env,
                arg,
            )),
            InternalContext::FrameAwaitArg(fun, ctx) => self.internal_apply(*ctx, env, fun, value),
            InternalContext::FrameAwaitFunValue(arg, ctx) => {
                self.internal_apply(*ctx, env, value, arg)
            }
            InternalContext::FrameConstr(frame_env, tag, mut fields, mut resolved_fields, ctx) => {
                resolved_fields.push(value);
                if !fields.is_empty() {
                    let popped_field = fields.pop().unwrap();
                    Ok(InternalState::Compute(
                        InternalContext::FrameConstr(
                            frame_env.clone(),
                            tag,
                            fields,
                            resolved_fields,
                            ctx,
                        ),
                        frame_env,
                        popped_field,
                    ))
                } else {
                    Ok(InternalState::Return(
                        *ctx,
                        frame_env,
                        Value::Constr {
                            tag,
                            fields: resolved_fields,
                        },
                    ))
                }
            }
            InternalContext::FrameCases(frame_env, branches, ctx) => match value {
                Value::Constr { tag, fields } => match branches.get(tag) {
                    Some(t) => Ok(InternalState::Compute(
                        internal_transfer_arg_stack(fields, *ctx),
                        frame_env,
                        t.clone(),
                    )),
                    None => Err(Error::MissingCaseBranch(
                        branches,
                        Value::Constr { tag, fields },
                    )),
                },
                v => Err(Error::NonConstrScrutinized(v)),
            },
        }
    }

    fn internal_force(
        &mut self,
        context: InternalContext,
        _env: value::Env<()>,
        value: Value<()>,
    ) -> Result<InternalState, Error> {
        match value {
            Value::Delay(body, delay_env) => Ok(InternalState::Compute(
                context,
                delay_env,
                body.as_ref().clone(),
            )),
            Value::Builtin { fun, mut runtime } => {
                if runtime.needs_force() {
                    runtime.consume_force();
                    let res = if runtime.is_ready() {
                        self.eval_builtin_app(runtime)?
                    } else {
                        Value::Builtin { fun, runtime }
                    };
                    Ok(InternalState::Return(context, Rc::new(vec![]), res))
                } else {
                    let term = discharge::value_as_term(Value::Builtin { fun, runtime });
                    Err(Error::BuiltinTermArgumentExpected(term))
                }
            }
            rest => Err(Error::NonPolymorphicInstantiation(rest)),
        }
    }

    fn internal_apply(
        &mut self,
        context: InternalContext,
        _env: value::Env<()>,
        function: Value<()>,
        argument: Value<()>,
    ) -> Result<InternalState, Error> {
        match function {
            Value::Lambda {
                parameter_name,
                body,
                mut env,
            } => {
                let e = Rc::make_mut(&mut env);
                e.push((parameter_name, argument));
                Ok(InternalState::Compute(
                    context,
                    Rc::new(e.clone()),
                    body.as_ref().clone(),
                ))
            }
            Value::Builtin { fun, runtime } => {
                if runtime.is_arrow() && !runtime.needs_force() {
                    let mut runtime = runtime;
                    runtime.push(argument)?;
                    let res = if runtime.is_ready() {
                        self.eval_builtin_app(runtime)?
                    } else {
                        Value::Builtin { fun, runtime }
                    };
                    Ok(InternalState::Return(context, Rc::new(vec![]), res))
                } else {
                    let term = discharge::value_as_term(Value::Builtin { fun, runtime });
                    Err(Error::UnexpectedBuiltinTermArgument(term))
                }
            }
            rest => Err(Error::NonFunctionalApplication(rest, argument)),
        }
    }

    // ===== Shared helpers =====

    fn eval_builtin_app<C: Clone>(
        &mut self,
        runtime: BuiltinRuntime<C>,
    ) -> Result<Value<C>, Error> {
        let cost = runtime.to_ex_budget(&self.costs.builtin_costs)?;
        self.spend_budget(cost)?;

        if let Some(counter) = &mut self.spend_counter {
            let i = (runtime.fun as usize + TERM_COUNT) * 2;
            counter[i] += cost.mem;
            counter[i + 1] += cost.cpu;
        }

        // Builtins now preserve context - when IfThenElse returns one of its
        // arguments, the context is preserved.
        runtime.call(&self.version, &mut self.traces)
    }

    fn lookup_var(
        &mut self,
        name: &NamedDeBruijn,
        env: &value::Env<()>,
    ) -> Result<Value<()>, Error> {
        let index = env.len() - usize::from(name.index);
        env.get(index).map(|(_, v)| v.clone()).ok_or_else(|| {
            Error::OpenTermEvaluated(Term::Var {
                name: name.clone().into(),
                context: (),
            })
        })
    }

    fn lookup_var_env<C: Clone>(
        &mut self,
        name: &NamedDeBruijn,
        env: &Env<C>,
    ) -> Result<Value<C>, Error> {
        env.get(usize::from(name.index)).cloned().ok_or_else(|| {
            Error::OpenTermEvaluated(Term::Var {
                name: name.clone().into(),
                context: (),
            })
        })
    }

    fn step_and_maybe_spend(&mut self, step: StepKind) -> Result<(), Error> {
        let index = step as u8;
        self.unbudgeted_steps[index as usize] += 1;
        self.unbudgeted_steps[9] += 1;

        if self.unbudgeted_steps[9] >= self.slippage {
            self.spend_unbudgeted_steps()?;
        }

        Ok(())
    }

    fn spend_unbudgeted_steps(&mut self) -> Result<(), Error> {
        for i in 0..self.unbudgeted_steps.len() - 1 {
            let mut unspent_step_budget =
                self.costs.machine_costs.get(StepKind::try_from(i as u8)?);
            unspent_step_budget.occurrences(self.unbudgeted_steps[i] as i64);
            self.spend_budget(unspent_step_budget)?;
            self.unbudgeted_steps[i] = 0;

            if let Some(counter) = &mut self.spend_counter {
                counter[i * 2] += unspent_step_budget.mem;
                counter[i * 2 + 1] += unspent_step_budget.cpu;
            }
        }

        self.unbudgeted_steps[9] = 0;
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

// ===== Helper functions =====

fn internal_transfer_arg_stack(mut args: Vec<Value<()>>, ctx: InternalContext) -> InternalContext {
    if args.is_empty() {
        ctx
    } else {
        let popped_field = args.pop().unwrap();
        internal_transfer_arg_stack(
            args,
            InternalContext::FrameAwaitFunValue(popped_field, ctx.into()),
        )
    }
}

fn step_transfer_arg_stack<C>(mut args: Vec<Value<C>>, ctx: Context<C>) -> Context<C> {
    if args.is_empty() {
        ctx
    } else {
        let popped_field = args.pop().unwrap();
        step_transfer_arg_stack(args, Context::FrameAwaitFunValue(popped_field, ctx.into()))
    }
}

/// Convert value::Env<C> to the named Env<C> type.
/// Now that value::Env stores names, we can preserve them.
fn value_env_to_env<C: Clone>(value_env: &value::Env<C>) -> Env<C> {
    let mut env = Env::new();
    for (name, val) in value_env.iter() {
        env.push((**name).clone(), val.clone());
    }
    env
}

/// Convert the named Env<C> type to value::Env<C>, preserving names.
fn env_to_value_env<C: Clone>(env: &Env<C>) -> value::Env<C> {
    Rc::new(
        env.values
            .iter()
            .map(|(name, v)| (Rc::new(name.clone()), v.clone()))
            .collect(),
    )
}

impl From<&Constant> for Type {
    fn from(constant: &Constant) -> Self {
        match constant {
            Constant::Integer(_) => Type::Integer,
            Constant::ByteString(_) => Type::ByteString,
            Constant::String(_) => Type::String,
            Constant::Unit => Type::Unit,
            Constant::Bool(_) => Type::Bool,
            Constant::ProtoList(t, _) => Type::List(Rc::new(t.clone())),
            Constant::ProtoPair(t1, t2, _, _) => {
                Type::Pair(Rc::new(t1.clone()), Rc::new(t2.clone()))
            }
            Constant::Data(_) => Type::Data,
            Constant::Bls12_381G1Element(_) => Type::Bls12_381G1Element,
            Constant::Bls12_381G2Element(_) => Type::Bls12_381G2Element,
            Constant::Bls12_381MlResult(_) => Type::Bls12_381MlResult,
        }
    }
}

#[cfg(test)]
mod tests {
    use num_bigint::BigInt;

    use super::{cost_model::ExBudget, runtime::Compressable};
    use crate::{
        ast::{Constant, NamedDeBruijn, Program, Term},
        builtins::DefaultFunction,
    };

    #[test]
    fn add_big_ints() {
        let program: Program<NamedDeBruijn> = Program {
            version: (0, 0, 0),
            term: Term::Apply {
                function: Term::Apply {
                    function: Term::Builtin {
                        func: DefaultFunction::AddInteger,
                        context: (),
                    }
                    .into(),
                    argument: Term::Constant {
                        value: Constant::Integer(i128::MAX.into()).into(),
                        context: (),
                    }
                    .into(),
                    context: (),
                }
                .into(),
                argument: Term::Constant {
                    value: Constant::Integer(i128::MAX.into()).into(),
                    context: (),
                }
                .into(),
                context: (),
            },
        };

        let eval_result = program.eval(ExBudget::default());

        let term = eval_result.result().unwrap();

        assert_eq!(
            term,
            Term::Constant {
                value: Constant::Integer(
                    Into::<BigInt>::into(i128::MAX) + Into::<BigInt>::into(i128::MAX)
                )
                .into(),
                context: (),
            }
        );
    }

    #[test]
    fn divide_integer() {
        let make_program = |fun: DefaultFunction, n: i32, m: i32| Program::<NamedDeBruijn> {
            version: (0, 0, 0),
            term: Term::Apply {
                function: Term::Apply {
                    function: Term::Builtin {
                        func: fun,
                        context: (),
                    }
                    .into(),
                    argument: Term::Constant {
                        value: Constant::Integer(n.into()).into(),
                        context: (),
                    }
                    .into(),
                    context: (),
                }
                .into(),
                argument: Term::Constant {
                    value: Constant::Integer(m.into()).into(),
                    context: (),
                }
                .into(),
                context: (),
            },
        };

        let test_data = vec![
            (DefaultFunction::DivideInteger, 8, 3, 2),
            (DefaultFunction::DivideInteger, 8, -3, -3),
            (DefaultFunction::DivideInteger, -8, 3, -3),
            (DefaultFunction::DivideInteger, -8, -3, 2),
            (DefaultFunction::QuotientInteger, 8, 3, 2),
            (DefaultFunction::QuotientInteger, 8, -3, -2),
            (DefaultFunction::QuotientInteger, -8, 3, -2),
            (DefaultFunction::QuotientInteger, -8, -3, 2),
            (DefaultFunction::RemainderInteger, 8, 3, 2),
            (DefaultFunction::RemainderInteger, 8, -3, 2),
            (DefaultFunction::RemainderInteger, -8, 3, -2),
            (DefaultFunction::RemainderInteger, -8, -3, -2),
            (DefaultFunction::ModInteger, 8, 3, 2),
            (DefaultFunction::ModInteger, 8, -3, -1),
            (DefaultFunction::ModInteger, -8, 3, 1),
            (DefaultFunction::ModInteger, -8, -3, -2),
        ];

        for (fun, n, m, result) in test_data {
            let eval_result = make_program(fun, n, m).eval(ExBudget::default());

            assert_eq!(
                eval_result.result().unwrap(),
                Term::Constant {
                    value: Constant::Integer(result.into()).into(),
                    context: (),
                }
            );
        }
    }

    #[test]
    fn case_constr_case_0() {
        let make_program =
            |fun: DefaultFunction, tag: usize, n: i32, m: i32| Program::<NamedDeBruijn> {
                version: (0, 0, 0),
                term: Term::Case {
                    constr: Term::Constr {
                        tag,
                        fields: vec![
                            Term::Constant {
                                value: Constant::Integer(n.into()).into(),
                                context: (),
                            },
                            Term::Constant {
                                value: Constant::Integer(m.into()).into(),
                                context: (),
                            },
                        ],
                        context: (),
                    }
                    .into(),
                    branches: vec![
                        Term::Builtin {
                            func: fun,
                            context: (),
                        },
                        Term::subtract_integer(),
                    ],
                    context: (),
                },
            };

        let test_data = vec![
            (DefaultFunction::AddInteger, 0, 8, 3, 11),
            (DefaultFunction::AddInteger, 1, 8, 3, 5),
        ];

        for (fun, tag, n, m, result) in test_data {
            let eval_result = make_program(fun, tag, n, m).eval(ExBudget::max());

            assert_eq!(
                eval_result.result().unwrap(),
                Term::Constant {
                    value: Constant::Integer(result.into()).into(),
                    context: (),
                }
            );
        }
    }

    #[test]
    fn case_constr_case_1() {
        let make_program = |tag: usize| Program::<NamedDeBruijn> {
            version: (0, 0, 0),
            term: Term::Case {
                constr: Term::Constr {
                    tag,
                    fields: vec![],
                    context: (),
                }
                .into(),
                branches: vec![
                    Term::integer(5.into()),
                    Term::integer(10.into()),
                    Term::integer(15.into()),
                ],
                context: (),
            },
        };

        let test_data = vec![(0, 5), (1, 10), (2, 15)];

        for (tag, result) in test_data {
            let eval_result = make_program(tag).eval(ExBudget::max());

            assert_eq!(
                eval_result.result().unwrap(),
                Term::Constant {
                    value: Constant::Integer(result.into()).into(),
                    context: (),
                }
            );
        }
    }

    #[test]
    fn bls_g1_add_associative() {
        let a = blst::blst_p1::uncompress(&[
            0xab, 0xd6, 0x18, 0x64, 0xf5, 0x19, 0x74, 0x80, 0x32, 0x55, 0x1e, 0x42, 0xe0, 0xac,
            0x41, 0x7f, 0xd8, 0x28, 0xf0, 0x79, 0x45, 0x4e, 0x3e, 0x3c, 0x98, 0x91, 0xc5, 0xc2,
            0x9e, 0xd7, 0xf1, 0x0b, 0xde, 0xcc, 0x04, 0x68, 0x54, 0xe3, 0x93, 0x1c, 0xb7, 0x00,
            0x27, 0x79, 0xbd, 0x76, 0xd7, 0x1f,
        ])
        .unwrap();

        let b = blst::blst_p1::uncompress(&[
            0x95, 0x0d, 0xfd, 0x33, 0xda, 0x26, 0x82, 0x26, 0x0c, 0x76, 0x03, 0x8d, 0xfb, 0x8b,
            0xad, 0x6e, 0x84, 0xae, 0x9d, 0x59, 0x9a, 0x3c, 0x15, 0x18, 0x15, 0x94, 0x5a, 0xc1,
            0xe6, 0xef, 0x6b, 0x10, 0x27, 0xcd, 0x91, 0x7f, 0x39, 0x07, 0x47, 0x9d, 0x20, 0xd6,
            0x36, 0xce, 0x43, 0x7a, 0x41, 0xf5,
        ])
        .unwrap();

        let c = blst::blst_p1::uncompress(&[
            0xb9, 0x62, 0xfd, 0x0c, 0xc8, 0x10, 0x48, 0xe0, 0xcf, 0x75, 0x57, 0xbf, 0x3e, 0x4b,
            0x6e, 0xdc, 0x5a, 0xb4, 0xbf, 0xb3, 0xdc, 0x87, 0xf8, 0x3a, 0xf4, 0x28, 0xb6, 0x30,
            0x07, 0x27, 0xb1, 0x39, 0xc4, 0x04, 0xab, 0x15, 0x9b, 0xdf, 0x2e, 0xae, 0xa3, 0xf6,
            0x49, 0x90, 0x34, 0x21, 0x53, 0x7f,
        ])
        .unwrap();

        let term: Term<NamedDeBruijn> = Term::bls12_381_g1_equal()
            .apply(
                Term::bls12_381_g1_add().apply(Term::bls12_381_g1(a)).apply(
                    Term::bls12_381_g1_add()
                        .apply(Term::bls12_381_g1(b))
                        .apply(Term::bls12_381_g1(c)),
                ),
            )
            .apply(
                Term::bls12_381_g1_add()
                    .apply(
                        Term::bls12_381_g1_add()
                            .apply(Term::bls12_381_g1(a))
                            .apply(Term::bls12_381_g1(b)),
                    )
                    .apply(Term::bls12_381_g1(c)),
            );

        let program = Program {
            version: (1, 0, 0),
            term,
        };

        let eval_result = program.eval(Default::default());

        let final_term = eval_result.result().unwrap();

        assert_eq!(final_term, Term::bool(true))
    }

    #[test]
    fn bls_g2_add_associative() {
        let a = blst::blst_p1::uncompress(&[
            0xab, 0xd6, 0x18, 0x64, 0xf5, 0x19, 0x74, 0x80, 0x32, 0x55, 0x1e, 0x42, 0xe0, 0xac,
            0x41, 0x7f, 0xd8, 0x28, 0xf0, 0x79, 0x45, 0x4e, 0x3e, 0x3c, 0x98, 0x91, 0xc5, 0xc2,
            0x9e, 0xd7, 0xf1, 0x0b, 0xde, 0xcc, 0x04, 0x68, 0x54, 0xe3, 0x93, 0x1c, 0xb7, 0x00,
            0x27, 0x79, 0xbd, 0x76, 0xd7, 0x1f,
        ])
        .unwrap();

        let b = blst::blst_p1::uncompress(&[
            0x95, 0x0d, 0xfd, 0x33, 0xda, 0x26, 0x82, 0x26, 0x0c, 0x76, 0x03, 0x8d, 0xfb, 0x8b,
            0xad, 0x6e, 0x84, 0xae, 0x9d, 0x59, 0x9a, 0x3c, 0x15, 0x18, 0x15, 0x94, 0x5a, 0xc1,
            0xe6, 0xef, 0x6b, 0x10, 0x27, 0xcd, 0x91, 0x7f, 0x39, 0x07, 0x47, 0x9d, 0x20, 0xd6,
            0x36, 0xce, 0x43, 0x7a, 0x41, 0xf5,
        ])
        .unwrap();

        let c = blst::blst_p1::uncompress(&[
            0xb9, 0x62, 0xfd, 0x0c, 0xc8, 0x10, 0x48, 0xe0, 0xcf, 0x75, 0x57, 0xbf, 0x3e, 0x4b,
            0x6e, 0xdc, 0x5a, 0xb4, 0xbf, 0xb3, 0xdc, 0x87, 0xf8, 0x3a, 0xf4, 0x28, 0xb6, 0x30,
            0x07, 0x27, 0xb1, 0x39, 0xc4, 0x04, 0xab, 0x15, 0x9b, 0xdf, 0x2e, 0xae, 0xa3, 0xf6,
            0x49, 0x90, 0x34, 0x21, 0x53, 0x7f,
        ])
        .unwrap();

        let term: Term<NamedDeBruijn> = Term::bls12_381_g1_equal()
            .apply(
                Term::bls12_381_g1_add().apply(Term::bls12_381_g1(a)).apply(
                    Term::bls12_381_g1_add()
                        .apply(Term::bls12_381_g1(b))
                        .apply(Term::bls12_381_g1(c)),
                ),
            )
            .apply(
                Term::bls12_381_g1_add()
                    .apply(
                        Term::bls12_381_g1_add()
                            .apply(Term::bls12_381_g1(a))
                            .apply(Term::bls12_381_g1(b)),
                    )
                    .apply(Term::bls12_381_g1(c)),
            );

        let program = Program {
            version: (1, 0, 0),
            term,
        };

        let eval_result = program.eval(Default::default());

        let final_term = eval_result.result().unwrap();

        assert_eq!(final_term, Term::bool(true))
    }

    #[test]
    fn stepping_preserves_variable_names_in_env() {
        use super::{Machine, MachineState};
        use crate::ast::DeBruijn;
        use std::rc::Rc;

        // Create a simple program: (\x -> \y -> x) 1 2
        // This should result in env containing [("x", 1), ("y", 2)] at some point
        let term: Term<NamedDeBruijn, u64> = Term::Lambda {
            parameter_name: Rc::new(NamedDeBruijn {
                text: "my_x_var".to_string(),
                index: DeBruijn::from(0),
            }),
            body: Rc::new(Term::Lambda {
                parameter_name: Rc::new(NamedDeBruijn {
                    text: "my_y_var".to_string(),
                    index: DeBruijn::from(0),
                }),
                body: Rc::new(Term::Var {
                    name: Rc::new(NamedDeBruijn {
                        text: "my_x_var".to_string(),
                        index: DeBruijn::from(2),
                    }),
                    context: 100,
                }),
                context: 200,
            }),
            context: 300,
        };

        // Apply to 1 and then 2
        let term = Term::Apply {
            function: Rc::new(Term::Apply {
                function: Rc::new(term),
                argument: Rc::new(Term::Constant {
                    value: Rc::new(Constant::Integer(1.into())),
                    context: 400,
                }),
                context: 500,
            }),
            argument: Rc::new(Term::Constant {
                value: Rc::new(Constant::Integer(2.into())),
                context: 600,
            }),
            context: 700,
        };

        let mut machine = Machine::new(
            pallas_primitives::conway::Language::PlutusV3,
            crate::machine::cost_model::CostModel::default(),
            ExBudget::default(),
            200,
        );

        let mut state = machine.get_initial_machine_state(term).unwrap();

        // Track the env names we see
        let mut found_x = false;
        let mut found_y = false;
        let mut found_both = false;

        for _ in 0..100 {
            match &state {
                MachineState::Compute(_, env, _) => {
                    // Check if we have the expected variable names in the env
                    for (name, _) in env.values.iter() {
                        if name.text == "my_x_var" {
                            found_x = true;
                        }
                        if name.text == "my_y_var" {
                            found_y = true;
                        }
                    }
                    // Check if both are present at the same time
                    if env.values.len() >= 2 {
                        let names: Vec<_> =
                            env.values.iter().map(|(n, _)| n.text.as_str()).collect();
                        if names.contains(&"my_x_var") && names.contains(&"my_y_var") {
                            found_both = true;
                        }
                    }
                }
                MachineState::Done(_) => break,
                MachineState::Return(_, _) => {}
            }
            state = machine.step(state).unwrap();
        }

        assert!(found_x, "Should have seen 'my_x_var' in env at some point");
        assert!(found_y, "Should have seen 'my_y_var' in env at some point");
        assert!(
            found_both,
            "Should have seen both 'my_x_var' and 'my_y_var' in env together"
        );
    }
}
