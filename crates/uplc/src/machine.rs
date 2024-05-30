use std::rc::Rc;

use crate::ast::{Constant, NamedDeBruijn, Term, Type};

pub mod cost_model;
mod discharge;
mod error;
pub mod eval_result;
pub mod runtime;
pub mod value;

use cost_model::{ExBudget, StepKind};
pub use error::Error;
use pallas_primitives::conway::Language;

use self::{
    cost_model::CostModel,
    runtime::BuiltinRuntime,
    value::{Env, Value},
};

enum MachineState {
    Return(Context, Value),
    Compute(Context, Env, Term<NamedDeBruijn>),
    Done(Term<NamedDeBruijn>),
}

#[derive(Clone)]
enum Context {
    FrameAwaitArg(Value, Box<Context>),
    FrameAwaitFunTerm(Env, Term<NamedDeBruijn>, Box<Context>),
    FrameAwaitFunValue(Value, Box<Context>),
    FrameForce(Box<Context>),
    FrameConstr(
        Env,
        usize,
        Vec<Term<NamedDeBruijn>>,
        Vec<Value>,
        Box<Context>,
    ),
    FrameCases(Env, Vec<Term<NamedDeBruijn>>, Box<Context>),
    NoFrame,
}

pub struct Machine {
    costs: CostModel,
    pub ex_budget: ExBudget,
    slippage: u32,
    unbudgeted_steps: [u32; 10],
    pub logs: Vec<String>,
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
            logs: vec![],
            version,
        }
    }

    pub fn run(&mut self, term: Term<NamedDeBruijn>) -> Result<Term<NamedDeBruijn>, Error> {
        use MachineState::*;

        let startup_budget = self.costs.machine_costs.get(StepKind::StartUp);

        self.spend_budget(startup_budget)?;

        let mut state = Compute(Context::NoFrame, Rc::new(vec![]), term);

        loop {
            state = match state {
                Compute(context, env, t) => self.compute(context, env, t)?,
                Return(context, value) => self.return_compute(context, value)?,
                Done(t) => {
                    return Ok(t);
                }
            };
        }
    }

    fn compute(
        &mut self,
        context: Context,
        env: Env,
        term: Term<NamedDeBruijn>,
    ) -> Result<MachineState, Error> {
        match term {
            Term::Var(name) => {
                self.step_and_maybe_spend(StepKind::Var)?;

                let val = self.lookup_var(name.as_ref(), &env)?;

                Ok(MachineState::Return(context, val))
            }
            Term::Delay(body) => {
                self.step_and_maybe_spend(StepKind::Delay)?;

                Ok(MachineState::Return(context, Value::Delay(body, env)))
            }
            Term::Lambda {
                parameter_name,
                body,
            } => {
                self.step_and_maybe_spend(StepKind::Lambda)?;

                Ok(MachineState::Return(
                    context,
                    Value::Lambda {
                        parameter_name,
                        body,
                        env,
                    },
                ))
            }
            Term::Apply { function, argument } => {
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
            Term::Constant(x) => {
                self.step_and_maybe_spend(StepKind::Constant)?;

                Ok(MachineState::Return(context, Value::Con(x)))
            }
            Term::Force(body) => {
                self.step_and_maybe_spend(StepKind::Force)?;

                Ok(MachineState::Compute(
                    Context::FrameForce(context.into()),
                    env,
                    body.as_ref().clone(),
                ))
            }
            Term::Error => Err(Error::EvaluationFailure),
            Term::Builtin(fun) => {
                self.step_and_maybe_spend(StepKind::Builtin)?;

                let runtime: BuiltinRuntime = fun.into();

                Ok(MachineState::Return(
                    context,
                    Value::Builtin { fun, runtime },
                ))
            }
            Term::Constr { tag, mut fields } => {
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
            Term::Case { constr, branches } => {
                self.step_and_maybe_spend(StepKind::Case)?;

                Ok(MachineState::Compute(
                    Context::FrameCases(env.clone(), branches, context.into()),
                    env,
                    constr.as_ref().clone(),
                ))
            }
        }
    }

    fn return_compute(&mut self, context: Context, value: Value) -> Result<MachineState, Error> {
        match context {
            Context::NoFrame => {
                if self.unbudgeted_steps[9] > 0 {
                    self.spend_unbudgeted_steps()?;
                }

                let term = discharge::value_as_term(value);

                Ok(MachineState::Done(term))
            }
            Context::FrameForce(ctx) => self.force_evaluate(*ctx, value),
            Context::FrameAwaitFunTerm(arg_env, arg, ctx) => Ok(MachineState::Compute(
                Context::FrameAwaitArg(value, ctx),
                arg_env,
                arg,
            )),
            Context::FrameAwaitArg(fun, ctx) => self.apply_evaluate(*ctx, fun, value),
            Context::FrameAwaitFunValue(arg, ctx) => self.apply_evaluate(*ctx, value, arg),
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
                        transfer_arg_stack(fields, *ctx),
                        env,
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

    fn force_evaluate(&mut self, context: Context, value: Value) -> Result<MachineState, Error> {
        match value {
            Value::Delay(body, env) => {
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
                    let term = discharge::value_as_term(Value::Builtin { fun, runtime });

                    Err(Error::BuiltinTermArgumentExpected(term))
                }
            }
            rest => Err(Error::NonPolymorphicInstantiation(rest)),
        }
    }

    fn apply_evaluate(
        &mut self,
        context: Context,
        function: Value,
        argument: Value,
    ) -> Result<MachineState, Error> {
        match function {
            Value::Lambda { body, mut env, .. } => {
                let e = Rc::make_mut(&mut env);

                e.push(argument);

                Ok(MachineState::Compute(
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

                    Ok(MachineState::Return(context, res))
                } else {
                    let term = discharge::value_as_term(Value::Builtin { fun, runtime });

                    Err(Error::UnexpectedBuiltinTermArgument(term))
                }
            }
            rest => Err(Error::NonFunctionalApplication(rest, argument)),
        }
    }

    fn eval_builtin_app(&mut self, runtime: BuiltinRuntime) -> Result<Value, Error> {
        let cost = runtime.to_ex_budget(&self.costs.builtin_costs);

        self.spend_budget(cost)?;

        runtime.call(&self.version, &mut self.logs)
    }

    fn lookup_var(&mut self, name: &NamedDeBruijn, env: &[Value]) -> Result<Value, Error> {
        env.get::<usize>(env.len() - usize::from(name.index))
            .cloned()
            .ok_or_else(|| Error::OpenTermEvaluated(Term::Var(name.clone().into())))
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

fn transfer_arg_stack(mut args: Vec<Value>, ctx: Context) -> Context {
    if args.is_empty() {
        ctx
    } else {
        let popped_field = args.pop().unwrap();

        transfer_arg_stack(args, Context::FrameAwaitFunValue(popped_field, ctx.into()))
    }
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
                    function: Term::Builtin(DefaultFunction::AddInteger).into(),
                    argument: Term::Constant(Constant::Integer(i128::MAX.into()).into()).into(),
                }
                .into(),
                argument: Term::Constant(Constant::Integer(i128::MAX.into()).into()).into(),
            },
        };

        let eval_result = program.eval(ExBudget::default());

        let term = eval_result.result().unwrap();

        assert_eq!(
            term,
            Term::Constant(
                Constant::Integer(
                    Into::<BigInt>::into(i128::MAX) + Into::<BigInt>::into(i128::MAX)
                )
                .into()
            )
        );
    }

    #[test]
    fn divide_integer() {
        let make_program = |fun: DefaultFunction, n: i32, m: i32| Program::<NamedDeBruijn> {
            version: (0, 0, 0),
            term: Term::Apply {
                function: Term::Apply {
                    function: Term::Builtin(fun).into(),
                    argument: Term::Constant(Constant::Integer(n.into()).into()).into(),
                }
                .into(),
                argument: Term::Constant(Constant::Integer(m.into()).into()).into(),
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
                Term::Constant(Constant::Integer(result.into()).into())
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
                            Term::Constant(Constant::Integer(n.into()).into()),
                            Term::Constant(Constant::Integer(m.into()).into()),
                        ],
                    }
                    .into(),
                    branches: vec![Term::Builtin(fun), Term::subtract_integer()],
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
                Term::Constant(Constant::Integer(result.into()).into())
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
                }
                .into(),
                branches: vec![
                    Term::integer(5.into()),
                    Term::integer(10.into()),
                    Term::integer(15.into()),
                ],
            },
        };

        let test_data = vec![(0, 5), (1, 10), (2, 15)];

        for (tag, result) in test_data {
            let eval_result = make_program(tag).eval(ExBudget::max());

            assert_eq!(
                eval_result.result().unwrap(),
                Term::Constant(Constant::Integer(result.into()).into())
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
}
