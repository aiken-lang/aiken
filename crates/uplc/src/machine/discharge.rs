use std::rc::Rc;

use crate::ast::{Constant, NamedDeBruijn, Term};

use super::value::{Env, Value};

#[derive(Clone)]
enum PartialTerm {
    Delay,
    Lambda(Rc<NamedDeBruijn>),
    Apply,
    Force,
}

#[derive(Clone)]
enum DischargeStep {
    DischargeValue(Value),
    DischargeValueEnv(usize, Env, Rc<Term<NamedDeBruijn>>),
    PopArgStack(PartialTerm),
}

pub(super) fn value_as_term(value: Value) -> Rc<Term<NamedDeBruijn>> {
    let mut stack = vec![DischargeStep::DischargeValue(value)];

    let mut arg_stack = vec![];

    while let Some(stack_frame) = stack.pop() {
        match stack_frame {
            DischargeStep::DischargeValue(value) => match value {
                Value::Con(x) => arg_stack.push(Term::Constant(x.clone()).into()),
                Value::Builtin { .. } => {
                    arg_stack.push(Term::Constant(Constant::Unit.into()).into())
                }
                Value::Delay(body, env) => {
                    stack.push(DischargeStep::DischargeValueEnv(
                        0,
                        env.clone(),
                        Term::Delay(body.clone()).into(),
                    ));
                }
                Value::Lambda {
                    parameter_name,
                    body,
                    env,
                } => {
                    stack.push(DischargeStep::DischargeValueEnv(
                        0,
                        env.clone(),
                        Term::Lambda {
                            parameter_name: parameter_name.clone(),
                            body: body.clone(),
                        }
                        .into(),
                    ));
                }
            },
            DischargeStep::DischargeValueEnv(lam_cnt, env, term) => match term.as_ref() {
                Term::Var(name) => {
                    let index: usize = name.index.into();

                    if lam_cnt >= index {
                        arg_stack.push(Rc::new(Term::Var(name.clone())));
                    } else {
                        let env = env.get::<usize>(env.len() - (index - lam_cnt)).cloned();

                        if let Some(v) = env {
                            stack.push(DischargeStep::DischargeValue(v));
                        } else {
                            arg_stack.push(Rc::new(Term::Var(name.clone())));
                        }
                    }
                }
                Term::Lambda {
                    parameter_name,
                    body,
                } => {
                    stack.push(DischargeStep::PopArgStack(PartialTerm::Lambda(
                        parameter_name.to_owned(),
                    )));
                    stack.push(DischargeStep::DischargeValueEnv(
                        lam_cnt + 1,
                        env,
                        body.to_owned(),
                    ));
                }
                Term::Apply { function, argument } => {
                    stack.push(DischargeStep::PopArgStack(PartialTerm::Apply));
                    stack.push(DischargeStep::DischargeValueEnv(
                        lam_cnt,
                        env.clone(),
                        argument.to_owned(),
                    ));
                    stack.push(DischargeStep::DischargeValueEnv(
                        lam_cnt,
                        env,
                        function.to_owned(),
                    ));
                }
                Term::Delay(body) => {
                    stack.push(DischargeStep::PopArgStack(PartialTerm::Delay));
                    stack.push(DischargeStep::DischargeValueEnv(
                        lam_cnt,
                        env.clone(),
                        body.to_owned(),
                    ));
                }

                Term::Force(body) => {
                    stack.push(DischargeStep::PopArgStack(PartialTerm::Force));
                    stack.push(DischargeStep::DischargeValueEnv(
                        lam_cnt,
                        env.clone(),
                        body.to_owned(),
                    ));
                }
                rest => {
                    arg_stack.push(rest.to_owned().into());
                }
            },
            DischargeStep::PopArgStack(term) => match term {
                PartialTerm::Delay => {
                    let body = arg_stack.pop().unwrap();
                    arg_stack.push(Term::Delay(body).into())
                }
                PartialTerm::Lambda(parameter_name) => {
                    let body = arg_stack.pop().unwrap();
                    arg_stack.push(
                        Term::Lambda {
                            parameter_name,
                            body,
                        }
                        .into(),
                    )
                }
                PartialTerm::Apply => {
                    let argument = arg_stack.pop().unwrap();
                    let function = arg_stack.pop().unwrap();

                    arg_stack.push(Term::Apply { function, argument }.into());
                }
                PartialTerm::Force => {
                    let body = arg_stack.pop().unwrap();
                    arg_stack.push(Term::Force(body).into())
                }
            },
        }
    }

    arg_stack.pop().unwrap()
}
