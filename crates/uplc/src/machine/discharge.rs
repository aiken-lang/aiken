use crate::ast::{NamedDeBruijn, Term};

use super::value::{Env, Value};

pub(super) fn value_as_term(value: Value) -> Term<NamedDeBruijn> {
    match value {
        Value::Con(x) => Term::Constant(x),
        Value::Builtin { runtime, fun } => {
            let mut term = Term::Builtin(fun);

            for _ in 0..runtime.forces {
                term = term.force();
            }

            for arg in runtime.args {
                term = term.apply(value_as_term(arg));
            }

            term
        }
        Value::Delay(body, env) => with_env(0, env, Term::Delay(body)),
        Value::Lambda {
            parameter_name,
            body,
            env,
        } => with_env(
            0,
            env,
            Term::Lambda {
                parameter_name: NamedDeBruijn {
                    text: parameter_name.text.clone(),
                    index: 0.into(),
                }
                .into(),
                body,
            },
        ),
        Value::Constr { tag, fields } => Term::Constr {
            tag,
            fields: fields.into_iter().map(value_as_term).collect(),
        },
    }
}

fn with_env(lam_cnt: usize, env: Env, term: Term<NamedDeBruijn>) -> Term<NamedDeBruijn> {
    match term {
        Term::Var(name) => {
            let index: usize = name.index.into();

            if lam_cnt >= index {
                Term::Var(name)
            } else {
                env.get::<usize>(env.len() - (index - lam_cnt))
                    .cloned()
                    .map_or(Term::Var(name), value_as_term)
            }
        }
        Term::Lambda {
            parameter_name,
            body,
        } => {
            let body = with_env(lam_cnt + 1, env, body.as_ref().clone());

            Term::Lambda {
                parameter_name,
                body: body.into(),
            }
        }
        Term::Apply { function, argument } => {
            let function = with_env(lam_cnt, env.clone(), function.as_ref().clone());
            let argument = with_env(lam_cnt, env, argument.as_ref().clone());

            Term::Apply {
                function: function.into(),
                argument: argument.into(),
            }
        }

        Term::Delay(x) => {
            let delay = with_env(lam_cnt, env, x.as_ref().clone());

            Term::Delay(delay.into())
        }
        Term::Force(x) => {
            let force = with_env(lam_cnt, env, x.as_ref().clone());

            Term::Force(force.into())
        }
        rest => rest,
    }
}
