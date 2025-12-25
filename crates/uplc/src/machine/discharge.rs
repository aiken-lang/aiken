use crate::ast::{NamedDeBruijn, Term};

use super::value::{Env, Value};

/// Convert a Value back to a Term (discharging closures).
/// Generic over context type C which is preserved through the conversion.
/// Newly created terms (like constants from values) use C::default() as context.
pub fn value_as_term<C: Clone + Default>(value: Value<C>) -> Term<NamedDeBruijn, C> {
    match value {
        Value::Con(x) => Term::Constant {
            value: x,
            context: C::default(),
        },
        Value::Builtin { runtime, fun } => {
            let mut term: Term<NamedDeBruijn, C> = Term::Builtin {
                func: fun,
                context: C::default(),
            };

            for _ in 0..runtime.forces {
                term = term.force();
            }

            // Args are now Value<C> since BuiltinRuntime preserves context
            for arg in runtime.args {
                let arg_term = value_as_term(arg);
                term = term.apply(arg_term);
            }

            term
        }
        Value::Delay(body, env) => {
            // The body already has context C from when it was stored
            // Wrap it in a Delay node with default context
            let inner = Term::Delay {
                term: body.clone(),
                context: C::default(),
            };
            with_env(0, env, inner)
        }
        Value::Lambda {
            parameter_name,
            body,
            env,
        } => {
            // The body already has context C from when it was stored
            // Wrap it in a Lambda node with default context
            let inner = Term::Lambda {
                parameter_name: NamedDeBruijn {
                    text: parameter_name.text.clone(),
                    index: 0.into(),
                }
                .into(),
                body: body.clone(),
                context: C::default(),
            };
            with_env(0, env, inner)
        }
        Value::Constr { tag, fields } => Term::Constr {
            tag,
            fields: fields.into_iter().map(value_as_term).collect(),
            context: C::default(),
        },
    }
}

/// Substitute variables in a term with values from the environment.
/// Preserves context C on existing nodes; uses C::default() for new nodes.
fn with_env<C: Clone + Default>(
    lam_cnt: usize,
    env: Env<C>,
    term: Term<NamedDeBruijn, C>,
) -> Term<NamedDeBruijn, C> {
    match term {
        Term::Var { name, context } => {
            let index: usize = name.index.into();

            if lam_cnt >= index {
                Term::Var { name, context }
            } else {
                env.get::<usize>(env.len() - (index - lam_cnt))
                    .cloned()
                    .map_or(Term::Var { name, context }, |(_, v)| value_as_term(v))
            }
        }
        Term::Lambda {
            parameter_name,
            body,
            context,
        } => {
            let body = with_env(lam_cnt + 1, env, body.as_ref().clone());

            Term::Lambda {
                parameter_name,
                body: body.into(),
                context,
            }
        }
        Term::Apply {
            function,
            argument,
            context,
        } => {
            let function = with_env(lam_cnt, env.clone(), function.as_ref().clone());
            let argument = with_env(lam_cnt, env, argument.as_ref().clone());

            Term::Apply {
                function: function.into(),
                argument: argument.into(),
                context,
            }
        }

        Term::Delay { term: x, context } => {
            let delay = with_env(lam_cnt, env, x.as_ref().clone());

            Term::Delay {
                term: delay.into(),
                context,
            }
        }
        Term::Force { term: x, context } => {
            let force = with_env(lam_cnt, env, x.as_ref().clone());

            Term::Force {
                term: force.into(),
                context,
            }
        }
        rest => rest,
    }
}
