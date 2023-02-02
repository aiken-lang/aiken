use std::rc::Rc;

use indexmap::IndexMap;
use itertools::Itertools;

use crate::{
    ast::{builder::apply_wrap, Name, Program, Term},
    builtins::DefaultFunction,
};
// use crate::builtins::{DefaultFunction};

#[derive(Eq, Hash, PartialEq, Clone)]
pub struct Occurrence {
    name: Rc<Name>,
    lambda_count: usize,
}

impl Program<Name> {
    pub fn lambda_reduce(self) -> Program<Name> {
        let mut term = self.term.clone();
        lambda_reduce(&mut term);
        Program {
            version: self.version,
            term,
        }
    }

    pub fn builtin_force_reduce(self) -> Program<Name> {
        let mut term = self.term.clone();
        let mut builtin_map = IndexMap::new();
        builtin_force_reduce(&mut term, &mut builtin_map);

        for default_func_index in builtin_map.keys().sorted().cloned() {
            let default_func: DefaultFunction = default_func_index.try_into().unwrap();

            term = apply_wrap(
                Term::Lambda {
                    parameter_name: Name {
                        text: format!("__{}_wrapped", default_func.aiken_name()),
                        unique: 0.into(),
                    }
                    .into(),
                    body: term.into(),
                },
                if default_func.force_count() == 1 {
                    Term::Builtin(default_func).force_wrap()
                } else {
                    Term::Builtin(default_func).force_wrap().force_wrap()
                },
            );
        }

        Program {
            version: self.version,
            term,
        }
    }

    pub fn inline_reduce(self) -> Program<Name> {
        let mut term = self.term.clone();
        inline_reduce(&mut term);
        Program {
            version: self.version,
            term,
        }
    }
}

fn builtin_force_reduce(term: &mut Term<Name>, builtin_map: &mut IndexMap<u8, ()>) {
    match term {
        Term::Force(f) => {
            let f = Rc::make_mut(f);

            match f {
                Term::Force(inner_f) => {
                    if let Term::Builtin(func) = inner_f.as_ref() {
                        builtin_map.insert(*func as u8, ());
                        *term = Term::Var(
                            Name {
                                text: format!("__{}_wrapped", func.aiken_name()),
                                unique: 0.into(),
                            }
                            .into(),
                        );
                        return;
                    }
                }
                Term::Builtin(func) => {
                    builtin_map.insert(*func as u8, ());
                    *term = Term::Var(
                        Name {
                            text: format!("__{}_wrapped", func.aiken_name()),
                            unique: 0.into(),
                        }
                        .into(),
                    );

                    return;
                }
                _ => {}
            }
            builtin_force_reduce(f, builtin_map);
        }
        Term::Delay(d) => {
            let d = Rc::make_mut(d);
            builtin_force_reduce(d, builtin_map);
        }
        Term::Lambda { body, .. } => {
            let body = Rc::make_mut(body);
            builtin_force_reduce(body, builtin_map);
        }
        Term::Apply { function, argument } => {
            let func = Rc::make_mut(function);
            builtin_force_reduce(func, builtin_map);

            let arg = Rc::make_mut(argument);
            builtin_force_reduce(arg, builtin_map);
        }
        _ => {}
    }
}

fn inline_reduce(term: &mut Term<Name>) {
    match term {
        Term::Delay(d) => {
            let d = Rc::make_mut(d);
            inline_reduce(d);
        }
        Term::Lambda { body, .. } => {
            let body = Rc::make_mut(body);
            inline_reduce(body);
        }
        Term::Apply { function, argument } => {
            let arg = Rc::make_mut(argument);
            inline_reduce(arg);

            let func = Rc::make_mut(function);
            inline_reduce(func);

            if let Term::Lambda {
                parameter_name,
                body,
            } = func
            {
                let mut occurrences = 0;
                var_occurrences(body, parameter_name.clone(), &mut occurrences);
                if occurrences <= 1 {
                    *term = substitute_term(body.as_ref(), parameter_name.clone(), argument);
                }
            }
        }
        Term::Force(f) => {
            let f = Rc::make_mut(f);
            inline_reduce(f);
        }
        _ => {}
    }
}

fn var_occurrences(term: &Term<Name>, search_for: Rc<Name>, occurrences: &mut usize) {
    match term {
        Term::Var(name) => {
            if name.as_ref() == search_for.as_ref() {
                *occurrences += 1;
            }
        }
        Term::Delay(body) => {
            var_occurrences(body.as_ref(), search_for, occurrences);
        }
        Term::Lambda {
            parameter_name,
            body,
        } => {
            if parameter_name.clone() != search_for {
                var_occurrences(body.as_ref(), search_for, occurrences);
            }
        }
        Term::Apply { function, argument } => {
            var_occurrences(function.as_ref(), search_for.clone(), occurrences);
            var_occurrences(argument.as_ref(), search_for, occurrences);
        }
        Term::Force(x) => {
            var_occurrences(x.as_ref(), search_for, occurrences);
        }
        _ => {}
    }
}

fn lambda_reduce(term: &mut Term<Name>) {
    match term {
        Term::Apply { function, argument } => {
            let func = Rc::make_mut(function);
            lambda_reduce(func);

            let arg = Rc::make_mut(argument);
            lambda_reduce(arg);

            if let Term::Lambda {
                parameter_name,
                body,
            } = func
            {
                if let replace_term @ (Term::Var(_) | Term::Constant(_)) = argument.as_ref() {
                    let body = Rc::make_mut(body);
                    *term = substitute_term(body, parameter_name.clone(), replace_term);
                }
            }
        }
        Term::Delay(d) => {
            let d = Rc::make_mut(d);
            lambda_reduce(d);
        }
        Term::Lambda { body, .. } => {
            let body = Rc::make_mut(body);
            lambda_reduce(body);
        }
        Term::Force(f) => {
            let f = Rc::make_mut(f);
            lambda_reduce(f);
        }
        _ => {}
    }
}

fn substitute_term(term: &Term<Name>, original: Rc<Name>, replace_with: &Term<Name>) -> Term<Name> {
    match term {
        Term::Var(name) => {
            if name.as_ref() == original.as_ref() {
                replace_with.clone()
            } else {
                Term::Var(name.clone())
            }
        }
        Term::Delay(body) => {
            Term::Delay(substitute_term(body.as_ref(), original, replace_with).into())
        }
        Term::Lambda {
            parameter_name,
            body,
        } => {
            if parameter_name.as_ref() != original.as_ref() {
                Term::Lambda {
                    parameter_name: parameter_name.clone(),
                    body: Rc::new(substitute_term(body.as_ref(), original, replace_with)),
                }
            } else {
                Term::Lambda {
                    parameter_name: parameter_name.clone(),
                    body: body.clone(),
                }
            }
        }
        Term::Apply { function, argument } => Term::Apply {
            function: Rc::new(substitute_term(
                function.as_ref(),
                original.clone(),
                replace_with,
            )),
            argument: Rc::new(substitute_term(argument.as_ref(), original, replace_with)),
        },
        Term::Force(x) => Term::Force(Rc::new(substitute_term(x.as_ref(), original, replace_with))),
        x => x.clone(),
    }
}
