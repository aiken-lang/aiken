use std::rc::Rc;

use crate::ast::{Name, Program, Term};

impl Program<Name> {
    pub fn shrink(self) -> Program<Name> {
        Program {
            version: self.version,
            term: shrink_term(self.term),
        }
    }
}

fn shrink_term(term: Term<Name>) -> Term<Name> {
    let mut term = term;
    remove_dead_code(&mut term);

    match term {
        Term::Delay(term) => Term::Delay(Rc::new(shrink_term(term.as_ref().clone()))),
        Term::Lambda {
            parameter_name,
            body,
        } => Term::Lambda {
            parameter_name,
            body: Rc::new(shrink_term(body.as_ref().clone())),
        },
        Term::Apply { function, argument } => Term::Apply {
            function: Rc::new(shrink_term(function.as_ref().clone())),
            argument: Rc::new(shrink_term(argument.as_ref().clone())),
        },
        Term::Force(term) => Term::Force(Rc::new(shrink_term(term.as_ref().clone()))),
        x => x,
    }
}

pub fn remove_dead_code(a: &mut Term<Name>) {
    match &*a {
        Term::Apply { function, argument } => match function.as_ref() {
            Term::Lambda {
                parameter_name,
                body,
            } => match argument.as_ref() {
                Term::Var(t) => {
                    *a=substitute_var(body, parameter_name.clone(), Term::Var(t.clone()))
                },
                Term::Constant(x) => {
                    *a=substitute_var(body, parameter_name.clone(), Term::Constant(x.clone()))
                }
                _ => (),
            },
            _ => (),
        },
        _ => (),
    }
}

fn substitute_var(term: &Term<Name>, original: Name, replace_with: Term<Name>) -> Term<Name> {
    match term {
        Term::Var(name) => {
            if name.text == original.text {
                replace_with
            } else {
                Term::Var(name.clone())
            }
        }
        Term::Delay(body) => Term::Delay(Rc::new(substitute_var(
            body.as_ref(),
            original,
            replace_with,
        ))),
        Term::Lambda {
            parameter_name,
            body,
        } => Term::Lambda {
            parameter_name: parameter_name.clone(),
            body: Rc::new(substitute_var(body.as_ref(), original, replace_with)),
        },
        Term::Apply { function, argument } => Term::Apply {
            function: Rc::new(substitute_var(
                function.as_ref(),
                original.clone(),
                replace_with.clone(),
            )),
            argument: Rc::new(substitute_var(argument.as_ref(), original, replace_with)),
        },
        Term::Force(x) => Term::Force(Rc::new(substitute_var(x.as_ref(), original, replace_with))),
        x => x.clone(),
    }
}
