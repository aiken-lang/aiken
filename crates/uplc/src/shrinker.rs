use std::rc::Rc;

use crate::ast::{Name, Program, Term};

// pub fn subname(a: &Name, b: &Name, c: &Term<Name>) -> Term<Name> {
//     let mut stack: Vec<Term<Name>> = vec![c.clone()];
//     let mut i = 0;
//     while i < stack.len() {
//         match &stack[i] {
//             Term::Var(name) => {
//                 if name.eq(a) {
//                     stack.push(Term::Var(b.clone()))
//                 }else{
//                     stack.push(Term::Var(name.clone()))
//                 }
//             },
//             x => {
//                 stack.push(x);
//                 match x {

//                 }
//             }
//         };
//         i+=1;
//     }
//     todo!()
// }

pub fn remove_dead_code(a: Term<Name>) -> Term<Name> {
    match a {
        Term::Apply { function, argument } => {
            match &*function {
                Term::Lambda {
                    parameter_name,
                    body,
                } => {
                    // let argument = argument.as_ref();
                    match &*argument {
                        Term::Var(t) => body.subvar(*parameter_name, Term::Var(t)), //Some(subname(parameter_name, t, body)),
                        Term::Constant(x) => body.subvar(*parameter_name, Term::Constant(x)),
                        _ => a,
                    }
                }
                _ => a,
            }
        }
        _ => a,
    }
}

trait Shrinkable {
    fn subvar(self, a: Name, b: Term<Name>) -> Self;
    fn shrinkterm(self) -> Self;
}

impl Shrinkable for Term<Name> {
    fn subvar(self, a: Name, b: Term<Name>) -> Self {
        match self {
            Term::Var(name) => {
                if name.eq(&a) {
                    b.clone()
                } else {
                    Term::Var(name.clone())
                }
            }
            Term::Delay(body) => Term::Delay(Rc::new(body.as_ref().clone().subvar(a, b))),
            Term::Lambda {
                parameter_name,
                body,
            } => Term::Lambda {
                parameter_name: parameter_name.clone(),
                body: Rc::new(body.as_ref().clone().subvar(a, b)),
            },
            Term::Apply { function, argument } => Term::Apply {
                function: Rc::new(function.as_ref().clone().subvar(a, b)),
                argument: Rc::new(argument.as_ref().clone().subvar(a, b)),
            },
            Term::Force(x) => Term::Force(Rc::new(x.clone().subvar(a, b))),
            x => x.clone(),
        }
    }
    fn shrinkterm(self) -> Term<Name> {
        let mut a = self.clone();
        a = remove_dead_code(a);
        match &a {
            Term::Delay(term) => Term::Delay(Rc::new(term.as_ref().clone().shrinkterm())),
            Term::Lambda {
                parameter_name,
                body,
            } => Term::Lambda {
                parameter_name: parameter_name.clone(),
                body: Rc::new(body.as_ref().clone().shrinkterm()),
            },
            Term::Apply { function, argument } => Term::Apply {
                function: Rc::new(function.as_ref().clone().shrinkterm()),
                argument: Rc::new(argument.as_ref().clone().shrinkterm()),
            },
            Term::Force(term) => Term::Force(Rc::new(term.as_ref().clone().shrinkterm())),
            x => x.clone(),
        }
    }
}

pub fn shrink(program: Program<Name>) -> Program<Name> {
    Program {
        version: program.version,
        term: program.term.shrinkterm(),
    }
}
