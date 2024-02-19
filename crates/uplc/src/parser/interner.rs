use std::{collections::HashMap, rc::Rc};

use crate::ast::{Name, Program, Term, Unique};

pub struct Interner {
    identifiers: HashMap<String, Unique>,
    current: Unique,
}

impl Default for Interner {
    fn default() -> Self {
        Self::new()
    }
}

impl Interner {
    pub fn new() -> Self {
        Interner {
            identifiers: HashMap::new(),
            current: Unique::new(0),
        }
    }

    pub fn program(&mut self, program: &mut Program<Name>) {
        self.term(&mut program.term);
    }

    pub fn term(&mut self, term: &mut Term<Name>) {
        match term {
            Term::Var(name) => {
                let name = Rc::make_mut(name);
                name.unique = self.intern(&name.text)
            }
            Term::Delay(term) => self.term(Rc::make_mut(term)),
            Term::Lambda {
                parameter_name,
                body,
            } => {
                let parameter_name = Rc::make_mut(parameter_name);
                parameter_name.unique = self.intern(&parameter_name.text);
                self.term(Rc::make_mut(body));
            }
            Term::Apply { function, argument } => {
                self.term(Rc::make_mut(function));
                self.term(Rc::make_mut(argument));
            }
            Term::Constant(_) => (),
            Term::Force(term) => self.term(Rc::make_mut(term)),
            Term::Error => (),
            Term::Builtin(_) => (),
            Term::Constr { fields, .. } => {
                for field in fields {
                    self.term(field);
                }
            }
            Term::Case { constr, branches } => {
                self.term(Rc::make_mut(constr));

                for branch in branches {
                    self.term(branch);
                }
            }
        }
    }

    pub fn intern(&mut self, text: &str) -> Unique {
        if let Some(u) = self.identifiers.get(text) {
            *u
        } else {
            let unique = self.current;

            self.identifiers.insert(text.to_string(), unique);

            self.current.increment();

            unique
        }
    }

    pub fn current_unique(&self) -> Unique {
        self.current
    }
}
