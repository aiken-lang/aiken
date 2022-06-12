use std::collections::HashMap;

use crate::ast::{Name, Program, Term, Unique};

pub struct Interner {
    identifiers: HashMap<String, Unique>,
    current: Unique,
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
            Term::Var(name) => name.unique = self.intern(&name.text),
            Term::Delay(term) => self.term(term),
            Term::Lambda {
                parameter_name,
                body,
            } => {
                parameter_name.unique = self.intern(&parameter_name.text);
                self.term(body);
            }
            Term::Apply { function, argument } => {
                self.term(function);
                self.term(argument);
            }
            Term::Constant(_) => (),
            Term::Force(term) => self.term(term),
            Term::Error => (),
            Term::Builtin(_) => (),
        }
    }

    fn intern(&mut self, text: &str) -> Unique {
        if let Some(u) = self.identifiers.get(text) {
            *u
        } else {
            let unique = self.current;

            self.identifiers.insert(text.to_string(), unique);

            self.current.increment();

            unique
        }
    }
}
