use std::{collections::HashMap, rc::Rc};

use crate::ast::{Name, Program, Term, Unique};

#[derive(Eq, Hash, PartialEq, Clone)]
pub struct InternKey {
    name: String,
    previous_unique: Unique,
}

pub struct CodeGenInterner {
    identifiers: HashMap<InternKey, Unique>,
    current: Unique,
}

impl Default for CodeGenInterner {
    fn default() -> Self {
        Self::new()
    }
}

/// Interner that uses previous uniques to prevent future unique collisions
/// when performing optimizations
impl CodeGenInterner {
    pub fn new() -> Self {
        Self {
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
                name.unique = self.intern(name.text.clone(), name.unique);
            }
            Term::Delay(term) => self.term(Rc::make_mut(term)),
            Term::Lambda {
                parameter_name,
                body,
            } => {
                let parameter_name = Rc::make_mut(parameter_name);
                parameter_name.unique =
                    self.intern(parameter_name.text.clone(), parameter_name.unique);
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

    pub fn intern(&mut self, text: String, previous_unique: Unique) -> Unique {
        let key = InternKey {
            name: text,
            previous_unique,
        };

        if let Some(u) = self.identifiers.get(&key) {
            *u
        } else {
            let unique = self.current;

            self.identifiers.insert(key, self.current_unique());

            self.current.increment();

            unique
        }
    }

    pub fn current_unique(&self) -> Unique {
        self.current
    }
}
