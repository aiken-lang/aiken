use std::{collections::HashMap, rc::Rc};

use crate::ast::{Name, Program, Term, Unique};

#[derive(Eq, Hash, PartialEq, Clone)]
pub struct InternKey {
    name: String,
    previous_unique: Unique,
}

/// Reassigns `Unique`s after optimization so the resulting named UPLC remains
/// structurally sound when converted back to DeBruijn form.
///
/// The optimizer passes frequently clone or synthesize `Name`s with placeholder
/// uniques, often reusing `Unique(0)` for multiple distinct binders. A simple
/// global map from `(text, previous_unique)` to a fresh unique is not enough in
/// that setting, because shadowing binders with the same source pair must still
/// receive different uniques while they are simultaneously in scope.
///
/// This interner therefore behaves like a scoped binder stack:
/// - `bind`: allocates a fresh unique for the binder currently being entered,
/// - `lookup`: resolves variables to the innermost active binder with the same `(text, previous_unique)` pair,
/// - `unbind`: pops that binder once we leave its scope.
///
/// The end result is that optimized terms preserve the same binding structure
/// as the original tree, even when the optimizer duplicated or rebuilt names.
pub struct CodeGenInterner {
    identifiers: HashMap<InternKey, Vec<Unique>>,
    current: Unique,
}

impl Default for CodeGenInterner {
    fn default() -> Self {
        Self::new()
    }
}

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
                name.unique = self.lookup(name.text.clone(), name.unique);
            }
            Term::Delay(term) => self.term(Rc::make_mut(term)),
            Term::Lambda {
                parameter_name,
                body,
            } => {
                let parameter_name = Rc::make_mut(parameter_name);
                let text = parameter_name.text.clone();
                let previous_unique = parameter_name.unique;

                parameter_name.unique = self.bind(text.clone(), previous_unique);
                self.term(Rc::make_mut(body));
                self.unbind(text, previous_unique);
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

    /// Register a binder entering scope and return the fresh unique that should
    /// replace its current placeholder unique.
    ///
    /// Multiple binders may share the same `(text, previous_unique)` pair after
    /// optimization. We therefore keep a stack per key and always push a fresh
    /// unique for the binder occurrence currently being traversed.
    fn bind(&mut self, text: String, previous_unique: Unique) -> Unique {
        let key = InternKey {
            name: text,
            previous_unique,
        };
        let unique = self.fresh_unique();
        self.identifiers.entry(key).or_default().push(unique);
        unique
    }

    /// Resolve a variable occurrence to the innermost binder currently in
    /// scope for its `(text, previous_unique)` pair.
    ///
    /// If no active binder exists, we still mint a fresh unique instead of
    /// panicking. That preserves the previous behaviour: genuinely invalid or
    /// free variables are still rejected later by the DeBruijn pass, but this
    /// interning pass itself stays total.
    fn lookup(&mut self, text: String, previous_unique: Unique) -> Unique {
        let key = InternKey {
            name: text,
            previous_unique,
        };

        if let Some(u) = self
            .identifiers
            .get(&key)
            .and_then(|uniques| uniques.last())
        {
            *u
        } else {
            // Preserve the old behaviour for invalid/free variables so the later
            // DeBruijn pass can still surface the actual scoping error.
            self.fresh_unique()
        }
    }

    /// Leave the scope of the binder identified by `(text, previous_unique)`.
    ///
    /// This must mirror a prior `bind` call for the same binder occurrence. We
    /// pop only the innermost entry so outer shadowed binders with the same key
    /// become visible again after exiting the nested scope.
    fn unbind(&mut self, text: String, previous_unique: Unique) {
        let key = InternKey {
            name: text,
            previous_unique,
        };

        let remove_entry = {
            let uniques = self
                .identifiers
                .get_mut(&key)
                .expect("missing binder during interning");

            uniques.pop().expect("empty binder stack during interning");
            uniques.is_empty()
        };

        if remove_entry {
            self.identifiers.remove(&key);
        }
    }

    fn fresh_unique(&mut self) -> Unique {
        let unique = self.current;
        self.current.increment();
        unique
    }
}

#[cfg(test)]
mod tests {
    use super::CodeGenInterner;
    use crate::ast::{Program, Term};

    #[test]
    fn fresh_shadowing_binders_get_distinct_uniques() {
        let mut program = Program {
            version: (1, 0, 0),
            term: Term::var("x").lambda("x").apply(Term::var("x")).lambda("x"),
        };

        CodeGenInterner::new().program(&mut program);

        let Term::Lambda {
            parameter_name: outer,
            body,
        } = &program.term
        else {
            panic!("expected outer lambda");
        };

        let Term::Apply { function, argument } = body.as_ref() else {
            panic!("expected application in outer body");
        };

        let Term::Lambda {
            parameter_name: inner,
            body,
        } = function.as_ref()
        else {
            panic!("expected inner lambda");
        };

        let Term::Var(inner_var) = body.as_ref() else {
            panic!("expected inner variable");
        };

        let Term::Var(outer_var) = argument.as_ref() else {
            panic!("expected outer variable");
        };

        assert_ne!(outer.unique, inner.unique);
        assert_eq!(inner.unique, inner_var.unique);
        assert_eq!(outer.unique, outer_var.unique);
    }
}
