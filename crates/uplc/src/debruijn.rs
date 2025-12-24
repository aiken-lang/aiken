use std::rc::Rc;

use thiserror::Error;

use crate::ast::{DeBruijn, FakeNamedDeBruijn, Name, NamedDeBruijn, Term, Unique};

mod bimap;

#[derive(Debug, Clone, PartialEq, Copy, Eq, Hash)]
struct Level(usize);

#[derive(Error, Debug)]
pub enum Error {
    #[error("Free Unique {} with name {}", .0.unique, .0.text)]
    FreeUnique(Name),
    #[error("Free Index `{0}`")]
    FreeIndex(DeBruijn),
}

pub struct Converter {
    current_level: Level,
    levels: Vec<bimap::BiMap>,
    current_unique: Unique,
}

impl Converter {
    pub fn new() -> Self {
        Converter {
            current_level: Level(0),
            levels: vec![bimap::BiMap::new()],
            current_unique: Unique::new(0),
        }
    }

    pub fn name_to_named_debruijn<C: Clone>(
        &mut self,
        term: &Term<Name, C>,
    ) -> Result<Term<NamedDeBruijn, C>, Error> {
        let converted_term = match term {
            Term::Var { name, context } => Term::Var {
                name: NamedDeBruijn {
                    text: name.text.to_string(),
                    index: self.get_index(name)?,
                }
                .into(),
                context: context.clone(),
            },
            Term::Delay { term: inner, context } => Term::Delay {
                term: Rc::new(self.name_to_named_debruijn(inner)?),
                context: context.clone(),
            },
            Term::Lambda {
                parameter_name,
                body,
                context,
            } => {
                self.declare_unique(parameter_name.unique);

                let index = self.get_index(parameter_name)?;

                let name = NamedDeBruijn {
                    text: parameter_name.text.to_string(),
                    index,
                };

                self.start_scope();

                let body = self.name_to_named_debruijn(body)?;

                self.end_scope();

                self.remove_unique(parameter_name.unique);

                Term::Lambda {
                    parameter_name: name.into(),
                    body: Rc::new(body),
                    context: context.clone(),
                }
            }
            Term::Apply { function, argument, context } => Term::Apply {
                function: Rc::new(self.name_to_named_debruijn(function)?),
                argument: Rc::new(self.name_to_named_debruijn(argument)?),
                context: context.clone(),
            },
            Term::Constant { value, context } => Term::Constant {
                value: value.clone(),
                context: context.clone(),
            },
            Term::Force { term: inner, context } => Term::Force {
                term: Rc::new(self.name_to_named_debruijn(inner)?),
                context: context.clone(),
            },
            Term::Error { context } => Term::Error { context: context.clone() },
            Term::Builtin { func, context } => Term::Builtin { func: *func, context: context.clone() },
            Term::Constr { tag, fields, context } => Term::Constr {
                tag: *tag,
                fields: fields
                    .iter()
                    .map(|field| self.name_to_named_debruijn(field))
                    .collect::<Result<_, _>>()?,
                context: context.clone(),
            },
            Term::Case { constr, branches, context } => Term::Case {
                constr: Rc::new(self.name_to_named_debruijn(constr)?),
                branches: branches
                    .iter()
                    .map(|branch| self.name_to_named_debruijn(branch))
                    .collect::<Result<_, _>>()?,
                context: context.clone(),
            },
        };

        Ok(converted_term)
    }

    pub fn name_to_debruijn(&mut self, term: &Term<Name>) -> Result<Term<DeBruijn>, Error> {
        let converted_term = match term {
            Term::Var { name, .. } => Term::Var {
                name: self.get_index(name)?.into(),
                context: (),
            },
            Term::Delay { term, .. } => Term::Delay {
                term: Rc::new(self.name_to_debruijn(term)?),
                context: (),
            },
            Term::Lambda {
                parameter_name,
                body,
                ..
            } => {
                self.declare_unique(parameter_name.unique);

                let name = self.get_index(parameter_name)?;

                self.start_scope();

                let body = self.name_to_debruijn(body)?;

                self.end_scope();

                self.remove_unique(parameter_name.unique);

                Term::Lambda {
                    parameter_name: name.into(),
                    body: Rc::new(body),
                    context: (),
                }
            }
            Term::Apply { function, argument, .. } => Term::Apply {
                function: Rc::new(self.name_to_debruijn(function)?),
                argument: Rc::new(self.name_to_debruijn(argument)?),
                context: (),
            },
            Term::Constant { value, .. } => Term::Constant {
                value: value.clone(),
                context: (),
            },
            Term::Force { term, .. } => Term::Force {
                term: Rc::new(self.name_to_debruijn(term)?),
                context: (),
            },
            Term::Error { .. } => Term::Error { context: () },
            Term::Builtin { func, .. } => Term::Builtin { func: *func, context: () },
            Term::Constr { tag, fields, .. } => Term::Constr {
                tag: *tag,
                fields: fields
                    .iter()
                    .map(|field| self.name_to_debruijn(field))
                    .collect::<Result<_, _>>()?,
                context: (),
            },
            Term::Case { constr, branches, .. } => Term::Case {
                constr: Rc::new(self.name_to_debruijn(constr)?),
                branches: branches
                    .iter()
                    .map(|branch| self.name_to_debruijn(branch))
                    .collect::<Result<_, _>>()?,
                context: (),
            },
        };

        Ok(converted_term)
    }

    pub fn named_debruijn_to_name<C: Clone>(
        &mut self,
        term: &Term<NamedDeBruijn, C>,
    ) -> Result<Term<Name, C>, Error> {
        let converted_term = match term {
            Term::Var { name: var_name, context } => Term::Var {
                name: Name {
                    text: var_name.text.to_string(),
                    unique: self.get_unique(&var_name.index)?,
                }
                .into(),
                context: context.clone(),
            },
            Term::Delay { term: inner, context } => Term::Delay {
                term: Rc::new(self.named_debruijn_to_name(inner)?),
                context: context.clone(),
            },
            Term::Lambda {
                parameter_name,
                body,
                context,
            } => {
                self.declare_binder();

                let unique = self.get_unique(&parameter_name.index)?;

                let name = Name {
                    text: parameter_name.text.to_string(),
                    unique,
                };

                self.start_scope();

                let body = self.named_debruijn_to_name(body)?;

                self.end_scope();

                Term::Lambda {
                    parameter_name: name.into(),
                    body: Rc::new(body),
                    context: context.clone(),
                }
            }
            Term::Apply { function, argument, context } => Term::Apply {
                function: Rc::new(self.named_debruijn_to_name(function)?),
                argument: Rc::new(self.named_debruijn_to_name(argument)?),
                context: context.clone(),
            },
            Term::Constant { value, context } => Term::Constant {
                value: value.clone(),
                context: context.clone(),
            },
            Term::Force { term: inner, context } => Term::Force {
                term: Rc::new(self.named_debruijn_to_name(inner)?),
                context: context.clone(),
            },
            Term::Error { context } => Term::Error { context: context.clone() },
            Term::Builtin { func, context } => Term::Builtin { func: *func, context: context.clone() },
            Term::Constr { tag, fields, context } => Term::Constr {
                tag: *tag,
                fields: fields
                    .iter()
                    .map(|field| self.named_debruijn_to_name(field))
                    .collect::<Result<_, _>>()?,
                context: context.clone(),
            },
            Term::Case { constr, branches, context } => Term::Case {
                constr: Rc::new(self.named_debruijn_to_name(constr)?),
                branches: branches
                    .iter()
                    .map(|branch| self.named_debruijn_to_name(branch))
                    .collect::<Result<_, _>>()?,
                context: context.clone(),
            },
        };

        Ok(converted_term)
    }

    pub fn debruijn_to_name(&mut self, term: &Term<DeBruijn>) -> Result<Term<Name>, Error> {
        let converted_term = match term {
            Term::Var { name: index, .. } => {
                let unique = self.get_unique(index)?;

                Term::Var {
                    name: Name {
                        text: format!("i_{unique}"),
                        unique,
                    }
                    .into(),
                    context: (),
                }
            }
            Term::Delay { term, .. } => Term::Delay {
                term: Rc::new(self.debruijn_to_name(term)?),
                context: (),
            },
            Term::Lambda {
                parameter_name,
                body,
                ..
            } => {
                self.declare_binder();

                let unique = self.get_unique(parameter_name)?;

                let name = Name {
                    text: format!("i_{unique}"),
                    unique,
                };

                self.start_scope();

                let body = self.debruijn_to_name(body)?;

                self.end_scope();

                Term::Lambda {
                    parameter_name: name.into(),
                    body: Rc::new(body),
                    context: (),
                }
            }
            Term::Apply { function, argument, .. } => Term::Apply {
                function: Rc::new(self.debruijn_to_name(function)?),
                argument: Rc::new(self.debruijn_to_name(argument)?),
                context: (),
            },
            Term::Constant { value, .. } => Term::Constant {
                value: value.clone(),
                context: (),
            },
            Term::Force { term, .. } => Term::Force {
                term: Rc::new(self.debruijn_to_name(term)?),
                context: (),
            },
            Term::Error { .. } => Term::Error { context: () },
            Term::Builtin { func, .. } => Term::Builtin { func: *func, context: () },
            Term::Constr { tag, fields, .. } => Term::Constr {
                tag: *tag,
                fields: fields
                    .iter()
                    .map(|field| self.debruijn_to_name(field))
                    .collect::<Result<_, _>>()?,
                context: (),
            },
            Term::Case { constr, branches, .. } => Term::Case {
                constr: Rc::new(self.debruijn_to_name(constr)?),
                branches: branches
                    .iter()
                    .map(|branch| self.debruijn_to_name(branch))
                    .collect::<Result<_, _>>()?,
                context: (),
            },
        };

        Ok(converted_term)
    }

    #[allow(clippy::only_used_in_recursion)]
    pub fn named_debruijn_to_debruijn(&mut self, term: &Term<NamedDeBruijn>) -> Term<DeBruijn> {
        match term {
            Term::Var { name, .. } => Term::Var {
                name: name.index.into(),
                context: (),
            },
            Term::Delay { term, .. } => Term::Delay {
                term: Rc::new(self.named_debruijn_to_debruijn(term)),
                context: (),
            },
            Term::Lambda {
                parameter_name,
                body,
                ..
            } => Term::Lambda {
                parameter_name: parameter_name.index.into(),
                body: Rc::new(self.named_debruijn_to_debruijn(body)),
                context: (),
            },
            Term::Apply { function, argument, .. } => Term::Apply {
                function: Rc::new(self.named_debruijn_to_debruijn(function)),
                argument: Rc::new(self.named_debruijn_to_debruijn(argument)),
                context: (),
            },
            Term::Constant { value, .. } => Term::Constant {
                value: value.clone(),
                context: (),
            },
            Term::Force { term, .. } => Term::Force {
                term: Rc::new(self.named_debruijn_to_debruijn(term)),
                context: (),
            },
            Term::Error { .. } => Term::Error { context: () },
            Term::Builtin { func, .. } => Term::Builtin { func: *func, context: () },
            Term::Constr { tag, fields, .. } => Term::Constr {
                tag: *tag,
                fields: fields
                    .iter()
                    .map(|field| self.named_debruijn_to_debruijn(field))
                    .collect(),
                context: (),
            },
            Term::Case { constr, branches, .. } => Term::Case {
                constr: Rc::new(self.named_debruijn_to_debruijn(constr)),
                branches: branches
                    .iter()
                    .map(|branch| self.named_debruijn_to_debruijn(branch))
                    .collect(),
                context: (),
            },
        }
    }

    #[allow(clippy::only_used_in_recursion)]
    pub fn debruijn_to_named_debruijn(&mut self, term: &Term<DeBruijn>) -> Term<NamedDeBruijn> {
        match term {
            Term::Var { name, .. } => Term::Var {
                name: NamedDeBruijn {
                    text: "i".to_string(),
                    index: *name.as_ref(),
                }
                .into(),
                context: (),
            },
            Term::Delay { term, .. } => Term::Delay {
                term: Rc::new(self.debruijn_to_named_debruijn(term)),
                context: (),
            },
            Term::Lambda {
                parameter_name,
                body,
                ..
            } => Term::Lambda {
                parameter_name: NamedDeBruijn::from(*parameter_name.as_ref()).into(),
                body: Rc::new(self.debruijn_to_named_debruijn(body)),
                context: (),
            },
            Term::Apply { function, argument, .. } => Term::Apply {
                function: Rc::new(self.debruijn_to_named_debruijn(function)),
                argument: Rc::new(self.debruijn_to_named_debruijn(argument)),
                context: (),
            },
            Term::Constant { value, .. } => Term::Constant {
                value: value.clone(),
                context: (),
            },
            Term::Force { term, .. } => Term::Force {
                term: Rc::new(self.debruijn_to_named_debruijn(term)),
                context: (),
            },
            Term::Error { .. } => Term::Error { context: () },
            Term::Builtin { func, .. } => Term::Builtin { func: *func, context: () },
            Term::Constr { tag, fields, .. } => Term::Constr {
                tag: *tag,
                fields: fields
                    .iter()
                    .map(|field| self.debruijn_to_named_debruijn(field))
                    .collect(),
                context: (),
            },
            Term::Case { constr, branches, .. } => Term::Case {
                constr: Rc::new(self.debruijn_to_named_debruijn(constr)),
                branches: branches
                    .iter()
                    .map(|branch| self.debruijn_to_named_debruijn(branch))
                    .collect(),
                context: (),
            },
        }
    }

    #[allow(clippy::only_used_in_recursion)]
    pub fn fake_named_debruijn_to_named_debruijn(
        &mut self,
        term: &Term<FakeNamedDeBruijn>,
    ) -> Term<NamedDeBruijn> {
        match term {
            Term::Var { name, .. } => Term::Var {
                name: NamedDeBruijn::from(name.as_ref().clone()).into(),
                context: (),
            },
            Term::Delay { term, .. } => Term::Delay {
                term: Rc::new(self.fake_named_debruijn_to_named_debruijn(term)),
                context: (),
            },
            Term::Lambda {
                parameter_name,
                body,
                ..
            } => Term::Lambda {
                parameter_name: NamedDeBruijn::from(parameter_name.as_ref().clone()).into(),
                body: Rc::new(self.fake_named_debruijn_to_named_debruijn(body)),
                context: (),
            },
            Term::Apply { function, argument, .. } => Term::Apply {
                function: Rc::new(self.fake_named_debruijn_to_named_debruijn(function)),
                argument: Rc::new(self.fake_named_debruijn_to_named_debruijn(argument)),
                context: (),
            },
            Term::Constant { value, .. } => Term::Constant {
                value: value.clone(),
                context: (),
            },
            Term::Force { term, .. } => Term::Force {
                term: Rc::new(self.fake_named_debruijn_to_named_debruijn(term)),
                context: (),
            },
            Term::Error { .. } => Term::Error { context: () },
            Term::Builtin { func, .. } => Term::Builtin { func: *func, context: () },
            Term::Constr { tag, fields, .. } => Term::Constr {
                tag: *tag,
                fields: fields
                    .iter()
                    .map(|field| self.fake_named_debruijn_to_named_debruijn(field))
                    .collect(),
                context: (),
            },
            Term::Case { constr, branches, .. } => Term::Case {
                constr: Rc::new(self.fake_named_debruijn_to_named_debruijn(constr)),
                branches: branches
                    .iter()
                    .map(|branch| self.fake_named_debruijn_to_named_debruijn(branch))
                    .collect(),
                context: (),
            },
        }
    }

    #[allow(clippy::only_used_in_recursion)]
    pub fn named_debruijn_to_fake_named_debruijn(
        &mut self,
        term: &Term<NamedDeBruijn>,
    ) -> Term<FakeNamedDeBruijn> {
        match term {
            Term::Var { name, .. } => Term::Var {
                name: FakeNamedDeBruijn::from(name.as_ref().clone()).into(),
                context: (),
            },
            Term::Delay { term, .. } => Term::Delay {
                term: Rc::new(self.named_debruijn_to_fake_named_debruijn(term)),
                context: (),
            },
            Term::Lambda {
                parameter_name,
                body,
                ..
            } => Term::Lambda {
                parameter_name: FakeNamedDeBruijn::from(parameter_name.as_ref().clone()).into(),
                body: Rc::new(self.named_debruijn_to_fake_named_debruijn(body)),
                context: (),
            },
            Term::Apply { function, argument, .. } => Term::Apply {
                function: Rc::new(self.named_debruijn_to_fake_named_debruijn(function)),
                argument: Rc::new(self.named_debruijn_to_fake_named_debruijn(argument)),
                context: (),
            },
            Term::Constant { value, .. } => Term::Constant {
                value: value.clone(),
                context: (),
            },
            Term::Force { term, .. } => Term::Force {
                term: Rc::new(self.named_debruijn_to_fake_named_debruijn(term)),
                context: (),
            },
            Term::Error { .. } => Term::Error { context: () },
            Term::Builtin { func, .. } => Term::Builtin { func: *func, context: () },
            Term::Constr { tag, fields, .. } => Term::Constr {
                tag: *tag,
                fields: fields
                    .iter()
                    .map(|field| self.named_debruijn_to_fake_named_debruijn(field))
                    .collect(),
                context: (),
            },
            Term::Case { constr, branches, .. } => Term::Case {
                constr: Rc::new(self.named_debruijn_to_fake_named_debruijn(constr)),
                branches: branches
                    .iter()
                    .map(|branch| self.named_debruijn_to_fake_named_debruijn(branch))
                    .collect(),
                context: (),
            },
        }
    }

    fn get_index(&mut self, name: &Name) -> Result<DeBruijn, Error> {
        for scope in self.levels.iter().rev() {
            if let Some(found_level) = scope.get(&name.unique) {
                let index = self.current_level.0 - found_level.0;

                return Ok(index.into());
            }
        }

        Err(Error::FreeUnique(name.clone()))
    }

    fn get_unique(&mut self, index: &DeBruijn) -> Result<Unique, Error> {
        for scope in self.levels.iter().rev() {
            let index = Level(
                self.current_level
                    .0
                    .checked_sub(index.inner())
                    .ok_or(Error::FreeIndex(*index))?,
            );

            if let Some(unique) = scope.get_right(&index) {
                return Ok(*unique);
            }
        }

        Err(Error::FreeIndex(*index))
    }

    fn declare_unique(&mut self, unique: Unique) {
        let scope = &mut self.levels[self.current_level.0];

        scope.insert(unique, self.current_level);
    }

    fn remove_unique(&mut self, unique: Unique) {
        let scope = &mut self.levels[self.current_level.0];

        scope.remove(unique, self.current_level);
    }

    fn declare_binder(&mut self) {
        let scope = &mut self.levels[self.current_level.0];

        scope.insert(self.current_unique, self.current_level);

        self.current_unique.increment();
    }

    fn start_scope(&mut self) {
        self.current_level = Level(self.current_level.0 + 1);

        self.levels.push(bimap::BiMap::new());
    }

    fn end_scope(&mut self) {
        self.current_level = Level(self.current_level.0 - 1);

        self.levels.pop();
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{Name, NamedDeBruijn, Term};
    use std::rc::Rc;

    #[test]
    fn test_roundtrip_preserves_context() {
        // Create a simple term with non-default spans
        let term: Term<Name, u64> = Term::Lambda {
            parameter_name: Rc::new(Name {
                text: "x".to_string(),
                unique: 0.into(),
            }),
            body: Rc::new(Term::Var {
                name: Rc::new(Name {
                    text: "x".to_string(),
                    unique: 0.into(),
                }),
                context: 42,
            }),
            context: 100,
        };

        // Convert to NamedDeBruijn and back
        let mut converter = Converter::new();
        let debruijn: Term<NamedDeBruijn, u64> = converter.name_to_named_debruijn(&term).unwrap();
        
        // Check context is preserved in debruijn
        match &debruijn {
            Term::Lambda { context, body, .. } => {
                assert_eq!(*context, 100, "Lambda context should be preserved");
                match body.as_ref() {
                    Term::Var { context, .. } => {
                        assert_eq!(*context, 42, "Var context should be preserved");
                    }
                    _ => panic!("Expected Var"),
                }
            }
            _ => panic!("Expected Lambda"),
        }

        // Convert back to Name
        let mut converter = Converter::new();
        let back: Term<Name, u64> = converter.named_debruijn_to_name(&debruijn).unwrap();

        // Check context is still preserved
        match &back {
            Term::Lambda { context, body, .. } => {
                assert_eq!(*context, 100, "Lambda context should be preserved after roundtrip");
                match body.as_ref() {
                    Term::Var { context, .. } => {
                        assert_eq!(*context, 42, "Var context should be preserved after roundtrip");
                    }
                    _ => panic!("Expected Var"),
                }
            }
            _ => panic!("Expected Lambda"),
        }
    }
}
