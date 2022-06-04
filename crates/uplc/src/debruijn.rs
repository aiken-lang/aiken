use std::collections::HashMap;

use crate::ast::{DeBruijn, Name, NamedDeBruijn, Term, Unique};

#[derive(Debug, Copy, Clone)]
struct Level(usize);

pub(crate) struct Converter {
    current_level: Level,
    levels: Vec<HashMap<Unique, Level>>,
}

impl Converter {
    pub(crate) fn new() -> Self {
        Converter {
            current_level: Level(0),
            levels: vec![HashMap::new()],
        }
    }

    pub(crate) fn name_to_named_debruijn(
        &mut self,
        term: Term<Name>,
    ) -> anyhow::Result<Term<NamedDeBruijn>> {
        let converted_term = match term {
            Term::Var(Name { text, unique }) => Term::Var(NamedDeBruijn {
                text,
                index: self.get_index(unique)?,
            }),
            Term::Delay(term) => Term::Delay(Box::new(self.name_to_named_debruijn(*term)?)),
            Term::Lambda {
                parameter_name,
                body,
            } => {
                self.declare_unique(parameter_name.unique);

                let index = self.get_index(parameter_name.unique)?;

                let name = NamedDeBruijn {
                    text: parameter_name.text,
                    index,
                };

                self.start_scope();

                let body = self.name_to_named_debruijn(*body)?;

                self.end_scope();

                Term::Lambda {
                    parameter_name: name,
                    body: Box::new(body),
                }
            }
            Term::Apply { function, argument } => Term::Apply {
                function: Box::new(self.name_to_named_debruijn(*function)?),
                argument: Box::new(self.name_to_named_debruijn(*argument)?),
            },
            Term::Constant(constant) => Term::Constant(constant),
            Term::Force(term) => Term::Force(Box::new(self.name_to_named_debruijn(*term)?)),
            Term::Error => Term::Error,
            Term::Builtin(builtin) => Term::Builtin(builtin),
        };

        Ok(converted_term)
    }

    pub(crate) fn named_debruijn_to_name(
        &mut self,
        _term: Term<NamedDeBruijn>,
    ) -> anyhow::Result<Term<Name>> {
        todo!()
    }

    fn get_index(&mut self, unique: Unique) -> anyhow::Result<DeBruijn> {
        for scope in self.levels.iter().rev() {
            if let Some(found_level) = scope.get(&unique) {
                let index = self.current_level.0 - found_level.0;

                return Ok(index.into());
            }
        }

        anyhow::bail!("Free unique: {}", isize::from(unique))
    }

    fn declare_unique(&mut self, unique: Unique) {
        let scope = &mut self.levels[self.current_level.0];

        scope.insert(unique, self.current_level);
    }

    fn start_scope(&mut self) {
        self.current_level = Level(self.current_level.0 + 1);

        self.levels.push(HashMap::new());
    }

    fn end_scope(&mut self) {
        self.current_level = Level(self.current_level.0 - 1);

        self.levels.pop();
    }
}
