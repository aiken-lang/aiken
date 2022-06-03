use std::collections::HashMap;

use crate::ast::{DeBruijn, Name, NamedDeBruijn, Term, Unique};

struct Level(u64);

pub(crate) struct Converter {
    current_level: Level,
    levels: HashMap<Unique, Level>,
}

impl Converter {
    pub(crate) fn new() -> Self {
        Converter {
            current_level: Level(0),
            levels: HashMap::new(),
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
                todo!()
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

    pub(crate) fn named_debruijn_to_name(&mut self, term: Term<NamedDeBruijn>) -> Term<Name> {
        todo!()
    }

    fn get_index(&mut self, unique: Unique) -> anyhow::Result<DeBruijn> {
        if let Some(found_level) = self.levels.get(&unique) {
            let index = self.current_level.0 - found_level.0;

            Ok(index.into())
        } else {
            anyhow::bail!("Free unique: {}", isize::from(unique));
        }
    }
}
