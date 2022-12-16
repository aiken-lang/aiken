use std::collections::{HashMap, HashSet};

use itertools::Itertools;

use super::error::Error;
use crate::ast::{CallArg, Span};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FieldMap {
    pub arity: usize,
    pub fields: HashMap<String, usize>,
}

impl FieldMap {
    pub fn new(arity: usize) -> Self {
        Self {
            arity,
            fields: HashMap::new(),
        }
    }

    pub fn insert(&mut self, label: String, index: usize, location: &Span) -> Result<(), Error> {
        match self.fields.insert(label.clone(), index) {
            Some(_) => Err(Error::DuplicateField {
                label,
                location: *location,
            }),
            None => Ok(()),
        }
    }

    pub fn into_option(self) -> Option<Self> {
        if self.fields.is_empty() {
            None
        } else {
            Some(self)
        }
    }

    /// Reorder an argument list so that labelled fields supplied out-of-order are
    /// in the correct order.
    pub fn reorder<A>(&self, args: &mut [CallArg<A>], location: Span) -> Result<(), Error> {
        let mut labeled_arguments_given = false;
        let mut seen_labels = std::collections::HashSet::new();
        let mut unknown_labels = Vec::new();

        if self.arity != args.len() {
            return Err(Error::IncorrectArity {
                labels: self.incorrect_arity_labels(args),
                location,
                expected: self.arity,
                given: args.len(),
            });
        }

        for arg in args.iter() {
            match &arg.label {
                Some(_) => {
                    labeled_arguments_given = true;
                }

                None => {
                    if labeled_arguments_given {
                        return Err(Error::PositionalArgumentAfterLabeled {
                            location: arg.location,
                        });
                    }
                }
            }
        }

        let mut i = 0;
        while i < args.len() {
            let label = &args.get(i).expect("Field indexing to get label").label;

            let (label, &location) = match label {
                // A labelled argument, we may need to reposition it in the array vector
                Some(l) => (
                    l,
                    &args
                        .get(i)
                        .expect("Indexing in labelled field reordering")
                        .location,
                ),

                // Not a labelled argument
                None => {
                    i += 1;
                    continue;
                }
            };

            let position = match self.fields.get(label) {
                None => {
                    unknown_labels.push((label.clone(), location));

                    i += 1;

                    continue;
                }

                Some(&p) => p,
            };

            // If the argument is already in the right place
            if position == i {
                seen_labels.insert(label.clone());

                i += 1;
            } else {
                if seen_labels.contains(label) {
                    return Err(Error::DuplicateArgument {
                        location,
                        label: label.to_string(),
                    });
                }

                seen_labels.insert(label.clone());

                args.swap(position, i);
            }
        }

        if unknown_labels.is_empty() {
            Ok(())
        } else {
            let valid = self.fields.keys().map(|t| t.to_string()).collect();

            Err(Error::UnknownLabels {
                valid,
                unknown: unknown_labels,
                supplied: seen_labels.into_iter().collect(),
            })
        }
    }

    pub fn incorrect_arity_labels<A>(&self, args: &[CallArg<A>]) -> Vec<String> {
        let given: HashSet<_> = args.iter().filter_map(|arg| arg.label.as_ref()).collect();

        self.fields
            .keys()
            .cloned()
            .filter(|f| !given.contains(f))
            .sorted()
            .collect()
    }
}
