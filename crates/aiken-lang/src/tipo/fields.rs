use super::error::{Error, UnknownLabels};
use crate::ast::{CallArg, Span};
use itertools::Itertools;
use std::collections::{HashMap, HashSet};

#[derive(Debug, Clone, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub struct FieldMap {
    pub arity: usize,
    pub fields: HashMap<String, (usize, Span)>,
    pub is_function: bool,
}

impl FieldMap {
    pub fn new(arity: usize, is_function: bool) -> Self {
        Self {
            arity,
            fields: HashMap::new(),
            is_function,
        }
    }

    #[allow(clippy::result_large_err)]
    pub fn insert(&mut self, label: String, index: usize, location: &Span) -> Result<(), Error> {
        match self.fields.insert(label.clone(), (index, *location)) {
            Some((_, location_other)) => {
                if self.is_function {
                    Err(Error::DuplicateArgument {
                        label,
                        location: *location,
                        duplicate_location: location_other,
                    })
                } else {
                    Err(Error::DuplicateField {
                        label,
                        location: *location,
                        duplicate_location: location_other,
                    })
                }
            }
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
    #[allow(clippy::result_large_err)]
    pub fn reorder<A>(&self, args: &mut [CallArg<A>], location: Span) -> Result<(), Error> {
        let mut last_labeled_arguments_given: Option<&CallArg<A>> = None;
        let mut seen_labels = std::collections::HashSet::new();
        let mut unknown_labels = Vec::new();

        if self.arity != args.len() {
            return Err(Error::IncorrectFieldsArity {
                labels: self.incorrect_arity_labels(args),
                location,
                expected: self.arity,
                given: args.len(),
            });
        }

        for arg in args.iter() {
            match &arg.label {
                Some(_) => {
                    last_labeled_arguments_given = Some(arg);
                }

                None => {
                    if let Some(label) = last_labeled_arguments_given {
                        return Err(Error::PositionalArgumentAfterLabeled {
                            location: arg.location,
                            labeled_arg_location: label.location,
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

            let (position, duplicate_location) = match self.fields.get(label) {
                None => {
                    unknown_labels.push(location);
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
                        duplicate_location,
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
            let valid = self.fields.keys().map(|t| t.to_string()).sorted().collect();

            Err(Error::UnknownLabels(vec![UnknownLabels {
                valid,
                unknown: unknown_labels,
                supplied: seen_labels.into_iter().collect(),
            }]))
        }
    }

    pub fn incorrect_arity_labels<A>(&self, args: &[CallArg<A>]) -> Vec<String> {
        let given: HashSet<_> = args.iter().filter_map(|arg| arg.label.as_ref()).collect();

        self.fields
            .keys()
            .filter(|f| !given.contains(f))
            .sorted()
            .cloned()
            .collect()
    }
}
