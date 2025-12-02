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

        let mut positional_args_after_labeled = Vec::new();

        for arg in args.iter() {
            match &arg.label {
                Some(_) => {
                    last_labeled_arguments_given = Some(arg);
                }
                None => {
                    if let Some(label) = last_labeled_arguments_given {
                        positional_args_after_labeled.push((arg.location, label.location))
                    }
                }
            }
        }

        if positional_args_after_labeled.len() > 1 {
            let (location, labeled_arg_location) = positional_args_after_labeled
                .first()
                .expect("more than one positional args");

            return Err(Error::PositionalArgumentAfterLabeled {
                location: *location,
                labeled_arg_location: *labeled_arg_location,
            });
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

#[cfg(test)]
mod tests {
    use super::FieldMap;
    use crate::tipo::{Span, fields::CallArg};
    use proptest::prelude::*;
    use std::collections::HashMap;

    fn any_field() -> impl Strategy<Value = String> {
        proptest::string::string_regex("[a-zA-Z]+").unwrap()
    }

    prop_compose! {
        fn any_field_map()(
            fields in proptest::collection::vec(any_field(), 2..5),
            is_function in any::<bool>(),
        ) -> FieldMap {
            let fields = fields
                .into_iter()
                .enumerate()
                .map(|(ix, field)| (field, (ix, Span::empty())))
                .collect::<HashMap<_, _>>();

            FieldMap {
                arity: fields.len(),
                fields,
                is_function,
            }
        }
    }

    proptest! {
        #[test]
        fn reorder_never_fails_with_only_one_positional(
            field_map in any_field_map(),
            positional_arg_index in any::<usize>(),
        ) {
            let positional_arg_index = positional_arg_index % field_map.fields.len();

            let mut call_args = field_map.fields.keys().cloned().enumerate().map(|(index, label)| {
                CallArg {
                    label: if index == positional_arg_index { None } else { Some(label) },
                    location: Span::empty(),
                    value: (),
                }
            }).collect::<Vec<_>>();

            assert!(field_map.reorder(&mut call_args[..], Span::empty()).is_ok());

            for (actual_index, arg) in call_args.iter().enumerate() {
                if let Some(label) = &arg.label {
                    let (expected_index, _) = field_map.fields.get(label).unwrap();
                    assert_eq!(&actual_index, expected_index);
                }
            }
        }
    }
}
