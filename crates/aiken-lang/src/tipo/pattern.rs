//! Type inference and checking of patterns used in case expressions
//! and variables bindings.
use super::{
    environment::{assert_no_labeled_arguments, collapse_links, EntityKind, Environment},
    error::{Error, Warning},
    hydrator::Hydrator,
    PatternConstructor, Type, ValueConstructorVariant,
};
use crate::ast::{CallArg, Pattern, Span, TypedPattern, UntypedPattern};
use itertools::Itertools;
use std::{
    collections::{HashMap, HashSet},
    ops::Deref,
    rc::Rc,
};

pub struct PatternTyper<'a, 'b> {
    environment: &'a mut Environment<'b>,
    hydrator: &'a Hydrator,
    mode: PatternMode,
    initial_pattern_vars: HashSet<String>,
}

enum PatternMode {
    Initial,
    Alternative(Vec<String>),
}

impl<'a, 'b> PatternTyper<'a, 'b> {
    pub fn new(environment: &'a mut Environment<'b>, hydrator: &'a Hydrator) -> Self {
        Self {
            environment,
            hydrator,
            mode: PatternMode::Initial,
            initial_pattern_vars: HashSet::new(),
        }
    }

    #[allow(clippy::result_large_err)]
    fn insert_variable(
        &mut self,
        name: &str,
        typ: Rc<Type>,
        location: Span,
        err_location: Span,
    ) -> Result<(), Error> {
        match &mut self.mode {
            PatternMode::Initial => {
                // Register usage for the unused variable detection
                self.environment
                    .init_usage(name.to_string(), EntityKind::Variable, location);

                // Ensure there are no duplicate variable names in the pattern
                if self.initial_pattern_vars.contains(name) {
                    return Err(Error::DuplicateVarInPattern {
                        name: name.to_string(),
                        location: err_location,
                    });
                }
                // Record that this variable originated in this pattern so any
                // following alternative patterns can be checked to ensure they
                // have the same variables.
                self.initial_pattern_vars.insert(name.to_string());

                // And now insert the variable for use in the code that comes
                // after the pattern.
                self.environment.insert_variable(
                    name.to_string(),
                    ValueConstructorVariant::LocalVariable { location },
                    typ,
                );
                Ok(())
            }

            PatternMode::Alternative(assigned) => {
                match self.environment.scope.get(name) {
                    // This variable was defined in the Initial multi-pattern
                    Some(initial) if self.initial_pattern_vars.contains(name) => {
                        assigned.push(name.to_string());
                        let initial_typ = initial.tipo.clone();
                        self.environment
                            .unify(initial_typ, typ, err_location, false)
                    }

                    // This variable was not defined in the Initial multi-pattern
                    _ => Err(Error::ExtraVarInAlternativePattern {
                        name: name.to_string(),
                        location: err_location,
                    }),
                }
            }
        }
    }

    #[allow(clippy::result_large_err)]
    pub fn infer_alternative_pattern(
        &mut self,
        pattern: UntypedPattern,
        subject: &Type,
        location: &Span,
    ) -> Result<TypedPattern, Error> {
        self.mode = PatternMode::Alternative(vec![]);
        let typed_pattern = self.infer_pattern(pattern, subject)?;
        match &self.mode {
            PatternMode::Initial => panic!("Pattern mode switched from Alternative to Initial"),
            PatternMode::Alternative(assigned)
                if assigned.len() != self.initial_pattern_vars.len() =>
            {
                for name in assigned {
                    self.initial_pattern_vars.remove(name);
                }
                Err(Error::MissingVarInAlternativePattern {
                    location: *location,
                    // It is safe to use expect here as we checked the length above
                    name: self
                        .initial_pattern_vars
                        .iter()
                        .next()
                        .expect("Getting undefined pattern variable")
                        .clone(),
                })
            }
            PatternMode::Alternative(_) => Ok(typed_pattern),
        }
    }

    #[allow(clippy::result_large_err)]
    pub fn infer_pattern(
        &mut self,
        pattern: UntypedPattern,
        subject: &Type,
    ) -> Result<TypedPattern, Error> {
        self.unify(pattern, Rc::new(subject.clone()), None, false)
    }

    #[allow(clippy::result_large_err)]
    /// When we have an assignment or a case expression we unify the pattern with the
    /// inferred type of the subject in order to determine what variables to insert
    /// into the environment (or to detect a type error).
    pub fn unify(
        &mut self,
        pattern: UntypedPattern,
        tipo: Rc<Type>,
        ann_type: Option<Rc<Type>>,
        warn_on_discard: bool,
    ) -> Result<TypedPattern, Error> {
        match pattern {
            Pattern::Discard { name, location } => {
                if warn_on_discard {
                    // Register declaration for the unused variable detection
                    self.environment
                        .warnings
                        .push(Warning::DiscardedLetAssignment {
                            name: name.clone(),
                            location,
                        });
                };

                Ok(Pattern::Discard { name, location })
            }

            Pattern::Var { name, location } => {
                self.insert_variable(&name, ann_type.unwrap_or(tipo), location, location)?;

                Ok(Pattern::Var { name, location })
            }

            Pattern::Assign {
                name,
                pattern,
                location,
            } => {
                self.insert_variable(
                    &name,
                    ann_type.clone().unwrap_or_else(|| tipo.clone()),
                    location,
                    pattern.location(),
                )?;

                let pattern = self.unify(*pattern, tipo, ann_type, false)?;

                Ok(Pattern::Assign {
                    name,
                    pattern: Box::new(pattern),
                    location,
                })
            }

            Pattern::Int {
                location,
                value,
                base,
            } => {
                self.environment.unify(tipo, Type::int(), location, false)?;

                Ok(Pattern::Int {
                    location,
                    value,
                    base,
                })
            }

            Pattern::ByteArray {
                location,
                value,
                preferred_format,
            } => {
                self.environment
                    .unify(tipo, Type::byte_array(), location, false)?;

                Ok(Pattern::ByteArray {
                    location,
                    value,
                    preferred_format,
                })
            }

            Pattern::List {
                location,
                elements,
                tail,
            } => match tipo.get_app_args(true, false, "", "List", 1, self.environment) {
                Some(args) => {
                    let tipo = args
                        .first()
                        .expect("Failed to get type argument of List")
                        .clone();

                    let elements = elements
                        .into_iter()
                        .map(|element| self.unify(element, tipo.clone(), None, false))
                        .try_collect()?;

                    let tail = match tail {
                        Some(tail) => Some(Box::new(self.unify(
                            *tail,
                            Type::list(tipo),
                            None,
                            false,
                        )?)),
                        None => None,
                    };

                    Ok(Pattern::List {
                        location,
                        elements,
                        tail,
                    })
                }

                None => Err(Error::CouldNotUnify {
                    given: Type::list(self.environment.new_unbound_var()),
                    expected: tipo.clone(),
                    situation: None,
                    location,
                    rigid_type_names: HashMap::new(),
                }),
            },

            Pattern::Pair { fst, snd, location } => match collapse_links(tipo.clone()).deref() {
                Type::Pair {
                    fst: t_fst,
                    snd: t_snd,
                    ..
                } => {
                    let fst = Box::new(self.unify(*fst, t_fst.clone(), None, false)?);
                    let snd = Box::new(self.unify(*snd, t_snd.clone(), None, false)?);
                    Ok(Pattern::Pair { fst, snd, location })
                }

                Type::Var { .. } => {
                    let t_fst = self.environment.new_unbound_var();
                    let t_snd = self.environment.new_unbound_var();

                    self.environment.unify(
                        Type::pair(t_fst.clone(), t_snd.clone()),
                        tipo,
                        location,
                        false,
                    )?;

                    let fst = Box::new(self.unify(*fst, t_fst, None, false)?);
                    let snd = Box::new(self.unify(*snd, t_snd, None, false)?);

                    Ok(Pattern::Pair { fst, snd, location })
                }

                _ => Err(Error::CouldNotUnify {
                    given: Type::pair(
                        self.environment.new_unbound_var(),
                        self.environment.new_unbound_var(),
                    ),
                    expected: tipo,
                    situation: None,
                    location,
                    rigid_type_names: HashMap::new(),
                }),
            },

            Pattern::Tuple { elems, location } => match collapse_links(tipo.clone()).deref() {
                Type::Tuple {
                    elems: type_elems, ..
                } => {
                    if elems.len() != type_elems.len() {
                        return Err(Error::IncorrectTupleArity {
                            location,
                            expected: type_elems.len(),
                            given: elems.len(),
                        });
                    }

                    let mut patterns = vec![];

                    for (pattern, typ) in elems.into_iter().zip(type_elems) {
                        let typed_pattern = self.unify(pattern, typ.clone(), None, false)?;

                        patterns.push(typed_pattern);
                    }

                    Ok(Pattern::Tuple {
                        elems: patterns,
                        location,
                    })
                }

                Type::Var { .. } => {
                    let elems_types: Vec<_> = (0..(elems.len()))
                        .map(|_| self.environment.new_unbound_var())
                        .collect();

                    self.environment.unify(
                        Type::tuple(elems_types.clone()),
                        tipo,
                        location,
                        false,
                    )?;

                    let mut patterns = vec![];

                    for (pattern, type_) in elems.into_iter().zip(elems_types) {
                        let typed_pattern = self.unify(pattern, type_, None, false)?;

                        patterns.push(typed_pattern);
                    }

                    Ok(Pattern::Tuple {
                        elems: patterns,
                        location,
                    })
                }

                _ => {
                    let elems_types = (0..(elems.len()))
                        .map(|_| self.environment.new_unbound_var())
                        .collect();

                    Err(Error::CouldNotUnify {
                        given: Type::tuple(elems_types),
                        expected: tipo,
                        situation: None,
                        location,
                        rigid_type_names: HashMap::new(),
                    })
                }
            },

            Pattern::Constructor {
                location,
                module,
                name,
                arguments: mut pattern_args,
                spread_location,
                is_record,
                ..
            } => {
                // Register the value as seen for detection of unused values
                self.environment.increment_usage(&name);

                let cons =
                    self.environment
                        .get_value_constructor(module.as_ref(), &name, location)?;

                let has_no_fields = cons.field_map().is_none();

                match cons.field_map() {
                    // The fun has a field map so labelled arguments may be present and need to be reordered.
                    Some(field_map) => {
                        if spread_location.is_some() {
                            // Using the spread operator when you have already provided variables for all of the
                            // record's fields throws an error
                            if pattern_args.len() == field_map.arity {
                                return Err(Error::UnnecessarySpreadOperator {
                                    location: Span {
                                        start: location.end - 3,
                                        end: location.end - 1,
                                    },
                                    arity: field_map.arity,
                                });
                            }

                            // The location of the spread operator itself
                            let spread_location = Span {
                                start: location.end - 3,
                                end: location.end - 1,
                            };

                            // Insert discard variables to match the unspecified fields
                            // In order to support both positional and labelled arguments we have to insert
                            // them after all positional variables and before the labelled ones. This means
                            // we have calculate that index and then insert() the discards. It would be faster
                            // if we could put the discards anywhere which would let us use push().
                            // Potential future optimisation.
                            let index_of_first_labelled_arg = pattern_args
                                .iter()
                                .position(|a| a.label.is_some())
                                .unwrap_or(pattern_args.len());

                            while pattern_args.len() < field_map.arity {
                                let new_call_arg = CallArg {
                                    value: Pattern::Discard {
                                        name: "_".to_string(),
                                        location: spread_location,
                                    },
                                    location: spread_location,
                                    label: None,
                                };

                                pattern_args.insert(index_of_first_labelled_arg, new_call_arg);
                            }
                        }

                        field_map.reorder(&mut pattern_args, location)?
                    }

                    // The fun has no field map and so we error if arguments have been labelled
                    None => assert_no_labeled_arguments(&pattern_args)
                        .map(|(location, label)| {
                            Err(Error::UnexpectedLabeledArgInPattern {
                                location,
                                label,
                                name: name.clone(),
                                args: pattern_args.clone(),
                                module: module.clone(),
                                spread_location,
                            })
                        })
                        .unwrap_or(Ok(()))?,
                }

                let constructor_typ = cons.tipo.clone();
                let constructor = match cons.variant {
                    ValueConstructorVariant::Record { ref name, .. } => {
                        PatternConstructor::Record {
                            name: name.clone(),
                            field_map: cons.field_map().cloned(),
                        }
                    }
                    ValueConstructorVariant::LocalVariable { .. }
                    | ValueConstructorVariant::ModuleConstant { .. }
                    | ValueConstructorVariant::ModuleFn { .. } => {
                        panic!("Unexpected value constructor type for a constructor pattern.",)
                    }
                };

                if let Some(field_map) = cons.field_map() {
                    if !is_record {
                        let arguments = field_map
                            .fields
                            .iter()
                            .sorted_by(|(a, _), (b, _)| a.cmp(b))
                            .zip(pattern_args.iter())
                            .filter_map(|((field, (_, _)), arg)| {
                                if arg.value.is_discard() {
                                    None
                                } else {
                                    Some(CallArg {
                                        label: Some(field.clone()),
                                        location: arg
                                            .location
                                            .map(|start, _| (start, start + field.len())),
                                        ..arg.clone()
                                    })
                                }
                            })
                            .collect::<Vec<_>>();

                        let spread_location = if arguments.len() == field_map.fields.len() {
                            None
                        } else {
                            Some(Span {
                                start: location.end - 3,
                                end: location.end - 1,
                            })
                        };

                        self.environment.warnings.push(Warning::UnusedRecordFields {
                            location,
                            suggestion: Pattern::Constructor {
                                is_record: true,
                                location,
                                name: name.clone(),
                                arguments,
                                module: module.clone(),
                                constructor: (),
                                spread_location,
                                tipo: (),
                            },
                        });
                    }
                }

                let instantiated_constructor_type = self.environment.instantiate(
                    constructor_typ,
                    &mut HashMap::new(),
                    self.hydrator,
                );

                match instantiated_constructor_type.deref() {
                    Type::Fn { args, ret, .. } => {
                        if spread_location.is_some() && has_no_fields {
                            if pattern_args.len() == args.len() {
                                return Err(Error::UnnecessarySpreadOperator {
                                    location: Span {
                                        start: location.end - 3,
                                        end: location.end - 1,
                                    },
                                    arity: args.len(),
                                });
                            }

                            while pattern_args.len() < args.len() {
                                let location = Span {
                                    start: location.end - 3,
                                    end: location.end - 1,
                                };

                                pattern_args.push(CallArg {
                                    value: Pattern::Discard {
                                        name: "_".to_string(),
                                        location,
                                    },
                                    location,
                                    label: None,
                                });
                            }
                        }

                        if args.len() == pattern_args.len() {
                            let pattern_args = pattern_args
                                .into_iter()
                                .zip(args)
                                .map(|(arg, typ)| {
                                    let CallArg {
                                        value,
                                        location,
                                        label,
                                    } = arg;

                                    let value = self.unify(value, typ.clone(), None, false)?;

                                    Ok::<_, Error>(CallArg {
                                        value,
                                        location,
                                        label,
                                    })
                                })
                                .try_collect()?;

                            self.environment.unify(tipo, ret.clone(), location, false)?;

                            Ok(Pattern::Constructor {
                                location,
                                module,
                                name,
                                arguments: pattern_args,
                                constructor,
                                spread_location,
                                tipo: instantiated_constructor_type,
                                is_record,
                            })
                        } else {
                            Err(Error::IncorrectPatternArity {
                                location,
                                given: pattern_args,
                                expected: args.len(),
                                name: name.clone(),
                                module: module.clone(),
                                is_record,
                            })
                        }
                    }

                    Type::App { .. } => {
                        if pattern_args.is_empty() {
                            self.environment.unify(
                                tipo,
                                instantiated_constructor_type.clone(),
                                location,
                                false,
                            )?;

                            Ok(Pattern::Constructor {
                                location,
                                module,
                                name,
                                arguments: vec![],
                                constructor,
                                spread_location,
                                tipo: instantiated_constructor_type,
                                is_record,
                            })
                        } else {
                            Err(Error::IncorrectPatternArity {
                                location,
                                given: pattern_args,
                                expected: 0,
                                name: name.clone(),
                                module: module.clone(),
                                is_record,
                            })
                        }
                    }

                    _ => panic!("Unexpected constructor type for a constructor pattern.",),
                }
            }
        }
    }
}
