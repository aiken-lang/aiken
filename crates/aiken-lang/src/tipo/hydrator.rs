use super::{
    environment::Environment,
    error::{Error, Warning},
    Type, TypeConstructor,
};
use crate::{ast::Annotation, tipo::Span};
use std::{collections::HashMap, rc::Rc};

/// The Hydrator takes an AST representing a type (i.e. a type annotation
/// for a function argument) and returns a Type for that annotation.
///
/// If a valid Type cannot be constructed it returns an error.
///
/// It keeps track of any type variables created. This is useful for:
///
/// - Determining if a generic type variable should be made into an
///   unbound type variable during type instantiation.
/// - Ensuring that the same type is constructed if the programmer
///   uses the same name for a type variable multiple times.
///
#[derive(Debug)]
pub struct Hydrator {
    created_type_variables: HashMap<String, Rc<Type>>,
    /// A rigid type is a generic type that was specified as being generic in
    /// an annotation. As such it should never be instantiated into an unbound
    /// variable. This type_id => name map is used for reporting the original
    /// annotated name on error.
    rigid_type_names: HashMap<u64, String>,
    permit_new_type_variables: bool,
}

#[derive(Debug)]
pub struct ScopeResetData {
    created_type_variables: HashMap<String, Rc<Type>>,
    rigid_type_names: HashMap<u64, String>,
}

impl Default for Hydrator {
    fn default() -> Self {
        Self::new()
    }
}

impl Hydrator {
    pub fn new() -> Hydrator {
        Hydrator {
            created_type_variables: HashMap::new(),
            rigid_type_names: HashMap::new(),
            permit_new_type_variables: true,
        }
    }

    pub fn open_new_scope(&mut self) -> ScopeResetData {
        let created_type_variables = self.created_type_variables.clone();
        let rigid_type_names = self.rigid_type_names.clone();

        ScopeResetData {
            created_type_variables,
            rigid_type_names,
        }
    }

    pub fn close_scope(&mut self, data: ScopeResetData) {
        self.created_type_variables = data.created_type_variables;
        self.rigid_type_names = data.rigid_type_names;
    }

    pub fn disallow_new_type_variables(&mut self) {
        self.permit_new_type_variables = false
    }

    /// A rigid type is a generic type that was specified as being generic in
    /// an annotation. As such it should never be instantiated into an unbound
    /// variable.
    pub fn is_rigid(&self, id: &u64) -> bool {
        self.rigid_type_names.contains_key(id)
    }

    pub fn rigid_names(&self) -> HashMap<u64, String> {
        self.rigid_type_names.clone()
    }

    #[allow(clippy::result_large_err)]
    pub fn type_from_option_annotation(
        &mut self,
        ast: &Option<Annotation>,
        environment: &mut Environment,
    ) -> Result<Rc<Type>, Error> {
        match ast {
            Some(ast) => self.type_from_annotation(ast, environment),
            None => Ok(environment.new_unbound_var()),
        }
    }

    #[allow(clippy::result_large_err)]
    pub fn type_from_annotation(
        &mut self,
        annotation: &Annotation,
        environment: &mut Environment,
    ) -> Result<Rc<Type>, Error> {
        let mut unbounds = vec![];
        let tipo = self.do_type_from_annotation(annotation, environment, &mut unbounds)?;

        if let Some(location) = unbounds.last() {
            environment.warnings.push(Warning::UnexpectedTypeHole {
                location: **location,
                tipo: tipo.clone(),
            });
        }

        Ok(tipo)
    }

    /// Construct a Type from an AST Type annotation.
    #[allow(clippy::result_large_err)]
    fn do_type_from_annotation<'a>(
        &mut self,
        annotation: &'a Annotation,
        environment: &mut Environment,
        unbounds: &mut Vec<&'a Span>,
    ) -> Result<Rc<Type>, Error> {
        let return_type = match annotation {
            Annotation::Constructor {
                location,
                module,
                name,
                arguments: args,
            } => {
                // Hydrate the type argument AST into types
                let mut argument_types = Vec::with_capacity(args.len());
                for t in args {
                    let typ = self.do_type_from_annotation(t, environment, unbounds)?;
                    argument_types.push((t.location(), typ));
                }

                // Look up the constructor
                let TypeConstructor {
                    parameters,
                    tipo: return_type,
                    ..
                } = environment
                    .get_type_constructor(module, name, *location)?
                    .clone();

                // Register the type constructor as being used if it is unqualified.
                // We do not track use of qualified type constructors as they may be
                // used in another module.
                if module.is_none() {
                    environment.increment_usage(name);
                }

                // Ensure that the correct number of arguments have been given to the constructor.
                //
                // NOTE:
                // We do consider a special case for 'Data', where we allow them to optionally
                // carry a phantom type. That type has no effect whatsoever on the semantic (since
                // anything can be cast to `Data` anyway) but it does provide some nice context for
                // blueprint schema generation.
                if args.len() != parameters.len() && !return_type.is_data()
                    || args.len() > 1 && return_type.is_data()
                {
                    return Err(Error::IncorrectTypeArity {
                        location: *location,
                        name: name.to_string(),
                        expected: parameters.len(),
                        given: args.len(),
                    });
                }

                // Instantiate the constructor type for this specific usage
                let mut type_vars = HashMap::new();
                #[allow(clippy::needless_collect)] // Not needless, used for size effects
                let parameter_types: Vec<_> = parameters
                    .into_iter()
                    .map(|typ| environment.instantiate(typ, &mut type_vars, self))
                    .collect();

                let return_type = environment.instantiate(return_type, &mut type_vars, self);

                // Unify argument types with instantiated parameter types so that the correct types
                // are inserted into the return type
                for (parameter, (location, argument)) in
                    parameter_types.into_iter().zip(argument_types)
                {
                    environment.unify(parameter, argument, location, false)?;
                }

                Ok(return_type)
            }

            Annotation::Fn { arguments, ret, .. } => {
                let mut args = Vec::with_capacity(arguments.len());

                for arg in arguments {
                    let arg = self.do_type_from_annotation(arg, environment, unbounds)?;

                    args.push(arg);
                }

                let ret = self.do_type_from_annotation(ret, environment, unbounds)?;

                Ok(Type::function(args, ret))
            }

            Annotation::Var { name, location, .. } => match self.created_type_variables.get(name) {
                Some(var) => Ok(var.clone()),

                None if self.permit_new_type_variables => {
                    let var = environment.new_generic_var();

                    self.rigid_type_names
                        .insert(environment.previous_uid(), name.clone());

                    self.created_type_variables
                        .insert(name.clone(), var.clone());

                    Ok(var)
                }

                None => Err(Error::UnknownType {
                    name: name.to_string(),
                    location: *location,
                    types: environment
                        .module_types
                        .keys()
                        .map(|t| t.to_string())
                        .collect(),
                }),
            },

            Annotation::Hole { location, .. } => {
                unbounds.push(location);
                Ok(environment.new_unbound_var())
            }

            Annotation::Tuple { elems, .. } => {
                let mut typed_elems = vec![];

                for elem in elems {
                    let typed_elem = self.do_type_from_annotation(elem, environment, unbounds)?;

                    typed_elems.push(typed_elem)
                }

                Ok(Type::tuple(typed_elems))
            }
            Annotation::Pair { fst, snd, .. } => {
                let fst = self.do_type_from_annotation(fst, environment, unbounds)?;
                let snd = self.do_type_from_annotation(snd, environment, unbounds)?;

                Ok(Type::pair(fst, snd))
            }
        }?;

        Ok(environment.annotate(return_type, annotation))
    }
}
