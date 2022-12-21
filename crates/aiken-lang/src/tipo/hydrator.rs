use std::{collections::HashMap, sync::Arc};

use crate::{
    ast::Annotation,
    builtins::{function, tuple},
};

use super::{environment::Environment, error::Error, Type, TypeConstructor};

/// The Hydrator takes an AST representing a type (i.e. a type annotation
/// for a function argument) and returns a Type for that annotation.
///
/// If a valid Type cannot be constructed it returns an error.
///
/// It keeps track of any type variables created. This is useful for:
///
/// - Determining if a generic type variable should be made into an
///   unbound type varable during type instantiation.
/// - Ensuring that the same type is constructed if the programmer
///   uses the same name for a type variable multiple times.
///
#[derive(Debug)]
pub struct Hydrator {
    created_type_variables: HashMap<String, Arc<Type>>,
    /// A rigid type is a generic type that was specified as being generic in
    /// an annotation. As such it should never be instantiated into an unbound
    /// variable. This type_id => name map is used for reporting the original
    /// annotated name on error.
    rigid_type_names: HashMap<u64, String>,
    permit_new_type_variables: bool,
    permit_holes: bool,
}

#[derive(Debug)]
pub struct ScopeResetData {
    created_type_variables: HashMap<String, Arc<Type>>,
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
            permit_holes: false,
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

    pub fn permit_holes(&mut self, flag: bool) {
        self.permit_holes = flag
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

    pub fn type_from_option_annotation<'a>(
        &mut self,
        ast: &Option<Annotation>,
        environment: &mut Environment<'a>,
    ) -> Result<Arc<Type>, Error> {
        match ast {
            Some(ast) => self.type_from_annotation(ast, environment),
            None => Ok(environment.new_unbound_var()),
        }
    }

    /// Construct a Type from an AST Type annotation.
    ///
    pub fn type_from_annotation<'a>(
        &mut self,
        annotation: &Annotation,
        environment: &mut Environment<'a>,
    ) -> Result<Arc<Type>, Error> {
        match annotation {
            Annotation::Constructor {
                location,
                module,
                name,
                arguments: args,
            } => {
                // Hydrate the type argument AST into types
                let mut argument_types = Vec::with_capacity(args.len());
                for t in args {
                    let typ = self.type_from_annotation(t, environment)?;
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

                // Register the type constructor as being used if it is unqualifed.
                // We do not track use of qualified type constructors as they may be
                // used in another module.
                if module.is_none() {
                    environment.increment_usage(name);
                }

                // Ensure that the correct number of arguments have been given to the constructor
                if args.len() != parameters.len() {
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
                    environment.unify(parameter, argument, location)?;
                }

                Ok(return_type)
            }

            Annotation::Fn { arguments, ret, .. } => {
                let mut args = Vec::with_capacity(arguments.len());

                for arg in arguments {
                    let arg = self.type_from_annotation(arg, environment)?;

                    args.push(arg);
                }

                let ret = self.type_from_annotation(ret, environment)?;

                Ok(function(args, ret))
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

            Annotation::Hole { .. } if self.permit_holes => Ok(environment.new_unbound_var()),

            Annotation::Hole { location, .. } => Err(Error::UnexpectedTypeHole {
                location: *location,
            }),

            Annotation::Tuple { elems, .. } => {
                let mut typed_elems = vec![];

                for elem in elems {
                    let typed_elem = self.type_from_annotation(elem, environment)?;

                    typed_elems.push(typed_elem)
                }

                Ok(tuple(typed_elems))
            }
        }
    }
}
