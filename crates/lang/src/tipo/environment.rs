use std::{
    collections::{HashMap, HashSet},
    ops::Deref,
    sync::Arc,
};

use itertools::Itertools;

use crate::{
    ast::{
        Annotation, ArgName, CallArg, DataType, Definition, Function, ModuleConstant, Pattern,
        RecordConstructor, RecordConstructorArg, Span, TypeAlias, TypedDefinition,
        UnqualifiedImport, UntypedDefinition, Use, PIPE_VARIABLE,
    },
    builtins::{self, function, generic_var, tuple, unbound_var},
    tipo::fields::FieldMap,
    IdGenerator,
};

use super::{
    error::{Error, Warning},
    hydrator::Hydrator,
    AccessorsMap, PatternConstructor, RecordAccessor, Type, TypeConstructor, TypeInfo, TypeVar,
    ValueConstructor, ValueConstructorVariant,
};

#[derive(Debug)]
pub struct ScopeResetData {
    local_values: HashMap<String, ValueConstructor>,
}

const EXCLUDE_DATA_UNIFY: [&str; 5] = [
    builtins::INT,
    builtins::BYTE_ARRAY,
    builtins::STRING,
    builtins::BOOL,
    builtins::LIST,
];

#[derive(Debug)]
pub struct Environment<'a> {
    /// Accessors defined in the current module
    pub accessors: HashMap<String, AccessorsMap>,
    pub current_module: &'a String,
    /// entity_usages is a stack of scopes. When an entity is created it is
    /// added to the top scope. When an entity is used we crawl down the scope
    /// stack for an entity with that name and mark it as used.
    /// NOTE: The bool in the tuple here tracks if the entity has been used
    pub entity_usages: Vec<HashMap<String, (EntityKind, Span, bool)>>,
    pub id_gen: IdGenerator,
    pub importable_modules: &'a HashMap<String, TypeInfo>,

    /// Modules that have been imported by the current module, along with the
    /// location of the import statement where they were imported.
    pub imported_modules: HashMap<String, (Span, &'a TypeInfo)>,
    pub imported_types: HashSet<String>,

    /// Types defined in the current module (or the prelude)
    pub module_types: HashMap<String, TypeConstructor>,

    /// Mapping from types to constructor names in the current module (or the prelude)
    pub module_types_constructors: HashMap<String, Vec<String>>,

    /// Values defined in the current module (or the prelude)
    pub module_values: HashMap<String, ValueConstructor>,

    previous_id: u64,

    /// Values defined in the current function (or the prelude)
    pub scope: HashMap<String, ValueConstructor>,

    /// Functions that have not yet been inferred then generalised.
    /// We use this to determine whether functions that call this one
    /// can safely be generalised.
    pub ungeneralised_functions: HashSet<String>,

    /// Names of types or values that have been imported an unqualified fashion
    /// from other modules. Used to prevent multiple imports using the same name.
    pub unqualified_imported_names: HashMap<String, Span>,

    pub unused_modules: HashMap<String, Span>,

    /// Warnings
    pub warnings: &'a mut Vec<Warning>,
}

impl<'a> Environment<'a> {
    pub fn close_scope(&mut self, data: ScopeResetData) {
        let unused = self
            .entity_usages
            .pop()
            .expect("There was no top entity scope.");

        self.handle_unused(unused);

        self.scope = data.local_values;
    }

    /// Converts entities with a usage count of 0 to warnings
    pub fn convert_unused_to_warnings(&mut self) {
        let unused = self
            .entity_usages
            .pop()
            .expect("Expected a bottom level of entity usages.");

        self.handle_unused(unused);

        for (name, location) in self.unused_modules.clone().into_iter() {
            self.warnings
                .push(Warning::UnusedImportedModule { name, location });
        }
    }

    pub fn match_fun_type(
        &mut self,
        tipo: Arc<Type>,
        arity: usize,
        fn_location: Span,
        call_location: Span,
    ) -> Result<(Vec<Arc<Type>>, Arc<Type>), Error> {
        if let Type::Var { tipo } = tipo.deref() {
            let new_value = match tipo.borrow().deref() {
                TypeVar::Link { tipo, .. } => {
                    return self.match_fun_type(tipo.clone(), arity, fn_location, call_location);
                }

                TypeVar::Unbound { .. } => {
                    let args: Vec<_> = (0..arity).map(|_| self.new_unbound_var()).collect();

                    let ret = self.new_unbound_var();

                    Some((args, ret))
                }

                TypeVar::Generic { .. } => None,
            };

            if let Some((args, ret)) = new_value {
                *tipo.borrow_mut() = TypeVar::Link {
                    tipo: function(args.clone(), ret.clone()),
                };

                return Ok((args, ret));
            }
        }

        if let Type::Fn { args, ret } = tipo.deref() {
            return if args.len() != arity {
                Err(Error::IncorrectArity {
                    expected: args.len(),
                    given: arity,
                    labels: vec![],
                    location: call_location,
                })
            } else {
                Ok((args.clone(), ret.clone()))
            };
        }

        Err(Error::NotFn {
            tipo,
            location: fn_location,
        })
    }

    fn custom_type_accessors<A>(
        &mut self,
        constructors: &[RecordConstructor<A>],
        hydrator: &mut Hydrator,
    ) -> Result<Option<HashMap<String, RecordAccessor>>, Error> {
        let args = get_compatible_record_fields(constructors);

        let mut fields = HashMap::with_capacity(args.len());

        hydrator.disallow_new_type_variables();

        for (index, label, ast) in args {
            let tipo = hydrator.type_from_annotation(ast, self)?;

            fields.insert(
                label.to_string(),
                RecordAccessor {
                    index: index as u64,
                    label: label.to_string(),
                    tipo,
                },
            );
        }
        Ok(Some(fields))
    }

    pub fn generalise_definition(
        &mut self,
        s: TypedDefinition,
        module_name: &String,
    ) -> TypedDefinition {
        match s {
            Definition::Fn(Function {
                doc,
                location,
                name,
                public,
                arguments: args,
                body,
                return_annotation,
                return_type,
                end_position,
            }) => {
                // Lookup the inferred function information
                let function = self
                    .get_variable(&name)
                    .expect("Could not find preregistered type for function");

                let field_map = function.field_map().cloned();

                let tipo = function.tipo.clone();

                // Generalise the function if not already done so
                let tipo = if self.ungeneralised_functions.remove(&name) {
                    generalise(tipo, 0)
                } else {
                    tipo
                };

                // Insert the function into the module's interface
                self.insert_module_value(
                    &name,
                    ValueConstructor {
                        public,
                        tipo,
                        variant: ValueConstructorVariant::ModuleFn {
                            name: name.clone(),
                            field_map,
                            module: module_name.to_owned(),
                            arity: args.len(),
                            location,
                            builtin: None,
                        },
                    },
                );

                Definition::Fn(Function {
                    doc,
                    location,
                    name,
                    public,
                    arguments: args,
                    return_annotation,
                    return_type,
                    body,
                    end_position,
                })
            }

            definition @ (Definition::TypeAlias { .. }
            | Definition::DataType { .. }
            | Definition::Use { .. }
            | Definition::Test { .. }
            | Definition::ModuleConstant { .. }) => definition,
        }
    }

    /// Lookup a type in the current scope.
    pub fn get_type_constructor(
        &mut self,
        module_alias: &Option<String>,
        name: &str,
        location: Span,
    ) -> Result<&TypeConstructor, Error> {
        match module_alias {
            None => self
                .module_types
                .get(name)
                .ok_or_else(|| Error::UnknownType {
                    location,
                    name: name.to_string(),
                    types: self.module_types.keys().map(|t| t.to_string()).collect(),
                }),

            Some(m) => {
                let (_, module) =
                    self.imported_modules
                        .get(m)
                        .ok_or_else(|| Error::UnknownModule {
                            location,
                            name: name.to_string(),
                            imported_modules: self
                                .importable_modules
                                .keys()
                                .map(|t| t.to_string())
                                .collect(),
                        })?;

                self.unused_modules.remove(m);

                module
                    .types
                    .get(name)
                    .ok_or_else(|| Error::UnknownModuleType {
                        location,
                        name: name.to_string(),
                        module_name: module.name.clone(),
                        type_constructors: module.types.keys().map(|t| t.to_string()).collect(),
                    })
            }
        }
    }

    /// Lookup a value constructor in the current scope.
    ///
    pub fn get_value_constructor(
        &mut self,
        module: Option<&String>,
        name: &str,
        location: Span,
    ) -> Result<&ValueConstructor, Error> {
        match module {
            None => self.scope.get(name).ok_or_else(|| Error::UnknownVariable {
                name: name.to_string(),
                variables: self.local_value_names(),
                location,
            }),

            Some(m) => {
                let (_, module) =
                    self.imported_modules
                        .get(m)
                        .ok_or_else(|| Error::UnknownModule {
                            name: name.to_string(),
                            imported_modules: self
                                .importable_modules
                                .keys()
                                .map(|t| t.to_string())
                                .collect(),
                            location,
                        })?;

                self.unused_modules.remove(m);

                module
                    .values
                    .get(name)
                    .ok_or_else(|| Error::UnknownModuleValue {
                        name: name.to_string(),
                        module_name: module.name.clone(),
                        value_constructors: module.values.keys().map(|t| t.to_string()).collect(),
                        location,
                    })
            }
        }
    }

    /// Lookup a variable in the current scope.
    pub fn get_variable(&self, name: &str) -> Option<&ValueConstructor> {
        self.scope.get(name)
    }

    fn handle_unused(&mut self, unused: HashMap<String, (EntityKind, Span, bool)>) {
        for (name, (kind, location, _)) in unused.into_iter().filter(|(_, (_, _, used))| !used) {
            let warning = match kind {
                EntityKind::ImportedType | EntityKind::ImportedTypeAndConstructor => {
                    Warning::UnusedType {
                        name,
                        imported: true,
                        location,
                    }
                }
                EntityKind::ImportedConstructor => Warning::UnusedConstructor {
                    name,
                    imported: true,
                    location,
                },
                EntityKind::PrivateConstant => {
                    Warning::UnusedPrivateModuleConstant { name, location }
                }
                EntityKind::PrivateTypeConstructor(_) => Warning::UnusedConstructor {
                    name,
                    imported: false,
                    location,
                },
                EntityKind::PrivateFunction => Warning::UnusedPrivateFunction { name, location },
                EntityKind::PrivateType => Warning::UnusedType {
                    name,
                    imported: false,
                    location,
                },
                EntityKind::ImportedValue => Warning::UnusedImportedValue { name, location },
                EntityKind::Variable => Warning::UnusedVariable { name, location },
            };

            self.warnings.push(warning);
        }
    }

    pub fn in_new_scope<T>(&mut self, process_scope: impl FnOnce(&mut Self) -> T) -> T {
        // Record initial scope state
        let initial = self.open_new_scope();

        // Process scope
        let result = process_scope(self);

        self.close_scope(initial);

        // Return result of typing the scope
        result
    }

    /// Increments an entity's usage in the current or nearest enclosing scope
    pub fn increment_usage(&mut self, name: &str) {
        let mut name = name.to_string();

        while let Some((kind, _, used)) = self
            .entity_usages
            .iter_mut()
            .rev()
            .find_map(|scope| scope.get_mut(&name))
        {
            *used = true;

            match kind {
                // If a type constructor is used, we consider its type also used
                EntityKind::PrivateTypeConstructor(type_name) if type_name != &name => {
                    name.clone_from(type_name);
                }
                _ => return,
            }
        }
    }

    /// Inserts an entity at the current scope for usage tracking.
    pub fn init_usage(&mut self, name: String, kind: EntityKind, location: Span) {
        use EntityKind::*;

        match self
            .entity_usages
            .last_mut()
            .expect("Attempted to access non-existant entity usages scope")
            .insert(name.to_string(), (kind, location, false))
        {
            // Private types can be shadowed by a constructor with the same name
            //
            // TODO: Improve this so that we can tell if an imported overriden
            // type is actually used or not by tracking whether usages apply to
            // the value or type scope
            Some((ImportedType | ImportedTypeAndConstructor | PrivateType, _, _)) => (),

            Some((kind, location, false)) => {
                // an entity was overwritten in the top most scope without being used
                let mut unused = HashMap::with_capacity(1);
                unused.insert(name, (kind, location, false));
                self.handle_unused(unused);
            }

            _ => (),
        }
    }

    pub fn insert_accessors(&mut self, type_name: &str, accessors: AccessorsMap) {
        self.accessors.insert(type_name.to_string(), accessors);
    }

    /// Insert a value into the current module.
    /// Errors if the module already has a value with that name.
    pub fn insert_module_value(&mut self, name: &str, value: ValueConstructor) {
        self.module_values.insert(name.to_string(), value);
    }

    /// Map a type in the current scope. Errors if the module
    /// already has a type with that name, unless the type is
    /// from the prelude.
    pub fn insert_type_constructor(
        &mut self,
        type_name: String,
        info: TypeConstructor,
    ) -> Result<(), Error> {
        let name = type_name.clone();
        let location = info.location;

        match self.module_types.insert(type_name, info) {
            None => Ok(()),
            Some(prelude_type) if prelude_type.module.is_empty() => Ok(()),
            Some(previous) => Err(Error::DuplicateTypeName {
                name,
                location,
                previous_location: previous.location,
            }),
        }
    }

    /// Map a type to constructors in the current scope.
    pub fn insert_type_to_constructors(&mut self, type_name: String, constructors: Vec<String>) {
        self.module_types_constructors
            .insert(type_name, constructors);
    }

    /// Insert a variable in the current scope.
    pub fn insert_variable(
        &mut self,
        name: String,
        variant: ValueConstructorVariant,
        tipo: Arc<Type>,
    ) {
        self.scope.insert(
            name,
            ValueConstructor {
                public: false,
                variant,
                tipo,
            },
        );
    }

    /// Instantiate converts generic variables into unbound ones.
    pub fn instantiate(
        &mut self,
        t: Arc<Type>,
        ids: &mut HashMap<u64, Arc<Type>>,
        hydrator: &Hydrator,
    ) -> Arc<Type> {
        match t.deref() {
            Type::App {
                public,
                name,
                module,
                args,
            } => {
                let args = args
                    .iter()
                    .map(|t| self.instantiate(t.clone(), ids, hydrator))
                    .collect();
                Arc::new(Type::App {
                    public: *public,
                    name: name.clone(),
                    module: module.clone(),
                    args,
                })
            }

            Type::Var { tipo } => {
                match tipo.borrow().deref() {
                    TypeVar::Link { tipo } => return self.instantiate(tipo.clone(), ids, hydrator),

                    TypeVar::Unbound { .. } => return Arc::new(Type::Var { tipo: tipo.clone() }),

                    TypeVar::Generic { id } => match ids.get(id) {
                        Some(t) => return t.clone(),
                        None => {
                            if !hydrator.is_rigid(id) {
                                // Check this in the hydrator, i.e. is it a created type
                                let v = self.new_unbound_var();
                                ids.insert(*id, v.clone());
                                return v;
                            } else {
                                // tracing::trace!(id = id, "not_instantiating_rigid_type_var")
                            }
                        }
                    },
                }
                Arc::new(Type::Var { tipo: tipo.clone() })
            }

            Type::Fn { args, ret, .. } => function(
                args.iter()
                    .map(|t| self.instantiate(t.clone(), ids, hydrator))
                    .collect(),
                self.instantiate(ret.clone(), ids, hydrator),
            ),

            Type::Tuple { elems } => tuple(
                elems
                    .iter()
                    .map(|t| self.instantiate(t.clone(), ids, hydrator))
                    .collect(),
            ),
        }
    }

    pub fn local_value_names(&self) -> Vec<String> {
        self.scope
            .keys()
            .filter(|&t| PIPE_VARIABLE != t)
            .map(|t| t.to_string())
            .collect()
    }

    fn make_type_vars(
        &mut self,
        args: &[String],
        location: &Span,
        hydrator: &mut Hydrator,
    ) -> Result<Vec<Arc<Type>>, Error> {
        let mut type_vars = Vec::new();

        for arg in args {
            let annotation = Annotation::Var {
                location: *location,
                name: arg.to_string(),
            };

            let tipo = hydrator.type_from_annotation(&annotation, self)?;

            type_vars.push(tipo);
        }

        Ok(type_vars)
    }

    pub fn new(
        id_gen: IdGenerator,
        current_module: &'a String,
        importable_modules: &'a HashMap<String, TypeInfo>,
        warnings: &'a mut Vec<Warning>,
    ) -> Self {
        let prelude = importable_modules
            .get("aiken")
            .expect("Unable to find prelude in importable modules");

        Self {
            previous_id: id_gen.next(),
            id_gen,
            ungeneralised_functions: HashSet::new(),
            module_types: prelude.types.clone(),
            module_types_constructors: prelude.types_constructors.clone(),
            module_values: HashMap::new(),
            imported_modules: HashMap::new(),
            unused_modules: HashMap::new(),
            unqualified_imported_names: HashMap::new(),
            accessors: prelude.accessors.clone(),
            scope: prelude.values.clone(),
            importable_modules,
            imported_types: HashSet::new(),
            current_module,
            warnings,
            entity_usages: vec![HashMap::new()],
        }
    }

    /// Create a new generic type that can stand in for any type.
    pub fn new_generic_var(&mut self) -> Arc<Type> {
        generic_var(self.next_uid())
    }

    /// Create a new unbound type that is a specific type, we just don't
    /// know which one yet.
    pub fn new_unbound_var(&mut self) -> Arc<Type> {
        unbound_var(self.next_uid())
    }

    pub fn next_uid(&mut self) -> u64 {
        let id = self.id_gen.next();
        self.previous_id = id;
        id
    }

    pub fn open_new_scope(&mut self) -> ScopeResetData {
        let local_values = self.scope.clone();

        self.entity_usages.push(HashMap::new());

        ScopeResetData { local_values }
    }

    pub fn previous_uid(&self) -> u64 {
        self.previous_id
    }

    pub fn register_import(&mut self, def: &UntypedDefinition) -> Result<(), Error> {
        match def {
            Definition::Use(Use {
                module,
                as_name,
                unqualified,
                location,
                ..
            }) => {
                let name = module.join("/");

                // Find imported module
                let module_info =
                    self.importable_modules
                        .get(&name)
                        .ok_or_else(|| Error::UnknownModule {
                            location: *location,
                            name,
                            imported_modules: self.imported_modules.keys().cloned().collect(),
                        })?;

                // Determine local alias of imported module
                let module_name = as_name
                    .as_ref()
                    .or_else(|| module.last())
                    .expect("Typer could not identify module name.")
                    .clone();

                // Insert unqualified imports into scope
                for UnqualifiedImport {
                    name,
                    location,
                    as_name,
                    ..
                } in unqualified
                {
                    let mut type_imported = false;
                    let mut value_imported = false;
                    let mut variant = None;

                    let imported_name = as_name.as_ref().unwrap_or(name);

                    // Check if value already was imported
                    if let Some(previous) = self.unqualified_imported_names.get(imported_name) {
                        return Err(Error::DuplicateImport {
                            location: *location,
                            previous_location: *previous,
                            name: name.to_string(),
                        });
                    }

                    // Register the name as imported so it can't be imported a
                    // second time in future
                    self.unqualified_imported_names
                        .insert(imported_name.clone(), *location);

                    // Register the unqualified import if it is a value
                    if let Some(value) = module_info.values.get(name) {
                        self.insert_variable(
                            imported_name.clone(),
                            value.variant.clone(),
                            value.tipo.clone(),
                        );
                        variant = Some(&value.variant);
                        value_imported = true;
                    }

                    // Register the unqualified import if it is a type constructor
                    if let Some(typ) = module_info.types.get(name) {
                        let typ_info = TypeConstructor {
                            location: *location,
                            ..typ.clone()
                        };

                        self.insert_type_constructor(imported_name.clone(), typ_info)?;

                        type_imported = true;
                    }

                    if value_imported && type_imported {
                        self.init_usage(
                            imported_name.to_string(),
                            EntityKind::ImportedTypeAndConstructor,
                            *location,
                        );
                    } else if type_imported {
                        self.imported_types.insert(imported_name.to_string());

                        self.init_usage(
                            imported_name.to_string(),
                            EntityKind::ImportedType,
                            *location,
                        );
                    } else if value_imported {
                        match variant {
                            Some(&ValueConstructorVariant::Record { .. }) => self.init_usage(
                                imported_name.to_string(),
                                EntityKind::ImportedConstructor,
                                *location,
                            ),
                            _ => self.init_usage(
                                imported_name.to_string(),
                                EntityKind::ImportedValue,
                                *location,
                            ),
                        };
                    } else if !value_imported {
                        // Error if no type or value was found with that name
                        return Err(Error::UnknownModuleField {
                            location: *location,
                            name: name.clone(),
                            module_name: module.join("/"),
                            value_constructors: module_info
                                .values
                                .keys()
                                .map(|t| t.to_string())
                                .collect(),
                            type_constructors: module_info
                                .types
                                .keys()
                                .map(|t| t.to_string())
                                .collect(),
                        });
                    }
                }

                if unqualified.is_empty() {
                    // When the module has no unqualified imports, we track its usage
                    // so we can warn if not used by the end of the type checking
                    self.unused_modules.insert(module_name.clone(), *location);
                }

                // Check if a module was already imported with this name
                if let Some((previous_location, _)) = self.imported_modules.get(&module_name) {
                    return Err(Error::DuplicateImport {
                        location: *location,
                        previous_location: *previous_location,
                        name: module_name,
                    });
                }

                // Register the name as imported so it can't be imported a
                // second time in future
                self.unqualified_imported_names
                    .insert(module_name.clone(), *location);

                // Insert imported module into scope
                self.imported_modules
                    .insert(module_name, (*location, module_info));

                Ok(())
            }

            _ => Ok(()),
        }
    }

    /// Iterate over a module, registering any new types created by the module into the typer
    pub fn register_types(
        &mut self,
        def: &'a UntypedDefinition,
        module: &String,
        hydrators: &mut HashMap<String, Hydrator>,
        names: &mut HashMap<&'a str, &'a Span>,
    ) -> Result<(), Error> {
        match def {
            Definition::DataType(DataType {
                name,
                public,
                parameters,
                location,
                constructors,
                ..
            }) => {
                assert_unique_type_name(names, name, location)?;

                // Build a type from the type Annotation
                let mut hydrator = Hydrator::new();

                let parameters = self.make_type_vars(parameters, location, &mut hydrator)?;

                let tipo = Arc::new(Type::App {
                    public: *public,
                    module: module.to_owned(),
                    name: name.clone(),
                    args: parameters.clone(),
                });

                hydrators.insert(name.to_string(), hydrator);

                self.insert_type_constructor(
                    name.clone(),
                    TypeConstructor {
                        location: *location,
                        module: module.to_owned(),
                        public: *public,
                        parameters,
                        tipo,
                    },
                )?;

                let constructor_names = constructors.iter().map(|c| c.name.clone()).collect();

                self.insert_type_to_constructors(name.clone(), constructor_names);

                // Keep track of private types so we can tell if they are later unused
                if !public {
                    self.init_usage(name.clone(), EntityKind::PrivateType, *location);
                }
            }

            Definition::TypeAlias(TypeAlias {
                location,
                public,
                parameters: args,
                alias: name,
                annotation: resolved_type,
                ..
            }) => {
                assert_unique_type_name(names, name, location)?;

                // Register the paramerterised types
                let mut hydrator = Hydrator::new();
                let parameters = self.make_type_vars(args, location, &mut hydrator)?;

                // Disallow creation of new types outside the paramerterised types
                hydrator.disallow_new_type_variables();

                // Create the type that the alias resolves to
                let tipo = hydrator.type_from_annotation(resolved_type, self)?;

                self.insert_type_constructor(
                    name.clone(),
                    TypeConstructor {
                        location: *location,
                        module: module.to_owned(),
                        public: *public,
                        parameters,
                        tipo,
                    },
                )?;

                // Keep track of private types so we can tell if they are later unused
                if !public {
                    self.init_usage(name.clone(), EntityKind::PrivateType, *location);
                }
            }

            Definition::Fn { .. }
            | Definition::Test { .. }
            | Definition::Use { .. }
            | Definition::ModuleConstant { .. } => {}
        }

        Ok(())
    }

    pub fn register_values(
        &mut self,
        def: &'a UntypedDefinition,
        module_name: &String,
        hydrators: &mut HashMap<String, Hydrator>,
        names: &mut HashMap<&'a str, &'a Span>,
    ) -> Result<(), Error> {
        match def {
            Definition::Fn(Function {
                name,
                arguments: args,
                location,
                return_annotation,
                public,
                ..
            }) => {
                assert_unique_value_name(names, name, location)?;

                self.ungeneralised_functions.insert(name.to_string());

                // Create the field map so we can reorder labels for usage of this function
                let mut field_map = FieldMap::new(args.len());

                for (i, arg) in args.iter().enumerate() {
                    if let ArgName::NamedLabeled { label, .. }
                    | ArgName::LabeledDiscard { label, .. } = &arg.arg_name
                    {
                        field_map.insert(label.clone(), i, location)?;
                    }
                }
                let field_map = field_map.into_option();

                // Construct type from annotations
                let mut hydrator = Hydrator::new();

                hydrator.permit_holes(true);

                let mut arg_types = Vec::new();

                for arg in args {
                    let tipo = hydrator.type_from_option_annotation(&arg.annotation, self)?;

                    arg_types.push(tipo);
                }

                let return_type = hydrator.type_from_option_annotation(return_annotation, self)?;

                let tipo = function(arg_types, return_type);

                // Keep track of which types we create from annotations so we can know
                // which generic types not to instantiate later when performing
                // inference of the function body.
                hydrators.insert(name.clone(), hydrator);

                // Insert the function into the environment
                self.insert_variable(
                    name.clone(),
                    ValueConstructorVariant::ModuleFn {
                        name: name.clone(),
                        field_map,
                        module: module_name.to_owned(),
                        arity: args.len(),
                        location: *location,
                        builtin: None,
                    },
                    tipo,
                );

                if !public {
                    self.init_usage(name.clone(), EntityKind::PrivateFunction, *location);
                }
            }

            Definition::Test(Function { name, location, .. }) => {
                hydrators.insert(name.clone(), Hydrator::new());
                let arg_types = vec![];
                let return_type = builtins::bool();
                self.insert_variable(
                    name.clone(),
                    ValueConstructorVariant::ModuleFn {
                        name: name.clone(),
                        field_map: None,
                        module: module_name.to_owned(),
                        arity: 0,
                        location: *location,
                        builtin: None,
                    },
                    function(arg_types, return_type),
                );
            }

            Definition::DataType(DataType {
                location,
                public,
                opaque,
                name,
                constructors,
                ..
            }) => {
                let mut hydrator = hydrators
                    .remove(name)
                    .expect("Could not find hydrator for register_values custom type");

                hydrator.disallow_new_type_variables();

                let typ = self
                    .module_types
                    .get(name)
                    .expect("Type for custom type not found in register_values")
                    .tipo
                    .clone();

                // If the custom type only has a single constructor then we can access the
                // fields using the record.field syntax, so store any fields accessors.
                if let Some(accessors) = self.custom_type_accessors(constructors, &mut hydrator)? {
                    let map = AccessorsMap {
                        public: (*public && !*opaque),
                        accessors,
                        // TODO: improve the ownership here so that we can use the
                        // `return_type_constructor` below rather than looking it up twice.
                        tipo: typ.clone(),
                    };

                    self.insert_accessors(name, map)
                }

                // Check and register constructors
                for constructor in constructors {
                    assert_unique_value_name(names, &constructor.name, &constructor.location)?;

                    let mut field_map = FieldMap::new(constructor.arguments.len());

                    let mut args_types = Vec::with_capacity(constructor.arguments.len());

                    for (
                        i,
                        RecordConstructorArg {
                            label, annotation, ..
                        },
                    ) in constructor.arguments.iter().enumerate()
                    {
                        let t = hydrator.type_from_annotation(annotation, self)?;

                        args_types.push(t);

                        if let Some(label) = label {
                            field_map.insert(label.clone(), i, location)?;
                        }
                    }

                    let field_map = field_map.into_option();

                    // Insert constructor function into module scope
                    let typ = match constructor.arguments.len() {
                        0 => typ.clone(),
                        _ => function(args_types, typ.clone()),
                    };

                    let constructor_info = ValueConstructorVariant::Record {
                        constructors_count: constructors.len() as u16,
                        name: constructor.name.clone(),
                        arity: constructor.arguments.len(),
                        field_map: field_map.clone(),
                        location: constructor.location,
                        module: module_name.to_owned(),
                    };

                    if !opaque {
                        self.insert_module_value(
                            &constructor.name,
                            ValueConstructor {
                                public: *public,
                                tipo: typ.clone(),
                                variant: constructor_info.clone(),
                            },
                        );
                    }

                    if !public {
                        self.init_usage(
                            constructor.name.clone(),
                            EntityKind::PrivateTypeConstructor(name.clone()),
                            constructor.location,
                        );
                    }

                    self.insert_variable(constructor.name.clone(), constructor_info, typ);
                }
            }

            Definition::ModuleConstant(ModuleConstant { name, location, .. }) => {
                assert_unique_const_name(names, name, location)?;
            }

            Definition::Use { .. } | Definition::TypeAlias { .. } => {}
        }
        Ok(())
    }

    /// Unify two types that should be the same.
    /// Any unbound type variables will be linked to the other type as they are the same.
    ///
    /// It two types are found to not be the same an error is returned.
    #[allow(clippy::only_used_in_recursion)]
    pub fn unify(&mut self, t1: Arc<Type>, t2: Arc<Type>, location: Span) -> Result<(), Error> {
        if t1 == t2 {
            return Ok(());
        }

        if let (Type::App { name: name1, .. }, Type::App { name: name2, .. }) =
            (t1.deref(), t2.deref())
        {
            if name1 == "Data" && !EXCLUDE_DATA_UNIFY.contains(&name2.as_str()) {
                return Ok(());
            }

            if name2 == "Data" && !EXCLUDE_DATA_UNIFY.contains(&name1.as_str()) {
                return Ok(());
            }
        }

        // Collapse right hand side type links. Left hand side will be collapsed in the next block.
        if let Type::Var { tipo } = t2.deref() {
            if let TypeVar::Link { tipo } = tipo.borrow().deref() {
                return self.unify(t1, tipo.clone(), location);
            }
        }

        if let Type::Var { tipo } = t1.deref() {
            enum Action {
                Unify(Arc<Type>),
                CouldNotUnify,
                Link,
            }

            let action = match tipo.borrow().deref() {
                TypeVar::Link { tipo } => Action::Unify(tipo.clone()),

                TypeVar::Unbound { id } => {
                    unify_unbound_type(t2.clone(), *id, location)?;
                    Action::Link
                }

                TypeVar::Generic { id } => {
                    if let Type::Var { tipo } = t2.deref() {
                        if tipo.borrow().is_unbound() {
                            *tipo.borrow_mut() = TypeVar::Generic { id: *id };
                            return Ok(());
                        }
                    }
                    Action::CouldNotUnify
                }
            };

            return match action {
                Action::Link => {
                    *tipo.borrow_mut() = TypeVar::Link { tipo: t2 };
                    Ok(())
                }

                Action::Unify(t) => self.unify(t, t2, location),

                Action::CouldNotUnify => Err(Error::CouldNotUnify {
                    location,
                    expected: t1.clone(),
                    given: t2,
                    situation: None,
                    rigid_type_names: HashMap::new(),
                }),
            };
        }

        if let Type::Var { .. } = t2.deref() {
            return self.unify(t2, t1, location).map_err(|e| e.flip_unify());
        }

        match (t1.deref(), t2.deref()) {
            (
                Type::App {
                    module: m1,
                    name: n1,
                    args: args1,
                    ..
                },
                Type::App {
                    module: m2,
                    name: n2,
                    args: args2,
                    ..
                },
            ) if m1 == m2 && n1 == n2 && args1.len() == args2.len() => {
                for (a, b) in args1.iter().zip(args2) {
                    unify_enclosed_type(
                        t1.clone(),
                        t2.clone(),
                        self.unify(a.clone(), b.clone(), location),
                    )?;
                }
                Ok(())
            }

            (Type::Tuple { elems: elems1, .. }, Type::Tuple { elems: elems2, .. })
                if elems1.len() == elems2.len() =>
            {
                for (a, b) in elems1.iter().zip(elems2) {
                    unify_enclosed_type(
                        t1.clone(),
                        t2.clone(),
                        self.unify(a.clone(), b.clone(), location),
                    )?;
                }
                Ok(())
            }

            (
                Type::Fn {
                    args: args1,
                    ret: retrn1,
                    ..
                },
                Type::Fn {
                    args: args2,
                    ret: retrn2,
                    ..
                },
            ) if args1.len() == args2.len() => {
                for (a, b) in args1.iter().zip(args2) {
                    self.unify(a.clone(), b.clone(), location).map_err(|_| {
                        Error::CouldNotUnify {
                            location,
                            expected: t1.clone(),
                            given: t2.clone(),
                            situation: None,
                            rigid_type_names: HashMap::new(),
                        }
                    })?;
                }
                self.unify(retrn1.clone(), retrn2.clone(), location)
                    .map_err(|_| Error::CouldNotUnify {
                        location,
                        expected: t1.clone(),
                        given: t2.clone(),
                        situation: None,
                        rigid_type_names: HashMap::new(),
                    })
            }

            _ => Err(Error::CouldNotUnify {
                location,
                expected: t1.clone(),
                given: t2.clone(),
                situation: None,
                rigid_type_names: HashMap::new(),
            }),
        }
    }

    /// Checks that the given patterns are exhaustive for given type.
    /// Currently only performs exhaustiveness checking for custom types,
    /// only at the top level (without recursing into constructor arguments).
    pub fn check_exhaustiveness(
        &mut self,
        patterns: Vec<Pattern<PatternConstructor, Arc<Type>>>,
        value_typ: Arc<Type>,
        location: Span,
    ) -> Result<(), Vec<String>> {
        match &*value_typ {
            Type::App {
                name: type_name,
                module,
                ..
            } => {
                let m = if module.is_empty() || module == self.current_module {
                    None
                } else {
                    Some(module.clone())
                };

                if let Ok(constructors) = self.get_constructors_for_type(&m, type_name, location) {
                    let mut unmatched_constructors: HashSet<String> =
                        constructors.iter().cloned().collect();

                    for p in &patterns {
                        // ignore Assign patterns
                        let mut pattern = p;
                        while let Pattern::Assign {
                            pattern: assign_pattern,
                            ..
                        } = pattern
                        {
                            pattern = assign_pattern;
                        }

                        match pattern {
                            // If the pattern is a Discard or Var, all constructors are covered by it
                            Pattern::Discard { .. } => return Ok(()),
                            Pattern::Var { .. } => return Ok(()),
                            // If the pattern is a constructor, remove it from unmatched patterns
                            Pattern::Constructor {
                                constructor: PatternConstructor::Record { name, .. },
                                ..
                            } => {
                                unmatched_constructors.remove(name);
                            }
                            _ => return Ok(()),
                        }
                    }

                    if !unmatched_constructors.is_empty() {
                        return Err(unmatched_constructors.into_iter().sorted().collect());
                    }
                }
                Ok(())
            }
            _ => Ok(()),
        }
    }

    /// Lookup constructors for type in the current scope.
    ///
    pub fn get_constructors_for_type(
        &mut self,
        full_module_name: &Option<String>,
        name: &str,
        location: Span,
    ) -> Result<&Vec<String>, Error> {
        match full_module_name {
            None => self
                .module_types_constructors
                .get(name)
                .ok_or_else(|| Error::UnknownType {
                    name: name.to_string(),
                    types: self.module_types.keys().map(|t| t.to_string()).collect(),
                    location,
                }),

            Some(m) => {
                let module =
                    self.importable_modules
                        .get(m)
                        .ok_or_else(|| Error::UnknownModule {
                            location,
                            name: name.to_string(),
                            imported_modules: self
                                .importable_modules
                                .keys()
                                .map(|t| t.to_string())
                                .collect(),
                        })?;

                self.unused_modules.remove(m);

                module
                    .types_constructors
                    .get(name)
                    .ok_or_else(|| Error::UnknownModuleType {
                        location,
                        name: name.to_string(),
                        module_name: module.name.clone(),
                        type_constructors: module.types.keys().map(|t| t.to_string()).collect(),
                    })
            }
        }
    }
}

/// For Keeping track of entity usages and knowing which error to display.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EntityKind {
    PrivateConstant,
    // String here is the type constructor's type name
    PrivateTypeConstructor(String),
    PrivateFunction,
    ImportedConstructor,
    ImportedType,
    ImportedTypeAndConstructor,
    ImportedValue,
    PrivateType,
    Variable,
}

/// This function makes sure that the type variable being unified
/// doesn't occur within the type it is being unified with. This
/// prevents the algorithm from inferring recursive types, which
/// could cause naively-implemented type checking to diverge.
/// While traversing the type tree.
fn unify_unbound_type(tipo: Arc<Type>, own_id: u64, location: Span) -> Result<(), Error> {
    if let Type::Var { tipo } = tipo.deref() {
        let new_value = match tipo.borrow().deref() {
            TypeVar::Link { tipo, .. } => {
                return unify_unbound_type(tipo.clone(), own_id, location)
            }

            TypeVar::Unbound { id } => {
                if id == &own_id {
                    return Err(Error::RecursiveType { location });
                } else {
                    Some(TypeVar::Unbound { id: *id })
                }
            }

            TypeVar::Generic { .. } => return Ok(()),
        };

        if let Some(t) = new_value {
            *tipo.borrow_mut() = t;
        }
        return Ok(());
    }

    match tipo.deref() {
        Type::App { args, .. } => {
            for arg in args {
                unify_unbound_type(arg.clone(), own_id, location)?
            }

            Ok(())
        }

        Type::Fn { args, ret } => {
            for arg in args {
                unify_unbound_type(arg.clone(), own_id, location)?;
            }

            unify_unbound_type(ret.clone(), own_id, location)
        }

        Type::Tuple { elems, .. } => {
            for elem in elems {
                unify_unbound_type(elem.clone(), own_id, location)?
            }

            Ok(())
        }

        Type::Var { .. } => unreachable!(),
    }
}

fn unify_enclosed_type(
    e1: Arc<Type>,
    e2: Arc<Type>,
    result: Result<(), Error>,
) -> Result<(), Error> {
    // If types cannot unify, show the type error with the enclosing types, e1 and e2.
    match result {
        Err(Error::CouldNotUnify {
            situation,
            location,
            rigid_type_names,
            ..
        }) => Err(Error::CouldNotUnify {
            expected: e1,
            given: e2,
            situation,
            location,
            rigid_type_names,
        }),

        _ => result,
    }
}

fn assert_unique_type_name<'a>(
    names: &mut HashMap<&'a str, &'a Span>,
    name: &'a str,
    location: &'a Span,
) -> Result<(), Error> {
    match names.insert(name, location) {
        Some(previous_location) => Err(Error::DuplicateTypeName {
            name: name.to_string(),
            previous_location: *previous_location,
            location: *location,
        }),
        None => Ok(()),
    }
}

fn assert_unique_value_name<'a>(
    names: &mut HashMap<&'a str, &'a Span>,
    name: &'a str,
    location: &'a Span,
) -> Result<(), Error> {
    match names.insert(name, location) {
        Some(previous_location) => Err(Error::DuplicateName {
            name: name.to_string(),
            previous_location: *previous_location,
            location: *location,
        }),
        None => Ok(()),
    }
}

fn assert_unique_const_name<'a>(
    names: &mut HashMap<&'a str, &'a Span>,
    name: &'a str,
    location: &'a Span,
) -> Result<(), Error> {
    match names.insert(name, location) {
        Some(previous_location) => Err(Error::DuplicateConstName {
            name: name.to_string(),
            previous_location: *previous_location,
            location: *location,
        }),
        None => Ok(()),
    }
}

pub(super) fn assert_no_labeled_arguments<A>(args: &[CallArg<A>]) -> Result<(), Error> {
    for arg in args {
        if let Some(label) = &arg.label {
            return Err(Error::UnexpectedLabeledArg {
                location: arg.location,
                label: label.to_string(),
            });
        }
    }
    Ok(())
}

pub(super) fn collapse_links(t: Arc<Type>) -> Arc<Type> {
    if let Type::Var { tipo } = t.deref() {
        if let TypeVar::Link { tipo } = tipo.borrow().deref() {
            return tipo.clone();
        }
    }
    t
}

/// Returns the fields that have the same label and type across all variants of
/// the given type.
fn get_compatible_record_fields<A>(
    constructors: &[RecordConstructor<A>],
) -> Vec<(usize, &str, &Annotation)> {
    let mut compatible = vec![];

    let first = match constructors.get(0) {
        Some(first) => first,
        None => return compatible,
    };

    'next_argument: for (index, first_argument) in first.arguments.iter().enumerate() {
        // Fields without labels do not have accessors
        let label = match first_argument.label.as_ref() {
            Some(label) => label.as_str(),
            None => continue 'next_argument,
        };

        // Check each variant to see if they have an field in the same position
        // with the same label and the same type
        for constructor in constructors.iter().skip(1) {
            // The field must exist in all variants
            let argument = match constructor.arguments.get(index) {
                Some(argument) => argument,
                None => continue 'next_argument,
            };

            // The labels must be the same
            if argument.label != first_argument.label {
                continue 'next_argument;
            }

            // The types must be the same
            if !argument
                .annotation
                .is_logically_equal(&first_argument.annotation)
            {
                continue 'next_argument;
            }
        }

        // The previous loop did not find any incompatible fields in the other
        // variants so this field is compatible across variants and we should
        // generate an accessor for it.
        compatible.push((index, label, &first_argument.annotation))
    }

    compatible
}

/// Takes a level and a type and turns all type variables within the type that have
/// level higher than the input level into generalized (polymorphic) type variables.
#[allow(clippy::only_used_in_recursion)]
pub(crate) fn generalise(t: Arc<Type>, ctx_level: usize) -> Arc<Type> {
    match t.deref() {
        Type::Var { tipo } => match tipo.borrow().deref() {
            TypeVar::Unbound { id } => generic_var(*id),
            TypeVar::Link { tipo } => generalise(tipo.clone(), ctx_level),
            TypeVar::Generic { .. } => Arc::new(Type::Var { tipo: tipo.clone() }),
        },

        Type::App {
            public,
            module,
            name,
            args,
        } => {
            let args = args
                .iter()
                .map(|t| generalise(t.clone(), ctx_level))
                .collect();

            Arc::new(Type::App {
                public: *public,
                module: module.clone(),
                name: name.clone(),
                args,
            })
        }

        Type::Fn { args, ret } => function(
            args.iter()
                .map(|t| generalise(t.clone(), ctx_level))
                .collect(),
            generalise(ret.clone(), ctx_level),
        ),

        Type::Tuple { elems } => tuple(
            elems
                .iter()
                .map(|t| generalise(t.clone(), ctx_level))
                .collect(),
        ),
    }
}
