use super::{
    error::{Error, Warning},
    exhaustive::{simplify, Matrix, PatternStack},
    hydrator::Hydrator,
    AccessorsMap, RecordAccessor, Type, TypeConstructor, TypeInfo, TypeVar, ValueConstructor,
    ValueConstructorVariant,
};
use crate::{
    ast::{
        self, Annotation, CallArg, DataType, Definition, Function, ModuleConstant, ModuleKind,
        RecordConstructor, RecordConstructorArg, Span, TypeAlias, TypedDefinition, TypedFunction,
        TypedPattern, TypedValidator, UnqualifiedImport, UntypedArg, UntypedDefinition,
        UntypedFunction, Use, Validator, PIPE_VARIABLE,
    },
    tipo::{fields::FieldMap, TypeAliasAnnotation},
    IdGenerator,
};
use std::{
    collections::{HashMap, HashSet},
    ops::Deref,
    rc::Rc,
};

#[derive(Debug)]
pub struct ScopeResetData {
    local_values: HashMap<String, ValueConstructor>,
}

#[derive(Debug)]
pub struct Environment<'a> {
    /// Accessors defined in the current module
    pub accessors: HashMap<String, AccessorsMap>,
    pub current_module: &'a String,
    pub current_kind: &'a ModuleKind,
    /// entity_usages is a stack of scopes. When an entity is created it is
    /// added to the top scope. When an entity is used we crawl down the scope
    /// stack for an entity with that name and mark it as used.
    /// NOTE: The bool in the tuple here tracks if the entity has been used
    pub entity_usages: Vec<HashMap<String, (EntityKind, Span, bool)>>,
    pub id_gen: IdGenerator,
    pub importable_modules: &'a HashMap<String, TypeInfo>,
    pub validator_params: HashSet<(String, Span)>,

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

    /// Top-level function definitions from the module
    pub module_functions: HashMap<String, &'a UntypedFunction>,

    /// Top-level validator definitions from the module
    pub module_validators: HashMap<String, (Span, Vec<String>)>,

    /// Top-level functions that have been inferred
    pub inferred_functions: HashMap<String, TypedFunction>,

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

    /// A mapping from known annotations to their resolved type.
    pub annotations: HashMap<Annotation, Rc<Type>>,

    /// The user-defined target environment referred to as the module 'env'.
    pub target_env: Option<&'a str>,

    /// Warnings
    pub warnings: &'a mut Vec<Warning>,
}

impl<'a> Environment<'a> {
    #[allow(clippy::result_large_err)]
    pub fn find_module(&self, fragments: &[String], location: Span) -> Result<&'a TypeInfo, Error> {
        let mut name = fragments.join("/");

        let is_env = name == ast::ENV_MODULE;

        if is_env {
            name = self
                .target_env
                .unwrap_or(ast::DEFAULT_ENV_MODULE)
                .to_string()
        }

        self.importable_modules.get(&name).ok_or_else(|| {
            if is_env {
                Error::UnknownEnvironment {
                    name,
                    known_environments: self
                        .importable_modules
                        .values()
                        .filter_map(|m| match m.kind {
                            ModuleKind::Env => Some(m.name.clone()),
                            ModuleKind::Lib | ModuleKind::Validator | ModuleKind::Config => None,
                        })
                        .collect(),
                }
            } else {
                Error::UnknownModule {
                    location,
                    name,
                    known_modules: self.importable_modules.keys().cloned().collect(),
                }
            }
        })
    }

    pub fn close_scope(&mut self, data: ScopeResetData) {
        let unused = self
            .entity_usages
            .pop()
            .expect("There was no top entity scope.");

        self.handle_unused(unused);

        self.scope = data.local_values;
    }

    pub fn annotate(&mut self, return_type: Rc<Type>, annotation: &Annotation) -> Rc<Type> {
        self.annotations
            .insert(annotation.clone(), return_type.clone());
        return_type
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

    #[allow(clippy::result_large_err)]
    pub fn match_fun_type(
        &mut self,
        tipo: Rc<Type>,
        arity: usize,
        fn_location: Span,
        call_location: Span,
    ) -> Result<(Vec<Rc<Type>>, Rc<Type>), Error> {
        if let Type::Var { tipo, alias } = tipo.deref() {
            let new_value = match tipo.borrow().deref() {
                TypeVar::Link { tipo } => {
                    let (args, ret) =
                        self.match_fun_type(tipo.clone(), arity, fn_location, call_location)?;
                    return Ok((args, Type::with_alias(ret, alias.clone())));
                }

                TypeVar::Unbound { .. } => {
                    let args: Vec<_> = (0..arity).map(|_| self.new_unbound_var()).collect();

                    let ret = Type::with_alias(self.new_unbound_var(), alias.clone());

                    Some((args, ret))
                }

                TypeVar::Generic { .. } => None,
            };

            if let Some((args, ret)) = new_value {
                *tipo.borrow_mut() = TypeVar::Link {
                    tipo: Type::function(args.clone(), ret.clone()),
                };

                return Ok((args, Type::with_alias(ret, alias.clone())));
            }
        }

        if let Type::Fn {
            args,
            ret,
            alias: _,
        } = tipo.deref()
        {
            return if args.len() != arity {
                Err(Error::IncorrectFunctionCallArity {
                    expected: args.len(),
                    given: arity,
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

    #[allow(clippy::result_large_err)]
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
                on_test_failure,
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
                    on_test_failure,
                })
            }
            Definition::Validator(Validator {
                doc,
                end_position,
                handlers,
                name,
                mut fallback,
                location,
                params,
            }) => {
                let handlers = handlers
                    .into_iter()
                    .map(|mut fun| {
                        let handler_name = TypedValidator::handler_name(&name, &fun.name);

                        let old_name = fun.name;
                        fun.name = handler_name;

                        let Definition::Fn(mut fun) =
                            self.generalise_definition(Definition::Fn(fun), module_name)
                        else {
                            unreachable!()
                        };

                        fun.name = old_name;

                        fun
                    })
                    .collect();

                let fallback_name = TypedValidator::handler_name(&name, &fallback.name);

                let old_name = fallback.name;
                fallback.name = fallback_name;

                let Definition::Fn(mut fallback) =
                    self.generalise_definition(Definition::Fn(fallback), module_name)
                else {
                    unreachable!()
                };

                fallback.name = old_name;

                Definition::Validator(Validator {
                    doc,
                    name,
                    end_position,
                    handlers,
                    fallback,
                    location,
                    params,
                })
            }

            definition @ (Definition::TypeAlias { .. }
            | Definition::DataType { .. }
            | Definition::Use { .. }
            | Definition::Test { .. }
            | Definition::ModuleConstant { .. }) => definition,
        }
    }

    #[allow(clippy::result_large_err)]
    pub fn get_type_constructor_mut(
        &mut self,
        name: &str,
        location: Span,
    ) -> Result<&mut TypeConstructor, Error> {
        let types = self.module_types.keys().map(|t| t.to_string()).collect();

        let constructor = self
            .module_types
            .get_mut(name)
            .ok_or_else(|| Error::UnknownType {
                location,
                name: name.to_string(),
                types,
            })?;

        Ok(constructor)
    }

    /// Lookup a type in the current scope.
    #[allow(clippy::result_large_err)]
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
                            known_modules: self
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
    #[allow(clippy::result_large_err)]
    pub fn get_value_constructor(
        &mut self,
        module: Option<&String>,
        name: &str,
        location: Span,
    ) -> Result<&ValueConstructor, Error> {
        match module {
            None => self
                .scope
                .get(name)
                .ok_or_else(|| Error::UnknownTypeConstructor {
                    location,
                    name: name.to_string(),
                    constructors: self.local_constructor_names(),
                }),

            Some(m) => {
                let (_, module) =
                    self.imported_modules
                        .get(m)
                        .ok_or_else(|| Error::UnknownModule {
                            name: m.to_string(),
                            known_modules: self
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
                EntityKind::ImportedType
                | EntityKind::ImportedTypeAndConstructor
                | EntityKind::ImportedConstructor
                | EntityKind::ImportedValue => {
                    Warning::UnusedImportedValueOrType { name, location }
                }
                EntityKind::PrivateConstant => {
                    Warning::UnusedPrivateModuleConstant { name, location }
                }
                EntityKind::PrivateTypeConstructor(_) => {
                    Warning::UnusedConstructor { name, location }
                }
                EntityKind::PrivateFunction => Warning::UnusedPrivateFunction { name, location },
                EntityKind::PrivateType => Warning::UnusedType { name, location },
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
            .expect("Attempted to access non-existent entity usages scope")
            .insert(name.to_string(), (kind, location, false))
        {
            // Private types can be shadowed by a constructor with the same name
            //
            // TODO: Improve this so that we can tell if an imported overridden
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
    #[allow(clippy::result_large_err)]
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
        tipo: Rc<Type>,
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
        t: Rc<Type>,
        ids: &mut HashMap<u64, Rc<Type>>,
        hydrator: &Hydrator,
    ) -> Rc<Type> {
        match t.deref() {
            Type::App {
                public,
                contains_opaque: opaque,
                name,
                module,
                args,
                alias,
            } => {
                let args = args
                    .iter()
                    .map(|t| self.instantiate(t.clone(), ids, hydrator))
                    .collect();

                Rc::new(Type::App {
                    public: *public,
                    contains_opaque: *opaque,
                    name: name.clone(),
                    module: module.clone(),
                    alias: alias.clone(),
                    args,
                })
            }

            Type::Var { tipo, alias } => {
                match tipo.borrow().deref() {
                    TypeVar::Link { tipo } => {
                        return Type::with_alias(
                            self.instantiate(tipo.clone(), ids, hydrator),
                            alias.clone(),
                        );
                    }

                    TypeVar::Unbound { .. } => {
                        return Rc::new(Type::Var {
                            tipo: tipo.clone(),
                            alias: alias.clone(),
                        });
                    }

                    TypeVar::Generic { id } => match ids.get(id) {
                        Some(t) => return Type::with_alias(t.clone(), alias.clone()),
                        None => {
                            if !hydrator.is_rigid(id) {
                                // Check this in the hydrator, i.e. is it a created type
                                let v = Type::with_alias(self.new_unbound_var(), alias.clone());
                                ids.insert(*id, v.clone());
                                return v;
                            }
                        }
                    },
                }
                Rc::new(Type::Var {
                    tipo: tipo.clone(),
                    alias: alias.clone(),
                })
            }

            Type::Fn { args, ret, alias } => Type::with_alias(
                Type::function(
                    args.iter()
                        .map(|t| self.instantiate(t.clone(), ids, hydrator))
                        .collect(),
                    self.instantiate(ret.clone(), ids, hydrator),
                ),
                alias.clone(),
            ),

            Type::Tuple { elems, alias } => Type::with_alias(
                Type::tuple(
                    elems
                        .iter()
                        .map(|t| self.instantiate(t.clone(), ids, hydrator))
                        .collect(),
                ),
                alias.clone(),
            ),
            Type::Pair { fst, snd, alias } => Type::with_alias(
                Type::pair(
                    self.instantiate(fst.clone(), ids, hydrator),
                    self.instantiate(snd.clone(), ids, hydrator),
                ),
                alias.clone(),
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

    pub fn local_constructor_names(&self) -> Vec<String> {
        self.scope
            .keys()
            .filter(|&t| t.chars().next().unwrap_or_default().is_uppercase())
            .map(|t| t.to_string())
            .collect()
    }

    #[allow(clippy::result_large_err)]
    fn make_type_vars(
        &mut self,
        args: &[String],
        location: &Span,
        hydrator: &mut Hydrator,
    ) -> Result<Vec<Rc<Type>>, Error> {
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
        current_kind: &'a ModuleKind,
        importable_modules: &'a HashMap<String, TypeInfo>,
        warnings: &'a mut Vec<Warning>,
        target_env: Option<&'a str>,
    ) -> Self {
        let prelude = importable_modules
            .get("aiken")
            .expect("Unable to find prelude in importable modules");

        Self {
            previous_id: id_gen.next(),
            id_gen,
            ungeneralised_functions: HashSet::new(),
            inferred_functions: HashMap::new(),
            module_types: prelude.types.clone(),
            module_types_constructors: prelude.types_constructors.clone(),
            module_values: HashMap::new(),
            module_functions: HashMap::new(),
            module_validators: HashMap::new(),
            imported_modules: HashMap::new(),
            unused_modules: HashMap::new(),
            unqualified_imported_names: HashMap::new(),
            accessors: prelude.accessors.clone(),
            scope: prelude.values.clone(),
            importable_modules,
            imported_types: HashSet::new(),
            current_module,
            current_kind,
            annotations: HashMap::new(),
            warnings,
            entity_usages: vec![HashMap::new()],
            validator_params: HashSet::new(),
            target_env,
        }
    }

    /// Create a new generic type that can stand in for any type.
    pub fn new_generic_var(&mut self) -> Rc<Type> {
        Type::generic_var(self.next_uid())
    }

    /// Create a new unbound type that is a specific type, we just don't
    /// know which one yet.
    pub fn new_unbound_var(&mut self) -> Rc<Type> {
        Type::unbound_var(self.next_uid())
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

    #[allow(clippy::result_large_err)]
    pub fn register_import(&mut self, def: &UntypedDefinition) -> Result<(), Error> {
        match def {
            Definition::Use(Use {
                module,
                as_name,
                unqualified,
                location,
                package: _,
            }) => {
                let module_info = self.find_module(module, *location)?;

                if module_info.kind.is_validator()
                    && (self.current_kind.is_lib() || self.current_kind.is_env())
                {
                    return Err(Error::ValidatorImported {
                        location: *location,
                        name: module.join("/"),
                    });
                }

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
                            module: module.clone(),
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
                        module: module.clone(),
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
    #[allow(clippy::result_large_err)]
    pub fn register_types(
        &mut self,
        definitions: Vec<&'a UntypedDefinition>,
        module: &String,
        hydrators: &mut HashMap<String, Hydrator>,
        names: &mut HashMap<&'a str, &'a Span>,
    ) -> Result<(), Error> {
        let known_types_before = names.keys().copied().collect::<Vec<_>>();

        let mut errors = vec![];
        let mut remaining_definitions = vec![];

        // in case we failed at registering a type-definition, we backtrack and
        // try again until either of:
        //
        // (a) we do not make any more progress;
        // (b) there's no more errors.
        //
        // This is because some definition, especially when combining type-aliases may depend on
        // types that we haven't yet seen (because declared later in the module). In which case, it
        // would suffice to register type in a different order. Thus instead of failing on the
        // first error, we try to register as many types as we can, recursively until we've
        // exhausted all the definitions or, until we no longer make any progress (which may signal
        // a cycle).
        for def in definitions {
            if let Err(e) = self.register_type(def, module, hydrators, names) {
                let type_name = match def {
                    Definition::TypeAlias(TypeAlias { alias, .. }) => {
                        names.remove(alias.as_str());
                        Some(alias)
                    }
                    Definition::DataType(DataType { name, .. }) => Some(name),
                    _ => None,
                };
                errors.push((type_name, e));
                remaining_definitions.push(def);
            };
        }

        if errors.is_empty() {
            return Ok(());
        }

        let known_types_after = names.keys().copied().collect::<Vec<_>>();
        if known_types_before == known_types_after {
            let (type_definitions, mut unknowns): (Vec<_>, Vec<_>) = errors.into_iter().unzip();

            let first_error = unknowns.first().cloned();

            unknowns.retain(|err| {
                if let Error::UnknownType { ref name, .. } = err {
                    !type_definitions.contains(&Some(name))
                } else {
                    false
                }
            });

            if unknowns.is_empty() {
                let cycle = remaining_definitions
                    .iter()
                    .filter_map(|def| match def {
                        Definition::TypeAlias(TypeAlias { location, .. })
                        | Definition::DataType(DataType { location, .. }) => Some(*location),
                        Definition::Fn { .. }
                        | Definition::Validator { .. }
                        | Definition::Use { .. }
                        | Definition::ModuleConstant { .. }
                        | Definition::Test { .. } => None,
                    })
                    .collect::<Vec<Span>>();

                Err(Error::CyclicTypeDefinitions { cycle })
            } else {
                Err(first_error.unwrap())
            }
        } else {
            self.register_types(remaining_definitions, module, hydrators, names)
        }
    }

    #[allow(clippy::result_large_err)]
    pub fn register_type(
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
                opaque,
                parameters,
                location,
                constructors,
                doc: _,
                typed_parameters: _,
            }) => {
                assert_unique_type_name(names, name, location)?;

                // Build a type from the type Annotation
                let mut hydrator = Hydrator::new();

                let parameters = self.make_type_vars(parameters, location, &mut hydrator)?;

                let tipo = Rc::new(Type::App {
                    public: *public,
                    contains_opaque: *opaque,
                    module: module.to_owned(),
                    name: name.clone(),
                    args: parameters.clone(),
                    alias: None,
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
                doc: _,
                tipo: _,
            }) => {
                assert_unique_type_name(names, name, location)?;

                // Register the parameterised types
                let mut hydrator = Hydrator::new();
                let parameters = self.make_type_vars(args, location, &mut hydrator)?;

                // Disallow creation of new types outside the parameterised types
                hydrator.disallow_new_type_variables();

                // Create the type that the alias resolves to
                let tipo = hydrator
                    .type_from_annotation(resolved_type, self)?
                    .as_ref()
                    .to_owned()
                    .set_alias(Some(
                        TypeAliasAnnotation {
                            alias: name.to_string(),
                            parameters: args.to_vec(),
                            annotation: resolved_type.clone(),
                        }
                        .into(),
                    ));

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
            | Definition::Validator { .. }
            | Definition::Test { .. }
            | Definition::Use { .. }
            | Definition::ModuleConstant { .. } => {}
        }

        Ok(())
    }

    #[allow(clippy::too_many_arguments)]
    #[allow(clippy::result_large_err)]
    fn register_function(
        &mut self,
        name: &str,
        arguments: &[UntypedArg],
        return_annotation: &Option<Annotation>,
        module_name: &String,
        hydrators: &mut HashMap<String, Hydrator>,
        names: &mut HashMap<String, &'a Span>,
        location: &'a Span,
    ) -> Result<(), Error> {
        assert_unique_value_name(names, name, location)?;

        self.ungeneralised_functions.insert(name.to_string());

        // Create the field map so we can reorder labels for usage of this function
        let mut field_map = FieldMap::new(arguments.len(), true);

        for (i, arg) in arguments.iter().enumerate() {
            field_map.insert(arg.arg_name(i).get_label(), i, &arg.location)?;
        }
        let field_map = field_map.into_option();

        // Construct type from annotations
        let mut hydrator = Hydrator::new();

        let mut arg_types = Vec::new();

        for arg in arguments {
            let tipo = hydrator.type_from_option_annotation(&arg.annotation, self)?;

            arg_types.push(tipo);
        }

        let return_type = hydrator.type_from_option_annotation(return_annotation, self)?;

        let tipo = Type::function(arg_types, return_type);

        // Keep track of which types we create from annotations so we can know
        // which generic types not to instantiate later when performing
        // inference of the function body.
        hydrators.insert(name.to_string(), hydrator);

        // Insert the function into the environment
        self.insert_variable(
            name.to_string(),
            ValueConstructorVariant::ModuleFn {
                name: name.to_string(),
                field_map,
                module: module_name.to_owned(),
                arity: arguments.len(),
                location: *location,
                builtin: None,
            },
            tipo,
        );

        Ok(())
    }

    #[allow(clippy::result_large_err)]
    pub fn register_values(
        &mut self,
        def: &'a UntypedDefinition,
        module_name: &String,
        hydrators: &mut HashMap<String, Hydrator>,
        names: &mut HashMap<String, &'a Span>,
        kind: ModuleKind,
    ) -> Result<(), Error> {
        match def {
            Definition::Fn(fun) => {
                self.register_function(
                    &fun.name,
                    &fun.arguments,
                    &fun.return_annotation,
                    module_name,
                    hydrators,
                    names,
                    &fun.location,
                )?;

                self.module_functions.insert(fun.name.clone(), fun);

                if !fun.public {
                    self.init_usage(fun.name.clone(), EntityKind::PrivateFunction, fun.location);
                }
            }

            Definition::Validator(Validator {
                handlers,
                fallback,
                params,
                name,
                doc: _,
                location,
                end_position: _,
            }) if kind.is_validator() => {
                let default_annotation = |mut arg: UntypedArg, ann: Annotation| {
                    if arg.annotation.is_none() {
                        arg.annotation = Some(ann);
                        arg
                    } else {
                        arg
                    }
                };

                let mut handler_names = vec![];

                let params_len = params.len();

                for handler in handlers {
                    let temp_params: Vec<UntypedArg> = params
                        .iter()
                        .cloned()
                        .chain(handler.arguments.clone())
                        .enumerate()
                        .map(|(ix, arg)| {
                            let is_datum = handler.is_spend() && ix == params_len;
                            let is_mint_policy = handler.is_mint() && ix == params_len + 1;
                            let location = arg.location;
                            default_annotation(
                                arg,
                                if is_datum {
                                    Annotation::option(Annotation::data(location))
                                } else if is_mint_policy {
                                    Annotation::bytearray(location)
                                } else {
                                    Annotation::data(location)
                                },
                            )
                        })
                        .collect();

                    handler_names.push(handler.name.clone());

                    self.register_function(
                        &TypedValidator::handler_name(name.as_str(), handler.name.as_str()),
                        &temp_params,
                        &handler.return_annotation,
                        module_name,
                        hydrators,
                        names,
                        &handler.location,
                    )?;
                }

                let temp_params: Vec<UntypedArg> = params
                    .iter()
                    .cloned()
                    .chain(fallback.arguments.clone())
                    .map(|arg| {
                        let location = arg.location;
                        default_annotation(arg, Annotation::data(location))
                    })
                    .collect();

                self.register_function(
                    &TypedValidator::handler_name(name.as_str(), fallback.name.as_str()),
                    &temp_params,
                    &fallback.return_annotation,
                    module_name,
                    hydrators,
                    names,
                    &fallback.location,
                )?;

                handler_names.push(fallback.name.clone());

                let err_duplicate_name = |previous_location: Span| {
                    Err(Error::DuplicateName {
                        name: name.to_string(),
                        previous_location,
                        location: location.map_end(|end| end + 1 + name.len()),
                    })
                };

                if let Some((previous_location, _)) = self.imported_modules.get(name) {
                    return err_duplicate_name(*previous_location);
                }

                match self
                    .module_validators
                    .insert(name.to_string(), (*location, handler_names))
                {
                    Some((previous_location, _)) => err_duplicate_name(previous_location),
                    None => Ok(()),
                }?
            }

            Definition::Validator(Validator { location, .. }) => {
                self.warnings.push(Warning::ValidatorInLibraryModule {
                    location: *location,
                })
            }

            Definition::Test(test) => {
                let arguments = test
                    .arguments
                    .iter()
                    .map(|arg| arg.clone().into())
                    .collect::<Vec<_>>();

                self.register_function(
                    &test.name,
                    &arguments,
                    &test.return_annotation,
                    module_name,
                    hydrators,
                    names,
                    &test.location,
                )?;
            }

            Definition::DataType(DataType {
                public,
                opaque,
                name,
                constructors,
                doc: _,
                location: _,
                parameters: _,
                typed_parameters: _,
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

                    let mut field_map = FieldMap::new(constructor.arguments.len(), false);

                    let mut args_types = Vec::with_capacity(constructor.arguments.len());

                    for (
                        i,
                        RecordConstructorArg {
                            label,
                            annotation,
                            location,
                            tipo: _,
                            doc: _,
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
                        _ => Type::function(args_types, typ.clone()),
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
    #[allow(clippy::result_large_err)]
    pub fn unify(
        &mut self,
        lhs: Rc<Type>,
        rhs: Rc<Type>,
        location: Span,
        allow_cast: bool,
    ) -> Result<(), Error> {
        if lhs == rhs {
            return Ok(());
        }

        // TODO: maybe we also care to check is_link?
        if allow_cast
            && (lhs.is_data() || rhs.is_data())
            && !(lhs.is_unbound() || rhs.is_unbound())
            && !(lhs.is_function() || rhs.is_function())
            && !(lhs.is_generic() || rhs.is_generic())
            && !(lhs.is_string() || rhs.is_string())
            && !lhs.contains_opaque()
        {
            return Ok(());
        }

        if allow_cast && lhs.contains_opaque() {
            return Err(Error::ExpectOnOpaqueType { location });
        }

        // Collapse right hand side type links. Left hand side will be collapsed in the next block.
        if let Type::Var { tipo, alias } = rhs.deref() {
            if let TypeVar::Link { tipo } = tipo.borrow().deref() {
                return self.unify(
                    lhs,
                    Type::with_alias(tipo.clone(), alias.clone()),
                    location,
                    allow_cast,
                );
            }
        }

        if let Type::Var { tipo, alias } = lhs.deref() {
            enum Action {
                Unify(Rc<Type>),
                CouldNotUnify,
                Link,
            }

            let action = match tipo.borrow().deref() {
                TypeVar::Link { tipo } => {
                    Action::Unify(Type::with_alias(tipo.clone(), alias.clone()))
                }

                TypeVar::Unbound { id } => {
                    unify_unbound_type(rhs.clone(), *id, location)?;
                    Action::Link
                }

                TypeVar::Generic { id } => {
                    if let Type::Var { tipo, alias: _ } = rhs.deref() {
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
                    *tipo.borrow_mut() = TypeVar::Link { tipo: rhs };
                    Ok(())
                }

                Action::Unify(t) => self.unify(t, rhs, location, allow_cast),

                Action::CouldNotUnify => Err(Error::CouldNotUnify {
                    location,
                    expected: lhs.clone(),
                    given: rhs,
                    situation: None,
                    rigid_type_names: HashMap::new(),
                }),
            };
        }

        if let Type::Var { .. } = rhs.deref() {
            return self
                .unify(rhs, lhs, location, false)
                .map_err(|e| e.flip_unify());
        }

        match (lhs.deref(), rhs.deref()) {
            (
                Type::App {
                    module: m1,
                    name: n1,
                    args: args1,
                    public: _,
                    contains_opaque: _,
                    alias: _,
                },
                Type::App {
                    module: m2,
                    name: n2,
                    args: args2,
                    public: _,
                    contains_opaque: _,
                    alias: _,
                },
            ) if m1 == m2 && n1 == n2 && args1.len() == args2.len() => {
                for (a, b) in args1.iter().zip(args2) {
                    unify_enclosed_type(
                        lhs.clone(),
                        rhs.clone(),
                        self.unify(a.clone(), b.clone(), location, false),
                    )?;
                }
                Ok(())
            }

            (
                Type::Tuple {
                    elems: elems1,
                    alias: _,
                },
                Type::Tuple {
                    elems: elems2,
                    alias: _,
                },
            ) if elems1.len() == elems2.len() => {
                for (a, b) in elems1.iter().zip(elems2) {
                    unify_enclosed_type(
                        lhs.clone(),
                        rhs.clone(),
                        self.unify(a.clone(), b.clone(), location, false),
                    )?;
                }
                Ok(())
            }

            (
                Type::Pair {
                    fst: lhs_fst,
                    snd: lhs_snd,
                    alias: _,
                },
                Type::Pair {
                    fst: rhs_fst,
                    snd: rhs_snd,
                    alias: _,
                },
            ) => {
                for (a, b) in [lhs_fst, lhs_snd].into_iter().zip([rhs_fst, rhs_snd]) {
                    unify_enclosed_type(
                        lhs.clone(),
                        rhs.clone(),
                        self.unify(a.clone(), b.clone(), location, false),
                    )?;
                }
                Ok(())
            }

            (
                Type::Fn {
                    args: args1,
                    ret: retrn1,
                    alias: _,
                },
                Type::Fn {
                    args: args2,
                    ret: retrn2,
                    alias: _,
                },
            ) if args1.len() == args2.len() => {
                for (a, b) in args1.iter().zip(args2) {
                    self.unify(a.clone(), b.clone(), location, allow_cast)
                        .map_err(|_| Error::CouldNotUnify {
                            location,
                            expected: lhs.clone(),
                            given: rhs.clone(),
                            situation: None,
                            rigid_type_names: HashMap::new(),
                        })?;
                }
                self.unify(retrn1.clone(), retrn2.clone(), location, false)
                    .map_err(|_| Error::CouldNotUnify {
                        location,
                        expected: lhs.clone(),
                        given: rhs.clone(),
                        situation: None,
                        rigid_type_names: HashMap::new(),
                    })
            }

            _ => Err(Error::CouldNotUnify {
                location,
                expected: lhs.clone(),
                given: rhs.clone(),
                situation: None,
                rigid_type_names: HashMap::new(),
            }),
        }
    }

    /// Checks that the given patterns are exhaustive for given type.
    /// https://github.com/elm/compiler/blob/047d5026fe6547c842db65f7196fed3f0b4743ee/compiler/src/Nitpick/PatternMatches.hs#L397-L475
    /// http://moscova.inria.fr/~maranget/papers/warn/index.html
    #[allow(clippy::result_large_err)]
    pub fn check_exhaustiveness(
        &mut self,
        unchecked_patterns: &[&TypedPattern],
        location: Span,
        is_let: bool,
    ) -> Result<(), Error> {
        let mut matrix = Matrix::new();

        for unchecked_pattern in unchecked_patterns {
            let pattern = simplify(self, unchecked_pattern)?;
            let pattern_stack = PatternStack::from(pattern);

            if matrix.is_useful(&pattern_stack) {
                matrix.push(pattern_stack);
            } else {
                let original = matrix
                    .flatten()
                    .into_iter()
                    .enumerate()
                    .find(|(_, p)| p == pattern_stack.head())
                    .and_then(|(index, _)| unchecked_patterns.get(index))
                    .map(|typed_pattern| typed_pattern.location());

                return Err(Error::RedundantMatchClause {
                    original,
                    redundant: unchecked_pattern.location(),
                });
            }
        }

        let missing_patterns = matrix.collect_missing_patterns(1).flatten();

        if !missing_patterns.is_empty() {
            let unmatched = missing_patterns
                .into_iter()
                .map(|pattern| pattern.pretty())
                .collect();

            return Err(Error::NotExhaustivePatternMatch {
                location,
                unmatched,
                is_let,
            });
        }

        Ok(())
    }

    /// Lookup constructors for type in the current scope.
    #[allow(clippy::result_large_err)]
    pub fn get_constructors_for_type(
        &mut self,
        full_module_name: &String,
        name: &str,
        location: Span,
    ) -> Result<Vec<ValueConstructor>, Error> {
        if full_module_name.is_empty() || full_module_name == self.current_module {
            self.module_types_constructors
                .get(name)
                .ok_or_else(|| Error::UnknownType {
                    name: name.to_string(),
                    types: self.module_types.keys().map(|t| t.to_string()).collect(),
                    location,
                })?
                .iter()
                .map(|constructor| {
                    self.scope
                        .get(constructor)
                        .cloned()
                        .ok_or_else(|| Error::UnknownModuleValue {
                            name: name.to_string(),
                            module_name: self.current_module.clone(),
                            value_constructors: self
                                .module_values
                                .keys()
                                .map(|t| t.to_string())
                                .collect(),
                            location,
                        })
                })
                .collect()
        } else {
            let module = self
                .importable_modules
                .get(full_module_name)
                .ok_or_else(|| Error::UnknownModule {
                    location,
                    name: name.to_string(),
                    known_modules: self
                        .importable_modules
                        .keys()
                        .map(|t| t.to_string())
                        .collect(),
                })?;

            self.unused_modules.remove(full_module_name);

            let constructors =
                module
                    .types_constructors
                    .get(name)
                    .ok_or_else(|| Error::UnknownModuleType {
                        location,
                        name: name.to_string(),
                        module_name: module.name.clone(),
                        type_constructors: module.types.keys().map(|t| t.to_string()).collect(),
                    })?;

            constructors
                .iter()
                .map(|constructor| {
                    module.values.get(constructor).cloned().ok_or_else(|| {
                        Error::UnknownModuleValue {
                            name: name.to_string(),
                            module_name: module.name.clone(),
                            value_constructors: module
                                .values
                                .keys()
                                .map(|t| t.to_string())
                                .collect(),
                            location,
                        }
                    })
                })
                .collect()
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
#[allow(clippy::result_large_err)]
fn unify_unbound_type(tipo: Rc<Type>, own_id: u64, location: Span) -> Result<(), Error> {
    if let Type::Var { tipo, alias } = tipo.deref() {
        let new_value = match tipo.borrow().deref() {
            TypeVar::Link { tipo } => {
                return unify_unbound_type(
                    Type::with_alias(tipo.clone(), alias.clone()),
                    own_id,
                    location,
                );
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
        Type::App {
            args,
            module: _,
            name: _,
            public: _,
            alias: _,
            contains_opaque: _,
        } => {
            for arg in args {
                unify_unbound_type(arg.clone(), own_id, location)?
            }

            Ok(())
        }

        Type::Fn {
            args,
            ret,
            alias: _,
        } => {
            for arg in args {
                unify_unbound_type(arg.clone(), own_id, location)?;
            }

            unify_unbound_type(ret.clone(), own_id, location)
        }

        Type::Tuple { elems, alias: _ } => {
            for elem in elems {
                unify_unbound_type(elem.clone(), own_id, location)?
            }

            Ok(())
        }
        Type::Pair { fst, snd, alias: _ } => {
            unify_unbound_type(fst.clone(), own_id, location)?;
            unify_unbound_type(snd.clone(), own_id, location)?;

            Ok(())
        }

        Type::Var { .. } => unreachable!(),
    }
}

#[allow(clippy::result_large_err)]
fn unify_enclosed_type(e1: Rc<Type>, e2: Rc<Type>, result: Result<(), Error>) -> Result<(), Error> {
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

#[allow(clippy::result_large_err)]
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

#[allow(clippy::result_large_err)]
fn assert_unique_value_name<'a>(
    names: &mut HashMap<String, &'a Span>,
    name: &str,
    location: &'a Span,
) -> Result<(), Error> {
    match names.insert(name.to_string(), location) {
        Some(previous_location) => Err(Error::DuplicateName {
            name: name.to_string(),
            previous_location: *previous_location,
            location: *location,
        }),
        None => Ok(()),
    }
}

#[allow(clippy::result_large_err)]
fn assert_unique_const_name<'a>(
    names: &mut HashMap<String, &'a Span>,
    name: &str,
    location: &'a Span,
) -> Result<(), Error> {
    match names.insert(name.to_string(), location) {
        Some(previous_location) => Err(Error::DuplicateConstName {
            name: name.to_string(),
            previous_location: *previous_location,
            location: *location,
        }),
        None => Ok(()),
    }
}

pub(super) fn assert_no_labeled_arguments<A>(args: &[CallArg<A>]) -> Option<(Span, String)> {
    for arg in args {
        if let Some(label) = &arg.label {
            return Some((arg.location, label.to_string()));
        }
    }
    None
}

pub fn collapse_links(t: Rc<Type>) -> Rc<Type> {
    if let Type::Var { tipo, alias } = t.deref() {
        if let TypeVar::Link { tipo } = tipo.borrow().deref() {
            return Type::with_alias(tipo.clone(), alias.clone());
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

    if constructors.len() > 1 {
        return compatible;
    }

    let first = match constructors.first() {
        Some(first) => first,
        None => return compatible,
    };

    for (index, first_argument) in first.arguments.iter().enumerate() {
        // Fields without labels do not have accessors
        let label = match first_argument.label.as_ref() {
            Some(label) => label.as_str(),
            None => continue,
        };

        compatible.push((index, label, &first_argument.annotation))
    }

    compatible
}

/// Takes a level and a type and turns all type variables within the type that have
/// level higher than the input level into generalized (polymorphic) type variables.
#[allow(clippy::only_used_in_recursion)]
pub(crate) fn generalise(t: Rc<Type>, ctx_level: usize) -> Rc<Type> {
    match t.deref() {
        Type::Var { tipo, alias } => Type::with_alias(
            match tipo.borrow().deref() {
                TypeVar::Unbound { id } => Type::generic_var(*id),
                TypeVar::Link { tipo } => generalise(tipo.clone(), ctx_level),
                TypeVar::Generic { .. } => Rc::new(Type::Var {
                    tipo: tipo.clone(),
                    alias: None,
                }),
            },
            alias.clone(),
        ),

        Type::App {
            public,
            contains_opaque: opaque,
            module,
            name,
            args,
            alias,
        } => {
            let args = args
                .iter()
                .map(|t| generalise(t.clone(), ctx_level))
                .collect();

            Rc::new(Type::App {
                public: *public,
                contains_opaque: *opaque,
                module: module.clone(),
                name: name.clone(),
                args,
                alias: alias.clone(),
            })
        }

        Type::Fn { args, ret, alias } => Type::with_alias(
            Type::function(
                args.iter()
                    .map(|t| generalise(t.clone(), ctx_level))
                    .collect(),
                generalise(ret.clone(), ctx_level),
            ),
            alias.clone(),
        ),

        Type::Tuple { elems, alias } => Type::with_alias(
            Type::tuple(
                elems
                    .iter()
                    .map(|t| generalise(t.clone(), ctx_level))
                    .collect(),
            ),
            alias.clone(),
        ),
        Type::Pair { fst, snd, alias } => Type::with_alias(
            Type::pair(
                generalise(fst.clone(), ctx_level),
                generalise(snd.clone(), ctx_level),
            ),
            alias.clone(),
        ),
    }
}
