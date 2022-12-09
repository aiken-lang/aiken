use std::collections::HashMap;

use crate::{
    ast::{
        DataType, Definition, Function, Layer, ModuleConstant, ModuleKind, RecordConstructor,
        RecordConstructorArg, TypeAlias, TypedDefinition, TypedModule, UntypedDefinition,
        UntypedModule, Use,
    },
    builtins,
    builtins::function,
    parser::token::Token,
    IdGenerator,
};

use super::{
    environment::{generalise, EntityKind, Environment},
    error::{Error, Warning},
    expr::ExprTyper,
    hydrator::Hydrator,
    TypeInfo, ValueConstructor, ValueConstructorVariant,
};

impl UntypedModule {
    pub fn infer(
        mut self,
        id_gen: &IdGenerator,
        kind: ModuleKind,
        package: &str,
        modules: &HashMap<String, TypeInfo>,
        warnings: &mut Vec<Warning>,
    ) -> Result<TypedModule, Error> {
        let name = self.name.clone();
        let docs = std::mem::take(&mut self.docs);
        let mut environment = Environment::new(id_gen.clone(), &name, modules, warnings);

        validate_module_name(&name)?;

        let mut type_names = HashMap::with_capacity(self.definitions.len());
        let mut value_names = HashMap::with_capacity(self.definitions.len());
        let mut hydrators = HashMap::with_capacity(self.definitions.len());

        // Register any modules, types, and values being imported
        // We process imports first so that anything imported can be referenced
        // anywhere in the module.
        for def in self.definitions() {
            environment.register_import(def)?;
        }

        // Register types so they can be used in constructors and functions
        // earlier in the module.
        for def in self.definitions() {
            environment.register_types(def, &name, &mut hydrators, &mut type_names)?;
        }

        // Register values so they can be used in functions earlier in the module.
        for def in self.definitions() {
            environment.register_values(def, &name, &mut hydrators, &mut value_names)?;
        }

        // Infer the types of each definition in the module
        // We first infer all the constants so they can be used in functions defined
        // anywhere in the module.
        let mut definitions = Vec::with_capacity(self.definitions.len());
        let mut consts = vec![];
        let mut not_consts = vec![];

        for def in self.definitions().cloned() {
            match def {
                Definition::ModuleConstant { .. } => consts.push(def),
                Definition::Fn { .. }
                | Definition::Test { .. }
                | Definition::TypeAlias { .. }
                | Definition::DataType { .. }
                | Definition::Use { .. } => not_consts.push(def),
            }
        }

        for def in consts.into_iter().chain(not_consts) {
            let definition = infer_definition(def, &name, &mut hydrators, &mut environment)?;

            definitions.push(definition);
        }

        // Generalise functions now that the entire module has been inferred
        let definitions = definitions
            .into_iter()
            .map(|def| environment.generalise_definition(def, &name))
            .collect();

        // Generate warnings for unused items
        environment.convert_unused_to_warnings();

        // Remove private and imported types and values to create the public interface
        environment
            .module_types
            .retain(|_, info| info.public && info.module == name);

        environment.module_values.retain(|_, info| info.public);

        environment
            .accessors
            .retain(|_, accessors| accessors.public);

        // Ensure no exported values have private types in their type signature
        for value in environment.module_values.values() {
            if let Some(leaked) = value.tipo.find_private_type() {
                return Err(Error::PrivateTypeLeak {
                    location: value.variant.location(),
                    leaked,
                });
            }
        }

        let Environment {
            module_types: types,
            module_types_constructors: types_constructors,
            module_values: values,
            accessors,
            ..
        } = environment;

        Ok(TypedModule {
            docs,
            name: name.clone(),
            definitions,
            kind,
            type_info: TypeInfo {
                name,
                types,
                types_constructors,
                values,
                accessors,
                kind,
                package: package.to_string(),
            },
        })
    }
}

fn infer_definition(
    def: UntypedDefinition,
    module_name: &String,
    hydrators: &mut HashMap<String, Hydrator>,
    environment: &mut Environment<'_>,
) -> Result<TypedDefinition, Error> {
    match def {
        Definition::Fn(Function {
            doc,
            location,
            name,
            public,
            arguments: args,
            body,
            return_annotation,
            end_position,
            ..
        }) => {
            let preregistered_fn = environment
                .get_variable(&name)
                .expect("Could not find preregistered type for function");

            let field_map = preregistered_fn.field_map().cloned();

            let preregistered_type = preregistered_fn.tipo.clone();

            let (args_types, return_type) = preregistered_type
                .function_types()
                .expect("Preregistered type for fn was not a fn");

            // Infer the type using the preregistered args + return types as a starting point
            let (tipo, args, body, safe_to_generalise) =
                environment.in_new_scope(|environment| {
                    let args = args
                        .into_iter()
                        .zip(&args_types)
                        .map(|(arg_name, tipo)| arg_name.set_type(tipo.clone()))
                        .collect();

                    let mut expr_typer = ExprTyper::new(environment);

                    expr_typer.hydrator = hydrators
                        .remove(&name)
                        .expect("Could not find hydrator for fn");

                    let (args, body) =
                        expr_typer.infer_fn_with_known_types(args, body, Some(return_type))?;

                    let args_types = args.iter().map(|a| a.tipo.clone()).collect();

                    let tipo = function(args_types, body.tipo());

                    let safe_to_generalise = !expr_typer.ungeneralised_function_used;

                    Ok((tipo, args, body, safe_to_generalise))
                })?;

            // Assert that the inferred type matches the type of any recursive call
            environment.unify(preregistered_type, tipo.clone(), location)?;

            // Generalise the function if safe to do so
            let tipo = if safe_to_generalise {
                environment.ungeneralised_functions.remove(&name);

                let tipo = generalise(tipo, 0);

                let module_fn = ValueConstructorVariant::ModuleFn {
                    name: name.clone(),
                    field_map,
                    module: module_name.to_owned(),
                    arity: args.len(),
                    location,
                    builtin: None,
                };

                environment.insert_variable(name.clone(), module_fn, tipo.clone());

                tipo
            } else {
                tipo
            };

            Ok(Definition::Fn(Function {
                doc,
                location,
                name,
                public,
                arguments: args,
                return_annotation,
                return_type: tipo
                    .return_type()
                    .expect("Could not find return type for fn"),
                body,
                end_position,
            }))
        }

        Definition::Test(f) => {
            if let Definition::Fn(f) =
                infer_definition(Definition::Fn(f), module_name, hydrators, environment)?
            {
                environment.unify(f.return_type.clone(), builtins::bool(), f.location)?;
                Ok(Definition::Test(f))
            } else {
                unreachable!("test defintion inferred as something else than a function?")
            }
        }

        Definition::TypeAlias(TypeAlias {
            doc,
            location,
            public,
            alias,
            parameters,
            annotation,
            ..
        }) => {
            let tipo = environment
                .get_type_constructor(&None, &alias, location)
                .expect("Could not find existing type for type alias")
                .tipo
                .clone();

            Ok(Definition::TypeAlias(TypeAlias {
                doc,
                location,
                public,
                alias,
                parameters,
                annotation,
                tipo,
            }))
        }

        Definition::DataType(DataType {
            doc,
            location,
            public,
            opaque,
            name,
            parameters,
            constructors,
            ..
        }) => {
            let constructors = constructors
                .into_iter()
                .map(
                    |RecordConstructor {
                         location,
                         name,
                         arguments: args,
                         documentation,
                         sugar,
                     }| {
                        let preregistered_fn = environment
                            .get_variable(&name)
                            .expect("Could not find preregistered type for function");

                        let preregistered_type = preregistered_fn.tipo.clone();

                        let args = if let Some((args_types, _return_type)) =
                            preregistered_type.function_types()
                        {
                            args.into_iter()
                                .zip(&args_types)
                                .map(
                                    |(
                                        RecordConstructorArg {
                                            label,
                                            annotation,
                                            location,
                                            ..
                                        },
                                        t,
                                    )| {
                                        RecordConstructorArg {
                                            label,
                                            annotation,
                                            location,
                                            tipo: t.clone(),
                                            doc: None,
                                        }
                                    },
                                )
                                .collect()
                        } else {
                            vec![]
                        };

                        RecordConstructor {
                            location,
                            name,
                            arguments: args,
                            documentation,
                            sugar,
                        }
                    },
                )
                .collect();

            let typed_parameters = environment
                .get_type_constructor(&None, &name, location)
                .expect("Could not find preregistered type constructor ")
                .parameters
                .clone();

            Ok(Definition::DataType(DataType {
                doc,
                location,
                public,
                opaque,
                name,
                parameters,
                constructors,
                typed_parameters,
            }))
        }

        Definition::Use(Use {
            location,
            module,
            as_name,
            mut unqualified,
            ..
        }) => {
            let name = module.join("/");

            // Find imported module
            let module_info =
                environment
                    .importable_modules
                    .get(&name)
                    .ok_or_else(|| Error::UnknownModule {
                        location,
                        name,
                        imported_modules: environment.imported_modules.keys().cloned().collect(),
                    })?;

            // TODO: remove this most likely
            // Record any imports that are types only as this information is
            // needed to prevent types being imported in generated JavaScript
            for import in unqualified.iter_mut() {
                if environment.imported_types.contains(import.variable_name()) {
                    import.layer = Layer::Type;
                }
            }

            Ok(Definition::Use(Use {
                location,
                module,
                as_name,
                unqualified,
                package: module_info.package.clone(),
            }))
        }

        Definition::ModuleConstant(ModuleConstant {
            doc,
            location,
            name,
            annotation,
            public,
            value,
            ..
        }) => {
            let typed_expr = ExprTyper::new(environment).infer_const(&annotation, *value)?;

            let tipo = typed_expr.tipo();

            let variant = ValueConstructor {
                public,
                variant: ValueConstructorVariant::ModuleConstant {
                    location,
                    literal: typed_expr.clone(),
                    module: module_name.to_owned(),
                },
                tipo: tipo.clone(),
            };

            environment.insert_variable(name.clone(), variant.variant.clone(), tipo.clone());

            environment.insert_module_value(&name, variant);

            if !public {
                environment.init_usage(name.clone(), EntityKind::PrivateConstant, location);
            }

            Ok(Definition::ModuleConstant(ModuleConstant {
                doc,
                location,
                name,
                annotation,
                public,
                value: Box::new(typed_expr),
                tipo,
            }))
        }
    }
}

fn validate_module_name(name: &str) -> Result<(), Error> {
    if name == "aiken" || name == "aiken/builtin" {
        return Err(Error::ReservedModuleName {
            name: name.to_string(),
        });
    };

    for segment in name.split('/') {
        if str_to_keyword(segment).is_some() {
            return Err(Error::KeywordInModuleName {
                name: name.to_string(),
                keyword: segment.to_string(),
            });
        }
    }

    Ok(())
}

fn str_to_keyword(word: &str) -> Option<Token> {
    // Alphabetical keywords:
    match word {
        "as" => Some(Token::As),
        "assert" => Some(Token::Assert),
        "check" => Some(Token::Check),
        "when" => Some(Token::When),
        "const" => Some(Token::Const),
        "fn" => Some(Token::Fn),
        "if" => Some(Token::If),
        "use" => Some(Token::Use),
        "let" => Some(Token::Let),
        "opaque" => Some(Token::Opaque),
        "pub" => Some(Token::Pub),
        "todo" => Some(Token::Todo),
        "try" => Some(Token::Trace),
        "type" => Some(Token::Type),
        _ => None,
    }
}
