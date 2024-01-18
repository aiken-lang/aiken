use std::collections::HashMap;

use crate::{
    ast::{
        ArgName, DataType, Definition, Function, Layer, ModuleConstant, ModuleKind,
        RecordConstructor, RecordConstructorArg, Tracing, TypeAlias, TypedDefinition,
        TypedFunction, TypedModule, UntypedDefinition, UntypedModule, Use, Validator,
    },
    builtins,
    builtins::function,
    line_numbers::LineNumbers,
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
        tracing: Tracing,
        warnings: &mut Vec<Warning>,
    ) -> Result<TypedModule, Error> {
        let name = self.name.clone();
        let docs = std::mem::take(&mut self.docs);
        let mut environment = Environment::new(id_gen.clone(), &name, &kind, modules, warnings);

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
        environment.register_types(
            self.definitions.iter().collect(),
            &name,
            &mut hydrators,
            &mut type_names,
        )?;

        // Register values so they can be used in functions earlier in the module.
        for def in self.definitions() {
            environment.register_values(def, &name, &mut hydrators, &mut value_names, kind)?;
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
                Definition::Validator { .. } if kind.is_validator() => not_consts.push(def),
                Definition::Validator { .. } => (),
                Definition::Fn { .. }
                | Definition::Test { .. }
                | Definition::TypeAlias { .. }
                | Definition::DataType { .. }
                | Definition::Use { .. } => not_consts.push(def),
            }
        }

        for def in consts.into_iter().chain(not_consts) {
            let definition = infer_definition(
                def,
                &name,
                &mut hydrators,
                &mut environment,
                &self.lines,
                tracing,
            )?;
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
            lines: self.lines,
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
    lines: &LineNumbers,
    tracing: Tracing,
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
            can_error,
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

                    let mut expr_typer = ExprTyper::new(environment, lines, tracing);

                    expr_typer.hydrator = hydrators
                        .remove(&name)
                        .expect("Could not find hydrator for fn");

                    let (args, body) =
                        expr_typer.infer_fn_with_known_types(args, body, Some(return_type))?;

                    let args_types = args.iter().map(|a| a.tipo.clone()).collect();

                    let tipo = function(args_types, body.tipo());

                    let safe_to_generalise = !expr_typer.ungeneralised_function_used;

                    Ok::<_, Error>((tipo, args, body, safe_to_generalise))
                })?;

            // Assert that the inferred type matches the type of any recursive call
            environment.unify(preregistered_type, tipo.clone(), location, false)?;

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
                can_error,
                end_position,
            }))
        }

        Definition::Validator(Validator {
            doc,
            location,
            end_position,
            mut fun,
            other_fun,
            params,
        }) => {
            let params_length = params.len();
            let temp_params = params.iter().cloned().chain(fun.arguments);
            fun.arguments = temp_params.collect();

            environment.in_new_scope(|environment| {
                let preregistered_fn = environment
                    .get_variable(&fun.name)
                    .expect("Could not find preregistered type for function");

                let preregistered_type = preregistered_fn.tipo.clone();

                let (args_types, _return_type) = preregistered_type
                    .function_types()
                    .expect("Preregistered type for fn was not a fn");

                for (arg, t) in params.iter().zip(args_types[0..params.len()].iter()) {
                    match &arg.arg_name {
                        ArgName::Named {
                            name,
                            is_validator_param,
                            ..
                        } if *is_validator_param => {
                            environment.insert_variable(
                                name.to_string(),
                                ValueConstructorVariant::LocalVariable {
                                    location: arg.location,
                                },
                                t.clone(),
                            );

                            environment.init_usage(
                                name.to_string(),
                                EntityKind::Variable,
                                arg.location,
                            );
                        }
                        ArgName::Named { .. } | ArgName::Discarded { .. } => (),
                    };
                }

                let Definition::Fn(mut typed_fun) = infer_definition(
                    Definition::Fn(fun),
                    module_name,
                    hydrators,
                    environment,
                    lines,
                    tracing,
                )?
                else {
                    unreachable!(
                        "validator definition inferred as something other than a function?"
                    )
                };

                if !typed_fun.return_type.is_bool() {
                    return Err(Error::ValidatorMustReturnBool {
                        return_type: typed_fun.return_type.clone(),
                        location: typed_fun.location,
                    });
                }

                let typed_params = typed_fun
                    .arguments
                    .drain(0..params_length)
                    .map(|mut arg| {
                        if arg.tipo.is_unbound() {
                            arg.tipo = builtins::data();
                        }

                        arg
                    })
                    .collect();

                if typed_fun.arguments.len() < 2 || typed_fun.arguments.len() > 3 {
                    return Err(Error::IncorrectValidatorArity {
                        count: typed_fun.arguments.len() as u32,
                        location: typed_fun.location,
                    });
                }

                for arg in typed_fun.arguments.iter_mut() {
                    if arg.tipo.is_unbound() {
                        arg.tipo = builtins::data();
                    }
                }

                let typed_other_fun = other_fun
                    .map(|mut other| -> Result<TypedFunction, Error> {
                        let params = params.into_iter().chain(other.arguments);
                        other.arguments = params.collect();

                        let Definition::Fn(mut other_typed_fun) = infer_definition(
                            Definition::Fn(other),
                            module_name,
                            hydrators,
                            environment,
                            lines,
                            tracing,
                        )?
                        else {
                            unreachable!(
                                "validator definition inferred as something other than a function?"
                            )
                        };

                        if !other_typed_fun.return_type.is_bool() {
                            return Err(Error::ValidatorMustReturnBool {
                                return_type: other_typed_fun.return_type.clone(),
                                location: other_typed_fun.location,
                            });
                        }

                        other_typed_fun.arguments.drain(0..params_length);

                        if other_typed_fun.arguments.len() < 2
                            || other_typed_fun.arguments.len() > 3
                        {
                            return Err(Error::IncorrectValidatorArity {
                                count: other_typed_fun.arguments.len() as u32,
                                location: other_typed_fun.location,
                            });
                        }

                        if typed_fun.arguments.len() == other_typed_fun.arguments.len() {
                            return Err(Error::MultiValidatorEqualArgs {
                                location: typed_fun.location,
                                other_location: other_typed_fun.location,
                                count: other_typed_fun.arguments.len(),
                            });
                        }

                        for arg in other_typed_fun.arguments.iter_mut() {
                            if arg.tipo.is_unbound() {
                                arg.tipo = builtins::data();
                            }
                        }

                        Ok(other_typed_fun)
                    })
                    .transpose();

                Ok(Definition::Validator(Validator {
                    doc,
                    end_position,
                    fun: typed_fun,
                    other_fun: typed_other_fun?,
                    location,
                    params: typed_params,
                }))
            })
        }

        Definition::Test(f) => {
            if let Definition::Fn(f) = infer_definition(
                Definition::Fn(f),
                module_name,
                hydrators,
                environment,
                lines,
                tracing,
            )? {
                environment.unify(f.return_type.clone(), builtins::bool(), f.location, false)?;

                Ok(Definition::Test(f))
            } else {
                unreachable!("test definition inferred as something other than a function?")
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
            constructors: untyped_constructors,
            ..
        }) => {
            let constructors = untyped_constructors
                .into_iter()
                .map(
                    |RecordConstructor {
                         location,
                         name,
                         arguments: args,
                         doc,
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
                                            doc,
                                            ..
                                        },
                                        t,
                                    )| {
                                        RecordConstructorArg {
                                            label,
                                            annotation,
                                            location,
                                            tipo: t.clone(),
                                            doc,
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
                            doc,
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

            let typed_data = DataType {
                doc,
                location,
                public,
                opaque,
                name,
                parameters,
                constructors,
                typed_parameters,
            };

            for constr in &typed_data.constructors {
                for RecordConstructorArg { tipo, location, .. } in &constr.arguments {
                    if tipo.is_function() {
                        return Err(Error::FunctionTypeInData {
                            location: *location,
                        });
                    }
                }
            }

            Ok(Definition::DataType(typed_data))
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
            let typed_expr =
                ExprTyper::new(environment, lines, tracing).infer_const(&annotation, *value)?;

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
