//! Produce an `ExportedDataSchema`-compatible schema from a fully-typed Aiken
//! `Type` backed by an `IndexMap<DataTypeKey, TypedDataType>`.
//!
//! This is a parallel path to [`Annotated::from_type`](super::schema::Annotated::from_type)
//! which derives its schema from the blueprint-visible AST. That path fails on
//! `opaque` types such as `cardano/transaction.Transaction`, because the
//! blueprint boundary deliberately hides the structural shape of opaque types.
//!
//! The verify pipeline has the full structural type information available via
//! `Project::data_types` and can therefore lower opaque types to their
//! underlying representation by calling [`convert_opaque_type`] at every
//! field boundary. The function in this module mirrors the logic of
//! `do_from_type` in [`super::schema`] but substitutes opaque-aware lookup for
//! the blueprint's `find_data_type`.
//!
//! The result is a [`Reference`] and a populated [`Definitions<Annotated<Schema>>`]
//! that can be wrapped directly in an [`ExportedDataSchema`](crate::export::ExportedDataSchema),
//! after which the existing `LeanDataShapeBuilder` machinery in `verify.rs`
//! produces the Lean predicates unchanged.

use std::{collections::HashMap, ops::Deref, rc::Rc};

use aiken_lang::{
    ast::{DataTypeKey, DecoratorKind, TypedDataType, well_known},
    tipo::{Type, TypeVar, convert_opaque_type, find_and_replace_generics},
};
use indexmap::IndexMap;

use crate::blueprint::{
    definitions::{Definitions, Reference},
    schema::{Annotated, Constructor, Data, Declaration, Items, Schema},
};

/// Build a schema for `tipo`, inserting any transitively-referenced
/// definitions into `definitions` and returning the root `Reference`.
///
/// Returns `None` if the type contains unresolved type variables or otherwise
/// cannot be lowered to a structural schema (e.g. `String`, `Fn`).
pub fn build_schema_from_type(
    tipo: &Rc<Type>,
    data_types: &IndexMap<&DataTypeKey, &TypedDataType>,
    definitions: &mut Definitions<Annotated<Schema>>,
) -> Option<Reference> {
    let mut type_parameters: HashMap<u64, Rc<Type>> = HashMap::new();
    do_build(tipo, data_types, &mut type_parameters, definitions)
}

fn do_build(
    tipo: &Rc<Type>,
    data_types: &IndexMap<&DataTypeKey, &TypedDataType>,
    type_parameters: &mut HashMap<u64, Rc<Type>>,
    definitions: &mut Definitions<Annotated<Schema>>,
) -> Option<Reference> {
    let original_tipo = tipo.clone();
    // Unwrap opaque wrappers so that, e.g., an `opaque type ScriptHash = Hash<…>`
    // reports the underlying representation structurally. `deep = true` keeps
    // unwrapping through nested opaque wrappers.
    let tipo = convert_opaque_type(tipo, data_types, true);
    let wrapper_title = (original_tipo.as_ref() != tipo.as_ref()).then(|| original_tipo.to_pretty(0));

    if contains_self_mapped_generic(original_tipo.as_ref(), type_parameters) {
        return None;
    }

    // Primitive leaves are tested BEFORE any structural match so that generic
    // ADTs like `Option<Int>` don't accidentally dispatch to the ADT branch
    // when args happen to include a primitive.
    if tipo.is_int() {
        return Some(register_inline(&original_tipo, type_parameters, definitions, |_| {
            if let Some(title) = wrapper_title.clone() {
                Annotated {
                    title: Some(title),
                    description: None,
                    annotated: Schema::Data(Data::Integer),
                }
            } else {
                Annotated::from(Schema::int())
            }
        }));
    }
    if tipo.is_bytearray() {
        return Some(register_inline(&original_tipo, type_parameters, definitions, |_| {
            if let Some(title) = wrapper_title.clone() {
                Annotated {
                    title: Some(title),
                    description: None,
                    annotated: Schema::Data(Data::Bytes),
                }
            } else {
                Annotated::from(Schema::bytes())
            }
        }));
    }
    if tipo.is_bool() {
        return Some(register_inline(&original_tipo, type_parameters, definitions, |_| {
            Annotated {
                title: wrapper_title.clone().or(Some("Bool".to_string())),
                description: None,
                annotated: Schema::bool(),
            }
        }));
    }
    if tipo.is_void() {
        return Some(register_inline(&original_tipo, type_parameters, definitions, |_| {
            Annotated {
                title: wrapper_title.clone().or(Some("Unit".to_string())),
                description: None,
                annotated: Schema::void(),
            }
        }));
    }
    if tipo.is_data() {
        return Some(register_inline(&original_tipo, type_parameters, definitions, |_| {
            Annotated {
                title: wrapper_title.clone().or(Some("Data".to_string())),
                description: Some("Any Plutus data.".to_string()),
                annotated: Schema::Data(Data::Opaque),
            }
        }));
    }
    if tipo.is_string() {
        // `String` has no `Data` encoding; surface as unrepresentable.
        return None;
    }
    if type_is_never(&tipo) {
        return Some(register_inline(&original_tipo, type_parameters, definitions, |_| {
            Annotated {
                title: wrapper_title.clone().or(Some("Never".to_string())),
                description: None,
                annotated: Schema::Data(Data::AnyOf(vec![Annotated {
                    title: Some("Never".to_string()),
                    description: Some("Nothing.".to_string()),
                    annotated: Constructor {
                        // NOTE: Tag 1 — the `__hole` constructor at index 0 is
                        // a placeholder that can never be inhabited.
                        index: 1,
                        fields: vec![],
                    },
                }])),
            }
        }));
    }

    match tipo.as_ref() {
        Type::App {
            module: module_name,
            name: type_name,
            args,
            ..
        } if module_name.is_empty() => match type_name.as_str() {
            "Option" => {
                let inner = args.first()?;
                let generic = do_build(inner, data_types, type_parameters, definitions)?;
                Some(register_inline_type(
                    &original_tipo,
                    type_parameters,
                    definitions,
                    Annotated {
                        title: wrapper_title.clone().or(Some("Option".to_string())),
                        description: None,
                        annotated: Schema::Data(Data::AnyOf(vec![
                            Annotated {
                                title: Some("Some".to_string()),
                                description: Some("An optional value.".to_string()),
                                annotated: Constructor {
                                    index: 0,
                                    fields: vec![Declaration::Referenced(generic).into()],
                                },
                            },
                            Annotated {
                                title: Some("None".to_string()),
                                description: Some("Nothing.".to_string()),
                                annotated: Constructor {
                                    index: 1,
                                    fields: vec![],
                                },
                            },
                        ])),
                    },
                ))
            }
            "List" => {
                let inner = args.first()?;
                let generic = do_build(inner, data_types, type_parameters, definitions)?;
                // If the element reduces to a `Pair`, emit a `Data::Map` like
                // the blueprint path does (Plutus convention inherited from
                // PlutusTx / LedgerApi).
                let data = match definitions.try_lookup(&generic).cloned() {
                    Some(Annotated {
                        annotated: Schema::Pair(left, right),
                        ..
                    }) => {
                        let left = left.map(|inner| match inner {
                            Schema::Data(data) => data,
                            _ => unreachable!("left inhabitant of pair isn't Data"),
                        });
                        let right = right.map(|inner| match inner {
                            Schema::Data(data) => data,
                            _ => unreachable!("right inhabitant of pair isn't Data"),
                        });
                        Data::Map(left, right)
                    }
                    _ => Data::List(Items::One(Declaration::Referenced(generic))),
                };
                Some(register_inline_type(
                    &original_tipo,
                    type_parameters,
                    definitions,
                    Annotated {
                        title: wrapper_title.clone().or(Some("List".to_string())),
                        description: None,
                        annotated: Schema::Data(data),
                    },
                ))
            }
            "Ordering" => Some(register_inline(&original_tipo, type_parameters, definitions, |_| {
                Annotated {
                    title: wrapper_title.clone().or(Some("Ordering".to_string())),
                    description: None,
                    annotated: Schema::Data(Data::AnyOf(vec![
                        Annotated {
                            title: Some("Less".to_string()),
                            description: None,
                            annotated: Constructor {
                                index: 0,
                                fields: vec![],
                            },
                        },
                        Annotated {
                            title: Some("Equal".to_string()),
                            description: None,
                            annotated: Constructor {
                                index: 1,
                                fields: vec![],
                            },
                        },
                        Annotated {
                            title: Some("Greater".to_string()),
                            description: None,
                            annotated: Constructor {
                                index: 2,
                                fields: vec![],
                            },
                        },
                    ])),
                }
            })),
            _ => None,
        },

        // Qualified ADT — the case the blueprint-backed schema cannot handle
        // for opaque types.
        Type::App {
            name, module, args, ..
        } => {
            let key = DataTypeKey {
                module_name: module.clone(),
                defined_type: name.clone(),
            };
            let data_type = *(data_types.get(&key)?);

            // Zip `typed_parameters` with the applied `args` to build a
            // substitution that monomorphises field types.
            let mut local_params = type_parameters.clone();
            collect_type_parameters(&mut local_params, &data_type.typed_parameters, args);

            if contains_self_mapped_generic(original_tipo.as_ref(), &local_params) {
                return None;
            }

            // `register` uses `Reference::from_type(tipo, type_parameters)` to
            // key the cache; it handles cycle detection by pre-inserting
            // `None` before calling the builder.
            let key_params = local_params.clone();
            let applied_args = args.clone();
            let result = definitions.register::<_, BuildError>(
                original_tipo.as_ref(),
                &key_params,
                |definitions| {
                    build_data_type(
                        data_type,
                        &applied_args,
                        wrapper_title.clone(),
                        data_types,
                        &mut local_params,
                        definitions,
                    )
                    .ok_or(BuildError::Unresolvable)
                },
            );
            result.ok()
        }

        Type::Pair { fst, snd, .. } => {
            let params_snapshot = type_parameters.clone();
            let title_override = wrapper_title.clone();
            let result = definitions.register::<_, BuildError>(
                original_tipo.as_ref(),
                &params_snapshot,
                |definitions| {
                    let mut inner = params_snapshot.clone();
                    let left = do_build(fst, data_types, &mut inner, definitions)
                        .ok_or(BuildError::Unresolvable)?;
                    let right = do_build(snd, data_types, &mut inner, definitions)
                        .ok_or(BuildError::Unresolvable)?;
                    Ok(Annotated {
                        title: title_override.or(Some("Pair".to_owned())),
                        description: None,
                        annotated: Schema::Pair(
                            Declaration::Referenced(left),
                            Declaration::Referenced(right),
                        ),
                    })
                },
            );
            result.ok()
        }

        Type::Tuple { elems, .. } => {
            let params_snapshot = type_parameters.clone();
            let title_override = wrapper_title.clone();
            let result = definitions.register::<_, BuildError>(
                original_tipo.as_ref(),
                &params_snapshot,
                |definitions| {
                    let mut inner = params_snapshot.clone();
                    let mut annotated_elems = Vec::with_capacity(elems.len());
                    for elem in elems {
                        let refr = do_build(elem, data_types, &mut inner, definitions)
                            .ok_or(BuildError::Unresolvable)?;
                        annotated_elems.push(Annotated::from(Declaration::Referenced(refr)));
                    }
                    Ok(Annotated {
                        title: title_override.or(Some("Tuple".to_owned())),
                        description: None,
                        annotated: Schema::Data(Data::List(Items::Many(annotated_elems))),
                    })
                },
            );
            result.ok()
        }

        Type::Var { tipo: var_tipo, .. } => match var_tipo.borrow().deref() {
            TypeVar::Link { tipo } => do_build(tipo, data_types, type_parameters, definitions),
            TypeVar::Generic { id } => {
                let concrete = type_parameters.get(id)?.clone();
                if concrete.get_generic_id() == Some(*id) {
                    None
                } else {
                    do_build(&concrete, data_types, type_parameters, definitions)
                }
            }
            TypeVar::Unbound { .. } => None,
        },

        Type::Fn { .. } => None,
    }
}

fn build_data_type(
    data_type: &TypedDataType,
    applied_args: &[Rc<Type>],
    title_override: Option<String>,
    data_types: &IndexMap<&DataTypeKey, &TypedDataType>,
    type_parameters: &mut HashMap<u64, Rc<Type>>,
    definitions: &mut Definitions<Annotated<Schema>>,
) -> Option<Annotated<Schema>> {
    // Re-collect so that monomorphisation happens against the specific
    // instantiation captured on the stack, not a stale snapshot.
    collect_type_parameters(type_parameters, &data_type.typed_parameters, applied_args);

    let title = title_override.or_else(|| Some(data_type.name.clone()));
    let description = data_type.doc.clone().map(|s| s.trim().to_string());

    // `@list` single-constructor sugar (e.g. `pub type Foo { Foo(Int, Bar) }`
    // decorated `@list` to force a plain `Data.List` encoding).
    if data_type.constructors.len() == 1
        && data_type.constructors[0].sugar
        && data_type
            .decorators
            .iter()
            .any(|d| matches!(d.kind, DecoratorKind::List))
    {
        let items = data_type.constructors[0]
            .arguments
            .iter()
            .map(|arg| {
                // Monomorphise the field type before recursing.
                let mono = find_and_replace_generics(&arg.tipo, &index_map_from(type_parameters));
                let refr = do_build(&mono, data_types, type_parameters, definitions)?;
                Some(Annotated {
                    title: arg.label.clone(),
                    description: None,
                    annotated: Declaration::Referenced(refr),
                })
            })
            .collect::<Option<Vec<_>>>()?;
        return Some(Annotated {
            title,
            description,
            annotated: Schema::Data(Data::List(Items::Many(items))),
        });
    }

    let mut variants = Vec::with_capacity(data_type.constructors.len());
    for (index, constructor) in data_type.constructors.iter().enumerate() {
        let mut fields = Vec::with_capacity(constructor.arguments.len());
        for field in constructor.arguments.iter() {
            let mono = find_and_replace_generics(&field.tipo, &index_map_from(type_parameters));
            let refr = do_build(&mono, data_types, type_parameters, definitions)?;
            fields.push(Annotated {
                title: field.label.clone(),
                description: field.doc.clone().map(|s| s.trim().to_string()),
                annotated: Declaration::Referenced(refr),
            });
        }

        // Tag override via `@tag(N)` decorator; otherwise use position.
        let decorators = if constructor.sugar {
            &data_type.decorators
        } else {
            &constructor.decorators
        };
        let index = decorators
            .iter()
            .find_map(|decorator| {
                if let DecoratorKind::Tag { value, .. } = &decorator.kind {
                    value.parse::<usize>().ok()
                } else {
                    None
                }
            })
            .unwrap_or(index);

        variants.push(Annotated {
            title: Some(constructor.name.clone()),
            description: constructor.doc.clone().map(|s| s.trim().to_string()),
            annotated: Constructor { index, fields },
        });
    }

    Some(Annotated {
        title,
        description,
        annotated: Schema::Data(Data::AnyOf(variants)),
    })
}

fn contains_self_mapped_generic(
    tipo: &Type,
    type_parameters: &HashMap<u64, Rc<Type>>,
) -> bool {
    match tipo {
        Type::App { args, .. } => args
            .iter()
            .any(|arg| contains_self_mapped_generic(arg.as_ref(), type_parameters)),
        Type::Tuple { elems, .. } => elems
            .iter()
            .any(|elem| contains_self_mapped_generic(elem.as_ref(), type_parameters)),
        Type::Pair { fst, snd, .. } => {
            contains_self_mapped_generic(fst.as_ref(), type_parameters)
                || contains_self_mapped_generic(snd.as_ref(), type_parameters)
        }
        Type::Fn { args, ret, .. } => {
            args.iter()
                .any(|arg| contains_self_mapped_generic(arg.as_ref(), type_parameters))
                || contains_self_mapped_generic(ret.as_ref(), type_parameters)
        }
        Type::Var { tipo, .. } => match tipo.borrow().deref() {
            TypeVar::Link { tipo } => contains_self_mapped_generic(tipo.as_ref(), type_parameters),
            TypeVar::Generic { id } | TypeVar::Unbound { id } => type_parameters
                .get(id)
                .is_some_and(|mapped| mapped.get_generic_id() == Some(*id)),
        },
    }
}

fn collect_type_parameters(
    type_parameters: &mut HashMap<u64, Rc<Type>>,
    generics: &[Rc<Type>],
    applications: &[Rc<Type>],
) {
    for (index, generic) in generics.iter().enumerate() {
        if let Type::Var { tipo, .. } = generic.as_ref()
            && let TypeVar::Generic { id } = *tipo.borrow()
            && let Some(applied) = applications.get(index)
        {
            type_parameters.insert(id, applied.clone());
        }
    }
}

/// Helper: `HashMap<u64, Rc<Type>>` -> `IndexMap<u64, Rc<Type>>` (for
/// `find_and_replace_generics`).
fn index_map_from(map: &HashMap<u64, Rc<Type>>) -> IndexMap<u64, Rc<Type>> {
    map.iter().map(|(k, v)| (*k, v.clone())).collect()
}

fn type_is_never(tipo: &Type) -> bool {
    match tipo {
        Type::App { module, name, .. } => module.is_empty() && name == well_known::NEVER,
        Type::Var { tipo, .. } => match tipo.borrow().deref() {
            TypeVar::Link { tipo } => type_is_never(tipo),
            _ => false,
        },
        _ => false,
    }
}

/// Register an inline (primitive/leaf) schema using the current type's
/// reference key. The callback is only invoked the first time the key is
/// encountered; subsequent registrations return the cached reference.
fn register_inline<F>(
    tipo: &Type,
    type_parameters: &HashMap<u64, Rc<Type>>,
    definitions: &mut Definitions<Annotated<Schema>>,
    build: F,
) -> Reference
where
    F: FnOnce(&mut Definitions<Annotated<Schema>>) -> Annotated<Schema>,
{
    definitions
        .register::<_, BuildError>(tipo, type_parameters, |defs| Ok(build(defs)))
        .expect("register_inline builder is infallible")
}

fn register_inline_type(
    tipo: &Type,
    type_parameters: &HashMap<u64, Rc<Type>>,
    definitions: &mut Definitions<Annotated<Schema>>,
    annotated: Annotated<Schema>,
) -> Reference {
    definitions
        .register::<_, BuildError>(tipo, type_parameters, |_| Ok(annotated))
        .expect("register_inline_type builder is infallible")
}

#[derive(Debug)]
enum BuildError {
    Unresolvable,
}

#[cfg(test)]
mod tests {
    use std::{cell::RefCell, rc::Rc};

    use aiken_lang::{
        ast::{
            DataType, DataTypeKey, Decorator, DecoratorKind, RecordConstructor,
            RecordConstructorArg, Span, TypedDataType,
        },
        parser::token::Base,
        tipo::{Type, TypeVar},
    };
    use indexmap::IndexMap;

    use super::*;
    use crate::blueprint::{
        definitions::Definitions,
        schema::{Data, Schema},
    };

    fn int_type() -> Rc<Type> {
        Rc::new(Type::App {
            public: true,
            contains_opaque: false,
            module: String::new(),
            name: "Int".to_string(),
            args: vec![],
            alias: None,
        })
    }

    fn bytearray_type() -> Rc<Type> {
        Rc::new(Type::App {
            public: true,
            contains_opaque: false,
            module: String::new(),
            name: "ByteArray".to_string(),
            args: vec![],
            alias: None,
        })
    }

    fn never_type() -> Rc<Type> {
        Rc::new(Type::App {
            public: true,
            contains_opaque: false,
            module: String::new(),
            name: aiken_lang::ast::well_known::NEVER.to_string(),
            args: vec![],
            alias: None,
        })
    }

    fn option_type(inner: Rc<Type>) -> Rc<Type> {
        Rc::new(Type::App {
            public: true,
            contains_opaque: false,
            module: String::new(),
            name: "Option".to_string(),
            args: vec![inner],
            alias: None,
        })
    }

    fn user_app(module: &str, name: &str, args: Vec<Rc<Type>>) -> Rc<Type> {
        Rc::new(Type::App {
            public: true,
            contains_opaque: false,
            module: module.to_string(),
            name: name.to_string(),
            args,
            alias: None,
        })
    }

    fn nullary_ctor(name: &str) -> RecordConstructor<Rc<Type>> {
        RecordConstructor {
            decorators: vec![],
            location: Span::empty(),
            name: name.to_string(),
            arguments: vec![],
            doc: None,
            sugar: false,
        }
    }

    fn ctor_with_field(name: &str, field_ty: Rc<Type>) -> RecordConstructor<Rc<Type>> {
        RecordConstructor {
            decorators: vec![],
            location: Span::empty(),
            name: name.to_string(),
            arguments: vec![RecordConstructorArg {
                label: Some("value".to_string()),
                annotation: aiken_lang::ast::Annotation::Hole {
                    location: Span::empty(),
                    name: "_".to_string(),
                },
                location: Span::empty(),
                tipo: field_ty,
                doc: None,
            }],
            doc: None,
            sugar: false,
        }
    }

    fn generic_var(id: u64) -> Rc<Type> {
        Rc::new(Type::Var {
            tipo: Rc::new(RefCell::new(TypeVar::Generic { id })),
            alias: None,
        })
    }

    fn make_opaque_wrapper_type(
        name: &str,
        field_ty: Rc<Type>,
        typed_parameters: Vec<Rc<Type>>,
    ) -> TypedDataType {
        DataType {
            decorators: vec![],
            constructors: vec![ctor_with_field(name, field_ty)],
            doc: None,
            location: Span::empty(),
            name: name.to_string(),
            opaque: true,
            parameters: vec![],
            public: true,
            typed_parameters,
        }
    }

    fn tagged_ctor(name: &str, tag: &str) -> RecordConstructor<Rc<Type>> {
        RecordConstructor {
            decorators: vec![Decorator {
                kind: DecoratorKind::Tag {
                    value: tag.to_string(),
                    base: Base::Decimal {
                        numeric_underscore: false,
                    },
                },
                location: Span::empty(),
            }],
            location: Span::empty(),
            name: name.to_string(),
            arguments: vec![],
            doc: None,
            sugar: false,
        }
    }

    fn make_data_type(name: &str, ctors: Vec<RecordConstructor<Rc<Type>>>) -> TypedDataType {
        DataType {
            decorators: vec![],
            constructors: ctors,
            doc: None,
            location: Span::empty(),
            name: name.to_string(),
            opaque: false,
            parameters: vec![],
            public: true,
            typed_parameters: vec![],
        }
    }

    #[test]
    fn primitive_int_produces_integer() {
        let dts: IndexMap<DataTypeKey, TypedDataType> = IndexMap::new();
        let refs = aiken_lang::utils::indexmap::as_ref_values(&dts);
        let mut defs = Definitions::new();
        let root = build_schema_from_type(&int_type(), &refs, &mut defs).unwrap();
        let entry = defs.lookup(&root).unwrap();
        assert!(matches!(entry.annotated, Schema::Data(Data::Integer)));
    }

    #[test]
    fn primitive_bytearray_produces_bytes() {
        let dts: IndexMap<DataTypeKey, TypedDataType> = IndexMap::new();
        let refs = aiken_lang::utils::indexmap::as_ref_values(&dts);
        let mut defs = Definitions::new();
        let root = build_schema_from_type(&bytearray_type(), &refs, &mut defs).unwrap();
        let entry = defs.lookup(&root).unwrap();
        assert!(matches!(entry.annotated, Schema::Data(Data::Bytes)));
    }

    #[test]
    fn option_int_produces_expected_anyof() {
        let dts: IndexMap<DataTypeKey, TypedDataType> = IndexMap::new();
        let refs = aiken_lang::utils::indexmap::as_ref_values(&dts);
        let mut defs = Definitions::new();
        let root = build_schema_from_type(&option_type(int_type()), &refs, &mut defs).unwrap();
        let entry = defs.lookup(&root).unwrap();
        match &entry.annotated {
            Schema::Data(Data::AnyOf(variants)) => {
                assert_eq!(variants.len(), 2);
                assert_eq!(variants[0].annotated.index, 0);
                assert_eq!(variants[0].annotated.fields.len(), 1);
                assert_eq!(variants[1].annotated.index, 1);
                assert_eq!(variants[1].annotated.fields.len(), 0);
            }
            other => panic!("expected Data::AnyOf, got {:?}", other),
        }
    }

    #[test]
    fn two_constructor_enum_produces_anyof_with_position_tags() {
        // type Color { Red, Blue }
        let color = make_data_type("Color", vec![nullary_ctor("Red"), nullary_ctor("Blue")]);
        let key = DataTypeKey {
            module_name: "mymod".to_string(),
            defined_type: "Color".to_string(),
        };
        let mut dts: IndexMap<DataTypeKey, TypedDataType> = IndexMap::new();
        dts.insert(key.clone(), color);
        let refs = aiken_lang::utils::indexmap::as_ref_values(&dts);
        let mut defs = Definitions::new();
        let root =
            build_schema_from_type(&user_app("mymod", "Color", vec![]), &refs, &mut defs).unwrap();
        match &defs.lookup(&root).unwrap().annotated {
            Schema::Data(Data::AnyOf(vs)) => {
                assert_eq!(vs.len(), 2);
                assert_eq!(vs[0].annotated.index, 0);
                assert_eq!(vs[1].annotated.index, 1);
            }
            other => panic!("expected AnyOf, got {:?}", other),
        }
    }

    #[test]
    fn tag_decorator_overrides_constructor_index() {
        // type Weird { Default, Tagged @tag(5) }
        let weird = make_data_type(
            "Weird",
            vec![nullary_ctor("Default"), tagged_ctor("Tagged", "5")],
        );
        let key = DataTypeKey {
            module_name: "mymod".to_string(),
            defined_type: "Weird".to_string(),
        };
        let mut dts: IndexMap<DataTypeKey, TypedDataType> = IndexMap::new();
        dts.insert(key.clone(), weird);
        let refs = aiken_lang::utils::indexmap::as_ref_values(&dts);
        let mut defs = Definitions::new();
        let root =
            build_schema_from_type(&user_app("mymod", "Weird", vec![]), &refs, &mut defs).unwrap();
        match &defs.lookup(&root).unwrap().annotated {
            Schema::Data(Data::AnyOf(vs)) => {
                assert_eq!(vs.len(), 2);
                assert_eq!(vs[0].annotated.index, 0);
                // The second constructor MUST be at 5 (from @tag(5)),
                // not at 1 (position).
                assert_eq!(vs[1].annotated.index, 5);
            }
            other => panic!("expected AnyOf, got {:?}", other),
        }
    }

    #[test]
    fn ctor_with_int_field_produces_single_field_constructor() {
        let cell = make_data_type("Cell", vec![ctor_with_field("Cell", int_type())]);
        let key = DataTypeKey {
            module_name: "mymod".to_string(),
            defined_type: "Cell".to_string(),
        };
        let mut dts: IndexMap<DataTypeKey, TypedDataType> = IndexMap::new();
        dts.insert(key.clone(), cell);
        let refs = aiken_lang::utils::indexmap::as_ref_values(&dts);
        let mut defs = Definitions::new();
        let root =
            build_schema_from_type(&user_app("mymod", "Cell", vec![]), &refs, &mut defs).unwrap();
        match &defs.lookup(&root).unwrap().annotated {
            Schema::Data(Data::AnyOf(vs)) => {
                assert_eq!(vs.len(), 1);
                assert_eq!(vs[0].annotated.fields.len(), 1);
            }
            other => panic!("expected AnyOf, got {:?}", other),
        }
    }

    #[test]
    fn opaque_wrappers_keep_distinct_references_and_titles() {
        let mut dts: IndexMap<DataTypeKey, TypedDataType> = IndexMap::new();
        dts.insert(
            DataTypeKey {
                module_name: "crypto".to_string(),
                defined_type: "ScriptHash".to_string(),
            },
            make_opaque_wrapper_type("ScriptHash", bytearray_type(), vec![]),
        );
        dts.insert(
            DataTypeKey {
                module_name: "crypto".to_string(),
                defined_type: "DatumHash".to_string(),
            },
            make_opaque_wrapper_type("DatumHash", bytearray_type(), vec![]),
        );

        let refs = aiken_lang::utils::indexmap::as_ref_values(&dts);
        let mut defs = Definitions::new();
        let script_ref =
            build_schema_from_type(&user_app("crypto", "ScriptHash", vec![]), &refs, &mut defs)
                .expect("opaque wrapper should resolve");
        let datum_ref =
            build_schema_from_type(&user_app("crypto", "DatumHash", vec![]), &refs, &mut defs)
                .expect("second opaque wrapper should resolve");

        assert_ne!(script_ref, datum_ref);

        let script = defs.lookup(&script_ref).expect("script wrapper should be registered");
        let datum = defs.lookup(&datum_ref).expect("datum wrapper should be registered");
        assert!(matches!(script.annotated, Schema::Data(Data::Bytes)));
        assert!(matches!(datum.annotated, Schema::Data(Data::Bytes)));
        assert!(
            script
                .title
                .as_deref()
                .is_some_and(|title| title.contains("ScriptHash"))
        );
        assert!(
            datum
                .title
                .as_deref()
                .is_some_and(|title| title.contains("DatumHash"))
        );
    }

    #[test]
    fn self_mapped_generic_returns_none_instead_of_recursing() {
        let generic = generic_var(0);
        let mut dts: IndexMap<DataTypeKey, TypedDataType> = IndexMap::new();
        dts.insert(
            DataTypeKey {
                module_name: "mymod".to_string(),
                defined_type: "Box".to_string(),
            },
            DataType {
                decorators: vec![],
                constructors: vec![ctor_with_field("Box", generic.clone())],
                doc: None,
                location: Span::empty(),
                name: "Box".to_string(),
                opaque: false,
                parameters: vec![],
                public: true,
                typed_parameters: vec![generic.clone()],
            },
        );

        let refs = aiken_lang::utils::indexmap::as_ref_values(&dts);
        let mut defs = Definitions::new();
        let result =
            build_schema_from_type(&user_app("mymod", "Box", vec![generic]), &refs, &mut defs);
        assert!(result.is_none());
    }

    #[test]
    fn unknown_adt_returns_none() {
        let dts: IndexMap<DataTypeKey, TypedDataType> = IndexMap::new();
        let refs = aiken_lang::utils::indexmap::as_ref_values(&dts);
        let mut defs = Definitions::new();
        let result =
            build_schema_from_type(&user_app("no_mod", "Missing", vec![]), &refs, &mut defs);
        assert!(result.is_none());
    }

    #[test]
    fn string_returns_none() {
        let s = Rc::new(Type::App {
            public: true,
            contains_opaque: false,
            module: String::new(),
            name: "String".to_string(),
            args: vec![],
            alias: None,
        });
        let dts: IndexMap<DataTypeKey, TypedDataType> = IndexMap::new();
        let refs = aiken_lang::utils::indexmap::as_ref_values(&dts);
        let mut defs = Definitions::new();
        let result = build_schema_from_type(&s, &refs, &mut defs);
        assert!(result.is_none());
    }

    #[test]
    fn never_uses_tag_one_not_zero() {
        let dts: IndexMap<DataTypeKey, TypedDataType> = IndexMap::new();
        let refs = aiken_lang::utils::indexmap::as_ref_values(&dts);
        let mut defs = Definitions::new();
        let root = build_schema_from_type(&never_type(), &refs, &mut defs).unwrap();
        match &defs.lookup(&root).unwrap().annotated {
            Schema::Data(Data::AnyOf(vs)) => {
                assert_eq!(vs.len(), 1);
                // CRITICAL: `Never` uses tag 1 because the `__hole` placeholder
                // occupies position 0 in `well_known::NEVER_CONSTRUCTORS`.
                assert_eq!(vs[0].annotated.index, 1);
                assert_eq!(vs[0].annotated.fields.len(), 0);
            }
            other => panic!("expected AnyOf, got {:?}", other),
        }
    }

    #[test]
    fn unbound_type_var_returns_none() {
        let var = Rc::new(Type::Var {
            tipo: Rc::new(RefCell::new(TypeVar::Unbound { id: 42 })),
            alias: None,
        });
        let dts: IndexMap<DataTypeKey, TypedDataType> = IndexMap::new();
        let refs = aiken_lang::utils::indexmap::as_ref_values(&dts);
        let mut defs = Definitions::new();
        let result = build_schema_from_type(&var, &refs, &mut defs);
        assert!(result.is_none());
    }
}
