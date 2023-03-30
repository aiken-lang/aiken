use crate::blueprint::definitions::{Definitions, Reference};
use crate::CheckedModule;
use aiken_lang::{
    ast::{DataType, Definition, TypedDefinition},
    builtins::wrapped_redeemer,
    tipo::{pretty, Type, TypeVar},
};
use owo_colors::{OwoColorize, Stream::Stdout};
use serde::{
    self,
    de::{self, Deserialize, Deserializer, MapAccess, Visitor},
    ser::{Serialize, SerializeStruct, Serializer},
};
use std::{collections::HashMap, fmt, ops::Deref, sync::Arc};

// NOTE: Can be anything BUT 0
pub const REDEEMER_DISCRIMINANT: usize = 1;

#[derive(Debug, PartialEq, Eq, Clone, serde::Serialize, serde::Deserialize)]
pub struct Annotated<T> {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub title: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub description: Option<String>,
    #[serde(flatten)]
    pub annotated: T,
}

/// A schema for low-level UPLC primitives.
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Schema {
    Unit,
    Boolean,
    Integer,
    Bytes,
    String,
    Pair(Data, Data),
    List(Vec<Data>),
    Data(Data),
}

/// A schema for Plutus' Data.
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Data {
    Integer,
    Bytes,
    // TODO: Generalize to work with either Reference or Data
    List(Items<Reference>),
    // TODO: Generalize to work with either Reference or Data
    Map(Box<Reference>, Box<Reference>),
    AnyOf(Vec<Annotated<Constructor>>),
    Opaque,
}

/// A structure that represents either one or many elements.
#[derive(Debug, PartialEq, Eq, Clone, serde::Deserialize)]
#[serde(untagged)]
pub enum Items<T> {
    One(Box<T>),
    Many(Vec<T>),
}

/// Captures a single UPLC constructor with its
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Constructor {
    pub index: usize,
    // TODO: Generalize to work with either Reference or Data
    pub fields: Vec<Annotated<Reference>>,
}

impl<T> From<T> for Annotated<T> {
    fn from(annotated: T) -> Self {
        Annotated {
            title: None,
            description: None,
            annotated,
        }
    }
}

impl Annotated<Schema> {
    pub fn as_wrapped_redeemer(
        definitions: &mut Definitions<Annotated<Schema>>,
        schema: Reference,
        type_info: Arc<Type>,
    ) -> Reference {
        definitions
            .register(
                &wrapped_redeemer(type_info),
                &HashMap::new(),
                |_| {
                    Ok::<_, Error>(Annotated {
                        title: Some("Wrapped Redeemer".to_string()),
                        description: Some("A redeemer wrapped in an extra constructor to make multi-validator detection possible on-chain.".to_string()),
                        annotated: Schema::Data(Data::AnyOf(vec![Constructor {
                            index: REDEEMER_DISCRIMINANT,
                            fields: vec![schema.into()],
                        }
                        .into()])),
                    })
                },
            )
            .expect("cannot fail because Ok")
    }

    pub fn from_type(
        modules: &HashMap<String, CheckedModule>,
        type_info: &Type,
        definitions: &mut Definitions<Self>,
    ) -> Result<Reference, Error> {
        Annotated::do_from_type(type_info, modules, &mut HashMap::new(), definitions)
    }

    fn do_from_type(
        type_info: &Type,
        modules: &HashMap<String, CheckedModule>,
        type_parameters: &mut HashMap<u64, Arc<Type>>,
        definitions: &mut Definitions<Self>,
    ) -> Result<Reference, Error> {
        match type_info {
            Type::App {
                module: module_name,
                name: type_name,
                args,
                ..
            } if module_name.is_empty() => {
                definitions.register(type_info, &type_parameters.clone(), |definitions| {
                    match &type_name[..] {
                        "Data" => Ok(Annotated {
                            title: Some("Data".to_string()),
                            description: Some("Any Plutus data.".to_string()),
                            annotated: Schema::Data(Data::Opaque),
                        }),

                        "ByteArray" => Ok(Schema::Data(Data::Bytes).into()),

                        "Int" => Ok(Schema::Data(Data::Integer).into()),

                        "String" => Ok(Schema::String.into()),

                        "Void" => Ok(Annotated {
                            title: Some("Unit".to_string()),
                            description: Some("The nullary constructor.".to_string()),
                            annotated: Schema::Data(Data::AnyOf(vec![Annotated {
                                title: None,
                                description: None,
                                annotated: Constructor {
                                    index: 0,
                                    fields: vec![],
                                },
                            }])),
                        }),

                        "Bool" => Ok(Annotated {
                            title: Some("Bool".to_string()),
                            description: None,
                            annotated: Schema::Data(Data::AnyOf(vec![
                                Annotated {
                                    title: Some("False".to_string()),
                                    description: None,
                                    annotated: Constructor {
                                        index: 0,
                                        fields: vec![],
                                    },
                                },
                                Annotated {
                                    title: Some("True".to_string()),
                                    description: None,
                                    annotated: Constructor {
                                        index: 1,
                                        fields: vec![],
                                    },
                                },
                            ])),
                        }),

                        "Ordering" => Ok(Annotated {
                            title: Some("Ordering".to_string()),
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
                        }),

                        "Option" => {
                            let generic = Annotated::do_from_type(
                                args.get(0)
                                    .expect("Option types have always one generic argument"),
                                modules,
                                type_parameters,
                                definitions,
                            )?;

                            Ok(Annotated {
                                title: Some("Optional".to_string()),
                                description: None,
                                annotated: Schema::Data(Data::AnyOf(vec![
                                    Annotated {
                                        title: Some("Some".to_string()),
                                        description: Some("An optional value.".to_string()),
                                        annotated: Constructor {
                                            index: 0,
                                            fields: vec![generic.into()],
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
                            })
                        }

                        "List" => {
                            let generic = Annotated::do_from_type(
                                args.get(0)
                                    .expect("List types have always one generic argument"),
                                modules,
                                type_parameters,
                                definitions,
                            )?;

                            // NOTE: Lists of 2-tuples are treated as Maps. This is an oddity we inherit
                            // from the PlutusTx / LedgerApi Haskell codebase, which encodes some elements
                            // as such. We don't have a concept of language maps in Aiken, so we simply
                            // make all types abide by this convention.
                            let data = match definitions
                                .lookup(&generic)
                                .expect(
                                    "Generic type argument definition was registered just above.",
                                )
                                .clone()
                            {
                                Annotated {
                                    annotated: Schema::Data(Data::List(Items::Many(xs))),
                                    ..
                                } if xs.len() == 2 => {
                                    definitions.remove(&generic);
                                    Data::Map(
                                        Box::new(
                                            xs.first()
                                                .expect("length (== 2) checked in pattern clause")
                                                .to_owned(),
                                        ),
                                        Box::new(
                                            xs.last()
                                                .expect("length (== 2) checked in pattern clause")
                                                .to_owned(),
                                        ),
                                    )
                                }
                                _ => {
                                    // let inner = schema.clone().into_data(type_info)?.annotated;
                                    Data::List(Items::One(Box::new(generic)))
                                }
                            };

                            Ok(Schema::Data(data).into())
                        }

                        _ => Err(Error::new(ErrorContext::UnsupportedType, type_info)),
                    }
                })
            }

            Type::App {
                name, module, args, ..
            } => definitions.register(type_info, &type_parameters.clone(), |definitions| {
                let module = modules
                    .get(module)
                    .unwrap_or_else(|| panic!("unknown module '{module}'\n\n{modules:?}"));

                let data_type =
                    find_data_type(name, &module.ast.definitions).unwrap_or_else(|| {
                        panic!(
                            "unknown data-type for '{name:?}' \n\n{definitions:?}",
                            definitions = module.ast.definitions
                        )
                    });

                collect_type_parameters(type_parameters, &data_type.typed_parameters, args);

                let annotated = Schema::Data(
                    Data::from_data_type(&data_type, modules, type_parameters, definitions)
                        .map_err(|e| e.backtrack(type_info))?,
                );

                Ok(Annotated {
                    title: Some(data_type.name.clone()),
                    description: data_type.doc.clone().map(|s| s.trim().to_string()),
                    annotated,
                })
            }),
            Type::Tuple { elems } => {
                definitions.register(type_info, &type_parameters.clone(), |definitions| {
                    let elems = elems
                        .iter()
                        .map(|elem| {
                            Annotated::do_from_type(elem, modules, type_parameters, definitions)
                        })
                        .collect::<Result<Vec<_>, _>>()
                        .map_err(|e| e.backtrack(type_info))?;

                    Ok(Annotated {
                        title: Some("Tuple".to_owned()),
                        description: None,
                        annotated: Schema::Data(Data::List(Items::Many(elems))),
                    })
                })
            }
            Type::Var { tipo } => match tipo.borrow().deref() {
                TypeVar::Link { tipo } => {
                    Annotated::do_from_type(tipo, modules, type_parameters, definitions)
                }
                TypeVar::Generic { id } => {
                    let tipo = type_parameters
                        .get(id)
                        .ok_or_else(|| Error::new(ErrorContext::FreeTypeVariable, type_info))?
                        .clone();
                    Annotated::do_from_type(&tipo, modules, type_parameters, definitions)
                }
                TypeVar::Unbound { .. } => {
                    Err(Error::new(ErrorContext::UnboundTypeVariable, type_info))
                }
            },
            Type::Fn { .. } => unreachable!(),
        }
    }

    fn into_data(self, type_info: &Type) -> Result<Annotated<Data>, Error> {
        match self {
            Annotated {
                title,
                description,
                annotated: Schema::Data(data),
            } => Ok(Annotated {
                title,
                description,
                annotated: data,
            }),
            _ => Err(Error::new(ErrorContext::ExpectedData, type_info)),
        }
    }
}

impl Data {
    fn from_data_type(
        data_type: &DataType<Arc<Type>>,
        modules: &HashMap<String, CheckedModule>,
        type_parameters: &mut HashMap<u64, Arc<Type>>,
        definitions: &mut Definitions<Annotated<Schema>>,
    ) -> Result<Self, Error> {
        let mut variants = vec![];

        let len_constructors = data_type.constructors.len();
        for (index, constructor) in data_type.constructors.iter().enumerate() {
            let mut fields = vec![];

            let len_arguments = data_type.constructors.len();
            for field in constructor.arguments.iter() {
                let reference =
                    Annotated::do_from_type(&field.tipo, modules, type_parameters, definitions)?;

                // NOTE: Opaque data-types with a single variant and a single field are transparent, they
                // are erased completely at compilation time.
                if data_type.opaque && len_constructors == 1 && len_arguments == 1 {
                    let schema = definitions
                        .lookup(&reference)
                        .expect("Schema definition registered just above")
                        .clone();
                    definitions.remove(&reference);
                    return Ok(schema.into_data(&field.tipo)?.annotated);
                }

                fields.push(Annotated {
                    title: field.label.clone(),
                    description: field.doc.clone().map(|s| s.trim().to_string()),
                    annotated: reference,
                });
            }

            let variant = Annotated {
                title: Some(constructor.name.clone()),
                description: constructor.doc.clone().map(|s| s.trim().to_string()),
                annotated: Constructor { index, fields },
            };

            variants.push(variant);
        }

        Ok(Data::AnyOf(variants))
    }
}

fn collect_type_parameters<'a>(
    type_parameters: &'a mut HashMap<u64, Arc<Type>>,
    generics: &'a [Arc<Type>],
    applications: &'a [Arc<Type>],
) {
    for (index, generic) in generics.iter().enumerate() {
        match &**generic {
            Type::Var { tipo } => match *tipo.borrow() {
                TypeVar::Generic { id } => {
                    type_parameters.insert(
                        id,
                        applications
                            .get(index)
                            .unwrap_or_else(|| panic!("Couldn't find generic identifier ({id}) in applied types: {applications:?}"))
                            .to_owned()
                    );
                }
                _ => unreachable!(),
            },
            _ => unreachable!(),
        }
    }
}

fn find_data_type(name: &str, definitions: &[TypedDefinition]) -> Option<DataType<Arc<Type>>> {
    for def in definitions {
        match def {
            Definition::DataType(data_type) if name == data_type.name => {
                return Some(data_type.clone())
            }
            Definition::Fn { .. }
            | Definition::Validator { .. }
            | Definition::DataType { .. }
            | Definition::TypeAlias { .. }
            | Definition::Use { .. }
            | Definition::ModuleConstant { .. }
            | Definition::Test { .. } => continue,
        }
    }
    None
}

// Needed because of Blueprint's default, but actually never used.
impl Default for Annotated<Schema> {
    fn default() -> Self {
        Schema::Data(Data::Opaque).into()
    }
}

impl Serialize for Schema {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        match self {
            Schema::Unit => {
                let mut s = serializer.serialize_struct("Unit", 1)?;
                s.serialize_field("dataType", "#unit")?;
                s.end()
            }
            Schema::Boolean => {
                let mut s = serializer.serialize_struct("Boolean", 1)?;
                s.serialize_field("dataType", "#boolean")?;
                s.end()
            }
            Schema::Integer => {
                let mut s = serializer.serialize_struct("Integer", 1)?;
                s.serialize_field("dataType", "#integer")?;
                s.end()
            }
            Schema::Bytes => {
                let mut s = serializer.serialize_struct("Bytes", 1)?;
                s.serialize_field("dataType", "#bytes")?;
                s.end()
            }
            Schema::String => {
                let mut s = serializer.serialize_struct("String", 1)?;
                s.serialize_field("dataType", "#string")?;
                s.end()
            }
            Schema::Pair(left, right) => {
                let mut s = serializer.serialize_struct("Pair", 3)?;
                s.serialize_field("dataType", "#pair")?;
                s.serialize_field("left", &left)?;
                s.serialize_field("right", &right)?;
                s.end()
            }
            Schema::List(items) => {
                let mut s = serializer.serialize_struct("List", 2)?;
                s.serialize_field("dataType", "#list")?;
                s.serialize_field("items", &items)?;
                s.end()
            }
            Schema::Data(data) => data.serialize(serializer),
        }
    }
}

impl Serialize for Data {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        match self {
            Data::Opaque => {
                let s = serializer.serialize_struct("Opaque", 0)?;
                s.end()
            }
            Data::Integer => {
                let mut s = serializer.serialize_struct("Integer", 1)?;
                s.serialize_field("dataType", "integer")?;
                s.end()
            }
            Data::Bytes => {
                let mut s = serializer.serialize_struct("Bytes", 1)?;
                s.serialize_field("dataType", "bytes")?;
                s.end()
            }
            Data::List(Items::One(item)) => {
                let mut s = serializer.serialize_struct("List", 2)?;
                s.serialize_field("dataType", "list")?;
                s.serialize_field("items", &item)?;
                s.end()
            }
            Data::List(Items::Many(items)) => {
                let mut s = serializer.serialize_struct("List", 2)?;
                s.serialize_field("dataType", "list")?;
                s.serialize_field("items", &items)?;
                s.end()
            }
            Data::Map(keys, values) => {
                let mut s = serializer.serialize_struct("Map", 3)?;
                s.serialize_field("dataType", "map")?;
                s.serialize_field("keys", &keys)?;
                s.serialize_field("values", &values)?;
                s.end()
            }
            Data::AnyOf(constructors) => {
                let mut s = serializer.serialize_struct("AnyOf", 1)?;
                s.serialize_field("anyOf", &constructors)?;
                s.end()
            }
        }
    }
}

impl<'a> Deserialize<'a> for Data {
    fn deserialize<D: Deserializer<'a>>(deserializer: D) -> Result<Self, D::Error> {
        #[derive(serde::Deserialize)]
        #[serde(field_identifier, rename_all = "camelCase")]
        enum Field {
            DataType,
            Items,
            Keys,
            Values,
            AnyOf,
            OneOf,
        }
        const FIELDS: &[&str] = &["dataType", "items", "keys", "values", "anyOf", "oneOf"];

        struct DataVisitor;

        impl<'a> Visitor<'a> for DataVisitor {
            type Value = Data;

            fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                formatter.write_str("Data")
            }

            fn visit_map<V>(self, mut map: V) -> Result<Data, V::Error>
            where
                V: MapAccess<'a>,
            {
                let mut data_type: Option<String> = None;
                let mut items: Option<Items<Reference>> = None;
                let mut keys = None;
                let mut values = None;
                let mut any_of = None;

                while let Some(key) = map.next_key()? {
                    match key {
                        Field::DataType => {
                            if data_type.is_some() {
                                return Err(de::Error::duplicate_field(FIELDS[0]));
                            }
                            data_type = Some(map.next_value()?);
                        }
                        Field::Items => {
                            if items.is_some() {
                                return Err(de::Error::duplicate_field(FIELDS[1]));
                            }
                            items = Some(map.next_value()?);
                        }
                        Field::Keys => {
                            if keys.is_some() {
                                return Err(de::Error::duplicate_field(FIELDS[2]));
                            }
                            keys = Some(map.next_value()?);
                        }
                        Field::Values => {
                            if values.is_some() {
                                return Err(de::Error::duplicate_field(FIELDS[3]));
                            }
                            values = Some(map.next_value()?);
                        }
                        Field::AnyOf => {
                            if any_of.is_some() {
                                return Err(de::Error::duplicate_field(FIELDS[4]));
                            }
                            any_of = Some(map.next_value()?);
                        }
                        Field::OneOf => {
                            if any_of.is_some() {
                                return Err(de::Error::duplicate_field(FIELDS[5]));
                            }
                            any_of = Some(map.next_value()?);
                        }
                    }
                }

                let expect_no_items = || {
                    if items.is_some() {
                        return Err(de::Error::custom(
                            "unexpected fields 'items' for non-list data-type",
                        ));
                    }
                    Ok(())
                };

                let expect_no_keys = || {
                    if keys.is_some() {
                        return Err(de::Error::custom(
                            "unexpected fields 'keys' for non-map data-type",
                        ));
                    }
                    Ok(())
                };

                let expect_no_values = || {
                    if values.is_some() {
                        return Err(de::Error::custom(
                            "unexpected fields 'values' for non-map data-type",
                        ));
                    }
                    Ok(())
                };

                let expect_no_any_of = || {
                    if any_of.is_some() {
                        return Err(de::Error::custom(
                            "unexpected fields 'anyOf' or 'oneOf'; applicators must singletons",
                        ));
                    }
                    Ok(())
                };

                match data_type {
                    None => {
                        expect_no_items()?;
                        expect_no_keys()?;
                        expect_no_values()?;
                        match any_of {
                            None => Ok(Data::Opaque),
                            Some(constructors) => Ok(Data::AnyOf(constructors)),
                        }
                    }
                    Some(data_type) if data_type == "integer" => {
                        expect_no_items()?;
                        expect_no_keys()?;
                        expect_no_values()?;
                        expect_no_any_of()?;
                        Ok(Data::Integer)
                    }
                    Some(data_type) if data_type == "bytes" => {
                        expect_no_items()?;
                        expect_no_keys()?;
                        expect_no_values()?;
                        expect_no_any_of()?;
                        Ok(Data::Bytes)
                    }
                    Some(data_type) if data_type == "list" => {
                        expect_no_keys()?;
                        expect_no_values()?;
                        expect_no_any_of()?;
                        match items {
                            None => Err(de::Error::missing_field(FIELDS[1])),
                            Some(items) => Ok(Data::List(items)),
                        }
                    }
                    Some(data_type) if data_type == "map" => {
                        expect_no_items()?;
                        expect_no_any_of()?;
                        match (keys, values) {
                            (Some(keys), Some(values)) => {
                                Ok(Data::Map(Box::new(keys), Box::new(values)))
                            }
                            (None, _) => Err(de::Error::missing_field(FIELDS[2])),
                            (Some(..), None) => Err(de::Error::missing_field(FIELDS[3])),
                        }
                    }
                    Some(..) => Err(de::Error::custom("unknown data-type")),
                }
            }
        }

        deserializer.deserialize_struct("Data", FIELDS, DataVisitor)
    }
}

impl Serialize for Constructor {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        let mut s = serializer.serialize_struct("Constructor", 3)?;
        s.serialize_field("dataType", "constructor")?;
        s.serialize_field("index", &self.index)?;
        s.serialize_field("fields", &self.fields)?;
        s.end()
    }
}

impl<'a> Deserialize<'a> for Constructor {
    fn deserialize<D: Deserializer<'a>>(deserializer: D) -> Result<Self, D::Error> {
        #[derive(serde::Deserialize)]
        #[serde(field_identifier, rename_all = "camelCase")]
        enum Field {
            Index,
            Fields,
        }
        const FIELDS: &[&str] = &["index", "fields"];

        struct ConstructorVisitor;

        impl<'a> Visitor<'a> for ConstructorVisitor {
            type Value = Constructor;

            fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                formatter.write_str("Constructor")
            }

            fn visit_map<V>(self, mut map: V) -> Result<Constructor, V::Error>
            where
                V: MapAccess<'a>,
            {
                let mut index = None;
                let mut fields = None;

                while let Some(key) = map.next_key()? {
                    match key {
                        Field::Index => {
                            if index.is_some() {
                                return Err(de::Error::duplicate_field(FIELDS[0]));
                            }
                            index = Some(map.next_value()?);
                        }
                        Field::Fields => {
                            if fields.is_some() {
                                return Err(de::Error::duplicate_field(FIELDS[1]));
                            }
                            fields = Some(map.next_value()?);
                        }
                    }
                }

                Ok(Constructor {
                    index: index.ok_or_else(|| de::Error::missing_field(FIELDS[0]))?,
                    fields: fields.ok_or_else(|| de::Error::missing_field(FIELDS[1]))?,
                })
            }
        }

        deserializer.deserialize_struct("Constructor", FIELDS, ConstructorVisitor)
    }
}

#[derive(Debug, PartialEq, Clone, thiserror::Error)]
#[error("{}", context)]
pub struct Error {
    context: ErrorContext,
    breadcrumbs: Vec<Type>,
}

#[derive(Debug, PartialEq, Clone, thiserror::Error)]
pub enum ErrorContext {
    #[error("I failed at my own job and couldn't figure out how to generate a specification for a type.")]
    UnsupportedType,

    #[error("I discovered a type hole where I would expect a concrete type.")]
    UnboundTypeVariable,

    #[error("I caught a free variable in the contract's interface boundary.")]
    FreeTypeVariable,

    #[error("I had the misfortune to find an invalid type in an interface boundary.")]
    ExpectedData,

    #[error("I figured you tried to export a function in your contract's binary interface.")]
    UnexpectedFunction,
}

impl Error {
    pub fn new(context: ErrorContext, type_info: &Type) -> Self {
        Error {
            context,
            breadcrumbs: vec![type_info.clone()],
        }
    }

    pub fn backtrack(self, type_info: &Type) -> Self {
        let mut breadcrumbs = vec![type_info.clone()];
        breadcrumbs.extend(self.breadcrumbs);
        Error {
            context: self.context,
            breadcrumbs,
        }
    }

    pub fn help(&self) -> String {
        match self.context {
            ErrorContext::UnsupportedType => format!(
                r#"I do not know how to generate a portable Plutus specification for the following type:

╰─▶ {signature}

This is likely a bug. I should know. May you be kind enough and report this on <https://github.com/aiken-lang/aiken>."#,
                signature = Error::fmt_breadcrumbs(&[self
                    .breadcrumbs
                    .last()
                    .expect("always at least one breadcrumb")
                    .to_owned()]),
            ),

            ErrorContext::FreeTypeVariable => format!(
                r#"There can't be any free type variable at the contract's boundary (i.e. in types used as datum and/or redeemer). Indeed, the validator can only be invoked with (very) concrete types. Since there's no reflexion possible inside a validator, it simply isn't possible to have any remaining free type variable in any of the datum or redeemer.

I got there when trying to generate a blueprint specification of the following type:

╰─▶ {breadcrumbs}"#,
                breadcrumbs = Error::fmt_breadcrumbs(&self.breadcrumbs)
            ),

            ErrorContext::UnboundTypeVariable => format!(
                r#"There cannot be any unbound type variable at the contract's boundary (i.e. in types used as datum and/or redeemer). Indeed, in order to generate an outward-facing specification of the contract's interface, I need to know what concrete representations will the datum and/or the redeemer have.

If your contract doesn't need datum or redeemer, you can always give them the type {type_Void} to indicate this. It is very concrete and will help me progress forward."#,
                type_Void = "Void"
                    .if_supports_color(Stdout, |s| s.bright_blue())
                    .if_supports_color(Stdout, |s| s.bold())
            ),

            ErrorContext::ExpectedData => format!(
                r#"While figuring out the outward-facing specification for your contract, I found a type that cannot actually be represented as valid Untyped Plutus Core (the low-level language Cardano uses to execute smart-contracts. For example, it isn't possible to have a list or a tuple of {type_String} because the underlying execution engine doesn't allow it.

There are few restrictions like this one. In this instance, here's the types I followed and that led me to this problem:

╰─▶ {breadcrumbs}"#,
                type_String = "String"
                    .if_supports_color(Stdout, |s| s.bright_blue())
                    .if_supports_color(Stdout, |s| s.bold()),
                breadcrumbs = Error::fmt_breadcrumbs(&self.breadcrumbs)
            ),

            ErrorContext::UnexpectedFunction => format!(
                r#"I can't allow that. Functions aren't serializable as data on-chain and thus cannot be used within your datum and/or redeemer types.

Here's the types I followed and that led me to this problem:

╰─▶ {breadcrumbs}"#,
                breadcrumbs = Error::fmt_breadcrumbs(&self.breadcrumbs)
            ),
        }
    }

    fn fmt_breadcrumbs(breadcrumbs: &[Type]) -> String {
        breadcrumbs
            .iter()
            .map(|type_info| {
                pretty::Printer::new()
                    .print(type_info)
                    .to_pretty_string(70)
                    .if_supports_color(Stdout, |s| s.bright_blue())
                    .if_supports_color(Stdout, |s| s.bold())
                    .to_string()
            })
            .collect::<Vec<_>>()
            .join(" → ")
    }
}

#[cfg(test)]
pub mod test {
    use super::*;
    use proptest::prelude::*;
    use serde_json::{self, json, Value};

    pub fn assert_json(schema: &impl Serialize, expected: Value) {
        assert_eq!(serde_json::to_value(schema).unwrap(), expected);
    }

    #[test]
    fn serialize_data_integer() {
        let schema = Schema::Data(Data::Integer);
        assert_json(
            &schema,
            json!({
                "dataType": "integer"
            }),
        );
    }

    #[test]
    fn serialize_data_bytes() {
        let schema = Schema::Data(Data::Bytes);
        assert_json(
            &schema,
            json!({
                "dataType": "bytes"
            }),
        );
    }

    #[test]
    fn serialize_data_list_1() {
        let ref_integer = Reference::new("Int");
        let schema = Schema::Data(Data::List(Items::One(Box::new(ref_integer))));
        assert_json(
            &schema,
            json!({
                "dataType": "list",
                "items": {
                    "$ref": "#/definitions/Int"
                }
            }),
        );
    }

    #[test]
    fn serialize_data_list_2() {
        let ref_list_integer = Reference::new("List$Int");
        let schema = Schema::Data(Data::List(Items::One(Box::new(ref_list_integer))));
        assert_json(
            &schema,
            json!({
                "dataType": "list",
                "items": {
                    "$ref": "#/definitions/List$Int"
                }
            }),
        );
    }

    #[test]
    fn serialize_data_map_1() {
        let ref_integer = Reference::new("Int");
        let ref_bytes = Reference::new("ByteArray");
        let schema = Schema::Data(Data::Map(Box::new(ref_integer), Box::new(ref_bytes)));
        assert_json(
            &schema,
            json!({
                "dataType": "map",
                "keys": {
                    "$ref": "#/definitions/Int"
                },
                "values": {
                    "$ref": "#/definitions/ByteArray"
                }
            }),
        )
    }

    #[test]
    fn serialize_data_constr_1() {
        let schema = Schema::Data(Data::AnyOf(vec![Constructor {
            index: 0,
            fields: vec![],
        }
        .into()]));
        assert_json(
            &schema,
            json!({
                "anyOf": [{
                    "dataType": "constructor",
                    "index": 0,
                    "fields": []
                }]
            }),
        )
    }

    #[test]
    fn serialize_data_constr_2() {
        let schema = Schema::Data(Data::AnyOf(vec![
            Constructor {
                index: 0,
                fields: vec![Reference::new("Int").into()],
            }
            .into(),
            Constructor {
                index: 1,
                fields: vec![Reference::new("Bytes").into()],
            }
            .into(),
        ]));
        assert_json(
            &schema,
            json!({
                "anyOf": [
                    {
                        "dataType": "constructor",
                        "index": 0,
                        "fields": [{ "$ref": "#/definitions/Int" }]
                    },
                    {
                        "dataType": "constructor",
                        "index": 1,
                        "fields": [{ "$ref": "#/definitions/Bytes" }]
                    }
                ]
            }),
        )
    }

    #[test]
    fn serialize_empty_data() {
        let schema = Schema::Data(Data::Opaque);
        assert_json(&schema, json!({}))
    }

    #[test]
    fn serialize_annotated_1() {
        let schema = Annotated {
            title: Some("foo".to_string()),
            description: None,
            annotated: Schema::Integer,
        };
        assert_json(
            &schema,
            json!({
                "title": "foo",
                "dataType": "#integer"
            }),
        )
    }

    #[test]
    fn serialize_annotated_2() {
        let schema = Annotated {
            title: Some("foo".to_string()),
            description: Some("Lorem Ipsum".to_string()),
            annotated: Schema::String,
        };
        assert_json(
            &schema,
            json!({
                "title": "foo",
                "description": "Lorem Ipsum",
                "dataType": "#string"
            }),
        )
    }

    #[test]
    fn deserialize_data_opaque() {
        assert_eq!(Data::Opaque, serde_json::from_value(json!({})).unwrap())
    }

    #[test]
    fn deserialize_data_integer() {
        assert_eq!(
            Data::Integer,
            serde_json::from_value(json!({
                "dataType": "integer",
            }))
            .unwrap()
        )
    }

    #[test]
    fn deserialize_data_bytes() {
        assert_eq!(
            Data::Bytes,
            serde_json::from_value(json!({
                "dataType": "bytes",
            }))
            .unwrap()
        )
    }

    #[test]
    fn deserialize_data_list_one() {
        assert_eq!(
            Data::List(Items::One(Box::new(Reference::new("foo")))),
            serde_json::from_value(json!({
                "dataType": "list",
                "items": { "$ref": "foo" }
            }))
            .unwrap()
        )
    }

    #[test]
    fn deserialize_data_list_many() {
        assert_eq!(
            Data::List(Items::Many(vec![
                Reference::new("foo"),
                Reference::new("bar")
            ])),
            serde_json::from_value(json!({
                "dataType": "list",
                "items": [
                  { "$ref": "foo" },
                  { "$ref": "bar" }
                ],
            }))
            .unwrap()
        )
    }

    #[test]
    fn deserialize_data_map() {
        assert_eq!(
            Data::Map(
                Box::new(Reference::new("foo")),
                Box::new(Reference::new("bar"))
            ),
            serde_json::from_value(json!({
                "dataType": "map",
                "keys": { "$ref": "foo" },
                "values": { "$ref": "bar" }
            }))
            .unwrap()
        )
    }

    #[test]
    fn deserialize_any_of() {
        assert_eq!(
            Data::AnyOf(vec![Constructor {
                index: 0,
                fields: vec![Reference::new("foo").into(), Reference::new("bar").into()],
            }
            .into()]),
            serde_json::from_value(json!({
                "anyOf": [{
                    "index": 0,
                    "fields": [
                        {
                            "$ref": "foo",
                        },
                        {
                            "$ref": "bar",
                        }
                    ]
                }]
            }))
            .unwrap()
        )
    }

    #[test]
    fn deserialize_one_of() {
        assert_eq!(
            Data::AnyOf(vec![Constructor {
                index: 0,
                fields: vec![Reference::new("foo").into(), Reference::new("bar").into()],
            }
            .into()]),
            serde_json::from_value(json!({
                "oneOf": [{
                    "index": 0,
                    "fields": [
                        {
                            "$ref": "foo",
                        },
                        {
                            "$ref": "bar",
                        }
                    ]
                }]
            }))
            .unwrap()
        )
    }

    fn arbitrary_data() -> impl Strategy<Value = Data> {
        let r = prop_oneof![".*".prop_map(|s| Reference::new(&s))];
        let constructor =
            (0..3usize, prop::collection::vec(r.clone(), 0..3)).prop_map(|(index, fields)| {
                Constructor {
                    index,
                    fields: fields.into_iter().map(|f| f.into()).collect(),
                }
                .into()
            });
        prop_oneof![
            Just(Data::Opaque),
            Just(Data::Bytes),
            Just(Data::Integer),
            (r.clone(), r.clone()).prop_map(|(k, v)| Data::Map(Box::new(k), Box::new(v))),
            r.clone().prop_map(|x| Data::List(Items::One(Box::new(x)))),
            prop::collection::vec(r, 1..3).prop_map(|xs| Data::List(Items::Many(xs))),
            prop::collection::vec(constructor, 1..3).prop_map(Data::AnyOf)
        ]
    }

    proptest! {
        #[test]
        fn data_serialization_roundtrip(data in arbitrary_data()) {
            let json = serde_json::to_value(data);
            let pretty = json
                    .as_ref()
                    .map(|v| serde_json::to_string_pretty(v).unwrap())
                    .unwrap_or_else(|_| "invalid".to_string());
            assert!(
                matches!(
                    json.and_then(serde_json::from_value::<Data>),
                    Ok{..}
                ),
                "\ncounterexample: {pretty}\n",
            )
        }
    }
}
