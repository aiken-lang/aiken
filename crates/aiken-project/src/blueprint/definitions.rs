use crate::{
    Annotated, Schema,
    blueprint::{
        parameter::Parameter,
        schema::{Data, Declaration, Items},
    },
};
use aiken_lang::tipo::{Type, TypeAliasAnnotation, TypeVar, pretty::resolve_alias};
use itertools::Itertools;
use serde::{
    self,
    de::{self, Deserialize, Deserializer, MapAccess, Visitor},
    ser::{Serialize, SerializeStruct, Serializer},
};
use std::{
    collections::{BTreeMap, BTreeSet, HashMap},
    fmt::{self, Display},
    ops::Deref,
    rc::Rc,
};

// ---------- Definitions

/// A map of definitions meant to be optionally registered and looked up.
#[derive(Debug, PartialEq, Eq, Clone, Default, serde::Serialize, serde::Deserialize)]
pub struct Definitions<T> {
    #[serde(flatten, default)]
    inner: BTreeMap<String, Option<T>>,
}

impl<T> Definitions<T> {
    /// Constructs a new empty definitions set.
    pub fn new() -> Self {
        Definitions {
            inner: BTreeMap::new(),
        }
    }

    /// True when there's no known definitions.
    pub fn is_empty(&self) -> bool {
        self.inner.is_empty()
    }

    /// Retrieve a definition, if it exists; fail if not resolved
    pub fn lookup(&self, reference: &Reference) -> Option<&T> {
        self.inner
            .get(&reference.as_key())
            .map(|v| v
              .as_ref()
              .expect("All registered definitions are 'Some'. 'None' state is only transient during registration")
            )
    }

    /// Retrieve a definition, if it exists and is resolved.
    pub fn try_lookup(&self, reference: &Reference) -> Option<&T> {
        self.inner.get(&reference.as_key()).and_then(|v| v.as_ref())
    }

    /// Merge two set of definitions together. Prioritize callee.
    pub fn merge(&mut self, other: &mut Definitions<T>) {
        self.inner.append(&mut other.inner);
    }

    /// Erase a known definition. Does nothing if the reference is unknown.
    pub fn remove(&mut self, reference: &Reference) {
        self.inner.remove(&reference.as_key());
    }

    /// Insert a new definition
    pub fn insert(&mut self, reference: &Reference, schema: T) {
        self.inner.insert(reference.as_key(), Some(schema));
    }

    /// Register a new definition only if it doesn't exist. This uses a strategy of
    /// mark-and-insert such that recursive definitions are only built once.
    pub fn register<F, E>(
        &mut self,
        type_info: &Type,
        type_parameters: &HashMap<u64, Rc<Type>>,
        build_schema: F,
    ) -> Result<Reference, E>
    where
        F: FnOnce(&mut Self) -> Result<T, E>,
    {
        let reference = Reference::from_type(type_info, type_parameters);
        let key = reference.as_key();

        if !self.inner.contains_key(&key) {
            self.inner.insert(key.clone(), None);
            let schema = build_schema(self)?;
            self.inner.insert(key, Some(schema));
        }

        Ok(reference)
    }
}

impl Definitions<Annotated<Schema>> {
    /// Remove orphan definitions. Such definitions can exist due to List of pairs being
    /// transformed to Maps. As a consequence, we may generate temporary Pair definitions
    /// which needs to get cleaned up later.
    ///
    /// Initially, we would clean those Pair definitions right-away, but this would cause
    /// Pair definitions to be missing in some legit cases when the Pair is also used as a
    /// standalone type.
    pub fn prune_orphan_pairs(&mut self, parameters: Vec<&Parameter>) -> &mut Self {
        fn traverse_schema(
            src: Reference,
            schema: &Schema,
            usage: &mut BTreeMap<Reference, BTreeSet<Reference>>,
        ) {
            match schema {
                Schema::Unit
                | Schema::Boolean
                | Schema::Integer
                | Schema::Bytes
                | Schema::String => (),
                Schema::Pair(left, right) => {
                    mark(src.clone(), left, usage, traverse_schema);
                    mark(src, right, usage, traverse_schema);
                }
                Schema::List(Items::One(item)) => {
                    mark(src, item, usage, traverse_schema);
                }
                Schema::List(Items::Many(items)) => {
                    items.iter().for_each(|item| {
                        mark(src.clone(), item, usage, traverse_schema);
                    });
                }
                Schema::Data(data) => traverse_data(src, data, usage),
            }
        }

        fn traverse_data(
            src: Reference,
            data: &Data,
            usage: &mut BTreeMap<Reference, BTreeSet<Reference>>,
        ) {
            match data {
                Data::Opaque | Data::Integer | Data::Bytes => (),
                Data::List(Items::One(item)) => {
                    mark(src, item, usage, traverse_data);
                }
                Data::List(Items::Many(items)) => {
                    items.iter().for_each(|item| {
                        mark(src.clone(), item, usage, traverse_data);
                    });
                }
                Data::Map(keys, values) => {
                    mark(src.clone(), keys, usage, traverse_data);
                    mark(src, values, usage, traverse_data);
                }
                Data::AnyOf(items) => {
                    items.iter().for_each(|item| {
                        item.annotated.fields.iter().for_each(|field| {
                            mark(src.clone(), &field.annotated, usage, traverse_data);
                        })
                    });
                }
            }
        }

        /// A mutually recursive function which works with either traverse_data or traverse_schema;
        /// it is meant to peel the 'Declaration' and keep traversing if needed (when inline).
        fn mark<F, T>(
            src: Reference,
            declaration: &Declaration<T>,
            usage: &mut BTreeMap<Reference, BTreeSet<Reference>>,
            mut traverse: F,
        ) where
            F: FnMut(Reference, &T, &mut BTreeMap<Reference, BTreeSet<Reference>>),
        {
            match declaration {
                Declaration::Referenced(reference) => {
                    if let Some(dependencies) = usage.get_mut(reference) {
                        dependencies.insert(src);
                    }
                }
                Declaration::Inline(schema) => traverse(src, schema, usage),
            }
        }

        let mut usage: BTreeMap<Reference, BTreeSet<Reference>> = BTreeMap::new();

        // 1. List all Pairs definitions
        for (src, annotated) in self.inner.iter() {
            if let Some(schema) = annotated.as_ref().map(|entry| &entry.annotated) {
                if matches!(schema, Schema::Pair(_, _)) {
                    usage.insert(Reference::new(src), BTreeSet::new());
                }
            }
        }

        // 2. Mark those used in other definitions
        for (src, annotated) in self.inner.iter() {
            if let Some(schema) = annotated.as_ref().map(|entry| &entry.annotated) {
                traverse_schema(Reference::new(src), schema, &mut usage)
            }
        }

        // 3. Mark also pairs definitions used in parameters / datums / redeemers
        for (ix, param) in parameters.iter().enumerate() {
            mark(
                // NOTE: The name isn't important, so long as it doesn't clash with other typical
                // references. If a definition is used in either of the parameter, it'll appear in
                // its dependencies and won't ever be removed because parameters are considered
                // always necessary.
                Reference::new(&format!("__param^{ix}")),
                &param.schema,
                &mut usage,
                traverse_schema,
            );
        }

        // 4. Repeatedly remove pairs definitions that aren't used. We need doing this repeatedly
        //    because a Pair definition may only be used by an unused one; so as we prune the first
        //    unused definitions, new ones may become unused.
        let mut last_len = usage.len();
        loop {
            let mut unused = None;
            for (k, v) in usage.iter() {
                if v.is_empty() {
                    unused = Some(k.clone());
                }
            }

            if let Some(k) = unused {
                usage.remove(&k);
                self.inner.remove(k.as_key().as_str());
                for (_, v) in usage.iter_mut() {
                    v.remove(&k);
                }
            }

            if usage.len() == last_len {
                break;
            } else {
                last_len = usage.len();
            }
        }

        self
    }

    pub fn replace_pairs_with_data_lists(&mut self) {
        fn swap_declaration(declaration: &mut Declaration<Schema>) -> Declaration<Data> {
            match std::mem::replace(declaration, Declaration::Inline(Schema::Unit.into())) {
                Declaration::Referenced(reference) => Declaration::Referenced(reference),
                Declaration::Inline(mut inline) => {
                    schema_to_data(&mut inline);
                    if let Schema::Data(data) = *inline {
                        Declaration::Inline(data.into())
                    } else {
                        unreachable!()
                    }
                }
            }
        }

        fn schema_to_data(schema: &mut Schema) {
            let items = match schema {
                Schema::Data(_) => None,
                Schema::Pair(left, right) => {
                    let left = swap_declaration(left);
                    let right = swap_declaration(right);
                    Some(Items::Many(vec![left, right]))
                }
                Schema::List(Items::One(item)) => {
                    let item = swap_declaration(item);
                    Some(Items::One(item))
                }
                Schema::List(Items::Many(items)) => Some(Items::Many(
                    items.iter_mut().map(swap_declaration).collect(),
                )),
                Schema::Integer => {
                    *schema = Schema::Data(Data::Integer);
                    None
                }
                Schema::Bytes => {
                    *schema = Schema::Data(Data::Bytes);
                    None
                }
                Schema::String => {
                    *schema = Schema::Data(Data::Bytes);
                    None
                }
                Schema::Unit => {
                    *schema = Schema::void();
                    None
                }
                Schema::Boolean => {
                    *schema = Schema::bool();
                    None
                }
            };

            if let Some(items) = items {
                *schema = Schema::Data(Data::List(items));
            }
        }

        for (_, entry) in self.inner.iter_mut() {
            if let Some(annotated) = entry {
                schema_to_data(&mut annotated.annotated);
            }
        }
    }
}

// ---------- Reference

/// A URI pointer to an underlying data-type.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Default)]
pub struct Reference {
    inner: String,
}

impl Reference {
    /// Create a (possibly unsound) Reference from a string. This isn't the preferred way to create
    /// a reference. One should use: `into()` on a 'Type' instead.
    pub fn new(path: &str) -> Reference {
        Reference {
            inner: path.to_string(),
        }
    }

    /// Turn a reference into a key suitable for lookup.
    pub(crate) fn as_key(&self) -> String {
        self.inner.replace("~1", "/")
    }

    /// Turn a reference into a valid JSON pointer. Note that the JSON pointer specification
    /// indicates that '/' must be escaped as '~1' in pointer addresses (as they are otherwise
    /// treated as path delimiter in pointers paths).
    pub(crate) fn as_json_pointer(&self) -> String {
        format!("#/definitions/{}", self.as_key().replace('/', "~1"))
    }
}

impl Display for Reference {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str(&self.inner)
    }
}

impl Reference {
    pub fn from_type(type_info: &Type, type_parameters: &HashMap<u64, Rc<Type>>) -> Self {
        if let Some(TypeAliasAnnotation {
            alias,
            parameters,
            annotation,
            module,
        }) = type_info.alias().as_deref()
        {
            if let Some(resolved_parameters) = resolve_alias(parameters, annotation, type_info) {
                return Self::from_type_alias(
                    type_info,
                    alias,
                    module.as_deref(),
                    resolved_parameters,
                    type_parameters,
                );
            }
        }

        match type_info {
            Type::App {
                module, name, args, ..
            } => {
                let args: Self = Self::from_types(args, type_parameters);
                Self {
                    inner: if module.is_empty() {
                        format!("{name}{args}")
                    } else {
                        format!("{module}/{name}{args}")
                    },
                }
            }

            Type::Tuple { elems, .. } => Self {
                inner: format!(
                    "Tuple{elems}",
                    elems = Self::from_types(elems, type_parameters)
                ),
            },
            Type::Pair { fst, snd, .. } => Self {
                inner: format!(
                    "Pair${fst}_{snd}",
                    fst = Self::from_type(fst, type_parameters),
                    snd = Self::from_type(snd, type_parameters)
                ),
            },

            // NOTE:
            //
            // Implementations below are only there for completeness. In practice, we should never
            // end up creating references for 'Var' or 'Fn' in the context of blueprints.
            Type::Var { tipo, .. } => match tipo.borrow().deref() {
                TypeVar::Link { tipo } => Self::from_type(tipo.as_ref(), type_parameters),
                TypeVar::Generic { id } | TypeVar::Unbound { id } => {
                    let tipo = type_parameters.get(id).unwrap();
                    Self::from_type(tipo, type_parameters)
                }
            },

            Type::Fn { args, ret, .. } => Self {
                inner: format!(
                    "Fn{args}_{ret}",
                    args = Self::from_types(args, type_parameters),
                    ret = Self::from_type(ret, type_parameters)
                ),
            },
        }
    }

    fn from_types(args: &[Rc<Type>], type_parameters: &HashMap<u64, Rc<Type>>) -> Self {
        if args.is_empty() {
            Reference::new("")
        } else {
            Reference {
                inner: format!(
                    "${}",
                    args.iter()
                        .map(|s| Self::from_type(s.as_ref(), type_parameters).inner)
                        .collect::<Vec<_>>()
                        .join("_")
                ),
            }
        }
    }

    fn from_type_alias(
        type_info: &Type,
        alias: &str,
        module: Option<&str>,
        parameters: Vec<Rc<Type>>,
        type_parameters: &HashMap<u64, Rc<Type>>,
    ) -> Self {
        let prefix = match module {
            Some(module) => format!("{module}/{alias}"),
            None => alias.to_string(),
        };

        if !parameters.is_empty() {
            Reference {
                inner: format!(
                    "{prefix}${}",
                    parameters
                        .iter()
                        .map(|param| {
                            // Avoid infinite recursion for recursive types instantiated to
                            // themselves. For example: type Identity<t> = t
                            if param.as_ref() == type_info {
                                Self::from_type(
                                    type_info.clone().set_alias(None).as_ref(),
                                    type_parameters,
                                )
                                .inner
                            } else {
                                Self::from_type(param, type_parameters).inner
                            }
                        })
                        .join("_"),
                ),
            }
        } else {
            Reference { inner: prefix }
        }
    }
}

impl Serialize for Reference {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        let mut s = serializer.serialize_struct("$ref", 1)?;
        s.serialize_field("$ref", &self.as_json_pointer())?;
        s.end()
    }
}

impl<'a> Deserialize<'a> for Reference {
    fn deserialize<D: Deserializer<'a>>(deserializer: D) -> Result<Self, D::Error> {
        #[derive(serde::Deserialize)]
        enum Field {
            #[serde(rename = "$ref")]
            Ref,
        }
        const FIELDS: &[&str] = &["$ref"];

        struct ReferenceVisitor;

        impl<'a> Visitor<'a> for ReferenceVisitor {
            type Value = Reference;

            fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                formatter.write_str("Reference")
            }

            fn visit_map<V>(self, mut map: V) -> Result<Reference, V::Error>
            where
                V: MapAccess<'a>,
            {
                let mut inner = None;

                while let Some(key) = map.next_key()? {
                    match key {
                        Field::Ref => {
                            if inner.is_some() {
                                return Err(de::Error::duplicate_field(FIELDS[0]));
                            }
                            inner = Some(map.next_value()?);
                        }
                    }
                }

                let inner: String = inner.ok_or_else(|| de::Error::missing_field(FIELDS[0]))?;

                match inner.strip_prefix("#/definitions/") {
                    Some(suffix) => Ok(Reference {
                        inner: suffix.to_string(),
                    }),
                    None => Err(de::Error::custom(
                        "Invalid reference; only local JSON pointer to #/definitions are allowed.",
                    )),
                }
            }
        }

        deserializer.deserialize_struct("Reference", FIELDS, ReferenceVisitor)
    }
}
