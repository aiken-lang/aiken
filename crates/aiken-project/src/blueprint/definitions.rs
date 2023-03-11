use aiken_lang::tipo::{Type, TypeVar};
use serde::{
    self,
    ser::{Serialize, SerializeStruct, Serializer},
};
use std::{
    collections::{BTreeMap, HashMap},
    fmt::{self, Display},
    ops::Deref,
    sync::Arc,
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

    /// Retrieve a definition, if it exists.
    pub fn lookup(&self, reference: &Reference) -> Option<&T> {
        self.inner
            .get(reference.as_key())
            .map(|v| v
              .as_ref()
              .expect("All registered definitions are 'Some'. 'None' state is only transient during registration")
            )
    }

    /// Merge two set of definitions together. Prioritize callee.
    pub fn merge(&mut self, other: &mut Definitions<T>) {
        self.inner.append(&mut other.inner);
    }

    /// Erase a known definition. Does nothing if the reference is unknown.
    pub fn remove(&mut self, reference: &Reference) {
        self.inner.remove(reference.as_key());
    }

    /// Register a new definition only if it doesn't exist. This uses a strategy of
    /// mark-and-insert such that recursive definitions are only built once.
    pub fn register<F, E>(
        &mut self,
        type_info: &Type,
        type_parameters: &HashMap<u64, Arc<Type>>,
        build_schema: F,
    ) -> Result<Reference, E>
    where
        F: FnOnce(&mut Self) -> Result<T, E>,
    {
        let reference = Reference::from_type(type_info, type_parameters);
        let key = reference.as_key();

        if !self.inner.contains_key(key) {
            self.inner.insert(key.to_string(), None);
            let schema = build_schema(self)?;
            self.inner.insert(key.to_string(), Some(schema));
        }

        Ok(reference)
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
    fn as_key(&self) -> &str {
        self.inner.as_str()
    }

    /// Turn a reference into a valid JSON pointer. Note that the JSON pointer specification
    /// indicates that '/' must be escaped as '~1' in pointer addresses (as they are otherwise
    /// treated as path delimiter in pointers paths).
    fn as_json_pointer(&self) -> String {
        format!("#/definitions/{}", self.as_key().replace('/', "~1"))
    }
}

impl Display for Reference {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str(&self.inner)
    }
}

impl Reference {
    pub fn from_type(type_info: &Type, type_parameters: &HashMap<u64, Arc<Type>>) -> Self {
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

            Type::Tuple { elems } => Self {
                inner: format!(
                    "Tuple{elems}",
                    elems = Self::from_types(elems, type_parameters)
                ),
            },

            // NOTE:
            //
            // Implementations below are only there for completeness. In practice, we should never
            // end up creating references for 'Var' or 'Fn' in the context of blueprints.
            Type::Var { tipo } => match tipo.borrow().deref() {
                TypeVar::Link { tipo } => Self::from_type(tipo.as_ref(), type_parameters),
                TypeVar::Generic { id } | TypeVar::Unbound { id } => {
                    let tipo = type_parameters.get(id).unwrap();
                    Self::from_type(tipo, type_parameters)
                }
            },

            Type::Fn { args, ret } => Self {
                inner: format!(
                    "Fn{args}_{ret}",
                    args = Self::from_types(args, type_parameters),
                    ret = Self::from_type(ret, type_parameters)
                ),
            },
        }
    }

    fn from_types(args: &Vec<Arc<Type>>, type_parameters: &HashMap<u64, Arc<Type>>) -> Self {
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
}

impl Serialize for Reference {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        let mut s = serializer.serialize_struct("$ref", 1)?;
        s.serialize_field("$ref", &self.as_json_pointer())?;
        s.end()
    }
}
