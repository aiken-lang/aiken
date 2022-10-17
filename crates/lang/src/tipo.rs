use std::{cell::RefCell, collections::HashMap, sync::Arc};

use crate::ast::{Constant, FieldMap, ModuleKind, Span, TypedConstant};

pub mod error;
pub mod infer;

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    /// A nominal (named) type such as `Int`, `Float`, or a programmer defined
    /// custom type such as `Person`. The type can take other types as
    /// arguments (aka "generics" or "parametric polymorphism").
    ///
    /// If the type is defined in the Aiken prelude the `module` field will be
    /// empty, otherwise it will contain the name of the module that
    /// defines the type.
    ///
    App {
        public: bool,
        module: Vec<String>,
        name: String,
        args: Vec<Arc<Type>>,
    },

    /// The type of a function. It takes arguments and returns a value.
    ///
    Fn {
        args: Vec<Arc<Type>>,
        ret: Arc<Type>,
    },

    /// A type variable. See the contained `TypeVar` enum for more information.
    ///
    Var { tipo: Arc<RefCell<TypeVar>> },

    /// A tuple is an ordered collection of 0 or more values, each of which
    /// can have a different type, so the `tuple` type is the sum of all the
    /// contained types.
    ///
    Tuple { elems: Vec<Arc<Type>> },
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypeVar {
    /// Unbound is an unbound variable. It is one specific type but we don't
    /// know what yet in the inference process. It has a unique id which can be used to
    /// identify if two unbound variable Rust values are the same Aiken type variable
    /// instance or not.
    ///
    Unbound { id: u64 },
    /// Link is type variable where it was an unbound variable but we worked out
    /// that it is some other type and now we point to that one.
    ///
    Link { tipo: Arc<Type> },
    /// A Generic variable stands in for any possible type and cannot be
    /// specialised to any one type
    ///
    /// # Example
    ///
    /// ```aiken
    /// type Cat(a) {
    ///   Cat(name: a)
    /// }
    /// // a is TypeVar::Generic
    /// ```
    ///
    Generic { id: u64 },
}

#[derive(Debug, Clone, PartialEq)]
pub struct ValueConstructor {
    pub public: bool,
    pub variant: ValueConstructorVariant,
    pub tipo: Arc<Type>,
}

impl ValueConstructor {
    pub fn public(tipo: Arc<Type>, variant: ValueConstructorVariant) -> ValueConstructor {
        ValueConstructor {
            public: true,
            variant,
            tipo,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ValueConstructorVariant {
    /// A locally defined variable or function parameter
    LocalVariable { location: Span },

    /// A module constant
    ModuleConstant {
        location: Span,
        module: String,
        literal: Constant<Arc<Type>, String>,
    },

    /// A function belonging to the module
    ModuleFn {
        name: String,
        field_map: Option<FieldMap>,
        module: Vec<String>,
        arity: usize,
        location: Span,
    },

    /// A constructor for a custom type
    Record {
        name: String,
        arity: usize,
        field_map: Option<FieldMap>,
        location: Span,
        module: String,
        constructors_count: u16,
    },
}

#[derive(Debug, Clone)]
pub struct Module {
    pub name: Vec<String>,
    pub kind: ModuleKind,
    pub package: String,
    pub types: HashMap<String, TypeConstructor>,
    pub types_constructors: HashMap<String, Vec<String>>,
    pub values: HashMap<String, ValueConstructor>,
    pub accessors: HashMap<String, AccessorsMap>,
}

#[derive(Debug, Clone)]
pub struct TypeConstructor {
    pub public: bool,
    pub origin: Span,
    pub module: Vec<String>,
    pub parameters: Vec<Arc<Type>>,
    pub tipo: Arc<Type>,
}

#[derive(Debug, Clone)]
pub struct AccessorsMap {
    pub public: bool,
    pub tipo: Arc<Type>,
    pub accessors: HashMap<String, RecordAccessor>,
}

#[derive(Debug, Clone)]
pub struct RecordAccessor {
    // TODO: smaller int. Doesn't need to be this big
    pub index: u64,
    pub label: String,
    pub tipo: Arc<Type>,
}

#[derive(Debug)]
pub enum PatternConstructor {
    Record {
        name: String,
        field_map: Option<FieldMap>,
    },
}

#[derive(Debug)]
pub enum ModuleValueConstructor {
    Record {
        name: String,
        arity: usize,
        type_: Arc<Type>,
        field_map: Option<FieldMap>,
        location: Span,
    },

    Fn {
        location: Span,
    },

    Constant {
        literal: TypedConstant,
        location: Span,
    },
}
