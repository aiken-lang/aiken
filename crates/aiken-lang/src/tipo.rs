use self::{environment::Environment, pretty::Printer};
use crate::{
    ast::{Constant, DefinitionLocation, ModuleKind, Span},
    tipo::fields::FieldMap,
};
use std::{cell::RefCell, collections::HashMap, ops::Deref, rc::Rc};
use uplc::{ast::Type as UplcType, builtins::DefaultFunction};

mod environment;
pub mod error;
mod exhaustive;
mod expr;
pub mod fields;
mod hydrator;
mod infer;
mod pattern;
mod pipe;
pub mod pretty;

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
        module: String,
        name: String,
        args: Vec<Rc<Type>>,
    },

    /// The type of a function. It takes arguments and returns a value.
    ///
    Fn {
        args: Vec<Rc<Type>>,
        ret: Rc<Type>,
    },

    /// A type variable. See the contained `TypeVar` enum for more information.
    ///
    Var {
        tipo: Rc<RefCell<TypeVar>>,
    },
    // /// A tuple is an ordered collection of 0 or more values, each of which
    // /// can have a different type, so the `tuple` type is the sum of all the
    // /// contained types.
    // ///
    Tuple {
        elems: Vec<Rc<Type>>,
    },
}

impl Type {
    pub fn is_result_constructor(&self) -> bool {
        match self {
            Type::Fn { ret, .. } => ret.is_result(),
            _ => false,
        }
    }

    pub fn is_result(&self) -> bool {
        matches!(self, Self::App { name, module, .. } if "Result" == name && module.is_empty())
    }

    pub fn is_unbound(&self) -> bool {
        matches!(self, Self::Var { tipo } if tipo.borrow().is_unbound())
    }

    pub fn is_function(&self) -> bool {
        matches!(self, Self::Fn { .. })
    }

    pub fn return_type(&self) -> Option<Rc<Self>> {
        match self {
            Self::Fn { ret, .. } => Some(ret.clone()),
            _ => None,
        }
    }

    pub fn function_types(&self) -> Option<(Vec<Rc<Self>>, Rc<Self>)> {
        match self {
            Self::Fn { args, ret, .. } => Some((args.clone(), ret.clone())),
            _ => None,
        }
    }

    pub fn is_primitive(&self) -> bool {
        self.is_bool()
            || self.is_bytearray()
            || self.is_int()
            || self.is_string()
            || self.is_void()
            || self.is_data()
    }

    pub fn is_void(&self) -> bool {
        match self {
            Self::App { module, name, .. } if "Void" == name && module.is_empty() => true,
            Self::Var { tipo } => tipo.borrow().is_void(),
            _ => false,
        }
    }

    pub fn is_bool(&self) -> bool {
        match self {
            Self::App { module, name, .. } if "Bool" == name && module.is_empty() => true,
            Self::Var { tipo } => tipo.borrow().is_bool(),
            _ => false,
        }
    }

    pub fn is_int(&self) -> bool {
        match self {
            Self::App { module, name, .. } if "Int" == name && module.is_empty() => true,
            Self::Var { tipo } => tipo.borrow().is_int(),
            _ => false,
        }
    }

    pub fn is_bytearray(&self) -> bool {
        match self {
            Self::App { module, name, .. } if "ByteArray" == name && module.is_empty() => true,
            Self::Var { tipo } => tipo.borrow().is_bytearray(),
            _ => false,
        }
    }

    pub fn is_string(&self) -> bool {
        match self {
            Self::App { module, name, .. } if "String" == name && module.is_empty() => true,
            Self::Var { tipo } => tipo.borrow().is_string(),
            _ => false,
        }
    }

    pub fn is_list(&self) -> bool {
        match self {
            Self::App { module, name, .. } if "List" == name && module.is_empty() => true,
            Self::Var { tipo } => tipo.borrow().is_list(),
            _ => false,
        }
    }

    pub fn is_option(&self) -> bool {
        match self {
            Self::App { module, name, .. } if "Option" == name && module.is_empty() => true,
            Self::Var { tipo } => tipo.borrow().is_option(),
            _ => false,
        }
    }

    pub fn is_map(&self) -> bool {
        match self {
            Self::App {
                module, name, args, ..
            } if "List" == name && module.is_empty() => {
                if let Type::Tuple { elems } = &*args[0] {
                    elems.len() == 2
                } else if let Type::Var { tipo } = &*args[0] {
                    matches!(tipo.borrow().get_uplc_type(), Some(UplcType::Pair(_, _)))
                } else {
                    false
                }
            }
            Self::Var { tipo } => tipo.borrow().is_map(),
            _ => false,
        }
    }

    pub fn is_tuple(&self) -> bool {
        match self {
            Type::Var { tipo } => tipo.borrow().is_tuple(),
            Type::Tuple { .. } => true,
            _ => false,
        }
    }

    pub fn is_2_tuple(&self) -> bool {
        match self {
            Type::Var { tipo } => tipo.borrow().is_2_tuple(),
            Type::Tuple { elems } => elems.len() == 2,
            _ => false,
        }
    }

    pub fn is_data(&self) -> bool {
        match self {
            Self::App { module, name, .. } => "Data" == name && module.is_empty(),
            Self::Var { tipo } => tipo.borrow().is_data(),
            _ => false,
        }
    }

    pub fn is_generic(&self) -> bool {
        match self {
            Type::App { args, .. } => {
                let mut is_a_generic = false;
                for arg in args {
                    is_a_generic = is_a_generic || arg.is_generic();
                }
                is_a_generic
            }

            Type::Var { tipo } => tipo.borrow().is_generic(),
            Type::Tuple { elems } => {
                let mut is_a_generic = false;
                for elem in elems {
                    is_a_generic = is_a_generic || elem.is_generic();
                }
                is_a_generic
            }
            Type::Fn { args, ret } => {
                let mut is_a_generic = false;
                for arg in args {
                    is_a_generic = is_a_generic || arg.is_generic();
                }
                is_a_generic || ret.is_generic()
            }
        }
    }

    pub fn arg_types(&self) -> Option<Vec<Rc<Self>>> {
        match self {
            Self::Fn { args, .. } => Some(args.clone()),
            Self::App { args, .. } => Some(args.clone()),
            Self::Var { tipo } => tipo.borrow().arg_types(),
            _ => None,
        }
    }

    pub fn get_generic(&self) -> Option<u64> {
        match self {
            Type::Var { tipo } => tipo.borrow().get_generic(),
            _ => None,
        }
    }

    pub fn get_inner_types(&self) -> Vec<Rc<Type>> {
        if self.is_list() {
            match self {
                Self::App { args, .. } => args.clone(),
                Self::Var { tipo } => tipo.borrow().get_inner_types(),
                _ => vec![],
            }
        } else if self.is_tuple() {
            match self {
                Self::Tuple { elems } => elems.to_vec(),
                Self::Var { tipo } => tipo.borrow().get_inner_types(),
                _ => vec![],
            }
        } else if matches!(self.get_uplc_type(), UplcType::Data) {
            match self {
                Type::App { args, .. } => args.clone(),
                Type::Fn { args, ret } => {
                    let mut args = args.clone();
                    args.push(ret.clone());
                    args
                }
                Type::Var { tipo } => tipo.borrow().get_inner_types(),
                _ => unreachable!(),
            }
        } else {
            vec![]
        }
    }

    pub fn get_uplc_type(&self) -> UplcType {
        if self.is_int() {
            UplcType::Integer
        } else if self.is_bytearray() {
            UplcType::ByteString
        } else if self.is_string() {
            UplcType::String
        } else if self.is_bool() {
            UplcType::Bool
        } else if self.is_map() {
            UplcType::List(UplcType::Pair(UplcType::Data.into(), UplcType::Data.into()).into())
        } else if self.is_list() {
            UplcType::List(UplcType::Data.into())
        } else if self.is_tuple() {
            match self {
                Self::Tuple { elems } => {
                    if elems.len() == 2 {
                        UplcType::Pair(UplcType::Data.into(), UplcType::Data.into())
                    } else {
                        UplcType::List(UplcType::Data.into())
                    }
                }
                Self::Var { tipo } => tipo.borrow().get_uplc_type().unwrap(),
                _ => todo!(),
            }
        } else {
            UplcType::Data
        }
    }

    /// Get the args for the type if the type is a specific `Type::App`.
    /// Returns None if the type is not a `Type::App` or is an incorrect `Type:App`
    ///
    /// This function is currently only used for finding the `List` type.
    pub fn get_app_args(
        &self,
        public: bool,
        module: &str,
        name: &str,
        arity: usize,
        environment: &mut Environment<'_>,
    ) -> Option<Vec<Rc<Self>>> {
        match self {
            Self::App {
                module: m,
                name: n,
                args,
                ..
            } => {
                if module == m && name == n && args.len() == arity {
                    Some(args.clone())
                } else {
                    None
                }
            }

            Self::Var { tipo } => {
                let args: Vec<_> = match tipo.borrow().deref() {
                    TypeVar::Link { tipo } => {
                        return tipo.get_app_args(public, module, name, arity, environment);
                    }

                    TypeVar::Unbound { .. } => {
                        (0..arity).map(|_| environment.new_unbound_var()).collect()
                    }

                    TypeVar::Generic { .. } => return None,
                };

                // We are an unbound type variable! So convert us to a type link
                // to the desired type.
                *tipo.borrow_mut() = TypeVar::Link {
                    tipo: Rc::new(Self::App {
                        name: name.to_string(),
                        module: module.to_owned(),
                        args: args.clone(),
                        public,
                    }),
                };
                Some(args)
            }

            _ => None,
        }
    }

    pub fn find_private_type(&self) -> Option<Self> {
        match self {
            Self::App { public: false, .. } => Some(self.clone()),

            Self::App { args, .. } => args.iter().find_map(|t| t.find_private_type()),

            Self::Tuple { elems, .. } => elems.iter().find_map(|t| t.find_private_type()),
            Self::Fn { ret, args, .. } => ret
                .find_private_type()
                .or_else(|| args.iter().find_map(|t| t.find_private_type())),

            Self::Var { tipo, .. } => match tipo.borrow().deref() {
                TypeVar::Unbound { .. } => None,

                TypeVar::Generic { .. } => None,

                TypeVar::Link { tipo, .. } => tipo.find_private_type(),
            },
        }
    }

    pub fn fn_arity(&self) -> Option<usize> {
        match self {
            Self::Fn { args, .. } => Some(args.len()),
            _ => None,
        }
    }

    pub fn to_pretty(&self, indent: usize) -> String {
        Printer::new().pretty_print(self, indent)
    }

    pub fn to_pretty_with_names(&self, names: HashMap<u64, String>, indent: usize) -> String {
        let mut printer = Printer::new();

        printer.with_names(names);

        printer.pretty_print(self, indent)
    }
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
    Link { tipo: Rc<Type> },
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

impl TypeVar {
    pub fn is_unbound(&self) -> bool {
        matches!(self, Self::Unbound { .. })
    }

    pub fn is_void(&self) -> bool {
        match self {
            Self::Link { tipo } => tipo.is_void(),
            _ => false,
        }
    }

    pub fn is_bool(&self) -> bool {
        match self {
            Self::Link { tipo } => tipo.is_bool(),
            _ => false,
        }
    }

    pub fn is_int(&self) -> bool {
        match self {
            Self::Link { tipo } => tipo.is_int(),
            _ => false,
        }
    }

    pub fn is_bytearray(&self) -> bool {
        match self {
            Self::Link { tipo } => tipo.is_bytearray(),
            _ => false,
        }
    }

    pub fn is_string(&self) -> bool {
        match self {
            Self::Link { tipo } => tipo.is_string(),
            _ => false,
        }
    }

    pub fn is_list(&self) -> bool {
        match self {
            Self::Link { tipo } => tipo.is_list(),
            _ => false,
        }
    }

    pub fn is_option(&self) -> bool {
        match self {
            Self::Link { tipo } => tipo.is_option(),
            _ => false,
        }
    }

    pub fn is_map(&self) -> bool {
        match self {
            Self::Link { tipo } => tipo.is_map(),
            _ => false,
        }
    }

    pub fn is_tuple(&self) -> bool {
        match self {
            Self::Link { tipo } => tipo.is_tuple(),
            _ => false,
        }
    }

    pub fn is_2_tuple(&self) -> bool {
        match self {
            Self::Link { tipo } => tipo.is_2_tuple(),
            _ => false,
        }
    }

    pub fn is_data(&self) -> bool {
        match self {
            Self::Link { tipo } => tipo.is_data(),
            _ => false,
        }
    }

    pub fn is_generic(&self) -> bool {
        match self {
            TypeVar::Generic { .. } => true,
            TypeVar::Link { tipo } => tipo.is_generic(),
            _ => false,
        }
    }

    pub fn get_generic(&self) -> Option<u64> {
        match self {
            TypeVar::Generic { id } => Some(*id),
            TypeVar::Link { tipo } => tipo.get_generic(),
            _ => None,
        }
    }

    pub fn arg_types(&self) -> Option<Vec<Rc<Type>>> {
        match self {
            Self::Link { tipo } => tipo.arg_types(),
            _ => None,
        }
    }

    pub fn get_inner_types(&self) -> Vec<Rc<Type>> {
        match self {
            Self::Link { tipo } => tipo.get_inner_types(),
            Self::Unbound { .. } => vec![],
            var => {
                vec![Type::Var {
                    tipo: RefCell::new(var.clone()).into(),
                }
                .into()]
            }
        }
    }

    pub fn get_uplc_type(&self) -> Option<UplcType> {
        match self {
            Self::Link { tipo } => Some(tipo.get_uplc_type()),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ValueConstructor {
    pub public: bool,
    pub variant: ValueConstructorVariant,
    pub tipo: Rc<Type>,
}

impl ValueConstructor {
    pub fn public(tipo: Rc<Type>, variant: ValueConstructorVariant) -> ValueConstructor {
        ValueConstructor {
            public: true,
            variant,
            tipo,
        }
    }

    fn field_map(&self) -> Option<&FieldMap> {
        match &self.variant {
            ValueConstructorVariant::ModuleFn { field_map, .. }
            | ValueConstructorVariant::Record { field_map, .. } => field_map.as_ref(),
            _ => None,
        }
    }

    pub fn is_local_variable(&self) -> bool {
        self.variant.is_local_variable()
    }

    pub fn definition_location(&self) -> DefinitionLocation<'_> {
        match &self.variant {
            ValueConstructorVariant::Record {
                module, location, ..
            }
            | ValueConstructorVariant::ModuleConstant {
                location, module, ..
            } => DefinitionLocation {
                module: Some(module.as_str()),
                span: *location,
            },

            ValueConstructorVariant::ModuleFn { location, .. }
            | ValueConstructorVariant::LocalVariable { location } => DefinitionLocation {
                module: None,
                span: *location,
            },
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
        literal: Constant,
    },

    /// A function belonging to the module
    ModuleFn {
        name: String,
        field_map: Option<FieldMap>,
        module: String,
        arity: usize,
        location: Span,
        builtin: Option<DefaultFunction>,
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

impl ValueConstructorVariant {
    fn to_module_value_constructor(
        &self,
        tipo: Rc<Type>,
        module_name: &str,
        function_name: &str,
    ) -> ModuleValueConstructor {
        match self {
            Self::Record {
                name,
                arity,
                field_map,
                location,
                ..
            } => ModuleValueConstructor::Record {
                name: name.clone(),
                field_map: field_map.clone(),
                arity: *arity,
                tipo,
                location: *location,
            },

            // TODO: remove this clone with an rc clone
            Self::ModuleConstant {
                literal, location, ..
            } => ModuleValueConstructor::Constant {
                literal: literal.clone(),
                location: *location,
            },

            Self::LocalVariable { location, .. } => ModuleValueConstructor::Fn {
                name: function_name.to_string(),
                module: module_name.to_string(),
                location: *location,
            },

            Self::ModuleFn {
                name,
                module,
                location,
                ..
            } => ModuleValueConstructor::Fn {
                name: name.clone(),
                module: module.clone(),
                location: *location,
            },
        }
    }

    pub fn location(&self) -> Span {
        match self {
            ValueConstructorVariant::LocalVariable { location }
            | ValueConstructorVariant::ModuleConstant { location, .. }
            | ValueConstructorVariant::ModuleFn { location, .. }
            | ValueConstructorVariant::Record { location, .. } => *location,
        }
    }

    /// Returns `true` if the variant is [`LocalVariable`].
    pub fn is_local_variable(&self) -> bool {
        matches!(self, Self::LocalVariable { .. })
    }
}

#[derive(Debug, Clone)]
pub struct TypeInfo {
    pub name: String,
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
    pub location: Span,
    pub module: String,
    pub parameters: Vec<Rc<Type>>,
    pub tipo: Rc<Type>,
}

#[derive(Debug, Clone)]
pub struct AccessorsMap {
    pub public: bool,
    pub tipo: Rc<Type>,
    pub accessors: HashMap<String, RecordAccessor>,
}

#[derive(Debug, Clone)]
pub struct RecordAccessor {
    // TODO: smaller int. Doesn't need to be this big
    pub index: u64,
    pub label: String,
    pub tipo: Rc<Type>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PatternConstructor {
    Record {
        name: String,
        field_map: Option<FieldMap>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum ModuleValueConstructor {
    Record {
        name: String,
        arity: usize,
        tipo: Rc<Type>,
        field_map: Option<FieldMap>,
        location: Span,
    },

    Fn {
        location: Span,
        /// The name of the module and the function
        /// Typically this will be the module that this constructor belongs to
        /// and the name that was used for the function. However it could also
        /// point to some other module and function when this is an `external fn`.
        ///
        /// This function has module "themodule" and name "wibble"
        ///     pub fn wibble() { Void }
        ///
        /// This function has module "other" and name "whoop"
        ///     pub external fn wibble() -> Void =
        ///       "other" "whoop"
        ///
        module: String,
        name: String,
    },

    Constant {
        literal: Constant,
        location: Span,
    },
}

impl ModuleValueConstructor {
    pub fn location(&self) -> Span {
        match self {
            ModuleValueConstructor::Fn { location, .. }
            | ModuleValueConstructor::Record { location, .. }
            | ModuleValueConstructor::Constant { location, .. } => *location,
        }
    }
}
