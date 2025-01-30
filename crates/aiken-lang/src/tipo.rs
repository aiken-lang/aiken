use self::{environment::Environment, pretty::Printer};
use crate::{
    ast::{
        well_known, Annotation, DataType, DataTypeKey, DefinitionLocation, ModuleKind, Span,
        TypedDataType,
    },
    tipo::fields::FieldMap,
};
use indexmap::IndexMap;
use itertools::Itertools;
use std::{cell::RefCell, collections::HashMap, ops::Deref, rc::Rc};
use uplc::{ast::Type as UplcType, builtins::DefaultFunction};

pub(crate) mod environment;
pub mod error;
mod exhaustive;
pub(crate) mod expr;
pub mod fields;
mod hydrator;
mod infer;
mod pattern;
mod pipe;
pub mod pretty;

pub use environment::collapse_links;

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct TypeAliasAnnotation {
    pub alias: String,
    pub parameters: Vec<String>,
    pub annotation: Annotation,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
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
        contains_opaque: bool,
        module: String,
        name: String,
        args: Vec<Rc<Type>>,
        alias: Option<Rc<TypeAliasAnnotation>>,
    },

    /// The type of a function. It takes arguments and returns a value.
    ///
    Fn {
        args: Vec<Rc<Type>>,
        ret: Rc<Type>,
        alias: Option<Rc<TypeAliasAnnotation>>,
    },

    /// A type variable. See the contained `TypeVar` enum for more information.
    ///
    Var {
        tipo: Rc<RefCell<TypeVar>>,
        alias: Option<Rc<TypeAliasAnnotation>>,
    },
    // /// A tuple is an ordered collection of 0 or more values, each of which
    // /// can have a different type, so the `tuple` type is the sum of all the
    // /// contained types.
    // ///
    Tuple {
        elems: Vec<Rc<Type>>,
        alias: Option<Rc<TypeAliasAnnotation>>,
    },

    Pair {
        fst: Rc<Type>,
        snd: Rc<Type>,
        alias: Option<Rc<TypeAliasAnnotation>>,
    },
}

impl PartialEq for Type {
    fn eq(&self, other: &Type) -> bool {
        match self {
            Type::App {
                public,
                module,
                name,
                args,
                contains_opaque: _,
                alias: _,
            } => {
                if let Type::App {
                    public: public2,
                    module: module2,
                    name: name2,
                    args: args2,
                    contains_opaque: _,
                    alias: _,
                } = other
                {
                    name == name2
                        && module == module2
                        && public == public2
                        && args.len() == args2.len()
                        && args.iter().zip(args2).all(|(left, right)| left == right)
                } else {
                    false
                }
            }

            Type::Fn { args, ret, .. } => {
                if let Type::Fn {
                    args: args2,
                    ret: ret2,
                    alias: _,
                } = other
                {
                    ret == ret2
                        && args.len() == args2.len()
                        && args.iter().zip(args2).all(|(left, right)| left == right)
                } else {
                    false
                }
            }

            Type::Tuple { elems, alias: _ } => {
                if let Type::Tuple { elems: elems2, .. } = other {
                    elems.len() == elems2.len()
                        && elems.iter().zip(elems2).all(|(left, right)| left == right)
                } else {
                    false
                }
            }

            Type::Var { tipo, alias: _ } => {
                if let Type::Var {
                    tipo: tipo2,
                    alias: _,
                } = other
                {
                    tipo == tipo2
                } else {
                    false
                }
            }
            Type::Pair { fst, snd, .. } => {
                if let Type::Pair {
                    fst: fst2,
                    snd: snd2,
                    ..
                } = other
                {
                    fst == fst2 && snd == snd2
                } else {
                    false
                }
            }
        }
    }
}

impl Type {
    pub fn alias(&self) -> Option<Rc<TypeAliasAnnotation>> {
        match self {
            Type::App { alias, .. }
            | Type::Fn { alias, .. }
            | Type::Var { alias, .. }
            | Type::Tuple { alias, .. }
            | Type::Pair { alias, .. } => alias.clone(),
        }
    }

    pub fn with_alias(tipo: Rc<Type>, alias: Option<Rc<TypeAliasAnnotation>>) -> Rc<Type> {
        match alias {
            None => tipo,
            Some(alias) => tipo.deref().to_owned().set_alias(Some(alias)),
        }
    }

    pub fn set_alias(self, alias: Option<Rc<TypeAliasAnnotation>>) -> Rc<Type> {
        Rc::new(match self {
            Type::App {
                public,
                contains_opaque: opaque,
                module,
                name,
                args,
                alias: _,
            } => Type::App {
                public,
                contains_opaque: opaque,
                module,
                name,
                args,
                alias,
            },
            Type::Fn {
                args,
                ret,
                alias: _,
            } => Type::Fn { args, ret, alias },
            Type::Var { tipo, alias: _ } => Type::Var { tipo, alias },
            Type::Tuple { elems, alias: _ } => Type::Tuple { elems, alias },
            Type::Pair { fst, snd, alias: _ } => Type::Pair { fst, snd, alias },
        })
    }

    pub fn qualifier(&self) -> Option<(String, String)> {
        match self {
            Type::App { module, name, .. } => Some((module.to_string(), name.to_string())),
            Type::Fn { .. } => None,
            Type::Var { ref tipo, .. } => match &*tipo.borrow() {
                TypeVar::Link { ref tipo } => tipo.qualifier(),
                _ => None,
            },
            Type::Tuple { .. } => Some((String::new(), "Tuple".to_string())),
            Type::Pair { .. } => Some((String::new(), "Pair".to_string())),
        }
    }

    pub fn contains_opaque(&self) -> bool {
        match self {
            Type::Var { tipo, .. } => tipo.borrow().is_or_holds_opaque(),
            Type::App {
                contains_opaque: opaque,
                args,
                ..
            } => *opaque || args.iter().any(|arg| arg.contains_opaque()),
            Type::Tuple { elems, .. } => elems.iter().any(|elem| elem.contains_opaque()),
            Type::Fn { .. } => false,
            Type::Pair { fst, snd, .. } => fst.contains_opaque() || snd.contains_opaque(),
        }
    }

    pub fn set_opaque(&mut self, opaque: bool) {
        match self {
            Type::App {
                contains_opaque, ..
            } => {
                *contains_opaque = opaque;
            }
            Type::Fn { .. } | Type::Var { .. } | Type::Tuple { .. } | Type::Pair { .. } => (),
        }
    }

    pub fn is_unbound(&self) -> bool {
        matches!(self, Self::Var { tipo, .. } if tipo.borrow().is_unbound())
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
        let uplc_type = self.get_uplc_type();
        match uplc_type {
            Some(
                UplcType::Bool
                | UplcType::Integer
                | UplcType::String
                | UplcType::ByteString
                | UplcType::Unit
                | UplcType::Bls12_381G1Element
                | UplcType::Bls12_381G2Element
                | UplcType::Bls12_381MlResult
                | UplcType::Data,
            ) => true,

            None => false,
            Some(UplcType::List(_) | UplcType::Pair(_, _)) => false,
        }
    }

    pub fn is_void(&self) -> bool {
        match self {
            Self::App { module, name, .. } if "Void" == name && module.is_empty() => true,
            Self::Var { tipo, .. } => tipo.borrow().is_void(),
            _ => false,
        }
    }

    pub fn is_bool(&self) -> bool {
        match self {
            Self::App { module, name, .. } if "Bool" == name && module.is_empty() => true,
            Self::Var { tipo, .. } => tipo.borrow().is_bool(),
            _ => false,
        }
    }

    pub fn is_int(&self) -> bool {
        match self {
            Self::App { module, name, .. } if well_known::INT == name && module.is_empty() => true,
            Self::Var { tipo, .. } => tipo.borrow().is_int(),
            _ => false,
        }
    }

    pub fn is_bytearray(&self) -> bool {
        match self {
            Self::App { module, name, .. }
                if well_known::BYTE_ARRAY == name && module.is_empty() =>
            {
                true
            }
            Self::Var { tipo, .. } => tipo.borrow().is_bytearray(),
            _ => false,
        }
    }

    pub fn is_bls381_12_g1(&self) -> bool {
        match self {
            Self::App { module, name, .. } => well_known::G1_ELEMENT == name && module.is_empty(),

            Self::Var { tipo, .. } => tipo.borrow().is_bls381_12_g1(),
            _ => false,
        }
    }

    pub fn is_bls381_12_g2(&self) -> bool {
        match self {
            Self::App { module, name, .. } => well_known::G2_ELEMENT == name && module.is_empty(),

            Self::Var { tipo, .. } => tipo.borrow().is_bls381_12_g2(),
            _ => false,
        }
    }

    pub fn is_ml_result(&self) -> bool {
        match self {
            Self::App { module, name, .. } => {
                well_known::MILLER_LOOP_RESULT == name && module.is_empty()
            }

            Self::Var { tipo, .. } => tipo.borrow().is_ml_result(),
            _ => false,
        }
    }

    pub fn is_string(&self) -> bool {
        match self {
            Self::App { module, name, .. } if "String" == name && module.is_empty() => true,
            Self::Var { tipo, .. } => tipo.borrow().is_string(),
            _ => false,
        }
    }

    pub fn is_list(&self) -> bool {
        match self {
            Self::App { module, name, .. } if "List" == name && module.is_empty() => true,
            Self::Var { tipo, .. } => tipo.borrow().is_list(),
            _ => false,
        }
    }

    pub fn is_option(&self) -> bool {
        match self {
            Self::App { module, name, .. } if "Option" == name && module.is_empty() => true,
            Self::Var { tipo, .. } => tipo.borrow().is_option(),
            _ => false,
        }
    }

    pub fn is_map(&self) -> bool {
        match self {
            Self::App {
                module, name, args, ..
            } if "List" == name && module.is_empty() => args
                .first()
                .expect("unreachable: List should have an inner type")
                .is_pair(),
            Self::Var { tipo, .. } => tipo.borrow().is_map(),
            _ => false,
        }
    }

    pub fn is_tuple(&self) -> bool {
        match self {
            Self::Var { tipo, .. } => tipo.borrow().is_tuple(),
            Self::Tuple { .. } => true,
            _ => false,
        }
    }

    pub fn is_pair(&self) -> bool {
        match self {
            Self::Var { tipo, .. } => tipo.borrow().is_pair(),
            Self::Pair { .. } => true,
            _ => false,
        }
    }

    pub fn is_data(&self) -> bool {
        match self {
            Self::App { module, name, .. } => "Data" == name && module.is_empty(),
            Self::Var { tipo, .. } => tipo.borrow().is_data(),
            _ => false,
        }
    }

    ///  Check whether a given type is fully specialized and has only one possible
    ///  form. Said differently, this recursively checks if the type still contains
    ///  unbound or generic variables.
    pub fn is_monomorphic(&self) -> bool {
        match self {
            Self::App { args, .. } => args.iter().all(|arg| arg.is_monomorphic()),
            Self::Fn { args, ret, .. } => {
                args.iter().all(|arg| arg.is_monomorphic()) && ret.is_monomorphic()
            }
            Self::Tuple { elems, .. } => elems.iter().all(|arg| arg.is_monomorphic()),
            Self::Pair { fst, snd, .. } => [fst, snd].iter().all(|arg| arg.is_monomorphic()),
            Self::Var { tipo, .. } => tipo.borrow().is_monomorphic(),
        }
    }

    pub fn is_generic(&self) -> bool {
        !self.collect_generics().is_empty()
    }

    pub fn collect_generics(&self) -> Vec<Rc<Type>> {
        match self {
            Self::App { args, .. } => args.iter().flat_map(|arg| arg.collect_generics()).collect(),
            Self::Var { tipo, .. } => {
                if tipo.borrow().is_generic() {
                    vec![self.clone().into()]
                } else {
                    Vec::new()
                }
            }
            Self::Tuple { elems, .. } => elems
                .iter()
                .flat_map(|arg| arg.collect_generics())
                .collect(),
            Self::Fn { args, ret, .. } => args
                .iter()
                .chain(std::iter::once(ret))
                .flat_map(|arg| arg.collect_generics())
                .collect(),
            Self::Pair { fst, snd, .. } => {
                let mut generics = fst.collect_generics();
                generics.extend(snd.collect_generics());
                generics
            }
        }
    }

    // TODO: Self::App { args, ..} looks fishy, because App's args are referring
    // to _type parameters_ not to value types unlike Fn's args. So this function
    // definition is probably wrong. Luckily, we likely never hit the `Self::App`
    // case at all.
    pub fn arg_types(&self) -> Option<Vec<Rc<Self>>> {
        match self {
            Self::Fn { args, .. } => Some(args.clone()),
            Self::App { args, .. } => Some(args.clone()),
            Self::Var { tipo, .. } => tipo.borrow().arg_types(),
            _ => None,
        }
    }

    pub fn get_generic(&self) -> Option<u64> {
        match self {
            Self::Var { tipo, .. } => tipo.borrow().get_generic(),
            _ => None,
        }
    }

    pub fn get_inner_types(&self) -> Vec<Rc<Type>> {
        if self.is_list() {
            match self {
                Self::App { args, .. } => args.clone(),
                Self::Var { tipo, .. } => tipo.borrow().get_inner_types(),
                _ => vec![],
            }
        } else if self.is_tuple() {
            match self {
                Self::Tuple { elems, .. } => elems.to_vec(),
                Self::Var { tipo, .. } => tipo.borrow().get_inner_types(),
                _ => vec![],
            }
        } else if self.is_pair() {
            match self {
                Self::Pair { fst, snd, .. } => vec![fst.clone(), snd.clone()],
                Self::Var { tipo, .. } => tipo.borrow().get_inner_types(),
                _ => vec![],
            }
        } else if self.get_uplc_type().is_none() {
            match self {
                Type::App { args, .. } => args.clone(),
                Type::Fn { args, ret, .. } => {
                    let mut args = args.clone();
                    args.push(ret.clone());
                    args
                }
                Type::Var { tipo, .. } => tipo.borrow().get_inner_types(),
                _ => unreachable!(),
            }
        } else {
            vec![]
        }
    }

    pub fn get_uplc_type(&self) -> Option<UplcType> {
        if self.is_int() {
            Some(UplcType::Integer)
        } else if self.is_bytearray() {
            Some(UplcType::ByteString)
        } else if self.is_string() {
            Some(UplcType::String)
        } else if self.is_bool() {
            Some(UplcType::Bool)
        } else if self.is_void() {
            Some(UplcType::Unit)
        } else if self.is_map() {
            Some(UplcType::List(
                UplcType::Pair(UplcType::Data.into(), UplcType::Data.into()).into(),
            ))
        } else if self.is_list() || self.is_tuple() {
            Some(UplcType::List(UplcType::Data.into()))
        } else if self.is_pair() {
            Some(UplcType::Pair(UplcType::Data.into(), UplcType::Data.into()))
        } else if self.is_bls381_12_g1() {
            Some(UplcType::Bls12_381G1Element)
        } else if self.is_bls381_12_g2() {
            Some(UplcType::Bls12_381G2Element)
        } else if self.is_ml_result() {
            Some(UplcType::Bls12_381MlResult)
        } else if self.is_data() {
            Some(UplcType::Data)
        } else {
            None
        }
    }

    /// Get the args for the type if the type is a specific `Type::App`.
    /// Returns None if the type is not a `Type::App` or is an incorrect `Type:App`
    ///
    /// This function is currently only used for finding the `List` type.
    pub fn get_app_args(
        &self,
        public: bool,
        opaque: bool,
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

            Self::Var { tipo, alias } => {
                let args: Vec<_> = match tipo.borrow().deref() {
                    TypeVar::Link { tipo } => {
                        return tipo.get_app_args(public, opaque, module, name, arity, environment);
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
                        public,
                        contains_opaque: opaque,
                        name: name.to_string(),
                        module: module.to_owned(),
                        args: args.clone(),
                        alias: alias.to_owned(),
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
            Self::Pair { fst, snd, .. } => {
                if let Some(private_type) = fst.find_private_type() {
                    Some(private_type)
                } else {
                    snd.find_private_type()
                }
            }
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

pub fn lookup_data_type_by_tipo(
    data_types: &IndexMap<&DataTypeKey, &TypedDataType>,
    tipo: &Type,
) -> Option<DataType<Rc<Type>>> {
    match tipo {
        Type::Fn { ret, .. } => match ret.as_ref() {
            Type::App { module, name, .. } => {
                let data_type_key = DataTypeKey {
                    module_name: module.clone(),
                    defined_type: name.clone(),
                };
                data_types.get(&data_type_key).map(|item| (*item).clone())
            }
            _ => None,
        },
        Type::App { module, name, .. } => {
            let data_type_key = DataTypeKey {
                module_name: module.clone(),
                defined_type: name.clone(),
            };

            data_types.get(&data_type_key).map(|item| (*item).clone())
        }
        Type::Var { tipo, .. } => {
            if let TypeVar::Link { tipo } = &*tipo.borrow() {
                lookup_data_type_by_tipo(data_types, tipo)
            } else {
                None
            }
        }
        _ => None,
    }
}

pub fn get_generic_id_and_type(tipo: &Type, param: &Type) -> Vec<(u64, Rc<Type>)> {
    let mut generics_ids = vec![];

    if let Some(id) = tipo.get_generic() {
        generics_ids.push((id, param.clone().into()));
        return generics_ids;
    }

    for (tipo, param_type) in tipo
        .get_inner_types()
        .iter()
        .zip(param.get_inner_types().iter())
    {
        generics_ids.append(&mut get_generic_id_and_type(tipo, param_type));
    }
    generics_ids
}

pub fn get_arg_type_name(tipo: &Type) -> String {
    match tipo {
        Type::App { name, args, .. } => {
            let inner_args = args.iter().map(|arg| get_arg_type_name(arg)).collect_vec();
            format!("{}_{}", name, inner_args.join("_"))
        }
        Type::Var { tipo, .. } => match tipo.borrow().clone() {
            TypeVar::Link { tipo } => get_arg_type_name(tipo.as_ref()),
            _ => unreachable!(),
        },
        Type::Tuple { elems, .. } => {
            let inner_args = elems.iter().map(|arg| get_arg_type_name(arg)).collect_vec();
            inner_args.join("_")
        }
        Type::Pair { fst, snd, .. } => {
            let inner_args = [fst, snd]
                .iter()
                .map(|arg| get_arg_type_name(arg))
                .collect_vec();
            inner_args.join("_")
        }
        _ => unreachable!("WTF {:#?}", tipo),
    }
}

pub fn convert_opaque_type(
    t: &Rc<Type>,
    data_types: &IndexMap<&DataTypeKey, &TypedDataType>,
    deep: bool,
) -> Rc<Type> {
    if check_replaceable_opaque_type(t, data_types) && matches!(t.as_ref(), Type::App { .. }) {
        let data_type = lookup_data_type_by_tipo(data_types, t).unwrap();

        let new_type_fields = data_type.typed_parameters;

        let mut mono_type_vec = vec![];

        for (tipo, param) in new_type_fields.iter().zip(t.arg_types().unwrap()) {
            mono_type_vec.append(&mut get_generic_id_and_type(tipo, &param));
        }
        let mono_types = mono_type_vec.into_iter().collect();

        let generic_type = &data_type.constructors[0].arguments[0].tipo;

        let mono_type = find_and_replace_generics(generic_type, &mono_types);

        if deep {
            convert_opaque_type(&mono_type, data_types, deep)
        } else {
            mono_type
        }
    } else {
        match t.as_ref() {
            Type::App {
                public,
                contains_opaque: opaque,
                module,
                name,
                args,
                alias,
            } => {
                let mut new_args = vec![];
                for arg in args {
                    let arg = convert_opaque_type(arg, data_types, deep);
                    new_args.push(arg);
                }
                Type::App {
                    public: *public,
                    contains_opaque: *opaque,
                    module: module.clone(),
                    name: name.clone(),
                    args: new_args,
                    alias: alias.clone(),
                }
                .into()
            }
            Type::Fn { args, ret, alias } => {
                let mut new_args = vec![];
                for arg in args {
                    let arg = convert_opaque_type(arg, data_types, deep);
                    new_args.push(arg);
                }

                let ret = convert_opaque_type(ret, data_types, deep);

                Type::Fn {
                    args: new_args,
                    ret,
                    alias: alias.clone(),
                }
                .into()
            }
            Type::Var { tipo: var_tipo, .. } => {
                if let TypeVar::Link { tipo } = &var_tipo.borrow().clone() {
                    convert_opaque_type(tipo, data_types, deep)
                } else {
                    t.clone()
                }
            }
            Type::Tuple { elems, alias } => {
                let mut new_elems = vec![];
                for arg in elems {
                    let arg = convert_opaque_type(arg, data_types, deep);
                    new_elems.push(arg);
                }
                Type::Tuple {
                    elems: new_elems,
                    alias: alias.clone(),
                }
                .into()
            }
            Type::Pair { fst, snd, alias } => {
                let fst = convert_opaque_type(fst, data_types, deep);
                let snd = convert_opaque_type(snd, data_types, deep);
                Type::Pair {
                    fst,
                    snd,
                    alias: alias.clone(),
                }
                .into()
            }
        }
    }
}

pub fn check_replaceable_opaque_type(
    t: &Type,
    data_types: &IndexMap<&DataTypeKey, &TypedDataType>,
) -> bool {
    let data_type = lookup_data_type_by_tipo(data_types, t);

    if let Some(data_type) = data_type {
        if let [constructor] = &data_type.constructors[..] {
            return constructor.arguments.len() == 1 && data_type.opaque;
        }
    }

    false
}

pub fn find_and_replace_generics(
    tipo: &Rc<Type>,
    mono_types: &IndexMap<u64, Rc<Type>>,
) -> Rc<Type> {
    if let Some(id) = tipo.get_generic() {
        mono_types
            .get(&id)
            .unwrap_or_else(|| {
                panic!("Unknown generic id {id:?} for type {tipo:?} in mono_types {mono_types:#?}");
            })
            .clone()
    } else if tipo.is_generic() {
        match &**tipo {
            Type::App {
                args,
                public,
                contains_opaque: opaque,
                module,
                name,
                alias,
            } => {
                let mut new_args = vec![];
                for arg in args {
                    let arg = find_and_replace_generics(arg, mono_types);
                    new_args.push(arg);
                }
                let t = Type::App {
                    args: new_args,
                    public: *public,
                    contains_opaque: *opaque,
                    module: module.clone(),
                    name: name.clone(),
                    alias: alias.clone(),
                };
                t.into()
            }
            Type::Fn { args, ret, alias } => {
                let mut new_args = vec![];
                for arg in args {
                    let arg = find_and_replace_generics(arg, mono_types);
                    new_args.push(arg);
                }

                let ret = find_and_replace_generics(ret, mono_types);

                let t = Type::Fn {
                    args: new_args,
                    ret,
                    alias: alias.clone(),
                };

                t.into()
            }
            Type::Tuple { elems, alias } => {
                let mut new_elems = vec![];
                for elem in elems {
                    let elem = find_and_replace_generics(elem, mono_types);
                    new_elems.push(elem);
                }
                let t = Type::Tuple {
                    elems: new_elems,
                    alias: alias.clone(),
                };
                t.into()
            }
            Type::Var { tipo: var_tipo, .. } => {
                let var_type = var_tipo.as_ref().borrow().clone();

                match var_type {
                    TypeVar::Link { tipo } => find_and_replace_generics(&tipo, mono_types),
                    TypeVar::Generic { .. } | TypeVar::Unbound { .. } => unreachable!(),
                }
            }
            Type::Pair { fst, snd, alias } => {
                let fst = find_and_replace_generics(fst, mono_types);
                let snd = find_and_replace_generics(snd, mono_types);
                Type::Pair {
                    fst,
                    snd,
                    alias: alias.clone(),
                }
                .into()
            }
        }
    } else {
        tipo.clone()
    }
}

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
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
    ///  Check whether a given type is fully specialized and has only one possible
    ///  form. Said differently, this recursively checks if the type still contains
    ///  unbound or generic variables.
    pub fn is_monomorphic(&self) -> bool {
        match self {
            Self::Link { tipo } => tipo.is_monomorphic(),
            Self::Unbound { .. } | Self::Generic { .. } => false,
        }
    }

    pub fn is_unbound(&self) -> bool {
        matches!(self, Self::Unbound { .. })
    }

    pub fn is_or_holds_opaque(&self) -> bool {
        match self {
            Self::Link { tipo } => tipo.contains_opaque(),
            _ => false,
        }
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

    pub fn is_bls381_12_g1(&self) -> bool {
        match self {
            Self::Link { tipo } => tipo.is_bls381_12_g1(),
            _ => false,
        }
    }

    pub fn is_bls381_12_g2(&self) -> bool {
        match self {
            Self::Link { tipo } => tipo.is_bls381_12_g2(),
            _ => false,
        }
    }
    pub fn is_ml_result(&self) -> bool {
        match self {
            Self::Link { tipo } => tipo.is_ml_result(),
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

    pub fn is_pair(&self) -> bool {
        match self {
            Self::Link { tipo } => tipo.is_pair(),
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
            TypeVar::Unbound { .. } => false,
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
                    alias: None,
                }
                .into()]
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
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

    pub fn known_enum(
        values: &mut HashMap<String, Self>,
        tipo: Rc<Type>,
        constructors: &[&str],
    ) -> Vec<String> {
        for constructor in constructors {
            values.insert(
                constructor.to_string(),
                ValueConstructor::public(
                    tipo.clone(),
                    ValueConstructorVariant::known_enum_variant(constructor, constructors.len(), 0),
                ),
            );
        }

        constructors
            .iter()
            .map(|constructor| constructor.to_string())
            .collect()
    }

    pub fn known_adt(
        values: &mut HashMap<String, Self>,
        constructors: &[(&str, Rc<Type>)],
    ) -> Vec<String> {
        for (constructor, tipo) in constructors {
            values.insert(
                constructor.to_string(),
                ValueConstructor::public(
                    tipo.clone(),
                    ValueConstructorVariant::known_enum_variant(
                        constructor,
                        constructors.len(),
                        tipo.fn_arity().unwrap_or(0),
                    ),
                ),
            );
        }

        constructors
            .iter()
            .map(|(constructor, _)| constructor.to_string())
            .collect()
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
            | ValueConstructorVariant::ModuleFn {
                module, location, ..
            }
            | ValueConstructorVariant::ModuleConstant {
                location, module, ..
            } => DefinitionLocation {
                module: Some(module.as_str()),
                span: *location,
            },

            ValueConstructorVariant::LocalVariable { location } => DefinitionLocation {
                module: None,
                span: *location,
            },
        }
    }
}

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
pub enum ValueConstructorVariant {
    /// A locally defined variable or function parameter
    LocalVariable { location: Span },

    /// A module constant
    ModuleConstant {
        location: Span,
        module: String,
        name: String,
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

            Self::ModuleConstant {
                name,
                module,
                location,
                ..
            } => ModuleValueConstructor::Constant {
                name: name.clone(),
                module: module.clone(),
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

    pub fn known_enum_variant(name: &str, constructors_count: usize, arity: usize) -> Self {
        ValueConstructorVariant::Record {
            module: "".into(),
            name: name.to_string(),
            field_map: None::<FieldMap>,
            arity,
            location: Span::empty(),
            constructors_count: constructors_count as u16,
        }
    }
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct TypeInfo {
    pub name: String,
    pub kind: ModuleKind,
    pub package: String,
    pub types: HashMap<String, TypeConstructor>,
    pub types_constructors: HashMap<String, Vec<String>>,
    pub values: HashMap<String, ValueConstructor>,
    pub accessors: HashMap<String, AccessorsMap>,
    pub annotations: HashMap<Annotation, Rc<Type>>,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct TypeConstructor {
    pub public: bool,
    pub location: Span,
    pub module: String,
    pub parameters: Vec<Rc<Type>>,
    pub tipo: Rc<Type>,
}

impl TypeConstructor {
    pub fn primitive(tipo: Rc<Type>) -> Self {
        TypeConstructor {
            location: Span::empty(),
            parameters: tipo.collect_generics(),
            tipo,
            module: "".to_string(),
            public: true,
        }
    }
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct AccessorsMap {
    pub public: bool,
    pub tipo: Rc<Type>,
    pub accessors: HashMap<String, RecordAccessor>,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct RecordAccessor {
    // TODO: smaller int. Doesn't need to be this big
    pub index: u64,
    pub label: String,
    pub tipo: Rc<Type>,
}

#[derive(Debug, Clone, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub enum PatternConstructor {
    Record {
        name: String,
        field_map: Option<FieldMap>,
    },
}

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
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
        location: Span,
        module: String,
        name: String,
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
