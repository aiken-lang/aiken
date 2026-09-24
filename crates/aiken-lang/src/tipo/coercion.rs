//! Compile-time proof for the explicit, unchecked representation coercion.
//! Keep the erasure rule in sync with `check_replaceable_opaque_type`.

use super::{Type, TypeInfo, TypeVar, find_and_replace_generics};
use indexmap::IndexMap;
use std::{collections::HashMap, rc::Rc};

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct OpaqueRepresentation {
    pub parameters: Vec<Rc<Type>>,
    pub inner: Rc<Type>,
}

#[derive(Debug, PartialEq, Eq)]
enum Representation {
    App(String, String, Vec<Representation>),
    Variable(u64),
    Tuple(Vec<Representation>),
    Pair(Box<Representation>, Box<Representation>),
}

pub fn compatible(
    source: &Rc<Type>,
    target: &Rc<Type>,
    current_module: &str,
    local: &HashMap<String, OpaqueRepresentation>,
    modules: &HashMap<String, TypeInfo>,
) -> bool {
    // Recursive opaque types do not have a finite erased representation.
    // Bound proof expansion and refuse a conversion when it cannot be proved.
    let source = representation(source, current_module, local, modules, 256, &mut 4096);
    let target = representation(target, current_module, local, modules, 256, &mut 4096);
    matches!((source, target), (Some(source), Some(target)) if source == target)
}

fn representation(
    tipo: &Rc<Type>,
    current_module: &str,
    local: &HashMap<String, OpaqueRepresentation>,
    modules: &HashMap<String, TypeInfo>,
    fuel: usize,
    nodes: &mut usize,
) -> Option<Representation> {
    let fuel = fuel.checked_sub(1)?;
    *nodes = nodes.checked_sub(1)?;
    let mut recurse = |tipo| representation(tipo, current_module, local, modules, fuel, nodes);
    match tipo.as_ref() {
        Type::App {
            module, name, args, ..
        } => {
            let wrapper = if module == current_module {
                local.get(name)
            } else {
                modules
                    .get(module)
                    .and_then(|info| info.opaque_representations.get(name))
            };
            if let Some(wrapper) = wrapper {
                if args.len() != wrapper.parameters.len() {
                    return None;
                }
                let parameters = wrapper
                    .parameters
                    .iter()
                    .zip(args)
                    .map(|(parameter, arg)| Some((parameter.get_generic_id()?, arg.clone())))
                    .collect::<Option<IndexMap<_, _>>>()?;
                recurse(&find_and_replace_generics(&wrapper.inner, &parameters))
            } else {
                Some(Representation::App(
                    module.clone(),
                    name.clone(),
                    args.iter().map(recurse).collect::<Option<_>>()?,
                ))
            }
        }
        Type::Var { tipo, .. } => match &*tipo.borrow() {
            TypeVar::Link { tipo } => recurse(tipo),
            TypeVar::Generic { id } | TypeVar::Unbound { id, .. } => {
                Some(Representation::Variable(*id))
            }
        },
        Type::Tuple { elems, .. } => Some(Representation::Tuple(
            elems.iter().map(recurse).collect::<Option<_>>()?,
        )),
        Type::Pair { fst, snd, .. } => Some(Representation::Pair(
            Box::new(recurse(fst)?),
            Box::new(recurse(snd)?),
        )),
        // No unchecked coercions of functions, including nested function types.
        Type::Fn { .. } => None,
    }
}
