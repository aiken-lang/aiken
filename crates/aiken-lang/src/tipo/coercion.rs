//! Compile-time proof for the explicit, unchecked representation coercion.
//! Metadata and code generation both use `erased_opaque_inner`.

use super::{Type, TypeInfo, TypeVar};
use std::{collections::HashMap, rc::Rc};

const MAX_DEPTH: usize = 256;
const MAX_NODES: usize = 4096;

#[cfg(test)]
mod tests;

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

// A substitution captures the caller's scope, not the scope of the wrapper
// being expanded. In Wrapped<Wrapped<Int>>, both applications use the same
// declaration parameter ID, but its bindings must remain distinct.
struct ScopedType<'a> {
    tipo: &'a Type,
    bindings: &'a Bindings<'a>,
}

type Bindings<'a> = HashMap<u64, ScopedType<'a>>;

struct Proof<'a> {
    current_module: &'a str,
    local: &'a HashMap<String, OpaqueRepresentation>,
    modules: &'a HashMap<String, TypeInfo>,
}

pub fn compatible(
    source: &Rc<Type>,
    target: &Rc<Type>,
    current_module: &str,
    local: &HashMap<String, OpaqueRepresentation>,
    modules: &HashMap<String, TypeInfo>,
) -> bool {
    let proof = Proof {
        current_module,
        local,
        modules,
    };
    let bindings = Bindings::new();
    let mut source_nodes = MAX_NODES;
    let mut target_nodes = MAX_NODES;
    let source = proof.representation(source, &bindings, MAX_DEPTH, &mut source_nodes);
    let target = proof.representation(target, &bindings, MAX_DEPTH, &mut target_nodes);
    matches!((source, target), (Some(source), Some(target)) if source == target)
}

fn spend(nodes: &mut usize) -> Option<()> {
    *nodes = nodes.checked_sub(1)?;
    Some(())
}

// Parameter metadata normally contains a direct generic variable. Still charge
// every link we follow, without calling an unbounded type-tree helper.
fn parameter_id(tipo: &Type, depth: usize, nodes: &mut usize) -> Option<u64> {
    let depth = depth.checked_sub(1)?;
    spend(nodes)?;
    match tipo {
        Type::Var { tipo, .. } => match &*tipo.borrow() {
            TypeVar::Link { tipo } => parameter_id(tipo, depth, nodes),
            TypeVar::Generic { id } => Some(*id),
            TypeVar::Unbound { .. } => None,
        },
        _ => None,
    }
}

impl Proof<'_> {
    fn representation(
        &self,
        tipo: &Type,
        bindings: &Bindings<'_>,
        depth: usize,
        nodes: &mut usize,
    ) -> Option<Representation> {
        let depth = depth.checked_sub(1)?;
        spend(nodes)?;
        match tipo {
            Type::App {
                module, name, args, ..
            } => {
                let wrapper = if module == self.current_module {
                    self.local.get(name)
                } else {
                    self.modules
                        .get(module)
                        .and_then(|info| info.opaque_representations.get(name))
                };
                if let Some(wrapper) = wrapper {
                    if args.len() != wrapper.parameters.len() {
                        return None;
                    }
                    let mut parameters = Bindings::new();
                    for (parameter, arg) in wrapper.parameters.iter().zip(args) {
                        let id = parameter_id(parameter, depth, nodes)?;
                        parameters.insert(
                            id,
                            ScopedType {
                                tipo: arg,
                                bindings,
                            },
                        );
                    }
                    // Resolve parameters lazily inside the same budget. No
                    // substituted type tree is scanned or cloned in advance.
                    self.representation(&wrapper.inner, &parameters, depth, nodes)
                } else {
                    Some(Representation::App(
                        module.clone(),
                        name.clone(),
                        args.iter()
                            .map(|arg| self.representation(arg, bindings, depth, nodes))
                            .collect::<Option<_>>()?,
                    ))
                }
            }
            Type::Var { tipo, .. } => match &*tipo.borrow() {
                TypeVar::Link { tipo } => self.representation(tipo, bindings, depth, nodes),
                TypeVar::Generic { id } => match bindings.get(id) {
                    Some(bound) => self.representation(bound.tipo, bound.bindings, depth, nodes),
                    None => Some(Representation::Variable(*id)),
                },
                TypeVar::Unbound { id, .. } => Some(Representation::Variable(*id)),
            },
            Type::Tuple { elems, .. } => Some(Representation::Tuple(
                elems
                    .iter()
                    .map(|elem| self.representation(elem, bindings, depth, nodes))
                    .collect::<Option<_>>()?,
            )),
            Type::Pair { fst, snd, .. } => Some(Representation::Pair(
                Box::new(self.representation(fst, bindings, depth, nodes)?),
                Box::new(self.representation(snd, bindings, depth, nodes)?),
            )),
            Type::Fn { .. } => None,
        }
    }
}
