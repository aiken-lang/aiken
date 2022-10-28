use std::{
    collections::{HashMap, HashSet},
    ops::{Deref, DerefMut},
    path::PathBuf,
};

use aiken_lang::ast::{ModuleKind, TypedModule, UntypedModule};
use petgraph::{algo, graph::NodeIndex, Direction, Graph};

use crate::error::Error;

#[derive(Debug)]
pub struct ParsedModule {
    pub path: PathBuf,
    pub name: String,
    pub code: String,
    pub kind: ModuleKind,
    pub package: String,
    pub ast: UntypedModule,
    // extra: ModuleExtra,
}

impl ParsedModule {
    pub fn deps_for_graph(&self) -> (String, Vec<String>) {
        let name = self.name.clone();

        let deps: Vec<_> = self
            .ast
            .dependencies()
            .into_iter()
            .map(|(dep, _span)| dep)
            .collect();

        (name, deps)
    }
}

pub struct ParsedModules(HashMap<String, ParsedModule>);

impl ParsedModules {
    pub fn sequence(&self) -> Result<Vec<String>, Error> {
        let inputs = self
            .0
            .values()
            .map(|m| m.deps_for_graph())
            .collect::<Vec<(String, Vec<String>)>>();

        let capacity = inputs.len();

        let mut graph = Graph::<(), ()>::with_capacity(capacity, capacity * 5);

        // TODO: maybe use a bimap?
        let mut indices = HashMap::with_capacity(capacity);
        let mut values = HashMap::with_capacity(capacity);

        for (value, _) in &inputs {
            let index = graph.add_node(());

            indices.insert(value.clone(), index);

            values.insert(index, value.clone());
        }

        for (value, deps) in inputs {
            if let Some(from_index) = indices.get(&value) {
                let deps = deps.into_iter().filter_map(|dep| indices.get(&dep));

                for to_index in deps {
                    graph.add_edge(*from_index, *to_index, ());
                }
            }
        }

        match algo::toposort(&graph, None) {
            Ok(sequence) => {
                let sequence = sequence
                    .iter()
                    .filter_map(|i| values.remove(i))
                    .rev()
                    .collect();

                Ok(sequence)
            }
            Err(cycle) => {
                let origin = cycle.node_id();

                let mut path = vec![];

                find_cycle(origin, origin, &graph, &mut path, &mut HashSet::new());

                let modules = path
                    .iter()
                    .filter_map(|index| values.remove(index))
                    .collect();

                Err(Error::ImportCycle { modules })
            }
        }
    }
}

impl From<HashMap<String, ParsedModule>> for ParsedModules {
    fn from(parsed_modules: HashMap<String, ParsedModule>) -> Self {
        ParsedModules(parsed_modules)
    }
}

impl From<ParsedModules> for HashMap<String, ParsedModule> {
    fn from(parsed_modules: ParsedModules) -> Self {
        parsed_modules.0
    }
}

impl Deref for ParsedModules {
    type Target = HashMap<String, ParsedModule>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for ParsedModules {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

fn find_cycle(
    origin: NodeIndex,
    parent: NodeIndex,
    graph: &petgraph::Graph<(), ()>,
    path: &mut Vec<NodeIndex>,
    seen: &mut HashSet<NodeIndex>,
) -> bool {
    seen.insert(parent);

    for node in graph.neighbors_directed(parent, Direction::Outgoing) {
        if node == origin {
            path.push(node);

            return true;
        }

        if seen.contains(&node) {
            continue;
        }

        if find_cycle(origin, node, graph, path, seen) {
            path.push(node);

            return true;
        }
    }

    false
}

#[derive(Debug)]
pub struct CheckedModule {
    pub name: String,
    pub code: String,
    pub input_path: PathBuf,
    pub kind: ModuleKind,
    pub ast: TypedModule,
    // pub extra: ModuleExtra,
}
