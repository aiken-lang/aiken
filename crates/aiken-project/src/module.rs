use crate::error::Error;
use aiken_lang::{
    ast::{
        DataType, Definition, Function, Located, ModuleKind, TypedModule, TypedValidator,
        UntypedModule, Validator,
    },
    parser::extra::{comments_before, Comment, ModuleExtra},
};
use petgraph::{algo, graph::NodeIndex, Direction, Graph};
use std::{
    collections::{BTreeSet, HashMap},
    io,
    ops::{Deref, DerefMut},
    path::PathBuf,
};

#[derive(Debug)]
pub struct ParsedModule {
    pub path: PathBuf,
    pub name: String,
    pub code: String,
    pub kind: ModuleKind,
    pub package: String,
    pub ast: UntypedModule,
    pub extra: ModuleExtra,
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
    pub fn new() -> Self {
        Self(HashMap::new())
    }

    pub fn sequence(&self, our_modules: &BTreeSet<String>) -> Result<Vec<String>, Error> {
        let inputs = self
            .0
            .values()
            .map(|m| m.deps_for_graph())
            .collect::<Vec<(String, Vec<String>)>>();

        let capacity = inputs.len();

        let mut graph = Graph::<String, ()>::with_capacity(capacity, capacity * 5);

        let mut indices = HashMap::with_capacity(capacity);

        let mut our_indices = BTreeSet::new();

        for (value, _) in &inputs {
            let index = graph.add_node(value.to_string());
            indices.insert(value.clone(), index);
            if our_modules.contains(value) {
                our_indices.insert(index);
            }
        }

        for (value, deps) in inputs {
            if let Some(from_index) = indices.get(&value) {
                let deps = deps.into_iter().filter_map(|dep| indices.get(&dep));

                for to_index in deps {
                    graph.add_edge(*from_index, *to_index, ());
                }
            }
        }

        let mut messed_up_indices = false;

        // Prune the dependency graph to only keep nodes that have a path to one of our (i.e. the
        // current project) module. This effectively prunes dependencies that are unused from the
        // graph to ensure that we only compile the modules we actually depend on.
        graph.retain_nodes(|graph, ix| {
            // When discarding a node, indices in the graph end up being rewritten. Yet, we need to
            // know starting indices for our search, so when we remove a dependency, we need find
            // back what those indices are.
            if messed_up_indices {
                our_indices = BTreeSet::new();
                for j in graph.node_indices() {
                    if our_modules.contains(graph[j].as_str()) {
                        our_indices.insert(j);
                    }
                }
            }

            for start in our_indices.iter() {
                if algo::astar(&*graph, *start, |end| end == ix, |_| 1, |_| 0).is_some() {
                    messed_up_indices = false;
                    return true;
                }
            }

            messed_up_indices = true;
            false
        });

        match algo::toposort(&graph, None) {
            Ok(sequence) => {
                let sequence = sequence
                    .iter()
                    .filter_map(|i| graph.node_weight(*i))
                    .rev()
                    .cloned()
                    .collect();

                Ok(sequence)
            }
            Err(cycle) => {
                let origin = cycle.node_id();

                let mut path = vec![];

                find_cycle(origin, origin, &graph, &mut path, &mut BTreeSet::new());

                let modules = path
                    .iter()
                    .filter_map(|i| graph.node_weight(*i))
                    .cloned()
                    .collect();

                Err(Error::ImportCycle { modules })
            }
        }
    }
}

impl Default for ParsedModules {
    fn default() -> Self {
        Self::new()
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

fn find_cycle<W>(
    origin: NodeIndex,
    parent: NodeIndex,
    graph: &petgraph::Graph<W, ()>,
    path: &mut Vec<NodeIndex>,
    seen: &mut BTreeSet<NodeIndex>,
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

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct CheckedModule {
    pub name: String,
    pub code: String,
    pub input_path: PathBuf,
    pub kind: ModuleKind,
    pub package: String,
    pub ast: TypedModule,
    pub extra: ModuleExtra,
}

impl CheckedModule {
    pub fn to_cbor(&self) -> Vec<u8> {
        let mut module_bytes = vec![];

        ciborium::into_writer(&self, &mut module_bytes)
            .expect("modules should not fail to serialize");

        module_bytes
    }

    pub fn from_cbor(bytes: &[u8]) -> Result<Self, ciborium::de::Error<io::Error>> {
        ciborium::from_reader(bytes)
    }

    pub fn to_cbor_hex(&self) -> (String, Vec<u8>) {
        let module_bytes = self.to_cbor();
        let hex_str = hex::encode(&module_bytes);

        (hex_str, module_bytes)
    }

    pub fn find_node(&self, byte_index: usize) -> Option<Located<'_>> {
        self.ast.find_node(byte_index)
    }

    pub fn attach_doc_and_module_comments(&mut self) {
        // Module Comments
        self.ast.docs = self
            .extra
            .module_comments
            .iter()
            .map(|span| {
                Comment::from((span, self.code.as_str()))
                    .content
                    .to_string()
            })
            .collect();

        // Order definitions to avoid dissociating doc comments from them
        let mut definitions: Vec<_> = self.ast.definitions.iter_mut().collect();
        definitions.sort_by(|a, b| a.location().start.cmp(&b.location().start));

        // Doc Comments
        let mut doc_comments = self.extra.doc_comments.iter().peekable();
        for def in &mut definitions {
            let docs: Vec<&str> =
                comments_before(&mut doc_comments, def.location().start, &self.code);
            if !docs.is_empty() {
                let doc = docs.join("\n");
                def.put_doc(doc);
            }

            match def {
                Definition::DataType(DataType { constructors, .. }) => {
                    for constructor in constructors {
                        let docs: Vec<&str> = comments_before(
                            &mut doc_comments,
                            constructor.location.start,
                            &self.code,
                        );
                        if !docs.is_empty() {
                            let doc = docs.join("\n");
                            constructor.put_doc(doc);
                        }

                        for argument in constructor.arguments.iter_mut() {
                            let docs: Vec<&str> = comments_before(
                                &mut doc_comments,
                                argument.location.start,
                                &self.code,
                            );
                            if !docs.is_empty() {
                                let doc = docs.join("\n");
                                argument.put_doc(doc);
                            }
                        }
                    }
                }
                Definition::Fn(Function { arguments, .. }) => {
                    for argument in arguments {
                        let docs: Vec<&str> =
                            comments_before(&mut doc_comments, argument.location.start, &self.code);

                        if !docs.is_empty() {
                            let doc = docs.join("\n");
                            argument.put_doc(doc);
                        }
                    }
                }
                Definition::Validator(Validator {
                    params,
                    fun,
                    other_fun,
                    ..
                }) => {
                    for param in params {
                        let docs: Vec<&str> =
                            comments_before(&mut doc_comments, param.location.start, &self.code);

                        if !docs.is_empty() {
                            let doc = docs.join("\n");
                            param.put_doc(doc);
                        }
                    }

                    for argument in fun.arguments.iter_mut() {
                        let docs: Vec<&str> =
                            comments_before(&mut doc_comments, argument.location.start, &self.code);

                        if !docs.is_empty() {
                            let doc = docs.join("\n");
                            argument.put_doc(doc);
                        }
                    }

                    if let Some(fun) = other_fun {
                        for argument in fun.arguments.iter_mut() {
                            let docs: Vec<&str> = comments_before(
                                &mut doc_comments,
                                argument.location.start,
                                &self.code,
                            );

                            if !docs.is_empty() {
                                let doc = docs.join("\n");
                                argument.put_doc(doc);
                            }
                        }
                    }
                }
                _ => (),
            }
        }
    }
}

#[derive(Default, Debug, Clone)]
pub struct CheckedModules(HashMap<String, CheckedModule>);

impl From<HashMap<String, CheckedModule>> for CheckedModules {
    fn from(checked_modules: HashMap<String, CheckedModule>) -> Self {
        CheckedModules(checked_modules)
    }
}

impl From<CheckedModules> for HashMap<String, CheckedModule> {
    fn from(checked_modules: CheckedModules) -> Self {
        checked_modules.0
    }
}

impl<'a> From<&'a CheckedModules> for &'a HashMap<String, CheckedModule> {
    fn from(checked_modules: &'a CheckedModules) -> Self {
        &checked_modules.0
    }
}

impl CheckedModules {
    pub fn singleton(module: CheckedModule) -> Self {
        let mut modules = Self::default();
        modules.insert(module.name.clone(), module);
        modules
    }

    pub fn validators(&self) -> impl Iterator<Item = (&CheckedModule, &TypedValidator)> {
        let mut items = vec![];

        for validator_module in self.0.values().filter(|module| module.kind.is_validator()) {
            for some_definition in validator_module.ast.definitions() {
                if let Definition::Validator(def) = some_definition {
                    items.push((validator_module, def));
                }
            }
        }

        items.sort_by(|left, right| {
            (
                left.0.package.to_string(),
                left.0.name.to_string(),
                left.1.fun.name.to_string(),
            )
                .cmp(&(
                    right.0.package.to_string(),
                    right.0.name.to_string(),
                    right.1.fun.name.to_string(),
                ))
        });

        items.into_iter()
    }

    pub fn into_validators(self) -> impl Iterator<Item = CheckedModule> {
        self.0
            .into_values()
            .filter(|module| module.kind.is_validator())
    }
}

impl Deref for CheckedModules {
    type Target = HashMap<String, CheckedModule>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for CheckedModules {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}
