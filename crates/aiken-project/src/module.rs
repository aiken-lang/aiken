use crate::{Error, Warning};
use aiken_lang::{
    ast::{
        DataType, DataTypeKey, Definition, Function, FunctionAccessKey, Located, ModuleKind,
        Tracing, TypedDataType, TypedFunction, TypedModule, TypedValidator, UntypedModule,
        Validator,
    },
    expr::TypedExpr,
    line_numbers::LineNumbers,
    parser::extra::{comments_before, Comment, ModuleExtra},
    tipo::TypeInfo,
    IdGenerator,
};
use indexmap::IndexMap;
use miette::NamedSource;
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
    pub fn deps_for_graph(&self, env_modules: &[String]) -> (String, Vec<String>) {
        let name = self.name.clone();
        let deps: Vec<_> = self.ast.dependencies(env_modules);
        (name, deps)
    }

    #[allow(clippy::too_many_arguments)]
    pub fn infer(
        self,
        id_gen: &IdGenerator,
        package: &str,
        tracing: Tracing,
        env: Option<&str>,
        validate_module_name: bool,
        module_sources: &mut HashMap<String, (String, LineNumbers)>,
        module_types: &mut HashMap<String, TypeInfo>,
        functions: &mut IndexMap<FunctionAccessKey, TypedFunction>,
        constants: &mut IndexMap<FunctionAccessKey, TypedExpr>,
        data_types: &mut IndexMap<DataTypeKey, TypedDataType>,
    ) -> Result<(CheckedModule, Vec<Warning>), Error> {
        let mut warnings = Vec::new();

        let ast = self
            .ast
            .infer(
                id_gen,
                self.kind,
                package,
                module_types,
                tracing,
                &mut warnings,
                env,
            )
            .map_err(|error| Error::Type {
                path: self.path.clone(),
                src: self.code.clone(),
                named: NamedSource::new(self.path.display().to_string(), self.code.clone()),
                error,
            })?;

        let warnings = warnings
            .into_iter()
            .map(|w| Warning::from_type_warning(w, self.path.clone(), self.code.clone()))
            .collect::<Vec<_>>();

        // Unless we're compiling prelude documentation, prevent keywords in module name
        if validate_module_name {
            ast.validate_module_name()?;
        }

        // Register module sources for an easier access later.
        module_sources.insert(
            self.name.clone(),
            (self.code.clone(), LineNumbers::new(&self.code)),
        );

        // Register the types from this module so they can be
        // imported into other modules.
        module_types.insert(self.name.clone(), ast.type_info.clone());

        // Register function definitions & data-types for easier access later.
        ast.register_definitions(functions, constants, data_types);

        Ok((
            CheckedModule {
                ast,
                kind: self.kind,
                extra: self.extra,
                name: self.name,
                code: self.code,
                package: self.package,
                input_path: self.path,
            },
            warnings,
        ))
    }
}

pub struct ParsedModules(HashMap<String, ParsedModule>);

impl ParsedModules {
    pub fn new() -> Self {
        Self(HashMap::new())
    }

    pub fn sequence(&self, our_modules: &BTreeSet<String>) -> Result<Vec<String>, Error> {
        let env_modules = self
            .0
            .values()
            .filter_map(|m| match m.kind {
                ModuleKind::Env => Some(m.name.clone()),
                ModuleKind::Lib | ModuleKind::Validator | ModuleKind::Config => None,
            })
            .collect::<Vec<String>>();

        let inputs = self
            .0
            .values()
            .map(|m| m.deps_for_graph(&env_modules))
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
    pub fn skip_doc_generation(&self) -> bool {
        self.ast
            .docs
            .first()
            .map(|s| s.as_str().trim())
            .unwrap_or_default()
            == "@hidden"
    }

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
                    handlers,
                    fallback,
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

                    for handler in handlers.iter_mut() {
                        for argument in handler.arguments.iter_mut() {
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

                    for argument in fallback.arguments.iter_mut() {
                        let docs: Vec<&str> =
                            comments_before(&mut doc_comments, argument.location.start, &self.code);

                        if !docs.is_empty() {
                            let doc = docs.join("\n");
                            argument.put_doc(doc);
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

    // todo: this might need fixing
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
                left.1.name.to_string(),
            )
                .cmp(&(
                    right.0.package.to_string(),
                    right.0.name.to_string(),
                    right.1.name.to_string(),
                ))
        });

        items.into_iter()
    }

    pub fn functions(&self) -> impl Iterator<Item = (&CheckedModule, &TypedFunction)> {
        let mut items = vec![];

        for module in self.0.values() {
            for some_definition in module.ast.definitions() {
                if let Definition::Fn(def) = some_definition {
                    items.push((module, def));
                }
            }
        }

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
