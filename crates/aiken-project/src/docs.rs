use crate::{
    config::{Config, Repository},
    module::CheckedModule,
};
use aiken_lang::{
    ast::{
        DataType, Definition, Function, ModuleConstant, RecordConstructor, Span, TypeAlias,
        TypedDefinition,
    },
    format,
    parser::extra::Comment,
    tipo::Type,
};
use askama::Template;
use itertools::Itertools;
use pulldown_cmark as markdown;
use regex::Regex;
use serde::Serialize;
use serde_json as json;
use std::{
    path::{Path, PathBuf},
    rc::Rc,
    time::{Duration, SystemTime},
};

const MAX_COLUMNS: isize = 80;
const VERSION: &str = env!("CARGO_PKG_VERSION");

pub mod link_tree;
pub mod source_links;

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct DocFile {
    pub path: PathBuf,
    pub content: String,
}

#[derive(Template)]
#[template(path = "module.html")]
struct ModuleTemplate<'a> {
    aiken_version: &'a str,
    breadcrumbs: String,
    page_title: &'a str,
    module_name: String,
    project_name: &'a str,
    project_version: &'a str,
    modules: &'a [DocLink],
    functions: Vec<Interspersed>,
    types: Vec<DocType>,
    constants: Vec<DocConstant>,
    documentation: String,
    source: &'a DocLink,
    timestamp: String,
}

impl ModuleTemplate<'_> {
    pub fn is_current_module(&self, module: &DocLink) -> bool {
        match module.path.split(".html").next() {
            None => false,
            Some(name) => self.module_name == name,
        }
    }
}

#[derive(Template)]
#[template(path = "page.html")]
struct PageTemplate<'a> {
    aiken_version: &'a str,
    breadcrumbs: &'a str,
    page_title: &'a str,
    project_name: &'a str,
    project_version: &'a str,
    modules: &'a [DocLink],
    content: String,
    source: &'a DocLink,
    timestamp: &'a str,
}

impl PageTemplate<'_> {
    pub fn is_current_module(&self, _module: &DocLink) -> bool {
        false
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub struct DocLink {
    indent: usize,
    name: String,
    path: String,
}

impl DocLink {
    pub fn is_empty(&self) -> bool {
        self.name.is_empty()
    }

    pub fn is_separator(&self) -> bool {
        self.path.is_empty()
    }
}

/// Generate documentation files for a given project.
///
/// The documentation is built using template files located at the root of this crate.
/// With the documentation, we also build a client-side search index to ease navigation
/// across multiple modules.
pub fn generate_all(root: &Path, config: &Config, modules: Vec<&CheckedModule>) -> Vec<DocFile> {
    let timestamp = new_timestamp();
    let modules_links = generate_modules_links(&modules);

    let source = match &config.repository {
        None => DocLink {
            indent: 0,
            name: String::new(),
            path: String::new(),
        },
        Some(Repository {
            user,
            project,
            platform,
        }) => DocLink {
            indent: 0,
            name: format!("{user}/{project}"),
            path: format!("https://{platform}.com/{user}/{project}"),
        },
    };

    let mut output_files: Vec<DocFile> = vec![];
    let mut search_indexes: Vec<SearchIndex> = vec![];

    for module in &modules {
        if module.skip_doc_generation() {
            continue;
        }

        let (indexes, file) =
            generate_module(root, config, module, &modules_links, &source, &timestamp);
        if !indexes.is_empty() {
            search_indexes.extend(indexes);
            output_files.push(file);
        }
    }

    output_files.extend(generate_static_assets(search_indexes));
    output_files.push(generate_readme(
        root,
        config,
        &modules_links,
        &source,
        &timestamp,
    ));

    output_files
}

fn generate_module(
    root: &Path,
    config: &Config,
    module: &CheckedModule,
    modules: &[DocLink],
    source: &DocLink,
    timestamp: &Duration,
) -> (Vec<SearchIndex>, DocFile) {
    let mut search_indexes = vec![];

    let source_linker = source_links::SourceLinker::new(root, config, module);

    // Section headers
    let mut section_headers = module
        .extra
        .comments
        .iter()
        .filter_map(|span| {
            let comment = Comment::from((span, module.code.as_str()))
                .content
                .trim_start();
            if comment.starts_with('#') {
                let trimmed = comment.trim_start_matches('#');
                let heading = comment.len() - trimmed.len();
                Some((
                    span,
                    DocSection {
                        heading,
                        title: trimmed.trim_start().to_string(),
                    },
                ))
            } else {
                None
            }
        })
        .collect_vec();

    // Functions
    let functions: Vec<(Span, DocFunction)> = module
        .ast
        .definitions
        .iter()
        .flat_map(|def| DocFunction::from_definition(def, &source_linker))
        .collect();

    functions.iter().for_each(|(_, function)| {
        search_indexes.push(SearchIndex::from_function(module, function))
    });

    let no_functions = functions.is_empty();

    let mut functions_and_headers = Vec::new();

    for (span_fn, function) in functions {
        let mut to_remove = vec![];
        for (ix, (span_h, header)) in section_headers.iter().enumerate() {
            if span_h.start < span_fn.start {
                functions_and_headers.push(Interspersed::Section(header.clone()));
                to_remove.push(ix);
            }
        }

        for ix in to_remove.iter().rev() {
            section_headers.remove(*ix);
        }

        functions_and_headers.push(Interspersed::Function(function))
    }

    // Types
    let types: Vec<DocType> = module
        .ast
        .definitions
        .iter()
        .flat_map(|def| DocType::from_definition(def, &source_linker))
        .sorted()
        .collect();
    types
        .iter()
        .for_each(|type_info| search_indexes.push(SearchIndex::from_type(module, type_info)));

    // Constants
    let constants: Vec<DocConstant> = module
        .ast
        .definitions
        .iter()
        .flat_map(|def| DocConstant::from_definition(def, &source_linker))
        .sorted()
        .collect();
    constants
        .iter()
        .for_each(|constant| search_indexes.push(SearchIndex::from_constant(module, constant)));

    let is_empty = no_functions && types.is_empty() && constants.is_empty();

    // Module
    if !is_empty {
        search_indexes.push(SearchIndex::from_module(module));
    }

    let module = ModuleTemplate {
        aiken_version: VERSION,
        breadcrumbs: to_breadcrumbs(&module.name),
        documentation: render_markdown(&module.ast.docs.iter().join("\n")),
        modules,
        project_name: &config.name.repo.to_string(),
        page_title: &format!("{} - {}", module.name, config.name),
        module_name: module.name.clone(),
        project_version: &config.version.to_string(),
        functions: functions_and_headers,
        types,
        constants,
        source,
        timestamp: timestamp.as_secs().to_string(),
    };

    let rendered_content = convert_latex_markers(
        module
            .render()
            .expect("Module documentation template rendering"),
    );

    (
        search_indexes,
        DocFile {
            path: PathBuf::from(format!("{}.html", module.module_name)),
            content: rendered_content,
        },
    )
}

#[cfg(windows)]
fn convert_latex_markers(input: String) -> String {
    input
}

#[cfg(not(windows))]
fn convert_latex_markers(input: String) -> String {
    let re_inline = Regex::new(r#"<span class="math math-inline">\s*(.+?)\s*</span>"#).unwrap();
    let re_block = Regex::new(r#"<span class="math math-display">\s*(.+?)\s*</span>"#).unwrap();

    let opts_inline = katex::Opts::builder()
        .display_mode(false) // Inline math
        .output_type(katex::OutputType::Mathml)
        .build()
        .unwrap();

    let opts_block = katex::Opts::builder()
        .display_mode(true) // Block math
        .output_type(katex::OutputType::Mathml)
        .build()
        .unwrap();

    let input = re_inline.replace_all(&input, |caps: &regex::Captures| {
        let formula = &caps[1];
        katex::render_with_opts(formula, &opts_inline).unwrap_or_else(|_| formula.to_string())
    });

    re_block
        .replace_all(&input, |caps: &regex::Captures| {
            let formula = &caps[1];
            katex::render_with_opts(formula, &opts_block).unwrap_or_else(|_| formula.to_string())
        })
        .to_string()
}

fn generate_static_assets(search_indexes: Vec<SearchIndex>) -> Vec<DocFile> {
    let mut assets: Vec<DocFile> = vec![];

    assets.push(DocFile {
        path: PathBuf::from("favicon.svg"),
        content: std::include_str!("../templates/favicon.svg").to_string(),
    });

    assets.push(DocFile {
        path: PathBuf::from("css/atom-one-light.min.css"),
        content: std::include_str!("../templates/css/atom-one-light.min.css").to_string(),
    });

    assets.push(DocFile {
        path: PathBuf::from("css/atom-one-dark.min.css"),
        content: std::include_str!("../templates/css/atom-one-dark.min.css").to_string(),
    });

    assets.push(DocFile {
        path: PathBuf::from("css/index.css"),
        content: std::include_str!("../templates/css/index.css").to_string(),
    });

    assets.push(DocFile {
        path: PathBuf::from("js/highlight.min.js"),
        content: std::include_str!("../templates/js/highlight.min.js").to_string(),
    });

    assets.push(DocFile {
        path: PathBuf::from("js/highlightjs-aiken.js"),
        content: std::include_str!("../templates/js/highlightjs-aiken.js").to_string(),
    });

    assets.push(DocFile {
        path: PathBuf::from("js/lunr.min.js"),
        content: std::include_str!("../templates/js/lunr.min.js").to_string(),
    });

    assets.push(DocFile {
        path: PathBuf::from("js/index.js"),
        content: std::include_str!("../templates/js/index.js").to_string(),
    });

    assets.push(DocFile {
        path: PathBuf::from("search-data.js"),
        content: format!(
            "window.Aiken.initSearch({});",
            json::to_string(&escape_html_contents(search_indexes))
                .expect("search index serialization")
        ),
    });

    assets
}

fn generate_readme(
    root: &Path,
    config: &Config,
    modules: &[DocLink],
    source: &DocLink,
    timestamp: &Duration,
) -> DocFile {
    let path = PathBuf::from("index.html");

    let content = std::fs::read_to_string(root.join("README.md")).unwrap_or_default();

    let template = PageTemplate {
        aiken_version: VERSION,
        breadcrumbs: ".",
        modules,
        project_name: &config.name.repo.to_string(),
        page_title: &config.name.to_string(),
        project_version: &config.version.to_string(),
        content: render_markdown(&content),
        source,
        timestamp: &timestamp.as_secs().to_string(),
    };

    DocFile {
        path,
        content: template.render().expect("Page template rendering"),
    }
}

fn generate_modules_links(modules: &[&CheckedModule]) -> Vec<DocLink> {
    let non_empty_modules = modules
        .iter()
        .filter(|module| {
            !module.skip_doc_generation()
                && module.ast.definitions.iter().any(|def| {
                    matches!(
                        def,
                        Definition::Fn(Function { public: true, .. })
                            | Definition::DataType(DataType { public: true, .. })
                            | Definition::TypeAlias(TypeAlias { public: true, .. })
                            | Definition::ModuleConstant(ModuleConstant { public: true, .. })
                    )
                })
        })
        .sorted_by(|a, b| a.name.cmp(&b.name))
        .collect_vec();

    let mut links = link_tree::LinkTree::default();

    for module in non_empty_modules {
        links.insert(module.name.as_str());
    }

    links.to_vec()
}

#[derive(Serialize, PartialEq, Eq, PartialOrd, Ord, Clone)]
struct SearchIndex {
    doc: String,
    title: String,
    content: String,
    url: String,
}

impl SearchIndex {
    fn from_function(module: &CheckedModule, function: &DocFunction) -> Self {
        SearchIndex {
            doc: module.name.to_string(),
            title: function.name.to_string(),
            content: format!("{}\n{}", function.signature, function.raw_documentation),
            url: format!("{}.html#{}", module.name, function.name),
        }
    }

    fn from_type(module: &CheckedModule, type_info: &DocType) -> Self {
        let constructors = type_info
            .constructors
            .iter()
            .map(|constructor| {
                format!(
                    "{}\n{}",
                    constructor.definition, constructor.raw_documentation
                )
            })
            .join("\n");

        SearchIndex {
            doc: module.name.to_string(),
            title: type_info.name.to_string(),
            content: format!(
                "{}\n{}\n{}",
                type_info.definition, type_info.raw_documentation, constructors,
            ),
            url: format!("{}.html#{}", module.name, type_info.name),
        }
    }

    fn from_constant(module: &CheckedModule, constant: &DocConstant) -> Self {
        SearchIndex {
            doc: module.name.to_string(),
            title: constant.name.to_string(),
            content: format!("{}\n{}", constant.definition, constant.raw_documentation),
            url: format!("{}.html#{}", module.name, constant.name),
        }
    }

    fn from_module(module: &CheckedModule) -> Self {
        SearchIndex {
            doc: module.name.to_string(),
            title: module.name.to_string(),
            content: module.ast.docs.iter().join("\n"),
            url: format!("{}.html", module.name),
        }
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
enum Interspersed {
    Section(DocSection),
    Function(DocFunction),
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
struct DocSection {
    heading: usize,
    title: String,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
struct DocFunction {
    name: String,
    signature: String,
    documentation: String,
    raw_documentation: String,
    source_url: String,
}

impl DocFunction {
    fn from_definition(
        def: &TypedDefinition,
        source_linker: &source_links::SourceLinker,
    ) -> Option<(Span, Self)> {
        match def {
            Definition::Fn(func_def) if func_def.public => Some((
                func_def.location,
                DocFunction {
                    name: func_def.name.clone(),
                    documentation: func_def
                        .doc
                        .as_deref()
                        .map(render_markdown)
                        .unwrap_or_default(),
                    raw_documentation: func_def.doc.as_deref().unwrap_or_default().to_string(),
                    signature: format::Formatter::new()
                        .docs_fn_signature(
                            &func_def.name,
                            &func_def.arguments,
                            &func_def.return_annotation,
                            func_def.return_type.clone(),
                        )
                        .to_pretty_string(MAX_COLUMNS),
                    source_url: source_linker
                        .url(func_def.location.map_end(|_| func_def.end_position)),
                },
            )),
            _ => None,
        }
    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord)]
struct DocConstant {
    name: String,
    definition: String,
    documentation: String,
    raw_documentation: String,
    source_url: String,
}

impl DocConstant {
    fn from_definition(
        def: &TypedDefinition,
        source_linker: &source_links::SourceLinker,
    ) -> Option<Self> {
        match def {
            Definition::ModuleConstant(const_def) if const_def.public => Some(DocConstant {
                name: const_def.name.clone(),
                documentation: const_def
                    .doc
                    .as_deref()
                    .map(render_markdown)
                    .unwrap_or_default(),
                raw_documentation: const_def.doc.as_deref().unwrap_or_default().to_string(),
                definition: format::Formatter::new()
                    .docs_const_expr(&const_def.name, &const_def.value)
                    .to_pretty_string(MAX_COLUMNS),
                source_url: source_linker.url(const_def.location),
            }),
            _ => None,
        }
    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug)]
struct DocType {
    name: String,
    definition: String,
    documentation: String,
    raw_documentation: String,
    constructors: Vec<DocTypeConstructor>,
    parameters: Vec<String>,
    opaque: bool,
    source_url: String,
}

impl DocType {
    fn from_definition(
        def: &TypedDefinition,
        source_linker: &source_links::SourceLinker,
    ) -> Option<Self> {
        match def {
            Definition::TypeAlias(info) if info.public => Some(DocType {
                name: info.alias.clone(),
                definition: format::Formatter::new()
                    .docs_type_alias(&info.alias, &info.parameters, &info.annotation)
                    .to_pretty_string(MAX_COLUMNS),
                documentation: info.doc.as_deref().map(render_markdown).unwrap_or_default(),
                raw_documentation: info.doc.as_deref().unwrap_or_default().to_string(),
                constructors: vec![],
                parameters: info.parameters.clone(),
                opaque: false,
                source_url: source_linker.url(info.location),
            }),

            Definition::DataType(info) if info.public && !info.opaque => Some(DocType {
                name: info.name.clone(),
                definition: format::Formatter::new()
                    .docs_data_type(
                        &info.name,
                        &info.parameters,
                        &info.constructors,
                        &info.location,
                    )
                    .to_pretty_string(MAX_COLUMNS),
                documentation: info.doc.as_deref().map(render_markdown).unwrap_or_default(),
                raw_documentation: info.doc.as_deref().unwrap_or_default().to_string(),
                constructors: info
                    .constructors
                    .iter()
                    .map(DocTypeConstructor::from_record_constructor)
                    .collect(),
                parameters: info.parameters.clone(),
                opaque: info.opaque,
                source_url: source_linker.url(info.location),
            }),

            Definition::DataType(info) if info.public && info.opaque => Some(DocType {
                name: info.name.clone(),
                definition: format::Formatter::new()
                    .docs_opaque_data_type(&info.name, &info.parameters, &info.location)
                    .to_pretty_string(MAX_COLUMNS),
                documentation: info.doc.as_deref().map(render_markdown).unwrap_or_default(),
                raw_documentation: info.doc.as_deref().unwrap_or_default().to_string(),
                constructors: vec![],
                parameters: info.parameters.clone(),
                opaque: info.opaque,
                source_url: source_linker.url(info.location),
            }),

            _ => None,
        }
    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug)]
struct DocTypeConstructor {
    definition: String,
    documentation: String,
    raw_documentation: String,
}

impl DocTypeConstructor {
    fn from_record_constructor(constructor: &RecordConstructor<Rc<Type>>) -> Self {
        let doc_args = constructor
            .arguments
            .iter()
            .filter_map(|arg| match (arg.label.as_deref(), arg.doc.as_deref()) {
                (Some(label), Some(doc)) => Some(format!("#### `.{label}`\n{doc}\n<hr/>\n",)),
                _ => None,
            })
            .join("\n");

        DocTypeConstructor {
            definition: format::Formatter::new()
                .docs_record_constructor(constructor)
                .to_pretty_string(format::MAX_COLUMNS),
            documentation: constructor
                .doc
                .as_deref()
                .map(|doc| render_markdown(&format!("{doc}\n{doc_args}")))
                .or(if doc_args.is_empty() {
                    None
                } else {
                    Some(render_markdown(&format!("\n{doc_args}")))
                })
                .unwrap_or_default(),
            raw_documentation: constructor.doc.as_deref().unwrap_or_default().to_string(),
        }
    }
}

// ------ Extra Helpers

fn render_markdown(text: &str) -> String {
    let mut s = String::with_capacity(text.len() * 3 / 2);
    let p = markdown::Parser::new_ext(text, markdown::Options::all());
    markdown::html::push_html(&mut s, p);
    s
}

fn escape_html_contents(indexes: Vec<SearchIndex>) -> Vec<SearchIndex> {
    fn escape_html_content(it: String) -> String {
        it.replace('&', "&amp;")
            .replace('<', "&lt;")
            .replace('>', "&gt;")
            .replace('\"', "&quot;")
            .replace('\'', "&#39;")
    }

    indexes
        .into_iter()
        .map(|idx| SearchIndex {
            doc: idx.doc,
            title: idx.title,
            content: escape_html_content(idx.content),
            url: idx.url,
        })
        .collect::<Vec<SearchIndex>>()
}

fn new_timestamp() -> Duration {
    SystemTime::now()
        .duration_since(SystemTime::UNIX_EPOCH)
        .expect("get current timestamp")
}

fn to_breadcrumbs(path: &str) -> String {
    let breadcrumbs = path
        .strip_prefix('/')
        .unwrap_or(path)
        .split('/')
        .skip(1)
        .map(|_| "..")
        .join("/");
    if breadcrumbs.is_empty() {
        ".".to_string()
    } else {
        breadcrumbs
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn to_breadcrumbs_test() {
        // Pages
        assert_eq!(to_breadcrumbs("a.html"), ".");
        assert_eq!(to_breadcrumbs("/a.html"), ".");
        assert_eq!(to_breadcrumbs("/a/b.html"), "..");
        assert_eq!(to_breadcrumbs("/a/b/c.html"), "../..");

        // Modules
        assert_eq!(to_breadcrumbs("a"), ".");
        assert_eq!(to_breadcrumbs("a/b"), "..");
        assert_eq!(to_breadcrumbs("a/b/c"), "../..");
    }

    #[test]
    fn convert_latex_markers_simple() {
        assert_eq!(
            convert_latex_markers(
                r#"<span class="math math-inline">\frac{4}{5}</span>"#.to_string()
            ),
            r#"<span class="katex"><math xmlns="http://www.w3.org/1998/Math/MathML"><semantics><mrow><mfrac><mn>4</mn><mn>5</mn></mfrac></mrow><annotation encoding="application/x-tex">\frac{4}{5}</annotation></semantics></math></span>"#,
        );
    }

    #[test]
    fn convert_latex_markers_sequence() {
        assert_eq!(
            convert_latex_markers(
                r#"<span class="math math-inline">\frac{4}{5}</span><span class="math math-inline">e^{i \times \pi}</span>"#.to_string()
            ),
            r#"<span class="katex"><math xmlns="http://www.w3.org/1998/Math/MathML"><semantics><mrow><mfrac><mn>4</mn><mn>5</mn></mfrac></mrow><annotation encoding="application/x-tex">\frac{4}{5}</annotation></semantics></math></span><span class="katex"><math xmlns="http://www.w3.org/1998/Math/MathML"><semantics><mrow><msup><mi>e</mi><mrow><mi>i</mi><mo>×</mo><mi>π</mi></mrow></msup></mrow><annotation encoding="application/x-tex">e^{i \times \pi}</annotation></semantics></math></span>"#,
        );
    }
}
