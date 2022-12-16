use crate::{config::Config, module::CheckedModule};
use aiken_lang::{
    ast::{Definition, TypedDefinition},
    format,
};
use askama::Template;
use itertools::Itertools;
use pulldown_cmark as markdown;
use serde::Serialize;
use serde_json as json;
use std::{
    path::PathBuf,
    time::{Duration, SystemTime},
};

const MAX_COLUMNS: isize = 80;
const VERSION: &str = env!("CARGO_PKG_VERSION");

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
    links: &'a Vec<DocLink>,
    modules: &'a Vec<DocLink>,
    functions: Vec<DocFunction>,
    types: Vec<DocType>,
    constants: Vec<DocConstant>,
    documentation: String,
    timestamp: String,
}

#[derive(Template)]
#[template(path = "page.html")]
struct PageTemplate<'a> {
    aiken_version: &'a str,
    breadcrumbs: &'a str,
    page_title: &'a str,
    project_name: &'a str,
    project_version: &'a str,
    links: &'a Vec<DocLink>,
    modules: &'a Vec<DocLink>,
    content: String,
    timestamp: &'a str,
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone)]
struct DocLink {
    name: String,
    path: String,
}

#[derive(Serialize, PartialEq, Eq, PartialOrd, Ord, Clone)]
struct SearchIndex {
    doc: String,
    title: String,
    content: String,
    url: String,
}

/// Generate documentation files for a given project.
///
/// The documentation is built using template files located at the root of this crate.
/// With the documentation, we also build a client-side search index to ease navigation
/// across multiple modules.
pub fn generate_all(root: &PathBuf, config: &Config, modules: Vec<&CheckedModule>) -> Vec<DocFile> {
    let timestamp = new_timestamp();

    let mut output_files: Vec<DocFile> = vec![];
    let mut search_indexes: Vec<SearchIndex> = vec![];

    for module in &modules {
        let (indexes, file) = generate_module(config, module, &timestamp);
        search_indexes.extend(indexes);
        output_files.push(file);
    }

    output_files.extend(generate_static_assets(search_indexes));
    output_files.push(generate_readme(root, config, modules, &timestamp));
    output_files
}

fn generate_module(
    config: &Config,
    module: &CheckedModule,
    timestamp: &Duration,
) -> (Vec<SearchIndex>, DocFile) {
    let mut search_indexes = vec![];

    // Functions
    let functions: Vec<DocFunction> = module
        .ast
        .definitions
        .iter()
        .flat_map(DocFunction::from_definition)
        .sorted()
        .collect();

    functions.iter().for_each(|function| {
        search_indexes.push(SearchIndex {
            doc: module.name.to_string(),
            title: function.name.to_string(),
            content: format!("{}\n{}", function.signature, function.documentation),
            url: format!("{}.html#{}", module.name, function.name),
        })
    });

    // Types

    // Constants

    // Template

    search_indexes.push(SearchIndex {
        doc: module.name.to_string(),
        title: module.name.to_string(),
        content: String::new(),
        url: format!("{}.html", module.name),
    });

    let template = ModuleTemplate {
        aiken_version: VERSION,
        breadcrumbs: to_breadcrumbs(&module.name),
        links: &vec![],
        documentation: render_markdown(&module.ast.docs.iter().join("\n")),
        modules: &vec![],
        project_name: &config.name,
        page_title: &format!("{} - {}", module.name, config.name),
        module_name: module.name.clone(),
        project_version: &config.version.to_string(),
        functions,
        types: vec![],
        constants: vec![],
        timestamp: timestamp.as_secs().to_string(),
    };

    (
        search_indexes,
        DocFile {
            path: PathBuf::from(format!("{}.html", module.name)),
            content: template
                .render()
                .expect("Module documentation template rendering"),
        },
    )
}

fn generate_static_assets(search_indexes: Vec<SearchIndex>) -> Vec<DocFile> {
    let mut assets: Vec<DocFile> = vec![];

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
        path: PathBuf::from("js/highlightjs-gleam.js"),
        content: std::include_str!("../templates/js/highlightjs-gleam.js").to_string(),
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
    root: &PathBuf,
    config: &Config,
    modules: Vec<&CheckedModule>,
    timestamp: &Duration,
) -> DocFile {
    let path = PathBuf::from("index.html");

    let content = std::fs::read_to_string(root.join("README.md")).unwrap_or_default();

    let mut modules_links = vec![];
    for module in modules {
        let module_path = [&module.name.clone(), ".html"].concat();
        modules_links.push(DocLink {
            path: module_path,
            name: module.name.to_string().clone(),
        });
    }
    modules_links.sort();

    let template = PageTemplate {
        aiken_version: VERSION,
        breadcrumbs: ".",
        links: &vec![],
        modules: &modules_links,
        project_name: &config.name,
        page_title: &config.name,
        project_version: &config.version.to_string(),
        content: render_markdown(&content),
        timestamp: &timestamp.as_secs().to_string(),
    };

    DocFile {
        path,
        content: template.render().expect("Page template rendering"),
    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord)]
struct DocFunction {
    name: String,
    signature: String,
    documentation: String,
    source_url: String,
}

impl DocFunction {
    fn from_definition(def: &TypedDefinition) -> Option<Self> {
        match def {
            Definition::Fn(func_def) if func_def.public => Some(DocFunction {
                name: func_def.name.clone(),
                documentation: func_def
                    .doc
                    .as_deref()
                    .map(render_markdown)
                    .unwrap_or_default(),
                signature: format::Formatter::new()
                    .docs_fn_signature(
                        true,
                        &func_def.name,
                        &func_def.arguments,
                        func_def.return_type.clone(),
                    )
                    .to_pretty_string(MAX_COLUMNS),
                source_url: "TODO: source_url".to_string(),
            }),
            _ => None,
        }
    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord)]
struct DocConstant {
    name: String,
    definition: String,
    documentation: String,
    text_documentation: String,
    source_url: String,
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug)]
struct DocType {
    name: String,
    definition: String,
    documentation: String,
    constructors: Vec<DocTypeConstructor>,
    text_documentation: String,
    source_url: String,
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug)]
struct DocTypeConstructor {
    definition: String,
    documentation: String,
    text_documentation: String,
    arguments: Vec<DocTypeConstructorArg>,
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug)]
struct DocTypeConstructorArg {
    name: String,
    doc: String,
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
    let unnest = path
        .strip_prefix('/')
        .unwrap_or(path)
        .split('/')
        .skip(1)
        .map(|_| "..")
        .join("/");
    if unnest.is_empty() {
        ".".to_string()
    } else {
        unnest
    }
}

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
