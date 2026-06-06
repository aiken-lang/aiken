use crate::{
    Project, builtins,
    module::{CheckedModule, ParsedModule},
    package_name::PackageName,
    telemetry::{CoverageMode, EventListener},
    utils,
};
use aiken_lang::{
    IdGenerator,
    ast::{
        DataTypeKey, FunctionAccessKey, ModuleKind, TraceLevel, Tracing, TypedDataType,
        TypedFunction,
    },
    expr::TypedExpr,
    gen_uplc::CodeGenerator,
    line_numbers::LineNumbers,
    parser,
    plutus_version::PlutusVersion,
    tipo::TypeInfo,
};
use indexmap::IndexMap;
use std::{
    collections::HashMap,
    fs,
    path::PathBuf,
    time::{SystemTime, UNIX_EPOCH},
};

mod gen_uplc;

// TODO: Possible refactor this out of the module and have it used by `Project`. The idea would
// be to make this struct below the actual project, and wrap it in another metadata struct
// which contains all the config and I/O stuff regarding the project.
pub struct TestProject {
    pub package: PackageName,
    pub id_gen: IdGenerator,
    pub functions: IndexMap<FunctionAccessKey, TypedFunction>,
    pub constants: IndexMap<FunctionAccessKey, TypedExpr>,
    pub data_types: IndexMap<DataTypeKey, TypedDataType>,
    pub module_types: HashMap<String, TypeInfo>,
    pub module_sources: HashMap<String, (String, LineNumbers)>,
}

impl TestProject {
    pub fn new() -> Self {
        let id_gen = IdGenerator::new();

        let package = PackageName {
            owner: "test".to_owned(),
            repo: "project".to_owned(),
        };

        let mut module_types = HashMap::new();
        module_types.insert("aiken".to_string(), builtins::prelude(&id_gen));
        module_types.insert("aiken/builtin".to_string(), builtins::plutus(&id_gen));

        let functions = builtins::prelude_functions(&id_gen, &module_types);
        let data_types = builtins::prelude_data_types(&id_gen);
        let constants = IndexMap::new();

        TestProject {
            package,
            id_gen,
            module_types,
            functions,
            constants,
            data_types,
            module_sources: HashMap::new(),
        }
    }

    pub fn new_generator(&'_ self, tracing: Tracing) -> CodeGenerator<'_> {
        CodeGenerator::new(
            PlutusVersion::default(),
            utils::indexmap::as_ref_values(&self.functions),
            utils::indexmap::as_ref_values(&self.constants),
            utils::indexmap::as_ref_values(&self.data_types),
            utils::indexmap::as_str_ref_values(&self.module_types),
            utils::indexmap::as_str_ref_values(&self.module_sources),
            tracing,
        )
    }

    pub fn parse(&self, source_code: &str) -> ParsedModule {
        let kind = ModuleKind::Validator;
        let name = "test_module".to_owned();
        let (mut ast, extra) = parser::module(source_code, kind).expect("Failed to parse module");
        ast.name.clone_from(&name);

        ParsedModule {
            kind,
            ast,
            code: source_code.to_string(),
            name,
            path: PathBuf::new(),
            extra,
            package: self.package.to_string(),
        }
    }

    pub fn check(&mut self, module: ParsedModule) -> CheckedModule {
        let mut warnings = vec![];

        let ast = module
            .ast
            .infer(
                &self.id_gen,
                module.kind,
                &self.package.to_string(),
                &self.module_types,
                Tracing::All(TraceLevel::Verbose),
                &mut warnings,
                None,
            )
            .expect("Failed to type-check module");

        // Register function definitions & data-types for easier access later.
        ast.register_definitions(
            &mut self.functions,
            &mut self.constants,
            &mut self.data_types,
        );

        // Register module sources for an easier access later.
        self.module_sources.insert(
            module.name.clone(),
            (module.code.clone(), LineNumbers::new(&module.code)),
        );

        // Register the types from this module so they can be
        // imported into other modules.
        self.module_types
            .insert(module.name.clone(), ast.type_info.clone());

        let mut checked_module = CheckedModule {
            kind: module.kind,
            extra: module.extra,
            name: module.name,
            code: module.code,
            package: module.package,
            input_path: module.path,
            ast,
        };

        checked_module.attach_doc_and_module_comments();

        checked_module
    }
}

#[derive(Debug, Clone, Copy)]
struct NoopEventListener;

impl EventListener for NoopEventListener {}

#[test]
fn cross_module_expect_decoder_cache_key_uses_module_identity() {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("clock should be after epoch")
        .as_nanos();
    let root = std::env::temp_dir().join(format!("aiken-cross-module-expect-{unique}"));

    fs::create_dir_all(root.join("lib")).expect("should create lib dir");
    fs::write(
        root.join("aiken.toml"),
        r#"name = "aiken-lang/actiontype-collision"
version = "0.0.0"
compiler = "v1.1.22"
plutus = "v3"
license = "Apache-2.0"
"#,
    )
    .expect("should write aiken.toml");

    fs::write(
        root.join("lib/mod_a.ak"),
        r#"pub type ActionType {
  Variant { payload: ByteArray }
}

pub type RedeemerA {
  tag: Int,
  action: ActionType,
}
"#,
    )
    .expect("should write mod_a");

    fs::write(
        root.join("lib/mod_b.ak"),
        r#"pub type ActionType {
  Variant { payload: Int }
}

pub type RedeemerB {
  tag: Int,
  action: ActionType,
}
"#,
    )
    .expect("should write mod_b");

    fs::write(
        root.join("lib/collision.ak"),
        r#"use mod_a
use mod_b

pub fn expect_both(a_raw: Data, b_raw: Data) -> Bool {
  expect _a: mod_a.RedeemerA = a_raw
  expect _b: mod_b.RedeemerB = b_raw
  True
}

test reproduces_collision() {
  let a_raw: Data =
    mod_a.RedeemerA { tag: 1, action: mod_a.Variant { payload: #"deadbeef" } }

  let b_raw: Data =
    mod_b.RedeemerB { tag: 2, action: mod_b.Variant { payload: 42 } }

  expect_both(a_raw, b_raw)
}
"#,
    )
    .expect("should write collision module");

    let mut project = Project::new(root.clone(), NoopEventListener).expect("should load project");

    let result = project.check(
        false,
        None,
        false,
        false,
        1,
        1,
        CoverageMode::default(),
        Tracing::All(TraceLevel::Verbose),
        true,
        None,
    );

    let _ = fs::remove_dir_all(&root);

    assert!(
        result.is_ok(),
        "cross-module expect decoders with the same local type name should not collide: {result:#?}"
    );
}
