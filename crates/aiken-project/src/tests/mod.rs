use std::collections::HashMap;
use std::path::PathBuf;

use aiken_lang::{
    ast::{ModuleKind, Tracing, TypedDataType, TypedFunction},
    gen_uplc::builder::{DataTypeKey, FunctionAccessKey},
    parser,
    tipo::TypeInfo,
    IdGenerator,
};
use indexmap::IndexMap;

use crate::{
    builtins,
    module::{CheckedModule, ParsedModule},
    package_name::PackageName,
};

mod gen_uplc;

// TODO: Possible refactor this out of the module and have it used by `Project`. The idea would
// be to make this struct below the actual project, and wrap it in another metadata struct
// which contains all the config and I/O stuff regarding the project.
pub struct TestProject {
    pub package: PackageName,
    pub id_gen: IdGenerator,
    pub module_types: HashMap<String, TypeInfo>,
    pub functions: IndexMap<FunctionAccessKey, TypedFunction>,
    pub data_types: IndexMap<DataTypeKey, TypedDataType>,
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

        let functions = builtins::prelude_functions(&id_gen);
        let data_types = builtins::prelude_data_types(&id_gen);

        TestProject {
            package,
            id_gen,
            module_types,
            functions,
            data_types,
        }
    }

    pub fn parse(&self, source_code: &str) -> ParsedModule {
        let kind = ModuleKind::Validator;
        let name = "test_module".to_owned();
        let (mut ast, extra) = parser::module(source_code, kind).expect("Failed to parse module");
        ast.name = name.clone();

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
                Tracing::KeepTraces,
                &mut warnings,
            )
            .expect("Failed to type-check module");

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
