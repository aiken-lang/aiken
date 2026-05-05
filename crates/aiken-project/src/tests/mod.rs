use crate::{
    builtins,
    export::FuzzerSemantics,
    module::{CheckedModule, CheckedModules, ParsedModule},
    package_name::PackageName,
    utils,
};
use aiken_lang::{
    IdGenerator,
    ast::{
        DataTypeKey, FunctionAccessKey, ModuleKind, Span, TraceLevel, Tracing, TypedDataType,
        TypedFunction,
    },
    expr::TypedExpr,
    gen_uplc::CodeGenerator,
    line_numbers::LineNumbers,
    parser,
    plutus_version::PlutusVersion,
    tipo::{Type, TypeInfo},
};
use indexmap::IndexMap;
use std::{collections::HashMap, path::PathBuf};

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

fn widgets_data_types() -> IndexMap<DataTypeKey, TypedDataType> {
    let mut data_types = IndexMap::new();
    data_types.insert(
        DataTypeKey {
            module_name: "widgets".to_string(),
            defined_type: "Result".to_string(),
        },
        TypedDataType {
            decorators: vec![],
            constructors: vec![],
            doc: None,
            location: Span::empty(),
            name: "Result".to_string(),
            opaque: false,
            parameters: vec!["a".to_string()],
            public: true,
            typed_parameters: vec![],
        },
    );
    data_types.insert(
        DataTypeKey {
            module_name: "widgets".to_string(),
            defined_type: "State".to_string(),
        },
        TypedDataType {
            decorators: vec![],
            constructors: vec![],
            doc: None,
            location: Span::empty(),
            name: "State".to_string(),
            opaque: false,
            parameters: vec![],
            public: true,
            typed_parameters: vec![],
        },
    );
    data_types
}

#[test]
fn resolve_type_name_to_type_preserves_generic_arguments() {
    let data_types = widgets_data_types();

    let result_int = super::resolve_type_name_to_type(&data_types, "widgets.Result<Int>")
        .expect("generic Result<Int> key should resolve");
    let result_bytes = super::resolve_type_name_to_type(&data_types, "widgets.Result<ByteArray>")
        .expect("generic Result<ByteArray> key should resolve");

    let Type::App { args: int_args, .. } = result_int.as_ref() else {
        panic!("Result<Int> should resolve to Type::App")
    };
    let Type::App {
        args: bytes_args, ..
    } = result_bytes.as_ref()
    else {
        panic!("Result<ByteArray> should resolve to Type::App")
    };
    assert!(int_args[0].is_int());
    assert!(bytes_args[0].is_bytearray());
    assert_ne!(int_args[0], bytes_args[0]);
}

#[test]
fn resolve_type_name_to_type_handles_tuple_generic_arguments() {
    let data_types = widgets_data_types();

    let result = super::resolve_type_name_to_type(&data_types, "widgets.Result<(Int, ByteArray)>")
        .expect("generic Result<(Int, ByteArray)> key should resolve");

    let Type::App { args, .. } = result.as_ref() else {
        panic!("Result<(Int, ByteArray)> should resolve to Type::App")
    };
    let Type::Tuple { elems, .. } = args[0].as_ref() else {
        panic!("tuple generic argument should resolve to Type::Tuple")
    };

    assert!(elems[0].is_int());
    assert!(elems[1].is_bytearray());
}

#[test]
fn resolve_type_name_to_type_handles_module_qualified_monomorphic_types() {
    let data_types = widgets_data_types();

    let state = super::resolve_type_name_to_type(&data_types, "widgets.State")
        .expect("monomorphic widgets.State key should resolve");

    let Type::App {
        module, name, args, ..
    } = state.as_ref()
    else {
        panic!("widgets.State should resolve to Type::App")
    };

    assert_eq!(module, "widgets");
    assert_eq!(name, "State");
    assert!(args.is_empty());
}

#[test]
fn resolve_type_name_to_type_handles_unique_local_custom_types() {
    let data_types = widgets_data_types();

    let state = super::resolve_type_name_to_type(&data_types, "State")
        .expect("unique local custom type should resolve");

    let Type::App {
        module, name, args, ..
    } = state.as_ref()
    else {
        panic!("State should resolve to Type::App")
    };
    assert_eq!(module, "widgets");
    assert_eq!(name, "State");
    assert!(args.is_empty());
}

#[test]
fn resolve_type_name_to_type_rejects_ambiguous_local_custom_types() {
    let mut data_types = widgets_data_types();
    data_types.insert(
        DataTypeKey {
            module_name: "other".to_string(),
            defined_type: "State".to_string(),
        },
        TypedDataType {
            decorators: vec![],
            constructors: vec![],
            doc: None,
            location: Span::empty(),
            name: "State".to_string(),
            opaque: false,
            parameters: vec![],
            public: true,
            typed_parameters: vec![],
        },
    );

    assert!(super::resolve_type_name_to_type(&data_types, "State").is_none());
}

#[test]
fn collect_inner_data_schemas_includes_local_custom_leaves() {
    let mut project = TestProject::new();
    let mut module = project.parse(
        r#"
pub type Result<a> {
  Ok(a)
  Err(ByteArray)
}
"#,
    );
    module.name = "widgets".to_string();
    module.ast.name = "widgets".to_string();
    let checked = project.check(module);
    let data_types = project.data_types.clone();
    let checked_modules = CheckedModules::from(HashMap::from([(checked.name.clone(), checked)]));

    let semantics = FuzzerSemantics::DataWithSchema {
        type_name: "Result<Int>".to_string(),
    };

    let schemas = super::collect_inner_data_schemas(
        &checked_modules,
        &data_types,
        &semantics,
        "widgets.test",
    )
    .expect("local custom DataWithSchema leaf should resolve and export");

    assert!(schemas.contains_key("Result<Int>"));
}

#[test]
fn collect_inner_data_schemas_errors_on_unresolvable_custom_leaves() {
    let data_types = widgets_data_types();
    let semantics = FuzzerSemantics::DataWithSchema {
        type_name: "MissingType".to_string(),
    };

    let err = super::collect_inner_data_schemas(
        &CheckedModules::default(),
        &data_types,
        &semantics,
        "widgets.test",
    )
    .expect_err("unresolvable DataWithSchema leaf must surface an error");

    let message = err.to_string();
    assert!(message.contains("MissingType"));
    assert!(message.contains("inner_data_schemas"));
}

#[test]
fn json_relative_path_keeps_project_relative_paths_and_redacts_external_absolutes() {
    let cwd = std::env::current_dir().expect("test process must know current dir");
    let root = cwd.join("workspace/member-a");
    let inside = root.join("validators/foo.ak");
    assert_eq!(
        super::json_relative_path(&root, &inside),
        "validators/foo.ak"
    );

    let outside = std::env::temp_dir().join("external-proof.ak");
    let public = super::json_relative_path(&root, &outside);
    assert_eq!(public, "<local>/external-proof.ak");
    assert!(
        !public.starts_with('/'),
        "public JSON paths must not leak absolute local paths: {public}"
    );
}


#[test]
fn transition_unsupported_log_includes_helper_widenings() {
    let widenings = vec![crate::export::TransitionWidening {
        kind: crate::export::TransitionWideningKind::Relation,
        message: "primary relation widening".to_string(),
    }];
    let helper_widenings = vec![crate::export::TransitionWidening {
        kind: crate::export::TransitionWideningKind::DataFreshening,
        message: "helper-side initial-state widening".to_string(),
    }];

    let log = super::transition_unsupported_log(&widenings, &helper_widenings);
    assert_eq!(
        log,
        vec![
            "primary relation widening".to_string(),
            "helper-side initial-state widening".to_string(),
        ],
        "legacy unsupported_log mirror must include helper-side widenings as well as theorem widenings"
    );
}