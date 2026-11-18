use super::Target;
use crate::{export::Export, module::CheckedModules};
use aiken_lang::{
    ast::Definition, gen_uplc::CodeGenerator, plutus_version::PlutusVersion, tipo::Type,
};
use aiken_ual::{self as ual, ValueSchema};
use serde_json::{Value, json};
use std::collections::BTreeMap;

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("cannot find function {module}.{name} in checked modules")]
    MissingFunction { module: String, name: String },
    #[error(transparent)]
    Export(Box<crate::Error>),
    #[error(transparent)]
    Json(#[from] serde_json::Error),
    #[error(transparent)]
    Digest(#[from] hex::FromHexError),
    #[error(
        "the pinned assurance consumer cannot represent the native result type {tipo} of {module}.{name}"
    )]
    UnsupportedResult {
        module: String,
        name: String,
        tipo: String,
    },
}

pub(super) fn compile(
    target: &Target,
    modules: &CheckedModules,
    generator: &mut CodeGenerator<'_>,
) -> Result<(ual::Function, BTreeMap<String, ValueSchema>), Error> {
    let missing = || Error::MissingFunction {
        module: target.module.clone(),
        name: target.name.clone(),
    };
    let module = modules.get(&target.module).ok_or_else(missing)?;
    let function = module
        .ast
        .definitions()
        .find_map(|definition| match definition {
            Definition::Fn(function) if function.name == target.name => Some(function),
            _ => None,
        })
        .ok_or_else(missing)?;

    // List<Pair<_, _>> is a native list of native pairs, not a list of Data.
    // The pinned consumer's #list binder is specifically List Data. Value is
    // another native constant with no representation in its value-schema dialect.
    if function.return_type.is_map() || function.return_type.is_value() {
        return Err(Error::UnsupportedResult {
            module: target.module.clone(),
            name: target.name.clone(),
            tipo: function.return_type.to_pretty(0),
        });
    }
    let export = Export::from_function(function, module, generator, modules, &PlutusVersion::V3)
        .map_err(|error| Error::Export(Box::new(error)))?;

    let mut definitions: BTreeMap<String, ValueSchema> =
        serde_json::from_value(serde_json::to_value(&export.definitions)?)?;
    // Export schemas describe source types. Its String and Pair schemas are
    // native, but generate_raw casts every argument from Data. These types also
    // occur inside Data constructors/containers, so normalize the finite schema
    // nodes, never recursively expand their references.
    for schema in definitions.values_mut() {
        data_schema(&mut schema.0);
    }
    let mut arguments = Vec::with_capacity(function.arguments.len());
    for (argument, parameter) in function.arguments.iter().zip(&export.parameters) {
        arguments.push(if argument.tipo.is_data() {
            raw_data(&mut definitions)
        } else {
            ValueSchema(serde_json::to_value(&parameter.schema)?)
        });
    }
    let result = match native_result(&function.return_type, &mut definitions) {
        Some(schema) => schema,
        None => ValueSchema(serde_json::to_value(&export.return_type.schema)?),
    };
    let (_, bytes) = export.program.compiled_code_and_hash();
    let compiled_code = hex::encode::<&[u8]>(bytes.as_ref());
    let hash = ual::function_digest(&compiled_code)?;
    Ok((
        ual::Function {
            compiled_code,
            serialization: ual::Serialization::CborFlat,
            hash,
            plutus_version: ual::PlutusVersion::V3,
            arguments,
            result,
        },
        definitions,
    ))
}

fn raw_data(definitions: &mut BTreeMap<String, ValueSchema>) -> ValueSchema {
    definitions.entry("Data".to_owned()).or_insert_with(|| {
        ValueSchema(json!({"title": "Data", "description": "Any Plutus data."}))
    });
    ValueSchema(json!({"$ref": "#/definitions/Data"}))
}

fn native_result(
    tipo: &Type,
    definitions: &mut BTreeMap<String, ValueSchema>,
) -> Option<ValueSchema> {
    let kind = if tipo.is_int() {
        "#integer"
    } else if tipo.is_bytearray() {
        "#bytes"
    } else if tipo.is_string() {
        "#string"
    } else if tipo.is_bool() {
        "#boolean"
    } else if tipo.is_void() {
        "#unit"
    } else if tipo.is_list() || tipo.is_tuple() {
        // Aiken represents even a two-element tuple as a native list of Data.
        return Some(ValueSchema(json!({
            "dataType": "#list", "items": raw_data(definitions)
        })));
    } else if tipo.is_pair() {
        let data = raw_data(definitions);
        return Some(ValueSchema(json!({
            "dataType": "#pair", "left": data, "right": data
        })));
    } else {
        return None;
    };
    Some(ValueSchema(json!({"dataType": kind})))
}

fn data_schema(schema: &mut Value) {
    match schema {
        Value::Array(values) => values.iter_mut().for_each(data_schema),
        Value::Object(object) => {
            object.values_mut().for_each(data_schema);
            match object.get("dataType").and_then(Value::as_str) {
                Some("#string") => {
                    // The generator uses encodeUtf8/bData and unBData/decodeUtf8.
                    object.insert("dataType".to_owned(), "bytes".into());
                }
                Some("#pair") => {
                    // Pair is encoded as a Data list, independently of the
                    // separate List<Pair<_, _>> => Data map convention.
                    let left = object
                        .remove("left")
                        .expect("export pair has a left schema");
                    let right = object
                        .remove("right")
                        .expect("export pair has a right schema");
                    object.insert("dataType".to_owned(), "list".into());
                    object.insert("items".to_owned(), Value::Array(vec![left, right]));
                }
                _ => {}
            }
        }
        _ => {}
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        assurance::{ExecutionSettings, TargetKind},
        tests::TestProject,
    };
    use aiken_lang::ast::{Span, TraceLevel, Tracing};

    fn target(name: &str) -> Target {
        Target {
            id: format!("test_module__{name}"),
            module: "test_module".to_owned(),
            name: name.to_owned(),
            kind: TargetKind::Function,
            location: Span::empty(),
            settings: ExecutionSettings {
                steps: 1000,
                semantics: "C".to_owned(),
            },
        }
    }

    fn lower(source: &str, name: &str) -> (ual::Function, BTreeMap<String, ValueSchema>) {
        let mut project = TestProject::new();
        let module = project.check(project.parse(source));
        let modules = CheckedModules::singleton(module);
        let mut generator = project.new_generator(Tracing::All(TraceLevel::Verbose));
        let lowered = compile(&target(name), &modules, &mut generator).unwrap();
        let (module, function) = modules
            .functions()
            .find(|(_, function)| function.name == name)
            .unwrap();
        let mut export_generator = project.new_generator(Tracing::All(TraceLevel::Verbose));
        let export = Export::from_function(
            function,
            module,
            &mut export_generator,
            &modules,
            &PlutusVersion::V3,
        )
        .unwrap();
        let exported = serde_json::to_value(export).unwrap();
        assert_eq!(
            lowered.0.compiled_code,
            exported["compiledCode"].as_str().unwrap()
        );
        assert_eq!(
            lowered.0.hash,
            ual::function_digest(&lowered.0.compiled_code).unwrap()
        );
        assert_eq!(lowered.0.arguments.len(), function.arguments.len());
        for schema in lowered
            .0
            .arguments
            .iter()
            .chain(std::iter::once(&lowered.0.result))
            .chain(lowered.1.values())
        {
            ual::validate(ual::DocumentKind::ValueSchema, &schema.0).unwrap();
        }
        crate::blueprint::dialect::graph::validate(&json!({
            "functions": {name: &lowered.0}, "definitions": &lowered.1
        }))
        .unwrap();
        lowered
    }

    #[test]
    fn primitive_arguments_are_data_and_results_are_native() {
        for (tipo, native) in [
            ("Int", "#integer"),
            ("ByteArray", "#bytes"),
            ("String", "#string"),
            ("Bool", "#boolean"),
            ("Void", "#unit"),
        ] {
            let (function, definitions) = lower(
                &format!("pub fn identity(value: {tipo}) -> {tipo} {{ value }}"),
                "identity",
            );
            assert_eq!(
                function.arguments[0].0,
                json!({"$ref": format!("#/definitions/{tipo}")})
            );
            assert_eq!(function.result.0, json!({"dataType": native}));
            let schema = &definitions[tipo].0;
            assert!(
                !schema["dataType"]
                    .as_str()
                    .is_some_and(|kind| kind.starts_with('#'))
            );
            if tipo == "String" {
                assert_eq!(schema["dataType"], "bytes");
            }
        }
    }

    #[test]
    fn raw_data_arguments_are_identity_even_with_an_erased_annotation() {
        for tipo in ["Data", "Data<Int>"] {
            let (function, definitions) = lower(
                &format!("pub fn identity(value: {tipo}) -> Data {{ value }}"),
                "identity",
            );
            assert_eq!(
                function.arguments[0].0,
                json!({"$ref": "#/definitions/Data"})
            );
            assert_eq!(function.result.0, json!({"$ref": "#/definitions/Data"}));
            assert_eq!(
                definitions["Data"].0,
                json!({"title": "Data", "description": "Any Plutus data."})
            );
        }
    }

    #[test]
    fn list_pair_and_both_tuple_sizes_have_data_elements() {
        for tipo in ["List<Int>", "(Int, String)", "(Int, String, Bool)"] {
            let (function, _) = lower(
                &format!("pub fn identity(value: {tipo}) -> {tipo} {{ value }}"),
                "identity",
            );
            assert_eq!(
                function.result.0,
                json!({"dataType": "#list", "items": {"$ref": "#/definitions/Data"}})
            );
        }
        let (function, definitions) = lower(
            "pub fn identity(value: Pair<Int, String>) -> Pair<Int, String> { value }",
            "identity",
        );
        assert_eq!(
            function.result.0,
            json!({"dataType": "#pair", "left": {"$ref": "#/definitions/Data"}, "right": {"$ref": "#/definitions/Data"}})
        );
        let reference = function.arguments[0].0["$ref"]
            .as_str()
            .unwrap()
            .trim_start_matches('#');
        let document = json!({"definitions": definitions});
        let pair = document.pointer(reference).unwrap();
        assert_eq!(pair["dataType"], "list");
        assert_eq!(pair["items"].as_array().unwrap().len(), 2);
    }

    #[test]
    fn recursive_and_optional_results_keep_finite_data_schemas() {
        let (function, definitions) = lower(
            "pub type Tree { Leaf(String) Branch(Tree, Tree) }\npub fn identity(value: Tree) -> Tree { value }",
            "identity",
        );
        assert_eq!(function.result, function.arguments[0]);
        assert_eq!(definitions.len(), 2);
        assert_eq!(definitions["String"].0["dataType"], "bytes");
        let document = json!({"definitions": definitions});
        let tree = document
            .pointer(
                function.result.0["$ref"]
                    .as_str()
                    .unwrap()
                    .trim_start_matches('#'),
            )
            .unwrap();
        assert_eq!(
            tree["anyOf"][1]["fields"][0]["$ref"],
            function.result.0["$ref"]
        );
        let (function, _) = lower(
            "pub fn identity(value: Option<Int>) -> Option<Int> { value }",
            "identity",
        );
        assert_eq!(function.result, function.arguments[0]);
    }

    #[test]
    fn map_arguments_remain_data_maps() {
        let (function, definitions) = lower(
            "pub fn ignore(_value: List<Pair<Int, String>>) -> Int { 1 }",
            "ignore",
        );
        let document = json!({"definitions": definitions});
        let schema = document
            .pointer(
                function.arguments[0].0["$ref"]
                    .as_str()
                    .unwrap()
                    .trim_start_matches('#'),
            )
            .unwrap();
        assert_eq!(schema["dataType"], "map");
    }

    #[test]
    fn unsupported_native_map_and_value_results_are_reported() {
        for tipo in ["List<Pair<Int, Int>>", "Value"] {
            let mut project = TestProject::new();
            let module = project.check(project.parse(&format!(
                "pub fn identity(value: {tipo}) -> {tipo} {{ value }}"
            )));
            let modules = CheckedModules::singleton(module);
            let mut generator = project.new_generator(Tracing::All(TraceLevel::Verbose));
            assert!(matches!(
                compile(&target("identity"), &modules, &mut generator),
                Err(Error::UnsupportedResult { .. })
            ));
        }
    }

    #[test]
    fn exact_module_and_function_names_select_the_export() {
        let (function, _) = lower(
            "pub fn first(value: Int) -> Int { value + 1 }\npub fn second(value: Int) -> Int { value + 2 }",
            "second",
        );
        assert_eq!(function.result.0, json!({"dataType": "#integer"}));
        let mut project = TestProject::new();
        let module = project.check(project.parse("pub fn present(value: Int) -> Int { value }"));
        let modules = CheckedModules::singleton(module);
        let mut generator = project.new_generator(Tracing::All(TraceLevel::Verbose));
        assert!(matches!(
            compile(&target("missing"), &modules, &mut generator),
            Err(Error::MissingFunction { .. })
        ));
        let mut absent_module = target("present");
        absent_module.module = "missing_module".to_owned();
        assert!(matches!(
            compile(&absent_module, &modules, &mut generator),
            Err(Error::MissingFunction { .. })
        ));
    }
}
