pub mod blueprint;
pub mod config;
pub mod deps;
pub mod docs;
pub mod error;
pub mod export;
pub mod format;
pub mod github;
pub mod module;
pub mod options;
pub mod package_name;
pub mod paths;
pub mod pretty;
pub mod telemetry;
pub mod verify;
pub mod watch;

mod test_framework;

#[cfg(test)]
mod tests;

use crate::{
    blueprint::{
        Blueprint,
        definitions::Definitions,
        schema::{
            Annotated, Data as BlueprintSchemaData, Declaration as BlueprintSchemaDeclaration,
            Items as BlueprintSchemaItems, Schema,
        },
    },
    config::ProjectConfig,
    error::{Error, Warning},
    module::{CheckedModule, CheckedModules, Glossary, ParsedModule, ParsedModules},
    options::BlueprintExport,
    telemetry::{CoverageMode, Event},
};
use aiken_lang::{
    IdGenerator,
    ast::{
        self, DataTypeKey, Definition, FunctionAccessKey, ModuleKind, Tracing, TypedDataType,
        TypedFunction, TypedTest, TypedValidator, UntypedDefinition,
    },
    builtins,
    expr::{TypedExpr, UntypedExpr},
    format::{Formatter, MAX_COLUMNS},
    gen_uplc::CodeGenerator,
    line_numbers::LineNumbers,
    test_framework::{
        AnalyzedPropertyTest, FuzzerConstraint as LangFuzzerConstraint,
        FuzzerSemantics as LangFuzzerSemantics, PropertyTest, RunnableKind,
        SemanticType as LangSemanticType, StateMachineAcceptance as LangStateMachineAcceptance,
        StateMachineTransitionSemantics as LangStateMachineTransitionSemantics, Test, TestResult,
    },
    tipo::{self, ModuleValueConstructor, Type, TypeInfo, ValueConstructorVariant},
    utils,
};
use export::{
    Export, ExportedDataSchema, ExportedProgram, ExportedPropertyTest, ExportedTests,
    FuzzerConstraint, FuzzerExactValue, FuzzerSemantics, StateMachineAcceptance,
    StateMachineTransitionSemantics, TestReturnMode, ValidatorTarget, VerificationTargetKind,
    fuzzer_output_type_from,
};
use indexmap::IndexMap;
use miette::NamedSource;
use options::{CodeGenMode, Options};
use package_name::PackageName;
use pallas_addresses::{Address, Network, ShelleyAddress, ShelleyDelegationPart, StakePayload};
use pallas_primitives::conway::PolicyId;
use std::{
    collections::{BTreeMap, BTreeSet, HashMap, HashSet},
    fs::{self, File},
    io::BufReader,
    path::{Path, PathBuf},
    rc::Rc,
};
use telemetry::EventListener;
use uplc::{
    PlutusData,
    ast::{Constant, Name, Program},
};

type CollectedRunnable = (PathBuf, String, TypedTest);
type TestMatch = (String, Option<Vec<String>>);

/// Convert a `FuzzerConstraint` from the lang crate to the export crate's version.
fn convert_exact_value(value: &aiken_lang::test_framework::FuzzerExactValue) -> FuzzerExactValue {
    match value {
        aiken_lang::test_framework::FuzzerExactValue::Bool(v) => FuzzerExactValue::Bool(*v),
        aiken_lang::test_framework::FuzzerExactValue::ByteArray(bytes) => {
            FuzzerExactValue::ByteArray(bytes.clone())
        }
        aiken_lang::test_framework::FuzzerExactValue::String(s) => {
            FuzzerExactValue::String(s.clone())
        }
        _ => unreachable!("unknown aiken-lang FuzzerExactValue variant"),
    }
}

fn convert_constraint(lang: &LangFuzzerConstraint) -> FuzzerConstraint {
    match lang {
        LangFuzzerConstraint::Any => FuzzerConstraint::Any,
        LangFuzzerConstraint::IntRange { min, max } => FuzzerConstraint::IntRange {
            min: min.clone(),
            max: max.clone(),
        },
        LangFuzzerConstraint::ByteStringLenRange { min_len, max_len } => {
            FuzzerConstraint::ByteStringLenRange {
                min_len: *min_len,
                max_len: *max_len,
            }
        }
        LangFuzzerConstraint::Exact(value) => FuzzerConstraint::Exact(convert_exact_value(value)),
        LangFuzzerConstraint::OneOf(values) => {
            FuzzerConstraint::OneOf(values.iter().map(convert_exact_value).collect())
        }
        LangFuzzerConstraint::Tuple(elems) => {
            FuzzerConstraint::Tuple(elems.iter().map(convert_constraint).collect())
        }
        LangFuzzerConstraint::List {
            elem,
            min_len,
            max_len,
        } => FuzzerConstraint::List {
            elem: Box::new(convert_constraint(elem)),
            min_len: *min_len,
            max_len: *max_len,
        },
        LangFuzzerConstraint::DataConstructorTags { tags } => {
            FuzzerConstraint::DataConstructorTags { tags: tags.clone() }
        }
        LangFuzzerConstraint::Map(inner) => {
            FuzzerConstraint::Map(Box::new(convert_constraint(inner)))
        }
        LangFuzzerConstraint::And(constraints) => {
            FuzzerConstraint::And(constraints.iter().map(convert_constraint).collect())
        }
        LangFuzzerConstraint::Unsupported { reason } => FuzzerConstraint::Unsupported {
            reason: reason.clone(),
        },
        // Public boundary: aiken-lang marks this enum non-exhaustive, so keep
        // accepting genuinely future variants as explicit unsupported metadata.
        _ => FuzzerConstraint::Unsupported {
            reason: "unsupported future fuzzer constraint variant".to_string(),
        },
    }
}

fn convert_semantic_type(lang: &LangSemanticType) -> export::FuzzerOutputType {
    match lang {
        LangSemanticType::Int => export::FuzzerOutputType::Int,
        LangSemanticType::Bool => export::FuzzerOutputType::Bool,
        LangSemanticType::ByteArray => export::FuzzerOutputType::ByteArray,
        LangSemanticType::String => export::FuzzerOutputType::String,
        LangSemanticType::Data => export::FuzzerOutputType::Data,
        LangSemanticType::List(inner) => {
            export::FuzzerOutputType::List(Box::new(convert_semantic_type(inner)))
        }
        LangSemanticType::Tuple(elems) => {
            export::FuzzerOutputType::Tuple(elems.iter().map(convert_semantic_type).collect())
        }
        LangSemanticType::Pair(fst, snd) => export::FuzzerOutputType::Pair(
            Box::new(convert_semantic_type(fst)),
            Box::new(convert_semantic_type(snd)),
        ),
        LangSemanticType::Unsupported(name) => export::FuzzerOutputType::Unsupported(name.clone()),
        _ => export::FuzzerOutputType::Unsupported(
            "unsupported future semantic type variant".to_string(),
        ),
    }
}

fn convert_state_machine_acceptance(lang: &LangStateMachineAcceptance) -> StateMachineAcceptance {
    match lang {
        LangStateMachineAcceptance::AcceptsSuccess => StateMachineAcceptance::AcceptsSuccess,
        LangStateMachineAcceptance::AcceptsFailure => StateMachineAcceptance::AcceptsFailure,
        _ => unreachable!(
            "aiken_lang::test_framework::StateMachineAcceptance gained a new variant without updating aiken-project export conversion",
        ),
    }
}

fn convert_state_machine_transition_semantics(
    lang: &LangStateMachineTransitionSemantics,
) -> StateMachineTransitionSemantics {
    StateMachineTransitionSemantics {
        terminal_tag: lang.terminal_tag,
        step_tag: lang.step_tag,
        label_field_index: lang.label_field_index,
        next_state_field_index: lang.next_state_field_index,
        event_field_index: lang.event_field_index,
        state_semantics: Box::new(convert_semantics(lang.state_semantics.as_ref())),
        step_input_semantics: lang
            .step_input_semantics
            .iter()
            .map(convert_semantics)
            .collect(),
        label_semantics: Box::new(convert_semantics(lang.label_semantics.as_ref())),
        event_semantics: Box::new(convert_semantics(lang.event_semantics.as_ref())),
    }
}

fn transition_unsupported_log(
    widenings: &[crate::export::TransitionWidening],
    helper_widenings: &[crate::export::TransitionWidening],
) -> Vec<String> {
    widenings
        .iter()
        .chain(helper_widenings.iter())
        .map(|entry| entry.message.clone())
        .collect()
}

fn convert_semantics(lang: &LangFuzzerSemantics) -> FuzzerSemantics {
    match lang {
        LangFuzzerSemantics::Bool => FuzzerSemantics::Bool,
        LangFuzzerSemantics::IntRange { min, max } => FuzzerSemantics::IntRange {
            min: min.clone(),
            max: max.clone(),
        },
        LangFuzzerSemantics::ByteArrayRange { min_len, max_len } => {
            FuzzerSemantics::ByteArrayRange {
                min_len: *min_len,
                max_len: *max_len,
            }
        }
        LangFuzzerSemantics::String => FuzzerSemantics::String,
        LangFuzzerSemantics::Data => FuzzerSemantics::Data,
        LangFuzzerSemantics::DataWithSchema { type_name } => FuzzerSemantics::DataWithSchema {
            type_name: type_name.clone(),
        },
        LangFuzzerSemantics::Exact(value) => FuzzerSemantics::Exact(convert_exact_value(value)),
        LangFuzzerSemantics::OneOf(values) => {
            FuzzerSemantics::OneOf(values.iter().map(convert_exact_value).collect())
        }
        LangFuzzerSemantics::Product(elems) => {
            FuzzerSemantics::Product(elems.iter().map(convert_semantics).collect())
        }
        LangFuzzerSemantics::List {
            element,
            min_len,
            max_len,
        } => FuzzerSemantics::List {
            element: Box::new(convert_semantics(element)),
            min_len: *min_len,
            max_len: *max_len,
        },
        LangFuzzerSemantics::Constructors { tags } => {
            FuzzerSemantics::Constructors { tags: tags.clone() }
        }
        LangFuzzerSemantics::StateMachineTrace {
            acceptance,
            state_type,
            step_input_types,
            label_type,
            event_type,
            transition_semantics,
            output_semantics,
            step_function_ir,
            step_ir_unsupported_reason,
            // `transition_prop` is intentionally dropped at the export
            // boundary — see the matching note in `export.rs`. S4 (Lean
            // emission) consumes it directly from the live `FuzzerSemantics`.
            transition_prop: _,
            initial_state_shallow_ir,
        } => FuzzerSemantics::StateMachineTrace {
            acceptance: convert_state_machine_acceptance(acceptance),
            state_type: convert_semantic_type(state_type),
            step_input_types: step_input_types.iter().map(convert_semantic_type).collect(),
            label_type: convert_semantic_type(label_type),
            event_type: convert_semantic_type(event_type),
            transition_semantics: convert_state_machine_transition_semantics(transition_semantics),
            output_semantics: Box::new(convert_semantics(output_semantics)),
            step_function_ir: step_function_ir.clone(),
            step_ir_unsupported_reason: step_ir_unsupported_reason.clone(),
            initial_state_shallow_ir: initial_state_shallow_ir.clone(),
        },
        LangFuzzerSemantics::Opaque { reason } => FuzzerSemantics::Opaque {
            reason: reason.clone(),
        },
        // Public boundary: aiken-lang marks this enum non-exhaustive, so keep
        // accepting genuinely future variants as explicit opaque metadata.
        _ => FuzzerSemantics::Opaque {
            reason: "unsupported future fuzzer semantics variant".to_string(),
        },
    }
}

/// Recursively walk a `FuzzerSemantics` tree and collect every qualified
/// `type_name` appearing in a `DataWithSchema` leaf. These are the types for
/// which `verify.rs` wants a structural predicate rather than the default
/// `True` placeholder.
fn collect_data_with_schema_type_names(semantics: &FuzzerSemantics) -> Vec<String> {
    let mut out = Vec::new();
    collect_data_with_schema_type_names_into(semantics, &mut out);
    out
}

fn collect_data_with_schema_type_names_into(semantics: &FuzzerSemantics, out: &mut Vec<String>) {
    match semantics {
        FuzzerSemantics::DataWithSchema { type_name } => out.push(type_name.clone()),
        FuzzerSemantics::Product(elems) => {
            for elem in elems {
                collect_data_with_schema_type_names_into(elem, out);
            }
        }
        FuzzerSemantics::List { element, .. } => {
            collect_data_with_schema_type_names_into(element.as_ref(), out);
        }
        FuzzerSemantics::StateMachineTrace {
            transition_semantics,
            output_semantics,
            ..
        } => {
            collect_data_with_schema_type_names_into(
                transition_semantics.state_semantics.as_ref(),
                out,
            );
            collect_data_with_schema_type_names_into(
                transition_semantics.label_semantics.as_ref(),
                out,
            );
            collect_data_with_schema_type_names_into(
                transition_semantics.event_semantics.as_ref(),
                out,
            );
            for input in &transition_semantics.step_input_semantics {
                collect_data_with_schema_type_names_into(input, out);
            }
            collect_data_with_schema_type_names_into(output_semantics.as_ref(), out);
        }
        FuzzerSemantics::Bool
        | FuzzerSemantics::IntRange { .. }
        | FuzzerSemantics::ByteArrayRange { .. }
        | FuzzerSemantics::String
        | FuzzerSemantics::Data
        | FuzzerSemantics::Exact(_)
        | FuzzerSemantics::OneOf(_)
        | FuzzerSemantics::Constructors { .. }
        | FuzzerSemantics::Opaque { .. } => {}
    }
}

fn collect_lang_transition_prop_data_with_schema_type_names(
    prop: &aiken_lang::test_framework::TransitionProp,
    out: &mut Vec<String>,
) {
    use aiken_lang::test_framework::TransitionProp as LangTransitionProp;

    match prop {
        LangTransitionProp::Exists { domain, body, .. } => {
            collect_data_with_schema_type_names_into(&convert_semantics(domain.as_ref()), out);
            collect_lang_transition_prop_data_with_schema_type_names(body.as_ref(), out);
        }
        LangTransitionProp::And(parts) | LangTransitionProp::Or(parts) => {
            for part in parts {
                collect_lang_transition_prop_data_with_schema_type_names(part, out);
            }
        }
        LangTransitionProp::IfThenElse { t, e, .. } => {
            collect_lang_transition_prop_data_with_schema_type_names(t.as_ref(), out);
            collect_lang_transition_prop_data_with_schema_type_names(e.as_ref(), out);
        }
        LangTransitionProp::Match { arms, .. } => {
            for arm in arms {
                collect_lang_transition_prop_data_with_schema_type_names(&arm.body, out);
            }
        }
        LangTransitionProp::Pure(_)
        | LangTransitionProp::EqOutput(_)
        | LangTransitionProp::SubGenerator { .. }
        | LangTransitionProp::Unsupported { .. } => {}
    }
}

fn schema_type_name_candidates(type_name: &str) -> Vec<String> {
    let mut candidates = vec![type_name.to_string()];
    if let Some((module, name)) = type_name.rsplit_once('.') {
        let slash = format!("{module}/{name}");
        if !candidates.contains(&slash) {
            candidates.push(slash);
        }
    }
    if let Some((module, name)) = type_name.rsplit_once('/') {
        let dotted = format!("{module}.{name}");
        if !candidates.contains(&dotted) {
            candidates.push(dotted);
        }
    }
    candidates
}

fn collect_schema_reference_keys_from_declaration(
    declaration: &BlueprintSchemaDeclaration<BlueprintSchemaData>,
    out: &mut Vec<String>,
) {
    match declaration {
        BlueprintSchemaDeclaration::Referenced(reference) => out.push(reference.as_key()),
        BlueprintSchemaDeclaration::Inline(data) => collect_schema_reference_keys_from_data(data, out),
    }
}

fn collect_schema_reference_keys_from_data(data: &BlueprintSchemaData, out: &mut Vec<String>) {
    match data {
        BlueprintSchemaData::Integer | BlueprintSchemaData::Bytes | BlueprintSchemaData::Opaque => {}
        BlueprintSchemaData::List(BlueprintSchemaItems::One(item)) => {
            collect_schema_reference_keys_from_declaration(item, out);
        }
        BlueprintSchemaData::List(BlueprintSchemaItems::Many(items)) => {
            for item in items {
                collect_schema_reference_keys_from_declaration(&item.annotated, out);
            }
        }
        BlueprintSchemaData::Map(keys, values) => {
            collect_schema_reference_keys_from_declaration(keys, out);
            collect_schema_reference_keys_from_declaration(values, out);
        }
        BlueprintSchemaData::AnyOf(constructors) => {
            for constructor in constructors {
                for field in &constructor.annotated.fields {
                    collect_schema_reference_keys_from_declaration(&field.annotated, out);
                }
            }
        }
    }
}

fn collect_exported_schema_dependency_keys(schema: &ExportedDataSchema) -> Vec<String> {
    let mut out = Vec::new();
    if let Some(annotated) = schema.definitions.try_lookup(&schema.root)
        && let Schema::Data(data) = &annotated.annotated
    {
        collect_schema_reference_keys_from_data(data, &mut out);
    }
    for (_key, annotated) in schema.definitions.iter() {
        if let Schema::Data(data) = &annotated.annotated {
            collect_schema_reference_keys_from_data(data, &mut out);
        }
    }
    out.sort();
    out.dedup();
    out
}

fn insert_inner_data_schema_with_dependencies(
    modules: &CheckedModules,
    data_types: &IndexMap<DataTypeKey, TypedDataType>,
    type_name: &str,
    test_name: &str,
    inner_data_schemas: &mut BTreeMap<String, ExportedDataSchema>,
) -> Result<(), Error> {
    if inner_data_schemas.contains_key(type_name) {
        return Ok(());
    }

    let (resolved_name, tipo) = schema_type_name_candidates(type_name)
        .into_iter()
        .find_map(|candidate| {
            resolve_type_name_to_type(data_types, &candidate).map(|tipo| (candidate, tipo))
        })
        .ok_or_else(|| {
            Error::StandardIo(std::io::Error::other(format!(
                "Test '{test_name}': DataWithSchema leaf '{type_name}' could not be resolved to a unique custom type for inner_data_schemas export."
            )))
        })?;

    if inner_data_schemas.contains_key(&resolved_name) {
        return Ok(());
    }

    let schema = export_data_schema(modules, data_types, &tipo).ok_or_else(|| {
        Error::StandardIo(std::io::Error::other(format!(
            "Test '{test_name}': DataWithSchema leaf '{type_name}' could not be exported to inner_data_schemas."
        )))
    })?;
    let dependency_keys = collect_exported_schema_dependency_keys(&schema);
    inner_data_schemas.insert(resolved_name, schema);

    for dependency in dependency_keys {
        if inner_data_schemas.contains_key(&dependency) {
            continue;
        }
        if let Some((candidate, _)) = schema_type_name_candidates(&dependency)
            .into_iter()
            .find_map(|candidate| {
                resolve_type_name_to_type(data_types, &candidate).map(|tipo| (candidate, tipo))
            })
        {
            insert_inner_data_schema_with_dependencies(
                modules,
                data_types,
                &candidate,
                test_name,
                inner_data_schemas,
            )?;
        }
    }

    Ok(())
}

fn extend_inner_data_schemas_from_transition_prop(
    modules: &CheckedModules,
    data_types: &IndexMap<DataTypeKey, TypedDataType>,
    prop: &aiken_lang::test_framework::TransitionProp,
    test_name: &str,
    inner_data_schemas: &mut BTreeMap<String, ExportedDataSchema>,
) -> Result<(), Error> {
    let mut type_names = Vec::new();
    collect_lang_transition_prop_data_with_schema_type_names(prop, &mut type_names);
    for type_name in type_names {
        let _ = insert_inner_data_schema_with_dependencies(
            modules,
            data_types,
            &type_name,
            test_name,
            inner_data_schemas,
        );
    }
    Ok(())
}

fn collect_inner_data_schemas(
    modules: &CheckedModules,
    data_types: &IndexMap<DataTypeKey, TypedDataType>,
    semantics: &FuzzerSemantics,
    test_name: &str,
 ) -> Result<BTreeMap<String, ExportedDataSchema>, Error> {
    let mut inner_data_schemas = BTreeMap::new();

    for type_name in collect_data_with_schema_type_names(semantics) {
        insert_inner_data_schema_with_dependencies(
            modules,
            data_types,
            &type_name,
            test_name,
            &mut inner_data_schemas,
        )?;
    }

    Ok(inner_data_schemas)
}

fn normalize_json_path(path: &Path) -> String {
    path.to_string_lossy().replace('\\', "/")
}

fn redact_absolute_json_path(path: &Path) -> String {
    path.file_name()
        .map(|name| format!("<local>/{}", name.to_string_lossy()))
        .unwrap_or_else(|| "<local>".to_string())
}

fn json_relative_path(root: &Path, path: &Path) -> String {
    if !path.is_absolute() {
        return normalize_json_path(path);
    }
    if let Ok(relative) = path.strip_prefix(root) {
        return normalize_json_path(relative);
    }
    if let Ok(cwd) = std::env::current_dir()
        && let Ok(relative) = path.strip_prefix(&cwd)
    {
        return normalize_json_path(relative);
    }
    redact_absolute_json_path(path)
}
fn split_top_level_type_items(raw: &str) -> Option<Vec<&str>> {
    let mut parts = Vec::new();
    let mut angle_depth = 0usize;
    let mut paren_depth = 0usize;
    let mut bracket_depth = 0usize;
    let mut start = 0usize;

    for (index, ch) in raw.char_indices() {
        match ch {
            '<' => angle_depth += 1,
            '>' => angle_depth = angle_depth.checked_sub(1)?,
            '(' => paren_depth += 1,
            ')' => paren_depth = paren_depth.checked_sub(1)?,
            '[' => bracket_depth += 1,
            ']' => bracket_depth = bracket_depth.checked_sub(1)?,
            ',' if angle_depth == 0 && paren_depth == 0 && bracket_depth == 0 => {
                let part = raw[start..index].trim();
                if part.is_empty() {
                    return None;
                }
                parts.push(part);
                start = index + 1;
            }
            _ => {}
        }
    }

    if angle_depth != 0 || paren_depth != 0 || bracket_depth != 0 {
        return None;
    }

    let tail = raw[start..].trim();
    if tail.is_empty() {
        return None;
    }
    parts.push(tail);

    Some(parts)
}

fn split_type_application(raw: &str) -> Option<(&str, Vec<&str>)> {
    let Some(args_start) = raw.find('<') else {
        return Some((raw, Vec::new()));
    };
    if !raw.ends_with('>') {
        return None;
    }

    let head = &raw[..args_start];
    if head.is_empty() {
        return None;
    }

    let args = &raw[args_start + 1..raw.len() - 1];
    Some((head, split_top_level_type_items(args)?))
}

fn split_tuple_type(raw: &str) -> Option<Vec<&str>> {
    if !(raw.starts_with('(') && raw.ends_with(')')) {
        return None;
    }

    let elems = &raw[1..raw.len() - 1];
    split_top_level_type_items(elems)
}

fn primitive_schema_type(name: &str) -> Option<Rc<Type>> {
    match name {
        "Int" => Some(Type::int()),
        "Bool" => Some(Type::bool()),
        "ByteArray" => Some(Type::byte_array()),
        "String" => Some(Type::string()),
        "Data" => Some(Type::data()),
        _ => None,
    }
}

/// Resolve a canonical `DataWithSchema` type key into a concrete `Rc<Type>`.
/// Keys use `module/path.TypeName<Arg,...>` and preserve applied generic
/// arguments so `Result<Int>` and `Result<ByteArray>` do not collide.
fn resolve_type_name_to_type(
    data_types: &IndexMap<DataTypeKey, TypedDataType>,
    type_name: &str,
) -> Option<Rc<Type>> {
    if let Some(elem_keys) = split_tuple_type(type_name) {
        let elems = elem_keys
            .iter()
            .map(|elem| resolve_type_name_to_type(data_types, elem))
            .collect::<Option<Vec<_>>>()?;
        return Some(Type::tuple(elems));
    }

    let (head, arg_keys) = split_type_application(type_name)?;
    if arg_keys.is_empty() {
        match head {
            "Void" => return Some(Type::void()),
            _ => {
                if let Some(tipo) = primitive_schema_type(head) {
                    return Some(tipo);
                }
            }
        }
    }

    let args = arg_keys
        .iter()
        .map(|arg| resolve_type_name_to_type(data_types, arg))
        .collect::<Option<Vec<_>>>()?;
    match (head, args.as_slice()) {
        ("List", [inner]) => return Some(Type::list(inner.clone())),
        ("Option", [inner]) => return Some(Type::option(inner.clone())),
        ("Pair", [fst, snd]) => return Some(Type::pair(fst.clone(), snd.clone())),
        _ => {}
    }

    if let Some((module_name, defined_type)) = head.rsplit_once('.') {
        let key = DataTypeKey {
            module_name: module_name.to_string(),
            defined_type: defined_type.to_string(),
        };
        let data_type = data_types.get(&key)?;
        return Some(Rc::new(Type::App {
            public: true,
            contains_opaque: data_type.opaque,
            module: module_name.to_string(),
            name: defined_type.to_string(),
            args,
            alias: None,
        }));
    }

    let mut matches = data_types
        .iter()
        .filter(|(key, _)| key.defined_type == head);
    let (key, data_type) = matches.next()?;
    if matches.next().is_some() {
        return None;
    }
    Some(Rc::new(Type::App {
        public: true,
        contains_opaque: data_type.opaque,
        module: key.module_name.clone(),
        name: key.defined_type.clone(),
        args,
        alias: None,
    }))
}

fn export_data_schema(
    modules: &CheckedModules,
    data_types: &IndexMap<DataTypeKey, TypedDataType>,
    tipo: &Rc<Type>,
) -> Option<ExportedDataSchema> {
    // Preferred path: use the blueprint-visible schema. This respects the
    // blueprint boundary (e.g. treats `@opaque` types as opaque) and matches
    // what `aiken blueprint` would emit.
    let mut definitions = Definitions::new();
    if let Ok(root) = Annotated::from_type(modules.into(), tipo.as_ref(), &mut definitions) {
        return Some(ExportedDataSchema { root, definitions });
    }

    // Fallback: derive the schema directly from `TypedDataType`. This is
    // essential for state-machine traces whose output domain contains opaque
    // types (e.g. `cardano/transaction.Transaction`) — the blueprint path
    // fails on opaque types, but the verify pipeline does not need to respect
    // the opaque boundary and can safely lower through `convert_opaque_type`.
    let data_type_refs = utils::indexmap::as_ref_values(data_types);
    let mut fallback_definitions = Definitions::new();
    let root = blueprint::schema_from_type::build_schema_from_type(
        tipo,
        &data_type_refs,
        &mut fallback_definitions,
    )?;
    Some(ExportedDataSchema {
        root,
        definitions: fallback_definitions,
    })
}

/// Maximum number of concrete halt witnesses to pre-compute per state-machine
/// halt test. Each witness becomes one `native_decide` instance theorem in the
/// generated Lean file, and every instance must individually prove for the
/// test to PASS.
///
/// Capped at 1 witness because Rust's UPLC machine uses `ExBudget`
/// (mem+cpu) while Lean's `cekExecuteProgram` uses a plain step count
/// (`Nat`). A seed that halts quickly under Rust's budget may exceed the
/// Lean step budget (default 20 000) on the same program, and `native_decide`
/// will then evaluate `isHaltStateBool (cek ... steps) = true` to `false`
/// — surfacing as a FAIL even though the test is actually correct.
/// Extra witnesses would give better coverage of the fuzzer's distribution
/// but increase the probability of tripping this budget mismatch on even
/// one sample; we trade that coverage for deterministic PASS/FAIL.
///
/// If the future Rust-side witness check learns the Lean step model (or
/// vice versa), this cap can be raised again without changing the Lean
/// emission side.
const MAX_CONCRETE_HALT_WITNESSES: usize = 1;
/// How many fuzzer seeds to try before giving up on finding halt witnesses.
/// Most fuzzers produce a successful run on the first seed, but state-machine
/// traces occasionally reject their own inputs (e.g. if the fuzzer body
/// asserts a post-condition that a random trace violates), so we allow more
/// seeds than the witness target.
const MAX_WITNESS_SEED_ATTEMPTS: usize = 32;

/// For a state-machine trace test whose theorem body is `proveTestsHalt`
/// (Void + FailImmediately), run the fuzzer with a handful of seeds and collect
/// the `PlutusData` inputs that drive the test body to a non-erroring halt
/// state. Each witness is returned as a CBOR hex string so it can be carried
/// through the `ExportedPropertyTest` serde export to `verify.rs`.
///
/// Returns an empty vec for any test that is not a `StateMachineTrace` +
/// `Void` + `FailImmediately` case, and also when no seed in the first
/// `MAX_WITNESS_SEED_ATTEMPTS` attempts produces a halting run.
fn maybe_compute_concrete_halt_witnesses(
    test: &PropertyTest,
    semantics: &FuzzerSemantics,
    return_mode: &TestReturnMode,
    plutus_version: &aiken_lang::plutus_version::PlutusVersion,
) -> Vec<String> {
    // Only the `proveTestsHalt` case needs witnesses — for `Bool` tests the
    // existing theorem path is already sound, and for `SucceedEventually` the
    // `proveTestsError` case has its own witness computation below.
    // `SucceedImmediately` is the explicit existential `fail once` form,
    // handled elsewhere.
    if !matches!(return_mode, TestReturnMode::Void) {
        return Vec::new();
    }
    if !matches!(
        test.on_test_failure,
        aiken_lang::ast::OnTestFailure::FailImmediately
    ) {
        return Vec::new();
    }
    if !matches!(semantics, FuzzerSemantics::StateMachineTrace { .. }) {
        return Vec::new();
    }

    collect_state_machine_trace_witnesses(test, plutus_version, /*want_error=*/ false)
}

/// For a state-machine trace test whose theorem body is `proveTestsError`
/// (Void + SucceedEventually — the `fail` annotation form), run the fuzzer
/// with a handful of seeds and collect the `PlutusData` inputs that drive the
/// test body to an erroring state. Each witness is returned as a CBOR hex
/// string so it can be carried through the `ExportedPropertyTest` serde
/// export to `verify.rs`.
///
/// Returns an empty vec for any test that is not a `StateMachineTrace` +
/// `Void` + `SucceedEventually` case, and also when no seed in the first
/// `MAX_WITNESS_SEED_ATTEMPTS` attempts produces an erroring run.
fn maybe_compute_concrete_error_witnesses(
    test: &PropertyTest,
    semantics: &FuzzerSemantics,
    return_mode: &TestReturnMode,
    plutus_version: &aiken_lang::plutus_version::PlutusVersion,
) -> Vec<String> {
    if !matches!(return_mode, TestReturnMode::Void) {
        return Vec::new();
    }
    if !matches!(
        test.on_test_failure,
        aiken_lang::ast::OnTestFailure::SucceedEventually
    ) {
        return Vec::new();
    }
    if !matches!(semantics, FuzzerSemantics::StateMachineTrace { .. }) {
        return Vec::new();
    }

    collect_state_machine_trace_witnesses(test, plutus_version, /*want_error=*/ true)
}

/// Shared seed-sweep for state-machine trace witness collection. When
/// `want_error` is false, keeps inputs that drive the test body to a
/// non-erroring halt (`halt_witnesses` semantics). When `want_error` is true,
/// keeps inputs that drive the body to an error state (`error_witnesses`
/// semantics).
///
/// Both paths share the same seed range, cap, and de-duplication behaviour —
/// the only difference is the filter on `eval.failed(false, language)`.
fn collect_state_machine_trace_witnesses(
    test: &PropertyTest,
    plutus_version: &aiken_lang::plutus_version::PlutusVersion,
    want_error: bool,
) -> Vec<String> {
    let mut witnesses: Vec<String> = Vec::new();
    for seed_offset in 0..MAX_WITNESS_SEED_ATTEMPTS {
        if witnesses.len() >= MAX_CONCRETE_HALT_WITNESSES {
            break;
        }
        let seed = crate::verify::CONCRETE_WITNESS_BASE_SEED.wrapping_add(seed_offset as u32);
        let prng = aiken_lang::test_framework::Prng::from_seed(seed);
        let Ok(Some((_next_prng, value))) = prng.sample(&test.fuzzer.program) else {
            continue;
        };
        let eval = test.eval(&value, plutus_version);
        // `failed(false, language)` returns true iff the program did NOT halt
        // cleanly (i.e. it errored, diverged, or returned a non-Unit/non-true
        // value). For halt witnesses we want `!failed(false, language)`; for
        // error witnesses we want `failed(false, language)`.
        let is_error = eval.failed(false, &plutus_version.into());
        if is_error != want_error {
            continue;
        }
        let hex = uplc::ast::Data::to_hex(value);
        if !witnesses.iter().any(|w| w == &hex) {
            witnesses.push(hex);
        }
    }
    witnesses
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct ValidatorHandlerReference {
    module: String,
    validator_name: String,
    handler_name: String,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct ModuleFunctionReference {
    module: String,
    function_name: String,
}

#[derive(Debug, Clone, PartialEq)]
struct ValidatorHandlerCall {
    module: String,
    reference: ValidatorHandlerReference,
    expression: TypedExpr,
}

#[derive(Debug)]
pub struct Source {
    pub path: PathBuf,
    pub name: String,
    pub code: String,
    pub kind: ModuleKind,
}

pub struct Checkpoint {
    module_types: HashMap<String, TypeInfo>,
    defined_modules: HashMap<String, PathBuf>,
}

#[derive(Debug, Clone)]
enum AddModuleBy {
    Source { name: String, code: String },
    Path(PathBuf),
}

pub struct Project<T>
where
    T: EventListener,
{
    config: ProjectConfig,
    defined_modules: HashMap<String, PathBuf>,
    checked_modules: CheckedModules,
    id_gen: IdGenerator,
    module_types: HashMap<String, TypeInfo>,
    root: PathBuf,
    sources: Vec<Source>,
    warnings: Vec<Warning>,
    checks_count: Option<usize>,
    event_listener: T,
    functions: IndexMap<FunctionAccessKey, TypedFunction>,
    constants: IndexMap<FunctionAccessKey, TypedExpr>,
    data_types: IndexMap<DataTypeKey, TypedDataType>,
    module_sources: HashMap<String, (String, LineNumbers)>,
    glossary: Glossary,
}

impl<T> Project<T>
where
    T: EventListener,
{
    #[allow(clippy::result_large_err)]
    pub fn new(root: PathBuf, event_listener: T) -> Result<Project<T>, Error> {
        let config = ProjectConfig::load(&root)?;

        let demanded_compiler_version = format!("v{}", config.compiler);

        let mut project = Project::new_with_config(config, root, event_listener);

        let current_compiler_version = config::compiler_version(false);

        if demanded_compiler_version != current_compiler_version {
            project.warnings.push(Warning::CompilerVersionMismatch {
                demanded: demanded_compiler_version,
                current: current_compiler_version,
            })
        }

        Ok(project)
    }

    pub fn new_with_config(config: ProjectConfig, root: PathBuf, event_listener: T) -> Project<T> {
        let id_gen = IdGenerator::new();

        let mut module_types = HashMap::new();

        module_types.insert(builtins::PRELUDE.to_string(), builtins::prelude(&id_gen));
        module_types.insert(builtins::BUILTIN.to_string(), builtins::plutus(&id_gen));

        let functions = builtins::prelude_functions(&id_gen, &module_types);

        let data_types = builtins::prelude_data_types(&id_gen);

        Project {
            config,
            checked_modules: CheckedModules::default(),
            defined_modules: HashMap::new(),
            id_gen,
            module_types,
            root,
            sources: vec![],
            warnings: vec![],
            checks_count: None,
            event_listener,
            functions,
            constants: IndexMap::new(),
            data_types,
            module_sources: HashMap::new(),
            glossary: Glossary::default(),
        }
    }

    pub fn new_generator(&'_ self, tracing: Tracing) -> CodeGenerator<'_> {
        CodeGenerator::new(
            self.config.plutus,
            utils::indexmap::as_ref_values(&self.functions),
            utils::indexmap::as_ref_values(&self.constants),
            utils::indexmap::as_ref_values(&self.data_types),
            utils::indexmap::as_str_ref_values(&self.module_types),
            utils::indexmap::as_str_ref_values(&self.module_sources),
            tracing,
        )
    }

    pub fn glossary(&self) -> &Glossary {
        &self.glossary
    }

    pub fn warnings(&mut self) -> Vec<Warning> {
        std::mem::take(&mut self.warnings)
    }

    pub fn modules(&self) -> Vec<CheckedModule> {
        self.checked_modules.values().cloned().collect()
    }

    pub fn importable_modules(&self) -> Vec<String> {
        self.module_types.keys().cloned().collect()
    }

    pub fn checkpoint(&self) -> Checkpoint {
        Checkpoint {
            module_types: self.module_types.clone(),
            defined_modules: self.defined_modules.clone(),
        }
    }

    pub fn restore(&mut self, checkpoint: Checkpoint) {
        self.module_types = checkpoint.module_types;
        self.defined_modules = checkpoint.defined_modules;
    }

    pub fn blueprint_path(&self, filepath: Option<&Path>) -> PathBuf {
        match filepath {
            Some(filepath) => filepath.to_path_buf(),
            None => self.root.join(Options::default().blueprint_path),
        }
    }

    /// Return the project root directory used for manifests, source discovery, and generated artifacts.
    pub fn root(&self) -> &Path {
        &self.root
    }

    pub fn build(
        &mut self,
        uplc: bool,
        tracing: Tracing,
        blueprint_path: PathBuf,
        blueprint_export: BlueprintExport,
        env: Option<String>,
    ) -> Result<(), Vec<Error>> {
        let options = Options {
            code_gen_mode: CodeGenMode::Build(uplc),
            tracing,
            env,
            blueprint_path,
            blueprint_export,
        };

        self.compile(options)
    }

    pub fn docs(
        &mut self,
        destination: Option<PathBuf>,
        include_dependencies: bool,
    ) -> Result<(), Vec<Error>> {
        self.event_listener
            .handle_event(Event::BuildingDocumentation {
                root: self.root.clone(),
                name: self.config.name.to_string(),
                version: self.config.version.clone(),
            });

        let config = self.config_definitions(None);

        self.read_source_files(config)?;

        let mut modules = self.parse_sources(self.config.name.clone())?;

        self.type_check(&mut modules, Tracing::silent(), None, false)?;

        let destination = destination.unwrap_or_else(|| self.root.join("docs"));

        self.event_listener.handle_event(Event::GeneratingDocFiles {
            output_path: destination.clone(),
        });

        let modules = self
            .checked_modules
            .values_mut()
            .filter(|CheckedModule { package, .. }| {
                include_dependencies || package == &self.config.name.to_string()
            })
            .map(|m| {
                m.attach_doc_and_module_comments();
                &*m
            })
            .collect();

        let doc_files = docs::generate_all(&self.root, &self.config, modules);

        for file in doc_files {
            let path = destination.join(file.path);
            fs::create_dir_all(path.parent().unwrap()).map_err(Error::from)?;
            fs::write(&path, file.content).map_err(Error::from)?;
        }

        Ok(())
    }

    #[allow(clippy::too_many_arguments)]
    pub fn check(
        &mut self,
        skip_tests: bool,
        match_tests: Option<Vec<String>>,
        verbose: bool,
        exact_match: bool,
        seed: u32,
        property_max_success: usize,
        coverage_mode: CoverageMode,
        tracing: Tracing,
        plain_numbers: bool,
        env: Option<String>,
    ) -> Result<(), Vec<Error>> {
        let options = Options {
            tracing,
            env,
            code_gen_mode: if skip_tests {
                CodeGenMode::NoOp
            } else {
                CodeGenMode::Test {
                    match_tests,
                    verbose,
                    exact_match,
                    seed,
                    property_max_success,
                    coverage_mode,
                    plain_numbers,
                }
            },
            blueprint_path: self.blueprint_path(None),
            ..Options::default()
        };

        self.compile(options)
    }

    #[allow(clippy::too_many_arguments)]
    pub fn benchmark(
        &mut self,
        match_benchmarks: Option<Vec<String>>,
        exact_match: bool,
        seed: u32,
        max_size: usize,
        tracing: Tracing,
        plain_numbers: bool,
        env: Option<String>,
    ) -> Result<(), Vec<Error>> {
        let options = Options {
            tracing,
            env,
            code_gen_mode: CodeGenMode::Benchmark {
                match_benchmarks,
                exact_match,
                seed,
                max_size,
                plain_numbers,
            },
            blueprint_path: self.blueprint_path(None),
            ..Options::default()
        };

        self.compile(options)
    }

    #[allow(clippy::result_large_err)]
    pub fn dump_uplc(&self, blueprint: &Blueprint) -> Result<(), Error> {
        let dir = self.root.join("artifacts");

        self.event_listener
            .handle_event(Event::DumpingUPLC { path: dir.clone() });

        fs::create_dir_all(&dir)?;

        for validator in &blueprint.validators {
            let path = dir.clone().join(format!("{}.uplc", validator.title));

            let program = &validator.program;
            let program: Program<Name> = program.inner().try_into().unwrap();

            fs::write(&path, program.to_pretty()).map_err(|error| Error::FileIo {
                error,
                path: Box::new(path),
            })?;
        }

        Ok(())
    }

    fn config_definitions(&mut self, env: Option<&str>) -> Option<Vec<UntypedDefinition>> {
        if !self.config.config.is_empty() {
            let env = env.unwrap_or(ast::DEFAULT_ENV_MODULE);

            match self.config.config.get(env) {
                None => {
                    self.warnings.push(Warning::NoConfigurationForEnv {
                        env: env.to_string(),
                    });
                    None
                }
                Some(config) => {
                    let mut conf_definitions = Vec::new();

                    for (identifier, value) in config.iter() {
                        conf_definitions.push(value.as_definition(identifier));
                    }

                    Some(conf_definitions)
                }
            }
        } else {
            None
        }
    }

    pub fn compile(&mut self, options: Options) -> Result<(), Vec<Error>> {
        self.event_listener
            .handle_event(Event::StartingCompilation {
                root: self.root.clone(),
                name: self.config.name.to_string(),
                version: self.config.version.clone(),
            });

        let env = options.env.as_deref();

        let config = self.config_definitions(env);

        self.read_source_files(config)?;

        let mut modules = self.parse_sources(self.config.name.clone())?;

        self.type_check(&mut modules, options.tracing, env, true)?;

        match options.code_gen_mode {
            CodeGenMode::Build(uplc_dump) => {
                self.event_listener
                    .handle_event(Event::GeneratingBlueprint {
                        path: options.blueprint_path.clone(),
                    });

                self.checked_modules.values_mut().for_each(|m| {
                    m.attach_doc_and_module_comments();
                });

                let mut generator = self.new_generator(options.tracing);

                let blueprint = Blueprint::new(
                    &self.config,
                    &self.checked_modules,
                    &mut generator,
                    options.blueprint_export == BlueprintExport::AllTypes,
                )
                .map_err(|err| Error::Blueprint(err.into()))?;

                if blueprint.validators.is_empty()
                    && matches!(
                        options.blueprint_export,
                        BlueprintExport::OnlyBinaryInterface,
                    )
                {
                    self.warnings.push(Warning::NoValidators);
                }

                if uplc_dump {
                    self.dump_uplc(&blueprint)?;
                }

                let json = serde_json::to_string_pretty(&blueprint).unwrap();

                fs::write(options.blueprint_path.as_path(), json).map_err(|error| {
                    Error::FileIo {
                        error,
                        path: Box::new(options.blueprint_path),
                    }
                    .into()
                })
            }
            CodeGenMode::Test {
                match_tests,
                verbose,
                exact_match,
                seed,
                property_max_success,
                coverage_mode,
                plain_numbers,
            } => {
                let tests =
                    self.collect_tests(verbose, match_tests, exact_match, options.tracing)?;

                if !tests.is_empty() {
                    self.event_listener.handle_event(Event::RunningTests);
                }

                let tests = self.run_runnables(tests, seed, property_max_success, options.tracing);

                self.checks_count = if tests.is_empty() {
                    None
                } else {
                    Some(tests.iter().fold(0, |acc, test| {
                        acc + match test {
                            TestResult::PropertyTestResult(r) => r.iterations,
                            _ => 1,
                        }
                    }))
                };

                let errors: Vec<Error> = tests
                    .iter()
                    .filter_map(|e| {
                        if e.is_success() {
                            None
                        } else {
                            Some(Error::from_test_result(e, verbose))
                        }
                    })
                    .collect();

                self.event_listener.handle_event(Event::FinishedTests {
                    seed,
                    coverage_mode,
                    tests,
                    plain_numbers,
                });

                if !errors.is_empty() {
                    Err(errors)
                } else {
                    Ok(())
                }
            }
            CodeGenMode::Benchmark {
                match_benchmarks,
                exact_match,
                seed,
                max_size,
                plain_numbers,
            } => {
                let verbose = false;

                let benchmarks = self.collect_benchmarks(
                    verbose,
                    match_benchmarks,
                    exact_match,
                    options.tracing,
                )?;

                if !benchmarks.is_empty() {
                    self.event_listener.handle_event(Event::RunningBenchmarks);
                }

                let benchmarks = self.run_runnables(benchmarks, seed, max_size, options.tracing);

                let errors: Vec<Error> = benchmarks
                    .iter()
                    .filter_map(|e| {
                        if e.is_success() {
                            None
                        } else {
                            Some(Error::from_test_result(e, verbose))
                        }
                    })
                    .collect();

                self.event_listener.handle_event(Event::FinishedBenchmarks {
                    seed,
                    benchmarks,
                    plain_numbers,
                });

                if !errors.is_empty() {
                    Err(errors)
                } else {
                    Ok(())
                }
            }
            CodeGenMode::NoOp => Ok(()),
        }
    }

    #[allow(clippy::result_large_err)]
    pub fn address(
        &self,
        module_name: Option<&str>,
        validator_name: Option<&str>,
        stake_address: Option<&str>,
        blueprint_path: &Path,
        mainnet: bool,
    ) -> Result<ShelleyAddress, Error> {
        // Parse stake address
        let stake_address = stake_address
            .map(|s| {
                Address::from_hex(s)
                    .or_else(|_| Address::from_bech32(s))
                    .map_err(|error| Error::MalformedStakeAddress {
                        error: Box::new(Some(error)),
                    })
                    .and_then(|addr| match addr {
                        Address::Stake(addr) => Ok(addr),
                        _ => Err(Error::MalformedStakeAddress {
                            error: Box::new(None),
                        }),
                    })
            })
            .transpose()?;
        let delegation_part = match stake_address.map(|addr| addr.payload().to_owned()) {
            None => ShelleyDelegationPart::Null,
            Some(StakePayload::Stake(key)) => ShelleyDelegationPart::Key(key),
            Some(StakePayload::Script(script)) => ShelleyDelegationPart::Script(script),
        };

        let blueprint = Self::blueprint(blueprint_path)?;

        // Calculate the address
        let when_too_many = |known_validators| {
            Error::Blueprint(
                blueprint::error::Error::MoreThanOneValidatorFound { known_validators }.into(),
            )
        };
        let when_missing = |known_validators| {
            Error::Blueprint(
                blueprint::error::Error::NoValidatorNotFound { known_validators }.into(),
            )
        };

        blueprint.with_validator(
            module_name,
            validator_name,
            when_too_many,
            when_missing,
            |validator| {
                let n = validator.parameters.len();

                if n > 0 {
                    Err(Error::Blueprint(
                        blueprint::error::Error::ParameterizedValidator { n }.into(),
                    ))
                } else {
                    let network = if mainnet {
                        Network::Mainnet
                    } else {
                        Network::Testnet
                    };

                    Ok(validator.program.inner().address(
                        network,
                        delegation_part.to_owned(),
                        &self.config.plutus.into(),
                    ))
                }
            },
        )
    }

    #[allow(clippy::result_large_err)]
    pub fn policy(
        &self,
        module_name: Option<&str>,
        validator_name: Option<&str>,
        blueprint_path: &Path,
    ) -> Result<PolicyId, Error> {
        let blueprint = Self::blueprint(blueprint_path)?;

        // Error handlers for ambiguous / missing validators
        let when_too_many = |known_validators| {
            Error::Blueprint(
                blueprint::error::Error::MoreThanOneValidatorFound { known_validators }.into(),
            )
        };
        let when_missing = |known_validators| {
            Error::Blueprint(
                blueprint::error::Error::NoValidatorNotFound { known_validators }.into(),
            )
        };

        blueprint.with_validator(
            module_name,
            validator_name,
            when_too_many,
            when_missing,
            |validator| {
                let n = validator.parameters.len();
                if n > 0 {
                    Err(Error::Blueprint(
                        blueprint::error::Error::ParameterizedValidator { n }.into(),
                    ))
                } else {
                    Ok(validator.program.compiled_code_and_hash().0)
                }
            },
        )
    }

    #[allow(clippy::result_large_err)]
    pub fn export(&self, module: &str, name: &str, tracing: Tracing) -> Result<Export, Error> {
        let checked_module =
            self.checked_modules
                .get(module)
                .ok_or_else(|| Error::ModuleNotFound {
                    module: module.to_string(),
                    known_modules: self.checked_modules.keys().cloned().collect(),
                })?;

        checked_module
            .ast
            .definitions()
            .find_map(|def| match def {
                Definition::Fn(func) if func.name == name => Some((checked_module, func)),
                _ => None,
            })
            .map(|(checked_module, func)| {
                let mut generator = self.new_generator(tracing);

                Export::from_function(
                    func,
                    checked_module,
                    &mut generator,
                    &self.checked_modules,
                    &self.config.plutus,
                )
                .map_err(|err| Error::Blueprint(err.into()))
            })
            .transpose()?
            .ok_or_else(|| Error::ExportNotFound {
                module: module.to_string(),
                name: name.to_string(),
            })
    }

    /// Compile and export property tests as a JSON-friendly manifest for external tooling.
    ///
    /// Unit tests and benchmarks are intentionally omitted; the returned [`ExportedTests`]
    /// contains only property tests plus the Plutus version needed to interpret encoded programs.
    #[allow(clippy::result_large_err)]
    pub fn export_tests(
        &mut self,
        match_tests: Option<Vec<String>>,
        exact_match: bool,
        tracing: Tracing,
        include_flat_bytes: bool,
    ) -> Result<ExportedTests, Error> {
        let plutus_version = self.config.plutus;
        let mut property_tests = self
            .collect_property_tests_with_analysis(false, match_tests, exact_match, tracing)?
            .into_iter()
            .map(|test| self.export_property_test(test, include_flat_bytes, tracing))
            .collect::<Result<Vec<_>, _>>()?;
        property_tests.sort_by(|left, right| {
            left.module
                .cmp(&right.module)
                .then(left.name.cmp(&right.name))
                .then(left.input_path.cmp(&right.input_path))
        });

        Ok(ExportedTests {
            version: crate::export::EXPORT_TESTS_VERSION.to_string(),
            plutus_version,
            property_tests,
        })
    }

    fn find_validator_definition(
        &self,
        module_name: &str,
        validator_name: &str,
        handler_name: &str,
    ) -> Option<(&CheckedModule, &TypedValidator)> {
        let checked_module = self.checked_modules.get(module_name)?;

        checked_module
            .ast
            .definitions()
            .find_map(|definition| match definition {
                Definition::Validator(validator)
                    if validator.name == validator_name
                        && (validator
                            .handlers
                            .iter()
                            .any(|handler| handler.name == handler_name)
                            || validator.fallback.name == handler_name) =>
                {
                    Some((checked_module, validator))
                }
                _ => None,
            })
    }

    fn find_function_definition(
        &self,
        module_name: &str,
        function_name: &str,
    ) -> Option<&TypedFunction> {
        let checked_module = self.checked_modules.get(module_name)?;

        checked_module
            .ast
            .definitions()
            .find_map(|definition| match definition {
                Definition::Fn(function) if function.name == function_name => Some(function),
                _ => None,
            })
    }

    fn function_reference_from_expr(&self, expr: &TypedExpr) -> Option<ModuleFunctionReference> {
        match expr {
            TypedExpr::Var { constructor, .. } => {
                if let ValueConstructorVariant::ModuleFn { module, name, .. } = &constructor.variant
                {
                    Some(ModuleFunctionReference {
                        module: module.clone(),
                        function_name: name.clone(),
                    })
                } else {
                    None
                }
            }
            TypedExpr::ModuleSelect { constructor, .. } => {
                if let ModuleValueConstructor::Fn { module, name, .. } = constructor {
                    Some(ModuleFunctionReference {
                        module: module.clone(),
                        function_name: name.clone(),
                    })
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    fn validator_handler_reference_from_function_reference(
        &self,
        function_reference: &ModuleFunctionReference,
    ) -> Option<ValidatorHandlerReference> {
        let (validator_name, handler_name) = function_reference.function_name.split_once('.')?;

        Some(ValidatorHandlerReference {
            module: function_reference.module.clone(),
            validator_name: validator_name.to_string(),
            handler_name: handler_name.to_string(),
        })
    }

    fn substitute_local_variables(
        &self,
        expr: &TypedExpr,
        substitutions: &HashMap<String, TypedExpr>,
    ) -> TypedExpr {
        match expr {
            TypedExpr::Var {
                constructor, name, ..
            } => {
                if matches!(
                    constructor.variant,
                    ValueConstructorVariant::LocalVariable { .. }
                ) && let Some(substituted) = substitutions.get(name)
                {
                    substituted.clone()
                } else {
                    expr.clone()
                }
            }
            TypedExpr::Sequence {
                location,
                expressions,
            } => TypedExpr::Sequence {
                location: *location,
                expressions: expressions
                    .iter()
                    .map(|expression| self.substitute_local_variables(expression, substitutions))
                    .collect(),
            },
            TypedExpr::Pipeline {
                location,
                expressions,
            } => TypedExpr::Pipeline {
                location: *location,
                expressions: expressions
                    .iter()
                    .map(|expression| self.substitute_local_variables(expression, substitutions))
                    .collect(),
            },
            TypedExpr::Fn {
                location,
                tipo,
                is_capture,
                args,
                body,
                return_annotation,
            } => {
                let mut scoped_substitutions = substitutions.clone();
                for arg in args {
                    if let Some(name) = arg.get_variable_name() {
                        scoped_substitutions.remove(name);
                    }
                }

                TypedExpr::Fn {
                    location: *location,
                    tipo: tipo.clone(),
                    is_capture: *is_capture,
                    args: args.clone(),
                    body: Box::new(self.substitute_local_variables(body, &scoped_substitutions)),
                    return_annotation: return_annotation.clone(),
                }
            }
            TypedExpr::List {
                location,
                tipo,
                elements,
                tail,
            } => TypedExpr::List {
                location: *location,
                tipo: tipo.clone(),
                elements: elements
                    .iter()
                    .map(|element| self.substitute_local_variables(element, substitutions))
                    .collect(),
                tail: tail
                    .as_ref()
                    .map(|tail| Box::new(self.substitute_local_variables(tail, substitutions))),
            },
            TypedExpr::Call {
                location,
                tipo,
                fun,
                args,
            } => TypedExpr::Call {
                location: *location,
                tipo: tipo.clone(),
                fun: Box::new(self.substitute_local_variables(fun, substitutions)),
                args: args
                    .iter()
                    .map(|arg| ast::CallArg {
                        label: arg.label.clone(),
                        location: arg.location,
                        value: self.substitute_local_variables(&arg.value, substitutions),
                    })
                    .collect(),
            },
            TypedExpr::BinOp {
                location,
                tipo,
                name,
                left,
                right,
            } => TypedExpr::BinOp {
                location: *location,
                tipo: tipo.clone(),
                name: *name,
                left: Box::new(self.substitute_local_variables(left, substitutions)),
                right: Box::new(self.substitute_local_variables(right, substitutions)),
            },
            TypedExpr::Assignment {
                location,
                tipo,
                value,
                pattern,
                kind,
                comment,
            } => TypedExpr::Assignment {
                location: *location,
                tipo: tipo.clone(),
                value: Box::new(self.substitute_local_variables(value, substitutions)),
                pattern: pattern.clone(),
                kind: *kind,
                comment: comment.clone(),
            },
            TypedExpr::Trace {
                location,
                tipo,
                then,
                text,
            } => TypedExpr::Trace {
                location: *location,
                tipo: tipo.clone(),
                then: Box::new(self.substitute_local_variables(then, substitutions)),
                text: Box::new(self.substitute_local_variables(text, substitutions)),
            },
            TypedExpr::When {
                location,
                tipo,
                subject,
                clauses,
            } => TypedExpr::When {
                location: *location,
                tipo: tipo.clone(),
                subject: Box::new(self.substitute_local_variables(subject, substitutions)),
                clauses: clauses
                    .iter()
                    .map(|clause| {
                        let mut clause = clause.clone();
                        let mut scoped_substitutions = substitutions.clone();
                        Self::remove_pattern_bound_substitutions(
                            &clause.pattern,
                            &mut scoped_substitutions,
                        );
                        clause.then =
                            self.substitute_local_variables(&clause.then, &scoped_substitutions);
                        clause
                    })
                    .collect(),
            },
            TypedExpr::If {
                location,
                branches,
                final_else,
                tipo,
            } => {
                let mut substituted_branches = branches.clone();
                for branch in substituted_branches.iter_mut() {
                    branch.condition =
                        self.substitute_local_variables(&branch.condition, substitutions);
                    let mut scoped_substitutions = substitutions.clone();

                    if let Some((pattern, _)) = &branch.is {
                        Self::remove_pattern_bound_substitutions(
                            pattern,
                            &mut scoped_substitutions,
                        );
                    }

                    branch.body =
                        self.substitute_local_variables(&branch.body, &scoped_substitutions);
                }

                TypedExpr::If {
                    location: *location,
                    branches: substituted_branches,
                    final_else: Box::new(
                        self.substitute_local_variables(final_else, substitutions),
                    ),
                    tipo: tipo.clone(),
                }
            }
            TypedExpr::RecordAccess {
                location,
                tipo,
                label,
                index,
                record,
            } => TypedExpr::RecordAccess {
                location: *location,
                tipo: tipo.clone(),
                label: label.clone(),
                index: *index,
                record: Box::new(self.substitute_local_variables(record, substitutions)),
            },
            TypedExpr::Tuple {
                location,
                tipo,
                elems,
            } => TypedExpr::Tuple {
                location: *location,
                tipo: tipo.clone(),
                elems: elems
                    .iter()
                    .map(|element| self.substitute_local_variables(element, substitutions))
                    .collect(),
            },
            TypedExpr::Pair {
                location,
                tipo,
                fst,
                snd,
            } => TypedExpr::Pair {
                location: *location,
                tipo: tipo.clone(),
                fst: Box::new(self.substitute_local_variables(fst, substitutions)),
                snd: Box::new(self.substitute_local_variables(snd, substitutions)),
            },
            TypedExpr::TupleIndex {
                location,
                tipo,
                index,
                tuple,
            } => TypedExpr::TupleIndex {
                location: *location,
                tipo: tipo.clone(),
                index: *index,
                tuple: Box::new(self.substitute_local_variables(tuple, substitutions)),
            },
            TypedExpr::RecordUpdate {
                location,
                tipo,
                spread,
                args,
            } => TypedExpr::RecordUpdate {
                location: *location,
                tipo: tipo.clone(),
                spread: Box::new(self.substitute_local_variables(spread, substitutions)),
                args: args
                    .iter()
                    .map(|arg| ast::TypedRecordUpdateArg {
                        label: arg.label.clone(),
                        location: arg.location,
                        value: self.substitute_local_variables(&arg.value, substitutions),
                        index: arg.index,
                    })
                    .collect(),
            },
            TypedExpr::UnOp {
                location,
                value,
                tipo,
                op,
            } => TypedExpr::UnOp {
                location: *location,
                value: Box::new(self.substitute_local_variables(value, substitutions)),
                tipo: tipo.clone(),
                op: *op,
            },
            TypedExpr::UInt { .. }
            | TypedExpr::String { .. }
            | TypedExpr::ByteArray { .. }
            | TypedExpr::CurvePoint { .. }
            | TypedExpr::ErrorTerm { .. }
            | TypedExpr::ModuleSelect { .. } => expr.clone(),
        }
    }

    fn collect_pattern_bound_names(pattern: &ast::TypedPattern, bound_names: &mut Vec<String>) {
        match pattern {
            ast::Pattern::Var { name, .. } => bound_names.push(name.clone()),
            ast::Pattern::Assign { name, pattern, .. } => {
                bound_names.push(name.clone());
                Self::collect_pattern_bound_names(pattern, bound_names);
            }
            ast::Pattern::List { elements, tail, .. } => {
                for element in elements {
                    Self::collect_pattern_bound_names(element, bound_names);
                }
                if let Some(tail) = tail {
                    Self::collect_pattern_bound_names(tail, bound_names);
                }
            }
            ast::Pattern::Pair { fst, snd, .. } => {
                Self::collect_pattern_bound_names(fst, bound_names);
                Self::collect_pattern_bound_names(snd, bound_names);
            }
            ast::Pattern::Tuple { elems, .. } => {
                for element in elems {
                    Self::collect_pattern_bound_names(element, bound_names);
                }
            }
            ast::Pattern::Constructor { arguments, .. } => {
                for argument in arguments {
                    Self::collect_pattern_bound_names(&argument.value, bound_names);
                }
            }
            ast::Pattern::Int { .. }
            | ast::Pattern::ByteArray { .. }
            | ast::Pattern::Discard { .. } => {}
        }
    }

    fn collect_pattern_bound_names_with_types(
        pattern: &ast::TypedPattern,
        tipo: Rc<Type>,
        bound_names: &mut Vec<(String, Rc<Type>)>,
    ) {
        let tipo = Type::collapse_links(tipo);

        match pattern {
            ast::Pattern::Var { name, .. } => bound_names.push((name.clone(), tipo)),
            ast::Pattern::Assign { name, pattern, .. } => {
                bound_names.push((name.clone(), tipo.clone()));
                Self::collect_pattern_bound_names_with_types(pattern, tipo, bound_names);
            }
            ast::Pattern::List { elements, tail, .. } => {
                if let Type::App {
                    module,
                    name: type_name,
                    args,
                    ..
                } = tipo.as_ref()
                    && module.is_empty()
                    && type_name == "List"
                    && let Some(element_tipo) = args.first()
                {
                    for element in elements {
                        Self::collect_pattern_bound_names_with_types(
                            element,
                            element_tipo.clone(),
                            bound_names,
                        );
                    }
                    if let Some(tail) = tail {
                        Self::collect_pattern_bound_names_with_types(
                            tail,
                            tipo.clone(),
                            bound_names,
                        );
                    }
                }
            }
            ast::Pattern::Pair { fst, snd, .. } => {
                if let Type::Pair {
                    fst: fst_tipo,
                    snd: snd_tipo,
                    ..
                } = tipo.as_ref()
                {
                    Self::collect_pattern_bound_names_with_types(
                        fst,
                        fst_tipo.clone(),
                        bound_names,
                    );
                    Self::collect_pattern_bound_names_with_types(
                        snd,
                        snd_tipo.clone(),
                        bound_names,
                    );
                }
            }
            ast::Pattern::Tuple { elems, .. } => {
                if let Type::Tuple {
                    elems: tuple_element_tipos,
                    ..
                } = tipo.as_ref()
                {
                    for (element, element_tipo) in elems.iter().zip(tuple_element_tipos.iter()) {
                        Self::collect_pattern_bound_names_with_types(
                            element,
                            element_tipo.clone(),
                            bound_names,
                        );
                    }
                }
            }
            ast::Pattern::Constructor {
                arguments,
                tipo: constructor_tipo,
                ..
            } => {
                let constructor_tipo = Type::collapse_links(constructor_tipo.clone());
                if let Type::Fn {
                    args: argument_tipos,
                    ..
                } = constructor_tipo.as_ref()
                {
                    for (argument, argument_tipo) in arguments.iter().zip(argument_tipos.iter()) {
                        Self::collect_pattern_bound_names_with_types(
                            &argument.value,
                            argument_tipo.clone(),
                            bound_names,
                        );
                    }
                }
            }
            ast::Pattern::Int { .. }
            | ast::Pattern::ByteArray { .. }
            | ast::Pattern::Discard { .. } => {}
        }
    }

    fn expression_for_pattern_bound_name(
        pattern: &ast::TypedPattern,
        value: &TypedExpr,
        name: &str,
        tipo: Rc<Type>,
    ) -> TypedExpr {
        match pattern {
            ast::Pattern::Var {
                name: pattern_name, ..
            }
            | ast::Pattern::Assign {
                name: pattern_name, ..
            } if pattern_name == name => value.clone(),
            _ => {
                let location = value.location();

                TypedExpr::When {
                    location,
                    tipo: tipo.clone(),
                    subject: Box::new(value.clone()),
                    clauses: vec![ast::TypedClause {
                        location: pattern.location(),
                        pattern: pattern.clone(),
                        then: TypedExpr::local_var(name, tipo, pattern.location()),
                    }],
                }
            }
        }
    }

    fn remove_pattern_bound_substitutions(
        pattern: &ast::TypedPattern,
        substitutions: &mut HashMap<String, TypedExpr>,
    ) {
        let mut bound_names = Vec::new();
        Self::collect_pattern_bound_names(pattern, &mut bound_names);

        for name in bound_names {
            substitutions.remove(&name);
        }
    }

    fn update_substitutions_from_assignment(
        &self,
        pattern: &ast::TypedPattern,
        value: &TypedExpr,
        substitutions: &mut HashMap<String, TypedExpr>,
    ) {
        Self::remove_pattern_bound_substitutions(pattern, substitutions);

        let mut bound_names = Vec::new();
        Self::collect_pattern_bound_names_with_types(pattern, value.tipo(), &mut bound_names);

        for (name, tipo) in bound_names {
            let bound_expression =
                Self::expression_for_pattern_bound_name(pattern, value, &name, tipo);
            substitutions.insert(name, bound_expression);
        }
    }

    #[allow(clippy::too_many_arguments)]
    fn collect_validator_handler_calls_from_expr(
        &self,
        expr: &TypedExpr,
        current_module: &str,
        substitutions: &HashMap<String, TypedExpr>,
        calls: &mut Vec<ValidatorHandlerCall>,
        visited_functions: &mut HashSet<ModuleFunctionReference>,
        saw_control_flow_dependent_handler_call: &mut bool,
        control_flow_dependent: bool,
        direct_result_position: bool,
    ) {
        match expr {
            TypedExpr::Sequence { expressions, .. } | TypedExpr::Pipeline { expressions, .. } => {
                let mut scoped_substitutions = substitutions.clone();

                for (index, expression) in expressions.iter().enumerate() {
                    let expression_is_tail = index + 1 == expressions.len();
                    self.collect_validator_handler_calls_from_expr(
                        expression,
                        current_module,
                        &scoped_substitutions,
                        calls,
                        visited_functions,
                        saw_control_flow_dependent_handler_call,
                        control_flow_dependent,
                        direct_result_position && expression_is_tail,
                    );

                    if let TypedExpr::Assignment { pattern, value, .. } = expression {
                        let substituted_value =
                            self.substitute_local_variables(value, &scoped_substitutions);
                        self.update_substitutions_from_assignment(
                            pattern,
                            &substituted_value,
                            &mut scoped_substitutions,
                        );
                    }
                }
            }
            TypedExpr::Fn { args, body, .. } => {
                let mut scoped_substitutions = substitutions.clone();

                for arg in args {
                    if let Some(name) = arg.get_variable_name() {
                        scoped_substitutions.remove(name);
                    }
                }

                self.collect_validator_handler_calls_from_expr(
                    body,
                    current_module,
                    &scoped_substitutions,
                    calls,
                    visited_functions,
                    saw_control_flow_dependent_handler_call,
                    control_flow_dependent,
                    false,
                );
            }
            TypedExpr::List { elements, tail, .. } => {
                for element in elements {
                    self.collect_validator_handler_calls_from_expr(
                        element,
                        current_module,
                        substitutions,
                        calls,
                        visited_functions,
                        saw_control_flow_dependent_handler_call,
                        control_flow_dependent,
                        false,
                    );
                }
                if let Some(tail) = tail {
                    self.collect_validator_handler_calls_from_expr(
                        tail,
                        current_module,
                        substitutions,
                        calls,
                        visited_functions,
                        saw_control_flow_dependent_handler_call,
                        control_flow_dependent,
                        false,
                    );
                }
            }
            TypedExpr::Call {
                location,
                tipo,
                fun,
                args,
            } => {
                let substituted_fun = self.substitute_local_variables(fun, substitutions);

                if let Some(function_reference) =
                    self.function_reference_from_expr(&substituted_fun)
                {
                    if let Some(reference) = self
                        .validator_handler_reference_from_function_reference(&function_reference)
                    {
                        if !control_flow_dependent && direct_result_position {
                            let call_expression = TypedExpr::Call {
                                location: *location,
                                tipo: tipo.clone(),
                                fun: Box::new(substituted_fun.clone()),
                                args: args
                                    .iter()
                                    .map(|arg| ast::CallArg {
                                        label: arg.label.clone(),
                                        location: arg.location,
                                        value: self
                                            .substitute_local_variables(&arg.value, substitutions),
                                    })
                                    .collect(),
                            };

                            let call = ValidatorHandlerCall {
                                module: current_module.to_string(),
                                reference,
                                expression: call_expression,
                            };

                            calls.push(call);
                        } else {
                            if control_flow_dependent {
                                *saw_control_flow_dependent_handler_call = true;
                            }
                        }
                    } else if let Some(function) = self.find_function_definition(
                        &function_reference.module,
                        &function_reference.function_name,
                    ) && args.len() == function.arguments.len()
                        && visited_functions.insert(function_reference.clone())
                    {
                        let mut call_substitutions = HashMap::new();

                        for (argument, parameter) in args.iter().zip(function.arguments.iter()) {
                            if let Some(parameter_name) = parameter.get_variable_name() {
                                call_substitutions.insert(
                                    parameter_name.to_string(),
                                    self.substitute_local_variables(&argument.value, substitutions),
                                );
                            }
                        }

                        self.collect_validator_handler_calls_from_expr(
                            &function.body,
                            &function_reference.module,
                            &call_substitutions,
                            calls,
                            visited_functions,
                            saw_control_flow_dependent_handler_call,
                            control_flow_dependent,
                            direct_result_position,
                        );

                        visited_functions.remove(&function_reference);
                    }
                }

                self.collect_validator_handler_calls_from_expr(
                    fun,
                    current_module,
                    substitutions,
                    calls,
                    visited_functions,
                    saw_control_flow_dependent_handler_call,
                    control_flow_dependent,
                    false,
                );
                for arg in args {
                    self.collect_validator_handler_calls_from_expr(
                        &arg.value,
                        current_module,
                        substitutions,
                        calls,
                        visited_functions,
                        saw_control_flow_dependent_handler_call,
                        control_flow_dependent,
                        false,
                    );
                }
            }
            TypedExpr::BinOp {
                name, left, right, ..
            } => {
                let right_control_flow_dependent =
                    control_flow_dependent || matches!(name, ast::BinOp::And | ast::BinOp::Or);

                self.collect_validator_handler_calls_from_expr(
                    left,
                    current_module,
                    substitutions,
                    calls,
                    visited_functions,
                    saw_control_flow_dependent_handler_call,
                    control_flow_dependent,
                    false,
                );
                self.collect_validator_handler_calls_from_expr(
                    right,
                    current_module,
                    substitutions,
                    calls,
                    visited_functions,
                    saw_control_flow_dependent_handler_call,
                    right_control_flow_dependent,
                    false,
                );
            }
            TypedExpr::Assignment { value, .. } => {
                self.collect_validator_handler_calls_from_expr(
                    value,
                    current_module,
                    substitutions,
                    calls,
                    visited_functions,
                    saw_control_flow_dependent_handler_call,
                    control_flow_dependent,
                    false,
                );
            }
            TypedExpr::Trace { text, then, .. } => {
                self.collect_validator_handler_calls_from_expr(
                    text,
                    current_module,
                    substitutions,
                    calls,
                    visited_functions,
                    saw_control_flow_dependent_handler_call,
                    control_flow_dependent,
                    false,
                );
                self.collect_validator_handler_calls_from_expr(
                    then,
                    current_module,
                    substitutions,
                    calls,
                    visited_functions,
                    saw_control_flow_dependent_handler_call,
                    control_flow_dependent,
                    direct_result_position,
                );
            }
            TypedExpr::When {
                subject, clauses, ..
            } => {
                self.collect_validator_handler_calls_from_expr(
                    subject,
                    current_module,
                    substitutions,
                    calls,
                    visited_functions,
                    saw_control_flow_dependent_handler_call,
                    true,
                    false,
                );
                for clause in clauses {
                    let mut scoped_substitutions = substitutions.clone();
                    Self::remove_pattern_bound_substitutions(
                        &clause.pattern,
                        &mut scoped_substitutions,
                    );
                    self.collect_validator_handler_calls_from_expr(
                        &clause.then,
                        current_module,
                        &scoped_substitutions,
                        calls,
                        visited_functions,
                        saw_control_flow_dependent_handler_call,
                        true,
                        direct_result_position,
                    );
                }
            }
            TypedExpr::If {
                branches,
                final_else,
                ..
            } => {
                for branch in branches {
                    self.collect_validator_handler_calls_from_expr(
                        &branch.condition,
                        current_module,
                        substitutions,
                        calls,
                        visited_functions,
                        saw_control_flow_dependent_handler_call,
                        true,
                        false,
                    );

                    let mut scoped_substitutions = substitutions.clone();
                    if let Some((pattern, _)) = &branch.is {
                        Self::remove_pattern_bound_substitutions(
                            pattern,
                            &mut scoped_substitutions,
                        );
                    }

                    self.collect_validator_handler_calls_from_expr(
                        &branch.body,
                        current_module,
                        &scoped_substitutions,
                        calls,
                        visited_functions,
                        saw_control_flow_dependent_handler_call,
                        true,
                        direct_result_position,
                    );
                }
                self.collect_validator_handler_calls_from_expr(
                    final_else,
                    current_module,
                    substitutions,
                    calls,
                    visited_functions,
                    saw_control_flow_dependent_handler_call,
                    true,
                    direct_result_position,
                );
            }
            TypedExpr::RecordAccess { record, .. } => {
                self.collect_validator_handler_calls_from_expr(
                    record,
                    current_module,
                    substitutions,
                    calls,
                    visited_functions,
                    saw_control_flow_dependent_handler_call,
                    control_flow_dependent,
                    false,
                );
            }
            TypedExpr::Tuple { elems, .. } => {
                for element in elems {
                    self.collect_validator_handler_calls_from_expr(
                        element,
                        current_module,
                        substitutions,
                        calls,
                        visited_functions,
                        saw_control_flow_dependent_handler_call,
                        control_flow_dependent,
                        false,
                    );
                }
            }
            TypedExpr::Pair { fst, snd, .. } => {
                self.collect_validator_handler_calls_from_expr(
                    fst,
                    current_module,
                    substitutions,
                    calls,
                    visited_functions,
                    saw_control_flow_dependent_handler_call,
                    control_flow_dependent,
                    false,
                );
                self.collect_validator_handler_calls_from_expr(
                    snd,
                    current_module,
                    substitutions,
                    calls,
                    visited_functions,
                    saw_control_flow_dependent_handler_call,
                    control_flow_dependent,
                    false,
                );
            }
            TypedExpr::TupleIndex { tuple, .. } => {
                self.collect_validator_handler_calls_from_expr(
                    tuple,
                    current_module,
                    substitutions,
                    calls,
                    visited_functions,
                    saw_control_flow_dependent_handler_call,
                    control_flow_dependent,
                    false,
                );
            }
            TypedExpr::RecordUpdate { spread, args, .. } => {
                self.collect_validator_handler_calls_from_expr(
                    spread,
                    current_module,
                    substitutions,
                    calls,
                    visited_functions,
                    saw_control_flow_dependent_handler_call,
                    control_flow_dependent,
                    false,
                );
                for arg in args {
                    self.collect_validator_handler_calls_from_expr(
                        &arg.value,
                        current_module,
                        substitutions,
                        calls,
                        visited_functions,
                        saw_control_flow_dependent_handler_call,
                        control_flow_dependent,
                        false,
                    );
                }
            }
            TypedExpr::UnOp { value, .. } => {
                self.collect_validator_handler_calls_from_expr(
                    value,
                    current_module,
                    substitutions,
                    calls,
                    visited_functions,
                    saw_control_flow_dependent_handler_call,
                    control_flow_dependent,
                    false,
                );
            }
            TypedExpr::Var { name, .. } => {
                if direct_result_position && let Some(substituted_expr) = substitutions.get(name) {
                    let same_variable_alias = matches!(
                        substituted_expr,
                        TypedExpr::Var {
                            name: substituted_name,
                            ..
                        } if substituted_name == name
                    );

                    if !same_variable_alias {
                        self.collect_validator_handler_calls_from_expr(
                            substituted_expr,
                            current_module,
                            substitutions,
                            calls,
                            visited_functions,
                            saw_control_flow_dependent_handler_call,
                            control_flow_dependent,
                            true,
                        );
                    }
                }
            }
            TypedExpr::ModuleSelect { .. } => {}
            TypedExpr::UInt { .. }
            | TypedExpr::String { .. }
            | TypedExpr::ByteArray { .. }
            | TypedExpr::CurvePoint { .. }
            | TypedExpr::ErrorTerm { .. } => {}
        }
    }

    fn infer_validator_handler_call(
        &self,
        module_name: &str,
        test_body: &TypedExpr,
    ) -> Option<ValidatorHandlerCall> {
        let mut calls = Vec::new();
        let mut visited_functions = HashSet::new();
        let mut saw_control_flow_dependent_handler_call = false;

        self.collect_validator_handler_calls_from_expr(
            test_body,
            module_name,
            &HashMap::new(),
            &mut calls,
            &mut visited_functions,
            &mut saw_control_flow_dependent_handler_call,
            false,
            true,
        );

        if calls.len() == 1 && !saw_control_flow_dependent_handler_call {
            calls.pop()
        } else {
            None
        }
    }

    fn stripped_test_arguments(&self, test_arguments: &[ast::TypedArgVia]) -> Vec<ast::TypedArg> {
        let data_types = utils::indexmap::as_ref_values(&self.data_types);

        test_arguments
            .iter()
            .cloned()
            .map(|arg_via| {
                let mut arg: ast::TypedArg = arg_via.into();
                arg.tipo = tipo::convert_opaque_type(&arg.tipo, &data_types, true);
                arg
            })
            .collect()
    }

    #[allow(clippy::result_large_err)]
    fn infer_validator_target(
        &self,
        module_name: &str,
        test_name: &str,
        include_flat_bytes: bool,
        tracing: Tracing,
    ) -> Result<Option<ValidatorTarget>, Error> {
        let checked_module = match self.checked_modules.get(module_name) {
            Some(module) => module,
            None => return Ok(None),
        };

        let test_definition =
            checked_module
                .ast
                .definitions()
                .find_map(|definition| match definition {
                    Definition::Test(test) if test.name == test_name => Some(test),
                    _ => None,
                });

        let Some(test_definition) = test_definition else {
            return Ok(None);
        };

        let Some(handler_call) =
            self.infer_validator_handler_call(module_name, &test_definition.body)
        else {
            return Ok(None);
        };

        let Some((_handler_module, _validator)) = self.find_validator_definition(
            &handler_call.reference.module,
            &handler_call.reference.validator_name,
            &handler_call.reference.handler_name,
        ) else {
            return Ok(None);
        };

        let mut generator = self.new_generator(tracing);
        let test_arguments = self.stripped_test_arguments(&test_definition.arguments);
        let handler_program = generator.generate_raw(
            &handler_call.expression,
            &test_arguments,
            &handler_call.module,
        );
        let handler_program = handler_program
            .to_debruijn()
            .map_err(|e| Error::debruijn(e.to_string()))?;

        let handler_hex = handler_program
            .to_hex()
            .map_err(|e| Error::flat_encode(e.to_string()))?;

        let handler_flat_bytes = if include_flat_bytes {
            Some(
                handler_program
                    .to_flat()
                    .map_err(|e| Error::flat_encode(e.to_string()))?,
            )
        } else {
            None
        };

        let validator_module = handler_call.reference.module;
        let validator_name = handler_call.reference.validator_name;
        let handler_name = handler_call.reference.handler_name;

        Ok(Some(ValidatorTarget {
            validator_module,
            validator_name: validator_name.clone(),
            handler_name: Some(format!("{validator_name}.{handler_name}")),
            handler_program: Some(ExportedProgram {
                hex: handler_hex,
                flat_bytes: handler_flat_bytes,
            }),
        }))
    }

    fn export_property_test(
        &self,
        test: AnalyzedPropertyTest,
        include_flat_bytes: bool,
        tracing: Tracing,
    ) -> Result<ExportedPropertyTest, Error> {
        let AnalyzedPropertyTest { test, analysis, .. } = test;

        let validator_target =
            self.infer_validator_target(&test.module, &test.name, include_flat_bytes, tracing)?;

        let mut printer = tipo::pretty::Printer::new();
        let fuzzer_type = printer.print(&test.fuzzer.type_info).to_pretty_string(80);

        // Clone so we can also run the test below to collect concrete halt
        // witnesses after consuming `test.program` / `test.fuzzer.program`
        // via `to_debruijn`.
        let test_for_witnesses = test.clone();

        let test_program = test
            .program
            .to_debruijn()
            .map_err(|e| Error::debruijn(e.to_string()))?;

        let fuzzer_program = test
            .fuzzer
            .program
            .to_debruijn()
            .map_err(|e| Error::debruijn(e.to_string()))?;

        let test_hex = test_program
            .to_hex()
            .map_err(|e| Error::flat_encode(e.to_string()))?;

        let fuzzer_hex = fuzzer_program
            .to_hex()
            .map_err(|e| Error::flat_encode(e.to_string()))?;

        let test_flat_bytes = if include_flat_bytes {
            Some(
                test_program
                    .to_flat()
                    .map_err(|e| Error::flat_encode(e.to_string()))?,
            )
        } else {
            None
        };

        let fuzzer_flat_bytes = if include_flat_bytes {
            Some(
                fuzzer_program
                    .to_flat()
                    .map_err(|e| Error::flat_encode(e.to_string()))?,
            )
        } else {
            None
        };

        let fuzzer_output_type = fuzzer_output_type_from(&test.fuzzer.stripped_type_info);

        let constraint = convert_constraint(&analysis.fuzzer.constraint);

        // Debug dump: when `AIKEN_DUMP_TRANSITION_PROP` is set, print the
        // `TransitionProp` captured for each state-machine trace test before
        // it is dropped at the export boundary. Useful for inspecting the
        // structure produced by `typed_expr_to_transition_prop` (Issue S3)
        // without needing a new export schema.
        if std::env::var_os("AIKEN_DUMP_TRANSITION_PROP").is_some()
            && let LangFuzzerSemantics::StateMachineTrace {
                transition_prop: Some(prop),
                ..
            } = &analysis.fuzzer.semantics
        {
            eprintln!(
                "[AIKEN_DUMP_TRANSITION_PROP] {}.{}:\n{:#?}",
                test.module, test.name, prop
            );
        }

        let semantics = convert_semantics(&analysis.fuzzer.semantics);
        let fuzzer_data_schema = export_data_schema(
            &self.checked_modules,
            &self.data_types,
            &test.fuzzer.stripped_type_info,
        );

        // Pre-compute structural schemas for every `DataWithSchema { type_name }`
        // leaf in `semantics` so that `verify.rs` can emit concrete per-type
        // predicates for the state / event / output domains of state-machine
        // traces, rather than defaulting to a vacuous `True` placeholder.
        let test_qualified_name = format!("{}.{}", test.module, test.name);
        let mut inner_data_schemas = collect_inner_data_schemas(
            &self.checked_modules,
            &self.data_types,
            &semantics,
            &test_qualified_name,
        )?;

        let return_mode = if analysis.return_type.is_void() {
            TestReturnMode::Void
        } else {
            TestReturnMode::Bool
        };
        let target_kind = if validator_target.is_some() {
            VerificationTargetKind::ValidatorHandler
        } else {
            VerificationTargetKind::PropertyWrapper
        };

        // For state-machine trace tests whose theorem body would be
        // `proveTestsHalt` (Void + FailImmediately), the universal theorem over
        // the reachability precondition is unsound because reachability
        // over-approximates the step function. Instead, pre-compute a small set
        // of concrete halt witnesses by running the fuzzer and test body with
        // seeded PRNGs; `verify.rs` emits `native_decide` instance theorems
        // from these witnesses. See `verify.rs::generate_instance_proof_file`.
        // S4 — pre-emit the `isValidTransition` Lean text from the
        // aiken-lang `TransitionProp` while the non-serializable tree is
        // still in scope. `verify.rs` consumes the resulting string at
        // proof-file generation time.
        let transition_prop_lean: Option<crate::export::ExportedTransitionProp> =
            match &analysis.fuzzer.semantics {
                LangFuzzerSemantics::StateMachineTrace {
                    transition_prop: Some(prop),
                    initial_state_shallow_ir,
                    ..
                } => {
                    extend_inner_data_schemas_from_transition_prop(
                        &self.checked_modules,
                        &self.data_types,
                        prop,
                        &test_qualified_name,
                        &mut inner_data_schemas,
                    )?;
                    // Use a placeholder function name; `verify.rs`
                    // rewrites `__FN__` into the actual prefixed name at
                    // proof-file generation time (the prefix depends on
                    // `lean_test_name`, which is sanitised there).
                    let (def, widenings, sub_gens, s0002_marker) =
                        crate::verify::emit_is_valid_transition_details_for_export_with_schemas(
                            "__FN__",
                            prop,
                            &test.name,
                            &inner_data_schemas,
                        );
                    let (initial_state_lean, helper_widenings) = initial_state_shallow_ir
                        .as_ref()
                        .map(|ir| {
                            let (body, existentials, mut helper_widenings) =
                                crate::verify::emit_initial_state_as_lean(ir);
                            // If existentials leaked through, discard the
                            // literal — the initial-state path needs a closed
                            // `Data` value. Record the widening explicitly so
                            // verify-side status gating can reject production
                            // `Partial`/`Proved` results that rely on the
                            // fallback existential.
                            if existentials.is_empty() {
                                (Some(body), helper_widenings)
                            } else {
                                helper_widenings.push(crate::export::TransitionWidening {
                                    kind: crate::export::TransitionWideningKind::DataFreshening,
                                    message: "initial-state helper could not emit a closed Data literal; isValidTrace falls back to an unconstrained initial-state witness".to_string(),
                                });
                                (None, helper_widenings)
                            }
                        })
                        .unwrap_or_else(|| (None, Vec::new()));
                    let unsupported_log =
                        transition_unsupported_log(&widenings, &helper_widenings);

                    // M3: pre-compute structural vacuity from the source
                    // AST.  The rendered Lean text we ship in
                    // `is_valid_transition_def` is opaque to the
                    // dispatcher (it would otherwise have to re-parse it
                    // via fragile string surgery — exactly the drift
                    // class M3 eliminates).  Doing the check here once
                    // keeps the `TransitionProp` AST and the verdict
                    // tightly coupled at the only site where both are
                    // in scope at the same time.
                    let is_vacuous = aiken_lang::test_framework::transition_prop_is_vacuous(prop);
                    Some(crate::export::ExportedTransitionProp {
                        is_valid_transition_def: def,
                        initial_state_lean,
                        helper_widenings,
                        widenings,
                        unsupported_log,
                        opaque_sub_generators: sub_gens,
                        s0002_marker,
                        is_vacuous,
                    })
                }
                _ => None,
            };

        let concrete_halt_witnesses = maybe_compute_concrete_halt_witnesses(
            &test_for_witnesses,
            &semantics,
            &return_mode,
            &self.config.plutus,
        );
        let concrete_error_witnesses = maybe_compute_concrete_error_witnesses(
            &test_for_witnesses,
            &semantics,
            &return_mode,
            &self.config.plutus,
        );

        Ok(ExportedPropertyTest {
            name: format!("{}.{}", &test.module, &test.name),
            module: test.module,
            input_path: json_relative_path(&self.root, &test.input_path),
            on_test_failure: test.on_test_failure,
            return_mode,
            target_kind,
            validator_target,
            test_program: ExportedProgram {
                hex: test_hex,
                flat_bytes: test_flat_bytes,
            },
            fuzzer_program: ExportedProgram {
                hex: fuzzer_hex,
                flat_bytes: fuzzer_flat_bytes,
            },
            fuzzer_type,
            fuzzer_output_type,
            constraint,
            semantics,
            fuzzer_data_schema,
            inner_data_schemas,
            transition_prop_lean,
            concrete_halt_witnesses,
            concrete_error_witnesses,
        })
    }

    #[allow(clippy::result_large_err)]
    pub fn blueprint(path: &Path) -> Result<Blueprint, Error> {
        let blueprint = File::open(path)
            .map_err(|_| Error::Blueprint(blueprint::error::Error::InvalidOrMissingFile.into()))?;
        Ok(serde_json::from_reader(BufReader::new(blueprint))?)
    }

    fn with_dependencies(&mut self, parsed_packages: &mut ParsedModules) -> Result<(), Vec<Error>> {
        let manifest = deps::download(&self.event_listener, &self.root, &self.config)?;

        for package in manifest.packages {
            let lib = self.root.join(paths::build_deps_package(&package.name));

            self.event_listener
                .handle_event(Event::StartingCompilation {
                    root: lib.clone(),
                    name: package.name.to_string(),
                    version: package.version.clone(),
                });

            self.read_package_source_files(&lib.join("lib"))?;

            let mut parsed_modules = self.parse_sources(package.name)?;

            use rayon::prelude::*;

            parsed_modules
                .par_iter_mut()
                .for_each(|(_module, parsed_module)| {
                    parsed_module.ast.definitions.retain(|def| {
                        !matches!(
                            def,
                            Definition::Test(..)
                                | Definition::Benchmark(..)
                                | Definition::Validator(..)
                        )
                    })
                });

            parsed_packages.extend(Into::<HashMap<_, _>>::into(parsed_modules));
        }

        Ok(())
    }

    #[allow(clippy::result_large_err)]
    fn read_source_files(&mut self, config: Option<Vec<UntypedDefinition>>) -> Result<(), Error> {
        let env = self.root.join("env");
        let lib = self.root.join("lib");
        let validators = self.root.join("validators");
        let root = self.root.clone();

        if let Some(defs) = config {
            self.add_module(
                AddModuleBy::Source {
                    name: ast::CONFIG_MODULE.to_string(),
                    code: Formatter::new()
                        .definitions(&defs[..])
                        .to_pretty_string(MAX_COLUMNS),
                },
                &root,
                ModuleKind::Config,
            )?;
        }

        self.aiken_files(&validators, ModuleKind::Validator)?;
        self.aiken_files(&lib, ModuleKind::Lib)?;
        self.aiken_files(&env, ModuleKind::Env)?;

        Ok(())
    }

    #[allow(clippy::result_large_err)]
    fn read_package_source_files(&mut self, lib: &Path) -> Result<(), Error> {
        self.aiken_files(lib, ModuleKind::Lib)?;

        Ok(())
    }

    fn parse_sources(&mut self, package_name: PackageName) -> Result<ParsedModules, Vec<Error>> {
        use rayon::prelude::*;

        let (parsed_modules, parse_errors, duplicates) = self
            .sources
            .par_drain(0..)
            .fold(
                || (ParsedModules::new(), Vec::new(), Vec::new()),
                |(mut parsed_modules, mut parse_errors, mut duplicates), elem| {
                    let Source {
                        path,
                        name,
                        code,
                        kind,
                    } = elem;

                    match aiken_lang::parser::module(&code, kind) {
                        Ok((mut ast, extra)) => {
                            // Store the name
                            ast.name.clone_from(&name);

                            let module = ParsedModule {
                                kind,
                                ast,
                                code,
                                name: name.clone(),
                                path,
                                extra,
                                package: package_name.to_string(),
                            };

                            let path = module.path.clone();

                            if let Some(first) = parsed_modules.insert(module.name.clone(), module)
                            {
                                duplicates.push((name, first.path.clone(), path))
                            }

                            (parsed_modules, parse_errors, duplicates)
                        }
                        Err(errs) => {
                            for error in errs {
                                parse_errors.push((
                                    path.clone(),
                                    code.clone(),
                                    NamedSource::new(path.display().to_string(), code.clone()),
                                    Box::new(error),
                                ))
                            }

                            (parsed_modules, parse_errors, duplicates)
                        }
                    }
                },
            )
            .reduce(
                || (ParsedModules::new(), Vec::new(), Vec::new()),
                |(mut parsed_modules, mut parse_errors, mut duplicates),
                 (mut parsed, mut errs, mut dups)| {
                    let keys_left = parsed_modules.keys().collect::<HashSet<_>>();
                    let keys_right = parsed.keys().collect::<HashSet<_>>();

                    for module in keys_left.intersection(&keys_right) {
                        duplicates.push((
                            module.to_string(),
                            parsed_modules
                                .get(module.as_str())
                                .map(|m| m.path.clone())
                                .unwrap(),
                            parsed.get(module.as_str()).map(|m| m.path.clone()).unwrap(),
                        ));
                    }

                    parsed_modules.extend(parsed.drain());

                    parse_errors.append(&mut errs);
                    duplicates.append(&mut dups);

                    (parsed_modules, parse_errors, duplicates)
                },
            );

        let mut errors: Vec<Error> = Vec::new();

        errors.extend(
            duplicates
                .into_iter()
                .map(|(module, first, second)| Error::DuplicateModule {
                    module: Box::new(module),
                    first: Box::new(first),
                    second: Box::new(second),
                })
                .collect::<Vec<_>>(),
        );

        errors.extend(
            parse_errors
                .into_iter()
                .map(|(path, src, named, error)| Error::Parse {
                    path: Box::new(path),
                    src: Box::new(src),
                    named: named.into(),
                    error,
                })
                .collect::<Vec<_>>(),
        );

        for parsed_module in parsed_modules.values() {
            if let Some(first) = self
                .defined_modules
                .insert(parsed_module.name.clone(), parsed_module.path.clone())
            {
                errors.push(Error::DuplicateModule {
                    module: Box::new(parsed_module.name.clone()),
                    first: Box::new(first),
                    second: Box::new(parsed_module.path.clone()),
                });
            }
        }

        if errors.is_empty() {
            Ok(parsed_modules)
        } else {
            Err(errors)
        }
    }

    fn type_check(
        &mut self,
        modules: &mut ParsedModules,
        tracing: Tracing,
        env: Option<&str>,
        validate_module_name: bool,
    ) -> Result<(), Vec<Error>> {
        let our_modules: BTreeSet<String> = modules.keys().cloned().collect();

        self.with_dependencies(modules)?;

        modules.extends_glossary(&mut self.glossary);

        for name in modules.sequence(&our_modules)? {
            if let Some(module) = modules.remove(&name) {
                let (checked_module, warnings) = module.infer(
                    &self.id_gen,
                    &self.config.name.to_string(),
                    tracing,
                    env,
                    validate_module_name,
                    &mut self.module_sources,
                    &mut self.module_types,
                    &mut self.functions,
                    &mut self.constants,
                    &mut self.data_types,
                )?;

                if our_modules.contains(checked_module.name.as_str())
                    && checked_module.name.as_str() != ast::CONFIG_MODULE
                {
                    self.warnings.extend(warnings);
                }

                self.checked_modules
                    .insert(checked_module.name.clone(), checked_module);
            }
        }

        Ok(())
    }

    #[allow(clippy::result_large_err)]
    fn collect_runnable_scripts(
        &mut self,
        kind: RunnableKind,
        match_tests: Option<Vec<String>>,
        exact_match: bool,
    ) -> (Vec<CollectedRunnable>, Option<Vec<TestMatch>>) {
        let mut scripts = Vec::new();

        let match_tests = match_tests.map(|mt| {
            mt.into_iter()
                .map(|match_test| {
                    let mut match_split_dot = match_test.split('.');

                    let match_module = if match_test.contains('.') || match_test.contains('/') {
                        match_split_dot.next().unwrap_or("")
                    } else {
                        ""
                    };

                    let match_names = match_split_dot.next().and_then(|names| {
                        let names = names.replace(&['{', '}'][..], "");
                        let names_split_comma = names.split(',');

                        let result = names_split_comma
                            .filter_map(|s| {
                                let s = s.trim();
                                if s.is_empty() {
                                    None
                                } else {
                                    Some(s.to_string())
                                }
                            })
                            .collect::<Vec<_>>();

                        if result.is_empty() {
                            None
                        } else {
                            Some(result)
                        }
                    });

                    self.event_listener.handle_event(Event::CollectingTests {
                        matching_module: if match_module.is_empty() {
                            None
                        } else {
                            Some(match_module.to_string())
                        },
                        matching_names: match_names.clone().unwrap_or_default(),
                    });

                    (match_module.to_string(), match_names)
                })
                .collect::<Vec<TestMatch>>()
        });

        if match_tests.is_none() {
            self.event_listener.handle_event(Event::CollectingTests {
                matching_module: None,
                matching_names: vec![],
            });
        }

        for checked_module in self.checked_modules.values() {
            if checked_module.package != self.config.name.to_string() {
                continue;
            }

            for def in checked_module.ast.definitions() {
                let func = match (kind, def) {
                    (RunnableKind::Test, Definition::Test(func)) => Some(func),
                    (RunnableKind::Bench, Definition::Benchmark(func)) => Some(func),
                    _ => None,
                };

                if let Some(func) = func {
                    if let Some(match_tests) = &match_tests {
                        let is_match = match_tests.iter().any(|(module, names)| {
                            let matched_module =
                                module.is_empty() || checked_module.name.contains(module);

                            let matched_name = match names {
                                None => true,
                                Some(names) => names.iter().any(|name| {
                                    if exact_match {
                                        name == &func.name
                                    } else {
                                        func.name.contains(name)
                                    }
                                }),
                            };

                            matched_module && matched_name
                        });

                        if is_match {
                            scripts.push((
                                checked_module.input_path.clone(),
                                checked_module.name.clone(),
                                func.to_owned(),
                            ))
                        }
                    } else {
                        scripts.push((
                            checked_module.input_path.clone(),
                            checked_module.name.clone(),
                            func.to_owned(),
                        ))
                    }
                }
            }
        }

        (scripts, match_tests)
    }

    fn warn_suspicious_test_match(&mut self, match_tests: Option<&[TestMatch]>, is_empty: bool) {
        match match_tests {
            Some(&[(ref s, Some(ref names))]) if is_empty => {
                if let [test] = names.as_slice() {
                    self.warnings.push(Warning::SuspiciousTestMatch {
                        test: if s.is_empty() {
                            test.to_string()
                        } else {
                            s.to_string()
                        },
                    });
                }
            }
            _ => (),
        }
    }

    #[allow(clippy::result_large_err)]
    fn collect_test_items(
        &mut self,
        kind: RunnableKind,
        verbose: bool,
        match_tests: Option<Vec<String>>,
        exact_match: bool,
        tracing: Tracing,
    ) -> Result<Vec<Test>, Error> {
        let (scripts, match_tests) = self.collect_runnable_scripts(kind, match_tests, exact_match);

        let mut generator = self.new_generator(tracing);
        let mut tests = Vec::new();

        for (input_path, module_name, test) in scripts.into_iter() {
            if verbose {
                self.event_listener.handle_event(Event::GeneratingUPLCFor {
                    name: test.name.clone(),
                    path: input_path.clone(),
                })
            }

            tests.push(Test::from_function_definition(
                &mut generator,
                test,
                module_name,
                input_path,
                kind,
            ));
        }

        self.warn_suspicious_test_match(match_tests.as_deref(), tests.is_empty());

        Ok(tests)
    }

    #[allow(clippy::result_large_err)]
    fn collect_property_tests_with_analysis(
        &mut self,
        verbose: bool,
        match_tests: Option<Vec<String>>,
        exact_match: bool,
        tracing: Tracing,
    ) -> Result<Vec<AnalyzedPropertyTest>, Error> {
        let (scripts, match_tests) =
            self.collect_runnable_scripts(RunnableKind::Test, match_tests, exact_match);
        let matched_any_tests = !scripts.is_empty();

        let mut generator = self.new_generator(tracing);
        let mut tests = Vec::new();

        for (input_path, module_name, test) in scripts.into_iter() {
            if test.arguments.is_empty() {
                continue;
            }

            if verbose {
                self.event_listener.handle_event(Event::GeneratingUPLCFor {
                    name: test.name.clone(),
                    path: input_path.clone(),
                })
            }

            tests.push(PropertyTest::from_function_definition(
                &mut generator,
                test,
                module_name,
                input_path,
            ));
        }

        self.warn_suspicious_test_match(match_tests.as_deref(), !matched_any_tests);
        Ok(tests)
    }

    #[allow(clippy::result_large_err)]
    fn collect_tests(
        &mut self,
        verbose: bool,
        match_tests: Option<Vec<String>>,
        exact_match: bool,
        tracing: Tracing,
    ) -> Result<Vec<Test>, Error> {
        self.collect_test_items(
            RunnableKind::Test,
            verbose,
            match_tests,
            exact_match,
            tracing,
        )
    }

    #[allow(clippy::result_large_err)]
    fn collect_benchmarks(
        &mut self,
        verbose: bool,
        match_tests: Option<Vec<String>>,
        exact_match: bool,
        tracing: Tracing,
    ) -> Result<Vec<Test>, Error> {
        self.collect_test_items(
            RunnableKind::Bench,
            verbose,
            match_tests,
            exact_match,
            tracing,
        )
    }

    fn run_runnables(
        &self,
        tests: Vec<Test>,
        seed: u32,
        max_success: usize,
        tracing: Tracing,
    ) -> Vec<TestResult<UntypedExpr, UntypedExpr>> {
        use rayon::prelude::*;

        let data_types = utils::indexmap::as_ref_values(&self.data_types);

        let plutus_version = &self.config.plutus;

        tests
            .into_par_iter()
            .map(|test| test.run(seed, max_success, plutus_version, tracing))
            .collect::<Vec<TestResult<(Constant, Rc<Type>), PlutusData>>>()
            .into_iter()
            .map(|test| test.reify(&data_types))
            .collect()
    }

    #[allow(clippy::result_large_err)]
    fn aiken_files(&mut self, dir: &Path, kind: ModuleKind) -> Result<(), Error> {
        let mut has_default = None;

        walkdir::WalkDir::new(dir)
            .follow_links(true)
            .into_iter()
            .filter_map(Result::ok)
            .filter(|e| e.file_type().is_file())
            .filter(|e| e.path().extension().is_some_and(|ext| ext == "ak"))
            .try_for_each(|d| {
                if has_default.is_none() {
                    has_default = Some(false);
                }

                let path = d.into_path();
                let keep = is_aiken_path(&path, dir);

                if !keep {
                    self.warnings
                        .push(Warning::InvalidModuleName { path: path.clone() });
                }

                if keep {
                    if self.module_name(dir, &path).as_str() == ast::DEFAULT_ENV_MODULE {
                        has_default = Some(true);
                    }
                    self.add_module(AddModuleBy::Path(path), dir, kind)
                } else {
                    Ok(())
                }
            })?;

        if kind == ModuleKind::Env && has_default == Some(false) {
            return Err(Error::NoDefaultEnvironment);
        }

        Ok(())
    }

    #[allow(clippy::result_large_err)]
    fn add_module(
        &mut self,
        add_by: AddModuleBy,
        dir: &Path,
        kind: ModuleKind,
    ) -> Result<(), Error> {
        let (name, code, path) = match add_by {
            AddModuleBy::Path(path) => {
                let name = self.module_name(dir, &path);
                let code = fs::read_to_string(&path).map_err(|error| Error::FileIo {
                    path: Box::new(path.clone()),
                    error,
                })?;
                (name, code, path)
            }
            AddModuleBy::Source { name, code } => (name, code, dir.to_path_buf()),
        };

        self.sources.push(Source {
            name,
            code,
            kind,
            path,
        });

        Ok(())
    }

    fn module_name(&self, package_path: &Path, full_module_path: &Path) -> String {
        // ../../{config.name}/module.ak

        // module.ak
        let mut module_path = full_module_path
            .strip_prefix(package_path)
            .expect("Stripping package prefix from module path")
            .to_path_buf();

        // module
        module_path.set_extension("");

        // Stringify
        let name = module_path
            .to_str()
            .expect("Module name path to str")
            .to_string();

        // normalise windows paths
        name.replace('\\', "/").replace('-', "_")
    }
}

fn is_aiken_path(path: &Path, dir: impl AsRef<Path>) -> bool {
    use regex::Regex;

    let re = Regex::new(&format!(
        "^({module}{slash})*{module}(\\.[-_a-z0-9]*)*\\.ak$",
        module = "[a-z][-_a-z0-9]*",
        slash = "(/|\\\\)",
    ))
    .expect("is_aiken_path() RE regex");

    re.is_match(
        path.strip_prefix(dir)
            .expect("is_aiken_path(): strip_prefix")
            .to_str()
            .expect("is_aiken_path(): to_str"),
    )
}
