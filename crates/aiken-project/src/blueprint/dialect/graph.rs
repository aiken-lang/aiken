use serde_json::Value;
use std::collections::HashMap;

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("invalid value-schema reference {reference} at {location}: {reason}")]
    InvalidReference {
        location: String,
        reference: String,
        reason: &'static str,
    },
    #[error("missing value-schema reference {reference} at {location}")]
    MissingReference { location: String, reference: String },
    #[error("reference-only value-schema cycle at {reference}")]
    ReferenceCycle { reference: String },
}

// These are schema-bearing keywords in the pinned compiled-interface value schema.
// In particular, annotations and arbitrary extension metadata are not graph edges.
const SINGLE_SCHEMAS: &[&str] = &["keys", "values", "left", "right", "not"];
const SCHEMA_ARRAYS: &[&str] = &["fields", "oneOf", "anyOf", "allOf"];

pub(crate) fn validate(document: &Value) -> Result<(), Error> {
    let mut pending = Vec::new();
    if let Some(definitions) = document.get("definitions").and_then(Value::as_object) {
        for (name, schema) in definitions {
            pending.push((format!("/definitions/{}", escape(name)), schema));
        }
    }
    if let Some(validators) = document.get("validators").and_then(Value::as_array) {
        for (index, validator) in validators.iter().enumerate() {
            for name in ["redeemer", "datum"] {
                if let Some(argument) = validator.get(name) {
                    argument_schemas(
                        &format!("/validators/{index}/{name}"),
                        argument,
                        &mut pending,
                    );
                }
            }
            if let Some(parameters) = validator.get("parameters").and_then(Value::as_array) {
                for (parameter, argument) in parameters.iter().enumerate() {
                    argument_schemas(
                        &format!("/validators/{index}/parameters/{parameter}"),
                        argument,
                        &mut pending,
                    );
                }
            }
        }
    }
    if let Some(functions) = document.get("functions").and_then(Value::as_object) {
        for (name, function) in functions {
            let path = format!("/functions/{}", escape(name));
            if let Some(arguments) = function.get("arguments").and_then(Value::as_array) {
                for (index, schema) in arguments.iter().enumerate() {
                    pending.push((format!("{path}/arguments/{index}"), schema));
                }
            }
            if let Some(result) = function.get("result") {
                pending.push((format!("{path}/result"), result));
            }
        }
    }

    let mut nodes = Vec::new();
    while let Some((path, schema)) = pending.pop() {
        for keyword in SINGLE_SCHEMAS {
            if let Some(child) = schema.get(*keyword) {
                pending.push((format!("{path}/{keyword}"), child));
            }
        }
        for keyword in SCHEMA_ARRAYS {
            if let Some(children) = schema.get(*keyword).and_then(Value::as_array) {
                for (index, child) in children.iter().enumerate() {
                    pending.push((format!("{path}/{keyword}/{index}"), child));
                }
            }
        }
        if let Some(items) = schema.get("items") {
            match items {
                Value::Array(children) => {
                    for (index, child) in children.iter().enumerate() {
                        pending.push((format!("{path}/items/{index}"), child));
                    }
                }
                _ => pending.push((format!("{path}/items"), items)),
            }
        }
        if let Some(constructors) = schema.get("constructors").and_then(Value::as_array) {
            for (constructor, value) in constructors.iter().enumerate() {
                if let Some(fields) = value.get("fields").and_then(Value::as_array) {
                    for (field, child) in fields.iter().enumerate() {
                        pending.push((
                            format!("{path}/constructors/{constructor}/fields/{field}"),
                            child,
                        ));
                    }
                }
            }
        }
        nodes.push((path, schema));
    }

    let indices: HashMap<_, _> = nodes
        .iter()
        .enumerate()
        .map(|(index, (path, _))| (path.as_str(), index))
        .collect();
    let mut edges = vec![Vec::new(); nodes.len()];
    for (index, (path, schema)) in nodes.iter().enumerate() {
        let Some(reference) = schema.get("$ref") else {
            // Alternatives/composition do not guard recursion. Only crossing a
            // constructor field or container element makes a recursive type productive.
            for keyword in ["oneOf", "anyOf", "allOf"] {
                if let Some(children) = schema.get(keyword).and_then(Value::as_array) {
                    for child in 0..children.len() {
                        edges[index].push(indices[format!("{path}/{keyword}/{child}").as_str()]);
                    }
                }
            }
            if schema.get("not").is_some() {
                edges[index].push(indices[format!("{path}/not").as_str()]);
            }
            continue;
        };
        let invalid = |reason| Error::InvalidReference {
            location: format!("{path}/$ref"),
            reference: reference.to_string(),
            reason,
        };
        let reference = reference
            .as_str()
            .ok_or_else(|| invalid("expected a string"))?;
        let pointer = reference
            .strip_prefix('#')
            .filter(|pointer| {
                pointer
                    .strip_prefix("/definitions/")
                    .is_some_and(|suffix| !suffix.is_empty() && !suffix.contains('/'))
                    && valid_pointer(pointer)
            })
            .ok_or_else(|| invalid("expected one escaped definition key after #/definitions/"))?;
        let target = indices.get(pointer).copied().ok_or_else(|| {
            if document.pointer(pointer).is_some() {
                invalid("target is not a value-schema node")
            } else {
                Error::MissingReference {
                    location: format!("{path}/$ref"),
                    reference: reference.to_owned(),
                }
            }
        })?;
        // Consumers resolve a direct reference before decoding its schema, so
        // neither annotations nor structural siblings can make an alias productive.
        // Recursion through the children of a non-reference node remains valid.
        edges[index].push(target);
    }

    // Iterative three-colour DFS of unguarded edges; never expand recursive
    // schemas, and never revisit a completed subgraph.
    let mut state = vec![0_u8; nodes.len()];
    let mut stack = Vec::new();
    for start in 0..nodes.len() {
        stack.push((start, false));
        while let Some((index, finished)) = stack.pop() {
            if finished {
                state[index] = 2;
                continue;
            }
            match state[index] {
                1 => {
                    return Err(Error::ReferenceCycle {
                        reference: format!("#{}", nodes[index].0),
                    });
                }
                2 => continue,
                _ => {
                    state[index] = 1;
                    stack.push((index, true));
                    stack.extend(edges[index].iter().map(|&child| (child, false)));
                }
            }
        }
    }
    Ok(())
}

fn argument_schemas<'a>(path: &str, argument: &'a Value, pending: &mut Vec<(String, &'a Value)>) {
    if let Some(schema) = argument.get("schema") {
        pending.push((format!("{path}/schema"), schema));
    }
    if let Some(alternatives) = argument.get("oneOf").and_then(Value::as_array) {
        for (index, alternative) in alternatives.iter().enumerate() {
            if let Some(schema) = alternative.get("schema") {
                pending.push((format!("{path}/oneOf/{index}/schema"), schema));
            }
        }
    }
}

fn escape(token: &str) -> String {
    token.replace('~', "~0").replace('/', "~1")
}

fn valid_pointer(pointer: &str) -> bool {
    let mut bytes = pointer.bytes();
    while let Some(byte) = bytes.next() {
        if byte == b'~' && !matches!(bytes.next(), Some(b'0' | b'1')) {
            return false;
        }
    }
    true
}

#[cfg(test)]
mod tests {
    use super::{Error, validate};
    use serde_json::json;

    #[test]
    fn recursive_constructors_are_accepted() {
        let document = json!({"definitions": {
            "Tree": {"anyOf": [
                {"dataType": "constructor", "index": 0, "fields": []},
                {"dataType": "constructor", "index": 1, "fields": [
                    {"$ref": "#/definitions/Tree"}
                ]}
            ]}
        }});
        assert!(validate(&document).is_ok());
    }

    #[test]
    fn alias_cycles_are_rejected_even_with_annotations() {
        for definitions in [
            json!({"A": {"$ref": "#/definitions/A"}}),
            json!({"A": {"$ref": "#/definitions/A", "dataType": "integer"}}),
            json!({"A": {"anyOf": [{"$ref": "#/definitions/A"}]}}),
            json!({
                "A": {"oneOf": [{"$ref": "#/definitions/B"}]},
                "B": {"anyOf": [{"$ref": "#/definitions/A"}]}
            }),
            json!({
                "A": {"$ref": "#/definitions/B", "title": "Alias"},
                "B": {"$ref": "#/definitions/A", "description": "Alias", "$comment": "Alias"}
            }),
        ] {
            assert!(matches!(
                validate(&json!({"definitions": definitions})),
                Err(Error::ReferenceCycle { .. })
            ));
        }
    }

    #[test]
    fn cycles_through_constructors_are_accepted() {
        let document = json!({"definitions": {
            "Alias": {"$ref": "#/definitions/Node", "title": "Alias"},
            "Node": {"dataType": "constructor", "index": 0, "fields": [
                {"$ref": "#/definitions/Alias"}
            ]}
        }});
        assert!(validate(&document).is_ok());
    }

    #[test]
    fn dangling_references_are_rejected_in_unused_definitions() {
        let document = json!({"definitions": {
            "Unused": {"dataType": "list", "items": {"$ref": "#/definitions/Missing"}}
        }});
        assert!(matches!(
            validate(&document),
            Err(Error::MissingReference { .. })
        ));
    }

    #[test]
    fn reference_tokens_use_rfc6901_escaping() {
        let document = json!({"definitions": {
            "module/Thing~name": {"dataType": "integer"},
            "literal~1": {"dataType": "bytes"},
            "A": {"$ref": "#/definitions/module~1Thing~0name"},
            "B": {"$ref": "#/definitions/literal~01"}
        }});
        assert!(validate(&document).is_ok());
    }

    #[test]
    fn invalid_reference_forms_are_rejected() {
        for reference in [
            json!(42),
            json!("other.json#/definitions/A"),
            json!("#/validators/0/redeemer/schema"),
            json!("#/definitions/A~2"),
            json!("#/definitions/A~"),
        ] {
            let document = json!({"definitions": {"A": {"$ref": reference}}});
            assert!(matches!(
                validate(&document),
                Err(Error::InvalidReference { .. })
            ));
        }
    }

    #[test]
    fn nested_pointers_are_not_definition_keys() {
        let document = json!({"definitions": {
            "A": {"dataType": "list", "items": {"dataType": "integer"}},
            "B": {"$ref": "#/definitions/A/items"}
        }});
        assert!(matches!(
            validate(&document),
            Err(Error::InvalidReference { .. })
        ));
    }

    #[test]
    fn metadata_is_not_traversed() {
        let document = json!({
            "metadata": {"$ref": "#/definitions/Missing"},
            "definitions": {"A": {
                "dataType": "integer",
                "metadata": {"$ref": "#/definitions/Missing"}
            }},
            "validators": [{
                "redeemer": {"schema": {}, "metadata": {"$ref": "#/definitions/Missing"}},
                "interface": {"$ref": "#/definitions/Missing"}
            }]
        });
        assert!(validate(&document).is_ok());
    }

    #[test]
    fn every_argument_schema_is_checked() {
        let dangling = json!({"schema": {"$ref": "#/definitions/Missing"}});
        for validator in [
            json!({"parameters": [dangling.clone()]}),
            json!({"redeemer": dangling.clone()}),
            json!({"datum": dangling.clone()}),
            json!({"redeemer": {"oneOf": [dangling.clone()]}}),
            json!({"datum": {"oneOf": [dangling]}}),
        ] {
            assert!(matches!(
                validate(&json!({"validators": [validator]})),
                Err(Error::MissingReference { .. })
            ));
        }
    }

    #[test]
    fn pinned_schema_child_slots_are_checked() {
        let dangling = json!({"$ref": "#/definitions/Missing"});
        for schema in [
            json!({"items": dangling.clone()}),
            json!({"items": [dangling.clone()]}),
            json!({"keys": dangling.clone()}),
            json!({"values": dangling.clone()}),
            json!({"left": dangling.clone()}),
            json!({"right": dangling.clone()}),
            json!({"not": dangling.clone()}),
            json!({"fields": [dangling.clone()]}),
            json!({"oneOf": [dangling.clone()]}),
            json!({"anyOf": [dangling.clone()]}),
            json!({"allOf": [dangling.clone()]}),
            json!({"constructors": [{"name": "Node", "fields": [dangling]}]}),
        ] {
            assert!(matches!(
                validate(&json!({"definitions": {"A": schema}})),
                Err(Error::MissingReference { .. })
            ));
        }
    }
}
