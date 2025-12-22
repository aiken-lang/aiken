use aiken_lang::{ast::Span, line_numbers::LineNumbers};
use indexmap::IndexMap;
use serde::{Deserialize, Deserializer, Serialize, Serializer};
use std::collections::BTreeMap;
use uplc::ast::{Name, Term};

/// Source map for a compiled UPLC program.
/// Maps post-order term indices to source locations.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct SourceMap {
    /// Format version
    pub version: u32,

    /// List of source file paths referenced
    pub sources: Vec<String>,

    /// Map from post-order node index to source location
    pub locations: BTreeMap<u64, SourceLocation>,

    /// Map from node index to variable name (Phase 2)
    #[serde(skip_serializing_if = "BTreeMap::is_empty", default)]
    pub names: BTreeMap<u64, String>,

    /// Map from node index to type reference (Phase 3)
    #[serde(skip_serializing_if = "BTreeMap::is_empty", default)]
    pub types: BTreeMap<u64, TypeRef>,
}

/// A source location within a file.
/// Serializes as [source_index, line, col] or [source_index, line, col, end_line, end_col]
#[derive(Debug, Clone, PartialEq)]
pub struct SourceLocation {
    pub source_index: usize,
    pub line: usize,
    pub column: usize,
    pub end_line: Option<usize>,
    pub end_column: Option<usize>,
}

/// Reference to a type in the blueprint definitions
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct TypeRef {
    #[serde(rename = "$ref")]
    pub reference: String,
}

impl Serialize for SourceLocation {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        match (self.end_line, self.end_column) {
            (Some(el), Some(ec)) => {
                (self.source_index, self.line, self.column, el, ec).serialize(serializer)
            }
            _ => (self.source_index, self.line, self.column).serialize(serializer),
        }
    }
}

impl<'de> Deserialize<'de> for SourceLocation {
    fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let arr: Vec<usize> = Vec::deserialize(deserializer)?;
        match arr.as_slice() {
            [source_index, line, column] => Ok(SourceLocation {
                source_index: *source_index,
                line: *line,
                column: *column,
                end_line: None,
                end_column: None,
            }),
            [source_index, line, column, end_line, end_column] => Ok(SourceLocation {
                source_index: *source_index,
                line: *line,
                column: *column,
                end_line: Some(*end_line),
                end_column: Some(*end_column),
            }),
            _ => Err(serde::de::Error::custom(
                "expected array of 3 or 5 elements",
            )),
        }
    }
}

impl SourceMap {
    pub fn new() -> Self {
        SourceMap {
            version: 1,
            sources: Vec::new(),
            locations: BTreeMap::new(),
            names: BTreeMap::new(),
            types: BTreeMap::new(),
        }
    }

    /// Get or insert a source file, returning its index
    fn get_or_insert_source(&mut self, path: &str) -> usize {
        if let Some(idx) = self.sources.iter().position(|s| s == path) {
            idx
        } else {
            let idx = self.sources.len();
            self.sources.push(path.to_string());
            idx
        }
    }

    /// Build a source map from a Term tree with Span context.
    /// Uses post-order traversal so parameter application doesn't shift indices.
    pub fn from_term(
        term: &Term<Name, Span>,
        module_name: &str,
        module_sources: &IndexMap<&str, &(String, LineNumbers)>,
    ) -> Self {
        let mut source_map = SourceMap::new();
        let mut counter: u64 = 0;

        visit_post_order(
            term,
            &mut counter,
            &mut source_map,
            module_name,
            module_sources,
        );

        source_map
    }
}

impl Default for SourceMap {
    fn default() -> Self {
        Self::new()
    }
}

/// Visit a term tree in post-order and assign indices.
/// Post-order means we visit children first, then assign index to current node.
fn visit_post_order(
    term: &Term<Name, Span>,
    counter: &mut u64,
    source_map: &mut SourceMap,
    module_name: &str,
    module_sources: &IndexMap<&str, &(String, LineNumbers)>,
) {
    // Visit children first (post-order)
    match term {
        Term::Apply {
            function, argument, ..
        } => {
            // Visit argument first, then function (right-to-left for consistency)
            visit_post_order(argument, counter, source_map, module_name, module_sources);
            visit_post_order(function, counter, source_map, module_name, module_sources);
        }
        Term::Lambda { body, .. } => {
            visit_post_order(body, counter, source_map, module_name, module_sources);
        }
        Term::Delay { term: inner, .. } => {
            visit_post_order(inner, counter, source_map, module_name, module_sources);
        }
        Term::Force { term: inner, .. } => {
            visit_post_order(inner, counter, source_map, module_name, module_sources);
        }
        Term::Case {
            constr, branches, ..
        } => {
            visit_post_order(constr, counter, source_map, module_name, module_sources);
            for branch in branches {
                visit_post_order(branch, counter, source_map, module_name, module_sources);
            }
        }
        Term::Constr { fields, .. } => {
            for field in fields {
                visit_post_order(field, counter, source_map, module_name, module_sources);
            }
        }
        // Leaf nodes: Var, Constant, Builtin, Error
        Term::Var { .. }
        | Term::Constant { .. }
        | Term::Builtin { .. }
        | Term::Error { .. } => {}
    }

    // Assign index to this node (after children)
    let index = *counter;
    *counter += 1;

    // Extract span from the term's context
    let span = get_span(term);

    // Skip empty spans
    if span.start == 0 && span.end == 0 {
        return;
    }

    // Convert span to line/column
    if let Some((src, line_numbers)) = module_sources.get(module_name) {
        // Check if span is within bounds
        if span.start < src.len() && span.end <= src.len() {
            if let Some(start_loc) = line_numbers.line_and_column_number(span.start) {
                let source_index = source_map.get_or_insert_source(module_name);

                let (end_line, end_column) = if span.end > span.start {
                    // Get end position (end is exclusive, so use end-1 for the last character)
                    line_numbers
                        .line_and_column_number(span.end.saturating_sub(1))
                        .map(|loc| (Some(loc.line), Some(loc.column)))
                        .unwrap_or((None, None))
                } else {
                    (None, None)
                };

                source_map.locations.insert(
                    index,
                    SourceLocation {
                        source_index,
                        line: start_loc.line,
                        column: start_loc.column,
                        end_line,
                        end_column,
                    },
                );
            }
        }
    }
}

/// Extract the span from a term's context field
fn get_span(term: &Term<Name, Span>) -> Span {
    match term {
        Term::Var { context, .. } => *context,
        Term::Delay { context, .. } => *context,
        Term::Lambda { context, .. } => *context,
        Term::Apply { context, .. } => *context,
        Term::Constant { context, .. } => *context,
        Term::Force { context, .. } => *context,
        Term::Error { context } => *context,
        Term::Builtin { context, .. } => *context,
        Term::Constr { context, .. } => *context,
        Term::Case { context, .. } => *context,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::{self, json};
    use std::rc::Rc;

    #[test]
    fn serialize_source_location_3_elements() {
        let loc = SourceLocation {
            source_index: 0,
            line: 10,
            column: 5,
            end_line: None,
            end_column: None,
        };
        assert_eq!(serde_json::to_value(&loc).unwrap(), json!([0, 10, 5]));
    }

    #[test]
    fn serialize_source_location_5_elements() {
        let loc = SourceLocation {
            source_index: 0,
            line: 10,
            column: 5,
            end_line: Some(10),
            end_column: Some(25),
        };
        assert_eq!(
            serde_json::to_value(&loc).unwrap(),
            json!([0, 10, 5, 10, 25])
        );
    }

    #[test]
    fn deserialize_source_location_3_elements() {
        let loc: SourceLocation = serde_json::from_value(json!([1, 42, 3])).unwrap();
        assert_eq!(loc.source_index, 1);
        assert_eq!(loc.line, 42);
        assert_eq!(loc.column, 3);
        assert_eq!(loc.end_line, None);
        assert_eq!(loc.end_column, None);
    }

    #[test]
    fn deserialize_source_location_5_elements() {
        let loc: SourceLocation = serde_json::from_value(json!([0, 10, 12, 10, 25])).unwrap();
        assert_eq!(loc.source_index, 0);
        assert_eq!(loc.line, 10);
        assert_eq!(loc.column, 12);
        assert_eq!(loc.end_line, Some(10));
        assert_eq!(loc.end_column, Some(25));
    }

    #[test]
    fn serialize_source_map() {
        let mut source_map = SourceMap::new();
        source_map.sources.push("validators/spend.ak".to_string());
        source_map.sources.push("lib/utils.ak".to_string());
        source_map.locations.insert(
            0,
            SourceLocation {
                source_index: 0,
                line: 10,
                column: 5,
                end_line: None,
                end_column: None,
            },
        );
        source_map.locations.insert(
            1,
            SourceLocation {
                source_index: 0,
                line: 10,
                column: 12,
                end_line: Some(10),
                end_column: Some(25),
            },
        );
        source_map.locations.insert(
            5,
            SourceLocation {
                source_index: 1,
                line: 42,
                column: 3,
                end_line: None,
                end_column: None,
            },
        );

        let expected = json!({
            "version": 1,
            "sources": ["validators/spend.ak", "lib/utils.ak"],
            "locations": {
                "0": [0, 10, 5],
                "1": [0, 10, 12, 10, 25],
                "5": [1, 42, 3]
            }
        });

        assert_eq!(serde_json::to_value(&source_map).unwrap(), expected);
    }

    #[test]
    fn roundtrip_source_map() {
        let json_str = r#"{
            "version": 1,
            "sources": ["validators/spend.ak"],
            "locations": {
                "0": [0, 10, 5],
                "1": [0, 10, 12, 10, 25]
            }
        }"#;

        let source_map: SourceMap = serde_json::from_str(json_str).unwrap();
        assert_eq!(source_map.version, 1);
        assert_eq!(source_map.sources, vec!["validators/spend.ak"]);
        assert_eq!(source_map.locations.len(), 2);

        // Verify roundtrip
        let serialized = serde_json::to_string(&source_map).unwrap();
        let deserialized: SourceMap = serde_json::from_str(&serialized).unwrap();
        assert_eq!(source_map, deserialized);
    }

    #[test]
    fn from_term_post_order_numbering() {
        use aiken_lang::line_numbers::LineNumbers;

        // Source code with known positions:
        // Line 1, col 1-3: "foo"  (bytes 0-3)
        // Line 2, col 1-3: "bar"  (bytes 4-7)
        // Line 3, col 1-6: "result" (bytes 8-14)
        let src = "foo\nbar\nresult";
        let module_name = "test";
        let data = (src.to_string(), LineNumbers::new(src));
        let mut module_sources: IndexMap<&str, &(String, LineNumbers)> = IndexMap::new();
        module_sources.insert(module_name, &data);

        // Build a simple term tree:
        //   Apply(function=Var("f"), argument=Var("x"))
        //
        // Post-order traversal visits:
        //   1. argument first (Var "x") -> index 0
        //   2. function next (Var "f") -> index 1
        //   3. Apply last -> index 2
        let term = Term::Apply {
            context: Span { start: 8, end: 14 }, // "result" on line 3
            function: Rc::new(Term::Var {
                context: Span { start: 0, end: 3 }, // "foo" on line 1
                name: Rc::new(Name {
                    text: "f".to_string(),
                    unique: 0.into(),
                }),
            }),
            argument: Rc::new(Term::Var {
                context: Span { start: 4, end: 7 }, // "bar" on line 2
                name: Rc::new(Name {
                    text: "x".to_string(),
                    unique: 1.into(),
                }),
            }),
        };

        let source_map = SourceMap::from_term(&term, module_name, &module_sources);

        // Verify structure
        assert_eq!(source_map.version, 1);
        assert_eq!(source_map.sources, vec!["test"]);

        // Verify post-order numbering:
        // index 0: argument (Var "x") at line 2, col 1
        // index 1: function (Var "f") at line 1, col 1
        // index 2: Apply at line 3, col 1
        assert_eq!(source_map.locations.len(), 3);

        let loc0 = source_map.locations.get(&0).expect("should have index 0");
        assert_eq!(loc0.source_index, 0);
        assert_eq!(loc0.line, 2); // "bar" is on line 2

        let loc1 = source_map.locations.get(&1).expect("should have index 1");
        assert_eq!(loc1.source_index, 0);
        assert_eq!(loc1.line, 1); // "foo" is on line 1

        let loc2 = source_map.locations.get(&2).expect("should have index 2");
        assert_eq!(loc2.source_index, 0);
        assert_eq!(loc2.line, 3); // "result" is on line 3
    }

    #[test]
    fn from_term_skips_empty_spans() {
        use aiken_lang::line_numbers::LineNumbers;

        let src = "foo\nbar";
        let module_name = "test";
        let data = (src.to_string(), LineNumbers::new(src));
        let mut module_sources: IndexMap<&str, &(String, LineNumbers)> = IndexMap::new();
        module_sources.insert(module_name, &data);

        // Term with empty span (start=0, end=0) should be skipped
        let term = Term::Apply {
            context: Span { start: 0, end: 0 }, // Empty span - should be skipped
            function: Rc::new(Term::Var {
                context: Span { start: 0, end: 3 }, // Valid span
                name: Rc::new(Name {
                    text: "f".to_string(),
                    unique: 0.into(),
                }),
            }),
            argument: Rc::new(Term::Var {
                context: Span { start: 0, end: 0 }, // Empty span - should be skipped
                name: Rc::new(Name {
                    text: "x".to_string(),
                    unique: 1.into(),
                }),
            }),
        };

        let source_map = SourceMap::from_term(&term, module_name, &module_sources);

        // Only index 1 (the function Var with valid span) should be present
        assert_eq!(source_map.locations.len(), 1);
        assert!(source_map.locations.contains_key(&1));
    }

    #[test]
    fn from_term_nested_lambdas() {
        use aiken_lang::line_numbers::LineNumbers;

        let src = "line1\nline2\nline3\nline4";
        let module_name = "test";
        let data = (src.to_string(), LineNumbers::new(src));
        let mut module_sources: IndexMap<&str, &(String, LineNumbers)> = IndexMap::new();
        module_sources.insert(module_name, &data);

        // Lambda { body: Lambda { body: Var } }
        // Post-order:
        //   0: inner Var
        //   1: inner Lambda
        //   2: outer Lambda
        let term = Term::Lambda {
            context: Span { start: 0, end: 5 }, // line 1
            parameter_name: Rc::new(Name {
                text: "x".to_string(),
                unique: 0.into(),
            }),
            body: Rc::new(Term::Lambda {
                context: Span { start: 6, end: 11 }, // line 2
                parameter_name: Rc::new(Name {
                    text: "y".to_string(),
                    unique: 1.into(),
                }),
                body: Rc::new(Term::Var {
                    context: Span { start: 12, end: 17 }, // line 3
                    name: Rc::new(Name {
                        text: "x".to_string(),
                        unique: 0.into(),
                    }),
                }),
            }),
        };

        let source_map = SourceMap::from_term(&term, module_name, &module_sources);

        assert_eq!(source_map.locations.len(), 3);

        // Index 0: innermost Var on line 3
        let loc0 = source_map.locations.get(&0).unwrap();
        assert_eq!(loc0.line, 3);

        // Index 1: inner Lambda on line 2
        let loc1 = source_map.locations.get(&1).unwrap();
        assert_eq!(loc1.line, 2);

        // Index 2: outer Lambda on line 1
        let loc2 = source_map.locations.get(&2).unwrap();
        assert_eq!(loc2.line, 1);
    }
}
