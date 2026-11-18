use serde::Serialize;
use serde_json::json;
use std::collections::BTreeMap;

#[derive(Debug, Clone, Serialize)]
pub struct VerificationTrust {
    pub method: String,
    pub z3: String,
    pub revisions: BTreeMap<String, String>,
    pub budget: String,
}

#[derive(Debug, Clone, Serialize)]
pub struct VerificationProperty {
    pub id: String,
    pub verdict: VerificationVerdict,
    pub counterexample: Option<String>,
}

#[derive(Debug, Clone, Serialize)]
#[serde(rename_all = "kebab-case")]
pub enum VerificationVerdict {
    Verified,
    Falsified,
    Inconclusive,
    NotChecked,
    Missing,
}

#[derive(Debug, Clone, Serialize)]
pub struct VerificationReport {
    pub title: String,
    pub trust: VerificationTrust,
    pub properties: Vec<VerificationProperty>,
    pub warnings: Vec<String>,
    pub errors: Vec<String>,
    pub success: bool,
}

pub fn verification_json_schema() -> serde_json::Value {
    json!({
        "$schema": "https://json-schema.org/draft/2020-12/schema",
        "title": "Aiken verification report",
        "type": "object",
        "additionalProperties": false,
        "required": ["title", "trust", "properties", "warnings", "errors", "success"],
        "properties": {
            "title": { "type": "string" },
            "trust": {
                "type": "object",
                "additionalProperties": false,
                "required": ["method", "z3", "revisions", "budget"],
                "properties": {
                    "method": { "type": "string" },
                    "z3": { "type": "string" },
                    "revisions": {
                        "type": "object",
                        "additionalProperties": { "type": "string" }
                    },
                    "budget": { "type": "string" }
                }
            },
            "properties": {
                "type": "array",
                "items": {
                    "type": "object",
                    "additionalProperties": false,
                    "required": ["id", "verdict", "counterexample"],
                    "properties": {
                        "id": { "type": "string" },
                        "verdict": {
                            "type": "string",
                            "enum": ["verified", "falsified", "inconclusive", "not-checked", "missing"]
                        },
                        "counterexample": { "type": ["string", "null"] }
                    }
                }
            },
            "warnings": { "type": "array", "items": { "type": "string" } },
            "errors": { "type": "array", "items": { "type": "string" } },
            "success": { "type": "boolean" }
        }
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn verification_schema() {
        insta::assert_json_snapshot!(verification_json_schema());
    }

    #[test]
    fn verification_report_serialization() {
        let report = VerificationReport {
            title: "example/validator".to_owned(),
            trust: VerificationTrust {
                method: "Blaster SMT; no reconstructed Lean proof".to_owned(),
                z3: "/usr/bin/z3".to_owned(),
                revisions: BTreeMap::from([("Blaster".to_owned(), "revision".to_owned())]),
                budget: "results relative to 100 steps".to_owned(),
            },
            properties: vec![
                VerificationProperty {
                    id: "verified".to_owned(),
                    verdict: VerificationVerdict::Verified,
                    counterexample: None,
                },
                VerificationProperty {
                    id: "falsified".to_owned(),
                    verdict: VerificationVerdict::Falsified,
                    counterexample: Some("input = 0".to_owned()),
                },
                VerificationProperty {
                    id: "inconclusive".to_owned(),
                    verdict: VerificationVerdict::Inconclusive,
                    counterexample: None,
                },
                VerificationProperty {
                    id: "not-checked".to_owned(),
                    verdict: VerificationVerdict::NotChecked,
                    counterexample: None,
                },
                VerificationProperty {
                    id: "missing".to_owned(),
                    verdict: VerificationVerdict::Missing,
                    counterexample: None,
                },
            ],
            warnings: vec!["revision mismatch".to_owned()],
            errors: vec!["consumer failed".to_owned()],
            success: false,
        };

        insta::assert_json_snapshot!(report);
    }
}
