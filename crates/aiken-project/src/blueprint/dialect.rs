//! Opt-in compiled-interface emission; ordinary blueprint serialization is unchanged.

pub(crate) mod graph;
#[cfg(test)]
mod tests;

use super::{Blueprint, parameter::Parameter, validator::Validator};
use crate::{assurance::emitted_target_id, config::PlutusVersion};
use aiken_ual::{self as ual, Purpose};
use std::collections::BTreeMap;
use uplc::ast::SerializableProgram;

pub use graph::Error as ReferenceError;

/// Destination reserved for the assurance writer, never the default build output.
pub const DEFAULT_PATH: &str = "assurance/plutus.json";

const PURPOSES: [(Purpose, &str); 6] = [
    (Purpose::Spend, "spend"),
    (Purpose::Mint, "mint"),
    (Purpose::Withdraw, "withdraw"),
    (Purpose::Publish, "publish"),
    (Purpose::Vote, "vote"),
    (Purpose::Propose, "propose"),
];

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("compiled-interface emission requires Plutus V3")]
    PlutusVersion,
    #[error("invalid validator handler title {0:?}; expected module.validator.handler")]
    HandlerTitle(String),
    #[error("target id {id:?} collides between {first:?} and {second:?}")]
    TargetIdCollision {
        id: String,
        first: String,
        second: String,
    },
    #[error("handlers of {0:?} disagree on compiled program or parameters")]
    InconsistentValidator(String),
    #[error("duplicate handler {0:?}")]
    DuplicateHandler(String),
    #[error(transparent)]
    Json(#[from] serde_json::Error),
    #[error(transparent)]
    References(#[from] ReferenceError),
    #[error(transparent)]
    Validation(#[from] ual::ValidationError),
}

impl Blueprint {
    /// Emit the pinned UAL dialect without recompiling or mutating this blueprint.
    ///
    /// Handler entries are grouped by qualified validator identity, not by script
    /// bytes. Every V3 purpose applies the parameters, then one raw Data context;
    /// unhandled purposes still execute the compiled fallback. Datum and redeemer
    /// schemas remain descriptive metadata, not extra runtime arguments.
    ///
    /// The returned value has passed offline interface-schema and definition-graph
    /// validation. This performs no filesystem writes; the assurance writer owns
    /// [`DEFAULT_PATH`]. Empty validator sets are rejected by the pinned schema.
    pub fn to_compiled_interface(&self) -> Result<ual::CompiledInterfaceBlueprint, Error> {
        if self.preamble.plutus_version != PlutusVersion::V3 {
            return Err(Error::PlutusVersion);
        }

        let mut groups: BTreeMap<(&str, &str), BTreeMap<&str, &Validator<SerializableProgram>>> =
            BTreeMap::new();
        for validator in &self.validators {
            if !matches!(validator.program, SerializableProgram::PlutusV3Program(_)) {
                return Err(Error::PlutusVersion);
            }
            let Some((module, name, handler)) = validator.handler_title_parts() else {
                return Err(Error::HandlerTitle(validator.title.clone()));
            };
            if handler != "else" && !PURPOSES.iter().any(|(_, name)| *name == handler) {
                return Err(Error::HandlerTitle(validator.title.clone()));
            }
            let handlers = groups.entry((module, name)).or_default();
            if let Some(first) = handlers.values().next()
                && (first.program != validator.program || first.parameters != validator.parameters)
            {
                return Err(Error::InconsistentValidator(format!("{module}.{name}")));
            }
            if handlers.insert(handler, validator).is_some() {
                return Err(Error::DuplicateHandler(validator.title.clone()));
            }
        }

        let mut ids = BTreeMap::new();
        let mut validators = Vec::with_capacity(groups.len());
        for ((module, name), handlers) in groups {
            let id = emitted_target_id(module, name);
            let title = format!("{module}.{name}");
            if let Some(first) = ids.insert(id.clone(), title.clone()) {
                return Err(Error::TargetIdCollision {
                    id,
                    first,
                    second: title,
                });
            }
            // A group is created only when its first handler is inserted.
            let first = handlers.values().next().expect("nonempty handler group");
            let parameters = first
                .parameters
                .iter()
                .map(|parameter| argument(parameter, None))
                .collect::<Result<Vec<_>, _>>()?;
            let mut redeemers = Vec::with_capacity(PURPOSES.len());
            let mut invocations = Vec::with_capacity(PURPOSES.len());
            for (purpose, handler) in PURPOSES {
                let redeemer = handlers
                    .get(handler)
                    .or_else(|| handlers.get("else"))
                    .and_then(|validator| validator.redeemer.as_ref());
                redeemers.push(match redeemer {
                    Some(parameter) => argument(parameter, Some(purpose))?,
                    None => raw_data_argument(purpose),
                });
                let mut arguments = Vec::with_capacity(parameters.len() + 1);
                arguments.extend((0..parameters.len()).map(|index| {
                    ual::InvocationArgument::Parameter {
                        source: format!("/parameters/{index}"),
                    }
                }));
                arguments.push(ual::InvocationArgument::Context {});
                invocations.push(ual::Invocation { purpose, arguments });
            }
            let datum = handlers
                .get("spend")
                .and_then(|validator| validator.datum.as_ref())
                .map(|parameter| {
                    PURPOSES
                        .iter()
                        .map(|&(purpose, _)| {
                            if purpose == Purpose::Spend {
                                argument(parameter, Some(purpose))
                            } else {
                                Ok(raw_data_argument(purpose))
                            }
                        })
                        .collect::<Result<Vec<_>, Error>>()
                })
                .transpose()?
                .map(|one_of| {
                    ual::BlueprintArgument::Alternatives(ual::ArgumentAlternatives { one_of })
                });
            let (hash, code) = first.program.compiled_code_and_hash();
            validators.push(ual::CompiledValidator {
                id,
                title,
                description: None,
                redeemer: ual::BlueprintArgument::Alternatives(ual::ArgumentAlternatives {
                    one_of: redeemers,
                }),
                datum,
                parameters: (!parameters.is_empty()).then_some(parameters),
                interface: ual::CompiledInterface {
                    calling_convention: ual::CallingConvention::LedgerV3,
                    invocations,
                },
                compiled_code: Some(hex::encode::<&[u8]>(code.as_ref())),
                hash: Some(hash.to_string()),
                extensions: BTreeMap::new(),
            });
        }

        let blueprint = ual::CompiledInterfaceBlueprint {
            schema: ual::COMPILED_INTERFACE_SCHEMA_ID.to_owned(),
            preamble: ual::BlueprintPreamble {
                title: self.preamble.title.clone(),
                description: self.preamble.description.clone(),
                version: self.preamble.version.clone(),
                plutus_version: ual::PlutusVersion::V3,
                compiler: self.preamble.compiler.as_ref().map(|compiler| ual::Compiler {
                    name: compiler.name.clone(),
                    version: Some(compiler.version.clone()),
                }),
                license: self.preamble.license.clone(),
            },
            validators,
            definitions: if self.definitions.is_empty() {
                None
            } else {
                Some(serde_json::from_value(serde_json::to_value(&self.definitions)?)?)
            },
            vocabulary: BTreeMap::from([
                ("https://cips.cardano.org/cips/cip57".to_owned(), true.into()),
                (
                    "https://cips.cardano.org/cips/cip57/extensions/compiled-interface/v1/vocabulary"
                        .to_owned(),
                    true.into(),
                ),
            ]),
            extensions: BTreeMap::new(),
        };
        let value = serde_json::to_value(&blueprint)?;
        ual::validate(ual::DocumentKind::Blueprint, &value)?;
        graph::validate(&value)?;
        Ok(blueprint)
    }
}

fn argument(parameter: &Parameter, purpose: Option<Purpose>) -> Result<ual::ArgumentSchema, Error> {
    Ok(ual::ArgumentSchema {
        title: parameter.title.clone(),
        description: None,
        schema: ual::ValueSchema(serde_json::to_value(&parameter.schema)?),
        purpose: purpose.map(ual::PurposeSelector::Single),
        extensions: BTreeMap::new(),
    })
}

fn raw_data_argument(purpose: Purpose) -> ual::ArgumentSchema {
    ual::ArgumentSchema {
        title: None,
        description: None,
        schema: ual::ValueSchema(serde_json::json!({})),
        purpose: Some(ual::PurposeSelector::Single(purpose)),
        extensions: BTreeMap::new(),
    }
}
