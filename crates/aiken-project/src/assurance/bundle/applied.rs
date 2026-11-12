//! Applied values are raw Flat terms; specialized scripts are single-CBOR programs.
//! These are the encodings checked by the pinned consumer's Applied.decodeValue
//! and Assurance.checkApplied, respectively. The template blueprint stays unchanged.

use crate::{assurance::Target, blueprint::Blueprint};
use aiken_ual as ual;
use pallas_codec::{flat, minicbor};
use std::collections::BTreeMap;
use uplc::{
    PlutusData,
    ast::{Constant, Data, DeBruijn, Term},
};

pub(super) fn lower(
    blueprint: &Blueprint,
    target: &Target,
    parameters: &[String],
    artifacts: &mut BTreeMap<String, Vec<u8>>,
) -> Result<ual::ParameterBinding, String> {
    let template = blueprint
        .validators
        .iter()
        .find(|validator| {
            validator
                .handler_title_parts()
                .is_some_and(|(module, name, _)| (module, name) == (&*target.module, &*target.name))
        })
        .ok_or_else(|| {
            format!(
                "applied validator {:?} is missing from the blueprint",
                target.id
            )
        })?;
    let count = template.parameters.len();
    if count == 0 {
        return Err(format!(
            "validator {:?} has no parameters; omit the parameter brackets",
            target.id
        ));
    }
    if parameters.len() != count {
        return Err(format!(
            "validator {:?} requires exactly {count} applied parameters in declaration order, but received {}",
            target.id,
            parameters.len()
        ));
    }

    // Validator::apply validates each next parameter against the existing blueprint
    // schema before applying it. Clone only this validator, never the reusable base.
    let mut specialized = template.clone();
    let mut values = Vec::with_capacity(count);
    for (index, parameter) in parameters.iter().enumerate() {
        let label = || format!("validator {:?} parameter {}", target.id, index + 1);
        let bytes = hex::decode(parameter)
            .map_err(|error| format!("{} is not CBOR hex: {error}", label()))?;
        let mut decoder = minicbor::Decoder::new(&bytes);
        let data: PlutusData = decoder
            .decode()
            .map_err(|error| format!("{} is not Plutus Data CBOR: {error}", label()))?;
        if decoder.position() != bytes.len() {
            return Err(format!(
                "{} has trailing bytes after Plutus Data CBOR",
                label()
            ));
        }
        specialized = specialized
            .apply(&blueprint.definitions, &data)
            .map_err(|error| format!("{} does not match its schema: {error}", label()))?;
        let term: Term<DeBruijn> = Term::Constant(Constant::Data(Data::from(data)).into());
        // Encode the term directly: Program::to_flat would prepend a version.
        let bytes = flat::encode(&term)
            .map_err(|error| format!("{} cannot be encoded as Flat: {error}", label()))?;
        values.push(ual::AppliedParameter {
            parameter: format!("/parameters/{index}"),
            term: artifact("value", "flat", bytes, artifacts),
        });
    }
    let (hash, script) = specialized.program.compiled_code_and_hash();
    Ok(ual::ParameterBinding::Applied {
        values,
        applied_script_hash: hash.to_string(),
        applied_script: artifact("script", "cbor", script.to_vec(), artifacts),
    })
}

fn artifact(
    kind: &str,
    extension: &str,
    bytes: Vec<u8>,
    artifacts: &mut BTreeMap<String, Vec<u8>>,
) -> ual::Artifact {
    let hash = ual::sha256_digest(&bytes);
    let uri = format!("applied-{kind}-{}.{extension}", hash.digest);
    artifacts.entry(uri.clone()).or_insert(bytes);
    ual::Artifact {
        uri,
        hash,
        media_type: Some("application/octet-stream".to_owned()),
        extensions: BTreeMap::new(),
    }
}
