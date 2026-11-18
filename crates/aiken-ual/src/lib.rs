//! UAL documents for producers and readers, independent of Aiken, Lean and Z3.
//!
//! [`validated_bytes`] emits schema-checked, deterministic JSON. Hash the returned
//! bytes with [`sha256_digest`] and write them unchanged. For existing artifacts,
//! hash their original bytes rather than parsing and re-serializing them.
//!
//! The five upstream schemas and consumer workspace lock are pinned in
//! `schemas/PIN`. Validation is offline. Schema validation does not execute UPLC,
//! elaborate formal source, verify evidence, or establish ledger acceptance.
//! [`validate_bindings`] checks the cross-document identity chain separately.

pub mod bindings;
pub mod canonical;
pub mod digest;
pub mod model;
pub mod validation;

pub use bindings::{BindingError, validate_bindings, validate_environment};
pub use canonical::canonical_bytes;
pub use digest::{DigestError, function_digest, sha256, sha256_digest};
pub use model::*;
pub use validation::{DocumentKind, ValidationError, validate};

/// The coordinated consumer lock, retained as data rather than build dependencies.
pub const ASSURANCE_WORKSPACE: &str = include_str!("../schemas/assurance-workspace.json");
pub const SCHEMA_PIN: &str = include_str!("../schemas/PIN");

#[derive(Debug, thiserror::Error)]
pub enum EncodeError {
    #[error(transparent)]
    Json(#[from] serde_json::Error),
    #[error(transparent)]
    Validation(#[from] ValidationError),
}

/// Validate against the selected pinned document schema before canonical encoding.
/// `Blueprint` selects the compiled-interface dialect, not ordinary CIP-57.
pub fn validated_bytes<T: serde::Serialize + ?Sized>(
    kind: DocumentKind,
    document: &T,
) -> Result<Vec<u8>, EncodeError> {
    let value = serde_json::to_value(document)?;
    validate(kind, &value)?;
    Ok(canonical::canonical_value(value)?)
}
