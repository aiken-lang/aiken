use cryptoxide::{digest::Digest as _, sha2::Sha256};

use crate::model::Digest;

/// SHA-256 of exact bytes. File input must not be parsed or normalized first.
pub fn sha256(bytes: &[u8]) -> [u8; 32] {
    let mut hasher = Sha256::new();
    hasher.input(bytes);
    finish(hasher)
}

fn finish(mut hasher: Sha256) -> [u8; 32] {
    let mut result = [0; 32];
    hasher.result(&mut result);
    result
}

/// A UAL digest object for exact file bytes.
pub fn sha256_digest(bytes: &[u8]) -> Digest {
    Digest::new("sha256", hex::encode(sha256(bytes)))
}

/// Hash hex-decoded `compiledCode`, including its single CBOR wrapper. Neither
/// the ASCII hex nor the unwrapped Flat program is the consumer's function hash.
/// Decoding uses bounded scratch space even for large programs.
pub fn function_digest(compiled_code: &str) -> Result<Digest, hex::FromHexError> {
    if !compiled_code.len().is_multiple_of(2) {
        return Err(hex::FromHexError::OddLength);
    }
    let mut hasher = Sha256::new();
    let mut decoded = [0; 1024];
    for chunk in compiled_code.as_bytes().chunks(decoded.len() * 2) {
        let bytes = &mut decoded[..chunk.len() / 2];
        hex::decode_to_slice(chunk, bytes)?;
        hasher.input(bytes);
    }
    Ok(Digest::new("sha256", hex::encode(finish(hasher))))
}

#[derive(Debug, thiserror::Error)]
pub enum DigestError {
    #[error("unsupported digest algorithm {0:?}; expected SHA-256")]
    UnsupportedAlgorithm(String),
    #[error("SHA-256 mismatch: expected {expected}, computed {actual}")]
    Mismatch { expected: String, actual: String },
}

impl Digest {
    /// Check an artifact's exact bytes, accepting the consumer's SHA-256 aliases.
    /// Other algorithms are never treated as an unchecked successful binding.
    pub fn verify(&self, bytes: &[u8]) -> Result<(), DigestError> {
        if !self.alg.eq_ignore_ascii_case("sha256") && !self.alg.eq_ignore_ascii_case("sha-256") {
            return Err(DigestError::UnsupportedAlgorithm(self.alg.clone()));
        }
        let actual = sha256(bytes);
        let mut expected = [0; 32];
        if hex::decode_to_slice(&self.digest, &mut expected).is_ok() && expected == actual {
            Ok(())
        } else {
            Err(DigestError::Mismatch {
                expected: self.digest.clone(),
                actual: hex::encode(actual),
            })
        }
    }
}
