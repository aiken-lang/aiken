use serde::Serialize;

/// Encode JSON with recursively sorted object keys, two-space indentation and one
/// final LF. Array order and string contents are preserved. This is a UAL file
/// encoding, not RFC 8785: hashes must cover these bytes, not re-serialized input.
///
/// Sorting is explicit so enabling serde_json's `preserve_order` feature elsewhere
/// in a producer's dependency graph cannot change the emitted bytes.
pub fn canonical_bytes<T: Serialize + ?Sized>(document: &T) -> Result<Vec<u8>, serde_json::Error> {
    canonical_value(serde_json::to_value(document)?)
}

pub(crate) fn canonical_value(mut value: serde_json::Value) -> Result<Vec<u8>, serde_json::Error> {
    value.sort_all_objects();
    let mut bytes = serde_json::to_vec_pretty(&value)?;
    bytes.push(b'\n');
    Ok(bytes)
}
