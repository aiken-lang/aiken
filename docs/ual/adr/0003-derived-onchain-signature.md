---
status: accepted
---
# The producer derives the ONCHAIN signature; users never write encodings

UAL lets the author write `{ Type : encoding }` per argument. Aiken already fixes the wire form: exported functions receive every argument as Data (`cast_validator_args`) and return native constants for Int/ByteArray/Bool/String/Void but Data for ADTs; validators take parameters as Data and a raw-Data context. A hand-written signature can only agree with this or be rejected, so the producer emits it and the user writes only execution settings. The emitted document is byte-identical to a correct hand-written one, so the mapping to UAL still holds at the document level, which is what the consumer sees.

## Consequences
- This deviates from "the annotation is authoritative" for one field. The consumer is unaffected: it checks interface equality, encodings and digests on the documents, whoever wrote them.
- Aiken's `List<T>` always has Data elements. Native `#list`/`#pair` results are emitted with Data elements, which is how the consumer binds them.
