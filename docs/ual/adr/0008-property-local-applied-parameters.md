---
status: accepted
---
# Property-local validator applications use CBOR scope suffixes

A property binds a validator's complete parameter list by appending `[CBOR_HEX,...]` to its scope, after any handler selector:

```aiken
//@ PROPERTY deployed_rejects for vesting.spend[00]
  "The zero-minimum deployment rejects spending."
  ∀ (ctx : ScriptContext),
    isUnsuccessful (fixtures__vesting (toLedgerData ctx))
@//
```

The values are hex-encoded Plutus Data CBOR, exactly the input representation of `aiken blueprint apply` (for example, `182a` is integer 42). Values follow validator declaration order. Whitespace is allowed inside brackets; the opening bracket immediately follows the scope. Hex case is insignificant. No suffix means universal mode. Function scopes cannot have this suffix. Applied mode requires a nonempty, complete parameter list; partial application, extra values, invalid CBOR, trailing CBOR payloads and schema mismatches are errors. Repeated scopes for one validator in a property must use the same binding; distinct properties may use different values or remain universal.

## Artifact contract

The blueprint retains the unapplied template, its hash, parameter schemas and all invocations. Each applied checking target has `parameters.mode: applied`, ordered `values` referencing `/parameters/i`, `appliedScript`, and `appliedScriptHash`. This is the document binding corresponding to UAL's `appliedParameters` producer API, not an additional blueprint field.

Each value artifact is a raw Flat **term**, a closed Data constant without a program-version prefix or CBOR envelope. The specialized program is the unoptimized, left-associated application of the template to these constants, serialized as single-CBOR Flat, with its normal Plutus V3 script hash. Content-addressed `applied-value-<sha256>.flat` and `applied-script-<sha256>.cbor` files live beside the checking contexts. Artifact SHA-256 digests bind exact bytes; the checking-context digest binds the values and specialized identity. Identical artifacts are shared. The writer owns these narrowly named files and prunes obsolete ones after publication.

The generated wrapper keeps its ordinary emitted target ID but accepts **runtime arguments only** for that property's context. Aiken does not rewrite the Lean proposition or remove its quantifiers: authors write the applied claim explicitly. Handler premises and execution settings are unchanged. The consumer validates values, order, coverage, hashes and exact application structure before checking the proposition.

## Alternatives and limits

- External parameter files would introduce path resolution and mutable input dependencies into the otherwise source-based annotation pass. Inline CBOR reuses the deployed-blueprint input representation without a second value language.
- A validator-wide binding would prevent universal and multiple deployment claims from coexisting. Property-local suffixes also support mixed scopes without positional ambiguity between validators.
- Optimizing after application could produce equivalent code but would violate the pinned consumer's exact AST application check. No post-application optimization is performed.
- This checks the specialized program directly, not an instantiation of a previously verified universal theorem. SMT trust and CEK bounds are unchanged.
- At the current consumer pin, historical evidence `scriptHash` identifies the template; `checkingContextHash` additionally binds the specialized hash and parameter artifacts. The run archive includes the exact value and specialized-program bytes. No schema or consumer pin is changed.

## Sources

- [UAL 1.0 applied deployments](https://github.com/input-output-hk/UniversalAnnotationLanguage/blob/1f7ab847736e5fa53cadc2099300cdaf3c720c6b/UniversalAnnotationLanguage.md#applied-deployments).
- Pinned consumer [Flat value validation](https://github.com/input-output-hk/PlutusCoreBlaster/blob/1c23de8f35fdc5decf6f126c7b56ed6649e4a57f/PlutusCore/UPLC/BlueprintEncoding/Applied.lean) and [application/evidence binding](https://github.com/input-output-hk/PlutusCoreBlaster/blob/1c23de8f35fdc5decf6f126c7b56ed6649e4a57f/PlutusCore/UPLC/BlueprintEncoding/Assurance.lean).
- Existing [blueprint apply CLI](../../../crates/aiken/src/cmd/blueprint/apply.rs).
