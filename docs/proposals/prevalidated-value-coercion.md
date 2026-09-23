# Checked Value construction and coercion of prevalidated values

Status: proposal for discussion; no compiler or stdlib behavior is changed.

## Problem

A validator can authenticate a datum containing an asset map using a state
token. Its minting policy checks the map when creating the initial output, and
the spending validator preserves that invariant for every continuing output.
Later consumers should be able to use the authenticated map as a
`cardano/assets.Value` without sorting it or checking every entry again.

This is different from trusting a transaction input's ledger-provided `value`.
An asset map inside a datum is user-supplied data, so the protocol must establish
its invariants explicitly.

Currently, stdlib's `assets.from_asset_list` checks asset-name order but inserts
policy entries into a dictionary, sorting the outer map. It cannot establish
that the original datum already had ascending policy order. The combination of
`dict.from_ascending_pairs` for each inner map and
`assets.from_ascending_pairs` for the outer map provides a checked conversion
without sorting, but still traverses the value.

`Value` and its nested `Dict` types are opaque. Their public APIs do not expose
an unchecked constructor from nested pairs. This proposal requests a supported
way for the libraries that own those types to provide one; it does not propose
using a type-checker loophole.

## Requested API

The motivating application API is:

```aiken
// Validate without sorting or dropping entries. Fail on invalid input.
pub fn is_canonical_value(
  xs: Pairs<PolicyId, Pairs<AssetName, Int>>,
) -> Value

// No validation, sorting, filtering, or traversal. Requires prior validation.
pub fn unsafe_coerce_value(
  xs: Pairs<PolicyId, Pairs<AssetName, Int>>,
) -> Value
```

These are proposed signatures, not executable declarations. Although the first
name starts with `is_`, its requested result is `Value`, not `Bool`; a name such
as `expect_canonical_value` or `from_ascending_asset_list` may better fit the
stdlib conventions.

The checked constructor must enforce the structural invariants of `Value`:

- Policy IDs are strictly ascending in bytewise lexicographic order.
- Asset names within each policy are strictly ascending in that same order.
- There are no duplicate keys at either level.
- Inner maps are nonempty and quantities are nonzero.
- The empty outer map represents zero.

Signed quantities remain valid for a general-purpose `Value`. Nonnegativity,
ledger policy-ID and asset-name length limits, and the special ADA namespace
are separate application constraints. This proposal must not silently change
those existing constructor semantics.

Successful validation preserves the original ordered Plutus data. In
particular, the byte strings for `alpha` and `beta` remain in that order even
though a length-first CBOR key ordering would put `beta` first. The relevant
ordering is on keys' bytes, not on their encoded lengths.

## Compiler and library boundary

The preferred design is an explicit, module-owned unchecked constructor for
representation-compatible opaque wrappers. The compiler would establish that
the conversion changes only the static type, and the library would opt into
exposing the unchecked operation with documented preconditions.

For the motivating case, both the source pairs and `Value` must have the same
representation after the compiler's supported opaque-wrapper erasure. A
conversion requiring list reconstruction, a recursive decoder, serialization,
or a different runtime representation does not meet the request.

The nested dictionaries matter: merely exposing the outer `Value` constructor
would leave the conversion from inner pairs to `Dict` unresolved. The mechanism
must account for both layers, with explicit participation from the owners of
both opaque types. A generic pair list's type parameters cannot simply be
changed through an ordinary identity function.

This is not a request for arbitrary coercion between unrelated types, or for
ordinary `expect` casts to stop respecting opacity. Existing safe casts and
constructor visibility should retain their behavior. The exact opt-in syntax
and how to express representation compatibility need maintainer agreement
before implementation.

## When the unchecked operation is valid

The calling protocol must establish all of the following:

1. Its actual minting policy validates the exact datum field before the state
   token authenticates the initial output.
2. The policy binds that token to the intended output and spending validator.
3. Every transition that can recreate the authenticated state either preserves
   the field exactly or validates a replacement before continuing.
4. The consuming script authenticates the correct policy, token, and state
   lineage, rather than accepting a datum merely because an output contains
   some token.
5. No alternative creation, update, or migration path bypasses that invariant.

The compiler cannot prove these cross-transaction facts. The unchecked
constructor's contract must make clear that the caller supplies this proof.
The checked constructor remains the entry point for untrusted datum fields.

## Acceptance criteria for an implementation

The checked constructor should have unit and property coverage for:

- Empty values, ADA-only values, and multiple policies and asset names.
- Exact preservation of valid input data.
- Rejection of decreasing or duplicate policy IDs and asset names.
- Rejection of zero quantities and empty inner maps.
- Acceptance of signed nonzero quantities.
- Variable-length names for which bytewise and length-first order differ.

The unchecked constructor needs tests showing that it preserves valid input
data and performs no validation, normalization, or traversal. Its documented
precondition must remain visible at the public API.

The zero-cost claim needs generated-code evidence, not timing of constant
examples: compare optimized UPLC for a function that consumes a dynamic pairs
argument directly with one that round-trips the same argument through the
unchecked constructor and the inverse view. They must produce identical code.
Execution-budget checks at several input sizes should corroborate that result.
Use parameters unknown at compilation so constant folding cannot hide a walk.

Compiler tests must cover imported and nested opaque wrappers, generic
instantiation, representation-incompatible conversions, and preservation of
existing opacity checks. Any opt-in mechanism must reject conversions that do
not satisfy its representation rule.

## Questions for maintainers

- Is a module-owned unchecked constructor an acceptable addition to Aiken's
  opacity model, or should authenticated data be modeled differently?
- What syntax or compiler API should express the representation constraint and
  opt-in for nested opaque wrappers?
- Should the checked constructor land independently in stdlib while compiler
  support for the unchecked constructor is discussed?

No zero-cost implementation or performance result is claimed by this proposal.
