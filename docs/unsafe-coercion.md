# Coercing prevalidated values without runtime validation

`aiken/builtin.unsafe_coerce` converts between types that differ only by
single-field, undecorated opaque wrappers. The compiler proves representation
compatibility and compiles the operation as identity. It does not check the
semantic invariants of an opaque type.

```aiken
use aiken/builtin
use cardano/assets.{AssetName, PolicyId, Value}

pub fn unsafe_coerce_value(
  xs: Pairs<PolicyId, Pairs<AssetName, Int>>,
) -> Value {
  builtin.unsafe_coerce(xs)
}
```

This requires the compiler change accompanying this document. It does not add
functions to an already released stdlib. The runnable example in
[`examples/validated_value`](../examples/validated_value) provides both
`is_canonical_value(...) -> Value` and `unsafe_coerce_value(...) -> Value`
against stdlib v3.1.0.

## Checked construction

A map stored inside a datum is user-supplied data, unlike the ledger-provided
value of an input or output. It needs explicit validation before it may be
trusted as a `Value`.

The example's checked constructor validates both map levels without sorting or
dropping entries. It fails unless policy IDs and asset names are strictly
ascending in bytewise lexicographic order, inner maps are nonempty, and
quantities are nonzero. Strict ordering also excludes duplicate keys. The empty
outer map represents zero; signed nonzero quantities remain valid for a general
`Value`. Nonnegativity, ledger key-length limits, and the special ADA namespace
are separate application constraints.

In particular, the bytes for `alpha` precede `beta`. Length-first CBOR ordering
would put `beta` first and is not the ordering checked here. The checked helper
returns `Value`, despite its application-requested `is_` name, and preserves the
original ordered Plutus data.

## The unchecked trust boundary

A state-token protocol may establish a value invariant once and preserve it
across transactions:

1. The actual minting policy checks the exact datum field before authenticating
   the initial output with its state token.
2. The policy binds that token to the intended output and spending validator.
3. Every continuing transition preserves the field exactly or validates its
   replacement. Alternative creation and migration paths must do the same.
4. A consuming script authenticates the correct policy, token, and state lineage
   before using the unchecked conversion.

A token's presence alone is insufficient. These cross-transaction facts are the
caller's responsibility; the compiler does not prove them. Untrusted datum
fields must go through checked construction.

This intrinsic is an explicit escape from opaque-type invariants. It can also
be used with imported opaque types; their defining module does not need to
export a constructor. The operation neither validates nor repairs the input.
Ordinary constructor visibility, `expect`, and `if/is` opacity checks remain
unchanged.

## Representation restriction

The source and result must have the same type after recursively erasing opaque
wrappers with exactly one constructor, one field, and no decorators. The type
checker and code generator share the same helper for both wrapper eligibility
and the erased inner type. The compiler retains representation
metadata across module imports so nested stdlib dictionaries can be handled.

The proof compares complete type structure, including generic arguments and
nominal identities of types that are not erased. It does not equate unrelated
records simply because both happen to use Plutus `Data`. No runtime decoder or
serialization round-trip is inserted.

Examples of rejected conversions include `Int` to `ByteArray`, raw `Data` to a
nested-map `Value`, and `List<Int>` to `List<ByteArray>`. Function representations
and unconstrained polymorphic casts are also rejected. Representation proof is
bounded to 256 recursive levels and 4096 visits per endpoint, including generic
parameter binding, substitution, and type-variable links. Substitutions retain
their caller's scope and resolve lazily during the budgeted traversal; no
substituted type tree is scanned or copied beforehand. A proof exceeding those
limits is rejected rather than attempting unbounded expansion.

The result normally needs a type annotation, such as a function's result type.
An explicitly annotated function alias and qualified, unqualified, renamed,
and pipeline uses are supported. A generic helper must prove compatibility for
all its type parameters; it cannot export an arbitrary `fn(a) -> b` cast.

## Verification

Type-checker tests cover local, imported, nested, generic, and decorated
wrappers, aliases, pipelines, forward references, and incompatible higher-order
uses. Existing ordinary opacity rejection tests remain in the suite.

Budget tests cover exact acceptance/rejection boundaries, repeated parameters,
linked types, recursive metadata, wide types, and nested substitution scopes.
Another test compares proof metadata and backend erasure across ordinary,
nested, decorated, and non-erased types.

Code-generation tests compile a function with a dynamic nested-map argument,
coerce it to an imported opaque value and back, and compare its optimized UPLC
bytes with the function that uses the pairs directly. This prevents constant
folding from hiding a traversal. The tests require identical code with both
silent and verbose tracing, and matching execution results and budgets for
0, 1, 8, and 64 policies. An annotated first-class alias has its own identical-
code check. Additional dynamic-input tests require identical code, results, and
budgets for primitive types, the native UPLC `Value`, lists, pairs, tuples,
options, and records. They exercise ordinary accessors and pattern matching,
multiple specializations in one script, nested wrappers, and generic helpers
imported through a second module. Imported module interfaces are serialized
and reloaded before those tests compile the consumer.

The example separately exercises the public stdlib `Value`, including a value
containing ADA, `alpha`, and `beta`, and rejects incorrectly ordered maps,
duplicate keys, zero quantities, and empty inner maps. Two property tests use
raw pairs generated independently by checked stdlib constructors and exercise
data preservation, lookups, flattening, addition, cancellation, and ADA removal
through the actual stdlib `Value`. CI runs 1000 cases per property with seed
1436, including signed quantities and ADA/alpha/beta.
