# Prevalidated datum values

This example requires the compiler containing `builtin.unsafe_coerce`; released
compilers without the intrinsic cannot compile it.

From the repository root:

```sh
cargo run -p aiken -- check examples/validated_value
```

`is_canonical_value` checks the stdlib `Value` invariants and returns a `Value`,
failing on invalid input without sorting. `unsafe_coerce_value` performs no
checks and requires those invariants to have been established already.

See [the compiler documentation](../../docs/unsafe-coercion.md)
for the state-token trust requirements and the generated-code tests that
establish zero runtime overhead. This is a utility example, not a state-token
validator or a proof of any application's minting and continuation rules.
