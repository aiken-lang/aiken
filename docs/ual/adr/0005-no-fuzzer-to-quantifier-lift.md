---
status: accepted
---
# Property-based tests are never lifted into formal properties

The previous integration translated `test f(x via fuzzer)` into `∀ x ∈ dom(fuzzer), f x = true`. This is rejected. A fuzzer is a sampling distribution chosen for test coverage, not a statement of the domain a property is meant to hold on; the lifted theorem silently says less than the specification (or something different, since fuzzers are neither surjective nor well-defined as sets once composed) while reading as if it verified the test. Properties are written once, by a specifier, as complete quantified propositions in `PROPERTY` blocks. A property test and a formal claim may check the same specification, but the test is never the source of the claim.

## Consequences
- There is no `verify` mode that runs without hand-written annotations.
- No fuzzer-domain table or `DOMAIN` annotation exists in Aiken or in `aiken-lang/fuzz`.
