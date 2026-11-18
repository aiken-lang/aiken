---
status: accepted
---
# Target ids are mangled qualified names: `module_path__name`

Decision Q21 chose qualified Aiken names for target ids so they are unique by construction. The consumer only accepts ids matching `^[A-Za-z0-9_-]+$` and uses the id as the Lean wrapper name that propositions call, so `auction/lib.outbids` cannot be emitted as is. The producer maps the module path, with `/`, `-` and `.` replaced by `_`, then `__`, then the name: `auction_lib__outbids`, `vesting__vesting`, and `fixtures_extra__v` for validator `v` in `validators/fixtures.extra.ak`. Annotation scopes keep using plain Aiken names (targets are always in the property's own module); only propositions use the emitted id. If two targets map to the same id, the producer reports an error naming both, never a silent rename.

## Domain

Aiken accepts module paths matching `([a-z][-_a-z0-9]*/)*[a-z][-_a-z0-9]*(\.[-_a-z0-9]*)*` (the file name may contain dots). Every such path maps into the schema alphabet. Ids of modules without dots are unchanged by the 2026-11-10 extension to `.`.

`.` maps to `_`, not to the schema-legal `-`, because the pinned consumer binds `sanitizeName(id)` (every character other than an ASCII letter, digit or `_` becomes `_`). With `_`, the id is exactly the Lean name a proposition writes; a `-` would give a distinct id with the same Lean name and add no separation.

The mapping is not injective: `a.b`, `a/b`, `a-b` and `a_b` give the same id prefix, and `a__b.c` meets `a.b__c`. Every such merge is an error naming both sources.

Blueprint titles keep the source spelling (`fixtures.extra.v`). The producer splits a handler title `module.validator.handler` from the right: validator and handler names cannot contain dots, so the module is everything before the last two dots.

## Generated-name collisions

Distinct ids are not enough. PlutusCoreBlaster `1c23de8` checks each property in a fresh copy of one Lean namespace, and declares there, after `sanitizeName` (written `S`):

| Source | Declarations | When |
| --- | --- | --- |
| Every blueprint validator, scoped or not | `S(title)`, `S(id)` | always |
| | `S(title)_hash` | the validator has a hash (always, for Aiken output) |
| | `S(title)_paramCount` | it has parameters and is not an applied target of this property |
| A validator selected as this property's checking target | `S(title)_script`, `S(id)_script` | the applied wrapper is emitted |
| A scoped function | `S(id)`, `S(id)_script`, `S(id)_returns` | in the property that scopes it |
| The property's fragment closure | each single-segment helper | in that property |

The producer computes this set for each property from the bundle it is about to write, and rejects a name declared twice before publishing. The diagnostic names both sources and the colliding Lean name. Examples: functions `f` and `f_returns` scoped together (`fixtures__f_returns`), validators `v` and `v_hash` in one module (`fixtures_v_hash`), and validator `v` in modules `m` and `m_` (`m__v` is `m.v`'s id and `m_.v`'s title binding). Because every property imports every validator, validator-only collisions fail every property; function, wrapper and helper collisions only matter inside the property that elaborates both. Functions `f` and `f_returns` scoped by separate properties are accepted.

Not modeled: schema-derived types, constructors and instances. Their names contain an uppercase letter (Aiken type names are capitalized), while validator and function names are lowercase, so they cannot collide with each other; a helper spelled like a generated type remains a consumer error. Names from the consumer's own libraries are also outside the model.

## Considered options
- Bare names, project-unique (the Plinth convention). Rejected: two modules each defining `validate` would collide in ordinary projects.
- An injective escape scheme. Rejected: wrapper names are written by hand in propositions and must stay readable; collisions are rare and caught.
- Mapping `.` to `-`. Rejected: see Domain.
- A project-wide ban on names ending in `_returns` or `_script`. Rejected: such targets are valid unless one property elaborates both.

## Consequences
- Renaming or moving a module changes the ids, so propositions that call the wrapper must change too, and old evidence no longer matches.
- The mapping lives in one function so it can follow the consumer if its id rules change; the declaration inventory lives in `assurance/bindings.rs` and must follow consumer changes to `elabBlueprintImport` and `emitAssuranceFunction`.
