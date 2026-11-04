---
status: accepted
---
# Target ids are mangled qualified names: `module_path__name`

Decision Q21 chose qualified Aiken names for target ids so they are unique by construction. The consumer only accepts ids matching `^[A-Za-z0-9_-]+$` and uses the id as the Lean wrapper name that propositions call, so `auction/lib.outbids` cannot be emitted as is. The producer maps the module path, with `/` and `-` replaced by `_`, then `__`, then the name: `auction_lib__outbids`, `vesting__vesting`. Annotation scopes keep using plain Aiken names (targets are always in the property's own module); only propositions use the emitted id. If two targets map to the same id, the producer reports an error naming both, never a silent rename.

## Considered options
- Bare names, project-unique (the Plinth convention). Rejected: two modules each defining `validate` would collide in ordinary projects.
- An injective escape scheme. Rejected: wrapper names are written by hand in propositions and must stay readable; collisions are rare and caught.

## Consequences
- Renaming or moving a module changes the ids, so propositions that call the wrapper must change too, and old evidence no longer matches.
- The mapping lives in one function so it can follow the consumer if its id rules change.
