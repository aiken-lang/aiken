---
status: accepted
---
# UAL annotations live in `//@` comment blocks, not new grammar

Predicates and properties are Lean text. Aiken has no raw-block token, and the lexer already drops comments while recording their spans, which `aiken fmt` re-interleaves verbatim. Carrying annotations in a dedicated `//@ … @//` comment region (a new trivia kind the lexer swallows up to the closer) therefore round-trips through the formatter and LSP with no grammar change, matches Plinth's `{-@ ... @-}` mechanism one-to-one, and keeps the feature invisible to users who do not opt in. An assurance pass parses the blocks after type checking and resolves target ids against the typed module.

## Considered options
- A new opaque-block token and AST nodes. Rejected for now: formatter, LSP and grammar work for no gain in what the consumer sees; properties are not type-checked by Aiken either way.
- Reusing `///` doc comments. Rejected: they are documentation and already have a consumer.

## Consequences
- Annotation diagnostics come from the assurance pass with their own spans, not from the type checker.
