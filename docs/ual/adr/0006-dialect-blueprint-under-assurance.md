---
status: accepted
---
# The compiled-interface blueprint is written to `assurance/plutus.json`; the root `plutus.json` is untouched

`aiken build` writes today's CIP-57 `plutus.json`, which wallets and off-chain tooling read. The UAL consumer needs the compiled-interface dialect, and `assurance.json` pins that blueprint's exact bytes by hash. Writing both to the same path would let a later `aiken build` silently invalidate the bundle. `aiken verify generate` therefore writes its blueprint inside `assurance/`, beside the documents that hash it, and never touches the root file.

## Consequences
- A project has two blueprints until the dialect is published and adopted as Aiken's default; they share compiledCode and script hashes for every validator.
