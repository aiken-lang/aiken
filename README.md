# AIKEN

A cardano smart contract language and toolchain

## Roadmap

These are generic milestones and the listed ordering
is not necessariy the implementation order or full scope.

- [x] compile plutus core into it's on chain encoding
- [x] reverse the on chain encoding into plutus core
- [ ] Plutus Core interpreter
- [ ] create a higher level syntax with inspiration from
  - JS
  - ReasonML
  - Elm
  - Roc
  - Rust
  - Gleam
- [ ] Language Server

## Resources

- Encoding/Decoding https://github.com/input-output-hk/plutus/blob/master/plutus-core/untyped-plutus-core/src/UntypedPlutusCore/Core/Instance/Flat.hs
- Typing https://github.com/input-output-hk/plutus/blob/master/plutus-core/untyped-plutus-core/src/UntypedPlutusCore/Core/Type.hs
- Serialization https://hydra.iohk.io/build/14133599/download/1/plutus-core-specification.pdf
