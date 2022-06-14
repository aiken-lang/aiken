# AIKEN

A cardano smart contract language and toolchain

## Install

For now you'll need rust installed, see [rustup](https://rustup.rs).

`cargo install aiken`

## Usage

For now the command line application can only encode/decode Untyped Plutus Core
to/from it's on chain format. See the roadmap below for a list of planned features and goals.

```sh
# compile an untyped plutus core program
aiken uplc flat program.uplc
```

## Roadmap

In general, the goal is to port everything we need for plutus to
Rust. This will be needed if we ever want to build a full node in
Rust. Since we will have these tools natively in Rust, we plan on
building a new high level language for writing smart contracts on Cardano.
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
