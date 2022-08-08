<div align="center">
  <img src="https://raw.githubusercontent.com/txpipe/aiken/main/assets/logo-light.png?sanitize=true#gh-dark-mode-only" alt="Aiken" max-height="240" />
  <img src="https://raw.githubusercontent.com/txpipe/aiken/main/assets/logo-dark.png?sanitize=true#gh-light-mode-only" alt="Aiken" max-height="240" />
  <hr />
    <h2 align="center" style="border-bottom: none">Cardano smart contract language and toolchain</h2>
  
[![Licence](https://img.shields.io/github/license/txpipe/aiken)](https://github.com/txpipe/aiken/blob/main/LICENSE) 
[![Crates.io](https://img.shields.io/crates/v/aiken)](https://crates.io/crates/aiken)
[![Rust Build](https://github.com/txpipe/aiken/actions/workflows/rust.yml/badge.svg?branch=main)](https://github.com/txpipe/aiken/actions/workflows/rust.yml)
[![Nix Build](https://github.com/txpipe/aiken/actions/workflows/nix.yml/badge.svg?branch=main)](https://github.com/txpipe/aiken/actions/workflows/nix.yml)
  
  <hr/>
</div>

## Install

For now you'll need rust installed, see [rustup](https://rustup.rs).

`cargo install aiken`

## Usage

For now the command line application can only encode/decode Untyped Plutus Core
to/from it's on chain format. See the roadmap below for a list of planned features and goals.

```sh
# help
aiken help

# compile an untyped plutus core program to flat
aiken uplc flat program.uplc

aiken uplc flat program.uplc --print

# output
00001011 00010110 00100001 01001000
00000101 10000001

aiken uplc flat program.uplc --out=something.flat

# decode an untyped plutus core program from flat
aiken uplc unflat program.flat

aiken uplc unflat program.flat --print

# output
(program
  11.22.33
  (con integer 11)
)

aiken uplc unflat program.flat --out=something.uplc
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
- [x] Plutus Core interpreter
- [ ] create a higher level syntax with inspiration from
  - JS
  - ReasonML
  - Elm
  - Roc
  - Rust
  - Gleam
- [ ] Language Server

## Art Credit

We'd like to give a special thanks to [@nkz](https://twitter.com/nkzthecreator)
for creating the logo and giving us the idea to name the project aiken.

**Why Aiken?**

The name comes from [Howard Aiken](https://en.wikipedia.org/wiki/Howard_H._Aiken),
an American physicist and a pioneer in computing.

## Resources

- Encoding/Decoding https://github.com/input-output-hk/plutus/blob/master/plutus-core/untyped-plutus-core/src/UntypedPlutusCore/Core/Instance/Flat.hs
- Typing https://github.com/input-output-hk/plutus/blob/master/plutus-core/untyped-plutus-core/src/UntypedPlutusCore/Core/Type.hs
- Serialization https://hydra.iohk.io/build/14133599/download/1/plutus-core-specification.pdf
