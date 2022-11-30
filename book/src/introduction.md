# Introduction

Aiken is a new programming language and toolchain for developing
smart contracts on the [Cardano](https://cardano.org) blockchain.

## Philosophy

Our main goal is to improve the smart contract development experience for the Cardano blockchain. Aiken takes inspiration from many modern languages such as aiken, rust, and elm which are known for friendly error messages and an overall excellent developer experience. We believe Cardano deserves a dedicated language with these kinds of features, developed in the open with the community.

## Goals

- We want an easy and safe way to write Cardano smart contracts. We should be able to get started in minutes not days.
- We want a complete and delightful experience. A modern blockchain deserves a modern smart contract toolkit. This includes editor integrations such as LSP and tree-sitter along with fancy error messages.
- We want there to be as little configuration as possible. It should work out of the box and have opinionated reasonable conventions established with the community.
- We want to have a modular design so that components can be picked and chosen as needed. Like the unix philosophy.

## Roadmap

In general, the goal is to port everything we need for plutus to
Rust. This will be needed if we ever want to build a full node in
Rust. Since we will have these tools natively in Rust, we plan on
building a new high level language for writing smart contracts on Cardano.

- [x] bare minimum toolkit for working with untyped plutus core
  - [x] serialize plutus core into it's on chain encoding
  - [x] deserialize the on chain encoding into plutus core
- [x] Plutus Core interpreter
- [ ] [v0.1.0 - Initial Alpha](https://github.com/txpipe/aiken/milestone/1)
  - [x] define aiken as a language
  - [x] implement lexing/parsing with pretty error messages
  - [x] type checking and inference
  - [ ] uplc code gen
- [ ] [v0.2.0 - Implement standard library
      ](https://github.com/txpipe/aiken/milestone/2)
- [ ] [v0.3.0 - Testing Framework](https://github.com/txpipe/aiken/milestone/4)
- [ ] [v0.4.0 - Improved Tooling](https://github.com/txpipe/aiken/milestone/3)
  - [ ] LSP 🚀
  - [ ] syntax highlighting plugins for editors 🎨

## Components

The Aiken project is made up of a few different components. The two most important
ones are the high level Aiken language for writing smart contracts and a rust library
for working with Untyped Plutus Core.

Learn more about them in the next sections of this book.
