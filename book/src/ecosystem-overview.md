# Ecosystem Overview

Within the Cardano community there has been a flourishing ecosystem
of alternative languages for writing smart contracts. So naturally, one might ask about
the differences between these and which they should use for their use case. There is
also a big misconception about how writing smart contracts actually works on Cardano. In this document,
we'll list some of the main alternatives along with their differences and similarities. Before we get into this though, let's discuss the misconception first so everyone is on the same page.

## The Misconception

**Cardano uses Haskell for smart contracts**

This is **not** entirely true.

The main Cardano node implementation does indeed happen to be written in
Haskell. The virtual machine for executing smart contracts that comes baked
into the node is then of course also implemented in Haskell. **But** that does
not mean that it is Haskell itself which is executed by the smart contract
virtual machine. Aiken actually has a fully working version of this [virtual
machine](https://github.com/txpipe/aiken/blob/main/crates/uplc/src/machine.rs#L63)
written in Rust.

So what's going on here? What is actually being executed?

Well, there is something called [Untyped Plutus Core](./uplc.md) which is the
lowest level representation of a smart contract and it is this low level
representation that actually gets executed by the virtual machine. So contrary
to popular knowledge, there isn't actually a coupling to Haskell. Armed with
this knowledge one may now ask another question:

**So what am I writing when I write Plutus?**

In the wild, Plutus tends to refer to one of three things:

1. _Plutus Core_, the low-level interpreted code that is executed by the
   Cardano virtual machine.
2. _PlutusTx_, a Haskell framework that compiles to Plutus Core through the
   means of a GHC plugin.
3. The _Plutus Platform_, which more broadly includes _Plutus Core_, _PlutusTx_
   and most of the tools developed around _Plutus Core_.

Most of the time, when people say _Plutus_, they mean _PlutusTx_, which has led
to a popular belief that Plutus is in fact Haskell.

_PlutusTx_ being built as a GHC plugin means that you even use Haskell tooling
like cabal for it. Even so, you are technically not writing Haskell. Code that
one writes using _PlutusTx_ is consumed by the plugin and then transformed into
_Untyped Plutus Core_. Essentially, it takes the intermediate representation of
Haskell, GHC Core, and turns that into Untyped Plutus Core. This results in not
needing to write a new parser and type checker. What you end up with is a kind
of embedded language that looks and feels like Haskell but the target runtime
is not GHC.

## The Alternatives

Now that this misconception is out of the way it should be possible to see how
other new languages can be created that ultimately compile to Untyped Plutus
Core. The current alternatives range from full blown new languages to embedded
Domain Specific Languages (abbrev. eDSLs.) Here is a list of the main ones:

- [Aiken](https://github.com/txpipe/aiken)
- [Helios](https://github.com/Hyperion-BT/Helios)
- [Plutarch](https://github.com/Plutonomicon/plutarch-plutus)
- [plu-ts](https://github.com/HarmonicLabs/plu-ts)
- [Scalus](https://github.com/nau/scalus)

The creators of each of these projects all know each other and are in open
communication with each other.

### Aiken

Aiken is a brand new language with it's own syntax and compiler. It is not Rust. The compiler
happens to be written in Rust but it is not Rust. Not only is Aiken a compiler
for a new language but we've also developed everything in such a way that all
the libraries we created in Rust are re-usable by people interested in doing
more low-level things. One example of this is
[Lucid](https://github.com/spacebudz/lucid), which uses Aiken's
[uplc](https://crates.io/crates/uplc) crate to evaluate transactions before
submission to calculate exact redeemer ExUnits without using a node, ogmios, or
blockfrost.

As a language, Aiken is purely functional with static typing and type
inference. This means most of the time the compiler is smart enough to know
what the type of something is without you annotating it. It also let's you make
custom types that are similar to records and enums. It does not have
higher-kinded types or typeclasses because Aiken aims for simplicity. Writing
smart contracts can be tedious, and we therefore believe that a language
should remain simple to avoid silly mistakes.

On-chain scripts are typically small in size and scope (relatively, compared to
other kind of applications being written nowadays) and, therefore, do not
require as much features as general-purpose languages that have to solve much
harder problems.

That being said Aiken may introduce more elaborate language features (such as
type classes/traits) at a later time if it's found that they are extremely
useful to developers.

### Helios

Helios is also a brand new language. One notable implementation difference is
that it's compiler is written in a [single javascript file without
dependencies](https://github.com/Hyperion-BT/Helios/blob/main/helios.js).
According to the creator, the intention of that was to make the compiler
implementation easier to audit.

As a language, Helios is also purely functional but has limited have type
inference. It also supports custom types similar to records and enums.

Another interesting thing is that because the compiler is a single javascript
file it's pretty easy to use Helios from within a javascript project.

### Plutarch

Plutarch is **not** a new language. You can consider it an eDSL for creating
smart contracts with Haskell. In some ways, Plutarch is what PlutusTx should
have been. There is no template Haskell involved.

Since Plutarch is just Haskell, you have everything available to you. Type
inference, typeclasses, higher-kinded types, etc.

### plu-ts

plu-ts is **not** a new language. You can consider it an eDSL for creating smart contracts with Typescript.
Because of this it's a bit closer to Plutarch conceptually than Aiken or Helios.

It implements it's own type system and at compile time (js runtime) checks the types to be correct.

### Scalus

A Scala implementation of Plutus.

Scalus is a set of libraries to work with Cardano Untyped Plutus Core that works on both JVM and JavaScript. This includes:

- Untyped Plutus Core (UPLC) data types and functions
- Flat, CBOR, JSON serialization
- CEK UPLC evaluation machine including execution cost calculation
- UPLC parser and pretty printer
- Type safe UPLC expression builder, think of Plutarch
- Macros to generate UPLC code from Scala code, think of PlutusTx but simpler

## Which should you use?

Only you can decide for yourself which of these fits your needs the best. Each
has made some different decisions around design and implementation. Aiken and
Helios are on the **new language** end of the spectrum while Plutarch and
plu-ts are on the eDSL end. Plutarch has the most expressive type system while
Aiken's types are in between Plutarch and Helios.

Embedded DSLs are nice because they integrate seamlessly with off-chain code and
usually allow to reuse existing tools that already work on the host language.

New languages are nice because they include bespoke checks and functionality
specifically for Cardano smart contracts directly in their compilers. While
they demand a lot of the tooling to be created anew, they also give the
opportunity to address shortcomings of existing tooling in various languages.

Which best serves your use case is for you to say. Being the maintainers behind
Aiken, we can't be fully partial in providing an unbiaised answer. We encourage
you to review the documentation, design decisions and overall project to make
an informed decision.
