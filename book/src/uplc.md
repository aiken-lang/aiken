# Untyped Plutus Core

One key feature of Aiken is how it helps you manipulate Untyped Plutus Core
(abbrev. UPLC in short). UPLC is ultimately the format whereby codes gets
executed on-chain. This is pretty-much as low-level as you can get when it
comes to Cardano smart-contracts.

Understanding how UPLC works, and having the right tools to troubleshoot
UPLC programs can be handy when developing contracts on Cardano. Fortunately,
this is something Aiken can help you with.

> **Note**
>
> While UPLC has erased any _explicit_ notion of types; functions, variables and
> constants are still _implicitly_ typed and, an interpreter will raise errors
> when encountering a type mismatch.
>
> For the sake of simplicity, we might speak about the type-signature of
> builtin functions such as `addInteger` which, in principle, only has a
> concrete meaning in Typed Plutus Core. Hence, even though they are _untyped_,
> we often think of UPLC programs has having implicit types, as if they were
> originally _typed_ programs whose types had simply been erased (in fact,
> that's exactly what they are).
