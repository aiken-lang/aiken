# Variables & Constants

## Variables

Aiken has let-bindings for variables. A value can be given a name using the keyword `let`.
Names can be reused by later let-bindings.

Values assigned to let-bindings are immutable, however new bindings can shadow
previous bindings.

```aiken
let x = 1
let y = x
let x = 2

y + x == 3
```

## Constants

Let-bindings aren't allowed in a top-level Aiken module. Yet, Aiken provides
module constants as a way to use certain fixed values in multiple places of a
Aiken project.

```aiken
const start_year = 2101
const end_year = 2111
```

Like all values in Aiken, constants are immutable. They cannot be used as global mutable state. When a constant is referenced, its value is inlined by the compiler so they can be used in any place where you'd have written a literal in the first place (e.g. when-expression guards, if clauses ...). We'll see some example of that when dealing with control flows.

## Type annotations

Variables and constants can be given type annotations. These annotations serve as documentation or can be used to provide a more specific type than the compiler would otherwise infer.

```aiken
const name: String = "Aiken"
const size: Int = 100

let result: Bool = 14 > 42
```

## assert

Sometimes, it is useful to fail the exit and fail the execution of a program
early on. This is where the `assert` keywords comes in handy. `assert` causes
the script to fail if it fails to bind a given expression.

It is particularly useful when combined with custom types that can have
multiple constructors, and that are expected to have a very specific shape. It
is primarily used for validating datums and redeemers -- which can really be
anything.

Having ways to enforce that some inputs have the requested shape is thus
often necessary. For example, let's consider we have a custom type `MyDatum`
that we want to turn some opaque `Data` into. We can do the following:

```aiken
// some_data : Data
assert my_datum : MyDatum = some_data
```
