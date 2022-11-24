# Syntax

Let's start with a little reminder about the syntax. The complete syntax for Untyped Plutus Core comes from the original [Formal Specification of the Plutus Core Language](https://hydra.iohk.io/build/14133599/download/1/plutus-core-specification.pdf).

## Primitive Types

Plutus Core has 7 primitive types (a.k.a. constants): `unit`, `bool`, `integer`, `bytestring`, `string`, `pair` and `list`.
One can construct constants using the `con` keyword, followed by the name of the primitive type and its value.

- Unit is denoted `()`;
- Bool are `True` or `False`;
- Bytestrings are denoted with a leading `#` followed by an hexadecimal sequence;
- Strings are UTF-8 text strings, between double quotes `"` `"`;
- Pair and lists are encapsulated between brackets `[` and `]`.

Note that each constant is named after its type. For pairs and lists -- which are compound types --, the type of their elements is specified between chevrons `<` and `>`.

| Primitive Type | Example                              |
| ---            | ---                                  |
| `unit`         | `con unit ()`                        |
| `bool`         | `con bool True`                      |
| `integer`      | `con integer 42`                     |
| `bytestring`   | `con bytestring #41696b656e`         |
| `string`       | `con string "Aiken"`                 |
| `pair`         | `con pair<bool, integer> [True, 42]` |
| `list`         | `con list<bytestring> [#00, #aa]`    |

## Functions

A function (or simply, lambda) is constructed with the keyword `lam` followed
by a variable name, and a term (i.e. a constant, another function, etc..). One
can apply variables to a function using squared brackets `[ ]`.

For example: `[ (lam x x) (con integer 42) ]`.

This little excerpt constructs a function that takes an argument `x` and returns
it; to which we immediately apply the constant `42`. If we were to evaluate this
program, it would simply output: `42`.

## Builtins

Plutus Core comes with a set of builtins functions which comes in handy to
define certain operations. Incidentally, there's no _operator_ even for basic
arithmetic operations, everything comes as a builtin.

You'll notice also that some builtins are very domain specific and tailored to
operations you'd expect a smart-contract to perform on a blockchain. Hence, new
builtins may be added in the future to address specific use cases that emerge.

Builtins are called with the keyword `builtin` followed by their names. They may
take one, two, three or really any number of arguments. The complete list of builtins
is given [in annexe](./builtins.md).

## Delay & Force

Plutus Core has the notion of type abstractions and type instantiations. That is, like lambdas are functions over term values, abstractions are functions over types. These abstractions allow to represent polymorphic types (such as, a list of elements, or an option type). UPLC has gotten rid of the types, but introduces two new keywords in order to preserve the abstractions in some form.

- `force` can be used on a polymorphic function to instantiate one type-parameter. For example, the branches of a builtin `ifThenElse` can be of any type -- though they have to be the same for both branches. In fact, `ifThenElse` has one type parameter. To be called, it must therefore be forced once: `[ [ [ force (builtin ifThenElse) p ] x ] y ]`

- Similarly, `delay` can be used to defer the evaluation of a certain term; this allows to artificially construct or preserve type abstractions, but also, to introduce a certain level of laziness in parts of the program.

## Data

In addition to primitive types, Plutus Core also has a more generalized `data`
data-type which is meant to represent any possible data-type in a program.

> **TODO**: Give additional detail about how the serialization is done and how
> to construct a Data.
>
> In particular, revisit after [#34](https://github.com/txpipe/aiken/issues/34)
> since the introduction of "list" and "pair" keywords may come in handy.

## Programs

Finally, UPLC programs are wraps in a `program` declaration, which indicates
the version (e.g. `1.0.0`) of Plutus Core that this programs uses. You don't
have to worry about that too much. Aiken supports the latest Plutus version
(`2.0.0`).
