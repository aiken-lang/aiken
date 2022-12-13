# Primitive Types

Aiken has 5 primitive types that are built in the language and can be typed as literals: booleans, integers, strings, byte arrays and data. The language also includes 2 base building blocks for associating types together: lists and tuples.

Worry not, we'll see later in this manual how to create your own custom types.

> **Note**
>
> Inline comments are denoted via `//`. We'll use them to illustrate the value
> of some expressions in examples given all across this guide.

## Bool

A `Bool` is a boolean value that can be either `True` or `False`.

Aiken defines a handful of operators that work with booleans. No doubts that they'll look
quite familiar.

| Operator          | Description                       |
| ---               | ---                               |
| `&&`              | Logical conjunction (a.k.a 'AND') |
| <code>\|\|</code> | Logical disjunction (a.k.a. 'OR') |
| `==`              | Equality                          |
| `!`               | Logical negatation (a.k.a 'NOT')  |


## Int

Aiken's only number type is an arbitrary sized integer. This means there is no underflow or overflow.

```aiken
42
14
1337
```

Literals can also be written with `_` as separators to enhance readability:

```
1_000_000
```

Aiken also supports writing integer literals in other bases than decimals. Binary, octal, and hexadecimal integers begin with `0b`, `0o`, and `0x` respectively.

```aiken
0b00001111 == 15
0o17 == 15
0xF == 15
```

Aiken has several binary arithmetic operators that work with integers.

| Operator | Description                 |
| ---      | ---                         |
| `+`      | Arithmetic sum              |
| `-`      | Arithmetic difference       |
| `/`      | Whole division              |
| `*`      | Arithmetic multiplication   |
| `%`      | Remainder by whole division |

Integers are also of course comparable, so they work with a variety of binary logical operators too:

| Operator | Description                 |
| ---      | ---                         |
| `==`     | Equality                    |
| `>`      | Greater than                |
| `<`      | Smaller than                |
| `>=`     | Greater or equal            |
| `<=`     | Smaller or equal            |

## String

In Aiken Strings can be written as text surrounded by double quotes.

```aiken
"Hello, Aiken!"
```

They can span multiple lines.

```aiken
"Hello
Aiken!"
```

Under the hood text strings are [UTF-8](https://en.wikipedia.org/wiki/UTF-8) encoded binaries
and can contain any valid unicode.

```aiken
"🌘 アルバイト Aiken 🌒"
```

## ByteArray

A _ByteArray_ is exactly what it seems, an array of bytes.

Aiken supports byte arrays literals, written as lists of integers ranging from 0 to 255 (a.k.a _bytes_):

```aiken
#[10, 255]
#[1, 256] // results in a parse error because 256 is bigger than 1 byte
```

It's important to mention that variables and patterns are not supported in
byte array literals due to how Untyped Plutus Core works behind the scene.

```aiken
let x = 10
#[x, 243] // not allowed
```

However, syntax rules for literal integers also apply to byte arrays. Thus, the following is a perfectly valid syntax:

```aiken
#[0xFF, 0x42]
```

## Data

A _Data_ is an opaque compound type that can represent any possible user-defined type in Aiken. We'll see later how Aiken lets you compose your own types from the primitives we just presented. In the meantime, think of _Data_ as a kind of wildcard that can possibly represent _any_ value.

This is useful when you need to use values from different types in an homogeneous structure. Any user-defined type can be cast to a _Data_, and you can try converting from a _Data_ to any custom type in a safe manner.

Besides, several language builtins only work with _Data_ as a way to deal with polymorphism.

We'll see more about _Data_ when we cover custom types.

## Tuples

Aiken has tuples which can be useful for grouping values. Each element in a tuple can have a different type.

```aiken
(10, "hello") // Type is (Int, String)
(1, 4, [0]) // Type is (Int, Int, List(Int))
```

Long tuples (i.e. more than 3 elements) are usually discouraged. Indeed, tuples are anonymous constructors, and while they are quick and easy to use, they often impede readability. When types become more complex, one should use records instead (as we'll see later).

Elements of a tuple can be accessed using the dot, followed by the index of the element (0-based indexing). So for example:

```aiken
let point = (14, 42)
let x = point.0
let y = point.1
(y, x) // (42, 14)
```

## List

Lists are ordered collections of values. They're one of the most common data structures in Aiken.

Unlike tuples, all the elements of a List must be of the same type. Attempting to make a list using multiple
different types will result in a type error.

```aiken
[1, 2, 3, 4]  // List(Int)

["text", 3, 4]  // Type error!
```

Inserting at the front of a list is very fast, and is the preferred way to add new values.

```aiken
[1, ..[2, 3]] // [1, 2, 3]
```

Note that all data structures in Aiken are immutable so prepending to a list does not change the original list. Instead it efficiently creates a new list with the new additional element.

```
let x = [2, 3]
let y = [1, ..x]

x // [2, 3]
y // [1, 2, 3]
```
