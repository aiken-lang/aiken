# Custom types

Aiken's custom types are named collections of keys and/or values. They are
similar to objects in object oriented languages, though they don't have
methods.

Custom types are defined with the `type` keyword.

```aiken
type Datum {
  Datum { signer: ByteArray, count: Int }
}
```

Here we have defined a custom type called `Datum`. Its constructor is called
`Datum` and it has two fields: A `signer` field which is a `ByteArray`, and a
`count` field which is an `Int`.

Once defined the custom type can be used in functions:

```aiken
fn datums() {
  // Fields can be given in any order
  let datum1 = Datum { signer: #[0xAA, 0xBB], count: 2001 }
  let datum2 = Datum { count: 1805, name: #[0xAA, 0xCC] }

  [cat1, cat2]
}
```

## Multiple constructors

Custom types in Aiken can be defined with multiple constructors, making them a
way of modeling data that can be one of a few different variants.

We've already seen a custom type with multiple constructors in the Language
Tour - [`Bool`](./primitive-types.md#bool).

Aiken's built-in `Bool` type is defined like this:

```aiken
/// A Bool is a value that is either `True` or `False`
type Bool {
  True
  False
}
```

It's a simple custom type with constructors that takes no arguments at all!
Use it to answer yes/no questions and to indicate whether something is `True`
or `False`.

The records created by different constructors for a custom type can contain
different values. For example a `User` custom type could have a `LoggedIn`
constructor that creates records with a name, and a `Guest` constructor which
creates records without any contained values.

```aiken
type User {
  LoggedIn { count: Int }  // A logged in user
  Guest                    // A guest user with no details
}
```

```aiken
let user1 = LoggedIn { count: 4 }
let user2 = LoggedIn { count: 2 }
let visitor = Guest
```

### Option

We define `Option` as a generic type like so:

```
type Option<a> {
    None
    Some(a)
}
```

Then, functions which fail may safely return an optional value.

```
fn get_head(a: List<a>) -> Option<a {
    when a is {
        [a, .._] -> Some(a)
        [] -> None
    }
}
```

The `Option` type is readily available in Aiken; it is part of the default types and values available by default. Don't hesitate to use it!


## Destructuring

When given a custom type record we can pattern match on it to determine which
record constructor matches, and to assign names to any contained values.

```aiken
fn get_name(user) {
  when user is {
    LoggedIn { count } -> count
    Guest -> "Guest user"
  }
}
```

Custom types can also be destructured with a `let` binding.

```aiken
type Score {
  Points(Int)
}

let score = Points(50)
let Points(p) = score // This brings a let-binding `p` in scope.

p // 50
```

During destructuring you may also use discards (`_`) or spreads (`..`).

```aiken
type Dog {
  Dog { name: ByteArray, cuteness: Int, age: Int }
}

let dog = Dog { name: #[67, 97, 115, 104, 101, 119], cuteness: 9001, age: 3 }
```

You will need to specify all args for a pattern match, or alternatively use the
spread operator.

```aiken
// All fields present
let Dog { name: name, cuteness: _, age: _ } = dog
builtin.decode_utf8(name) // "Cashew"

// Other fields ignored by spreading.
// Field punning is supported. Hence `age` is a shorthand for `age: age`.
let Dog { age, .. } = dog
age // 3
```

## Named accessors

If a custom type has only one variant and named fields they can be accessed
using `.field_name`.

For example using the `Dog` type defined earlier.

```aiken
let dog = Dog { name: #[82, 105, 110], cuteness: 2001 }
dog.cuteness // This returns 2001
```

This is actually so common that Aiken has some syntactic sugar for defining these
kinds of single-constructor types: one can omit the constructor:

```aiken
type Dog {
  name: ByteArray,
  cuteness: Int,
  age: Int,
}
```

## Generics

Custom types can be be parameterised with other types, making their contents
variable.

For example, this `Box` type is a simple record that holds a single value.

```aiken
type Box<inner_type> {
  Box(inner: inner_type)
}
```

The type of the field `inner` is `inner_type`, which is a parameter of the `Box`
type. If it holds an int the box's type is `Box<Int>`, if it holds a string the
box's type is `Box<String>`.

```aiken
fn foo() {
  let a = Box(420) // type is Box<Int>
  let b = Box("That's my ninja way!") // type is Box<String>
}
```

## Record updates

Aiken provides a dedicated syntax for updating some of the fields of a custom
type record.

```aiken
type Person {
  name: ByteArray,
  shoe_size: Int,
  age: Int,
  is_happy: Bool,
}

fn have_birthday(person) {
  // It's this person's birthday, so increment their age and
  // make them happy
  Person { ..person, age: person.age + 1, is_happy: True }
}
```

The update syntax created a new record with the values of the initial record.
It replaces the given binding with their new values.

## Type aliases

A type alias lets you create a name which is identical to
another type, without any additional information.

```aiken
type MyNumber = Integer
```

They are most useful for simplifying type signatures.

```aiken
type Person = (String, Integer)

fn create_person(name: String, age: Integer) -> Person {
    #(name, age)
}
```

## Data

At runtime custom types become an opaque Plutus' Data. In Aiken's type system
`Data` matches with any user-defined type (but with none of the primitive
types).

Thus, it's also possible to cast any `Data` to a [custom
type](./custom-types.md), and vice versa.

```aiken
fn to_datum(datum: Data) -> Datum {
    let d: Datum = datum
    d
}
```

Note that this conversion will fail if the given `Data` isn't actually a valid
representation of the target type. The primary use-case here is for
instantiating script contexts, datums and redeemers provided to scripts in an
opaque fashion.

It's also useful for interacting with builtins that operate on raw `Data`. In
this case, the conversation happens implicitly. Simply expect any function that
accept `Data` to automatically work on any custom type.

```aiken
type Datum {
  count: Int,
}

let datum = Datum { count: 1 }

// fn(Data) -> ByteArray
builtin.serialize_data(datum) // some bytearray
```
