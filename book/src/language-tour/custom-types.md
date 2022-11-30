# Custom types

Aiken's custom types are named collections of keys and/or values. They are
similar to objects in object oriented languages, though they don't have
methods.

Custom types are defined with the `type` keyword.

```aiken
pub type Datum {
  Datum { signer: ByteArray, count: Int }
}
```

Here we have defined a custom type called `Datum`. Its constructor is called
`Datum` and it has two fields: A `signer` field which is a `ByteArray`, and a
`count` field which is an `Int`.

The `pub` keyword makes this type usable from other [modules](./modules.md).

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
Tour - [`Bool`](./bool.md).

Aiken's built-in `Bool` type is defined like this:

```aiken
// A Bool is a value that is either `True` or `False`
pub type Bool {
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
  Guest                      // A guest user with no details
}
```

```aiken
let user1 = LoggedIn { count: 4 }
let user2 = LoggedIn { count: 2 }
let visitor = Guest
```

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
```

```aiken
let score = Points(50)
let Points(p) = score

p // => 50
```

During destructuring you may also use discards (`_`) or spreads (`..`).

```aiken
pub type Dog {
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

// Other fields ignored by spreading. Field punning is supported.
let Dog { age, .. } = dog
age // 3
```

## Named accessors

If a custom type has only one variant and named fields they can be accessed
using `.field_name`.

For example using the `Cat` type defined earlier.

```aiken
let dog = Dog { name: #[82, 105, 110], cuteness: 2001 }
builtin.decode_utf8(dog.name) // This returns "Rin"
dog.cuteness // This returns 2001
```

This is actually so common that Aiken has some sugar for defining these
kinds of types.

```aiken
pub type Dog {
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
pub type Box(inner_type) {
  Box(inner: inner_type)
}
```

The type of the field `inner` is `inner_type`, which is a parameter of the `Box`
type. If it holds an int the box's type is `Box(Int)`, if it holds a string the
box's type is `Box(String)`.

```aiken
pub fn main() {
  let a = Box(420) // type is Box(Int)
  let b = Box("That's my ninja way!") // type is Box(String)
}
```

## Opaque types

At times it may be useful to create a type and make the constructors and
fields private so that users of this type can only use the type through
publically exported functions.

For example we can create a `Counter` type which holds an int which can be
incremented. We don't want the user to alter the int value other than by
incrementing it, so we can make the type opaque to prevent them from being
able to do this.

```aiken
// The type is defined with the opaque keyword
pub opaque type Counter {
  Counter(value: Int)
}

pub fn new() {
  Counter(0)
}

pub fn increment(counter: Counter) {
  Counter(counter.value + 1)
}
```

Because the `Counter` type has been marked as `opaque` it is not possible for
code in other modules to construct or pattern match on counter values or
access the `value` field. Instead other modules have to manipulate the opaque
type using the exported functions from the module, in this case `new` and
`increment`.

## Record updates

Aiken provides a dedicated syntax for updating some of the fields of a custom
type record.

```aiken
pub type Person {
  name: ByteArray,
  shoe_size: Int,
  age: Int,
  is_happy: Bool,
}

pub fn have_birthday(person) {
  // It's this person's birthday, so increment their age and
  // make them happy
  Person { ..person, age: person.age + 1, is_happy: True }
}
```

Update syntax created a new record with the values of the initial record
with the new values added.

## Plutus

At runtime custom types become instances of `PlutusData`.

In Aiken's type system `Data` matches with any user-defined type
except for most of the types included in the prelude module.
