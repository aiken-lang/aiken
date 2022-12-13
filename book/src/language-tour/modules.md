# Modules

Aiken programs are made up of bundles of functions and types called modules.
Each module has its own namespace and can export types and values to be used
by other modules in the program.

```aiken
// inside module lib/straw_hats/sunny.ak

fn count_down() {
  "3... 2... 1..."
}

fn blast_off() {
  "BOOM!"
}

pub fn set_sail() {
  [
    count_down(),
    blast_off(),
  ]
}
```

Here we can see a module named `straw_hats/sunny`, the name determined by the
filename `lib/straw_hats/sunny.ak`. Typically all the modules for one
project would live within a directory with the name of the project, such as
`straw_hats` in this example.

The `pub` keyword makes this type usable from other modules.

For the functions `count_down` and `blast_off` we have omitted the `pub`
keyword, so these functions are _private_ module functions. They can only be
called by other functions within the same module.

Functions, type-aliases and constants can all be exported from a module using
the `pub` keyword.

## Import

To use functions or types from another module we need to import them using the
`use` keyword.

```aiken
// inside module src/straw_hats/laugh_tale.ak

use straw_hats/sunny

pub fn find_the_one_piece() {
  sunny.set_sail()
}
```

The definition `use straw_hats/sunny` creates a new variable with the name
`sunny` and the value of the `sunny` module.

In the `find_the_one_piece` function we call the imported module's public `set_sail`
function using the `.` operator. If we had attempted to call `count_down` it
would result in a compile time error as this function is private to the
`sunny` module.

## Named import

It is also possible to give a module a custom name
when importing it using the `as` keyword.

```aiken
use unix/dog
use animal/dog as kitty
```

This may be useful to differentiate between multiple modules that would have
the same default name when imported.

## Unqualified import

Values and types can also be imported in an unqualified fashion.

```aken
use animal/dog.{Dog, stroke}

pub fn foo() {
  let puppy = Dog { name: "Zeus" }
  stroke(puppy)
}
```

This may be useful for values that are used frequently in a module, but
generally qualified imports are preferred as it makes it clearer where the
value is defined.

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

## The prelude module

There are two modules that are built into the language, the first is the
`aiken` prelude module. By default its types and values are automatically
imported into every module you write, but you can still chose to import it the
regular way. This may be useful if you have created a type or value with the
same name as an item from the prelude.

```aiken
use aiken

/// This definition locally overrides the `Option` type
/// and the `Some` constructor.
pub type Option {
  Some
}

/// The original `Option` and `Some` can still be used
pub fn go() -> aiken.Option<Int> {
  aiken.Some(1)
}
```

The prelude module contains these types:

- `ByteArray`
- `Bool`
- `Int`
- `List<element>`
- `Nil`
- `Option<value>`
- `String`
- `Data`

And these values:

- `None`
- `Some`
- `False`
- `True`
- `Nil`

## The builtin module

The second module that comes with the language is for exposing useful builtin
functions from Plutus core. Most underlying platform functions are available
here using a snake case name. Much of Aiken's syntax ends up compiling to
combinations of certain bultins but many aren't "exposed" through the syntax
and need to be used directly. The standard library wraps these in a more
Aiken-friendly interface so you'll probably never need to use these directly
unless you're making your own standard library.

```aiken
use aiken/builtin

fn eq(a, b) {
    builtin.equals_integer(a, b)
}

// is implicitly used when doing:

a == b
```

## Documentation

You may add user facing documentation at the head of modules with a module
documentation comment `////` (quadruple slash!) per line. Markdown is supported
and this text block will be included with the module's entry in generated HTML
documentation.
