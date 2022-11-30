# Functions

## Named functions

Named functions in Aiken are defined using the `pub fn` keywords.

```aiken
pub fn add(x: Int, y: Int) -> Int {
  x + y
}

pub fn multiply(x: Int, y: Int) -> Int {
  x * y
}
```

Functions in Aiken are first class values and so can be assigned to variables,
passed to functions, or anything else you might do with any other data type.

```aiken
/// This function takes a function as an argument
pub fn twice(f: fn(t) -> t, x: t) -> t {
  f(f(x))
}

pub fn add_one(x: Int) -> Int {
  x + 1
}

pub fn add_two(x: Int) -> Int {
  twice(add_one, x)
}
```

## Pipe Operator

Aiken provides syntax for passing the result of one function to the arguments of another function, the pipe operator (`|>`). This is similar in functionality to the same operator in Elixir or F#.

The pipe operator allows you to chain function calls without using a lot of parenthesis and nesting.
For a simple example, consider the following implementation of an imaginary `string.reverse` in Aiken:

```aiken
string_builder.to_string(string_builder.reverse(string_builder.from_string(string)))
```

This can be expressed more naturally using the pipe operator, eliminating the need to track parenthesis closure.

```aiken
string
|> string_builder.from_string
|> string_builder.reverse
|> string_builder.to_string
```

Each line of this expression applies the function to the result of the previous line. This works easily because each of these functions take only one argument. Syntax is available to substitute specific arguments of functions that take more than one argument; for more, look below in the section "Function capturing".

## Type annotations

Function arguments are normally annotated with their type, and the
compiler will check these annotations and ensure they are correct.

```aiken
fn identity(x: some_type) -> some_type {
  x
}

fn inferred_identity(x) {
  x
}
```

The Aiken compiler can infer all the types of Aiken code without annotations
and both annotated and unannotated code is equally safe. It's considered a
best practice to always write type annotations for your functions as they
provide useful documentation, and they encourage thinking about types as code
is being written.

## Generic functions

At times you may wish to write functions that are generic over multiple types.
For example, consider a function that consumes any value and returns a list
containing two of the value that was passed in. This can be expressed in Aiken
like this:

```aiken
fn list_of_two(my_value: a) -> List(a) {
  [my_value, my_value]
}
```

Here the type variable `a` is used to represent any possible type.

You can use any number of different type variables in the same function. This
function declares type variables `a` and `b`.

```aiken
fn multi_result(x: a, y: b, condition: Bool) -> Result(a, b) {
  case condition {
    True -> Ok(x)
    False -> Error(y)
  }
}
```

Type variables can be named anything, but the names must be lower case and may
contain underscores. Like other type annotations, they are completely optional,
but may aid in understanding the code.

## Labeled arguments

When functions take several arguments it can be difficult for the user to
remember what the arguments are, and what order they are expected in.

To help with this Aiken supports _labeled arguments_, where function
arguments are given an external label in addition to their internal name.

Take this function that replaces sections of a string:

```aiken
pub fn replace(string: String, pattern: String, replacement: String) {
  // ...
}
```

It can be given labels like so.

```aiken
pub fn replace(
  in string: String,
  each pattern: String,
  with replacement: String,
) {
  // The variables `string`, `pattern`, and `replacement` are in scope here
}
```

These labels can then be used when calling the function.

```aiken
replace(in: "A,B,C", each: ",", with: " ")

// Labeled arguments can be given in any order
replace(each: ",", with: " ", in: "A,B,C")

// Arguments can still be given in a positional fashion
replace("A,B,C", ",", " ")
```

The use of argument labels can allow a function to be called in an expressive,
sentence-like manner, while still providing a function body that is readable
and clear in intent.

## Anonymous functions

Anonymous functions can be defined with a similar syntax.

```aiken
pub fn run() {
  let add = fn(x, y) { x + y }

  add(1, 2)
}
```

## Function capturing

There is a shorthand syntax for creating anonymous functions that take one
argument and call another function. The `_` is used to indicate where the
argument should be passed.

```aiken
pub fn add(x, y) {
  x + y
}

pub fn run() {
  let add_one = add(1, _)

  add_one(2)
}
```

The function capture syntax is often used with the pipe operator to create
a series of transformations on some data.

```aiken
pub fn add(x: Int , y: Int ) -> Int {
  x + y
}

pub fn run() {
  // This is the same as add(add(add(1, 3), 6), 9)
  1
  |> add(_, 3)
  |> add(_, 6)
  |> add(_, 9)
}
```

In fact, this usage is so common that there is a special shorthand for it.

```aiken
pub fn run() {
  // This is the same as the example above
  1
  |> add(3)
  |> add(6)
  |> add(9)
}
```

The pipe operator will first check to see if the left hand value could be used
as the first argument to the call, e.g. `a |> b(1, 2)` would become `b(a, 1, 2)`.

If not it falls back to calling the result of the right hand side as a function
, e.g. `b(1, 2)(a)`.

## Documentation

You may add user facing documentation in front of function definitions with a
documentation comment `///` per line. Markdown is supported and this text
will be included with the module's entry in generated HTML documentation.

```aiken
/// Does nothing, returns `Nil`.
///
fn returns_nil(a) -> Nil {
  Nil
}
```
