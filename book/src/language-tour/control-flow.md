# Control flow

## Blocks

Every block in Aiken is an expression. All expressions in the block are
executed, and the result of the last expression is returned.

```aiken
let value: Bool = {
    "Hello"
    42 + 12
    False
} // False
```

Expression blocks can be used instead of parenthesis to change the precedence of operations.

```aiken
let celsius = { fahrenheit - 32 } * 5 / 9
```

## Matching

The `when *expr* is` expression is the most common kind of flow control in Aiken code. It
allows us to say "if the data has this shape then do that", which we call
_pattern matching_.

Here we match on an `Int` and return a specific string for the values 0, 1,
and 2. The final pattern `n` matches any other value that did not match any of
the previous patterns.

```aiken
when some_number is {
  0 -> "Zero"
  1 -> "One"
  2 -> "Two"
  n -> "Some other number" // This matches anything
}
```

Aiken's `when *expr* is` is an expression, meaning it returns a value and can be used
anywhere we would use a value. For example, we can name the value of a when
expression with a `let` binding.

```aiken
type Answer {
  Yes
  No
}

let answer = Yes

let description =
  when answer is {
    Yes -> "It's yes!"
    No -> "It's not yes."
  }

description  // => "It's yes!"
```

## If-Else

Pattern matching on a `Bool` value is discouraged and `if / else`
expressions should be use instead.

```aiken
let some_bool = True

if some_bool {
  "It's true!"
} else {
  "It's not true."
}
```

Note that, while it may look like an imperative instruction: if this then do
that or else do that, it is in fact one single expression. This means, in
particular, that the return types of both branches have to match.

## Destructuring

A `when *expr* is` expression can be used to destructure values that
contain other values, such as tuples and lists.

```aiken
when xs is {
  [] -> "This list is empty"
  [a] -> "This list has 1 element"
  [a, b] -> "This list has 2 elements"
  _other -> "This list has more than 2 elements"
}
```

It's not just the top level data structure that can be pattern matched,
contained values can also be matched. This gives `when` the ability to
concisely express flow control that might be verbose without pattern matching.

```aiken
when xs is {
  [[]] -> "The only element is an empty list"
  [[], ..] -> "The 1st element is an empty list"
  [[4], ..] -> "The 1st element is a list of the number 4"
  other -> "Something else"
}
```

Pattern matching also works in `let` bindings, though patterns that do not
match all instances of that type may result in a runtime error.

```aiken
let [a] = [1]    // a is 1
let [b] = [1, 2] // Runtime error! The pattern has 1 element but the value has 2
```

## Assigning names to sub-patterns

Sometimes when pattern matching we want to assign a name to a value while
specifying its shape at the same time. We can do this using the `as` keyword.

```aiken
when xs is {
  [[_, ..] as inner_list] -> inner_list
  _other -> []
}
```

## Checking equality and ordering in patterns

The `if` keyword can be used to add a guard expression to a when clause. Both
the patterns have to match and the guard has to evaluate to `True` for the
clause to match. The guard expression can check for equality or ordering for
`Int`.

```aiken
when xs is {
  [a, b, c] if a == b && b != c -> "ok"
  _other -> "ko"
}
```

## Alternative clause patterns

Alternative patterns can be given for a when clause using the `|` operator. If
any of the patterns match then the clause matches.

Here the first clause will match if the variable `number` holds 2, 4, 6 or 8.

```aiken
when number is {
  2 | 4 | 6 | 8 -> "This is an even number"
  1 | 3 | 5 | 7 -> "This is an odd number"
  _ -> "I'm not sure"
}
```

If the patterns declare variables then the same variables must be declared in
all patterns, and the variables must have the same type in all the patterns.

```aiken
when list is {
  [1, x] | x -> x // Error! Int != List(Int)
  _ -> 0
}
```

## Todo

Aiken's `todo` keyword is used to indicate that some code is not yet finished.

It can be useful when designing a module, type checking functions and types but
leaving the implementation of the functions until later.

```aiken
fn favourite_number() -> Int {
  // The type annotations says this returns an Int, but we don't need
  // to implement it yet.
  todo
}

fn idk() {
  favourite_number() * 2
}
```

When this code is built Aiken will type check and compile the code to ensure it
is valid, and the `todo` will be replaced with code that crashes the program if
that function is run.

A message can be given as a form of documentation. The message will be traced
when the `todo` code is run.

```aiken
fn not_sure_yet() -> Int {
  todo("Believe in the you that believes in yourself!")
}
```

When the compiler finds a `todo` it will print a warning, which can be useful
to avoid accidentally forgetting to remove a `todo`.

The warning also includes the expected type of the expression that needs to
replace the `todo`. This can be a useful way of asking the compiler what type
is needed if you are ever unsure.

```aiken
fn foo() {
  my_complicated_function(
    // What type does this function take again...?
    todo
  )
}
```
