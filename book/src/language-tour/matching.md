# Matching

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

Pattern matching on a `Bool` value is discouraged and `if else`
expressions should be use instead.

```aiken
if some_bool {
  "It's true!"
else {
  "It's not true."
}
```

Aiken's `when *expr* is` is an expression, meaning it returns a value and can be used
anywhere we would use a value. For example, we can name the value of a case
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

description  // => "It's true!"
```

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
contained values can also be matched. This gives `case` the ability to
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
case xs {
  [[_, ..] as inner_list] -> inner_list
  other -> []
}
```

## Checking equality and ordering in patterns

The `if` keyword can be used to add a guard expression to a case clause. Both
the patterns have to match and the guard has to evaluate to `True` for the
clause to match. The guard expression can check for equality or ordering for
`Int`.

```aiken
case xs {
  [a, b, c] if a == b && b != c -> "ok"
  _other -> "ko"
}
```

## Alternative clause patterns

Alternative patterns can be given for a case clause using the `|` operator. If
any of the patterns match then the clause matches.

Here the first clause will match if the variable `number` holds 2, 4, 6 or 8.

```aiken
case number {
  2 | 4 | 6 | 8 -> "This is an even number"
  1 | 3 | 5 | 7 -> "This is an odd number"
  _ -> "I'm not sure"
}
```

If the patterns declare variables then the same variables must be declared in
all patterns, and the variables must have the same type in all the patterns.

```aiken
case list {
  [1, x] | x -> x // Error! Int != List(Int)
  _ -> 0
}
```
