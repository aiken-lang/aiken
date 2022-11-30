# Blocks

Every block in Aiken is an expression. All expressions in the
block are executed, and the result of the last expression is returned.

```aiken
let value: Bool = {
    "Hello"
    42 + 12
    False
} // => False
```

Expression blocks can be used instead of parenthesis to change the precedence of operations.

```
let celsius = { fahrenheit - 32 } * 5 / 9
```
