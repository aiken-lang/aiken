# Constants

Aikens's module constants provide a way to use a
certain fixed value in multiple places in a Aiken project.

```aiken
pub const start_year = 2101
pub const end_year = 2111

pub fn is_before(year: Int) -> Bool {
  year < start_year
}

pub fn is_during(year: Int) -> Bool {
  start_year <= year && year <= end_year
}
```

Like all values in Aiken constants are immutable. They cannot be used as global mutable state.

When a constant is referenced the value is inlined by the compiler, so they can be used in case expression guards.

```aiken
pub const start_year = 2101
pub const end_year = 2111

pub describe(year: Int) -> String {
  when year is {
    year if year < start_year -> "Before"
    year if year > end_year -> "After"
    - -> "During"
  }
}
```

## Type annotations

Constants can also be given type annotations.

```aiken
pub const name: String = "Aiken"
pub const size: Int = 100
```

These annotations serve as documentation or can be used to provide a more specific type than the compiler would otherwise infer.
