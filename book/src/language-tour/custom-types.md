# Custom types


### Enum Types

Instantiation

```gleam
type Color {
    Red
    Blue
    Green
}

```

Enum types can be thought of as the unit of product types.

Usage

```gleam
fn myFavouriteColor() -> Color {
    Red
}
```

### Product Types

```gleam
type MyValue {
    Name(String)
    Age(Int)
}
```

```gleam
fn eitherNameOrAge(b: Bool) -> MyValue {
    if b {
        Name("James")
    } else {
        Age(20)
    }
}
```

This example is a bit nonsensical...