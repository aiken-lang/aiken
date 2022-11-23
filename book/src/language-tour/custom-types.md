# Custom types


### Enum Types

Instantiation

```
type Color {
    Red
    Blue
    Green
}

```

Enum types can be thought of as the unit of product types.

Usage

```
fn myFavouriteColor() -> Color {
    Red
}
```

### Product Types

```
type MyValue {
    Name(String)
    Age(Int)
}
```

```
fn eitherNameOrAge(b: Bool) -> MyValue {
    if b {
        Name("James")
    } else {
        Age(20)
    }
}
```

This example is a bit nonsensical...