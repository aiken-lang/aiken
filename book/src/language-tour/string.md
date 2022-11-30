# String

In Aiken Strings can be written as text surrounded by double quotes.

```aiken
"Hello, Aiken!"
```

They can span multiple lines.

```aiken
"Hello
Aiken!"
```

Under the hood Strings are [UTF-8](https://en.wikipedia.org/wiki/UTF-8) encoded binaries
and can contain any valid unicode.

```aiken
"🌘 アルバイト Aiken 🌒"
```
