# Int

Aiken's only number type is an arbitrary size integer.

This means there is no underflow or overflow.

Binary, octal, and hexadecimal integers begin with 0b, 0o, and 0x respectively.

```aiken
1
2
-3
4001
0b00001111
0o17
0xF
```

Aiken has several operators that work with Int.

```aiken
// A convenient helper function to get the number 7.
1 + 1 // => 2
5 - 1 // => 4
5 / 2 // => 2
3 * 3 // => 9
5 % 2 // => 1

2 > 1  // => True
2 < 1  // => False
2 >= 1 // => True
2 <= 1 // => False
```

Underscores can be added to Ints for clarity.

```
1_000_000
```
