# Int

Ints are plutus integers which are arbitrary size.
So, there is no underflow or overflow. Basic arithmetic can be done for O(1) between ints (+,-,*).

```gleam
// A convenient helper function to get the number 7.
pub fn get7(){
    let x = 3
    let y = 2
    let z = 1
    x*y + z
}
```