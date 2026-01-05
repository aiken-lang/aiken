# Aiken WASM

A WASM-friendly API for Aiken.

## Pre-requisite

### wasm-pack

```console
cargo install wasm-pack
```

### wasm32 rustool target

```console
rustup target add wasm32-unknown-unknown
```

### WebAssembly/binaryen

For optimized release builds: see [WebAssembly/binaryen](https://github.com/WebAssembly/binaryen).

## Compiling all

```console
make
```

### Browser only

```console
make browser
```

This produces JavaScript & WASM files under `./aiken-wasm-browser`

### Vanilla JS only (no modules)

```console
make vanilla
```

This produces JavaScript & WASM files under `./aiken-wasm-vanilla`

## Example

```html
<!doctype html>
<html>
<body>
  <script src="aiken-wasm-vanilla/aiken_wasm.js"></script>
  <script>
    const { Project, Tracing, ErrorKind, parse } = wasm_bindgen;
    async function main() {
        await wasm_bindgen();

	    const project = new Project();

        try {
          const module = parse(`
            use aiken/builtin.{append_bytearray, blake2b_256, decode_utf8}

            pub fn hello_world(msg: ByteArray) -> String {
              msg
                |> blake2b_256
                |> encode_base16([], 0, _)
                |> append_bytearray("Hello, ", _)
                |> decode_utf8
            }
          `);

          project.add(module, Tracing.silent());
        } catch(e) {
          const start = e.location.start();
          const end = e.location.end();

          const kind = {
            [ErrorKind.ParseError]: "Parse error",
            [ErrorKind.TypeCheckError]: "Type-check error",
          }[e.kind]

          console.log(`${kind} at [${start}, ${end}]: ${e.message}\n${e.help}`);
        }
    }

    main();
  </script>
</body>
</html>
```

## Documentation

```console
make docs
make preview-docs
```

And then, visit http://localhost:3000
