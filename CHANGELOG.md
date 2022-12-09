# Changelog

## [next] - 2022-MM-DD

### Added

- **aiken-lang**: integrated unit tests

  Aiken now supports writing unit tests directly in source files using the new
  `test` keyword. Tests are functions with no arguments that are implicitly typed
  to `bool`. For example:

  ```gleam
  test foo () {
    1 + 1 == 2
  }
  ```

- **aiken**: new `--skip-tests` flag for the `check` command

### Changed

- **aiken**: `check` now also runs and reports on any `test` found in the project

### Removed

N/A

## [v0.0.26] - 2022-11-23

### Added

- **aiken-lsp**: handle `DidSaveTextDocument` notification
- **aiken-lsp**: convert errors into `lsp_types::Diagnostic`
- **aiken-lang**: doc comment parsing
- **aiken-lang**: code generation for pattern matching expressions
- **aiken-lang**: extended script context
- **aiken-lang**: added Option to builtins
- **aiken-lang**: properly handle record parsing and sugar in patterns

## [v0.0.25] - 2022-11-14

### Added

- **aiken**: new `lsp` command
- **aiken**: new `fmt` command
- **aiken**: `build` command now works and outputs assets
- **aiken**: validate project name on `aiken new`
- **aiken-lang**: formatter for `UntypedExpr`
- **aiken-lang**: uplc code gen
- **aiken-lang**: add `Data` to prelude
- **aiken-lang**: allow `Data` to unify with anything that's not in the prelude
- **aiken-project**: validate if validator function return bool
- **aiken-project**: validate if validator function has minimum number of arguments
- **aiken-lsp**: new crate that contains the aiken language server

### Changed

- **uplc**: `Converter::get_index` now takes the full name to provide better error messages for `Error::FreeUnique`

## [v0.0.24] - 2022-11-04

### Changed

- **uplc**: Sorted remaining structured in the ScriptContext (Value, Wdrl, (Ref) Inputs, Mint, Required signers, Data, Redeemers)

## [v0.0.23] - 2022-11-03

### Changed

- **uplc**: sort inputs for script context fixes an issue in lucid https://github.com/spacebudz/lucid/issues/109

## [v0.0.22] - 2022-10-31

### Added

- **aiken**: Fancy errors using [miette](https://github.com/zkat/miette)
- **aiken**: Typechecking
- **aiken**: Inject `aiken/builtin` module with some functions from `DefaultFunction` in UPLC directly exposed
- **aiken-lang**: add `infer` method to `UntypedModule` which returns a `TypedModule`
- **uplc**: Expose various Pallas primitives from UPLC to make constructing
            UPLC types possible for consumers

### Changed

- **aiken**: Project structure is now a bit different. See [examples/sample](https://github.com/txpipe/aiken/tree/main/examples/sample) for more

## [v0.0.21] - 2022-10-23

### Added

- **flat-rs**: New errors for debugging flat decoding issues

### Changed

- **uplc**: Fixed overflow issue by changing `i64` to `i128` in `BigInt::Int` instances
- **uplc**: Added `apply_params_to_script` function (applies params to script and serializes the new script).

## [v0.0.20] - 2022-10-17

### Added

- **aiken**: `Project` module which is responsible loading modules and running the compilation steps
- **aiken**: `UplcCommand::Flat` flip the cbor_hex if condition so that the correct logic runs when using the flag
- **uplc**: use i128 for `Constant::Integer`
- **flat-rs**: add support for i128 encode and decode
- **flat-rs**: add i128 zigzag function
