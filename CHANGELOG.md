# Changelog

## [next] - 2022-MM-DD

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
