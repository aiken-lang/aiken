# Changelog

## [next] - 2022-MM-DD

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
