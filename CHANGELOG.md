# Changelog

## v1.0.13-alpha - unreleased

### Fixed

- **aiken-lang**: fail, todo, and trace had issues with sequences and expressions

## v1.0.12-alpha - 2023-07-14

### Added

- **aiken**: added a `blueprint policy` command to compute the policy ID of a minting script
- **uplc**: parsing and pretty printing for PlutusData

### Fixed

- **aiken-lang**: Prevent mutual recursion caused by conflicting function names for generic expect type
- **aiken-lang**: UPLC evaluation of large integers literals (> max u64)
- **aiken-lang**: Parsing of error / todo keywords in when cases
- **aiken-lang**: Parsing of negative integer patterns and constants
- **aiken-lang**: automatically infer unused validator args as `Data`
- **aiken-lang**: test crashing when referencing validators
- **aiken**: mem and cpu values were not showing in white terminals, switched to cyan

### Changed

- **uplc**: make list type and pair type parsing match the plutus core spec
- **aiken-lang**: rename `error` to `fail`
- **aiken-lang**: new failing test syntax `test name() fail {`

### Removed

## v1.0.11-alpha - 2023-06-23

### Added

- **aiken**: enhance `aiken new`
- **aiken-lang**: Binary operator are now treated like first-class citizen in
  expressions. In particular, they can be used as function arguments directly:

  ```
  compare_with(a, >=, b) == compare_with(a, fn(l, r) { l >= r }, b)
  ```

- **aiken-lang**: Make traces produced by expect dependent on the value of the
  tracing flag.

### Fixed

- **aiken-lang**: Explain discards and expect a bit better in the unused var warning
- **aiken-lang**: Fix expect \_ = ... not including the cast from data logic if the type is data and right hand has a type annotation
- **aiken-lang**: Fix for the final clause of a when expecting another clause
  afterwards in nested list cases.
- **aiken-lang**: Fix for all elements were being destructured in tuple clauses
  even if not used.
- **aiken-lang**: Fix for tuple clause not consuming the next case causing
  incomplete contracts. Now tuple clause will always consume the next case
  unless it is the final clause
- **aiken-lang**: Fix for builtins using the incorrect data to type conversion when used as a function param.

## v1.0.10-alpha - 2023-06-13

### Added

- **aiken**: added panic hook to present a link to make a bug report

### Fixed

- **aiken-lang**: fmt for `module.Constr { field: value }`
- **aiken-lang**: Issue where using var pattern in a when was passing the constr
  index instead of the constr
- **aiken-lang**: Issue where expecting on a list had unexpected behaviors based
  on list length.
- **aiken-lang**: Issue where expecting on a list from data was using the wrong
  expect cast for the list tail.

### Removed

N/A

## v1.0.8-alpha - 2023-06-08

### Added

- **aiken-lang**: numbers can now be written as hexadecimal digits (e.g. `0x42`)
- **aiken-lang**: numbers can now be written using numeric underscores as
  separator (e.g. `1_000_000`)

### Fixed

- **aiken-lang**: fixed operator precedences, in particular the pipe operator
  (`|>`) which is now of the lowest precedence.
- **aiken-project**: need to convert to Program<Name> before dumping to uplc
- **aiken-lang**: fmt crashing when comment at end of file with no newline
- **aiken-lang**: Fixed error when using nested boolean checks in when
  conditions
- **aiken-lang**: Had the wrong conversion for constant maps to plutus data.
  Fixed to check for right conversion
- **aiken-lang**: Zero arg functions were grabbing extra dependencies they
  didn't need to
- **aiken-lang**: Rearrange list clauses and fill in gaps now handles nested
  patterns in a uniform way
- **aiken-lang**: Fixed discards in records that were being sorted incorrectly
  leading to type issues

### Removed

N/A

## v1.0.7-alpha - 2023-06-02

### Added

- **aiken-lang**: Add a way to express that tests can return an error

### Fixed

- **uplc**: Fix pair formatting
- **aiken-lang**: forced new line in formatter for assignments
- **aiken-lang**: Incorrect parsing of generic type annotation prefixed with
  module
- **aiken-lang**: Incorrect handling of comments at end of a file when newline
  not present
- **aiken-lang**: Record update in code gen is now flexible enough to support
  fields being passed in any order.
- **aiken-lang**: Record update now produces better uplc code then creating a
  record by the normal instantiation.
- **aiken-lang**: Issue with Constructors being passed as functions to other
  function arguments was fixed.
- **aiken-lang**: show module name when type mismatch names are the same
- **aiken**: make `uplc flat` and `uplc unflat` subcommands more consistent and
  flexible

## v1.0.6-alpha - 2023-05-17

### Fixed

- **aiken-lang**: Fix for cases where identity function is used as a param to a
  function or assigned to a var.
- **aiken-lang**: Fix for free unique caused by code gen function having a
  missing dependency.

## v1.0.5-alpha - 2023-05-14

### Added

- **aiken-lang**: Add Record Module support for records with 0 fields
- **aiken-lang**: Added some optimization tests
- **aiken-lang**: Added an Aiken to uplc conversion tests on validators
- **aiken-lang**: Added some uplc builder functions for builtin creation
- **aiken-lang**: Added optimization where identity functions called on an arg
  are reduced to just the arg.

### Fixed

- **aiken-lang**: Fix for nested constructors where the type had a single
  constructor. The fields exposed were not being added to the Air.

## v1.0.4-alpha - 2023-05-09

### Added

- **aiken-lang**: ChooseUnit builtin uses a more efficient way of handling the
  first arg (unit) by just assigning to lambda

### Fixed

- **aiken-lang**: Negative numbers now show up as a constant instead of 0 - that
  number
- **aiken-lang**: Expect on constructors without field maps no longer panics
- **aiken-lang**: Expect on constructors with discard as assigned field names
  now no longer throws free unique

### Changed

- **aiken-lang**: Refactor how builtins are processed to uplc in code gen

## v1.0.3-alpha - 2023-04-28

### Added

- **aiken-lang**: added optimization to help prevent unnecessary data wraps or
  unwraps
- **aiken-lang**: optimization to strip unnecessary lambdas
- **aiken-lang**: implement Clone for ParseError and tipo::Error for the
  playground
- **aiken-project**: added end to end tests on conversion from aiken lang to
  uplc

### Fixed

- **aiken**: fixed 'new' instructions to properly show project name and folder
- **aiken-lang**: Add name of var to the unused var warning
- **aiken-lang**: fix expect on an empty list
- **aiken-lang**: pattern match on boolean with simple clause bodies
- **aiken-lang**: fix for inline_direct_reduce to be applied to pattern match
  function instead of argument
- **aiken-lang**: code gen function dependencies won't be hoisted to the top,
  instead hoisted at the location depended on.

## v1.0.2-alpha - 2023-04-17

### Fixed

- **aiken-lang**: needed to assert_no_assignment in when and if blocks
- **uplc**: need to return deserialization error in some cases for the machine
  builtins

## v1.0.1-alpha - 2023-04-16

### Fixed

- **aiken-lang**: incorrect scoping for anonymous functions
- **aiken-lang**: duplicate arguments were allowed in anonymous functions

## v1.0.0-alpha - 2023-04-13

### Added

- **aiken**: new command `blueprint convert`

### Changed

- **aiken-project**: tests filtering with `-m` during check now happens in
  `Project::collect_tests`
- **aiken-project**: fixed generation of blueprints for recursive and mutually
  recursive data-types
- **aiken-project**: perform validation of parameters on `blueprint apply`

- **aiken-lang**: block `Data` and `String` from unifying when casting
- **aiken-lang**: remove ability for a type with many variants with matching
  field labels and types to support field access
- **aiken-lang**: various uplc code gen fixes
- **aiken-lang**: update todo warning to include type
- **aiken-lang**: `|>` operator can now be formatted as a single (short) line or
  forced over multiline in a flexible manner
- **aiken-lang**: the compiler now provides better feedback for type holes (i.e.
  `_`) in type annotations
- **aiken-lang**: assignment and clause guard are now always formatted on a new
  line
- **aiken-lang**: unused let-bindings are now fully removed from generated code
  and discarded unused let-binding now raise a warning
- **aiken-lang**: support multi-clause patterns (only as a syntactic sugar)
- **aiken-lang**: fix lexer panic when parsing too large (> u32) tuple-indexes

- **uplc**: Greatly improved the Plutus virtual machine performances for script
  evaluation

## v0.0.29 - 2023-02-23

### Added

- **aiken-project**: new dep rayon for parallel test execution
- **aiken**: new blueprint command
- **aiken-lang**: new syntax for defining validators
- **aiken**: new address command for deriving addresses out of `plutus.json`
- **aiken-lang**: Add missing Plutus builtins to Aiken's lang.
- **aiken**: fancy nix stuff
- **aiken-lsp**: go to definition
- **aiken-lsp**: docs on hover
- **aiken-lsp**: enable compiler a project

### Changed

- **aiken-lang**: `assert` renamed to `expect`
- **aiken-lang**: new syntax for strings and byte array literals
- **aiken-lang**: lots of code gen improvements
- **aiken-lang**: validator checks now happen during infer instead of in project
- **aiken-lang**: fixed unicode parsing
- **aiken-lang**: update default costs models
- **aiken-lang**: Use variable-length threshold for levenshtein distance
- **aiken-project**: Move module name validation outside of type-checking
- **aiken-project**: Add 'plutusVersion' to blueprints

### Removed

- **aiken-project**: remove assets folder in favor of `plutus.json`
- **aiken-lang**: removed some unused constant related data types

## v0.0.28 - 2023-01-06

### Added

N/A

### Changed

- **uplc**: Reward accounts are now correctly turned into script credentials in
  ScriptContext.
- **all**: bump pallas version to `v0.16.0`

### Removed

N/A

## v0.0.27 - 2022-MM-DD

### Added

- **aiken-lang**: integrated unit tests

  Aiken now supports writing unit tests directly in source files using the new
  `test` keyword. Tests are functions with no arguments that are implicitly
  typed to `bool`. For example:

  ```gleam
  test foo () {
    1 + 1 == 2
  }
  ```

- **aiken**: new `--skip-tests` flag for the `check` command

### Changed

- **aiken**: `check` now also runs and reports on any `test` found in the
  project
- **aiken**: fix Plutus V1 `to_plutus_data()` for post-alonzo txout with no
  datum hash

### Removed

N/A

## v0.0.26 - 2022-11-23

### Added

- **aiken-lsp**: handle `DidSaveTextDocument` notification
- **aiken-lsp**: convert errors into `lsp_types::Diagnostic`
- **aiken-lang**: doc comment parsing
- **aiken-lang**: code generation for pattern matching expressions
- **aiken-lang**: extended script context
- **aiken-lang**: added Option to builtins
- **aiken-lang**: properly handle record parsing and sugar in patterns

## v0.0.25 - 2022-11-14

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
- **aiken-project**: validate if validator function has minimum number of
  arguments
- **aiken-lsp**: new crate that contains the aiken language server

### Changed

- **uplc**: `Converter::get_index` now takes the full name to provide better
  error messages for `Error::FreeUnique`

## v0.0.24 - 2022-11-04

### Changed

- **uplc**: Sorted remaining structured in the ScriptContext (Value, Wdrl, (Ref)
  Inputs, Mint, Required signers, Data, Redeemers)

## v0.0.23 - 2022-11-03

### Changed

- **uplc**: sort inputs for script context fixes an issue in lucid
  https://github.com/spacebudz/lucid/issues/109

## v0.0.22 - 2022-10-31

### Added

- **aiken**: Fancy errors using [miette](https://github.com/zkat/miette)
- **aiken**: Typechecking
- **aiken**: Inject `aiken/builtin` module with some functions from
  `DefaultFunction` in UPLC directly exposed
- **aiken-lang**: add `infer` method to `UntypedModule` which returns a
  `TypedModule`
- **uplc**: Expose various Pallas primitives from UPLC to make constructing UPLC
  types possible for consumers

### Changed

- **aiken**: Project structure is now a bit different. See
  [examples/sample](https://github.com/aiken-lang/aiken/tree/main/examples/sample)
  for more

## v0.0.21 - 2022-10-23

### Added

- **flat-rs**: New errors for debugging flat decoding issues

### Changed

- **uplc**: Fixed overflow issue by changing `i64` to `i128` in `BigInt::Int`
  instances
- **uplc**: Added `apply_params_to_script` function (applies params to script
  and serializes the new script).

## v0.0.20 - 2022-10-17

### Added

- **aiken**: `Project` module which is responsible loading modules and running
  the compilation steps
- **aiken**: `UplcCommand::Flat` flip the cbor_hex if condition so that the
  correct logic runs when using the flag
- **uplc**: use i128 for `Constant::Integer`
- **flat-rs**: add support for i128 encode and decode
- **flat-rs**: add i128 zigzag function
