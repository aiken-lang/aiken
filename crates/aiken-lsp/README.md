# Aiken Language Server Protocol (LSP)

The `aiken` command-line comes built-in with a _Language Server_ implementing
(part of) the [Language Server Protocol](https://microsoft.github.io/language-server-protocol/).

## Getting Started

### General

Regardless of your IDE, you'll likely need the following information to configure your language client:

- command: `aiken lsp`
  > **Note**
  > The command is hidden from the command-line help usage

- root pattern: `aiken.toml`
- filetype: `aiken` `(.ak)`

### VsCode

Simply install the [VSCode Aiken extension](https://marketplace.visualstudio.com/items?itemName=TxPipe.aiken) from the official marketplace.

### NeoVim

If you're using NeoVim and using [`nvim-lspconfig`](https://github.com/neovim/nvim-lspconfig) then there's almost
nothing to do as Aiken is directly supported. Simply follow the [official
instructions](https://github.com/neovim/nvim-lspconfig/blob/master/doc/server_configurations.md#aiken)

## Supported Capabilities

- [x] Document syncing and on-the-fly compilation
- [x] Document formatting (akin to `aiken fmt`)
- [x] Go-to definition
- [x] Type annotation on hover
- [x] Code actions providing quickfixes for a variety of errors:

  | error                                            | quickfix                                           |
  | ---                                              | ---                                                |
  | `check::unknown::variable`                       | Add relevant import or use qualified import inline |
  | `check::unknown::type`                           | Add relevant import or use qualified import inline |
  | `check::unknown::type_constructor`               | Add relevant import or use qualified import inline |
  | `check::unknown::module`                         | Add relevant import                                |
  | `check::unused:import::value`                    | Remove redundant imports                           |
  | `check::unused::import::module`                  | Remove redundant imports                           |
  | `check::single_constructor_expect`               | Replace `expect` with `let`                        |
  | `check::syntax::unused_record_fields`            | Remove redundant fields                            |
  | `check::syntax::bytearray_literal_is_hex_string` | Prefix string with `#`                             |
  | `check::unexpected::type_hole`                   | Replace type hole with inferred type               |
  | `check::unused::function`                        | Make private function public                       |
  | `check::unused::constant`                        | Make private constant public                       |
  | `check::unused::type`                            | Make private type public                           |
  | `check::private_leak`                            | Make leaked type public                            |
