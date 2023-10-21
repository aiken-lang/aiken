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
    - [x] `aiken::check::unknown::variable`
    - [x] `aiken::check::unknown::type`
    - [x] `aiken::check::unknown::type_constructor`
    - [x] `aiken::check::unknown::module`
