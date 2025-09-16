# Copilot Instructions for Aiken

## ⚠️ MANDATORY: Always Use MCP Servers

**CRITICAL RULE**: AI agents working with this codebase MUST use Serena and Cipher MCP servers for all code analysis and knowledge management tasks. Do NOT attempt manual file reading or navigation without these tools.

## Architecture Overview

Aiken is a smart contract language for Cardano built in Rust with a monorepo structure:

- **`aiken-lang`**: Core language (AST, parser, type checker, code generation to UPLC)
- **`aiken-project`**: Project management, configuration, building, testing
- **`aiken-lsp`**: Language Server Protocol implementation
- **`aiken`**: CLI that orchestrates all functionality
- **`uplc`**: Untyped Plutus Core utilities

## Key Patterns & Conventions

### Module System

- `.ak` files are Aiken smart contracts; `aiken.toml` marks project root
- Modules have `ModuleKind`: `Lib`, `Validator`, `Env`, `Config`
- Validators are entry points for Cardano; libs contain shared functions
- Configuration lives in `env/` modules or `[config.default]` in `aiken.toml`

### Error Handling & Diagnostics

- Custom error types per crate: `ServerError`, `ProjectError`, etc.
- LSP converts internal errors to LSP diagnostics via `process_diagnostic()` in `server.rs`
- Errors include structured codes like `aiken::check::unknown::variable`
- Quickfixes map specific error codes to automated fixes (see `quickfix.rs`)

### Testing Framework

- Three test types: `UnitTest`, `PropertyTest` (with fuzzing), `Benchmark`
- Run via `aiken check` with filters: `-m module_name` or `-m "module.{test_name}"`
- Property tests use `via` keyword for custom fuzzers, support shrinking
- Coverage modes: `relative-to-tests` vs `relative-to-labels`

### LSP Implementation

- Document sync in memory via `edited` HashMap in `server.rs`
- Compile-on-save triggers diagnostics publishing
- Quickfixes grouped by type (especially unused imports as single action)
- Position-based queries use `node_at_position()` with `LineNumbers`

## Essential Workflows

### Development Commands

```bash
# Build project (creates plutus.json blueprint)
cargo run -- build [--watch] [--env ENV_NAME]

# Run tests with property testing
cargo run -- check [-m PATTERN] [--seed N] [--max-success N]

# Benchmark performance
cargo run -- benchmark [-m PATTERN] [--seed N] [--max-size N]

# Start LSP (hidden command for editors)
cargo run -- lsp
```

### Project Structure

```
validators/           # Smart contract entry points (.ak)
lib/                 # Shared library functions (.ak)
env/                 # Environment-specific configurations
aiken.toml           # Project config (dependencies, plutus version)
plutus.json          # Generated blueprint (build output)
```

### Code Generation

- Aiken → AST → Type-checked AST → UPLC → Plutus bytecode
- Use `CodeGenerator` for UPLC compilation
- Tracing levels: `user-defined`, `compiler-generated`, `all`

## Integration Points

- **Cardano**: Via Plutus/UPLC, uses `pallas-*` crates for primitives
- **Package management**: GitHub-based with `aiken.lock`
- **Benchmarking**: Against previous versions, results in markdown tables
- **Editors**: VSCode extension, NeoVim via LSP

## AI Agent Integration via MCP Servers

### 🔒 MANDATORY USAGE RULE

**ALL AI agents must use Serena and Cipher MCP servers when working with this codebase. This is not optional.**

- **Serena**: Required for all code navigation, symbol analysis, and file modifications
- **Cipher**: Required for knowledge storage, retrieval, and session continuity
- **No Exceptions**: Do not use manual file reading tools when MCP servers are available

### Cipher MCP Server (Knowledge & Memory Management)

- **Primary Function**: Long-term knowledge storage and retrieval across sessions
- **Key Tool**: `mcp_cipher_ask_cipher` - Store/retrieve insights with natural language queries
- **Critical Use Cases**:
  - Store Aiken-specific patterns: "Remember that validators use ModuleKind::Validator"
  - Document LSP diagnostic codes and their corresponding quickfixes
  - Save cross-crate dependency insights and module interaction patterns
  - Record test framework specifics (PropertyTest vs UnitTest vs Benchmark)
  - Preserve UPLC compilation flow understanding
- **Best Practices**:
  - Always query Cipher before starting complex tasks to leverage prior knowledge
  - Store new architectural discoveries immediately after learning them
  - Use descriptive queries: "How do error diagnostics flow in Aiken LSP?"
  - Update knowledge when patterns change or evolve

### Serena MCP Server (Intelligent Code Navigation)

- **Primary Function**: AST-aware code exploration and precise editing without reading entire files
- **Special Focus**: Essential for developing Aiken Language Server (`aiken-lsp`) in Rust
- **Essential Workflow**:

  1. **Project Setup**: `mcp_serena_activate_project` and `mcp_serena_check_onboarding_performed`
  2. **Code Discovery**: `mcp_serena_get_symbols_overview` for file structure understanding
  3. **Symbol Navigation**: `mcp_serena_find_symbol` with name paths like "Server/compile" or "/ModuleKind"
  4. **Pattern Search**: `mcp_serena_search_for_pattern` for regex-based code discovery
  5. **Intelligent Editing**: `mcp_serena_replace_symbol_body` for precise modifications

- **Aiken LSP Development Patterns**:

  - **LSP Server Architecture**: Navigate `crates/aiken-lsp/src/server.rs` to understand request/notification handling
  - **Diagnostic Pipeline**: Track error conversion from `aiken-lang` errors to LSP diagnostics
  - **Quickfix Implementation**: Analyze `quickfix.rs` patterns for automated code fixes
  - **Position Mapping**: Understand `LineNumbers` and `node_at_position()` for cursor-based queries
  - **Document Sync**: Study `edited` HashMap patterns for in-memory file management
  - **Cross-Crate Integration**: Map dependencies between `aiken-lsp`, `aiken-lang`, and `aiken-project`

- **Aiken-Specific Usage Patterns**:

  - Navigate crate boundaries: Find symbols across `aiken-lang`, `aiken-project`, `aiken-lsp`
  - Understand AST structures: Use depth parameter to explore nested definitions
  - Track error handling: Find all diagnostic conversion patterns
  - Analyze test patterns: Locate test framework implementations across modules
  - Map LSP request handlers: Find notification/request handling in server.rs

- **Critical Guidelines**:
  - **NEVER** read entire source files - use symbolic tools first
  - Use `include_body=true` only after finding the exact symbol needed
  - Leverage `relative_path` to restrict searches to specific crates
  - Use `substring_matching=true` for fuzzy symbol discovery
  - Always call `mcp_serena_think_about_collected_information` after exploration

### Integrated Agent Workflow

1. **Session Start**: Query Cipher for relevant Aiken knowledge
2. **Code Exploration**: Use Serena's symbolic navigation to understand structure
3. **Knowledge Building**: Store new insights in Cipher for future sessions
4. **Precise Editing**: Use Serena's symbol-aware editing tools
5. **Context Preservation**: Update both Serena project memory and Cipher knowledge

## File References

- Core language entry: `crates/aiken-lang/src/ast.rs`
- LSP server logic: `crates/aiken-lsp/src/server.rs`
- Error patterns: `crates/aiken-project/src/error.rs`
- Test framework: `crates/aiken-lang/src/test_framework.rs`
- CLI commands: `crates/aiken/src/cmd/`
