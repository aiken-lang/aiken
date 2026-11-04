---
status: accepted
---
# Aiken is a UAL producer only; `aiken build` never depends on Lean

Formal assurance is delivered by emitting UAL documents (blueprint in the compiled-interface dialect, `assurance.json`, per-property checking contexts) that PlutusCoreBlaster consumes. Aiken never elaborates Lean, never runs Z3, and never writes evidence. A separate command wraps the consumer as a subprocess and fails CI on falsified or inconclusive claims; the environment manifest it needs is an input, and a `--no-environment` dry run validates annotations on machines without Lean.

## Considered options
- Generate Lean files from Aiken and call Blaster directly (the previous integration). Rejected: it duplicates the consumer's job, and every Blaster change becomes an Aiken release.
- Embed Lean/Z3 toolchain management in Aiken. Rejected: pins Aiken's release cycle to theirs.

## Consequences
- Blaster/UAL changes are absorbed by re-pinning the UAL document version and re-running the conformance fixtures; the plan must budget for weekly drift.
- A bundle produced in dry-run mode is explicitly unrunnable and must never be reported as verified.
