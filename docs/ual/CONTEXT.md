# Aiken × UAL (formal assurance)

The part of Aiken that turns annotated `.ak` source into UAL assurance documents that an external checker (PlutusCoreBlaster) can verify against the compiled UPLC. Aiken produces claims; it never produces evidence.

## Language

### Roles

**Producer**:
A compiler that emits a blueprint and an assurance document from annotated source. Aiken is a producer.
_Avoid_: generator, frontend, emitter

**Consumer**:
The tool that imports the compiled UPLC and checks each property. PlutusCoreBlaster is the consumer; Aiken is never one.
_Avoid_: verifier, checker, backend, Blaster (when the role is meant)

### Claims

**Target**:
A compiled UPLC program, identified by stable id and content digest, that properties are stated about. Corresponds to UAL `ONCHAIN`.
_Avoid_: subject, entry point

**Script target**:
A target that is a ledger entry point and lives in the blueprint.

**Function target**:
A target that is a separately compiled library function and lives only in the assurance document's function registry.
_Avoid_: exported function, helper

**Property**:
A named, described Lean proposition about one or more targets. It is a claim, not a proof. Corresponds to UAL `PROPERTY`.
_Avoid_: theorem, test, spec, proof

**Predicate**:
A reusable Lean `def`/`abbrev` with no proof obligation, scoped to the module it appears in. Corresponds to UAL `PREDICATE`.
_Avoid_: helper, lemma

**Fragment**:
The Lean source emitted for one module's predicates, carrying its import edges to other fragments.

### Aiken-side

**Annotation block**:
A region delimited by `//@` and `@//` carrying one UAL annotation, possibly multi-line. Inert to the Aiken grammar; read by the assurance pass.
_Avoid_: attribute, pragma, directive

**Derived signature**:
The ONCHAIN argument/result encodings the producer computes from a target's Aiken type instead of the user writing them.

**Handler**:
One purpose-specific body (`spend`, `mint`, …, `else`) of a validator. Handlers are not targets; the validator is.
_Avoid_: entry point, script (for a handler)

### Documents

**Blueprint**:
The CIP-57 `plutus.json`, in the compiled-interface dialect.

**Assurance document**:
The detached `assurance.json` holding targets, properties, fragments, definitions and (after checking, never at compile time) evidence.
_Avoid_: spec file, formal.json

**Checking context**:
A digest-bound artifact fixing the execution settings (step budget, CEK semantics variant) and applied parameters for one property.
_Avoid_: config, environment (that is a different thing)

**Environment manifest**:
The consumer-side hash manifest of Lean, Z3 and imported `.olean` files that a check must reproduce exactly. Supplied to the producer, not created by it.

**Emitted id**:
The consumer-legal name of a target (`module_path__name`), used in documents and called as the Lean wrapper in propositions.
_Avoid_: target name (for the emitted form), wrapper id

**Consumer workspace**:
The pinned set of sibling checkouts (PlutusCoreBlaster, Lean-blaster, CardanoLedgerApiBlaster) plus Lean and Z3 in which properties are checked.
_Avoid_: Blaster install, Lean project

**Trust base**:
Everything a verdict relies on without proof: Blaster's translation and optimizer, Z3, the Lean CEK model, the ledger model, and the step budget. Stated with every verdict.
_Avoid_: assumptions (UAL uses that word for something else)

**Evidence**:
A checker-produced record of a property's verdict (verified, falsified, inconclusive, not checked). The producer never writes evidence.
_Avoid_: result, proof, pass
