"""Producer rejection expectations shared by drift and optional consumer e2e."""

NEGATIVES = {
    "unknown_scope": "Scope 'missing'",
    "wrong_module": "Scope 'target'",
    "missing_fragment_helper": "Fragment 'helpers' is not available",
    "opaque_parameter": "illegal::opaque_type_in_abi",
    "missing_steps": "@onchain requires a positive 'steps' setting",
    "invalid_semantics": "@onchain semantics must be C or E",
    "id_collision": "both map to emitted id",
    "mixed_execution_settings/steps": "mixes execution settings",
    "mixed_execution_settings/semantics": "mixes execution settings",
    "unterminated_block": "annotation is not closed",
}
