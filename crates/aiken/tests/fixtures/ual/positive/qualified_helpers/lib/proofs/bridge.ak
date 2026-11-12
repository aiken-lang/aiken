// Annotation-free: its `use` edge still carries the helper fragment.
use proofs/helpers

pub fn same(value: Int) -> Int {
  helpers.identity(value)
}
