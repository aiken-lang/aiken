// `example` is a Lean keyword: fragment id `example`, Lean namespace `«example»`.
use proofs/bridge

//@ PREDICATE
def positive (x : Int) : Prop := proofs.helpers.larger 0 x
@//

pub fn same(value: Int) -> Int {
  bridge.same(value)
}
