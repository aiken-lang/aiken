//@ PREDICATE
def larger (old proposed : Int) : Prop := old < proposed
def pick {α : Type} (x : α) : α := x
def increasing (a b c : Int) : Prop := larger a b ∧ larger b c
@//

pub fn identity(value: Int) -> Int {
  value
}
