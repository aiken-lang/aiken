pub type LinkedList<a> {
  Empty
  Node(a, LinkedList<a>)
}

pub fn size(t: LinkedList<alg>) -> Int {
  when t is {
    Empty -> 0
    Node(_, tail) -> 1 + size(tail)
  }
}

test foo() {
  let xs = Node(0, Node(1, Node(2, Empty)))
  size(xs) == 3
}
