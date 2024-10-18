use aiken/collection/list

pub fn quicksort(xs: List<a>, compare: fn(a, a) -> Ordering) -> List<a> {
  when xs is {
    [] -> []
    [head, ..tail] -> {
      let before =
        tail
          |> list.filter(fn(x) { compare(x, head) == Less })
          |> quicksort(compare)
      let after =
        tail
          |> list.filter(fn(x) { compare(x, head) != Less })
          |> quicksort(compare)
      list.concat(before, [head, ..after])
    }
  }
}
