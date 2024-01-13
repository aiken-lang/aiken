pub fn generic_wrap(x: a) -> Option<a> {
  Some(x)
}

pub fn generic_in_list(x: a) -> List<a> {
  [x]
}

test multiple_wraps() {
  let a = 1
  let b = ""
  let c =
    [0]
  let d =
    [""]
  let e =
    []
  let f = (1, "")
  let g = ("", 1)
  let h = (1, 2, "")
  let i = None
  let j = Some("")
  let k =
    [(1, "")]
  let l =
    [(2, 3, "")]

  and {
    generic_wrap(a) == Some(a),
    generic_wrap(b) == Some(b),
    generic_wrap(c) == Some(c),
    generic_wrap(d) == Some(d),
    generic_wrap(e) == Some(e),
    generic_wrap(f) == Some(f),
    generic_wrap(g) == Some(g),
    generic_wrap(h) == Some(h),
    generic_wrap(i) == Some(i),
    generic_wrap(j) == Some(j),
    generic_wrap(k) == Some(k),
    generic_wrap(l) == Some(l),
  }
}

test multiple_in_list() {
  let a = 1
  let b = ""
  let c =
    [0]
  let d =
    [""]
  let e =
    []
  let f = (1, "")
  let g = ("", 1)
  let h = (1, 2, "")
  let i = None
  let j = Some("")
  let k =
    [(1, "")]
  let l =
    [(2, 3, "")]

  and {
    generic_in_list(a) == [a],
    generic_in_list(b) == [b],
    generic_in_list(c) == [c],
    generic_in_list(d) == [d],
    generic_in_list(e) == [e],
    generic_in_list(f) == [f],
    generic_in_list(g) == [g],
    generic_in_list(h) == [h],
    generic_in_list(i) == [i],
    generic_in_list(j) == [j],
    generic_in_list(k) == [k],
    generic_in_list(l) == [l],
  }
}

test edge_case_wrap() {
  let a = (1, (1, 1, 1))
  let b = (1, (1, 1), 1)

  and {
    generic_wrap(a) == Some(a),
    generic_wrap(b) == Some(b),
  }
}

test edge_case_in_list() {
  let a = (1, (1, 1, 1))
  let b = (1, (1, 1), 1)

  and {
    generic_in_list(a) == [a],
    generic_in_list(b) == [b],
  }
}
