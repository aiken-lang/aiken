// TODO: To be written once we have support for tuples
//
// pub fn map2(xs: List(a), ys: List(b), fn (a, b) -> c) -> List c
// pub fn map3(xs: List(a), ys: List(b), zs: List(c), fn (a, b, c) -> d) -> List d
// pub fn zip(xs: List(a), ys: List(b)) -> List(a, b)
// pub fn unzip(xs: List(a, b)) -> (List(a), List(b))
use aiken/builtin

/// Merge two lists together.
///
/// ----- TODO: Add support for writing tests.
///
/// test concat_1() {
///   concat([1,2,3], [4,5,6]) == [1,2,3,4,5,6]
/// }
///
/// test concat_2() {
///   concat([1,2,3], []) == [1,2,3]
/// }
///
/// test concat_3() {
///   concat([], [1,2,3]) == [1,2,3]
/// }
pub fn concat(left: List(a), right: List(a)) -> List(a) {
  foldr(left, fn(x, xs) { [x, ..xs] }, right)
}

/// Construct a list filled with n copies of a value.
///
/// ----- TODO: Add support for writing tests.
///
/// test repeat_1() {
///   repeat(0, 42) == []
/// }
///
/// test repeat_2() {
///   repeat(3, 14) == [14,14,14]
/// }
pub fn repeat(x: a, n: Int) -> List(a) {
  if n <= 0 {
    []
  } else {
    [x, ..repeat(x, n - 1)]
  }
}

/// Construct a list of a integer from a given range.
///
/// ----- TODO: Add support for writing tests.
///
/// test range_1() {
///   range(-1, 1) == [-1, 0, 1]
/// }
pub fn range(from: Int, to: Int) -> List(Int) {
  if from > to {
    []
  } else {
    [from, ..range(from + 1, to)]
  }
}

/// Get the first element of a list
pub fn head(xs: List(a)) -> Option(a) {
  when xs is {
    [] -> None
    _ -> Some(builtin.head_list(xs))
  }
}

/// Get elements of a list after the first one, if any
pub fn tail(xs: List(a)) -> Option(List(a)) {
  when xs is {
    [] -> None
    [_, ..rest] -> Some(rest)
  }
}

/// Get the first `n` elements of a list.
pub fn take(xs: List(a), n: Int) -> List(a) {
  if n <= 0 {
    []
  } else {
    when xs is {
    [] -> []
    [x, ..rest] -> [x, ..take(rest, n - 1)]
  }
  }
}

/// Drop the first `n` elements of a list.
pub fn drop(xs: List(a), n: Int) -> List(a) {
  if n <= 0 {
    xs
  } else {
    when xs is {
    [] -> []
    [_x, ..rest] -> drop(rest, n - 1)
  }
  }
}

/// Get the number of elements in the given list.
///
/// ----- TODO: Add support for writing tests.
///
/// test length_1() {
///     length([1,2,3]) == 3
/// }
///
/// test length_2() {
///     length([]) == 0
/// }
pub fn length(xs: List(a)) -> Int {
  when xs is {
    [] -> 0
    [_, ..rest] -> 1 + length(rest)
  }
}

/// Get the number of elements in the given list.
///
/// ----- TODO: Add support for writing tests.
///
/// test reverse_1() {
///     length([1,2,3]) == [3,2,1]
/// }
///
/// test reverse_2() {
///     length([]) == []
/// }
pub fn reverse(xs: List(a)) -> List(a) {
  foldr(xs, fn(x, rest) { [x, ..rest] }, [])
}

/// Figures out whether a list contain the given element.
///
/// ----- TODO: Add support for writing tests.
///
/// test is_elem_1() {
///     is_elem([1,2,3], 1) == True
/// }
///
/// test is_elem_2() {
///     is_elem([1,2,3], 14) == False
/// }
///
/// test is_elem_3() {
///     is_elem([], 14) == False
/// }
pub fn is_elem(xs: List(a), x: a) -> Bool {
  when xs is {
    [] -> False
    [y, ..rest] ->
      if x == y {
        True
      } else {
        is_elem(rest, x)
      }
  }
}

/// Determine if all elements of the list satisfy the given predicate.
///
/// ----- TODO: Add support for writing tests.
///
/// test all_1() {
///     all([1,2,3], fn(n) { n > 0 }) == True
/// }
///
/// test all_2() {
///     all([1,2,3], fn(n) { n > 42 }) == False
/// }
///
/// test all_3() {
///     all([], fn(n) { n == 42 }) == True
/// }
pub fn all(xs: List(a), predicate: fn(a) -> Bool) -> Bool {
  foldr(xs, fn(x, result) { predicate(x) && result }, True)
}

/// Determine if at least one element of the list satisfies the given predicate.
///
/// ----- TODO: Add support for writing tests.
///
/// test any_1() {
///     any([1,2,3], fn(n) { n > 0 }) == True
/// }
///
/// test any_2() {
///     any([1,2,3], fn(n) { n > 42 }) == False
/// }
///
/// test any_3() {
///     any([], fn(n) { n == 42 }) == False
/// }
pub fn any(xs: List(a), predicate: fn(a) -> Bool) -> Bool {
  foldr(xs, fn(x, result) { predicate(x) || result }, True)
}

/// Apply a function from each element of a list.
///
/// ----- TODO: Add support for writing tests.
///
/// test map_1() {
///   map([1,2,3,4], fn(n) { n + 1 }) == [2,3,4,5]
/// }
///
/// test map_2() {
///   map([], fn(n) { n + 1 }) == []
/// }
pub fn map(xs: List(a), f: fn(a) -> b) -> List(b) {
  when xs is {
    [] -> []
    [x, ..rest] -> [f(x), ..map(rest, f)]
  }
}

/// Reduce a list from left to right.
///
/// ----- TODO: Add support for writing tests.
///
/// test foldl_1() {
///   foldl([1, 2, 3, 4, 5], fn(n, total) { n + total }, 0) == 15
/// }
pub fn foldl(xs: List(a), f: fn(a, b) -> b, zero: b) -> b {
  when xs is {
    [] -> zero
    [x, ..rest] -> foldl(rest, f, f(x, zero))
  }
}

/// Reduce a list from right to left.
///
/// ----- TODO: Add support for writing tests.
///
/// test foldr_1() {
///   foldr([1, 2, 3, 4, 5], fn(n, total) { n + total }, 0) == 15
/// }
pub fn foldr(xs: List(a), f: fn(a, b) -> b, zero: b) -> b {
  when xs is {
    [] -> zero
    [x, ..rest] -> f(x, foldr(rest, f, zero))
  }
}

/// Produce a list from elements that statisfy a predicate.
///
/// ----- TODO: Add support for writing tests.
///
/// test filter_1() {
///   filter([1, 2, 3, 4, 5, 6], fn(x) { builtin.modInteger(x, 2) == 0 }) == [2, 4, 6]
/// }
pub fn filter(xs: List(a), f: fn(a) -> Bool) -> List(a) {
  foldr(
    xs,
    fn(x, ys) {
      if f(x) {
        [x, ..ys]
      } else {
        ys
      }
    },
    [],
  )
}

/// Find a element satisfying the given predicate.
pub fn find(xs: List(a), f: fn(a) -> Bool) -> Option(a) {
  when xs is {
    [] -> None
    [x, ..rest] ->
      if f(x) {
        Some(x)
      } else {
        find(rest, f)
      }
  }
}

/// Produce a list from elements that statisfy a predicate.
///
/// ----- TODO: Add support for writing tests.
///
/// test filter_map_1() {
///   filter_map([1, 2, 3, 4, 5, 6], fn(x) { if (builtin.modInteger(x, 2) != 0) { Some(3*x) } else { None } }) == [3, 9, 15]
/// }
pub fn filter_map(xs: List(a), f: fn(a) -> Option(b)) -> List(b) {
  foldr(
    xs,
    fn(x, ys) {
      when f(x) is {
        None -> ys
        Some(y) -> [y, ..ys]
      }
    },
    [],
  )
}

/// Map elements of a list into a new list and flatten the result.
pub fn flat_map(xs: List(a), f: fn(a) -> List(b)) -> List(b) {
  foldr(xs, fn(x, ys) { concat(f(x), ys) }, [])
}
