use aiken/collection/list
use aiken/primitive/int

// ------------------------------------------------------------------ Benchmarks

test bench_clausify_f1() {
  run_clausify(F1) == []
}

test bench_clausify_f2() {
  run_clausify(F2) == []
}

test bench_clausify_f3() {
  run_clausify(F3) == [([1], [])]
}

test bench_clausify_f4() {
  run_clausify(F4) == [
    ([1], [2, 3, 4, 5, 6, 7]),
    ([1, 2, 3], [4, 5, 6, 7]),
    ([1, 2, 3, 4, 5], [6, 7]),
    ([1, 2, 3, 4, 5, 6, 7], []),
    ([1, 2, 3, 4, 6], [5, 7]),
    ([1, 2, 3, 4, 7], [5, 6]),
    ([1, 2, 3, 5, 6], [4, 7]),
    ([1, 2, 3, 5, 7], [4, 6]),
    ([1, 2, 3, 6, 7], [4, 5]),
    ([1, 2, 4], [3, 5, 6, 7]),
    ([1, 2, 4, 5, 6], [3, 7]),
    ([1, 2, 4, 5, 7], [3, 6]),
    ([1, 2, 4, 6, 7], [3, 5]),
    ([1, 2, 5], [3, 4, 6, 7]),
    ([1, 2, 5, 6, 7], [3, 4]),
    ([1, 2, 6], [3, 4, 5, 7]),
    ([1, 2, 7], [3, 4, 5, 6]),
    ([1, 3, 4], [2, 5, 6, 7]),
    ([1, 3, 4, 5, 6], [2, 7]),
    ([1, 3, 4, 5, 7], [2, 6]),
    ([1, 3, 4, 6, 7], [2, 5]),
    ([1, 3, 5], [2, 4, 6, 7]),
    ([1, 3, 5, 6, 7], [2, 4]),
    ([1, 3, 6], [2, 4, 5, 7]),
    ([1, 3, 7], [2, 4, 5, 6]),
    ([1, 4, 5], [2, 3, 6, 7]),
    ([1, 4, 5, 6, 7], [2, 3]),
    ([1, 4, 6], [2, 3, 5, 7]),
    ([1, 4, 7], [2, 3, 5, 6]),
    ([1, 5, 6], [2, 3, 4, 7]),
    ([1, 5, 7], [2, 3, 4, 6]),
    ([1, 6, 7], [2, 3, 4, 5]),
    ([2], [1, 3, 4, 5, 6, 7]),
    ([2, 3, 4], [1, 5, 6, 7]),
    ([2, 3, 4, 5, 6], [1, 7]),
    ([2, 3, 4, 5, 7], [1, 6]),
    ([2, 3, 4, 6, 7], [1, 5]),
    ([2, 3, 5], [1, 4, 6, 7]),
    ([2, 3, 5, 6, 7], [1, 4]),
    ([2, 3, 6], [1, 4, 5, 7]),
    ([2, 3, 7], [1, 4, 5, 6]),
    ([2, 4, 5], [1, 3, 6, 7]),
    ([2, 4, 5, 6, 7], [1, 3]),
    ([2, 4, 6], [1, 3, 5, 7]),
    ([2, 4, 7], [1, 3, 5, 6]),
    ([2, 5, 6], [1, 3, 4, 7]),
    ([2, 5, 7], [1, 3, 4, 6]),
    ([2, 6, 7], [1, 3, 4, 5]),
    ([3], [1, 2, 4, 5, 6, 7]),
    ([3, 4, 5], [1, 2, 6, 7]),
    ([3, 4, 5, 6, 7], [1, 2]),
    ([3, 4, 6], [1, 2, 5, 7]),
    ([3, 4, 7], [1, 2, 5, 6]),
    ([3, 5, 6], [1, 2, 4, 7]),
    ([3, 5, 7], [1, 2, 4, 6]),
    ([3, 6, 7], [1, 2, 4, 5]),
    ([4], [1, 2, 3, 5, 6, 7]),
    ([4, 5, 6], [1, 2, 3, 7]),
    ([4, 5, 7], [1, 2, 3, 6]),
    ([4, 6, 7], [1, 2, 3, 5]),
    ([5], [1, 2, 3, 4, 6, 7]),
    ([5, 6, 7], [1, 2, 3, 4]),
    ([6], [1, 2, 3, 4, 5, 7]),
    ([7], [1, 2, 3, 4, 5, 6]),
  ]
}

test bench_clausify_f5() {
  run_clausify(F5) == []
}

// ----------------------------------------------------------------------- Setup

type Var =
  Int

type LRVars =
  (List<Var>, List<Var>)

type Formula {
  Sym(Var)
  Not(Formula)
  Dis(Formula, Formula)
  Con(Formula, Formula)
  Imp(Formula, Formula)
  Eqv(Formula, Formula)
}

type StaticFormula {
  F1
  F2
  F3
  F4
  F5
}

fn run_clausify(static_formula: StaticFormula) -> List<LRVars> {
  static_formula
    |> get_formula
    |> clauses
}

fn get_formula(static_formula: StaticFormula) -> Formula {
  when static_formula is {
    // (a = a) = (a = a) = (a = a)
    F1 ->
      Eqv(Eqv(Sym(1), Sym(1)), Eqv(Eqv(Sym(1), Sym(1)), Eqv(Sym(1), Sym(1))))

    // (a = a = a) = (a = a = a)
    F2 ->
      Eqv(Eqv(Sym(1), Eqv(Sym(1), Sym(1))), Eqv(Sym(1), Eqv(Sym(1), Sym(1))))

    // (a = a = a) = (a = a) = (a = a)
    F3 ->
      Eqv(
        Eqv(Sym(1), Eqv(Sym(1), Sym(1))),
        Eqv(Eqv(Sym(1), Sym(1)), Eqv(Sym(1), Sym(1))),
      )

    // (a = b = c) = (d = e) = (f = g)
    F4 ->
      Eqv(
        Eqv(Sym(1), Eqv(Sym(2), Sym(3))),
        Eqv(Eqv(Sym(4), Sym(5)), Eqv(Sym(6), Sym(7))),
      )

    // (a = a = a) = (a = a = a) = (a = a)
    F5 ->
      Eqv(
        Eqv(Sym(1), Eqv(Sym(1), Sym(1))),
        Eqv(Eqv(Sym(1), Eqv(Sym(1), Sym(1))), Eqv(Sym(1), Sym(1))),
      )
  }
}

fn clauses(formula: Formula) -> List<LRVars> {
  formula
    |> elim
    |> negin
    |> disin
    |> split
    |> unicl
}

fn clause(formula: Formula) -> LRVars {
  do_clause(formula, ([], []))
}

fn do_clause(formula: Formula, vars: LRVars) -> LRVars {
  when formula is {
    Dis(p, q) -> do_clause(p, do_clause(q, vars))
    Sym(s) -> (insert_ordered(vars.1st, s, int.compare), vars.2nd)
    Not(Sym(s)) -> (vars.1st, insert_ordered(vars.2nd, s, int.compare))
    _ -> fail
  }
}

fn conjunct(formula: Formula) -> Bool {
  when formula is {
    Con(_, _) -> True
    _ -> False
  }
}

/// eliminate connectives other than not, disjunction and conjunction
fn elim(formula: Formula) -> Formula {
  when formula is {
    Sym(s) -> Sym(s)
    Not(p) -> Not(elim(p))
    Dis(p, q) -> Dis(elim(p), elim(q))
    Con(p, q) -> Con(elim(p), elim(q))
    Imp(p, q) -> Dis(Not(elim(p)), elim(q))
    Eqv(f1, f2) -> Con(elim(Imp(f1, f2)), elim(Imp(f2, f1)))
  }
}

/// -- shift negation to innermost positions
fn negin(formula: Formula) -> Formula {
  when formula is {
    Not(Not(p)) -> negin(p)
    Not(Con(p, q)) -> Dis(negin(Not(p)), negin(Not(q)))
    Not(Dis(p, q)) -> Con(negin(Not(p)), negin(Not(q)))
    Dis(p, q) -> Dis(negin(p), negin(q))
    Con(p, q) -> Con(negin(p), negin(q))
    p -> p
  }
}

/// shift disjunction within conjunction
fn disin(formula: Formula) -> Formula {
  when formula is {
    Dis(p, Con(q, r)) -> Con(disin(Dis(p, q)), disin(Dis(p, r)))
    Dis(Con(p, q), r) -> Con(disin(Dis(p, r)), disin(Dis(q, r)))
    Dis(p, q) -> {
      let dp = disin(p)
      let dq = disin(q)
      if conjunct(dp) || conjunct(dq) {
        disin(Dis(dp, dq))
      } else {
        Dis(dp, dq)
      }
    }
    Con(p, q) -> Con(disin(p), disin(q))
    p -> p
  }
}

/// split conjunctive proposition into a list of conjuncts
fn split(formula: Formula) -> List<Formula> {
  do_split(formula, [])
}

fn do_split(f: Formula, fs: List<Formula>) -> List<Formula> {
  when f is {
    Con(p, q) -> do_split(p, do_split(q, fs))
    _ -> [f, ..fs]
  }
}

/// form unique clausal axioms excluding tautologies
fn unicl(formulas: List<Formula>) -> List<LRVars> {
  list.foldr(
    formulas,
    [],
    fn(p, xs) {
      let cp = clause(p)
      if tautclause(cp) {
        xs
      } else {
        insert_ordered(xs, cp, compare_lr_vars)
      }
    },
  )
}

/// does any symbol appear in both consequent and antecedent of clause
fn tautclause(var: LRVars) -> Bool {
  list.any(var.1st, list.has(var.2nd, _))
}

/// insertion of an item into an ordered list
fn insert_ordered(es: List<a>, e: a, compare: fn(a, a) -> Ordering) -> List<a> {
  when es is {
    [] -> [e]
    [head, ..tail] ->
      when compare(e, head) is {
        Less -> [e, ..es]
        Greater -> [head, ..insert_ordered(tail, e, compare)]
        Equal -> es
      }
  }
}

fn compare_lr_vars(a: LRVars, b: LRVars) -> Ordering {
  when compare_list(a.1st, b.1st) is {
    Equal -> compare_list(a.2nd, b.2nd)
    ord -> ord
  }
}

fn compare_list(xs: List<Int>, ys: List<Int>) -> Ordering {
  when xs is {
    [] ->
      when ys is {
        [] -> Equal
        _ -> Less
      }
    [x, ..xs] ->
      when ys is {
        [] -> Greater
        [y, ..ys] -> {
          let ord = int.compare(x, y)
          if ord == Equal {
            compare_list(xs, ys)
          } else {
            ord
          }
        }
      }
  }
}
