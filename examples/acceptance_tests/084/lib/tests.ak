use aiken/builtin.{snd_pair}
use aiken/cbor
use aiken/collection/list

test tuple_when() {
  let items =
    [(#"", #"", 50), (#"aa", #"bb", 70)]

  let amount = 70
  let policy = #"aa"

  let filtered =
    list.filter(
      items,
      fn(item) {
        when item is {
          (token_policy, _, token_amount) ->
            amount == token_amount && policy == token_policy
        }
      },
    )

  list.length(filtered) > 0
}

test t() {
  trace cbor.diagnostic(list.map([Pair(#"", 20)], snd_pair))
  True
}
