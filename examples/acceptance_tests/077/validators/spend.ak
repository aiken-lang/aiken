use aiken/list.{find, foldr}
use aiken/transaction.{Input, ScriptContext, Spend}
use aiken/transaction/value.{add, zero}

validator staking {
  fn(_datum: Void, _redeemer: Void, context: ScriptContext) -> Bool {
    expect Spend(ref) =
      context.purpose

    expect Some(i) =
      find(context.transaction.inputs, fn(x) { x.output_reference == ref })
    let Input { output, .. } =
      i
    let staking_addr =
      output.address

    let v_in =
      foldr(
        context.transaction.inputs,
        fn(x, y) {
          if x.output.address == staking_addr {
            add(x.output.value, y)
          } else {
            y
          }
        },
        zero(),
      )

    let v_out =
      foldr(
        context.transaction.outputs,
        fn(x, y) {
          if x.address == staking_addr {
            add(x.value, y)
          } else {
            y
          }
        },
        zero(),
      )
    v_in == v_out
  }
}
