use aiken/list
use aiken/transaction.{Output, ScriptContext}

validator {
  fn backtrace(_datum: Void, _redeemer: Void, context: ScriptContext) -> Bool {
    expect Some(_) = list.find(context.transaction.outputs, fn(_) { True })
    let _ = find_stuff(context)
    True
  }
}

fn find_stuff(context: ScriptContext) -> Output {
  expect Some(stuff) = list.find(context.transaction.outputs, fn(_) { True })
  stuff
}
