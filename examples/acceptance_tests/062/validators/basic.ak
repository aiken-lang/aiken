use aiken/list
use aiken/transaction.{ScriptContext}

fn spend(_datum: Void, _redeemer: Void, ctx: ScriptContext) {
  list.length(ctx.transaction.outputs) == 1
}
