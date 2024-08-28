use aiken/collection/list
use cardano/address.{Script, VerificationKey}
use cardano/governance.{
  Abstain, ConstitutionalCommitteeMember, DelegateRepresentative,
  GovernanceActionId, No, StakePool, Voter, Yes,
}
use cardano/transaction.{Transaction}

type Foo {
  Foo(Int)
}

const only0s = #"00000000000000000000000000000000000000000000000000000000"

const only7s =
  #"7777777777777777777777777777777777777777777777777777777777777777"

const only8s =
  #"8888888888888888888888888888888888888888888888888888888888888888"

const only9s =
  #"9999999999999999999999999999999999999999999999999999999999999999"

validator purposes {
  vote(redeemer: Data, _voter: Voter, transaction: Transaction) {
    assert_redeemer(redeemer)

    let votes = transaction.votes

    expect Some(
      Pair(ConstitutionalCommitteeMember(Script(..)),
      [Pair(GovernanceActionId { transaction, proposal_procedure: 1 }, Yes)]),
    ) = list.at(votes, 0)
    expect transaction == only9s

    expect
      Some(
        Pair(
          ConstitutionalCommitteeMember(VerificationKey(only0s)),
          [
            Pair(
              GovernanceActionId {
                transaction: only9s,
                proposal_procedure: 152,
              },
              No,
            ),
          ],
        ),
      ) == list.at(votes, 1)

    expect Some(Pair(DelegateRepresentative(Script(..)), [_, ..])) =
      list.at(votes, 2)

    expect
      Some(
        Pair(
          DelegateRepresentative(VerificationKey(only0s)),
          [
            Pair(
              GovernanceActionId { transaction: only7s, proposal_procedure: 2 },
              Abstain,
            ),
            Pair(
              GovernanceActionId { transaction: only8s, proposal_procedure: 1 },
              Abstain,
            ),
            Pair(
              GovernanceActionId { transaction: only9s, proposal_procedure: 0 },
              Abstain,
            ),
          ],
        ),
      ) == list.at(votes, 3)

    expect Some(Pair(StakePool(pool_id), [_, ..])) = list.at(votes, 4)
    expect pool_id == only0s

    True
  }

  else(_ctx) {
    fail
  }
}

fn assert_redeemer(data: Data) {
  let is_valid =
    if data is Foo(42): Foo {
      trace data
      True
    } else if data is Void {
      trace data
      True
    } else {
      False
    }

  expect is_valid
  Void
}
