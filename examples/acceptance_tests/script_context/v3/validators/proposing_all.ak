use aiken/collection/list
use aiken/math/rational
use cardano/address.{Script, VerificationKey}
use cardano/governance.{
  Constitution, ConstitutionalCommittee, GovernanceAction, GovernanceActionId,
  HardFork, NewConstitution, NicePoll, NoConfidence, ProposalProcedure,
  ProtocolVersion, TreasuryWithdrawal,
}
use cardano/transaction.{Propose, Redeemer, ScriptPurpose, Transaction}

const null28 = #"00000000000000000000000000000000000000000000000000000000"

const null32 =
  #"0000000000000000000000000000000000000000000000000000000000000000"

validator guardrails {
  propose(_redeemer: Void, _: ProposalProcedure, transaction: Transaction) {
    let procedures = transaction.proposal_procedures

    assert_redeemers(transaction.redeemers)

    assert_proposal_procedure(
      list.at(procedures, 0),
      fn(action) {
        expect
          HardFork { ancestor: None, new_version: ProtocolVersion(10, 0) } == action
        Void
      },
    )

    assert_proposal_procedure(
      list.at(procedures, 1),
      fn(action) {
        expect
          HardFork {
            ancestor: Some(GovernanceActionId(null32, 0)),
            new_version: ProtocolVersion(11, 0),
          } == action
        Void
      },
    )

    assert_proposal_procedure(
      list.at(procedures, 2),
      fn(action) {
        expect
          TreasuryWithdrawal {
            beneficiaries: [
              Pair(
                VerificationKey(
                  #"11111111111111111111111111111111111111111111111111111111",
                ),
                1_000_000,
              ),
            ],
            guardrails: None,
          } == action
        Void
      },
    )

    assert_proposal_procedure(
      list.at(procedures, 3),
      fn(action) {
        expect TreasuryWithdrawal { guardrails: Some(..), .. } = action
        Void
      },
    )

    assert_proposal_procedure(
      list.at(procedures, 4),
      fn(action) {
        expect NoConfidence { ancestor: None } == action
        Void
      },
    )

    assert_proposal_procedure(
      list.at(procedures, 5),
      fn(action) {
        expect Some(quorum) = rational.new(1, 2)
        expect
          ConstitutionalCommittee {
            ancestor: None,
            evicted_members: [VerificationKey(null28)],
            added_members: [Pair(VerificationKey(null28), 500)],
            quorum,
          } == action
        Void
      },
    )

    assert_proposal_procedure(
      list.at(procedures, 6),
      fn(action) {
        expect
          NewConstitution {
            ancestor: None,
            constitution: Constitution { guardrails: None },
          } == action
        Void
      },
    )

    assert_proposal_procedure(
      list.at(procedures, 7),
      fn(action) {
        expect
          NewConstitution {
            ancestor: None,
            constitution: Constitution { guardrails: Some(null28) },
          } == action
        Void
      },
    )

    assert_last_proposal_procedure(list.at(procedures, 8))

    True
  }

  else(_ctx) {
    fail
  }
}

fn assert_redeemers(redeemers: Pairs<ScriptPurpose, Redeemer>) -> Void {
  expect [
    Pair(Propose(
      3,
      ProposalProcedure { deposit, return_address, governance_action },
    ),
    data),
  ] = redeemers
  expect deposit == 2_000_000
  expect return_address == Script(null28)
  expect TreasuryWithdrawal { .. } = governance_action
  expect Void = data
  Void
}

fn assert_proposal_procedure(
  procedure: Option<ProposalProcedure>,
  with: fn(GovernanceAction) -> Void,
) -> Void {
  expect Some(procedure) = procedure
  expect procedure.deposit == 2_000_000
  expect procedure.return_address == Script(null28)
  with(procedure.governance_action)
}

fn assert_last_proposal_procedure(procedure: Option<ProposalProcedure>) -> Void {
  expect Some(procedure) = procedure
  expect procedure.deposit == 2_000_000
  expect procedure.return_address == VerificationKey(null28)
  expect procedure.governance_action == NicePoll
  Void
}
