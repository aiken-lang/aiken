use aiken/math/rational.{Rational}
use cardano/governance.{ProposalProcedure, ProtocolParameters}
use cardano/governance/protocol_parameters.{
  ConstitutionalCommitteeThresholds, DelegateRepresentativeVotingThresholds,
  ExecutionUnits, ProtocolParametersThresholds, ScriptExecutionPrices,
  StakePoolOperatorVotingThresholds, collateral_percentage, cost_models,
  delegate_representative_deposit, delegate_representative_max_idle_time,
  delegate_representative_voting_thresholds, desired_number_of_stake_pools,
  governance_proposal_deposit, governance_proposal_lifetime, max_block_body_size,
  max_block_execution_units, max_block_header_size, max_collateral_inputs,
  max_constitutional_committee_mandate, max_transaction_execution_units,
  max_transaction_size, max_value_size, min_constitutional_committee_size,
  min_fee_coefficient, min_fee_constant, min_stake_pool_cost,
  min_utxo_deposit_coefficient, monetary_expansion,
  reference_scripts_tier_fee_initial_factor, script_execution_prices,
  stake_credential_deposit, stake_pool_deposit,
  stake_pool_operator_voting_thresholds, stake_pool_pledge_influence,
  stake_pool_retirement_horizon, treasury_expansion,
}
use cardano/transaction.{Transaction}

validator guardrails {
  propose(
    _redeemer: Void,
    proposal: ProposalProcedure,
    _transaction: Transaction,
  ) {
    expect ProposalProcedure {
      governance_action: ProtocolParameters {
        ancestor: None,
        guardrails: Some(..),
        new_parameters,
      },
      ..
    } = proposal

    expect ( new_parameters |> min_fee_coefficient ) == Some(44)

    expect ( new_parameters |> min_fee_constant ) == Some(155_381)

    expect ( new_parameters |> max_block_body_size ) == Some(90_112)

    expect ( new_parameters |> max_transaction_size ) == Some(16_384)

    expect ( new_parameters |> max_block_header_size ) == Some(1_100)

    expect ( new_parameters |> stake_credential_deposit ) == Some(2_000_000)

    expect ( new_parameters |> stake_pool_deposit ) == Some(500_000_000)

    expect ( new_parameters |> stake_pool_retirement_horizon ) == Some(18)

    expect ( new_parameters |> desired_number_of_stake_pools ) == Some(500)

    expect
      ( new_parameters |> stake_pool_pledge_influence ) == Some(
        expect_rational(3, 10),
      )

    expect
      ( new_parameters |> monetary_expansion ) == Some(
        expect_rational(3, 1_000),
      )

    expect
      ( new_parameters |> treasury_expansion ) == Some(expect_rational(1, 5))

    expect ( new_parameters |> min_stake_pool_cost ) == Some(340)

    expect ( new_parameters |> min_utxo_deposit_coefficient ) == Some(4310)

    expect ( new_parameters |> cost_models ) == None

    expect
      ( new_parameters |> script_execution_prices ) == Some(
        ScriptExecutionPrices {
          memory: expect_rational(577, 1_000),
          cpu: expect_rational(721, 1_000_000),
        },
      )

    expect
      ( new_parameters |> max_transaction_execution_units ) == Some(
        ExecutionUnits { memory: 14000000, cpu: 10000000000 },
      )

    expect
      ( new_parameters |> max_block_execution_units ) == Some(
        ExecutionUnits { memory: 62000000, cpu: 20000000000 },
      )

    expect ( new_parameters |> max_value_size ) == Some(5000)

    expect ( new_parameters |> collateral_percentage ) == Some(150)

    expect ( new_parameters |> max_collateral_inputs ) == Some(3)

    expect
      ( new_parameters |> stake_pool_operator_voting_thresholds ) == Some(
        spo_thresholds(),
      )

    expect
      ( new_parameters |> delegate_representative_voting_thresholds ) == Some(
        drep_thresholds(),
      )

    expect ( new_parameters |> min_constitutional_committee_size ) == Some(7)

    expect
      ( new_parameters |> max_constitutional_committee_mandate ) == Some(146)

    expect ( new_parameters |> governance_proposal_lifetime ) == Some(6)

    expect
      ( new_parameters |> governance_proposal_deposit ) == Some(100_000_000_000)

    expect
      ( new_parameters |> delegate_representative_deposit ) == Some(500_000_000)

    expect
      ( new_parameters |> delegate_representative_max_idle_time ) == Some(20)

    expect
      ( new_parameters |> reference_scripts_tier_fee_initial_factor ) == Some(
        expect_rational(15, 1),
      )

    True
  }

  else(_ctx) {
    fail
  }
}

fn expect_rational(numerator: Int, denominator: Int) -> Rational {
  expect Some(r) = rational.new(numerator, denominator)
  r
}

fn spo_thresholds() -> StakePoolOperatorVotingThresholds {
  StakePoolOperatorVotingThresholds {
    motion_of_no_confidence: expect_rational(51, 100),
    constitutional_committee: ConstitutionalCommitteeThresholds {
      default: expect_rational(13, 25),
      under_no_confidence: expect_rational(53, 100),
    },
    hard_fork: expect_rational(27, 50),
    protocol_parameters: ProtocolParametersThresholds {
      security_group: expect_rational(11, 20),
      network_group: Void,
      economic_group: Void,
      technical_group: Void,
      governance_group: Void,
    },
  }
}

fn drep_thresholds() -> DelegateRepresentativeVotingThresholds {
  DelegateRepresentativeVotingThresholds {
    motion_of_no_confidence: expect_rational(67, 100),
    constitutional_committee: ConstitutionalCommitteeThresholds {
      default: expect_rational(67, 100),
      under_no_confidence: expect_rational(3, 5),
    },
    constitution: expect_rational(3, 4),
    hard_fork: expect_rational(3, 5),
    protocol_parameters: ProtocolParametersThresholds {
      security_group: Void,
      network_group: expect_rational(67, 100),
      economic_group: expect_rational(67, 100),
      technical_group: expect_rational(67, 100),
      governance_group: expect_rational(3, 4),
    },
    treasury_withdrawal: expect_rational(67, 100),
  }
}
