# V3 Script Context Tests

This project contains a few handcrafted Plutus V3 validators and transactions
whose sole purpose is to test the interpretation of the `ScriptContext` from
within an Aiken's validators.

So validators are meant to work hand-in-hand with an associated context.
Because we can't have fully static context (since they contain the validator
and its hash), we define _templates_.

Everything is a bit clunky, but steps have been captured in a `test.sh` script
for convenience. The test still assumes a few things. For any
`VALIDATOR_GROUP`:

- There's a `ctx/{VALIDATOR_GROUP}/tx.template` and
  `ctx/{VALIDATOR_GROUP}/resolved_inputs.template` respectively.

- There's a corresponding validator `validators/{VALIDATOR_GROUP}.ak`

- Templates may reference variables using a mustache-template-like syntax `{{ ... }}`.
  Provided variables are:
  - `{VALIDATOR_GROUP}.{VALIDATOR_TITLE}.hash`
  - `{VALIDATOR_GROUP}.{VALIDATOR_TITLE}.cbor`

  Where `VALIDATOR_TITLE` corresponds to the validator Aiken's name.

## How to use

```
./test.sh VALIDATOR_GROUP
```

> ![TIP]
> By default, this recompiles the project in --release mode, which can be long
> when iterating / testing. You can provide a binary to use as a second
> argument. For a dev build, just do:
>
> ```
> ./test.sh VALIDATOR_GROUP "cargo run --"
> ```

## Test Coverage

- Purpose
  - [x] spend
  - [x] mint
  - [x] withdraw
  - [x] publish
  - [x] voting
  - [x] proposing

- Transaction body
  - [x] inputs
  - reference inputs
    - [x] none
    - [x] some
  - outputs
    - [x] none
    - [x] some
  - [x] fee
  - [x] mint
  - certificates
    - [x] none
    - some
      - Register credential
          - [x] no deposit
          - [x] with deposit
      - Unregister credential
          - [x] no deposit
          - [x] with deposit
      - [x] Delegate
      - [x] Register & delegate credential
      - [x] Register drep
      - [x] Unregister drep
      - [x] Update drep
      - [x] Register pool
      - [x] Retire pool
      - [x] Delegate CC
      - [x] Retire CC
  - withdrawals
    - [x] none
    - [x] some
  - [x] validity range
  - extra signatories
    - [x] none
    - [x] some
  - [x] redeemers
  - [x] datums
  - votes
     - [x] none
     - [x] some
  - proposal procedures
     - [x] none
     - [x] some
  - current treasury
     - [x] with
     - [x] without
  - treasury donation
     - [x] with
     - [x] without

- Address
    - [x] type-0 (key | key)
    - [x] type-1 (script | key)
    - [x] type-2 (key | script)
    - [x] type-3 (script | script)
    - [x] type-4 (key | ptr)
    - [x] type-5 (script | ptr)
    - [x] type-6 (key | ø)
    - [x] type-7 (key | ø)

- Value
    - [x] only ada
    - [x] multi-assets

- Output datum
    - [x] none
    - [x] hash
    - [x] inline

- Output script
    - [x] none
    - [x] inline

- Governance Action
  - parameter change
    - [x] with action id
    - [x] without action id
  - hardfork initiation
    - [x] with action id
    - [x] without action id
  - treasuryWithdrawals
    - [x] with constitution
    - [x] without constitution
  - no confidence
    - [x] with action id
    - [x] without action id
  - update committee
    - [x] with action id
    - [x] without action id
  - new constitution
    - [x] with action id
    - [x] without action id
  - [x] info action

- Vote
  - [x] No
  - [x] Yes
  - [x] Abstain

- Voter
  - [x] CC
  - [x] DRep
  - [x] SPO

- ChangedParameters
  - [x] txFeePerByte
  - [x] txFeeFixed
  - [x] maxBlockBodySize
  - [x] maxTxSize
  - [x] maxBlockHeaderSize
  - [x] stakeAddressDeposit
  - [x] stakePoolDeposit
  - [x] poolRetireMaxEpoch
  - [x] stakePoolTargetNum
  - [x] poolPledgeInfluence
  - [x] monetaryExpansion
  - [x] treasuryCut
  - [x] minPoolCost
  - [x] utxoCostPerByte
  - [ ] costModels
  - [x] executionUnitPrices
  - [x] maxTxExecutionUnits
  - [x] maxBlockExecutionUnits
  - [x] maxValueSize
  - [x] collateralPercentage
  - [x] maxCollateralInputs
  - [x] poolVotingThresholds
  - [x] dRepVotingThresholds
  - [x] committeeMinSize
  - [x] committeeMaxTermLength
  - [x] govActionLifetime
  - [x] govActionDeposit
  - [x] dRepDeposit
  - [x] dRepActivity
  - [x] minFeeRefScriptCostPerByte

- Constitution
  - [x] with guardrail script
  - [x] without guardrail script

- Credential
  - [x] key
  - [x] script

- Delegatee
  - [x] pool
  - [x] drep
  - [x] pool + drep

- DRep
  - [x] key
  - [x] script
  - [x] abstain
  - [x] no confidence

- Boundary
  - [x] closed
  - [x] open
