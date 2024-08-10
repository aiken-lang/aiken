# V3 Script Context Tests

This project contains a few handcrafted Plutus V3 validators and transactions
whose sole purpose is to test the interpretation of the `ScriptContext` from
within an Aiken's validators.

So validators are meant to work hand-in-hand with an associated context.
Because we can't have fully static context (since they contain the validator
and its hash), we define _templates_.

Everything is a bit clunky, but steps have been captured in a `test.sh` script
for convenience.

## How to use

```
./test.sh [VALIDATOR_TITLE]
```

## Test Coverage

- Purpose
  - [x] spend
  - [x] mint
  - [ ] withdraw
  - [ ] publish
  - [ ] voting
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
          - [ ] no deposit
          - [ ] with deposit
      - Unregister credential
          - [ ] no deposit
          - [ ] with deposit
      - [ ] Delegate
      - [ ] Register & delegate credential
      - [ ] Register drep
      - [ ] Unregister drep
      - [ ] Update drep
      - [ ] Register pool
      - [ ] Retire pool
      - [ ] Delegate CC
      - [ ] Retire CC
  - withdrawals
    - [x] none
    - [ ] some
  - [ ] validity range
  - extra signatories
    - [x] none
    - [ ] some
  - [x] redeemers
  - [x] datums
  - votes
     - [x] none
     - [ ] some
  - proposal procedures
     - [x] none
     - [x] some
  - current treasury
     - [ ] with
     - [x] without
  - treasury donation
     - [ ] with
     - [x] without

- Address
    - [x] type-0 (key | key)
    - [x] type-1 (script | key)
    - [ ] type-2 (key | script)
    - [ ] type-3 (script | script)
    - [ ] type-4 (key | ptr)
    - [ ] type-5 (script | ptr)
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
    - [ ] with action id
    - [ ] without action id
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
  - [ ] No
  - [ ] Yes
  - [ ] Abstain

- Voter
  - [ ] CC
  - [ ] DRep
  - [ ] SPO

- ChangedParameters
  - [ ] 0: coin
  - [ ] 1 : coin
  - [ ] 2 : uint .size 4
  - [ ] 3 : uint .size 4
  - [ ] 4 : uint .size 2
  - [ ] 5 : coin
  - [ ] 6 : coin
  - [ ] 7 : epoch_interval
  - [ ] 8 : uint .size 2
  - [ ] 9 : nonnegative_interval
  - [ ] 10 : unit_interval
  - [ ] 11 : unit_interval
  - [ ] 16 : coin
  - [ ] 17 : coin
  - [ ] 18 : costmdls
  - [ ] 19 : ex_unit_prices
  - [ ] 20 : ex_units
  - [ ] 21 : ex_units
  - [ ] 22 : uint .size 4
  - [ ] 23 : uint .size 2
  - [ ] 24 : uint .size 2
  - [ ] 25 : pool_voting_thresholds
  - [ ] 26 : drep_voting_thresholds
  - [ ] 27 : uint .size 2
  - [ ] 28 : epoch_interval
  - [ ] 29 : epoch_interval
  - [ ] 30 : coin
  - [ ] 31 : coin
  - [ ] 32 : epoch_interval
  - [ ] 33 : nonnegative_interval}

- Constitution
  - [x] with guardrail script
  - [x] without guardrail script

- Credential
  - [x] key
  - [x] script

- Delegatee
  - [ ] pool
  - [ ] drep
  - [ ] pool + drep

- DRep
  - [ ] key
  - [ ] script
  - [ ] abstain
  - [ ] no confidence

- Boundary
  - [ ] closed
  - [ ] open
