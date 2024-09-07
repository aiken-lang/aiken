use aiken/collection/list
use cardano/address.{Script, VerificationKey}
use cardano/certificate.{
  AlwaysAbstain, AlwaysNoConfidence, AuthorizeConstitutionalCommitteeProxy,
  Certificate, DelegateBlockProduction, DelegateBoth, DelegateCredential,
  DelegateVote, RegisterAndDelegateCredential, RegisterCredential,
  RegisterDelegateRepresentative, RegisterStakePool, Registered,
  RetireFromConstitutionalCommittee, RetireStakePool, UnregisterCredential,
  UnregisterDelegateRepresentative, UpdateDelegateRepresentative,
}
use cardano/transaction.{Transaction}

const only0s = #"00000000000000000000000000000000000000000000000000000000"

const only1s = #"11111111111111111111111111111111111111111111111111111111"

const only2s = #"22222222222222222222222222222222222222222222222222222222"

const only9s =
  #"9999999999999999999999999999999999999999999999999999999999999999"

validator purposes {
  publish(_redeemer: Void, certificate: Certificate, transaction: Transaction) {
    assert_script_info(certificate)

    let certificates = transaction.certificates

    expect Some(10_000_000) = transaction.current_treasury_amount

    expect Some(14) = transaction.treasury_donation

    expect
      Some(RegisterCredential { credential: Script(only2s), deposit: Never }) == list.at(
        certificates,
        0,
      )

    expect
      Some(
        RegisterCredential {
          credential: VerificationKey(only0s),
          deposit: Never,
        },
      ) == list.at(certificates, 1)

    expect
      Some(
        UnregisterCredential {
          credential: VerificationKey(only0s),
          refund: Never,
        },
      ) == list.at(certificates, 2)

    expect
      Some(RegisterStakePool { stake_pool: only1s, vrf: only9s }) == list.at(
        certificates,
        3,
      )

    expect
      Some(RetireStakePool { stake_pool: only1s, at_epoch: 1337 }) == list.at(
        certificates,
        4,
      )

    expect
      Some(
        RegisterCredential {
          credential: VerificationKey(only0s),
          deposit: Never,
        },
      ) == list.at(certificates, 5)

    expect
      Some(
        UnregisterCredential {
          credential: VerificationKey(only0s),
          refund: Never,
        },
      ) == list.at(certificates, 6)

    expect
      Some(
        DelegateCredential {
          credential: VerificationKey(only0s),
          delegate: DelegateVote(Registered(VerificationKey(only0s))),
        },
      ) == list.at(certificates, 7)

    expect
      Some(
        DelegateCredential {
          credential: VerificationKey(only0s),
          delegate: DelegateVote(Registered(Script(only0s))),
        },
      ) == list.at(certificates, 8)

    expect
      Some(
        DelegateCredential {
          credential: VerificationKey(only0s),
          delegate: DelegateVote(AlwaysAbstain),
        },
      ) == list.at(certificates, 9)

    expect
      Some(
        DelegateCredential {
          credential: VerificationKey(only0s),
          delegate: DelegateVote(AlwaysNoConfidence),
        },
      ) == list.at(certificates, 10)

    expect
      Some(
        DelegateCredential {
          credential: VerificationKey(only0s),
          delegate: DelegateBoth {
            stake_pool: only1s,
            delegate_representative: AlwaysNoConfidence,
          },
        },
      ) == list.at(certificates, 11)

    expect
      Some(
        RegisterAndDelegateCredential {
          credential: VerificationKey(only0s),
          delegate: DelegateBlockProduction(only1s),
          deposit: 3_000_000,
        },
      ) == list.at(certificates, 12)

    expect
      Some(
        RegisterAndDelegateCredential {
          credential: VerificationKey(only0s),
          delegate: DelegateVote(AlwaysNoConfidence),
          deposit: 3_000_000,
        },
      ) == list.at(certificates, 13)

    expect
      Some(
        RegisterAndDelegateCredential {
          credential: VerificationKey(only0s),
          delegate: DelegateBoth {
            stake_pool: only1s,
            delegate_representative: AlwaysNoConfidence,
          },
          deposit: 3_000_000,
        },
      ) == list.at(certificates, 14)

    expect
      Some(
        AuthorizeConstitutionalCommitteeProxy {
          constitutional_committee_member: VerificationKey(only0s),
          proxy: VerificationKey(only2s),
        },
      ) == list.at(certificates, 15)

    expect
      Some(
        RetireFromConstitutionalCommittee {
          constitutional_committee_member: VerificationKey(only0s),
        },
      ) == list.at(certificates, 16)

    expect
      Some(
        RegisterDelegateRepresentative {
          delegate_representative: VerificationKey(only0s),
          deposit: 3_000_000,
        },
      ) == list.at(certificates, 17)

    expect
      Some(
        UnregisterDelegateRepresentative {
          delegate_representative: VerificationKey(only0s),
          refund: 3_000_000,
        },
      ) == list.at(certificates, 18)

    expect
      Some(
        UpdateDelegateRepresentative {
          delegate_representative: VerificationKey(only0s),
        },
      ) == list.at(certificates, 19)

    expect Some(DelegateCredential {
      credential: Script(..),
      delegate: DelegateBlockProduction(..),
    }) = list.at(certificates, 20)

    True
  }

  else(_ctx) {
    fail
  }
}

fn assert_script_info(certificate: Certificate) {
  expect DelegateCredential { credential, delegate } = certificate
  expect DelegateBlockProduction { stake_pool: only1s } == delegate
  expect Script(..) = credential
  Void
}
