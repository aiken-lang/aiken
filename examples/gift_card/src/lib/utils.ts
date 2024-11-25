import {
  applyDoubleCborEncoding,
  applyParamsToScript,
  Constr,
  fromText,
  validatorToAddress,
  validatorToScriptHash,
  type MintingPolicy,
  type OutRef,
  type SpendingValidator
} from '@lucid-evolution/lucid';
import blueprint from '../../plutus.json' assert { type: 'json' };

export type Validators = {
  giftCard: string;
};

export type LocalCache = {
  tokenName: string;
  giftADA: string;
  lockTxHash: string;
  parameterizedValidators: AppliedValidators;
};

export type AppliedValidators = {
  redeem: SpendingValidator;
  giftCard: MintingPolicy;
  policyId: string;
  lockAddress: string;
};

export function readValidators(): Validators {
  const giftCard = blueprint.validators.find(
    (v) => v.title === 'oneshot.gift_card.spend'
  );

  if (!giftCard) {
    throw new Error('Gift Card validator not found');
  }

  return {
    giftCard: giftCard.compiledCode
  };
}

export function applyParams(
  tokenName: string,
  outputReference: OutRef,
  validator: string
): AppliedValidators {
  const outRef = new Constr(0, [
    new Constr(0, [outputReference.txHash]),
    BigInt(outputReference.outputIndex)
  ]);

  const giftCard = applyParamsToScript(validator, [
    fromText(tokenName),
    outRef
  ]);

  const policyId = validatorToScriptHash({
    type: 'PlutusV2',
    script: giftCard
  });

  const lockAddress = validatorToAddress('Preprod', {
    type: 'PlutusV2',
    script: giftCard
  });

  return {
    redeem: { type: 'PlutusV2', script: applyDoubleCborEncoding(giftCard) },
    giftCard: { type: 'PlutusV2', script: applyDoubleCborEncoding(giftCard) },
    policyId,
    lockAddress
  };
}
