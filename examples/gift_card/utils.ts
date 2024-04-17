import {
  applyDoubleCborEncoding,
  applyParamsToScript,
  Constr,
  fromText,
  Lucid,
  MintingPolicy,
  OutRef,
  SpendingValidator,
} from "lucid/mod.ts";

import { Blueprint } from "./blueprint.ts";
import blueprint from "./plutus.json" assert { type: "json" };

export type Validators = {
  redeem: SpendingValidator;
  giftCard: MintingPolicy;
};

export type LocalCache = {
  tokenName: string;
  giftADA: string;
  lockTxHash: string;
  parameterizedValidators: AppliedValidators;
};

export function readValidators(): Validators {
  const redeem = (blueprint as Blueprint).validators.find((v) =>
    v.title === "oneshot.redeem"
  );

  if (!redeem) {
    throw new Error("Redeem validator not found");
  }

  const giftCard = (blueprint as Blueprint).validators.find((v) =>
    v.title === "oneshot.gift_card"
  );

  if (!giftCard) {
    throw new Error("Gift Card validator not found");
  }

  return {
    redeem: {
      type: "PlutusV2",
      script: redeem.compiledCode,
    },
    giftCard: {
      type: "PlutusV2",
      script: giftCard.compiledCode,
    },
  };
}

export type AppliedValidators = {
  redeem: SpendingValidator;
  giftCard: MintingPolicy;
  policyId: string;
  lockAddress: string;
};

export function applyParams(
  tokenName: string,
  outputReference: OutRef,
  validators: Validators,
  lucid: Lucid,
): AppliedValidators {
  const outRef = new Constr(0, [
    new Constr(0, [outputReference.txHash]),
    BigInt(outputReference.outputIndex),
  ]);

  const giftCard = applyParamsToScript(validators.giftCard.script, [
    fromText(tokenName),
    outRef,
  ]);

  const policyId = lucid.utils.validatorToScriptHash({
    type: "PlutusV2",
    script: giftCard,
  });

  const redeem = applyParamsToScript(validators.redeem.script, [
    fromText(tokenName),
    policyId,
  ]);

  const lockAddress = lucid.utils.validatorToAddress({
    type: "PlutusV2",
    script: redeem,
  });

  return {
    redeem: { type: "PlutusV2", script: applyDoubleCborEncoding(redeem) },
    giftCard: { type: "PlutusV2", script: applyDoubleCborEncoding(giftCard) },
    policyId,
    lockAddress,
  };
}
