import {
  applyDoubleCborEncoding,
  applyParamsToScript,
  Constr,
  fromText,
  Lucid,
  MintingPolicy,
  OutRef,
  SpendingValidator,
} from "~/vendor/lucid@0.9.4/mod.ts";

import { Blueprint } from "~/blueprint.ts";

export type Validators = {
  lock: SpendingValidator;
  mint: MintingPolicy;
};

export type LocalCache = {
  tokenName: string;
  giftADA: string;
  lockTxHash: string;
  parameterizedValidators: AppliedValidators;
};

export async function readValidators(): Promise<Validators> {
  const blueprint: Blueprint = JSON.parse(
    await Deno.readTextFile("plutus.json"),
  );

  const lock = blueprint.validators.find((v) => v.title === "main.lock");

  if (!lock) {
    throw new Error("Lock validator not found");
  }

  const mint = blueprint.validators.find((v) => v.title === "main.mint");

  if (!mint) {
    throw new Error("Mint validator not found");
  }

  return {
    lock: {
      type: "PlutusV2",
      script: lock.compiledCode,
    },
    mint: {
      type: "PlutusV2",
      script: mint.compiledCode,
    },
  };
}

export type AppliedValidators = {
  lock: SpendingValidator;
  mint: MintingPolicy;
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

  const mint = applyParamsToScript(validators.mint.script, [
    fromText(tokenName),
    outRef,
  ]);

  const policyId = lucid.utils.validatorToScriptHash({
    type: "PlutusV2",
    script: mint,
  });

  const lock = applyParamsToScript(validators.lock.script, [
    fromText(tokenName),
    policyId,
  ]);

  const lockAddress = lucid.utils.validatorToAddress({
    type: "PlutusV2",
    script: lock,
  });

  return {
    lock: { type: "PlutusV2", script: applyDoubleCborEncoding(lock) },
    mint: { type: "PlutusV2", script: applyDoubleCborEncoding(mint) },
    policyId,
    lockAddress,
  };
}
