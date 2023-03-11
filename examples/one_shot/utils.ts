import {
  applyDoubleCborEncoding,
  applyParamsToScript,
  C,
  Constr,
  Data,
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

export function applyParams(
  tokenName: string,
  outputReference: OutRef,
  validators: Validators,
  lucid: Lucid,
): { lock: string; mint: string } {
  const outRef = new Constr(0, [
    new Constr(0, [outputReference.txHash]),
    BigInt(outputReference.outputIndex),
  ]);

  const mint = applyParamsToScript(validators.mint.script, [
    fromText(tokenName),
    outRef,
  ]);

  const policyId = lucid.utils.validatorToScriptHash(validators.mint);

  const lock = applyParamsToScript(validators.lock.script, [
    fromText(tokenName),
    policyId,
  ]);

  return {
    lock: applyDoubleCborEncoding(lock),
    mint: applyDoubleCborEncoding(mint),
  };
}
