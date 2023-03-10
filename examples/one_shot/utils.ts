import {
  applyParamsToScript,
  Data,
  fromHex,
  MintingPolicy,
  SpendingValidator,
  toHex,
} from "lucid";
import * as cbor from "cbor";

import { Blueprint } from "~/blueprint.ts";

export type Validators = {
  lock: SpendingValidator;
  mint: MintingPolicy;
};

async function readValidators(): Promise<Validators> {
  const blueprint: Blueprint = JSON
    .parse(await Deno.readTextFile("plutus.json"));

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
      script: toHex(cbor.encode(fromHex(lock.compiledCode))),
    },
    mint: {
      type: "PlutusV2",
      script: toHex(cbor.encode(fromHex(mint.compiledCode))),
    },
  };
}

export async function applyParams(
  // bytes
  tokenName: string,
  // bytes
  policyId: string,
  // aiken/transaction.{OutputReference}
  outputReference: any,
): Promise<{ lock: string; mint: string }> {
  const validators = await readValidators();

  return {
    // TODO: apply tokenName and policyId
    lock: applyParamsToScript(validators.lock.script, []),
    // TODO: apply tokenName and outputReference
    mint: applyParamsToScript(validators.mint.script, []),
  };
}
