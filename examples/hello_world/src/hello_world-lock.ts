import {
  Blockfrost,
  Constr,
  Data,
  fromHex,
  Blaze,
  SpendingValidator,
  toHex,
} from "@blaze-cardano/sdk";
import { TransactionId } from "@blaze-cardano/core";
import * as cbor from "https://deno.land/x/cbor@v1.4.1/index.js";

const blaze = new Blaze(
  new Blockfrost({
    network: "cardano-preview",
    projectId: process.env.BLOCKFROST_API_KEY ?? "",
  }),
  "Preview",
);

blaze.selectWalletFromPrivateKey(await Deno.readTextFile("./key.sk"));

const validator = await readValidator();

// --- Supporting functions

async function readValidator(): Promise<SpendingValidator> {
  const validator = JSON.parse(await Deno.readTextFile("plutus.json"))
    .validators[0];

  return {
    type: "PlutusV2",
    script: toHex(cbor.encode(fromHex(validator.compiledCode))),
  };
}

const publicKeyHash = blaze.utils.getAddressDetails(
  await blaze.wallet.address(),
).paymentCredential?.hash;

const datum = Data.to(new Constr(0, [publicKeyHash]));

const txId = await lock(1000000n, { into: validator, owner: datum });

await blaze.provider.awaitTransactionConfirmation(txId);

console.log(`1 tADA locked into the contract at:
    Tx ID: ${txHash}
    Datum: ${datum}
`);

// --- Supporting functions

async function lock(
  lovelace: bigint,
  { into, owner }: { into: SpendingValidator; owner: string },
): Promise<TransactionId> {
  const contractAddress = blaze.utils.validatorToAddress(into);

  const txBuilder = await blaze.newTransaction();

  const tx = await txBuilder
    .lockLovelace(contractAddress, lovelace, owner)
    .complete();

  const witnessSet = await blaze.wallet.signTransaction(tx, false);

  tx.setWitnessSet(witnessSet);

  const txId = await blaze.wallet.postTransaction(tx);

  return txId;
}
