import { Blockfrost, Blaze } from "@blaze-cardano/sdk";
import {
  NetworkId,
  TransactionId,
  Ed25519PrivateNormalKeyHex,
  PlutusData,
} from "@blaze-cardano/core";
import { HotWallet } from "@blaze-cardano/wallet";
import * as fs from "node:fs";

const privateKey = fs.readFileSync("./key.sk", { encoding: "utf8" });

const provider = new Blockfrost({
  network: "cardano-preview",
  projectId: process.env.BLOCKFROST_API_KEY ?? "",
});

const wallet = new HotWallet(
  Ed25519PrivateNormalKeyHex(privateKey),
  NetworkId.Preview,
  provider,
);

const blaze = new Blaze(provider, wallet);

const validator = await readValidator();

// --- Supporting functions

async function readValidator(): Promise<SpendingValidator> {
  const validator = JSON.parse(
    fs.readFileSync("plutus.json", { encoding: "utf8" }),
  ).validators[0];

  return {
    type: "PlutusV2",
    script: toHex(cbor.encode(fromHex(validator.compiledCode))),
  };
}

const addresses = await wallet.getUnusedAddresses();
const publicKeyHash = addresses[0].getProps().paymentPart!;

PlutusData.Constr(0, [publicKeyHash]);

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
