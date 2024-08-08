import {
  generateMnemonic,
  wordlist,
  mnemonicToEntropy,
  Bip32PrivateKey,
  Ed25519PrivateNormalKeyHex,
  NetworkId,
} from "@blaze-cardano/core";
import { Blockfrost } from "@blaze-cardano/sdk";
import { HotWallet } from "@blaze-cardano/wallet";
import * as fs from "node:fs";

const seedPhrase = generateMnemonic(wordlist);

const entropy = mnemonicToEntropy(seedPhrase, wordlist);

const bip32priv = await Bip32PrivateKey.fromBip39Entropy(
  Buffer.from(entropy),
  "",
);

const privateKey = bip32priv.toHex();

fs.writeFileSync("key.sk", privateKey, { encoding: "utf8" });

const provider = new Blockfrost({
  network: "cardano-preview",
  projectId: process.env.BLOCKFROST_API_KEY ?? "",
});

const wallet = new HotWallet(
  Ed25519PrivateNormalKeyHex(privateKey),
  NetworkId.Preview,
  provider,
);

const address = wallet.address.toBech32();

fs.writeFileSync("key.addr", address, { encoding: "utf8" });
