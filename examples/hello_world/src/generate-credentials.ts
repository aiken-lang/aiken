import {
  generateMnemonic,
  wordlist,
  mnemonicToEntropy,
  Bip32PrivateKey,
} from "@blaze-cardano/core";
import * as fs from "node:fs";

const seedPhrase = generateMnemonic(wordlist);

const entropy = mnemonicToEntropy(seedPhrase, wordlist);

const bip32priv = await Bip32PrivateKey.fromBip39Entropy(
  Buffer.from(entropy),
  "",
);

const privateKey = bip32priv.toHex();

fs.writeFileSync("key.sk", privateKey, { encoding: "utf8" });

const address = await lucid
  .selectWalletFromPrivateKey(privateKey)
  .wallet.address();

fs.writeFileSync("key.addr", address, { encoding: "utf8" });
