import { useEffect, useState } from "preact/hooks";
import { Blockfrost, Lucid } from "~/vendor/lucid@0.9.4/mod.ts";

import { Input } from "~/components/Input.tsx";
import { Button } from "~/components/Button.tsx";

import { applyParams, Validators } from "~/utils.ts";

export interface AppProps {
  validators: Validators;
}

export default function App({ validators }: AppProps) {
  const [lucid, setLucid] = useState<Lucid | null>(null);
  const [tokenName, setTokenName] = useState<string>("");

  const setupLucid = async (blockfrostApiKey: string) => {
    const lucid = await Lucid.new(
      new Blockfrost(
        "https://cardano-mainnet.blockfrost.io/api/v0",
        blockfrostApiKey,
      ),
      "Mainnet",
    );

    setLucid(lucid);
  };

  useEffect(() => {
    if (lucid) {
      window.cardano
        .eternl
        .enable()
        .then((wallet) => {
          lucid.selectWallet(wallet);
        });
    }
  }, [lucid]);

  const submitTokenName = async (e: Event) => {
    e.preventDefault();

    const utxos = await lucid?.wallet.getUtxos()!;

    const utxo = utxos[0];
    const outputReference = {
      txHash: utxo.txHash,
      outputIndex: utxo.outputIndex,
    };

    const { lock, mint } = applyParams(tokenName, outputReference, validators);

    console.log(lock, mint);
  };

  return (
    <div>
      {!lucid
        ? (
          <Input
            type="password"
            id="blockfrostApiKey"
            onKeyDown={async (e) => {
              if (e.key === "Enter") {
                await setupLucid(e.currentTarget.value);
              }
            }}
          >
            Blockfrost API KEY (PRESS ENTER)
          </Input>
        )
        : (
          <form
            class="mt-10 grid grid-cols-1 gap-y-8"
            onSubmit={submitTokenName}
          >
            <Input
              type="text"
              name="tokenName"
              id="tokenName"
              onInput={(e) => setTokenName(e.currentTarget.value)}
            >
              Token Name
            </Input>

            {tokenName && (
              <Button type="submit">
                Make Contracts
              </Button>
            )}
          </form>
        )}
    </div>
  );
}
