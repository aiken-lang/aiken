import { useEffect, useState } from "preact/hooks";
import { Blockfrost, Lucid } from "~/vendor/lucid@0.9.4/mod.ts";

import { Input } from "~/components/Input.tsx";
import { Button } from "~/components/Button.tsx";

import { Validators } from "~/utils.ts";

export interface AppProps {
  validators: Validators;
}

export default function App({ validators }: AppProps) {
  const [lucid, setLucid] = useState<Lucid | null>(null);

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

          return lucid.wallet.getUtxos();
        })
        .then((utxos) => console.log(utxos));
    }
  }, [lucid]);

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
          <form class="mt-10 grid grid-cols-1 gap-y-8">
            <Input type="text" name="tokenName" id="tokenName">
              Token Name
            </Input>

            <Button type="submit">
              Make Contract
            </Button>
          </form>
        )}
    </div>
  );
}
