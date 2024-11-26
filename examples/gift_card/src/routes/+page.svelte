<script lang="ts">
  import { onMount } from 'svelte';
  import { getWeldContext } from '$lib/wallet.svelte';
  import {
    Lucid,
    Blockfrost,
    type LucidEvolution,
    Data,
    Constr,
    fromText
  } from '@lucid-evolution/lucid';

  // Components
  import Input from '$lib/components/Input.svelte';
  import Button from '$lib/components/Button.svelte';

  // Utils
  import {
    applyParams,
    type AppliedValidators,
    type LocalCache
  } from '$lib/utils';

  // Local Types
  import type { PageData } from './$types';

  // Props
  type Props = {
    data: PageData;
  };

  let { data }: Props = $props();

  let weld = getWeldContext();

  let displayedBalance = $derived(weld.wallet.balanceAda?.toFixed(2) ?? '-');

  let blockfrostAPIKey = $state('');
  let tokenName = $state('');
  let giftADA: string | undefined = $state();
  let lockTxHash: string | undefined = $state();
  let unlockTxHash: string | undefined = $state();
  let parameterizedContracts: AppliedValidators | undefined = $state();

  let waitingLockTx = $state(false);
  let waitingUnlockTx = $state(false);

  let lucid: LucidEvolution | undefined = $state();

  onMount(() => {
    weld.wallet.connect('eternl');
  });

  async function setupBlockfrost(e: Event) {
    e.preventDefault();

    lucid = await Lucid(
      new Blockfrost(
        'https://cardano-preprod.blockfrost.io/api/v0',
        blockfrostAPIKey
      ),
      'Preprod'
    );

    const cache = localStorage.getItem('cache');

    if (cache) {
      const localCache: LocalCache = JSON.parse(cache);

      tokenName = localCache.tokenName;
      giftADA = localCache.giftADA;
      parameterizedContracts = localCache.parameterizedValidators;
      lockTxHash = localCache.lockTxHash;
    }

    // @ts-expect-error this is normal
    lucid.selectWallet.fromAPI(weld.wallet.handler!.enabledApi);
  }

  async function submitTokenName(e: Event) {
    e.preventDefault();

    const utxos = await lucid!.wallet().getUtxos()!;

    const utxo = utxos[0];
    const outputReference = {
      txHash: utxo.txHash,
      outputIndex: utxo.outputIndex
    };

    const contracts = applyParams(tokenName, outputReference, data.validator);

    parameterizedContracts = contracts;
  }

  async function createGiftCard(e: Event) {
    e.preventDefault();

    waitingLockTx = true;

    try {
      const lovelace = Number(giftADA) * 1000000;

      const assetName = `${parameterizedContracts!.policyId}${fromText(
        tokenName
      )}`;

      // Action::Mint
      const mintRedeemer = Data.to(new Constr(0, []));

      const utxos = await lucid!.wallet().getUtxos()!;
      const utxo = utxos[0];

      const tx = await lucid!
        .newTx()
        .collectFrom([utxo])
        .attach.MintingPolicy(parameterizedContracts!.giftCard)
        .mintAssets({ [assetName]: BigInt(1) }, mintRedeemer)
        .pay.ToContract(
          parameterizedContracts!.lockAddress,
          { kind: 'inline', value: Data.void() },
          { lovelace: BigInt(lovelace) }
        )
        .complete();

      const txSigned = await tx.sign.withWallet().complete();

      const txHash = await txSigned.submit();

      const success = await lucid!.awaitTx(txHash);

      // Wait a little bit longer so ExhaustedUTxOError doesn't happen
      // in the next Tx
      setTimeout(() => {
        waitingLockTx = false;

        if (success) {
          localStorage.setItem(
            'cache',
            JSON.stringify({
              tokenName,
              giftADA,
              parameterizedValidators: parameterizedContracts,
              lockTxHash: txHash
            })
          );

          lockTxHash = txHash;
        }
      }, 3000);
    } catch {
      waitingLockTx = false;
    }
  }

  async function redeemGiftCard(e: Event) {
    e.preventDefault();

    waitingUnlockTx = true;

    try {
      const utxos = await lucid!.utxosAt(parameterizedContracts!.lockAddress);

      const assetName = `${parameterizedContracts!.policyId}${fromText(
        tokenName
      )}`;

      // Action::Burn
      const burnRedeemer = Data.to(new Constr(1, []));

      const tx = await lucid!
        .newTx()
        .collectFrom(utxos, Data.void())
        .attach.MintingPolicy(parameterizedContracts!.giftCard)
        .attach.SpendingValidator(parameterizedContracts!.redeem)
        .mintAssets({ [assetName]: BigInt(-1) }, burnRedeemer)
        .complete();

      const txSigned = await tx.sign.withWallet().complete();

      const txHash = await txSigned.submit();

      const success = await lucid!.awaitTx(txHash);

      waitingUnlockTx = false;

      if (success) {
        localStorage.removeItem('cache');

        unlockTxHash = txHash;
      }
    } catch {
      waitingUnlockTx = false;
    }
  }
</script>

<svelte:head>
  <title>One Shot</title>
</svelte:head>

<div class="mx-auto mb-10 mt-20 max-w-2xl">
  <div class="mb-10">
    <h2 class="text-lg font-semibold text-gray-900">
      Make a one shot minting and lock contract
    </h2>

    balance: {displayedBalance}

    <h3 class="mb-2 mt-4">Gift Card Template</h3>
    <pre
      class="overflow-x-scroll rounded bg-gray-200 p-2">{data.validator}</pre>
  </div>

  <div>
    {#if !lucid}
      <form class="mt-10 grid grid-cols-1 gap-y-8" onsubmit={setupBlockfrost}>
        <Input
          type="password"
          id="blockfrostAPIKey"
          bind:value={blockfrostAPIKey}
        >
          Blockfrost API Key
        </Input>

        <Button type="submit">Setup Wallet</Button>
      </form>
    {:else}
      <form class="mt-10 grid grid-cols-1 gap-y-8" onsubmit={submitTokenName}>
        <Input
          type="text"
          name="tokenName"
          id="tokenName"
          bind:value={tokenName}>Token Name</Input
        >

        {#if tokenName.length > 0}
          <Button type="submit">Make Contracts</Button>
        {/if}
      </form>
    {/if}

    {#if lucid && parameterizedContracts}
      <h3 class="mb-2 mt-4">New Gift Card</h3>
      <pre
        class="overflow-x-scroll rounded bg-gray-200 p-2">{parameterizedContracts
          .redeem.script}</pre>

      <div class="mt-10 grid grid-cols-1 gap-y-8">
        <form onsubmit={createGiftCard}>
          <Input type="text" name="giftADA" id="giftADA" bind:value={giftADA}>
            ADA Amount
          </Input>

          <Button type="submit" disabled={waitingLockTx || !!lockTxHash}>
            {#if waitingLockTx}
              Waiting for Tx...
            {:else}
              Create Gift Card (Locks ADA)
            {/if}
          </Button>
        </form>

        {#if lockTxHash}
          <h3 class="mb-2 mt-4">ADA Locked</h3>

          <a
            class="mb-2"
            target="_blank"
            href={`https://preprod.cardanoscan.io/transaction/${lockTxHash}`}
          >
            {lockTxHash}
          </a>

          <form onsubmit={redeemGiftCard}>
            <Button type="submit" disabled={waitingLockTx || !!unlockTxHash}>
              {#if waitingUnlockTx}
                Waiting for Tx...
              {:else}
                Redeem Gift Card (Unlocks ADA)
              {/if}
            </Button>
          </form>
        {/if}

        {#if unlockTxHash}
          <h3 class="mb-2 mt-4">ADA Unlocked</h3>

          <a
            class="mb-2"
            target="_blank"
            href={`https://preprod.cardanoscan.io/transaction/${unlockTxHash}`}
          >
            {unlockTxHash}
          </a>
        {/if}
      </div>
    {/if}
  </div>
</div>
