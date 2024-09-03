<script lang="ts">
  import { Blaze, Blockfrost, WebWallet } from '@blaze-cardano/sdk';

  // Components
  import Input from '$lib/components/Input.svelte';
  import Button from '$lib/components/Button.svelte';

  // Local Types
  import type { PageData } from './$types';

  // Props
  type Props = {
    data: PageData;
  };

  let { data }: Props = $props();

  // Page Local State
  let blaze: Blaze<Blockfrost, WebWallet> | undefined = $state();

  let blockfrostAPIKey = $state('');

  let tokenName = $state('');

  async function setupBlaze(e: Event) {
    e.preventDefault();

    const walletApi = await window.cardano?.eternl.enable();

    if (walletApi) {
      blaze = await Blaze.from(
        new Blockfrost({ network: 'cardano-preview', projectId: blockfrostAPIKey }),
        new WebWallet(walletApi)
      );
    }
  }

  function submitTokenName(e: Event) {
    e.preventDefault();

    console.log('TODO: apply params to raw validators');
  }
</script>

<svelte:head>
  <title>One Shot</title>
</svelte:head>

<div class="mx-auto mb-10 mt-20 max-w-2xl">
  <div class="mb-10">
    <h2 class="text-lg font-semibold text-gray-900">Make a one shot minting and lock contract</h2>

    <h3 class="mb-2 mt-4">Gift Card</h3>
    <pre class="overflow-x-scroll rounded bg-gray-200 p-2">{data.validator}</pre>
  </div>

  <div>
    {#if blaze}
      <form class="mt-10 grid grid-cols-1 gap-y-8" onsubmit={setupBlaze}>
        <Input type="password" id="blockfrostAPIKey" bind:value={blockfrostAPIKey}>
          Blockfrost API Key
        </Input>

        <Button type="submit">Setup Blaze</Button>
      </form>
    {:else}
      <form class="mt-10 grid grid-cols-1 gap-y-8" onsubmit={submitTokenName}>
        <Input type="text" name="tokenName" id="tokenName" bind:value={tokenName}>Token Name</Input>

        {#if tokenName.length > 0}
          <Button type="submit">Make Contracts</Button>
        {/if}
      </form>
    {/if}
  </div>
</div>
