<script lang="ts">
  import '../app.css';

  import { setWeldContext } from '$lib/wallet.svelte';

  let { children } = $props();

  setWeldContext({ enablePersistence: true });
</script>

{@render children()}
