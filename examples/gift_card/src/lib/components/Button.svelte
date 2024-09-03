<script lang="ts">
  import type { Snippet } from 'svelte';
  import type { HTMLButtonAttributes } from 'svelte/elements';

  interface Props extends HTMLButtonAttributes {
    children: Snippet;
  }

  let { children, ...props }: Props = $props();
</script>

<button
  {...props}
  class="group inline-flex items-center justify-center rounded-full bg-blue-600 px-4 py-2 text-sm font-semibold text-white hover:bg-blue-500 focus:outline-none active:bg-blue-800 active:text-blue-100"
  >{@render children()}</button
>
