<script lang="ts">
  import type { Snippet } from 'svelte';
  import type { HTMLInputAttributes } from 'svelte/elements';

  interface Props extends HTMLInputAttributes {
    children: Snippet<[]>;
  }

  let { id, children, value = $bindable(), ...props }: Props = $props();
</script>

<div>
  <label for={id} class="mb-3 block text-sm font-medium text-gray-700">
    {@render children()}
  </label>
  <input
    bind:value
    {...props}
    {id}
    class="block w-full appearance-none rounded-md border border-gray-200 bg-gray-50 px-3 py-2 text-gray-900 placeholder-gray-400 focus:border-blue-500 focus:bg-white focus:outline-none focus:ring-blue-500 sm:text-sm"
  />
</div>
