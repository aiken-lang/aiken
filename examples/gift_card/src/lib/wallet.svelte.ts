import { createWeldInstance, type WeldConfig } from '@ada-anvil/weld';
import { getContext, setContext } from 'svelte';

export class Weld {
  weld = createWeldInstance();

  // Use the $state rune to create a reactive object for each Weld store
  config = $state(this.weld.config.getState());
  wallet = $state(this.weld.wallet.getState());
  extensions = $state(this.weld.extensions.getState());

  constructor(persist?: Partial<WeldConfig>) {
    this.weld.config.update({ updateInterval: 2000 });

    if (persist) this.weld.persist(persist);

    $effect(() => {
      this.weld.init();

      // Subscribe to Weld stores and update reactive objects when changse occur
      // Note: No need to use subscribeWithSelector as $state objects are deeply reactive
      this.weld.config.subscribe((s) => (this.config = s));
      this.weld.wallet.subscribe((s) => (this.wallet = s));
      this.weld.extensions.subscribe((s) => (this.extensions = s));

      return () => this.weld.cleanup();
    });
  }
}

// Use the context API to scope weld stores and prevent unwanted sharing
// of data between clients when rendering on the server
const weldKey = Symbol('weld');

export function setWeldContext(persist?: Partial<WeldConfig>) {
  const value = new Weld(persist);
  setContext(weldKey, value);
  return value;
}

export function getWeldContext() {
  return getContext<ReturnType<typeof setWeldContext>>(weldKey);
}
