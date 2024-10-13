import type { CIP30Interface } from '@blaze-cardano/wallet';

// See https://kit.svelte.dev/docs/types#app
// for information about these interfaces
declare global {
  namespace App {
    // interface Error {}
    // interface Locals {}
    // interface PageData {}
    // interface PageState {}
    // interface Platform {}
  }
}

type WalletOption = {
  name: string;
  icon: string;
  apiVersion: string;
  enable(): Promise<CIP30Interface>;
  isEnabled(): Promise<boolean>;
};

type Cardano = {
  [key: string]: WalletOption;
};

declare global {
  interface Window {
    cardano?: Cardano;
  }
}

export {};
