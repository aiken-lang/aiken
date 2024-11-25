import { sveltekit } from '@sveltejs/kit/vite';
import wasm from 'vite-plugin-wasm';
import topLevelAwait from 'vite-plugin-top-level-await';
import { defineConfig } from 'vite';

export default defineConfig({
  plugins: [sveltekit(), wasm(), topLevelAwait()],
  server: {
    fs: {
      // Allow serving files from one level up to the project root
      allow: ['plutus.json']
    }
  }
});
