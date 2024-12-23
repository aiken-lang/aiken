import { MeshWallet } from '@meshsdk/core';
import fs from 'node:fs';
 
const secret_key = MeshWallet.brew(true) as string;
 
fs.writeFileSync('me.sk', secret_key);
 
const wallet = new MeshWallet({
  networkId: 0,
  key: {
    type: 'root',
    bech32: secret_key,
  },
});
 
wallet.getUnusedAddresses().then(res => {
    fs.writeFileSync('me.addr', res[0]);
})
