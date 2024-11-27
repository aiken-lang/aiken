import {
  deserializeAddress,
  mConStr0,
  stringToHex,
} from "@meshsdk/core";
import { getScript, getTxBuilder, getUtxoByTxHash, wallet } from "./common";
 
async function main() {
  // get utxo, collateral and address from wallet
  const utxos = await wallet.getUtxos();
  const walletAddress = (await wallet.getUsedAddresses())[0];
  const collateral = (await wallet.getCollateral())[0];
 
  const { scriptCbor } = getScript();
 
  // hash of the public key of the wallet, to be used in the datum
  const signerHash = deserializeAddress(walletAddress).pubKeyHash;
  // redeemer value to unlock the funds
  const message = "Hello, World!";
 
  // get the utxo from the script address of the locked funds
  const txHashFromDesposit = process.argv[2];
  const scriptUtxo = await getUtxoByTxHash(txHashFromDesposit);
 
    // build transaction with MeshTxBuilder
  const txBuilder = getTxBuilder();
  await txBuilder
    .spendingPlutusScript("V3") // we used plutus v3
    .txIn( // spend the utxo from the script address
      scriptUtxo.input.txHash,
      scriptUtxo.input.outputIndex,
      scriptUtxo.output.amount,
      scriptUtxo.output.address
    )
    .txInScript(scriptCbor)
    .txInRedeemerValue(mConStr0([stringToHex(message)])) // provide the required redeemer value `Hello, World!`
    .txInDatumValue(mConStr0([signerHash])) // only the owner of the wallet can unlock the funds
    .requiredSignerHash(signerHash)
    .changeAddress(walletAddress)
    .txInCollateral(
      collateral.input.txHash,
      collateral.input.outputIndex,
      collateral.output.amount,
      collateral.output.address
    )
    .selectUtxosFrom(utxos)
    .setNetwork('preprod')
    .complete();
  const unsignedTx = txBuilder.txHex;
 
  const signedTx = await wallet.signTx(unsignedTx);
  const txHash = await wallet.submitTx(signedTx);
  console.log(`1 tADA unlocked from the contract at Tx ID: ${txHash}`);
}
 
main();

