

{-# LANGUAGE DataKinds #-}

import Network.EasyBitcoin
import Control.Lens



{- | Using mulsignature escrows, step by step how to create and collaboratively sign transactions from a mulstiginature escrow:

     1. So, Alice and Bob have together a business, they want to store the profit somewhere it can only be moved if both of them agrees

     2. After a while, both of them decide to retrieve all their their funds back to their personal addresses in an specific amount.  

-}




alice_prv_key       = read "92CPvDya9qrURXEL7JcZFsATZXgwE5jWKjJPA5nHvrVoD2JuMQV" :: Key Private TestNet 
alice_pub_key       = derivePublic alice_prv_key
alice_personal_addr = read "miHFo3HWc2HhCL5GYcfJeimekcJqttm7hR"                  :: Address TestNet

bob_prv_key         = read "9347FdKFqEgi9Ud4RDAUNQHUkmjuHmQNR31o47nePBrvA7utm33" :: Key Private TestNet
bob_pub_key         = derivePublic bob_prv_key
bob_personal_addr   = read "mwy5HWCeMEdRosujdRYw1WzjyPnHf7wLw5"                  :: Address TestNet

----------------------------------------------------------------------------------




-- Bob and Alice interchange their addresses to combine them on a script redeem, they have to agree the same order for the key list, otherwise they'll
-- end up with different escrows addresses.


redeemScript   = RedeemScript 2 [alice_pub_key, bob_pub_key]
escrow_address = address redeemScript




-----------------------------------------------------------------------------------
-- | Checking on the BlockChain, they find somebody has sent them 1mBTC,
--   they decide to split it into 2 pieces and send them to their own personal addresses.
--
--   They agree to create a transaction sending 0.0004 to Alice, 0.0004 to Bob and the
--   remaining 0.0002 as miner fees.
--

utxo     = Outpoint (read "4b085c9b36c56d31e65bb2152a119ad0b2eb73f2cbb4df4647faf3bf67d02541") 1  -- contains 0.001 btc

unsigned = unsignedTransaction [utxo] [(alice_personal_addr, btc 0.0004), (bob_personal_addr,btc 0.0004)]


-- now they both have to sign it. First, Alice sign it
--

signed_by_alice = signTxAt utxo (Just redeemScript) alice_prv_key   unsigned


-- now she sends her signed transaction to bob so he can sign it, but Bob does not trust her, so before signing it, he checks, what Alice sent was actually
-- the transaction suppossed to be:

valid_alice_tx =  (txInputs signed_by_alice  == [utxo]) 
               && (txOutputs signed_by_alice == [(Just alice_personal_addr, btc 0.0004), (Just bob_personal_addr,btc 0.0004)])


-- That means she is not cheating missleading us to sign something else, like a transaction where she obtains more than what was agreed.
-- But still it could be, that she miss-sign it; we have to check that option as well before continue.

valid_alice_signature = case signed_by_alice ^? scriptSig utxo . escrowSignaturesFor redeemScript of
                          Just [sign]
                            | checkSignatureAt signed_by_alice utxo (Just redeemScript) sign alice_pub_key -> True

                          _                                                                                -> False

-- As everything is ok, Bob signs as well the transaction:

signed_by_alice_then_bob = signTxAt utxo (Just redeemScript) bob_prv_key signed_by_alice


-- So far so good :) ....but it is not working, the btc client only accepts signatures if the were signed on the same order as the signature were defined on
-- the redeem_script!! That is, if the redeem script was (RedeemScript n [sigA,sigB,sigC,sigD]), first we need to sign with D, then C, then B and so on...
--
-- We'll need to reverse the signatures, fortunately, the scriptSig is not used when signed a transaction, therefore we can modify it keeping any signature
-- still valid.

signed_by_bob_then_alice = signed_by_alice_then_bob & scriptSig utxo . escrowSignaturesFor redeemScript %~ reverse 



--
-- Aaaand, we are done! The transaction is ready to be broadcasted! 
--

main = print signed_by_bob_then_alice




-------------------------

transaction_ = transaction [(utxo,alice_prv_key)] (address alice_prv_key, btc 200.30) []
unsigned_    = unsignedTransaction [utxo] [(address alice_prv_key, btc 200.30)]

transaction_2 = signTxAt utxo Nothing alice_prv_key unsigned_

-- 01000000014125d067bff3fa4746dfb4cbf273ebb2d09a112a15b25be6316dc5369b5c084b010000006b483045022100f24e1ac2c2816fe29a16a2aca8f9c22c6f0bf66a728cef0586b128a462aaa610022022957ee2fd135ea60e0033a159c73adb27e2685f4eb8e925b910db3a8e5ae7cb01210250232d54ed7b5d6aeb43fc465247b86a949b35c69872d826803e5a894a6a3236ffffffff01808be1a9040000001976a914a13e2a5b5a91f2e024a0a32939af2f974333fc6188ac00000000






