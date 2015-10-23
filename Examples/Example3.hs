

{-# LANGUAGE DataKinds #-}

import Network.EasyBitcoin




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


-- now they both have to sign it on the proper order, that is, first Bob, then Alic (it would be possible to process otherwise, but
-- that would require to reorder the signatures by the last participant).
--


signed_by_bob  = signTxAt utxo (Just redeemScript) bob_prv_key   unsigned
signed_by_both = signTxAt utxo (Just redeemScript) alice_prv_key signed_by_bob



------------------------------------------------
-- alicePubKey: 0250232d54ed7b5d6aeb43fc465247b86a949b35c69872d826803e5a894a6a3236 
-- bobPubKey:   02b0903900f4b9c30e27ecf65aeb49abaae8fe9dab7dec132e6f58fc74d862e284 

-- scriptPubKey a914cc3e2b86e7974a303a34ed01c5270c016752d64287

{-

redeem:
  client : 52210250232d54ed7b5d6aeb43fc465247b86a949b35c69872d826803e5a894a6a32362102b0903900f4b9c30e27ecf65aeb49abaae8fe9dab7dec132e6f58fc74d862e28452ae
           2NBsAK4pDvbEuusQhQBj6Gs1P2DiV6UFUJV

  easyF  : 52210250232d54ed7b5d6aeb43fc465247b86a949b35c69872d826803e5a894a6a32362102b0903900f4b9c30e27ecf65aeb49abaae8fe9dab7dec132e6f58fc74d862e28452ae
           2NBsAK4pDvbEuusQhQBj6Gs1P2DiV6UFUJV

unsigned:
  client : 01000000014125d067bff3fa4746dfb4cbf273ebb2d09a112a15b25be6316dc5369b5c084b0100000000ffffffff02409c0000000000001976a9141e512975ac69a61cbba37f61034a1d821033dc5f88ac409c0000000000001976a914b472a9b6796928ab809b2eaba68e1760c7d94ed088ac00000000

  easyF  : 01000000014125d067bff3fa4746dfb4cbf273ebb2d09a112a15b25be6316dc5369b5c084b0100000000ffffffff02409c0000000000001976a9141e512975ac69a61cbba37f61034a1d821033dc5f88ac409c0000000000001976a914b472a9b6796928ab809b2eaba68e1760c7d94ed088ac00000000

signed Bob
  client : 01000000014125d067bff3fa4746dfb4cbf273ebb2d09a112a15b25be6316dc5369b5c084b0100000049004752210250232d54ed7b5d6aeb43fc465247b86a949b35c69872d826803e5a894a6a32362102b0903900f4b9c30e27ecf65aeb49abaae8fe9dab7dec132e6f58fc74d862e28452aeffffffff02409c0000000000001976a9141e512975ac69a61cbba37f61034a1d821033dc5f88ac409c0000000000001976a914b472a9b6796928ab809b2eaba68e1760c7d94ed088ac00000000

  easyF  : 01000000014125d067bff3fa4746dfb4cbf273ebb2d09a112a15b25be6316dc5369b5c084b01000000910047304402202542b46bc0cc9b23cfac4e6a4cb31ea7a8e3f5f8303846948fe0d64762812c47022007b9a042af47a022d90714e7538e59dbf468d37ffb99749706f2888c9b8852fd014752210250232d54ed7b5d6aeb43fc465247b86a949b35c69872d826803e5a894a6a32362102b0903900f4b9c30e27ecf65aeb49abaae8fe9dab7dec132e6f58fc74d862e28452aeffffffff02409c0000000000001976a9141e512975ac69a61cbba37f61034a1d821033dc5f88ac409c0000000000001976a914b472a9b6796928ab809b2eaba68e1760c7d94ed088ac00000000


signed by Alice:




bitcoin-cli -testnet signrawtransaction 01000000014125d067bff3fa4746dfb4cbf273ebb2d09a112a15b25be6316dc5369b5c084b01000000d90047304402201e767acaf7a61e52a0eb9de805bfdb01c31753035b13ae6aeb7c96536f92013f02202269421b186378edc9eedfc7d066913ebd4a95f56642bd8fcad940ac059683c40147304402202542b46bc0cc9b23cfac4e6a4cb31ea7a8e3f5f8303846948fe0d64762812c47022007b9a042af47a022d90714e7538e59dbf468d37ffb99749706f2888c9b8852fd014752210250232d54ed7b5d6aeb43fc465247b86a949b35c69872d826803e5a894a6a32362102b0903900f4b9c30e27ecf65aeb49abaae8fe9dab7dec132e6f58fc74d862e28452aeffffffff02409c0000000000001976a9141e512975ac69a61cbba37f61034a1d821033dc5f88ac409c0000000000001976a914b472a9b6796928ab809b2eaba68e1760c7d94ed088ac00000000 '[{"txid":"4b085c9b36c56d31e65bb2152a119ad0b2eb73f2cbb4df4647faf3bf67d02541","vout":1 ,"scriptPubKey":"a914cc3e2b86e7974a303a34ed01c5270c016752d64287", "redeemScript":"52210250232d54ed7b5d6aeb43fc465247b86a949b35c69872d826803e5a894a6a32362102b0903900f4b9c30e27ecf65aeb49abaae8fe9dab7dec132e6f58fc74d862e28452ae" }]' '["9347FdKFqEgi9Ud4RDAUNQHUkmjuHmQNR31o47nePBrvA7utm33","92CPvDya9qrURXEL7JcZFsATZXgwE5jWKjJPA5nHvrVoD2JuMQV"]'



signed Both
  client : 01000000014125d067bff3fa4746dfb4cbf273ebb2d09a112a15b25be6316dc5369b5c084b010000004b0000004752210250232d54ed7b5d6aeb43fc465247b86a949b35c69872d826803e5a894a6a32362102b0903900f4b9c30e27ecf65aeb49abaae8fe9dab7dec132e6f58fc74d862e28452aeffffffff02409c0000000000001976a9141e512975ac69a61cbba37f61034a1d821033dc5f88ac409c0000000000001976a914b472a9b6796928ab809b2eaba68e1760c7d94ed088ac00000000
           01000000014125d067bff3fa4746dfb4cbf273ebb2d09a112a15b25be6316dc5369b5c084b010000004b0000004752210250232d54ed7b5d6aeb43fc465247b86a949b35c69872d826803e5a894a6a32362102b0903900f4b9c30e27ecf65aeb49abaae8fe9dab7dec132e6f58fc74d862e28452aeffffffff02409c0000000000001976a9141e512975ac69a61cbba37f61034a1d821033dc5f88ac409c0000000000001976a914b472a9b6796928ab809b2eaba68e1760c7d94ed088ac00000000
           01000000014125d067bff3fa4746dfb4cbf273ebb2d09a112a15b25be6316dc5369b5c084b01000000d90047304402201e767acaf7a61e52a0eb9de805bfdb01c31753035b13ae6aeb7c96536f92013f02202269421b186378edc9eedfc7d066913ebd4a95f56642bd8fcad940ac059683c40147304402202542b46bc0cc9b23cfac4e6a4cb31ea7a8e3f5f8303846948fe0d64762812c47022007b9a042af47a022d90714e7538e59dbf468d37ffb99749706f2888c9b8852fd014752210250232d54ed7b5d6aeb43fc465247b86a949b35c69872d826803e5a894a6a32362102b0903900f4b9c30e27ecf65aeb49abaae8fe9dab7dec132e6f58fc74d862e28452aeffffffff02409c0000000000001976a9141e512975ac69a61cbba37f61034a1d821033dc5f88ac409c0000000000001976a914b472a9b6796928ab809b2eaba68e1760c7d94ed088ac00000000


bitcoin-cli -testnet signrawtransaction 01000000014125d067bff3fa4746dfb4cbf273ebb2d09a112a15b25be6316dc5369b5c084b0100000049004752210250232d54ed7b5d6aeb43fc465247b86a949b35c69872d826803e5a894a6a32362102b0903900f4b9c30e27ecf65aeb49abaae8fe9dab7dec132e6f58fc74d862e28452aeffffffff02409c0000000000001976a9141e512975ac69a61cbba37f61034a1d821033dc5f88ac409c0000000000001976a914b472a9b6796928ab809b2eaba68e1760c7d94ed088ac00000000 '[{"txid":"4b085c9b36c56d31e65bb2152a119ad0b2eb73f2cbb4df4647faf3bf67d02541","vout":1 ,"scriptPubKey":"a914cc3e2b86e7974a303a34ed01c5270c016752d64287", "redeemScript":"52210250232d54ed7b5d6aeb43fc465247b86a949b35c69872d826803e5a894a6a32362102b0903900f4b9c30e27ecf65aeb49abaae8fe9dab7dec132e6f58fc74d862e28452ae" }]' '["92CPvDya9qrURXEL7JcZFsATZXgwE5jWKjJPA5nHvrVoD2JuMQV"]' 

-}


















