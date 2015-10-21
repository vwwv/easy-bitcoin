{-# LANGUAGE DataKinds, TypeFamilies #-}

module Network.EasyBitcoin.Transaction
( transaction
, SimpleTxError(..)
, txInputs
, txOutputs
, checkSimple
, checkSigs
, unsignedTransaction
) where

import Network.EasyBitcoin.Internal.ByteString
import Network.EasyBitcoin.Internal.Words
import Network.EasyBitcoin.Internal.Transaction
import Network.EasyBitcoin.Internal.Signatures
import Network.EasyBitcoin.BitcoinUnits
import Network.EasyBitcoin.Script
import Network.EasyBitcoin.Internal.Script

import Network.EasyBitcoin.Address
import Network.EasyBitcoin.Keys
import Network.EasyBitcoin.NetworkParams
import Control.Applicative

--------------------------------------------------------------------------------
-- | A full signed, transaction with 1 or more, outpoints to Pay2PKH, inputs and 1 or more
--   outputs to Pay2SH and/or Pay2PH.
--
--   When importing 'SimpleTx's, signature verification is performanced, but
--   the bitcoin balance and the correct privatekey/outpoint relation is not verified
--   as it is not possible with the information a single transaction  contain.  
data SimpleTx      net = SimpleTx [(Outpoint, (Key Public net, TxSignature))] 
                                  [(Address net,BTC net)]
                         deriving Show

transaction::(BlockNetwork net) 
           => [(Outpoint, Key Private net)]  -- ^ Transaction's inputs 
           -> (Address net,BTC net)          -- ^ Transaction's output
           -> [(Address net,BTC net)]        -- ^ Additional optional outputs
           -> Tx net
transaction  a b  = genericTx . simpleTx  a b

simpleTx :: (BlockNetwork net) 
         => [(Outpoint, Key Private net)]  -- ^ Transaction's inputs 
         -> (Address net,BTC net)          -- ^ Transaction's output
         -> [(Address net,BTC net)]        -- ^ Additional optional outputs
         -> SimpleTx net

simpleTx  ins x xs = let unsigned_tx = unsignedTransaction  (map fst ins) (x:xs)
                       in SimpleTx [ (out,(derivePublic key,simpletTxSignature n key out unsigned_tx)) 
                                   | ((out,key),n) <- zip ins [0..]
                                   ] 
                                   (x:xs)


data SimpleTxError = SimpleTx_InvalidSignature
                   | SimpleTx_NotSimpleTxPattern
                   | SimpleTx_StrangeVersion
                   | SimpleTx_UseLockTime Int 
                   deriving Show
 
genericTx (SimpleTx   inns outs) = Tx 1  
                                     [ TxIn out (encodeInputPayPKH signat (pub_key key) ) maxBound | (out,(key,signat))<- inns]
                                     [ TxOut (fromIntegral$asSatoshis btc) (encodeOutput addr) | (addr,btc) <- outs]
                                     0

   
interpreted (Tx v inns outs lock)
    --  | lock /= 0                 = Left $ SimpleTx_UseLockTime lock
      
      | v    /= 1                 = Left SimpleTx_StrangeVersion
      
      | otherwise                 = let inns_ = sequence [ (,) out <$> dncodeInputPayPKH script 
                                                         | TxIn out script seq <- inns
                                                         ]
                                        outs_ = sequence [ flip (,) (satoshis value) <$> decodeOutput script
                                                         | TxOut value script <- outs
                                                         ] 

                                     in case ( inns_ , outs_ ) of 
                                             (Just inputs, Just outputs) -> if True -- later on check with production net... -- checkSigs $ SimpleTx inputs outputs
                                                                             then Right $  SimpleTx inputs outputs
                                                                             else error $ show (SimpleTx  inputs outputs)  -- Left  $  SimpleTx_InvalidSignature

                                             _                           -> let inns2 =          [ (,) out <$> (dncodeInputPayPKH script::Maybe (Key Public TestNet,TxSignature)) 
                                                                                                 | TxIn out script seq <- inns
                                                                                                 ]
                                                                                outs2 =          [ maybe (Left script) Right r 
                                                                                                 | TxOut value script <- outs
                                                                                                 , let r = flip (,) (satoshis value) <$> (decodeOutput script::Maybe (Address TestNet)) 
                                                                                                 ] 

                                                                             in Left SimpleTx_NotSimpleTxPattern -- error $ show ( inns2
                                                                                   --          , outs2
                                                                                   --          )
                                                                               -- 

txInputs::Tx net -> [Outpoint]
txInputs (Tx v inns outs lock) = [ out | TxIn out script seq <- inns ]

txOutputs::(BlockNetwork net) => Tx net -> [(Maybe (Address net), BTC net )]
txOutputs (Tx v inns outs lock) = [ (decodeOutput script, satoshis value )
                                  | TxOut value script <- outs
                                  ] 


checkSimple :: Tx net -> Maybe SimpleTxError
checkSimple (Tx v inns outs lock) 
  
--    | lock /= 0                 = Just $ SimpleTx_UseLockTime lock
      
      | v    /= 1                 = Just SimpleTx_StrangeVersion
      
      | otherwise                 = let inns_ = sequence [ (,) out <$> dncodeInputPayPKH script 
                                                         | TxIn out script seq <- inns
                                                         ]
                                        outs_ = sequence [ flip (,) (satoshis value) <$> (decodeOutput script :: Maybe (Address TestNet))
                                                         | TxOut value script <- outs
                                                         ] 

-- we use TestNet, cause actually transaction data is represented the same way no matter in which network is used.
                                     in case ( inns_ , outs_ ) of 
                                             (Just inputs, Just outputs) -> if checkSigs $ SimpleTx inputs outputs
                                                                             then Nothing
                                                                             else Just  $  SimpleTx_InvalidSignature

                                             _                           -> Just SimpleTx_NotSimpleTxPattern
    



checkSigs :: (BlockNetwork net) =>  SimpleTx      net  -> Bool
checkSigs (SimpleTx   inns outs) = let unsigned_tx = unsignedTransaction  (fst<$>inns) outs 

                                    in  and [ simpletTxSignatureCheck i pub out unsigned_tx sig
                                            | ((out,(pub,sig)),i) <- zip inns [0..]
                                            ]
-------------------------------------------------------------------------------------------------------------------------
-- Private Functions:


-- We should try to minimize going from private to public several times....
simpletTxSignature :: (BlockNetwork net) => Int -> Key Private net -> Outpoint -> Tx net -> TxSignature
simpletTxSignature i key out tx = let sh      = SigAll False
                                      msg     = txSigHash tx (encodeOutput $ address key) i sh
                                   
                                   in TxSignature (detSignMsg msg key) sh
 

simpletTxSignatureCheck :: (BlockNetwork net) => Int -> Key Public net -> Outpoint -> Tx net -> TxSignature -> Bool
simpletTxSignatureCheck i key out tx (TxSignature sig sh)
                         | SigAll False /= sh             = False
                         | otherwise                      = let msg = txSigHash tx (encodeOutput $ address key) i sh
                                                             in checkSig msg sig  key

--                    in detSignTx rawTx ins

 -- TODO that maxBoundIsWrong!!!
unsignedTransaction :: [Outpoint] -> [(Address net,BTC net)] -> Tx net
unsignedTransaction xs ys = Tx 1 
                            [ TxIn  point (Script []) maxBound             | point        <- xs] 
                            [ TxOut (asSatoshis btc) (encodeOutput addr)   | (addr,btc)   <- ys] 
                            0

------------------------------------------------------------------------------------------------------------------------------

decodeMultSig :: (BlockNetwork net) => Tx net -> Script -> Maybe ([(Key Public net,Maybe TxSignature)],Int,RedeemScript net)
decodeMultSig tx (Script script) = case script of
                                     (OP__ 0 : rest)
                                   
                                       | (OP_PUSHDATA content _: signatures)             <- reverse rest
                                   
                                       , Just redeem@(RedeemScript n pks)                <- decodeToMaybe content 
                                   
                                       , all pushData signatures                                       
                                       , let output_ = encodeOutput_  redeem
                                       
                                       , msg <- txSigHash tx output_  0 (SigAll False)

                                       , Just signed <- sequence [ decodeToMaybe payload 
                                                                 | OP_PUSHDATA payload _ <- signatures
                                                                 ]                          

                                       , sigList <- checkSig_ msg signed <$> pks      -> Just (sigList,n,redeem)
                                     _                                                -> Nothing
        where

            checkSig_ msg ss p = let solutions = [ ts
                                                 | ts@(TxSignature s _) <- ss 
                                                 , checkSig msg s p
                                                 ]

                                  in case solutions of
                                        ts:_  -> (p, Just ts)
                                        []    -> (p, Nothing)

            pushData (OP_PUSHDATA _ _) = True
            pushData _                 = False

