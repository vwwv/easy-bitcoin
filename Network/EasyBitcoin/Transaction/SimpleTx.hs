{-# LANGUAGE DataKinds, TypeFamilies #-}

module Network.EasyBitcoin.Transaction.SimpleTx where

import Network.EasyBitcoin.Internal.Serialization.ByteString
import Network.EasyBitcoin.Internal.Words
import Network.EasyBitcoin.Internal.Transaction
import Network.EasyBitcoin.Internal.Signatures
import Network.EasyBitcoin.BitcoinUnits
import Network.EasyBitcoin.Script
import Network.EasyBitcoin.NetworkParams
import Control.Applicative
import Data.Time.Clock.POSIX
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
           -> Tx
transaction  a b  = genericTx . simpleTx  a b

simpleTx :: (BlockNetwork net) 
         => [(Outpoint, Key Private net)]  -- ^ Transaction's inputs 
         -> (Address net,BTC net)          -- ^ Transaction's output
         -> [(Address net,BTC net)]        -- ^ Additional optional outputs
         -> SimpleTx net

simpleTx  ins x xs = let unsigned_tx = buildTx_  (map fst ins) (x:xs)
                       in SimpleTx [ (out,(derivePublic key,simpletTxSignature n key out unsigned_tx)) 
                                   | ((out,key),n) <- zip ins [0..]
                                   ] 
                                   (x:xs)


--class Transaction tx where
--  data TxError tx :: *
--  genericTx       :: tx -> Tx 
--  interpreted     :: Tx -> Either (TxError tx) tx

instance (BlockNetwork net) => Transaction (SimpleTx net) where
   
   data TxError (SimpleTx net) = SimpleTx_InvalidSignature
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

checkSigs :: (BlockNetwork net) =>  SimpleTx      net  -> Bool
checkSigs (SimpleTx   inns outs) = let unsigned_tx = buildTx_  (fst<$>inns) outs 

                                    in  and [ simpletTxSignatureCheck i pub out unsigned_tx sig
                                            | ((out,(pub,sig)),i) <- zip inns [0..]
                                            ]
-------------------------------------------------------------------------------------------------------------------------
-- Private Functions:


-- We should try to minimize going from private to public several times....
simpletTxSignature :: (BlockNetwork net) => Int -> Key Private net -> Outpoint -> Tx -> TxSignature
simpletTxSignature i key out tx = let sh      = SigAll False
                                      msg     = txSigHash tx (encodeOutput $ address key) i sh
                                   
                                   in TxSignature (detSignMsg msg $ prv_key key) sh
 

simpletTxSignatureCheck :: (BlockNetwork net) => Int -> Key Public net -> Outpoint -> Tx -> TxSignature -> Bool
simpletTxSignatureCheck i key out tx (TxSignature sig sh)
                         | SigAll False /= sh             = False
                         | otherwise                      = let msg = txSigHash tx (encodeOutput $ address key) i sh
                                                             in checkSig msg sig (pub_key key)

--                    in detSignTx rawTx ins

--buildTx_ :: [Outpoint] -> [(Address net,BTC net)] -> Tx
--buildTx_ xs ys = Tx 1 
--                    [ TxIn  point (Script []) maxBound             | point        <- xs] 
--                    [ TxOut (asSatoshis btc) (encodeOutput addr)   | (addr,btc)   <- ys] 
--                    0

