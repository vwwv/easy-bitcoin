{-# LANGUAGE DataKinds,TypeFamilies #-}

module Network.EasyBitcoin.Transaction.MultisigTx where


import Network.EasyBitcoin.Internal.Transaction
import Network.EasyBitcoin.Internal.Serialization.ByteString
import Network.EasyBitcoin.Internal.Signatures
import Network.EasyBitcoin.Script
import Network.EasyBitcoin.NetworkParams
import Control.Applicative
import Data.Maybe(isNothing,isJust)
-- | A partially or tottaly signed transaction, with one input from and outpoint to a n-m-multisig escrow Pay2Sh address and one or more outputs
--   to Pay2SH and/or Pay2PH.

-- TODO: make explicit the order they should be signed!!

data MultisigTx    net = UnsignedMultisig                                                                     Outpoint [((Address net,BTC net))]
                       | PartiallySignedMult  Int [(Key Public net, Maybe (TxSignature) )] (RedeemScript net) Outpoint [((Address net,BTC net))]
                       deriving(Show)
-- data RedeemScript  net = RedeemScript Int [Key Public net]                
       

-- | Create a non-signed 'MultisigTx' and redeem script its associated redeem.

multisigTX        :: Int                                -- ^ number of signatures required 
                  -> [Key v net]                        -- ^ list of allow public keys (they're derived to public from private if requiered).
                  -> Outpoint                           -- ^ Transaction's input
                  -> (Address net, BTC net)             -- ^ Transaction's output
                  -> [(Address net,BTC net)]            -- ^ Additional optional outputs
                  -> (RedeemScript net, MultisigTx net)

multisigTX n keys out x xs = let public = derivePublic<$>keys

                              in ( RedeemScript     n  public
                                 , UnsignedMultisig out (x:xs) 
                                 )




--simpletTxSignatureCheck :: Int -> Key Public net -> Outpoint -> Tx -> TxSignature -> Bool
--simpletTxSignatureCheck i key out tx (TxSignature sig sh)
--                         | SigAll False /= sh             = False
--                         | otherwise                      = let msg = txSigHash tx (encode'. encodeOutput $ address key) i sh
--                                                             in checkSig key sig msg


-- | Combine the signatures of two 'MultisigTx' into one, verify both transactions are related to the same outpoint and redeem script and
--   if needed, sort the signatures (bitcoin clients only accept multisig transactions if they are signed using the same order as defined on
--   the redeem script).

--combineSignatures ::  RedeemScript net -> MultisigTx net -> MultisigTx net -> Either (TxError (MultisigTx net)) (MultisigTx net)
--combineSignatures redeem@(RedeemScript n _) mult1 mult2 
--                    | maybe True (==redeem) (extractRedeem mult1) = Left MultisigTx_DifferentRedeem
--                    | maybe True (==redeem) (extractRedeem mult2) = Left MultisigTx_DifferentRedeem
--                    | outputsFrom mult1 /= outputsFrom mult2      = Left MultisigTx_NotMatchEachOther
--                    | inputsFrom  mult1 /= inputsFrom  mult2      = Left MultisigTx_NotMatchEachOther

--                    | otherwise                                   = let  signed1 = case mult1 of 
--                                                                                     UnsignedMultisig    _ _     -> []
--                                                                                     PartiallySignedMult _ l _ _ -> l 

--                                                                         signed2 = case mult2 of                                                                        
--                                                                                     UnsignedMultisig    _ _     -> []
--                                                                                     PartiallySignedMult _ l _ _ -> l 

--                                                                         combine (k1,s1) (_,s2) = (k1, s1<|>s2)
                                                                        
--                                                                         inn_     = inputsFrom  mult1
--                                                                         out_     = outputsFrom mult1

--                                                                     in case (signed1,signed2) of 
--                                                                        ([],[])  -> Right $ UnsignedMultisig         inn_ out_
--                                                                        (l_,[])  -> Right $ PartiallySignedMult n l_ inn_ out_
--                                                                        ([],l_)  -> Right $ PartiallySignedMult n l_ inn_ out_
--                                                                        (l1,l2)  -> Right $ PartiallySignedMult n (zipWith combine l1 l2) inn_ out_
outputsFrom::MultisigTx net -> [(Address net,BTC net)]
outputsFrom (UnsignedMultisig          _ out)  = out
outputsFrom (PartiallySignedMult _ _ _ _ out)  = out


inputsFrom::MultisigTx net -> Outpoint
inputsFrom (UnsignedMultisig          inn _) = inn
inputsFrom (PartiallySignedMult _ _ _ inn _) = inn


-- | Extract the redeem script from the transaction if it has at least one signature already, or nothing otherwise.
extractRedeem :: MultisigTx net -> Maybe (RedeemScript net)
extractRedeem mult = case mult of 

                      UnsignedMultisig _ _                  -> Nothing
    
                      PartiallySignedMult n list redeem _ _ -> Just $ redeem



-- | Returns the public keys that have already being used to sign the transaction. They are interpreted as root keys.
signaturesUsed :: MultisigTx net -> [Key Public net]
signaturesUsed mult = case mult of 

                        UnsignedMultisig _ _             -> []

                        PartiallySignedMult _ list _ _ _ -> [ k | (k,Just _) <- list]


  


instance (BlockNetwork net) => Transaction (MultisigTx net) where
   
   data TxError (MultisigTx net) = MultisigTx_InvalidSignature
                                 | MultisigTx_MultisigTxPattern
                                 | MultisigTx_StrangeVersion
                                 | MultisigTx_UseLockTime
                                 | MultisigTx_NotMatchEachOther
                                 | MultisigTx_DifferentRedeem
                                 | MultisigTx_NotRelatedKey
                                 deriving Show

   genericTx  (UnsignedMultisig                  inn out) = buildTx_ [inn] out
   genericTx  (PartiallySignedMult n list redeem inn out) = (buildTx_ [inn] out)
                                                             { txIn = let ops = OP__ 0 : [opPushData $ encode' sig       |(k,Just sig) <- list]
                                                                                      ++ [opPushData . encode' $ redeem  ]
                                                                       

                                                                       in [TxIn inn (Script ops) maxBound] -- TODO that maxBoundIsWrong!!!
                                                             }

   interpreted tx = case tx of 
                     Tx v _ _ lock
                         | lock /= 0                               -> Left MultisigTx_UseLockTime
                         | v    /= 1                               -> Left MultisigTx_StrangeVersion

                     Tx _ [TxIn p (Script []) _] txout _
                         | Just toos        <- decodeOutput' txout -> Right $ UnsignedMultisig p toos

                     Tx _ [TxIn p script _] txout _
                         | Just toos        <- decodeOutput' txout      -- TODO: fail if sh is not the (SigAll False) !!!!!!!!!!!!
                         , Just (sigList,n,redeem) <- decodeMultSig tx script    

                                                                   -> Right $ PartiallySignedMult n sigList redeem p toos


                     _                                             -> Left $ MultisigTx_MultisigTxPattern


      where

        decodeOutput' txout = sequence
                            $ [ flip (,) (satoshis amount) <$> decodeOutput out 
                              | TxOut amount out <- txout 
                              ]



-- | Sign a 'MultisigTx' and verifies the signature is correct, the private keys is within the allowed on the redeem script, and
--   was not used before.
addSignature      :: (BlockNetwork net) => Key Private net -> RedeemScript net -> MultisigTx net -> Either (TxError (MultisigTx net)) (MultisigTx net)
addSignature key redeem@(RedeemScript n keys) mult = let pkey = deriveRoot $ derivePublic key 
                                                      in case mult of
                                                       
                                                       _ | notElem pkey keys                  -> Left MultisigTx_NotRelatedKey

                                                       UnsignedMultisig out toos              -> let siglist = [ if k == pkey     
                                                                                                                  then ( pkey
                                                                                                                       , Just . multSignature key redeem out toos
                                                                                                                              $ buildTx_ [out] toos
                                                                                                                       )
                                                                                                                  else (k   , Nothing)  
                                                                                                               | k <-keys
                                                                                                               ]

                                                                                                  in Right $ PartiallySignedMult n siglist redeem out toos 
                                                       
                                                       PartiallySignedMult n siglist redeem_ out toos 
                                                         | redeem_ /= redeem                  -> error $ "Unpaired redeems: " ++ show [redeem_, redeem]  
                                                                                                 -- Left MultisigTx_DifferentRedeem

                                                         | otherwise                          -> let siglist_ = [ if k == pkey && isNothing may     
                                                                                                                  then ( pkey
                                                                                                                       , Just . multSignature key redeem out toos
                                                                                                                              $ buildTx_ [out] toos
                                                                                                                       )
                                                                                                                  else (k   , may)

                                                                                                                | (k,may) <- siglist
                                                                                                                ]

                                                                                                  in Right $ PartiallySignedMult n siglist_ redeem out toos 


multSignature :: (BlockNetwork net) => Key Private net  -> RedeemScript net -> Outpoint -> [((Address net,BTC net))] ->Tx ->  TxSignature
multSignature key redeem out toos tx = let sh      = SigAll False
                                           msg     = txSigHash tx (encodeOutput_ redeem) 0 sh
                                   
                                       in TxSignature (detSignMsg msg $ prv_key key) sh
