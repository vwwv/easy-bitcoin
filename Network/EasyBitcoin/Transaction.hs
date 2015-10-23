{-# LANGUAGE DataKinds, TypeFamilies, RankNTypes #-}

module Network.EasyBitcoin.Transaction
    ( Outpoint (..)
    , Txid()
    , txid
    , Tx ()
    , transaction
    , unsignedTransaction
    , txOutputs
    , txInputs
    , checkInput

    -- * Escrows and Signatures:
    , RedeemScript(..)
    , ScriptSig()
    , TxSignature()
    , signTxAt
    , scriptSig
    , escrowSignatures
    , escrowSignaturesFor
    , simpleSignature                        
    , checkSignatureAt
    , createSignature
    , createSignatureAs
    , SigHash(..)
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
import Control.Lens
--------------------------------------------------------------------------------


-- | Creates a transaction ready to be broadcasted.
transaction::(BlockNetwork net) 
           => [(Outpoint, Key Private net)]  -- ^ Transaction's inputs 
           -> (Address net,BTC net)          -- ^ Transaction's output
           -> [(Address net,BTC net)]        -- ^ Additional optional outputs
           -> Tx net

transaction  ins x xs  = let unsigned_tx = unsignedTransaction  (map fst ins) (x:xs)
                           in Tx 1 
                                 [ TxIn out (encodeInputPayPKH signat (pub_key key') ) maxBound
                                 | ((out,key),n) <- zip ins [0..]
                                 , let key'   = derivePublic key
                                       signat = simpletTxSignature n key out unsigned_tx
                                 ] 
                                 [ TxOut (fromIntegral$asSatoshis btc) (encodeOutput addr) | (addr,btc) <- (x:xs)
                                 ]
                                 0    

   

-- | Returns those 'Outpoint's used by the transaction's inputs
txInputs::Tx net -> [Outpoint]
txInputs (Tx _ inns _ _) = [ out | TxIn out _ _ <- inns ]

txInputs_::Tx net -> [(Outpoint, ScriptSig )]
txInputs_ (Tx v inns outs lock) = [ (out, ScriptSig script) | TxIn out script seq <- inns ]


-- | Return's the amount spent for each transaction's output and its address in case it can be parsed (Pay2PKH or Pay2SH).
txOutputs::(BlockNetwork net) => Tx net -> [(Maybe (Address net), BTC net )]
txOutputs (Tx v inns outs lock) = [ (decodeOutput script, satoshis value )
                                  | TxOut value script <- outs
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


-- | Return a transaction without signatures so it can be signed later on or by other participants (in case of multisignature escrows).
unsignedTransaction :: [Outpoint] -> [(Address net,BTC net)] -> Tx net
unsignedTransaction xs ys = Tx 1 
                            [ TxIn  point (Script []) maxBound             | point        <- xs] 
                            [ TxOut (asSatoshis btc) (encodeOutput addr)   | (addr,btc)   <- ys] 
                            0

------------------------------------------------------------------------------------------------------------------------------


-- explain the order matter!
-- explain how signing with the wrong address erase, and with the worn redeem is ignored..also, the problem signing several times!
signTxAt :: (BlockNetwork net) => Outpoint -> Maybe (RedeemScript net) -> Key Private net  -> Tx net ->Tx net
signTxAt out redeem_ key tx = let signa = createSignature tx out redeem_ key :: TxSignature 
                               
                               in case redeem_ of
                                   -- change this branch for a more intuitive behaviour
                                   Just redeem -> tx & scriptSig out . escrowSignaturesFor redeem %~ (signa:) 

                                   Nothing     -> tx & scriptSig out .~ 
                                                      ( (signa, derivePublic key) ^. re simpleSignature :: ScriptSig)  


checkInput :: (BlockNetwork net) => Tx net -> Outpoint -> Address net ->  Bool
checkInput tx out addr = case  [ sig_script | (out',sig_script) <- txInputs_ tx] of

                          [script]



                           | Just (sig,key)                                   <- script ^? simpleSignature
                           , address key == addr
                           , checkSignatureAt tx out Nothing sig key          -> True
                      
                           | Just (sigs ,Just redeem@(RedeemScript n keys) )  <- script ^? escrowSignatures 
                           ,  address redeem == addr 
                           , n == length sigs
                           , check redeem sigs keys                           -> True

                          _                                                   -> False
   where
     check redeem sigs keys = go (reverse sigs) (reverse keys)
        where
          go []     _    = True
          go _      []   = False
          go (x:xs) keys = go xs . drop 1 $ dropWhile (not.checkSignatureAt tx out (Just redeem) x) keys 



-- Todo, move the one not mentioning transactions to the Script module!
escrowSignaturesFor :: (BlockNetwork net) => RedeemScript net -> Prism' ScriptSig [TxSignature] 
escrowSignaturesFor redeem = prism (fromEscrowFor redeem) (toEscrowFor redeem) 

simpleSignature :: Prism' ScriptSig (TxSignature, Key Public net)  
simpleSignature = prism fromSimple toSimple

escrowSignatures ::(BlockNetwork net) => Prism' ScriptSig ([TxSignature],Maybe (RedeemScript net))
escrowSignatures = prism fromEscrow toEscrow

fromSimple :: (TxSignature, Key Public net) -> ScriptSig
fromSimple (sig,key) = ScriptSig $ encodeInputPayPKH sig (pub_key key)

toSimple:: ScriptSig -> Either ScriptSig (TxSignature, Key Public net)
toSimple x@(ScriptSig script) = maybe (Left x) Right $ dncodeInputPayPKH script 




fromEscrow :: (BlockNetwork net) => ([TxSignature],Maybe (RedeemScript net)) -> ScriptSig
fromEscrow (sigs,Just redeem)  = ScriptSig . Script $  OP__ 0 : (opPushData . encode' <$> sigs) ++ [opPushData . encode' $ redeem  ]
                                                    
fromEscrow ([],Nothing)      = ScriptSig . Script $ []

-- This branch makes no sense, but we'll need to behave this
-- way to obey the prism laws!
fromEscrow (sigs,Nothing)      = ScriptSig . Script $  OP__ 0 : (opPushData . encode' <$> sigs)


toEscrow :: (BlockNetwork net) =>  ScriptSig -> Either ScriptSig ([TxSignature],Maybe (RedeemScript net))
toEscrow x@(ScriptSig (Script script)) = case script of
                                           OP__ 0 : rest 

                                              | (OP_PUSHDATA content _: signatures)     <- reverse rest
                                              , Just redeem@(RedeemScript n pks)        <- decodeToMaybe content 
                                              , all pushData signatures                                       

                                              , Just signed <- sequence
                                                                [ decodeToMaybe payload  
                                                                | OP_PUSHDATA payload _ <- signatures
                                                                ]                                                 -> Right (reverse signed,Just redeem)


                                              -- This branch makes no sense, but we'll need to behave this
                                              -- way to obey the prism laws!
                                              | all pushData rest                                       
                                              , Just signed <- sequence
                                                                [ decodeToMaybe payload  
                                                                | OP_PUSHDATA payload _ <- rest
                                                                ]                                                 -> Right (signed, Nothing)

                                           []                                                                     -> Right ([]    , Nothing)

                                           _                                                                      -> Left x

    where
            pushData (OP_PUSHDATA _ _) = True
            pushData _                 = False



fromEscrowFor :: (BlockNetwork net) =>  RedeemScript net -> [TxSignature] -> ScriptSig
fromEscrowFor redeem sigs = fromEscrow (sigs,Just redeem)


toEscrowFor :: (BlockNetwork net) =>  RedeemScript net -> ScriptSig -> Either ScriptSig [TxSignature]
toEscrowFor redeem  script = case toEscrow script of 
                               Right (sigs,Just redeem')
                                  | redeem == redeem'       -> Right sigs
                               
                               Right ([],Nothing)           -> Right []

                               Left script'                 -> Left script'



scriptSig :: Outpoint ->  Traversal' (Tx net) ScriptSig -- Lens' (Tx net) ScriptSig 
scriptSig out f (Tx v inn txOuts lock) = let appF (TxIn out' script seq) 
                                               | out' == out = (\(ScriptSig script') -> TxIn out' script' seq) <$> f (ScriptSig script ) 
                                               | otherwise   = pure (TxIn out' script seq) 
                                          
                                          in (\x -> Tx v x txOuts lock ) <$> traverse appF inn  




createSignatureAs ::(BlockNetwork net) => SigHash -> Tx net -> Outpoint -> Maybe (RedeemScript net) -> Key Private net ->  TxSignature
createSignatureAs sh tx out redeem_ key = let msg = createMessage_ sh tx out (maybe (Left key) Right redeem_)
                                           in TxSignature (detSignMsg msg key) sh

-- Here is the problem!
createMessage_ :: (BlockNetwork net) => SigHash -> Tx net -> Outpoint -> Either (Key v net) (RedeemScript net) ->  Word256
createMessage_ sh tx@(Tx _ inn _ _) out fromInn = txSigHash tx output i sh
     where
        output   = either (encodeOutput.address) generalScript fromInn  
        i = case [ i | (i, TxIn out' _ _) <- zip [0..] inn , out' == out] of 
                  [x] -> x 
                  _   -> 0

createSignature :: (BlockNetwork net) => Tx net -> Outpoint -> Maybe (RedeemScript net) -> Key Private net ->  TxSignature
createSignature  = createSignatureAs (SigAll False) 




checkSignatureAt :: (BlockNetwork net) =>  Tx net -> Outpoint ->  Maybe (RedeemScript net) -> TxSignature -> Key v net ->   Bool
checkSignatureAt tx out fromInn (TxSignature sig sh) key = let msg  = createMessage_ sh tx out $ maybe (Left key) Right fromInn
                                                            in checkSig msg sig (derivePublic key)



--txSigHash :: Tx              -- ^ Transaction to sign.
--          -> Script          -- ^ Output script that is being spent.
--          -> Int             -- ^ Index of the input that is being signed.
--          -> SigHash         -- ^ What parts of the transaction should be signed.
--          -> Word256         -- ^ Result hash to be signed.
