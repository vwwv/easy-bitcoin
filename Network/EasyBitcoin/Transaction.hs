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


-- | Sign an specific input of a transaction.
signTxAt :: (BlockNetwork net) =>  Tx net                   -- ^ Transaction to sign 
                               -> Outpoint                  -- ^ Reference the input within the transaction to be signed.
                               -> Maybe (RedeemScript net)  -- ^ If using to see multisig-escrow, this should contain the redeemScript defining that
                                                            --   escrow.
                               -> Key Private net           -- ^ Key to sign
                               -> Tx net
signTxAt tx out redeem_ key  = let signa = createSignature tx out redeem_ key :: TxSignature 
                               
                               in case redeem_ of
                                   -- change this branch for a more intuitive behaviour
                                   Just redeem -> tx & scriptSig out . escrowSignaturesFor redeem %~ (signa:) 

                                   Nothing     -> tx & scriptSig out .~ 
                                                      ( (signa, derivePublic key) ^. re simpleSignature :: ScriptSig)  

-- | Check an specific input of a transaction is fully signed, it understand both, Pay2PKH and Pay2SH for multisig-escrow. for other kinds of
--   transaction it will return always 'False'.
--
--   In case of multisig-escrow, it also check signatures use the right order defined on the RedeemScript.
--
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



-- | A Traversal focusing on the ScriptSig of a transaction at a particular input referenced by an 'Outpoint' 
--   Notice, a valid transaction will always have exactly 0 or 1 scriptSig for a given 'Outpoint'; invalid transactions
--   might have more than one.
scriptSig :: Outpoint ->  Traversal' (Tx net) ScriptSig -- Lens' (Tx net) ScriptSig 
scriptSig out f (Tx v inn txOuts lock) = let appF (TxIn out' script seq) 
                                               | out' == out = (\(ScriptSig script') -> TxIn out' script' seq) <$> f (ScriptSig script ) 
                                               | otherwise   = pure (TxIn out' script seq) 
                                          
                                          in (\x -> Tx v x txOuts lock ) <$> traverse appF inn  


-- | The prism successes when the scriptSig is either empty, partially or full signed escrow-multisig; unless it is empty, 
--   it will also require than the redeem used by the scriptSig is an specific one.
escrowSignaturesFor :: (BlockNetwork net) => RedeemScript net -> Prism' ScriptSig [TxSignature] 
escrowSignaturesFor redeem = prism (fromEscrowFor redeem) (toEscrowFor redeem) 


-- | The prism successes when the scriptSig is either empty, partially or full signed escrow-multisig.
escrowSignatures ::(BlockNetwork net) => Prism' ScriptSig ([TxSignature],Maybe (RedeemScript net))
escrowSignatures = prism fromEscrow toEscrow



-- | The prism successes when the scriptSig is from an already signed Pay2PKH, it does not check whether this signature is valid or not.
simpleSignature :: Prism' ScriptSig (TxSignature, Key Public net)  
simpleSignature = prism fromSimple toSimple
    where

      fromSimple (sig,key) = ScriptSig $ encodeInputPayPKH sig (pub_key key)

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





-- | Creates an specif type of signature for a transaction's input.
createSignatureAs ::(BlockNetwork net) => SigHash -> Tx net -> Outpoint -> Maybe (RedeemScript net) -> Key Private net ->  TxSignature
createSignatureAs sh tx out redeem_ key = let msg = createMessage_ sh tx out (maybe (Left key) Right redeem_)
                                           in TxSignature (detSignMsg msg key) sh


createMessage_ :: (BlockNetwork net) => SigHash 
                                     -> Tx net 
                                     -> Outpoint 
                                     -> Either (Key v net) (RedeemScript net) 
                                     ->  Word256
createMessage_ sh tx@(Tx _ inn _ _) out fromInn = txSigHash tx output i sh
     where
        output   = either (encodeOutput.address) generalScript fromInn  
        i = case [ i | (i, TxIn out' _ _) <- zip [0..] inn , out' == out] of 
                  [x] -> x 
                  _   -> 0

-- | Creates a "sig-all" signature of a transaction input.
createSignature :: (BlockNetwork net) => Tx net 
                                      -> Outpoint 
                                      -> Maybe (RedeemScript net) 
                                      -> Key Private net 
                                      ->  TxSignature
createSignature  = createSignatureAs (SigAll False) 



-- | Verify a signature for a transaction input was done using an specific key.
checkSignatureAt :: (BlockNetwork net) => Tx net                       -- ^ Transaction to verify.
                                       -> Outpoint                     -- ^ Reference the input within the transaction to be verified.
                                       -> Maybe (RedeemScript net)     -- ^ In case of multisig-escrow this should contain the RedeemScript.
                                       -> TxSignature                  -- ^ The signature to verify.
                                       -> Key v net                    -- ^ The signature's key. 
                                       -> Bool
checkSignatureAt tx out fromInn (TxSignature sig sh) key = let msg  = createMessage_ sh tx out $ maybe (Left key) Right fromInn
                                                            in checkSig msg sig (derivePublic key)


