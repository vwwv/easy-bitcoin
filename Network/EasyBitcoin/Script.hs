{-# LANGUAGE  DataKinds, ScopedTypeVariables #-}
module Network.EasyBitcoin.Script 
 where



import Network.EasyBitcoin.Internal.Script
import Network.EasyBitcoin.Keys
import Network.EasyBitcoin.Internal.Words
import Network.EasyBitcoin.Internal.ByteString
import Network.EasyBitcoin.Internal.Transaction
import Network.EasyBitcoin.Internal.Signatures
import Network.EasyBitcoin.Internal.Keys
import Data.Word
import qualified Data.ByteString as BS
import Data.List (nub) -- not to use nub!! TODO

import Network.EasyBitcoin.Internal.InstanciationHelpers
import Data.Binary (Binary, get, put)
import Data.Binary.Get ( getWord64be
                       , getWord32be
                       , getWord8
                       , getWord16le
                       , getWord32le
                       , getByteString
                       , Get
                       , isEmpty    
                       )
import Data.Binary.Put( putWord64be
                      , putWord32be
                      , putWord32le
                      , putWord16le
                      , putWord8
                      , putByteString
                      )
import Database.PostgreSQL.Simple.ToField
import Control.Monad                   (unless, guard,replicateM,forM_,liftM2)
import Control.Applicative((<$>))
import Network.EasyBitcoin.Address
import Network.EasyBitcoin.Internal.HashFunctions
import Network.EasyBitcoin.NetworkParams
import Network.EasyBitcoin.BitcoinUnits

import Data.Aeson



-- | Contains:
--    * number of requiered signatures.
--    * public keys allowed to use for signing.
data RedeemScript  net = RedeemScript Int [Key Public net]  deriving (Eq)


instance (BlockNetwork net) => Show (RedeemScript net) where
  show = showAsBinary

instance (BlockNetwork net) => Read (RedeemScript net) where
  readsPrec = readsPrecAsBinary


instance (BlockNetwork net) => ToJSON (RedeemScript net) where
  toJSON = toJSON.show



generalScript (RedeemScript minKeys keys) = Script $  [ op minKeys ] 
                                                   ++ [ opPushData $ serializeCompressedSingleton k | k  <- keys]
                                                   ++ [ op $ length keys, OP_CHECKMULTISIG]

interpret (Script (opMin : rest )) = case splitAt (length rest -2) rest of

                                       (rest_, [opMax,OP_CHECKMULTISIG]) -> do op_min     <- opNumber opMin
                                                                               op_max     <- opNumber opMax
                                                                               compressed <- mapM getPubKeys rest_
                                                                              
                                                                               if op_max == length rest_ &&  op_min <= op_max 
                                                                                
                                                                                   then return $ RedeemScript op_min compressed
                                                                                   else Nothing
                                                                                                 
                                       _              -> Nothing
     where
        getPubKeys scriptOp = do content          <- opContent scriptOp
                                 Compressed _ pub <- decodeToMaybe content
                                 return $ ExtendedPub 0 0 0 0 pub

instance Addressable RedeemScript where
    address = ScriptAddress . hash160 . hash256BS . encode' 

------------------------------------------------------------------------------------------------------------------------------------

encodeOutput :: Address net -> Script
encodeOutput addr = case addr of

                       PubKeyAddress payload -> Script [ OP_DUP
                                                       , OP_HASH160
                                                       , opPushData (encode' payload)
                                                       , OP_EQUALVERIFY
                                                       , OP_CHECKSIG
                                                       ]

                       ScriptAddress payload -> Script [ OP_HASH160
                                                       , opPushData (encode' payload)
                                                       , OP_EQUAL
                                                       ]


encodeOutput_ :: (BlockNetwork net) => RedeemScript net -> Script 
encodeOutput_ = generalScript

------------------------------------------------------------------------------------------------------------------------------------
encodeInputPayPKH :: TxSignature -> PubKey a  -> Script
encodeInputPayPKH ts p = Script $ [ opPushData$encode' ts, opPushData$encode' p]

------------------------------------------------------------------------------------------------------------------------------------


-- TODO: Serialization for keys being compressed or not!
decodeOutput :: forall net. BlockNetwork net => Script -> Maybe (Address net)
decodeOutput (Script script) = case script of 
    
                                [OP_DUP, OP_HASH160, OP_PUSHDATA payload _, OP_EQUALVERIFY, OP_CHECKSIG] 
                                   | Just content      <- decodeToMaybe payload                          -> Just $ PubKeyAddress content
                                
                                [OP_HASH160, OP_PUSHDATA payload _, OP_EQUAL]                            
                                   | Just content      <- decodeToMaybe payload                          -> Just $ ScriptAddress content 
                                _                                                                        -> Nothing




decodeInput :: forall net. BlockNetwork net => Script -> Maybe (Address net)
decodeInput  (Script script) = case script of 
                                [OP_PUSHDATA _ _ , OP_PUSHDATA content _]                 -> Just . PubKeyAddress . hash160 $ hash256BS content
 
                                OP__ 0 : rest       
                                   | (OP_PUSHDATA content _: _)  <- reverse rest
                                   , Just redeem                 <- decodeToMaybe content -> Just $address (redeem::RedeemScript net)
                                   
                                _  -> Nothing



-------------------------------------------------------------------------------------------------------------

buildTx_ :: [Outpoint] -> [(Address net,BTC net)] -> Tx net
buildTx_   xs ys = Tx 1 
                   [ TxIn  point (Script []) maxBound             | point        <- xs] -- TODO that maxBoundIsWrong!!!
                   [ TxOut (asSatoshis btc) (encodeOutput addr)   | (addr,btc)   <- ys] 
                   0

---------------------------------------------------------------------------------------------------------------
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






dncodeInputPayPKH :: Script -> Maybe (Key Public a,TxSignature)
dncodeInputPayPKH (Script script) = case script of 
                                     [OP_PUSHDATA payload1 _, OP_PUSHDATA payload2 _] 
                                      | Just sign <- decodeToMaybe payload1
                                      , Just pubk <- decodeToMaybe payload2           -> Just $ (ExtendedPub 0 0 0 0 pubk, sign) 
                                     _                                                -> Nothing

-------------------------------------------------------------------------------------------------------------
instance (BlockNetwork net) => Binary (RedeemScript net) where
    get = get >>= maybe (fail "This script is not an standard multisg redeem script") return . interpret 
    put = put . generalScript
