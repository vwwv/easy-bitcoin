{-# LANGUAGE TypeFamilies #-}

module Network.EasyBitcoin.Internal.Transaction 
 where


import Network.EasyBitcoin.Internal.Words
import Network.EasyBitcoin.Internal.Base58 ( encodeBase58
                                           , decodeBase58
                                           , addRedundancy
                                           , liftRedundacy
                                           )

import Network.EasyBitcoin.Internal.ByteString
import Network.EasyBitcoin.Internal.InstanciationHelpers
import Network.EasyBitcoin.Internal.Signatures
import Network.EasyBitcoin.Keys
import Network.EasyBitcoin.BitcoinUnits
import Network.EasyBitcoin.Address
import Network.EasyBitcoin.Internal.Script
import Network.EasyBitcoin.Internal.InstanciationHelpers
import Network.EasyBitcoin.Internal.HashFunctions
import Data.Bits                       (testBit, clearBit, setBit)
import Control.Applicative
import Control.Monad(replicateM,forM_)
import qualified Data.ByteString as BS
import Data.Char
import Data.Binary
import Data.Aeson(ToJSON(..),FromJSON(..))
import Data.Binary.Get ( getWord64be
                       , getWord32be
                       , getWord64le
                       , getWord8
                       , getWord16le
                       , getWord32le
                       , getByteString
                       , Get
                       )
import Data.Binary.Put( putWord64be
                      , putWord32be
                      , putWord32le
                      , putWord64le
                      , putWord16le
                      , putWord8
                      , putByteString
                      )

import Data.Maybe(fromMaybe)









 
--------------------------------------------------------------------------------
-- | Bitcoin transaction. Its 'Binary' instance follow the p2p bitcoin protocol format.
--  For its 'Read' and 'Show' instances it uses the hexadecimal char representation of its 'Binary' instance.
--
-- When parsed, only syntax validation is performanced, particulary, signature validation is not computed.

data Tx net = Tx   { txVersion      :: Int -- !Word32
                   , txIn           :: [TxIn]
                   , txOut          :: [TxOut]
                   , txLockTime     :: Int -- Either a b  -- Word32
                   } deriving (Eq)


data TxIn  = TxIn  { prevOutput     :: Outpoint -- | Reference the previous transaction output (hash + position)
                   , scriptInput    :: Script
                   , txInSequence   :: Int
                   } deriving (Show,Eq)

data TxOut = TxOut { outValue       :: Int -- Word64 -- change to ¿BTC?
                   , scriptOutput   :: Script
                   } deriving (Show,Eq)







instance Binary (Tx net) where
    
    get =   Tx <$> (fmap fromIntegral getWord32le)
               <*> (replicateList =<< get)
               <*> (replicateList =<< get)
               <*> (fmap fromIntegral getWord32le)
          
       where
         replicateList (VarInt c) = replicateM (fromIntegral c) get

    put (Tx v   is os l) = do putWord32le (fromIntegral v)
                              put $ VarInt $ fromIntegral $ length is
                              forM_ is put
                            
                              put $ VarInt $ fromIntegral $ length os
                            
                              forM_ os put
                              putWord32le (fromIntegral l)




instance Binary TxOut where

   get = do val          <- getWord64le
            VarInt len   <- get
            raw_script   <- getByteString $ fromIntegral len
            
            case decodeToMaybe raw_script of

                Just script -> return$TxOut (fromIntegral val) script
                _           -> fail "could not decode the pub-script"  


   put (TxOut o s) = do putWord64le (fromIntegral o)
                        let s_ = encode' s
                        put $ VarInt $ BS.length s_
                        putByteString s_


instance Binary TxIn where

   get = do outpoint   <- get 
            VarInt len <- get
            raw_script <- getByteString $ fromIntegral len
            val        <- getWord32le

            case decodeToMaybe raw_script of

                Just script -> return$TxIn outpoint script (fromIntegral val)
                _           -> fail "could not decode the sig-script"


   put (TxIn o s q) = do  put o
                          let s_ =  encode' s
                          put $ VarInt $ BS.length s_
                          putByteString s_
                          putWord32le (fromIntegral q)



-- | Represents a reference to a transaction output, that is, a transaction hash ('Txid') plus the outvector number
--   of that output. 
data Outpoint          = Outpoint Txid Int deriving (Eq,Show,Ord,Read)

-- | A transaction identification as a hash of the transaction. 2 transaction are consider different if they have different
--   'Txid's. In some cases, it might be possible for a peer to modify a transaction into an equivalent one having a different
--   'Txid', for futher info read about the "transaction-malleability-issue".

--------------------------------------------------------------------------------------------------------------------------------------
data Txid              = Txid{ txidHash :: Word256} deriving (Eq,Ord)


txid:: Tx net ->  Txid
txid = Txid . fromIntegral . doubleHash256 . encode' 


instance Show (Txid) where
    show (Txid x) = bsToHex . BS.reverse  $ encode' x

instance Read Txid where

    readsPrec _ str = case readsPrec__ str of 
                        ( Just result, rest) -> [(result,rest)]
                        _                    -> []

     where
      readsPrec__ str       = let (word , rest) = span (not.isSpace)$ dropWhile isSpace str
                               in (fmap Txid . decodeToMaybe . BS.reverse =<< hexToBS word ,rest)


instance Binary Txid where
    get = Txid <$> get
    put = put . txidHash 


instance ToJSON Txid where
    toJSON = toJSON.show


instance ToJSON (Tx net) where
    toJSON = toJSON.show




instance Binary Outpoint where

    get                = Outpoint <$> get <*> (fromIntegral<$>getWord32le)

    put (Outpoint h i) = put h >> putWord32le (fromIntegral i)


instance Show (Tx net) where
    show = showAsBinary

instance Read (Tx net) where
    readsPrec = readsPrecAsBinary
---------------------------------------------------------------------------------------------------------------------------------------


--------------------------------------------------------------------------------

---------------------------------------------------------------------------------
-- Todo, create here the function "signatureOfTransaction..." so it does not need to export txSigHash




txSigHash :: Tx net          -- ^ Transaction to sign.
          -> Script          -- ^ Output script that is being spent.
          -> Int             -- ^ Index of the input that is being signed.
          -> SigHash         -- ^ What parts of the transaction should be signed.
          -> Word256         -- ^ Result hash to be signed.

txSigHash tx out i sh = do let newIn = buildInputs (txIn tx) out i sh 
                           fromMaybe (setBit 0 248)                     -- When SigSingle and input index > outputs, then sign integer 1
                                 $ do newOut <- buildOutputs (txOut tx) i sh
                                      let newTx = tx{ txIn = newIn, txOut = newOut }
                                      return $ doubleHash256 $ encode' newTx `BS.append` encodeSigHash32 sh
                                      --error $ (bsToHex                       $ encode' newTx `BS.append` encodeSigHash32 sh)
                                      --        ++ "  ------------------------  " ++
                                      --        (bsToHex.encode'.doubleHash256 $ encode' newTx `BS.append` encodeSigHash32 sh)



-- Builds transaction inputs for computing SigHashes
buildInputs :: [TxIn] -> Script -> Int -> SigHash -> [TxIn]
buildInputs txins out i sh 
                | anyoneCanPay sh                = [ (txins !! i) { scriptInput =  out } ]
                | isSigAll sh || isSigUnknown sh = single
                | otherwise                      = map noSeq $ zip single [0..]
   where

        empty        = map (\ti -> ti{ scriptInput = Script [] }) txins
        single       = updateIndex i empty $ \ti -> ti{ scriptInput = out }
        noSeq (ti,j) 
         | i == j    = ti  
         | otherwise = ti{ txInSequence = 0 }


-- Build transaction outputs for computing SigHashes
buildOutputs :: [TxOut] -> Int -> SigHash -> Maybe [TxOut]
buildOutputs txos i sh
        | isSigAll sh || isSigUnknown sh = return txos
        | isSigNone sh                   = return []
        | i >= length txos               = Nothing
        | otherwise                      = return $ buffer ++ [txos !! i]

    where
      buffer = replicate i $ TxOut (-1) (Script [])

updateIndex::Int -> [a] -> (a->a) -> [a]
updateIndex i xs f
             | i < 0 || i >= length xs = xs
             | otherwise = l ++ (f h : r)
         where
           (l,h:r) = splitAt i xs

-- | Encodes a 'SigHash' to a 32 bit-long bytestring.
encodeSigHash32 :: SigHash -> BS.ByteString
encodeSigHash32 sh = encode' sh `BS.append` BS.pack [0,0,0]

------------------------------------------------------------------------------------------------------
data SigHash = SigAll     { anyoneCanPay :: !Bool }
             | SigNone    { anyoneCanPay :: !Bool }
             | SigSingle  { anyoneCanPay :: !Bool }
             | SigUnknown { anyoneCanPay :: !Bool
                          , getSigCode   :: !Word8
                          } 
             deriving (Eq, Show, Read)

data TxSignature = TxSignature { txSignature :: !Signature
                               , sigHashType :: !SigHash
                               } deriving (Eq, Show, Read)



instance Binary TxSignature where


    put (TxSignature sig sh) = put sig >> put sh 

    get = TxSignature <$> get <*> get

instance Binary SigHash where

   get = do w <- getWord8  
            let acp = testBit w 7
            return $ case clearBit w 7 of
                        1 -> SigAll acp
                        2 -> SigNone acp
                        3 -> SigSingle acp
                        _ -> SigUnknown acp w

   put sh = putWord8 $ case sh of
                        
                        SigAll    acp 
                          | acp        -> 0x81 
                          | otherwise  -> 0x01
                        
                        SigNone   acp  
                          | acp        -> 0x82 
                          | otherwise  -> 0x02
                        
                        SigSingle acp  
                          | acp        -> 0x83 
                          | otherwise  -> 0x03
                        
                        SigUnknown _ w -> w


-------------------------------------------------------------------------------------------------------
--  Returns True if the 'SigHash' has the value SigAll.
isSigAll :: SigHash -> Bool
isSigAll sh = case sh of
               SigAll _ -> True
               _        -> False


--  Returns True if the 'SigHash' has the value SigNone.
isSigNone :: SigHash -> Bool
isSigNone sh = case sh of
               SigNone _ -> True
               _         -> False


--  Returns True if the 'SigHash' has the value SigSingle.
isSigSingle :: SigHash -> Bool
isSigSingle sh = case sh of
               SigSingle _ -> True
               _           -> False


--  Returns True if the 'SigHash' has the value SigUnknown.
isSigUnknown :: SigHash -> Bool
isSigUnknown sh = case sh of
               SigUnknown _ _ -> True
               _              -> False
