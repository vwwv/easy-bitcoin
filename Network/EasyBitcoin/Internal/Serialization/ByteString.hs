module Network.EasyBitcoin.Internal.Serialization.ByteString
     ( module Network.EasyBitcoin.Internal.Serialization.ByteString
     , module Data.Binary.Get
     , module Data.Binary.Put 
     ) 
 where

import Data.Binary
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as C ( pack, unpack)
import Control.Monad (guard,(<=<))
import Control.Applicative
import Data.List (unfoldr)
import Data.Bits ((.|.), shiftL, shiftR)
import Data.Binary.Put  (Put, runPut)
import qualified Data.ByteString.Lazy  as BL
import Data.Binary.Get  ( Get, runGetOrFail, getByteString, ByteOffset, runGet)
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

encode' :: Binary a => a -> BS.ByteString
encode' = toStrictBS . encode



decode' :: Binary a => BS.ByteString -> a
decode' = decode . toLazyBS

-- ByteString helpers
-- | Transforms a lazy bytestring into a strict bytestring
toStrictBS :: BL.ByteString -> BS.ByteString
toStrictBS = BS.concat . BL.toChunks



decodeToMaybe :: Binary a => BS.ByteString -> Maybe a
decodeToMaybe bs = case decodeOrFail $ toLazyBS bs of
                    Left  (lbs,o,err)  -> Nothing
                    Right (lbs,o,res)  -> Just res


integerToBS :: Integer -> BS.ByteString
integerToBS 0 = BS.pack [0]
integerToBS i 
    | i > 0     = BS.pack $ reverse $ unfoldr f i
    | otherwise = error "integerToBS not defined for negative values"
  where 
    f 0 = Nothing
    f x = Just $ (fromInteger x :: Word8, x `shiftR` 8)

-- | Decode a big endian Integer from a bytestring
bsToInteger :: BS.ByteString -> Integer
bsToInteger = (foldr f 0) . reverse . BS.unpack
  where 
    f w n = (toInteger w) .|. shiftL n 8



bsToHex :: BS.ByteString -> String
bsToHex = C.unpack . B16.encode

hexToBS :: String -> Maybe BS.ByteString
hexToBS xs = guard (bad == BS.empty) >> return x
   where
    (x, bad) = B16.decode $ C.pack xs



--bsToString :: BS.ByteString -> String
--bsToString = C.unpack


-- | Strict version of @Data.Binary.runPut@
runPut' :: Put -> BS.ByteString
runPut' = toStrictBS . runPut




-- | Isolate a Data.Binary.Get monad for the next @Int@ bytes. Only the next
-- @Int@ bytes of the input bytestring will be available for the Get monad to
-- consume. This function will fail if the Get monad fails or some of the input
-- is not consumed.
isolate :: Binary a => Int -> Get a -> Get a
isolate i g = do 
                 bs <- getByteString i
                 case runGetOrFail' g bs of
                  Left (_, _, err)           -> fail err
                  Right (unconsumed, _, res)
                    | BS.null unconsumed     -> return res
                    | otherwise              -> fail "Isolate: unconsumed input"





-- | Strict version of @Data.Binary.runGet@
runGet' :: Binary a => Get a -> BS.ByteString -> a
runGet' m = runGet m . toLazyBS






newtype VarInt = VarInt { getVarInt :: Int } deriving (Eq, Show, Read)

instance Binary VarInt where

    get = VarInt <$> ( getWord8 >>= go )
       where
        go 0xff = fromIntegral <$> getWord64le
        go 0xfe = fromIntegral <$> getWord32le
        go 0xfd = fromIntegral <$> getWord16le
        go x    = fromIntegral <$> return x

    put (VarInt x)
        | x < 0xfd        = putWord8 (fromIntegral x)
        | x <= 0xffff     = putWord8 0xfd >> putWord16le (fromIntegral x)
        | x <= 0xffffffff = putWord8 0xfe >> putWord32le (fromIntegral x)
        | otherwise       = putWord8 0xff >> putWord64le (fromIntegral x)

----------------------------------------------------------------------------------------------
-- | Transforms a strict bytestring into a lazy bytestring
toLazyBS :: BS.ByteString -> BL.ByteString
toLazyBS bs = BL.fromChunks [bs]





runGetOrFail' :: Binary a => Get a -> BS.ByteString -> Either (BS.ByteString, ByteOffset, String) (BS.ByteString, ByteOffset, a)
runGetOrFail' m bs = case runGetOrFail m $ toLazyBS bs of
                        Left (lbs,o,err) -> Left (toStrictBS lbs,o,err)
                        Right (lbs,o,res) -> Right (toStrictBS lbs,o,res)

