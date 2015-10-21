{-# LANGUAGE OverloadedStrings #-}



module Network.EasyBitcoin.Internal.Base58 
 ( encodeBase58
 , decodeBase58
 , addRedundancy
 , liftRedundacy
 )
 where

import qualified Data.ByteString as BS
import Data.Char (ord, chr)
import Data.Word (Word8)
import Data.Maybe (fromJust, isJust, listToMaybe)
import Numeric (showIntAtBase, readInt)
import Data.String (fromString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.Text as T
import Control.Applicative
import Data.Bits
import Data.List(unfoldr)

import Numeric (showIntAtBase, readInt)
import Network.EasyBitcoin.Internal.HashFunctions
import Network.EasyBitcoin.Internal.ByteString




-------------------------------------------------------------------------------
-----------------------------------------------------




addRedundancy :: BS.ByteString -> BS.ByteString 
addRedundancy bs = BS.append bs (encode' $ chksum32 bs)


liftRedundacy :: BS.ByteString -> Maybe BS.ByteString
liftRedundacy bs = let (original,extra) = BS.splitAt (BS.length bs - 4) bs 

                    in if encode' (chksum32 original) == extra
                        
                        then Just original
                        else Nothing




encodeBase58::BS.ByteString -> String
encodeBase58 bs = l++r
     where   
        (z,b)         = BS.span (== 0) bs
        l             = replicate (BS.length z) '1'     -- preserve leading 0's
        
        r | BS.null b = ""
          | otherwise = encodeBase58I $ bsToInteger b

decodeBase58::String -> Maybe BS.ByteString
decodeBase58 str = r >>= return . (BS.append prefix)
   where
    (z,b)  = span (== '1') $ str
    prefix = BS.replicate (length z) 0                  -- preserve leading 1's

    r | null b    = Just BS.empty
      | otherwise = integerToBS <$> decodeBase58I  b


---------------------------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------------------------

b58Data :: BS.ByteString
b58Data = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"


b58 :: Word8 -> Word8
b58 i = BS.index b58Data (fromIntegral i)

b58' :: Word8 -> Maybe Word8
b58' w = fromIntegral <$> BS.elemIndex w b58Data


encodeBase58I :: Integer -> String
encodeBase58I i = showIntAtBase (58 :: Integer) f (fromIntegral i) ""
    where
        f = chr . fromIntegral . b58 . fromIntegral

decodeBase58I :: String -> Maybe Integer
decodeBase58I s = case listToMaybe $ readInt 58 p f s of
                    Just (r,[]) -> Just r
                    _           -> Nothing
  where
    c  = b58' . fromIntegral . ord
    p  = isJust . c
    f  = fromIntegral . fromJust . c


------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------


-- | Transforms a string into a strict bytestring
stringToBS :: String -> BS.ByteString
stringToBS = B8.pack



-- | Transform a strict bytestring to a string
bsToString :: BS.ByteString -> String
bsToString = B8.unpack








