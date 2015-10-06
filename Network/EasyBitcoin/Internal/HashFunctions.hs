{-# LANGUAGE PackageImports #-}
module Network.EasyBitcoin.Internal.HashFunctions 
 where


import "cryptohash" Crypto.Hash  ( Digest
                    , SHA512
                    , SHA256
                    , SHA1
                    , RIPEMD160
                    , hash
                    )

import "cryptohash" Crypto.MAC.HMAC (hmac)
import Data.Word (Word16, Word32)
import Data.Byteable (toBytes)
import Data.Binary (Binary, get)
import Data.Binary.Get (getWord32le)

import Data.Bits    ( shiftL
                    , shiftR
                    , rotateL
                    , xor
                    , (.&.), (.|.)
                    )

import qualified Data.ByteString as BS  ( ByteString
                                        , null
                                        , append
                                        , cons
                                        , concat
                                        , take
                                        , empty
                                        , length
                                        , replicate
                                        , drop
                                        )

import Network.EasyBitcoin.Internal.Words
import Network.EasyBitcoin.Internal.Serialization.ByteString
import Data.Word 
import qualified Data.ByteString as BS




type CheckSum32      = Word32
type WorkingState    = (BS.ByteString, BS.ByteString, Word16)
type AdditionalInput = BS.ByteString
type ProvidedData    = BS.ByteString
type EntropyInput    = BS.ByteString
type Nonce           = BS.ByteString
type PersString      = BS.ByteString





hmac512 :: BS.ByteString -> BS.ByteString -> Word512
hmac512 key = decode' . (hmac512BS key)

hash160 :: BS.ByteString -> Word160
hash160 bs = runGet' get (run160 bs)

run160 :: BS.ByteString -> BS.ByteString
run160 = (toBytes :: Digest RIPEMD160 -> BS.ByteString) . hash

run256 :: BS.ByteString -> BS.ByteString
run256 = (toBytes :: Digest SHA256 -> BS.ByteString) . hash

run512 :: BS.ByteString -> BS.ByteString
run512 = (toBytes :: Digest SHA512 -> BS.ByteString) . hash
-- | Computes SHA-512 and returns the result as a bytestring.
hash512BS :: BS.ByteString -> BS.ByteString
hash512BS bs = run512 bs

-- | Computes HMAC over SHA-512 and return the result as a bytestring.
hmac512BS :: BS.ByteString -> BS.ByteString -> BS.ByteString
hmac512BS key msg = hmac hash512BS 128 key msg





doubleHash256 :: BS.ByteString -> Word256
doubleHash256 bs = runGet' get (run256 $ run256 bs)



-- | Computes SHA-256 and returns the result as a bytestring.
hash256BS :: BS.ByteString -> BS.ByteString
hash256BS bs =  run256 bs



hmacDRBGNew :: EntropyInput -> Nonce -> PersString -> WorkingState
hmacDRBGNew seed nonce info | (BS.length seed + BS.length nonce) * 8 < 384  = error $ "Entropy + nonce input length must be at least 384 bit"
                            | (BS.length seed + BS.length nonce) * 8 > 1000 = error $ "Entropy + nonce input length can not be greater than 1000 bit"
                            | BS.length info * 8 > 256                      = error $ "Maximum personalization string length is 256 bit"
                            | otherwise                                     = (k1,v1,1) -- 10.1.2.3.6
             where
                s       = BS.concat [seed, nonce, info]  -- 10.1.2.3.1
                k0      = BS.replicate 32 0              -- 10.1.2.3.2
                v0      = BS.replicate 32 1              -- 10.1.2.3.3
                (k1,v1) = hmacDRBGUpd s k0 v0            -- 10.1.2.3.4



hmacDRBGUpd :: ProvidedData -> BS.ByteString -> BS.ByteString -> (BS.ByteString, BS.ByteString)
hmacDRBGUpd info k0 v0 | BS.null info = (k1,v1)               -- 10.1.2.2.3
                       | otherwise    = (k2,v2)               -- 10.1.2.2.6
     where
        k1 = hmac256BS k0 $ v0 `BS.append` (0 `BS.cons` info) -- 10.1.2.2.1
        v1 = hmac256BS k1 v0                                  -- 10.1.2.2.2
        k2 = hmac256BS k1 $ v1 `BS.append` (1 `BS.cons` info) -- 10.1.2.2.4
        v2 = hmac256BS k2 v1                                  -- 10.1.2.2.5


-- | Computes HMAC over SHA-256 and return the result as a bytestring.
hmac256BS :: BS.ByteString -> BS.ByteString -> BS.ByteString
hmac256BS key msg = hmac hash256BS 64 key msg



hmacDRBGGen :: WorkingState -> Word16 -> AdditionalInput -> (WorkingState, Maybe BS.ByteString)
hmacDRBGGen (k0,v0,c0) bytes info | bytes * 8 > 7500 = error "Maximum bits per request is 7500"
                                  | c0 > 10000       = ((k0,v0,c0), Nothing) -- 10.1.2.5.1
                                  | otherwise        = ((k2,v3,c1), Just res) -- 10.1.2.5.8
      where
       (k1,v1) | BS.null info = (k0,v0)
               | otherwise = hmacDRBGUpd info k0 v0 -- 10.1.2.5.2

       (tmp,v2) = go (fromIntegral bytes) k1 v1 BS.empty -- 10.1.2.5.3/4

       res      = BS.take (fromIntegral bytes) tmp -- 10.1.2.5.5
       
       (k2,v3)  = hmacDRBGUpd info k1 v2 -- 10.1.2.5.6

       c1       = c0 + 1 -- 10.1.2.5.7
       
       go l k v acc | BS.length acc >= l = (acc,v)
                    
                    | otherwise = let vn = hmac256BS k v
                                   in go l k vn (acc `BS.append` vn)


chksum32 :: BS.ByteString -> CheckSum32
chksum32 bs = fromIntegral $ (doubleHash256 bs) `shiftR` 224
