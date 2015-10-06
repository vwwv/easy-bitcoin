module Network.EasyBitcoin.NetworkParams 
 ( -- * Network Parameters
   Params(..)
 , ProdNet
 
   -- ** Instances
 , TestNet
 , BlockNetwork(..)
 ) where

import Data.Word (Word8, Word32, Word64)


-- | Network parameters to adapt the library to work with different networks, such when using it for different alt-coins.

data  Params         net = Params { addrPrefix      :: Word8   -- ^ Prefix for base58 PubKey hash address
                                  , scriptPrefix    :: Word8   -- ^ Prefix for base58 script hash address
                                  , wifFormat       :: Word8   -- ^ Prefix for private key WIF format
                                  , extPubKeyPrefix :: Word32  -- ^ Prefix for extended public  keys (BIP0032)
                                  , extPrvKeyPrefix :: Word32  -- ^ Prefix for extended private keys (BIP0032)
                                  } deriving Show

-- | Original bitcoin network, where "real" bitcoin used on production system.
data ProdNet

-- | Bitcoin network for test, where "fake" bitcoins can be used to test systems.
data TestNet

class BlockNetwork net where
    valuesOf :: Params net 

instance BlockNetwork ProdNet where
    valuesOf = Params { addrPrefix      = 0x00 
                      , scriptPrefix    = 0x05
                      , wifFormat       = 0x80
                      , extPubKeyPrefix = 0x0488b21e
                      , extPrvKeyPrefix = 0x0488ade4
                      }

instance BlockNetwork TestNet where
    valuesOf = Params { addrPrefix      = 0x6F
                      , scriptPrefix    = 0xc4
                      , wifFormat       = 0xEF
                      , extPubKeyPrefix = 0x043587CF
                      , extPrvKeyPrefix = 0x04358394
                      }

---------------------------------------------------------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------------------------------------------------------


