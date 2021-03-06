{-# LANGUAGE ScopedTypeVariables #-}
module Network.EasyBitcoin.Address 
  where


import Network.EasyBitcoin.Internal.Words
import Network.EasyBitcoin.Internal.Base58 ( encodeBase58
                                           , decodeBase58
                                           , addRedundancy
                                           , liftRedundacy
                                           )

import Network.EasyBitcoin.Internal.ByteString
import Network.EasyBitcoin.Internal.InstanciationHelpers
import Network.EasyBitcoin.Internal.HashFunctions
import Network.EasyBitcoin.Internal.Keys (PrvKey(), PubKey(),Compressed(..))
import Network.EasyBitcoin.Keys
import Network.EasyBitcoin.NetworkParams
import qualified Data.ByteString as BS
import Data.Char(isSpace)
import Data.Word

-- | Bitcoin address, either Pay2PKH or Pay2SH
data Address       net = PubKeyAddress { getAddrHash :: Word160 }
                       | ScriptAddress { getAddrHash :: Word160 }
                       deriving (Eq, Ord)



-- | Values from where an address can be derived. Keys, are interpreted as compressed by default, if need to derive an address from
--   an uncompressed key, use 'addressFromUncompressed' instead.
class Addressable add where
   address :: (BlockNetwork net) => add net -> Address net  

-- | As addresses are obtained from public keys hashes, when deriving from a private key, it will first get derived to public 
--
-- prop> address key  = address (derivePublic key)
--  
-- Addresses derived from Keys will always be Pay2PKH addresses:
--
-- prop> isPay2PKH (address key) = True
instance Addressable (Key v) where
    address = PubKeyAddress . hash160 . hash256BS . encode' . Compressed True. pub_key . derivePublic
 

-- | Derive an address from a key as uncompressed.
addressFromUncompressed:: Key v net -> Address net
addressFromUncompressed = PubKeyAddress . hash160 . hash256BS . encode' . Compressed False . pub_key . derivePublic



-- | Address was derived from an script hash. Though these addresses can represent the hash of any script, only redeem
--   scripts for multi-signature are currently supported.
isPay2SH  :: Address net -> Bool
isPay2SH  addr = case addr of 
                  PubKeyAddress _ -> True
                  _               -> False

-- | Address was derived from a public key hash.
isPay2PKH :: Address net -> Bool
isPay2PKH addr = case addr of 
                  ScriptAddress _ -> True
                  _               -> False
---------------------------------------------------------------------------------------------------------------------------------
instance (BlockNetwork net ) => Show (Address net) where
    show = show_aux
      where
        show_aux :: forall net . (BlockNetwork net ) =>  Address net -> String
        show_aux addr = let params = (valuesOf :: Params net)
                         in case addr of
                              PubKeyAddress payload -> show_ (addrPrefix params) payload
                              ScriptAddress payload -> show_ (scriptPrefix params) payload

 
instance (BlockNetwork net ) => Read (Address net) where
    readsPrec _ = read_aux
      where
        read_aux :: forall net . (BlockNetwork net ) =>  ReadS (Address net)
        read_aux str = let params = (valuesOf :: Params net)
                        
                        in case readsPrec_ str of

                            ( Just (prefix, payload), rest) 
                                  | addrPrefix params   == prefix   -> [(PubKeyAddress payload, rest)]
                                  | scriptPrefix params == prefix   -> [(ScriptAddress payload, rest)]

                            _                                       -> []


----------------------------------------------------------------------------------------------------------------------------------










