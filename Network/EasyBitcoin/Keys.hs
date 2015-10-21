{-# LANGUAGE DataKinds
           , KindSignatures
           , ScopedTypeVariables
           , GADTs
           , StandaloneDeriving
           , TypeFamilies
           , PolyKinds
           , FlexibleInstances 
           #-}

module Network.EasyBitcoin.Keys 
 ( Key(..)
 , Visibility(..)
 , derive
 , derivePublic
 , deriveHardened
 , deriveRoot
 , showAsCompressedSingletonKey
 , showAsUncompressedSingletonKey
 , serializeCompressedSingleton
 , serializeUncompressedSingleton
 , decodeCheckingCompression
 , (===)
 ) where


import qualified Data.ByteString as BS
import GHC.Word
import Data.Bits(shiftR,setBit)
import Data.Binary

import Network.EasyBitcoin.Internal.Keys (PrvKey(), PubKey(),derivePubKey_,xPrvID,xPubID,addPrvKeys_,addPubKeys_,Compressed(..))
import Network.EasyBitcoin.Internal.Words
import Network.EasyBitcoin.Internal.ByteString
import Network.EasyBitcoin.Internal.HashFunctions(hmac512)
import Network.EasyBitcoin.NetworkParams
import Network.EasyBitcoin.Internal.InstanciationHelpers
import Control.Monad
import Data.Binary.Put
import Data.Binary.Get
import Control.Applicative
type ChainCode    = Word256


-- | 'Key's represents public and private bitcoin keys. 
-- 
--   'Key's can be used either as singleton keys or as hierarchical deterministic keys
--   as defined on BIP0032. It supports compressed and uncompressed keys.
--   It has an additional phantom type to describe on which network this key is suppossed to be used .
--
--   Its 'Read' instance understands the WIF format for singleton private keys, the BIP0032 format for private and public hierarchical 
--   deterministic keys, and for singleton public keys it understands as hex characters representing a binary serialized OpenSSL public key.
--
--   When reading a singleton key, no matter compressed or umcompressed, it is interpreted internally actually as root hierarchical deterministic key.
--
--   Its 'Show' instance works always as defined on BIP0032, treating always them as hierarchical even in those cases where they were originally represented
--   as singleton. In case you need to show it as singleton, you can use the functions 'showAsSingletonKey' and 'showAsSingletonKeyUncompressed' from
--   module 'Network.EasyBitcoin..Keys'.

data Key (visibility::Visibility) net where

    ExtendedPrv :: { prv_depth      :: Word8
                   , prv_parent     :: Word32 
                   , prv_index      :: Word32
                   , prv_chain      :: ChainCode
                   , prv_key        :: PrvKey net
                   } -> Key Private net 
    
    ExtendedPub :: { pub_depth      :: Word8
                   , pub_parent     :: Word32 
                   , pub_index      :: Word32
                   , pub_chain      :: ChainCode
                   , pub_key        :: PubKey net
                   } -> Key Public  net

deriving instance Eq (Key v net)

-- | @'Key' 'Public' net@ represents public keys, @'Key' 'Private' net@ represents private keys.
data Visibility = Private 
                | Public

-- | Derives the n-th child of a key, keeping it public if the parent was public, otherwise private.
derive           :: Int -> Key v net -> Key v net
derive n key     = case key of 
                    r@(ExtendedPrv _ _ _ _ _) -> prvSubKey r (fromIntegral n)
                    r@(ExtendedPub _ _ _ _ _) -> pubSubKey r (fromIntegral n)

-- | Transform a private key into a public key, or does nothing if it was already a public key.
derivePublic     :: Key v net -> Key Public net
derivePublic  k  = case k of 
                     r@(ExtendedPub _ _ _ _ _) -> r 
                     ExtendedPrv d p i c k     -> ExtendedPub d p i c (derivePubKey_ k)

-- | Like 'derive' but derives a hardened child. Hardened child can only be derived from private keys.
deriveHardened   :: Int -> Key Private net -> Key Private net 
deriveHardened  n k = primeSubKey k (fromIntegral n)

-- | Takes a hierarchical key keeping its ECSDA point or exponent, but setting it as root.
-- It is equivalent to: 
--
--  
-- prop> asDerivationRoot = read . showAsSingletonKey 
--  
deriveRoot :: Key v net -> Key v net 
deriveRoot (ExtendedPrv _ _ _ _ k) = ExtendedPrv 0 0 0 0 k 
deriveRoot (ExtendedPub _ _ _ _ k) = ExtendedPub 0 0 0 0 k 

-- | Compares 2 keys not taking into account their hierarchical position.
--
-- 
-- prop> k1 === k2 = asDerivationRoot k1 == asDerivationRoot k2
-- 
(===)            :: Key v net -> Key v net -> Bool
k1 === k2 = deriveRoot k1 == deriveRoot k2

-- | Show the key as a singleton compressed key as defined by the WIF format for private keys and
--   as hexadecimal representation of the OpenSSL binary serialization for public keys. 
showAsCompressedSingletonKey :: (BlockNetwork net) => Key v net -> String
showAsCompressedSingletonKey key = case key of 
                    ExtendedPrv _ _ _ _ k -> show $ Compressed True k
                    ExtendedPub _ _ _ _ k -> show $ Compressed True k

-- | Like 'showAsCompressedSingletonKey' but interpreting the keys as uncompressed. Notice this function is
--   for legacy keys, as currently most software only use compressed keys.
showAsUncompressedSingletonKey :: (BlockNetwork net) => Key v net -> String
showAsUncompressedSingletonKey key = case key of 
                    ExtendedPrv _ _ _ _ k -> show $ Compressed False k
                    ExtendedPub _ _ _ _ k -> show $ Compressed False k


serializeCompressedSingleton :: (BlockNetwork net) => Key v net -> BS.ByteString
serializeCompressedSingleton key = case key of 
                    ExtendedPrv _ _ _ _ k -> encode' $ Compressed True k
                    ExtendedPub _ _ _ _ k -> encode' $ Compressed True k


serializeUncompressedSingleton ::(BlockNetwork net) => Key v net -> BS.ByteString
serializeUncompressedSingleton key  = case key of 
                    ExtendedPrv _ _ _ _ k -> encode' $ Compressed False k
                    ExtendedPub _ _ _ _ k -> encode' $ Compressed False k


decodeCheckingCompression :: (Visibility_ v,BlockNetwork net) => BS.ByteString -> Maybe (Key v net,Bool)
decodeCheckingCompression = decodeCheckingCompression_


class Visibility_ (a::Visibility) where
    decodeCheckingCompression_ ::(BlockNetwork net) => BS.ByteString -> Maybe (Key a net,Bool)


instance Visibility_ Private where
    decodeCheckingCompression_ bs = do Compressed b k <- decodeToMaybe bs
                                       Just $ (ExtendedPrv 0 0 0 0 k, b)


instance Visibility_ Public where
    decodeCheckingCompression_ bs = do Compressed b k <- decodeToMaybe bs
                                       Just $ (ExtendedPub 0 0 0 0 k, b)


--data Get (n::Visibility) = Get{get_ :: Visibility}

------------------------------------------------------------------------------------------------------------------------
---- Private functions:


prvSubKey :: Key Private a -> Word32   -> Key Private a    
prvSubKey xkey child =  ExtendedPrv (prv_depth xkey + 1) (xPrvFP xkey) child c k 
        where 

            k     = addPrvKeys_ (prv_key xkey) a

            msg   = BS.append (encode'. Compressed True . derivePubKey_ $ prv_key xkey)
                            $ (encode'$ child)

            (a,c) = split512 $ hmac512 (encode' $  prv_chain xkey) msg


pubSubKey :: Key Public a   -> Word32  -> Key Public a 
pubSubKey xKey child = ExtendedPub (pub_depth xKey + 1) (xPubFP xKey) child c pK
    where 
       
       pK    = addPubKeys_ (pub_key xKey) a

       msg   = BS.append (encode'. Compressed True  $ pub_key xKey) (encode' child)
       (a,c) = split512 $ hmac512 (encode' $ pub_chain xKey) msg



primeSubKey :: Key Private a  -> Word32  -> Key Private a    
primeSubKey xkey child = ExtendedPrv (prv_depth xkey + 1) (xPrvFP xkey) i c k  --checked
      where

        k     = addPrvKeys_ (prv_key xkey) a
         -- problem found!!!...?? -----------------> TODO: find what happened here :(
        i     =  setBit child 31 :: Word32
        msg   = BS.cons 0x00 $  BS.append (encode'(fromIntegral . prv_key $ xkey :: Word256)) (encode' i)
        (a,c) = split512 $ hmac512 (encode' $ prv_chain xkey) msg

{-                    


----------------------------------------------------------------------------------------------------------------------


-}
----------------------------------------------------------------------------------------------------------------------
instance (BlockNetwork net) => Show (Key Private net) where
    show     = showAsBinary58

instance (BlockNetwork net) => Show (Key Public net) where
    show     = showAsBinary58 -- change this!!


-- important!!! -> on the instantiation make sure it consumes all input!!!
instance (BlockNetwork net) => Binary (Key Public  net) where
    
    get      = get_aux 
      where
        get_aux :: forall net. (BlockNetwork net) => Get (Key Public  net)
        get_aux = do let params = (valuesOf :: Params net)
                     (version,k) <- (get_aux1 <|> get_aux2)
                     case version of 
                      Just v
                        | v == extPubKeyPrefix params -> return k
                        | otherwise                   -> fail "wrong version. are you using the same network for this key?" 
                      Nothing                         -> return k 
        

        get_aux1 = do ver                        <- getWord32be
                      dep                        <- getWord8
                      par                        <- getWord32be
                      idx                        <- getWord32be
                      chn                        <- get
                      Compressed compression pub <- get
                     
                      unless compression $ fail $ "Get: Extended key using an uncompressed public key"
                     
                      return (Just ver,ExtendedPub dep par idx chn pub)
        
        get_aux2 = do Compressed _ pub <- get 
                      return (Nothing, ExtendedPub 0 0 0 0 pub)



    put      = put_aux
      where 
        put_aux :: forall net. (BlockNetwork net) => Key Public  net -> Put 
        put_aux key = do let params = (valuesOf :: Params net)  
                         putWord32be $ extPubKeyPrefix params
                         putWord8    $ pub_depth  key
                         putWord32be $ pub_parent key
                         putWord32be $ pub_index  key
                         put         $ pub_chain  key
                         put         $ Compressed True (pub_key    key)


instance (BlockNetwork net) => Binary (Key Private net) where
    
    get      = get_aux1 <|> get_aux2
      where
        get_aux1 :: forall net. (BlockNetwork net) => Get (Key Private  net)
        get_aux1 = do let params = (valuesOf :: Params net)
                      
                      ver <- getWord32be
                     
                      unless (ver == extPrvKeyPrefix params) $ fail  "Get: Invalid version for extended private key"
                     
                      dep                        <- getWord8
                      par                        <- getWord32be
                      idx                        <- getWord32be
                      chn                        <- get
                      prv                        <- getPadPrvKey
                     
                      return $ ExtendedPrv dep par idx chn prv
        
        get_aux2 = do Compressed _ prv <- get 
                      return $ ExtendedPrv 0 0 0 0 prv


        getPadPrvKey::Get (PrvKey net_)
        getPadPrvKey   = do pad <- getWord8
                            guard $ pad == 0x00
                            fromIntegral <$> (get :: Get Word256)

    put      = put_aux
      where 
        put_aux :: forall net. (BlockNetwork net) => Key Private  net -> Put 
        put_aux key = do let params = (valuesOf :: Params net)  
                         
                         putWord32be  $ extPrvKeyPrefix params
                         putWord8     $ prv_depth  key
                         putWord32be  $ prv_parent key
                         putWord32be  $ prv_index  key
                         put          $ prv_chain  key
                         putPadPrvKey $ prv_key    key


        putPadPrvKey k = putWord8 0x00 >> put (fromIntegral k :: Word256)


-------------------------------




instance (BlockNetwork net) => Read (Key Private net) where
   readsPrec = readsPrecAsBinary58


instance (BlockNetwork net) => Read (Key Public  net) where
   readsPrec n s = readsPrecAsBinary58 n s ++ readsPrecAsBinary n s

                      
----------------------------------------------------------------------------------------------------------------------
xPubFP :: Key Public a -> Word32
xPubFP = fromIntegral . (`shiftR` 128) . xPubID . pub_key

xPrvFP :: Key Private a -> Word32
xPrvFP = fromIntegral . (`shiftR` 128) . xPrvID . prv_key

