{-# LANGUAGE DataKinds,GADTs #-}
module Network.EasyBitcoin.Internal.Signatures 
 ( detSignMsg
 , Signature()
 , checkSig
 )where


import Network.EasyBitcoin.Keys
import Network.EasyBitcoin.Internal.Keys
import Network.EasyBitcoin.Internal.Words
import Network.EasyBitcoin.Internal.CurveConstants
import Network.EasyBitcoin.Internal.ByteString
import Network.EasyBitcoin.Internal.Words
import Network.EasyBitcoin.Internal.HashFunctions
import qualified Data.ByteString as BS
import Data.Binary (Binary, get, put, Word64,Word32,Word16)
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
import Control.Monad
import GHC.Word
import Control.Applicative
import Data.Bits

import Control.DeepSeq                 (NFData, rnf)
import Control.Monad                   (unless, guard)
import Data.Maybe
-- | Sign a message using ECDSA deterministic signatures as defined by
-- RFC 6979 <http://tools.ietf.org/html/rfc6979>


detSignMsg :: Word256 -> Key Private net -> Signature
detSignMsg n (ExtendedPrv _ _ _ _ (PrvKey  x)) = detSignMsg_ n x 

detSignMsg_ :: Word256 -> FieldN -> Signature
detSignMsg_ h d = go $ hmacDRBGNew (enc d) (encode' h) BS.empty
           
    where
        enc::FieldN -> BS.ByteString
        enc x = encode' (fromIntegral x ::Word256) 
        
        go ws = case hmacDRBGGen ws 32 BS.empty of         

                  (ws', Just  k)   -> let kI    = bsToInteger k
                                          p     = mulPoint (fromInteger kI) curveG
                                          sigM  = unsafeSignMsg h d (fromInteger kI,p)
                                              
                                       in if (isIntegerValidKey kI) 
                                             then fromMaybe (go ws') sigM 
                                             else go ws' 

                  (_  , Nothing)   -> error "detSignMsg: No suitable K value found"



-- Signs a message by providing the nonce
unsafeSignMsg :: Word256 -> FieldN -> (FieldN, Point) -> Maybe Signature
unsafeSignMsg _ 0 _     = Nothing
unsafeSignMsg h d (k,p) = do let (x,_) = getAffine p
                                      -- 4.1.3.3
                                 r     = (fromIntegral x :: FieldN)
                             --guard (r /= 0) -- is it necesary?
                                 e     = (fromIntegral h :: FieldN) -- double check this work!
                                 s'    = (e + r*d)/k

                                 -- Canonicalize signatures: s <= order/2
                                 -- maxBound/2 = (maxBound+1)/2 = order/2 (because order is odd)
                                 s     = if s' > (maxBound `div` 2) then (-s') else s'
                             
                                 -- 4.1.3.4 / 4.1.3.5
                                 --guard (s /= 0)
                                 -- 4.1.3.7
                             return $ Signature r s




--checkSig = undefined


-- Section 4.1.4 http://www.secg.org/download/aid-780/sec1-v2.pdf
-- | Verify an ECDSA signature
checkSig :: Word256 -> Signature -> Key Public net -> Bool
checkSig h sig ( ExtendedPub _ _ _ _ key) = checkSig_ h sig key
  where
-- 4.1.4.1 (r and s can not be zero)
    checkSig_ _ (Signature 0 _) _ = False
    checkSig_ _ (Signature _ 0) _ = False
    checkSig_ h (Signature r s) q = case Just $ getAffine p of
        Nothing    -> False
        Just (x,_) -> (fromIntegral x :: FieldN) == r
      where 
        -- 4.1.4.2 / 4.1.4.3
        e  = (fromIntegral h :: FieldN)
        -- 4.1.4.4
        s' = inverseN s
        u1 = e*s'
        u2 = r*s'
        -- 4.1.4.5 (u1*G + u2*q)
        p  = shamirsTrick u1 curveG u2 (pubKeyPoint q)


data Signature = Signature { sigR :: !FieldN
                           , sigS :: !FieldN
                           } deriving (Read, Show, Eq)



--------------------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------------

instance Binary Signature where

    get = do t <- getWord8 -- 0x30 is DER sequence type
             unless (t == 0x30) (fail $ "Bad DER identifier byte " ++ (show t) ++ ". Expecting 0x30")
             l <- getWord8 -- Length = (33 + 1 identifier byte + 1 length byte) * 2
             isolate (fromIntegral l) $ Signature <$> get <*> get


    put (Signature 0 _) = error "0 is an invalid r value in a Signature"
    put (Signature _ 0) = error "0 is an invalid s value in a Signature"
    
    put (Signature r s) = do putWord8 0x30
                             let c = runPut' $ put r >> put s -- this is wrong!!!!!!!!!
                             putWord8 (fromIntegral $ BS.length c)
                            -- error .show  $  (r,s)
                             putByteString c


-- Efficiently compute r1*p1 + r2*p2
shamirsTrick :: FieldN -> Point -> FieldN -> Point -> Point
shamirsTrick r1 p1 r2 p2 = addPoint (mulPoint r1 p1) (mulPoint r2 p2)
--shamirsTrick r1 p1 r2 p2 = go r1 r2
--  where 
--    q      = addPoint p1 p2
--    go 0 0 = Nothing -- error "TODO: think about this" --InfPoint
--    go a b 
--        | ea && eb  = Just $ b2
--        | ea        = Just $ addPoint b2 p2
--        | eb        = Just $ addPoint b2 p1
--        | otherwise = Just $ addPoint b2 q
--      where 
--        b2 = doublePoint <$> go (a `shiftR` 1) (b `shiftR` 1)
--        ea = even a
--        eb = even b



------------------------------------------------------------------------------------------------------------------------------


{-
-- Efficiently compute r1*p1 + r2*p2
shamirsTrick :: FieldN -> Point -> FieldN -> Point -> Point
shamirsTrick r1 p1 r2 p2 = go r1 r2
   where
        q = addPoint p1 p2
        go 0 0 = InfPoint
        go a b
            | ea && eb = b2
            | ea = addPoint b2 p2
            | eb = addPoint b2 p1
            | otherwise = addPoint b2 q
          where
            b2 = doublePoint $ go (a `shiftR` 1) (b `shiftR` 1)
            ea = even a
            eb = even b
-}


quadraticResidue :: FieldP -> [FieldP]
quadraticResidue x = guard (y^(2 :: Int) == x) >> [y, (-y)]
  where
     q = (curveP + 1) `div` 4
     y = x^q







---------------------------------------------------------------------------

------------------------------------------------------------------------------


---- delete this!
--instance Binary PointWithCommpression where
    
--    -- check this works!!!
--    get = do index <- getWord8
--             case index of 
--                2 -> PWC Compressed   <$> (compressedWith True  =<< get)
--                3 -> PWC Compressed   <$> (compressedWith False =<< get) 
--                4 -> PWC NoCompressed <$> (makePoint  <$>  get  <*> get)
--                _ -> fail $ "Get: Invalid public key encoding: " 

--        where
--            compressedWith isEven x = let a  = x ^ (3 :: Integer) + (curveA * x) + curveB
--                                          ys = filter ((== isEven) . even) (quadraticResidue a)
                                   
--                                       in case ys of
--                                           y:_ ->  return $ makePoint x y 
--                                           _   ->  fail   $ "No ECC point for x = "  ++ (show x)

--    put (PWC comp point) = let (x,y) = getAffine point 
--                            in case comp of
--                                Compressed   
--                                   | even y     -> putWord8 2 >> put x
--                                   | otherwise  -> putWord8 3 >> put x
--                                NoCompressed    -> putWord8 4 >> put x >> put y








