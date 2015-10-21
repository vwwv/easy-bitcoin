{-# LANGUAGE FlexibleInstances, EmptyDataDecls, ScopedTypeVariables #-}

module Network.EasyBitcoin.Internal.Words 
 where


import Network.EasyBitcoin.Internal.CurveConstants(curveP,curveN)
import Network.EasyBitcoin.Internal.ByteString
import Numeric
import Control.Arrow
import Control.Applicative
import Data.Ratio                      (numerator, denominator)
import Data.Bits                       (shiftR   , shiftL, Bits(..))
import Data.Binary(Binary(..))
import qualified Data.ByteString as BS
import Control.Monad
import Data.Binary.Get ( getWord64be
                       , getWord32be
                       , getWord8
                       , getByteString
                       , Get
                       )
import Data.Binary.Put( putWord64be
                      , putWord32be
                      , putWord8
                      , putByteString
                      )

--import Network.Haskoin.Util

-- | Type representing a transaction hash.
type TxHash = BigWord Mod256Tx


-- | Type representing a block hash.
type BlockHash = BigWord Mod256Block


-- | Data type representing a 512 bit unsigned integer.
-- It is implemented as an Integer modulo 2^512.
type Word512 = BigWord Mod512


-- | Data type representing a 256 bit unsigned integer.
-- It is implemented as an Integer modulo 2^256.
type Word256 = BigWord Mod256


-- | Data type representing a 160 bit unsigned integer.
-- It is implemented as an Integer modulo 2^160.
type Word160 = BigWord Mod160


-- | Data type representing a 128 bit unsigned integer.
-- It is implemented as an Integer modulo 2^128.
type Word128 = BigWord Mod128


-- | Data type representing an Integer modulo coordinate field order P.
type FieldP = BigWord ModP


-- | Data type representing an Integer modulo curve order N.
type FieldN = BigWord ModN

data Mod512
data Mod256
data Mod256Tx
data Mod256Block
data Mod160
data Mod128
data ModP
data ModN

newtype BigWord n = BigWord { getBigWordInteger :: Integer 
                            } deriving (Eq, Ord) -- change the read and show instances....

class BigWordMod a where
    maxVal ::BigWord a -> Integer 
    maxVal x = 2 ^ rBitSize x 
    
    rBitSize     :: BigWord a -> Int
    -- = ds

instance BigWordMod Mod512 where
    rBitSize _     = 512

instance BigWordMod Mod256 where
    rBitSize _     = 256

instance BigWordMod Mod256Tx where
    rBitSize _     = 256

instance BigWordMod Mod256Block where
    rBitSize _     = 256

instance BigWordMod Mod160 where
    rBitSize _     = 160

instance BigWordMod Mod128 where
    rBitSize _     = 128

instance BigWordMod ModP where
    maxVal   _ = curveP
    rBitSize _ = 256


instance BigWordMod ModN where
    maxVal   _ = curveN
    rBitSize _ = 256


---------------------------------------------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------------------------------------------

instance (BigWordMod a) => Show (BigWord a) where
    
    show x@(BigWord x_) = let digits = rBitSize x `div` 4 
                           in ("0x"++). reverse . take digits $ (reverse $ showHex x_ "") ++ repeat '0'


instance Read (BigWord a) where
    readsPrec k x = first BigWord <$> readsPrec k x



------------------------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------------------------



rFromInteger :: forall a . (BigWordMod a) => Integer -> BigWord a   
rFromInteger  x = let max = maxVal (undefined :: BigWord a)     
                   in BigWord $ ((x `mod` max) + max) `mod` max -- make sure it yields the correct value for negative integers!

instance BigWordMod n => Num (BigWord n) where
    fromInteger = rFromInteger
    (BigWord i1) + (BigWord i2) = fromInteger $ i1 + i2
    (BigWord i1) * (BigWord i2) = fromInteger $ i1 * i2
    negate (BigWord i) = fromInteger $ negate i
    abs r = r
    signum (BigWord i) = fromInteger $ signum i


instance BigWordMod n => Bounded (BigWord n) where
    minBound = fromInteger 0
    maxBound = fromInteger (-1)


instance BigWordMod n => Real (BigWord n) where
   toRational (BigWord i) = toRational i


instance BigWordMod n => Enum (BigWord n) where
    
    succ r@(BigWord i)
      | r == maxBound = error "BigWord: tried to take succ of maxBound"
      | otherwise = fromInteger $ succ i
    
    pred r@(BigWord i)
      | r == minBound = error "BigWord: tried to take pred of minBound"
      | otherwise = fromInteger $ pred i
    
    toEnum i
      | toInteger i >= toInteger (minFrom r) && toInteger i <= toInteger (maxFrom r) = r
      | otherwise = error "BigWord: toEnum is outside of bounds"
    
          where
            r = fromInteger $ toEnum i
    
            minFrom :: BigWordMod a => BigWord a -> BigWord a
            minFrom _ = minBound

            maxFrom :: BigWordMod a => BigWord a -> BigWord a
            maxFrom _ = maxBound

    fromEnum (BigWord i) = fromEnum i


inverseP :: FieldP -> FieldP
inverseP (BigWord i) = fromInteger $ mulInverse i curveP


inverseN :: FieldN -> FieldN
inverseN (BigWord i) = fromInteger $ mulInverse i curveN


instance BigWordMod n => Integral (BigWord n) where
    (BigWord i1) `quot` (BigWord i2) = fromInteger $ i1 `quot` i2
    (BigWord i1) `rem` (BigWord i2) = fromInteger $ i1 `rem` i2
    (BigWord i1) `div` (BigWord i2) = fromInteger $ i1 `div` i2
    (BigWord i1) `mod` (BigWord i2) = fromInteger $ i1 `mod` i2
    (BigWord i1) `quotRem` (BigWord i2) = (fromInteger a, fromInteger b)
       where
        (a,b) = i1 `quotRem` i2
    (BigWord i1) `divMod` (BigWord i2) = (fromInteger a, fromInteger b)
       where
        (a,b) = i1 `divMod` i2
    
    toInteger (BigWord i) = i

{- Fractional is only defined for prime orders -}
instance Fractional (BigWord ModP) where
    recip = inverseP
    fromRational r = fromInteger (numerator r) / fromInteger (denominator r)


instance Fractional (BigWord ModN) where
    recip = inverseN
    fromRational r = fromInteger (numerator r) / fromInteger (denominator r)


{- Binary instances for serialization / deserialization -}




--isIntegerValidKey :: Integer -> Bool
--isIntegerValidKey i = i > 0 && i < curveN


-- Extended euclidean algorithm
-- Calculates the multiplicative inverse modulo p
extendedModGCD :: Integer -> Integer -> Integer -> (Integer, Integer)
extendedModGCD a b p | b == 0     = (1,0)
                     | otherwise  = (t, (s - q*t) `mod` p)
    where
        (q,r) = quotRem a b
        (s,t) = extendedModGCD b r p

        
-- Find multiplicative inverse of a : a*s = 1 (mod p)
mulInverse :: Integer -> Integer -> Integer
mulInverse a p | a*s `mod` p == 1 = s
               | otherwise        = error "No multiplicative inverse (mod p) for a"
    where
        (s,_) = extendedModGCD a p p


-- TODO, make this not necesary?
isIntegerValidKey :: Integer -> Bool
isIntegerValidKey i = i > 0 && i < curveN

--------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------

instance Binary (BigWord Mod256) where
    --get = error "dddd"
    get = do a <- fromIntegral <$> getWord64be
           --  error $ show a
             b <- fromIntegral <$> getWord64be
             c <- fromIntegral <$> getWord64be
             d <- fromIntegral <$> getWord64be
             return $ (a `shiftL` 192) + (b `shiftL` 128) + (c `shiftL` 64) + d

    put (BigWord i) = do putWord64be $ fromIntegral (i `shiftR` 192)
                         putWord64be $ fromIntegral (i `shiftR` 128)
                         putWord64be $ fromIntegral (i `shiftR` 64)
                         putWord64be $ fromIntegral i


instance Binary (BigWord Mod160) where
    get = do a <- fromIntegral <$> getWord32be
             b <- fromIntegral <$> getWord64be
             c <- fromIntegral <$> getWord64be
             return $ (a `shiftL` 128) + (b `shiftL` 64) + c

    put (BigWord i) = do putWord32be $ fromIntegral (i `shiftR` 128)
                         putWord64be $ fromIntegral (i `shiftR` 64)
                         putWord64be $ fromIntegral i


 {-
- Recheck twice!! 
 -}
 -- This one is wrong!!
instance Binary (BigWord ModN) where
    get = do t <- getWord8
             
             unless (t == 0x02) (fail $  "Bad DER identifier byte " ++ (show t) ++ ". Expecting 0x02" )
             
             l <- getWord8
             i <- bsToInteger <$> getByteString (fromIntegral l)
             
             unless (i > 0 && i < curveN) $ fail $ "Invalid fieldN element: " ++ (show i) -- shouldn't be that a >= instead of a > ?

             return $ fromInteger i
    
    put (BigWord 0) = error "0 is an invalid FieldN element to serialize"
    
    put (BigWord i) = do putWord8 0x02 -- Integer type
                         
                         let b = integerToBS i
                             l = fromIntegral $ BS.length b -- recheck if this works fine, also see how this affect whether is compressed or not....
                         
                         if BS.head b >= 0x80
                             then putWord8 (l + 1) >> putWord8 0x00
                             else putWord8 l
                         
                         putByteString b -- so here finally the error is !!!!!!!!!!!!!!!


instance Binary (BigWord ModP) where
    -- Section 2.3.6 http://www.secg.org/download/aid-780/sec1-v2.pdf
    get = do (BigWord i) <- get :: Get Word256
             unless (i>= 0 && i < curveP) (fail $ "Get: Integer not in FieldP: " ++ (show i))
             return $ fromInteger i
             -- Section 2.3.7 http://www.secg.org/download/aid-780/sec1-v2.pdf
    put r = put (fromIntegral r :: Word256)


instance Binary (BigWord Mod512) where
    get = do a <- fromIntegral <$> (get :: Get Word256)
             b <- fromIntegral <$> (get :: Get Word256)
             return $ (a `shiftL` 256) + b

    put (BigWord i) = do put $ (fromIntegral (i `shiftR` 256) :: Word256)
                         put $ (fromIntegral i :: Word256)


---------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------

-- | Split a 'Word512' into a pair of 'Word256'.
split512 :: Word512 -> (Word256, Word256)
split512 i = (fromIntegral $ i `shiftR` 256, fromIntegral i)


-- | Join a pair of 'Word256' into a 'Word512'.
join512 :: (Word256, Word256) -> Word512
join512 (a,b) = ((fromIntegral a :: Word512) `shiftL` 256) + (fromIntegral b :: Word512)



instance BigWordMod n => Bits (BigWord n) where
    (BigWord i1) .&.   (BigWord i2) = fromInteger $ i1 .&. i2
    (BigWord i1) .|.   (BigWord i2) = fromInteger $ i1 .|. i2
    (BigWord i1) `xor` (BigWord i2) = fromInteger $ i1 `xor` i2
    
    complement (BigWord i) = fromInteger $ complement i
    
    shift (BigWord i) j = fromInteger $ shift i j
    
    bitSize = rBitSize
    
    testBit (BigWord i) = testBit i
    
    bit n = fromInteger $ bit n
    
    popCount (BigWord i) = popCount i
    isSigned _ = False




















