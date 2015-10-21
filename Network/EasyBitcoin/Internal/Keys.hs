{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleContexts, FlexibleInstances, ScopedTypeVariables #-}
module Network.EasyBitcoin.Internal.Keys 
        where


import Network.EasyBitcoin.Internal.Words(Word256,FieldN,FieldP,Word160)
import Network.EasyBitcoin.Internal.InstanciationHelpers
import Network.EasyBitcoin.Internal.ByteString
import Network.EasyBitcoin.Internal.CurveConstants
import Network.EasyBitcoin.Internal.HashFunctions
import Network.EasyBitcoin.NetworkParams
import Data.Binary
import Control.Applicative
import Control.Monad
import Data.Aeson

newtype PrvKey net     = PrvKey FieldN deriving (Eq, Ord,Num,Enum,Real,Integral) 
data PubKey net        = PubKey {pubKeyPoint::Point}  deriving Eq

instance Binary (PubKey net) where
    get = fmap fromCompressed get
    put = put.Compressed True


derivePubKey_ :: PrvKey net -> PubKey net
derivePubKey_ (PrvKey k) = PubKey $ mulPoint k curveG


addPrvKeys_ :: PrvKey net -> Word256 -> PrvKey net 
addPrvKeys_ key i =  key + fromIntegral i

addPubKeys_ :: PubKey net -> Word256 -> PubKey net
addPubKeys_ (PubKey pub) i = PubKey $ addPoint pub $ mulPoint (fromIntegral i) curveG                 


-- | Computes the key identifier of an extended private key.
xPrvID :: PrvKey net -> Word160
xPrvID = xPubID . derivePubKey_
-- | Computes the key identifier of an extended public key.
xPubID :: PubKey net -> Word160
xPubID =  hash160 . hash256BS . encode' . Compressed True


------------------------------------------------------------------------------------------------------------------
data Compressed key = Compressed{ isCompressed   :: Bool
                                , fromCompressed :: key
                                } 


instance Show (Compressed (PubKey net)) where
    show = showAsBinary

instance Read (Compressed (PubKey net)) where
    readsPrec = readsPrecAsBinary

instance (BlockNetwork net) => Show (Compressed (PrvKey net)) where
    show = showAsBinary58

instance (BlockNetwork net) => Read (Compressed (PrvKey net)) where
    readsPrec = readsPrecAsBinary58

instance Binary (Compressed (PubKey net)) where

    get = do index <- getWord8
             case index of 
                2 -> Compressed True  . PubKey <$> (compressedWith True  =<< get)
                3 -> Compressed True  . PubKey <$> (compressedWith False =<< get) 
                4 -> Compressed False . PubKey <$> (makePoint  <$>  get  <*> get)
                _ -> fail $ "Get: Invalid public key encoding: " 

        where
            compressedWith isEven x = let a  = x ^ (3 :: Integer) + (curveA * x) + curveB
                                          ys = filter ((== isEven) . even) (quadraticResidue a)
                                   
                                       in case ys of
                                           y:_ ->  return $ makePoint x y 
                                           _   ->  fail   $ "No ECC point for x = "  ++ (show x)

    put (Compressed comp (PubKey point)) = let (x,y) = getAffine point 
                                            in case comp of
                                                True   
                                                   | even y     -> putWord8 2 >> put x
                                                   | otherwise  -> putWord8 3 >> put x
                                                False    -> putWord8 4 >> put x >> put y


quadraticResidue :: FieldP -> [FieldP]
quadraticResidue x = guard (y^(2 :: Int) == x) >> [y, (-y)]
  where
     q = (curveP + 1) `div` 4
     y = x^q








instance (BlockNetwork net) => Binary (Compressed (PrvKey net)) where
    get = get_
     where
        get_ :: forall x. (BlockNetwork x) => Get (Compressed (PrvKey x))
        get_ = let params = valuesOf:: Params x
                in getPriv (wifFormat params)
    
    put = put_
     where
        put_ :: forall x. (BlockNetwork x) => Compressed (PrvKey x) -> Put
        put_ = let params = valuesOf:: Params x
                in putPriv (wifFormat params)

---- wifFormatMainNet
---- wifFormatTestNet3

getPriv prefix = do mark       <- getWord8
                    payload    <- fromIntegral <$> (get::Get Word256)
                    compressed <- (getWord8 >>= (guard.(==0x01)) >> return True ) <|>  (return False)
                    guard (mark == prefix)

                    return (Compressed compressed$PrvKey payload)

putPriv prefix (Compressed c (PrvKey k)) = case c of 
                                             True   -> putWord8 prefix >> put (fromIntegral k::Word256) >> putWord8 0x01
                                             False  -> putWord8 prefix >> put (fromIntegral k::Word256)


--------------------------------------------------------------------------------------------------------------------

data Point = Point !FieldP !FieldP !FieldP   deriving(Show)  -- add extra field for performance

--Use the jacobian in the correct way...¿memoization of the right values for faster comparison?
-- this equal instance is wrong...
instance Eq Point where
 (Point x1 y1 z1) == (Point x2 y2 z2) = a == b && c == d
  where
    a = x1*z2 ^ (2 :: Int)
    b = x2*z1 ^ (2 :: Int)
    c = y1*z2 ^ (3 :: Int)
    d = y2*z1 ^ (3 :: Int)


getAffine :: Point -> (FieldP, FieldP)
getAffine (Point x y z) = (x/z ^ (2 :: Int), y/z ^ (3 :: Int))






-- ¿create here the Q point?


-- Elliptic curve point addition
addPoint :: Point -> Point -> Point
addPoint p1@(Point x1 y1 z1) (Point x2 y2 z2) = Point x3 y3 z3
                where
                    u1 = x1*z2 ^ (2 :: Int)
                    u2 = x2*z1 ^ (2 :: Int)
                    s1 = y1*z2 ^ (3 :: Int)
                    s2 = y2*z1 ^ (3 :: Int)
                    h  = u2 - u1
                    r  = s2 - s1
                    x3 = r ^ (2 :: Int) - h ^ (3 :: Int) - 2*u1*h ^ (2 :: Int)
                    y3 = r*(u1 * h ^ (2 :: Int) - x3) - s1 * h ^ (3 :: Int)
                    z3 = h * z1 * z2


-- Elliptic curve point doubling
doublePoint :: Point -> Point
doublePoint (Point x y z) = Point x' y' z'
        where
            s  = 4*x*y ^ (2 :: Int)
            m  = 3*x ^ (2 :: Int) + curveA * z ^ (4 :: Int)
            x' = m ^ (2 :: Int) - 2*s
            y' = m*(s - x') - 8*y ^ (4 :: Int)
            z' = 2*y*z


mulPoint :: FieldN -> Point -> Point
mulPoint 0 p             = error "please change this!!"-- p -- this is not correct...
mulPoint 1 p             = p
mulPoint n p | odd n     = addPoint p (mulPoint (n-1) p)
             | otherwise = mulPoint (n `div` 2) (doublePoint p)







-------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------
-- change the name
curveG :: Point
curveG = makePoint (fromInteger $ fst pairG)
                   (fromInteger $ snd pairG)


curveA :: FieldP
curveA = fromInteger integerA

curveB :: FieldP
curveB = fromInteger integerB

----------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------

makePoint :: FieldP -> FieldP -> Point 
makePoint x y = Point x y 1