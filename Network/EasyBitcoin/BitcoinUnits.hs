{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Network.EasyBitcoin.BitcoinUnits 
 ( btc
 , mBTC
 , satoshis
 , asBtc
 , asMbtc
 , asSatoshis
 , showAsBtc
 , showAsMbtc
 , showAsSatoshis
 , BTC()
 )where


import Network.EasyBitcoin.NetworkParams
import Network.EasyBitcoin.Internal.InstanciationHelpers
import Control.Arrow(first)
import Control.Applicative((<$>))
import Data.Aeson

-- | Bitcoins are represented internally as an integer value, but showed and read as a decimal values.
--   When importing them, extra significative digits will be silently dropped.

newtype BTC  a = Satoshis Int   deriving (Eq,Ord,Num)

btc            :: Double -> BTC net
btc          x = Satoshis $ round (x*100000000) 

mBTC           :: Double -> BTC net
mBTC         x = Satoshis $ round (x*100000)  

satoshis       :: Int    -> BTC net
satoshis       = Satoshis      

asBtc          :: BTC net -> Double
asBtc      (Satoshis x) = fromIntegral x/100000000   

asMbtc         :: BTC net -> Double
asMbtc     (Satoshis x) = fromIntegral x /100000    

asSatoshis     :: BTC net -> Int
asSatoshis (Satoshis x) = x        


showAsBtc      :: BTC net -> String
showAsBtc (Satoshis x) = let str                 = reverse (show x) ++ replicate 9 '0'
                             (smallers,biggers)  = splitAt 8 str
                          in if all (=='0') biggers
                                then  "0." ++ reverse smallers 
                                else  dropWhile (=='0') $ reverse  biggers ++ "." ++ reverse smallers


showAsMbtc     :: BTC net -> String
showAsMbtc (Satoshis x) = let str                 = reverse (show x) ++ replicate 6 '0'
                              (smallers,biggers)  = splitAt 5 str
                           in if all (=='0') biggers
                                then  "0." ++ reverse smallers 
                                else  dropWhile (=='0') $ reverse  biggers ++ "." ++ reverse smallers



showAsSatoshis :: BTC net -> String
showAsSatoshis = show.asSatoshis


instance Show (BTC a) where
  show = showAsBtc

instance Read (BTC a) where
  readsPrec n str = first btc <$> (readsPrec n str:: [(Double,String)])


instance ToJSON (BTC a) where
  toJSON = toJSON.asBtc
