module Network.EasyBitcoin.Internal.InstanciationHelpers 
 where



import Network.EasyBitcoin.Internal.Base58 ( encodeBase58
                                           , decodeBase58
                                           , addRedundancy
                                           , liftRedundacy
                                           )

import Network.EasyBitcoin.Internal.ByteString
import Data.Binary

import qualified Data.ByteString as BS
import Data.Char(isSpace)
import Data.Aeson(FromJSON(..),ToJSON(..))
import Safe

readsPrec_ str       = let (word , rest) = span (not.isSpace)$ dropWhile isSpace str

                        in (decodeToMaybe =<< liftRedundacy =<< decodeBase58 word ,rest)



show_ prefix = encodeBase58 . addRedundancy . BS.cons prefix . encode' 





showAsBinary::(Binary a) => a -> String
showAsBinary = bsToHex.encode'

readsPrecAsBinary :: (Binary a) => Int -> ReadS a
readsPrecAsBinary  _ str = case readsPrec__ str of 
                                ( Just result, rest) -> [(result,rest)]
                                _                    -> []


    where
      readsPrec__ str       = let (word , rest) = span (not.isSpace)$ dropWhile isSpace str
                               in ( decodeToMaybe =<< hexToBS word ,rest)



showAsBinary58::(Binary a) => a -> String
showAsBinary58 = encodeBase58 . addRedundancy . encode' 

readsPrecAsBinary58 :: (Binary a) => Int -> ReadS a
readsPrecAsBinary58  _ str = case readsPrec_ str of 
                                ( Just result, rest) -> [(result,rest)]
                                _                    -> []