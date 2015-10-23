
{-# LANGUAGE DataKinds, OverloadedStrings #-}

import Network.EasyBitcoin
import Control.Monad(forever)
import Network.HTTP.Client(HttpException(StatusCodeException))
import Network.Wreq(get,post,statusCode,responseBody)
import Control.Exception(handleJust)
import Control.Lens
import Data.Aeson.Lens
import Data.Aeson
import Safe
import Control.Applicative
import Control.Monad
import Control.Concurrent


   --Example program:
   --  - Let's imagine, on a blog, there's a donation address mm8LjcoUYdPNKgWshGs7dueFu33aK56ckb (private key 
   --    91vaDsoxZACAZeGM89Y7dBnbTB7wrvtBeEkMTpL2sCgEtHf4RBn). 

   --  - The blog is written by blogger A, who wrote 70% of the posts, and blogger B, who wrote the remaining 30%.

   --  - They want split the donation proportionally to the number of posts they have written

   --  - Blogger A has as personal address miHWju2dzq9RcUPESzYBgVWa3W3swTXtLo

   --  - Blogger B has as  personal address

   --  As this is an example, we won't use real bitcoin, but testnet bitcoin, also, we'll use Coinbase
   --  public bitcoin client, Toshi, so we don't have to install anything in our computer.


----------------------------------------------------------------------------------------------
incoming  = read "91vaDsoxZACAZeGM89Y7dBnbTB7wrvtBeEkMTpL2sCgEtHf4RBn" :: Key Private TestNet 
outgoingA = read "miHWju2dzq9RcUPESzYBgVWa3W3swTXtLo"                  :: Address TestNet
outgoingB = read "mvsXpubWQSw2dK2L85iYFppnNjGm439aWK"                  :: Address TestNet
----------------------------------------------------------------------------------------------
fee           = btc 0.0001  -- the miner fee to use.
threshold     = btc 0.2     -- won't send any transaction till reach this amount. This is important
                            -- to avoid creating "dust" transactions.

server        = "https://testnet3.toshi.io/" -- The Coinbase Toshi client testnet url.
secondsToPool = 20
----------------------------------------------------------------------------------------------

-- General workflow:
--    each 20 seconds:
--       - read from Toshi all unspent outpoints.
--       - if not enough funds holds on the unspent outpoints:
--           continue next iteration.
--       - else:
--           combining all avaliable outpoints into a transaction to miHWju2dzq9RcUPESzYBgVWa3W3swTXtLo and mvsXpubWQSw2dK2L85iYFppnNjGm439aWK
--           send this transaction to Toshi to be broadcasted into the network.

main::IO ()
main = do putStrLn $ "Rebrodcasting from " ++ show (address incoming) ++ " to "++ show outgoingA ++ " and " ++ show outgoingB
          forever  $ do readToshi >>= maybe (return ()) sendToshi . createTransaction 
                        threadDelay (secondsToPool*1000000)



-- If not enough funds, returns Nothing, otherwise, returns the transaction to be sent.
createTransaction :: [(Outpoint, BTC TestNet)] -> Maybe (Tx TestNet)
createTransaction inputs = if amount > threshold then Just txToSend
                                                 else Nothing
   where
      amount    = sum  (fmap snd inputs) - fee 
      amountToA = btc (asBtc amount * 0.7)
      amountToB = amount - amountToA
      
      txToSend  = transaction [ (outpoint,incoming) | (outpoint, _ ) <- inputs]
                              (outgoingA,amountToA)  [(outgoingB,amountToB)]


sendToshi :: Tx TestNet -> IO ()
sendToshi tx = do putStrLn $ "Sending tx: " ++ show (txid tx)
                  post (server ++ "/api/v0/transactions") (toJSON$show tx)
                  return ()

-- Querying and parsing the Toshi client about the unspent_outputs holds on the address defined by the private key 
-- 91vaDsoxZACAZeGM89Y7dBnbTB7wrvtBeEkMTpL2sCgEtHf4RBn (that is mm8LjcoUYdPNKgWshGs7dueFu33aK56ckb).
readToshi :: IO [(Outpoint, BTC TestNet)]
readToshi = handleJust isNotFound (const$ return []) 
          
          $ do body <- get $ server ++ "api/v0/addresses/"++ show (address incoming) ++ "/unspent_outputs" 
               return $ body ^.. responseBody . values . to parseOutpoint . _Just
 where
    -- Toshi returns 404 if the address has never received any tx
    isNotFound ex = case ex of 
                     StatusCodeException s _ _
                       | s ^. statusCode == 404 -> Just ()
                     _                          -> Nothing 

    parseOutpoint val = do vout   <- (val ^? key "output_index"     ._JSON)
                           txid   <- (val ^? key "transaction_hash" ._JSON. to readMay._Just)
                           amount <- (val ^? key "amount"           ._JSON. to satoshis)
                           
                           Just (Outpoint txid vout, amount)

