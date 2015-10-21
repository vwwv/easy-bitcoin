{-# LANGUAGE DataKinds, OverloadedStrings, MultiWayIf #-}


import Network.EasyBitcoin( transaction 
                          , btc
                          , BTC 
                          , TestNet
                          , Outpoint(..)
                          , Tx
                          , txid
                          , Txid
                          , Key
                          , Visibility(Private)
                          , address
                          , Address
                          )

import System.Environment(getArgs)                        -- To read the console arguments used to invoke the program.

import Network.Wreq(get,post,statusCode,responseBody)     -- Used to send request over HTTP. 

import Data.Aeson((.=),object)                            -- Used to manage Json data.
import Data.Aeson.Lens(key,values,_JSON)
import Control.Lens(toListOf,_Just,(^?),to)               -- We'll use lenses to parse the JSON responses.

import Safe(readMay)
import Control.Applicative((<$>))
import Data.List(sort)
import Data.Maybe(listToMaybe,fromMaybe)

--main::IO ()
--main = do args <- getArgs
--          case args of
            
--            ["print_address"]                      -> print_address
            
--            ["check_balance"]                      -> check_balance

--            ["send_bitcoins", to_, amount_]
--                  | Just to     <- readMay to_
--                  , Just amount <- readMay amount_ 
--                  , amount > 0                     -> send_bitcoins (btc amount) to
                                                                                

--                  | otherwise                      -> putStrLn "Address and/or amount to send could not be parsed."

--            _                                      -> putStrLn$ unlines [ "Undefined action, correct syntax is:"
--                                                                        , "   wallet check_balance : to check current balance."
--                                                                        , "   wallet print_address : to print the address where to receive funds"
--                                                                        , "   wallet send_bitcoins <address> <amount> :"++ 
--                                                                               " to send the specific amount (measured in btc) to the specific address"
--                                                                        ] 


main :: IO ()
main = do args <- getArgs
          case args of
           ["print_address"]                -> print_address
           ["check_balance"]                -> check_balance
           ["send_bitcoins", to_, amount_]
            | Just to     <- readMay to_
            , Just amount <- readMay amount_ 
            , amount > 0                    -> send_bitcoins (btc amount) to 
 

            | otherwise                     -> putStrLn "Address and/or amount to send could not be parsed."
           _                                -> putStrLn help
                       
  where
   help = unlines
         [ "Undefined action, correct syntax is:"
        , "   wallet check_balance : to check current balance."
        , "   wallet print_address : to print the address where to receive funds"
        , "   wallet send_bitcoins <address> <amount> :"++ 
               " to send the specific amount (measured in btc) to the specific address"
        ]


-- | this ought to be kept secret, otherwise it would be possible to steal our coins
secret :: Key Private TestNet
secret =  read "tprv8ZgxMBicQKsPcsbCVeqqF1KVdH7gwDJbxbzpCxDUsoXHdb6SnTPYxdwSAKYr9mrdtPfo3MkHsmViXxedm6MJD59TMuhj9vprD9UpGKnStwq"  

server      = "https://tbtc.blockr.io/api/v1/" 
threshold   = btc 0.0002
fee         = btc 0.0001


-- | For privacity enhalcement, nowdays most wallet generate a new address for each incoming transaction, but we'll keep things simple
--   and reuse the same.
our_address = address secret 


print_address :: IO ()
print_address = print our_address

check_balance:: IO ()
check_balance = do result <- readBlockExplorer
                   
                   let confirmed = sum [ btc | (True,_,btc) <- result ]
                       all_funds = sum [ btc | (_   ,_,btc) <- result ]
                   
                   putStrLn $ show all_funds ++ " BTCs ( " ++ show confirmed ++ " confirmed )"






send_bitcoins :: BTC TestNet -> Address TestNet -> IO ()
send_bitcoins to_send addr = do utxo            <- readBlockExplorer
                                
                                let (utxo',sending) = selectOutputs (to_send+fee) utxo 
                                    remaining       = sending - to_send - fee

                                if | remaining <  0         -> putStrLn "Not enough funds."

                                   | to_send   <  threshold -> putStrLn "Amount to send too small, considered dust."

                                   | remaining <  threshold -> send $ transaction [ (out,secret) | out <- utxo' ] (addr,to_send)
                                                                                  []

                                   | remaining >= threshold -> send $ transaction [ (out,secret) | out <- utxo' ] (addr,to_send)
                                                                                  [(our_address,remaining)]





selectOutputs:: BTC net -> [(Bool,Outpoint,BTC net)] -> ([Outpoint],BTC net)
selectOutputs total = fromMaybe ([],0) 
                    . listToMaybe   
                    . dropWhile ((<total).snd)         
                    . scanl step ([],0)                
                    . reverse 
                    . sort 

                              

      where
        step (outputs,total) (_,output,amount) = (output:outputs,amount+total) 





send :: Tx net -> IO () 
send tx = do post  resource (object [ "hex" .= show tx ])
             putStrLn $ "Transaction broadcasted, txid = " ++ show (txid tx) 
   where
    resource = server++"tx/push"



readBlockExplorer :: IO [(Bool,Outpoint,BTC TestNet)]
readBlockExplorer = toListOf (responseBody . key "data" . key "unspent" . values . to parseOutpoint . _Just) <$> get resource

     where 
       
       resource          = server ++ "address/unspent/"++ show our_address ++ "?unconfirmed=1"

       parseOutpoint val = do txid      <- val ^? key "tx"            . _JSON . to readMay . _Just
                              n         <- val ^? key "n"             . _JSON
                              amount    <- val ^? key "amount"        . _JSON . to readMay . _Just -- WARNING: amount is shown as String by Blockr.io
                              confirmed <- val ^? key "confirmations" . _JSON . to (>(0::Int))

                              return (confirmed,Outpoint txid n,btc amount)




