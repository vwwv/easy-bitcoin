{-|
Module      : EasyBitcoin
Description : Libray to parse and compose bitcoin transactions, keys, addresses and escrows. 
Copyright   : (c) Alejandro Durán Pallarés, 2015
License     : BSD3
Maintainer  : vwwv@correo.ugr.es
Stability   : experimental


EasyBitcoin is a simple haskell library providing types and class-instances for bitcoin related code; 
it also include an small set of functions to handle addresses, transactions and escrows.

Some small programs as examples can be found at <http://example.com label> .

-}

{-# LANGUAGE DataKinds, GADTs, TypeFamilies, EmptyDataDecls #-}


module Network.EasyBitcoin( -- * Usage Example:
                            -- $example 

                         -- * Addresses:
                            Address()
                          , Addressable(..)
                          , isPay2SH 
                          , isPay2PKH
                          , addressFromUncompressed
                          

                          -- * Keys: 
                          , Key()
                          , Visibility(..)
                          , derive
                          , derivePublic
                          , deriveHardened
                          , deriveRoot
                          , (===)


                          -- * Transactions:
                          , Outpoint (..)
                          , Txid()
                          , txid
                          , Tx ()
                          , transaction
                          , unsignedTransaction
                          , txOutputs
                          , txInputs

                          -- * Escrows and Signatures:
                          , RedeemScript(..)
                          , ScriptSig()
                          , TxSignature()
                          , signTxAt
                          , scriptSig
                          , escrowSignatures
                          , escrowSignaturesFor
                          , simpleSignature                        
                          , checkInput
                          , checkSignatureAt
                          , createSignature
                          , createSignatureAs
                     

                          , BTC()
                          , btc
                          , mBTC
                          , satoshis
                          , asBtc
                          , asMbtc
                          , asSatoshis
                          , showAsBtc
                          , showAsMbtc
                          , showAsSatoshis

                          -- * Network Parameters:
                          , ProdNet
                          , TestNet
                          , BlockNetwork(..)
                          , Params(..)
                          ) where


import Network.EasyBitcoin.Address      ( Address
                                        , Addressable(..)
                                        , isPay2SH 
                                        , isPay2PKH
                                        , addressFromUncompressed
                                        )

import Network.EasyBitcoin.Script       ( RedeemScript(..)
                                        )

import Network.EasyBitcoin.NetworkParams( ProdNet
                                        , TestNet
                                        , BlockNetwork
                                        , Params(..)
                                        )

import Network.EasyBitcoin.Internal.Transaction
                                        ( Outpoint (..)
                                        , Txid
                                        , txid
                                        , Tx()
                                        )

import Network.EasyBitcoin.Transaction  ( transaction
                                        , txOutputs
                                        , txInputs
                                        , unsignedTransaction
                                        , ScriptSig()
                                        , TxSignature()
                                        , signTxAt
                                        , scriptSig
                                        , escrowSignatures
                                        , escrowSignaturesFor
                                        , simpleSignature                        
                                        , checkInput
                                        , checkSignatureAt
                                        , createSignature
                                        , createSignatureAs
                                        , SigHash(..)
                                        )

import Network.EasyBitcoin.Keys         ( Key()
                                        , Visibility(..)
                                        , derive
                                        , derivePublic
                                        , deriveHardened
                                        , deriveRoot
                                        , (===)
                                        )

import Network.EasyBitcoin.BitcoinUnits( BTC()
                                       , btc
                                       , mBTC
                                       , satoshis
                                       , asBtc
                                       , asMbtc
                                       , asSatoshis
                                       , showAsBtc
                                       , showAsMbtc
                                       , showAsSatoshis
                                       )

{- $example

   TODO:

    - explain examples of btc parsing looking the same
    - laws of derivation
    - show how parsing can make a node root!
    -  -- TODO that maxBoundIsWrong!!! ( on unsignedTransaction transaction!! file...)

    - Add warning!
           check: 
                          , txOutputs
                          , txInputs
                          , checkSimple

                          -- * Escrows:
                        --  , RedeemScript(..)
                        --  , unsignedTransaction
                        --  , signTx
                        --  , signedTx
                        --  , formatSignatures

                        --  , createSignature 
                        --  , checkSignature
                        --  , signatureAt 


   As a toy example, let's imagine the following scenario:
     
   *  On a blog, there's a donation address @mm8LjcoUYdPNKgWshGs7dueFu33aK56ckb@ (private   key @__91vaDsoxZACAZeGM89Y7dBnbTB7wrvtBeEkMTpL2sCgEtHf4RBn__@). 

   *  The blog is written by blogger A, who wrote __70%__ of the posts, and blogger B, who wrote the remaining __30%__.

   *  They want split the donation proportionally to the number of posts they have written.

   *  Blogger A has as personal address @__miHWju2dzq9RcUPESzYBgVWa3W3swTXtLo__@ .

   *  Blogger B has as personal address @__mvsXpubWQSw2dK2L85iYFppnNjGm439aWK__@ .

   As this is an example, we won't use real bitcoin, but testnet bitcoin, also, we'll use Coinbase's
   public bitcoin client, Toshi, so we don't have to install anything in our computer:


   @

    {-# LANGUAGE DataKinds, OverloadedStrings #-}

    import Network.EasyBitcoin
    import Control.Monad(forever)
    import Network.HTTP.Client(HttpException(StatusCodeException))
    import Network.Wreq(get,statusCode,responseBody)
    import Control.Exception(handleJust)
    import Control.Lens
    import Data.Aeson.Lens
    import Safe
    import Control.Applicative
    import Control.Monad
    import Control.Concurrent


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


   @

-}