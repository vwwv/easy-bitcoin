{-|
Module      : EasyBitcoin
Description : Libray to parse and compose bitcoin transactions, keys and addresses. 
Copyright   : (c) Alejandro Durán Pallarés, 2015
License     : GPL-3
Maintainer  : vwwv@correo.ugr.es
Stability   : experimental


EasyBitcoin is a simple haskell library providing types and class-instances for bitcoin related code; 
it also include an small set of functions to handle addresses, transactions and escrows.

-}

{-# LANGUAGE DataKinds, GADTs, TypeFamilies, EmptyDataDecls #-}


module Network.EasyBitcoin 
 (
 -- * Usage Example:
 -- $example 

 -- * Addresses:
   Address()
 , Addressable(..)
 -- , addressFromUncompressed TODO: on docs, explain one could need this
 --                                 speak about 'legacy'

 -- * Transactions:
 , Outpoint (..)
 , Txid()
 , txid
 -- ** Generic Transactions:
 , Tx ()
 , Transaction(..)
 , transaction
 , txOutputs
 , txInputs
 , (===)
 -- ** Simple Transactions:
 , SimpleTx(..)
 , simpleTx

 -- ** Multisig Transactions:
 --, MultisigTx
 --, multisigTX
 --, addSignature
 --, combineSignatures
 --, extractRedeem
 --, signaturesUsed
 --, RedeemScript
 
 -- * Keys: 
 , Key()
 , Visibility(..)
 , derive
 , derivePublic
 , deriveHardened
 , deriveRoot

 -- * Bitcoin Units:
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
 , BlockNetwork()
 ) where

import Network.EasyBitcoin.NetworkParams( ProdNet
                                        , TestNet
                                        , BlockNetwork
                                        )

import Network.EasyBitcoin.Keys         ( Key()
                                        , Visibility(..)
                                        , derive
                                        , derivePublic
                                        , deriveHardened
                                        , deriveRoot
                                        , (===)
                                        )

import Network.EasyBitcoin.BitcoinUnits ( btc
                                        , mBTC
                                        , satoshis
                                        , asBtc
                                        , asMbtc
                                        , asSatoshis
                                        , showAsBtc
                                        , showAsMbtc
                                        , showAsSatoshis
                                        )

import Network.EasyBitcoin.Address      ( Address
                                        , Addressable(..)
                                        )

import Network.EasyBitcoin.Transaction  ( Outpoint(..)
                                        , Tx(..)
                                        , TxIn(..)
                                        , TxOut(..)
                                        , SimpleTx(..)
                                        , Txid
                                        , txid
                                        , Transaction(..)
                                        , simpleTx
                                        , transaction
                                       -- , MultisigTx(..)
                                       -- , multisigTX
                                       -- , addSignature
                                       -- , combineSignatures
                                       -- , extractRedeem
                                       -- , signaturesUsed
                                        )

import Network.EasyBitcoin.Script       ( RedeemScript(..)
                                        , decodeOutput
                                        , decodeInput
                                        )

import Network.EasyBitcoin.BitcoinUnits ( BTC()
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



---- Clean this a bit...
--txOutputs :: (BlockNetwork net) => Tx -> [(BTC net, Maybe (Address net))] 
txOutputs :: Tx -> [(BTC TestNet, Maybe (Address TestNet))]
txOutputs tx = fmap transform $ txOut tx 
   where
    transform x = ( satoshis $ outValue x
                  , decodeOutput $ scriptOutput x
                  )

--txInputs :: (BlockNetwork net) => Tx -> [(Outpoint,Maybe (Address net))]
--should only return the address in case its signature is verified and complete!!!
txInputs :: Tx -> [(Outpoint,Maybe (Address TestNet))]
txInputs tx = fmap transform $ txIn tx 
   where
    transform x = ( prevOutput  x
                  , decodeInput $ scriptInput  x 
                  )


{- $example

   @
   import Network.SimpleBitcoin
   import Data.Aeson.Lens
   import Control.Lens
   import Network.Wreq(get)
   import Control.Monad(forever)
   
   incoming  = read "xxxxxxxx" :: Key Private TestNet 
   outgoingA = read \"YYYYYYYY\" :: Address TestNet
   outgoingB = read \"YYYYYYYY\" :: Address TestNet

   fee       = btc 0.0001 

   main = do putStrLn $ "Rebrodcasting from " ++ address incoming
             forever  $ readToshi >>= maybe (return ()) sendToshi . createTransaction
   
   
   readToshi :: IO [(Outpoint, BTC TestNet)]
   readToshi = undefined
   
   sendToshi :: Tx -> IO ()
   sendToshi = undefined

   createTransaction :: [(Outpoint, BTC TestNet)] -> Maybe Tx
   createTransaction = undefined
   @

-}






---------------------------------------------------------------------------------


