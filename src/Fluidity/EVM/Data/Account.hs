{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module Fluidity.EVM.Data.Account where

import Control.DeepSeq
import GHC.Generics (Generic)
import Data.ByteString (ByteString)
import Data.Map (Map)
import qualified Data.Map as Map

import Fluidity.Common.Crypto
import Fluidity.EVM.Data.Value
import Fluidity.EVM.Data.ByteField


data Account
  = User Value
  | Contract Value ByteField StorageDB
  deriving (Show, Generic, NFData)

type StorageDB = Map ByteString (Value, Value)
type AccountDB = Map ByteString Account


getStorageAt :: Value -> StorageDB -> Value
getStorageAt k sdb = getStorageAt' (bytes k) sdb

setStorageAt :: Value -> Value -> StorageDB -> StorageDB
setStorageAt k v sdb = Map.insert (keccak256 $ bytes k) (k, v) sdb

getStorageAt' :: ByteString -> StorageDB -> Value
getStorageAt' k sdb = Map.findWithDefault uninitialised (keccak256 k) (Map.map snd sdb)

--setStorageAt' :: ByteString -> Value -> StorageDB -> StorageDB
--setStorageAt' k v sdb = Map.insert (keccak256 $ bytes k) (k, v) sdb
