{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module Fluidity.EVM.Data.Account where

import Control.DeepSeq
import GHC.Generics (Generic)
import Data.ByteString (ByteString)
import Data.Map (Map)
import Data.Semigroup
import qualified Data.ByteString as B
import qualified Data.Map as Map

import Fluidity.Common.Binary
import Fluidity.EVM.Data.Value
import Fluidity.EVM.Data.ByteField (ByteField)
import Fluidity.EVM.Data.Operations (add, sub)
import qualified Fluidity.EVM.Data.ByteField as BF
import qualified Fluidity.EVM.Data.Prov as Prov


-- Accounts
-- ---------------------------------------------------------------------

data Account = Account
  { acctBalance  :: Value
  , acctCodeHash :: ByteString
  , acctStorage  :: StorageDB
  } deriving (Eq, Show, Generic, NFData)

data AccountDB = AccountDB (Map ByteString Account)
  deriving (Eq, Show, Generic, NFData)

instance Semigroup AccountDB where
  (AccountDB a) <> (AccountDB b) = AccountDB (a <> b)

instance Monoid AccountDB where
  mappend a b = a <> b
  mempty = AccountDB mempty

-- Basic data access

fresh :: Account
fresh = Account
  { acctBalance  = valueFrom (Prov.Ext Prov.Balance) 0
  , acctCodeHash = mempty
  , acctStorage  = mempty
  }

accounts :: AccountDB -> Map ByteString Account
accounts (AccountDB x) = x

updateAccounts :: (Map ByteString Account -> Map ByteString Account) -> AccountDB -> AccountDB
updateAccounts f = AccountDB . f . accounts

-- CRUD operations

account :: ByteString -> AccountDB -> Account
account k = Map.findWithDefault fresh (toWord 20 k) . accounts

setAccount :: ByteString -> Account -> AccountDB -> AccountDB
setAccount k v = updateAccounts $ Map.insert (toWord 20 k) v

updateAccount :: (Account -> Account) -> ByteString -> AccountDB -> AccountDB
updateAccount f k = updateAccounts $ Map.adjust f (toWord 20 k)

deleteAccount :: ByteString -> AccountDB -> AccountDB
deleteAccount k = updateAccounts $ Map.delete (toWord 20 k)

-- Search functions

accountsByPrefix :: ByteString -> AccountDB -> [(ByteString, Account)]
accountsByPrefix bs = Map.toList . Map.filterWithKey (\k _ -> bs `B.isPrefixOf` k) . accounts

addressesByPrefix :: ByteString -> AccountDB -> [ByteString]
addressesByPrefix bs = map fst . accountsByPrefix bs

-- Field getters and setters

balance :: ByteString -> AccountDB -> Value
balance k = acctBalance . account k

codeHash :: ByteString -> AccountDB -> ByteString
codeHash k = acctCodeHash . account k

storage :: ByteString -> AccountDB -> StorageDB
storage k = acctStorage . account k

setBalance :: ByteString -> Value -> AccountDB -> AccountDB
setBalance k v = flip updateAccount k $ \x -> x { acctBalance = v }

setCodeHash :: ByteString -> ByteString -> AccountDB -> AccountDB
setCodeHash k v = flip updateAccount k $ \x -> x { acctCodeHash = v }

setStorage :: ByteString -> StorageDB -> AccountDB -> AccountDB
setStorage k v = flip updateAccount k $ \x -> x { acctStorage = v }

updateBalance :: (Value -> Value) -> ByteString -> AccountDB -> AccountDB
updateBalance f k = flip updateAccount k $ \x -> x { acctBalance = f $ acctBalance x }

updateCodeHash :: (ByteString -> ByteString) -> ByteString -> AccountDB -> AccountDB
updateCodeHash f k = flip updateAccount k $ \x -> x { acctCodeHash = f $ acctCodeHash x }

updateStorage :: (StorageDB -> StorageDB) -> ByteString -> AccountDB -> AccountDB
updateStorage f k = flip updateAccount k $ \x -> x { acctStorage = f $ acctStorage x }

-- Special modifiers

credit :: ByteString -> Value -> AccountDB -> AccountDB
credit k x = updateBalance (add x) k

debit :: ByteString -> Value -> AccountDB -> AccountDB
debit k x = updateBalance (flip sub x) k

transfer :: ByteString -> ByteString -> Value -> AccountDB -> AccountDB
transfer k1 k2 x = credit k2 x . debit k1 x

-- Code and storage access

code :: ByteString -> AccountDB -> CodeDB -> ByteField
code k db =  lookupCode $ codeHash k db

storageAt :: (Bytes a, Bytes b) => a -> b -> AccountDB -> Value
storageAt k i = storageAtDB (toBytes i) . storage (toBytes k)

setStorageAt :: Bytes a => a -> Value -> Value -> AccountDB -> AccountDB
setStorageAt k i x = flip updateStorage (toBytes k) $ setStorageAtDB i x

updateStorageAt :: (Value -> Value) -> ByteString -> Value -> AccountDB -> AccountDB
updateStorageAt f k i = flip updateStorage k $ updateStorageAtDB f i

deleteStorageAt :: ByteString -> ByteString -> AccountDB -> AccountDB
deleteStorageAt k i = flip updateStorage k $ deleteStorageAtDB i


-- Code
-- ---------------------------------------------------------------------

data CodeDB = CodeDB (Map ByteString ByteField)
  deriving (Eq, Show, Generic, NFData)

instance Semigroup CodeDB where
  (CodeDB a) <> (CodeDB b) = CodeDB (a <> b)

instance Monoid CodeDB where
  mappend a b = a <> b
  mempty = CodeDB mempty

-- Basic data access

codeDB :: CodeDB -> Map ByteString ByteField
codeDB (CodeDB x) = x

updateCodeDB :: (Map ByteString ByteField -> Map ByteString ByteField) -> CodeDB -> CodeDB
updateCodeDB f = CodeDB . f . codeDB

codeCount :: CodeDB -> Int
codeCount = Map.size . codeDB

-- CRUD operations

lookupCode :: ByteString -> CodeDB -> ByteField
lookupCode k = Map.findWithDefault mempty k . codeDB

insertCode :: ByteString -> ByteField -> CodeDB -> CodeDB
insertCode k v = updateCodeDB $ Map.insert k v

updateCode :: (ByteField -> ByteField) -> ByteString -> CodeDB -> CodeDB
updateCode f k = updateCodeDB $ Map.adjust f k

deleteCode :: ByteString -> CodeDB -> CodeDB
deleteCode k = updateCodeDB $ Map.delete k

codeSize :: ByteString -> CodeDB -> Value
codeSize k = BF.sizeValue . lookupCode k 


-- Storage
-- ---------------------------------------------------------------------

data StorageDB = StorageDB (Map ByteString (Value, Value))
  deriving (Eq, Show, Generic, NFData)

instance Semigroup StorageDB where
  (StorageDB a) <> (StorageDB b) = StorageDB (a <> b)

instance Monoid StorageDB where
  mappend a b = a <> b
  mempty = StorageDB mempty

-- Basic data access

storageDB :: StorageDB -> Map ByteString (Value, Value)
storageDB (StorageDB x) = x

updateStorageDB :: (Map ByteString (Value, Value) -> Map ByteString (Value, Value)) -> StorageDB -> StorageDB
updateStorageDB f = StorageDB . f . storageDB

storageSizeDB :: StorageDB -> Int
storageSizeDB = Map.size . storageDB

-- CRUD operations

storageAtDB :: Bytes a => a -> StorageDB -> Value
storageAtDB k = Map.findWithDefault uninitialised (padBytes 32 $ toBytes k) . Map.map snd . storageDB

setStorageAtDB :: Value -> Value -> StorageDB -> StorageDB
setStorageAtDB k v = updateStorageDB $ Map.insert (padBytes 32 $ toBytes k) (k, v)

updateStorageAtDB :: (Value -> Value) -> Value -> StorageDB -> StorageDB
updateStorageAtDB f k = updateStorageDB $ Map.adjust (\(_, b) -> (k, f b)) (padBytes 32 $ toBytes k)

deleteStorageAtDB :: Bytes a => a -> StorageDB -> StorageDB
deleteStorageAtDB k = updateStorageDB $ Map.delete (padBytes 32 $ toBytes k)

-- Keys and values

storageEntriesDB :: StorageDB -> [(Value, Value)]
storageEntriesDB = Map.elems . storageDB

storageKeysDB :: StorageDB -> [Value]
storageKeysDB = map fst . storageEntriesDB

storageValuesDB :: StorageDB -> [Value]
storageValuesDB = map snd . storageEntriesDB

