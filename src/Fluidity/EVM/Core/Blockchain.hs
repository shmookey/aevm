{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module Fluidity.EVM.Core.Blockchain where

import Prelude hiding (fail)
import Data.Map (Map)
import Data.ByteString (ByteString)
import GHC.Generics (Generic)
import Control.DeepSeq
import qualified Data.Map as Map
import qualified Data.ByteString as B

import Control.Monad.Resultant

import Fluidity.Common.Binary
import Fluidity.EVM.Types (Address, Balance, Bytecode, Log, Gas)
import Fluidity.EVM.Data.Value (cloneWith, uninitialised, bytes)
import Fluidity.EVM.Data.ByteField (size)
import Fluidity.EVM.Data.Operations (add, sub)
import Fluidity.EVM.Data.Transaction
import Fluidity.EVM.Data.Prov (Prov(Env), Env(..))
import Fluidity.EVM.Data.Value (Value, value, asAddress, uint)
import Fluidity.EVM.Data.Account (Account, AccountDB, CodeDB, StorageDB)
import qualified Fluidity.EVM.Data.Account as Acct

type Blockchain = Resultant State Error

data State = State
  { stBlocks        :: [Block]
  , stAccountDB     :: AccountDB
  , stCurrentBlock  :: Block
  , stCodeDB        :: CodeDB
  } deriving (Eq, Show, Generic, NFData)

data Error
  = InsufficientFunds
  | NotAContract ByteString
  | UnknownAccount ByteString
  | NoSuchBlock Int
  deriving (Eq, Show, Generic, NFData)


-- Blocks
-- ---------------------------------------------------------------------

data Block = Block
  { blkNumber       :: Value
  , blkHash         :: Value
  , blkTime         :: Value
  , blkGasPrice     :: Value
  , blkDifficulty   :: Value
  , blkCoinbase     :: Value
  , blkTransactions :: [Transaction]
  } deriving (Eq, Show, Generic, NFData)

getBlock :: Int -> Blockchain Block
getBlock n = getBlocks >>= \xs ->
  if n >= length xs
  then fail $ NoSuchBlock n
  else return $ xs !! n

-- | Commit a block and move on to the next one.
-- Ensure there are no transactions in progress!
commitBlock :: Integer -> Blockchain ()
commitBlock t = 
  let
    update k f v = let x = uint v in value (f x) (Env k $ toBytes x)
  in do
    blocks <- getBlocks
    current <- getCurrentBlock
    setBlocks $ blocks ++ [current]
    setCurrentBlock $ current
      { blkTime   = update BlockTime   (+t) $ blkTime current
      , blkHash   = update BlockHash   (+1) $ blkHash current
      , blkNumber = update BlockNumber (+1) $ blkNumber current
      }

blockTime       = blkTime       <$> getCurrentBlock
blockHash       = blkHash       <$> getCurrentBlock
blockNumber     = blkNumber     <$> getCurrentBlock
blockGasPrice   = blkGasPrice   <$> getCurrentBlock
blockDifficulty = blkDifficulty <$> getCurrentBlock
blockCoinbase   = blkCoinbase   <$> getCurrentBlock


-- Proxy functions to Fluidity.EVM.Data.Account
-- ---------------------------------------------------------------------

account         k     = Acct.account   k   <$> getAccountDB
balance         k     = Acct.balance   k   <$> getAccountDB
codeHash        k     = Acct.codeHash  k   <$> getAccountDB
storage         k     = Acct.storage   k   <$> getAccountDB
storageAt       k i   = Acct.storageAt k i <$> getAccountDB
code            k     = Acct.code      k   <$> getAccountDB <*> getCodeDB
setAccount      k x   = updateAccountDB $ Acct.setAccount      k x
setBalance      k x   = updateAccountDB $ Acct.setBalance      k x
setCodeHash     k x   = updateAccountDB $ Acct.setCodeHash     k x
setStorage      k x   = updateAccountDB $ Acct.setStorage      k x
setStorageAt    k i x = updateAccountDB $ Acct.setStorageAt    k i x
updateAccount   f k   = updateAccountDB $ Acct.updateAccount   f k
updateBalance   f k   = updateAccountDB $ Acct.updateBalance   f k
updateCodeHash  f k   = updateAccountDB $ Acct.updateCodeHash  f k
updateStorage   f k   = updateAccountDB $ Acct.updateStorage   f k
updateStorageAt f k i = updateAccountDB $ Acct.updateStorageAt f k i
deleteAccount   k     = updateAccountDB $ Acct.deleteAccount   k
deleteStorageAt k x   = updateAccountDB $ Acct.deleteStorageAt k x
credit          k x   = updateAccountDB $ Acct.credit          k x
debit           k x   = updateAccountDB $ Acct.debit           k x
transfer        k j x = updateAccountDB $ Acct.transfer        k j x
accountsByPrefix  k   = Acct.accountsByPrefix  k <$> getAccountDB
addressesByPrefix k   = Acct.addressesByPrefix k <$> getAccountDB


-- Monad state initialisation and access
-- ---------------------------------------------------------------------

getBlocks          = stBlocks       <$> getState
getCurrentBlock    = stCurrentBlock <$> getState
getAccountDB       = stAccountDB    <$> getState
getCodeDB          = stCodeDB       <$> getState
setBlocks        x = updateState (\st -> st { stBlocks       = x }) :: Blockchain ()
setCurrentBlock  x = updateState (\st -> st { stCurrentBlock = x }) :: Blockchain ()
setAccountDB     x = updateState (\st -> st { stAccountDB    = x }) :: Blockchain ()
setCodeDB        x = updateState (\st -> st { stCodeDB       = x }) :: Blockchain ()
updateAccountDB  f = getAccountDB >>= setAccountDB . f

initState :: State
initState = State
  { stBlocks       = mempty
  , stCurrentBlock = genesis
  , stAccountDB    = mempty
  , stCodeDB       = mempty
  }

genesis :: Block
genesis = 
  let
    nil k = value 0 . Env k $ toBytes (0 :: Int)
  in Block
    { blkNumber       = nil BlockNumber
    , blkHash         = nil BlockHash
    , blkTime         = nil BlockTime
    , blkGasPrice     = nil GasPrice
    , blkDifficulty   = nil Difficulty
    , blkCoinbase     = nil Coinbase
    , blkTransactions = mempty
    }

