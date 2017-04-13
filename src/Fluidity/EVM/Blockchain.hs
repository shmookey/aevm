{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module Fluidity.EVM.Blockchain where

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
import Fluidity.EVM.Data.Account
import qualified Fluidity.EVM.Data.Account as Account

type Blockchain = Resultant State Error

data State = State
  { stBlocks        :: [Block]
  , stAccounts      :: AccountDB
  , stCurrentBlock  :: Block
  } deriving (Show)

data Error
  = InsufficientFunds
  | NotAContract ByteString
  | UnknownAccount ByteString
  | NoSuchBlock Int
  deriving (Show, Generic, NFData)


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
  } deriving (Show)

getBlock :: Int -> Blockchain Block
getBlock n = getBlocks >>= \xs ->
  if n >= length xs
  then fail $ NoSuchBlock n
  else return $ xs !! n

-- | Commit a block and move on to the next one.
-- Ensure there are no transactions in progress!
commitBlock :: Blockchain ()
commitBlock = 
  let
    update k f v = let x = uint v in value (f x) (Env k $ toBytes x)
  in do
    blocks <- getBlocks
    current <- getCurrentBlock
    setBlocks $ blocks ++ [current]
    setCurrentBlock $ current
      { blkTime   = update BlockTime   (+1) $ blkTime current
      , blkHash   = update BlockHash   (+1) $ blkHash current
      , blkNumber = update BlockNumber (+1) $ blkNumber current
      }

currentBlockTime :: Blockchain Value
currentBlockTime = blkTime <$> getCurrentBlock

currentBlockHash :: Blockchain Value
currentBlockHash = blkHash <$> getCurrentBlock

currentBlockNumber :: Blockchain Value
currentBlockNumber = blkNumber <$> getCurrentBlock

currentBlockGasPrice :: Blockchain Value
currentBlockGasPrice = blkGasPrice <$> getCurrentBlock

currentBlockDifficulty :: Blockchain Value
currentBlockDifficulty = blkDifficulty <$> getCurrentBlock

currentBlockCoinbase :: Blockchain Value
currentBlockCoinbase = blkCoinbase <$> getCurrentBlock

-- TODO: get rid of this, don't originate values here
genesisBlock :: Block
genesisBlock = 
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


-- Accounts
-- ---------------------------------------------------------------------

getAccount :: Address -> Blockchain Account
getAccount addr = getAccount' $ bytes addr

-- | Like getAccount, but returns an uninitialised User account if the address does not exist
lookupAccount :: Address -> Blockchain Account
lookupAccount addr = recover (const $ User uninitialised) (getAccount addr)

setAccount :: Address -> Account -> Blockchain ()
setAccount addr = setAccount' $ bytes addr

updateAccount :: Address -> (Account -> Account) -> Blockchain ()
updateAccount addr f = lookupAccount addr >>= setAccount addr . f

updateAccountM :: Address -> (Account -> Blockchain Account) -> Blockchain ()
updateAccountM addr f = do
  acct <- lookupAccount addr
  acct' <- f acct
  setAccount addr acct'

matchAccounts :: ByteString -> Blockchain [(ByteString, Account)]
matchAccounts x = do
  accts <- getAccounts
  return . Map.toList $ Map.filterWithKey (\k _ -> B.isPrefixOf x k) accts

-- | Retrieve an account by address using the raw ByteString key
getAccount' :: ByteString -> Blockchain Account
getAccount' addr = getAccounts >>= \accts ->
  case Map.lookup addr accts of
    Just x  -> return x 
    Nothing -> fail $ UnknownAccount addr

-- | Set an account by address using the raw ByteString key
setAccount' :: ByteString -> Account -> Blockchain ()
setAccount' addr acct = getAccounts >>= setAccounts . Map.insert addr acct


-- Balances
-- ---------------------------------------------------------------------

getBalance :: Address -> Blockchain Value
getBalance addr = do
  acct <- getAccount addr
  case acct of
    Contract x _ _ -> return x
    User x         -> return x

lookupBalance :: Address -> Blockchain Value
lookupBalance = withDefault uninitialised . getBalance

setBalance :: Value -> Address -> Blockchain ()
setBalance x addr = updateAccount addr $ \acct -> case acct of
  Contract _ code storage -> Contract x code storage
  User _                  -> User x

updateBalance :: Address -> (Value -> Value) -> Blockchain ()
updateBalance addr f = getBalance addr >>= setBalance addr . f

creditAccount :: Value -> Address -> Blockchain ()
creditAccount val addr = updateBalance addr (add val)

debitAccount :: Value -> Address -> Blockchain ()
debitAccount val addr = updateBalance addr (flip sub val)


-- Contracts
-- ---------------------------------------------------------------------

getCode :: Address -> Blockchain Bytecode
getCode addr = getAccount addr >>= \acct -> case acct of
  Contract _ code _ -> return code
  _                 -> fail $ NotAContract (toBytes addr)

lookupCode :: Address -> Blockchain Bytecode
lookupCode = withDefault mempty . getCode

getStorage :: Address -> Blockchain StorageDB
getStorage addr = getAccount addr >>= \acct -> case acct of
  Contract _ _ x -> return x
  _              -> fail $ NotAContract (toBytes addr)

getStorage' :: Bytes a => a -> Blockchain StorageDB
getStorage' a = getAccount' (toBytes a) >>= \acct -> case acct of
  Contract _ _ x -> return x
  _              -> fail $ NotAContract (toBytes a)

getStorageAt :: Value -> Address -> Blockchain Value
getStorageAt k addr = getStorage addr >>= return . Account.getStorageAt k

getStorageAt' :: (Bytes k, Bytes a) => k -> a -> Blockchain Value
getStorageAt' k a = getStorage' (toBytes a) >>= return . Account.getStorageAt' (toBytes k)

setStorage :: Address -> StorageDB -> Blockchain ()
setStorage addr storage = updateAccountM addr $ \acct -> case acct of
  Contract balance code _ -> return $ Contract balance code storage
  _                       -> fail $ NotAContract (toBytes addr)

setStorageAt :: Value -> Value -> Address -> Blockchain ()
setStorageAt k v addr = updateStorage addr $ Account.setStorageAt k v

updateStorage :: Address -> (StorageDB -> StorageDB) -> Blockchain ()
updateStorage addr f = getStorage addr >>= setStorage addr . f

importContract :: Address -> Balance -> Bytecode -> StorageDB -> Blockchain ()
importContract addr val code storage =
  setAccount addr (Contract val code storage)


-- Monad state initialisation and access
-- ---------------------------------------------------------------------

initState :: State
initState = State
  { stBlocks       = mempty
  , stCurrentBlock = genesisBlock
  , stAccounts     = mempty 
  }

getBlocks       = stBlocks       <$> getState
getAccounts     = stAccounts     <$> getState
getCurrentBlock = stCurrentBlock <$> getState

setBlocks       x = updateState (\st -> st { stBlocks       = x }) :: Blockchain ()
setCurrentBlock x = updateState (\st -> st { stCurrentBlock = x }) :: Blockchain ()
setAccounts     x = updateState (\st -> st { stAccounts     = x }) :: Blockchain ()


