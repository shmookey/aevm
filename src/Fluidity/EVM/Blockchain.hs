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

import Fluidity.EVM.Types (Address, Balance, Bytecode, Log, Gas, Storage)
import Fluidity.EVM.Data (ByteField, Value, asBytesAddress, (/+/))
import qualified Fluidity.EVM.Data as Data


type Blockchain = Resultant State Error

data State = State
  { stBlocks        :: [Block]
  , stAccounts      :: Map Address Account
  , stCurrentBlock  :: Block
  } deriving (Show)

data Account
  = User Balance
  | Contract Balance Bytecode Storage 
  deriving (Show)

data Error
  = InsufficientFunds
  | NotAContract Address
  | UnknownAccount Address
  | NoSuchBlock Int
  deriving (Show, Generic, NFData)

data Transaction = Transaction
  { txFrom      :: Address
  , txTo        :: Address
  , txHash      :: Value
  , txValue     :: Balance
  , txData      :: ByteField
  , txGas       :: Gas
  , txCalls     :: [MessageCall]
  } deriving (Show)


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
commitBlock = do
  blocks <- getBlocks
  current <- getCurrentBlock
  setBlocks $ blocks ++ [current]
  setCurrentBlock $ initBlock
    { blkTime   = blkTime   current /+/ o 1
    , blkHash   = blkHash   current /+/ o 1
    , blkNumber = blkNumber current /+/ o 1
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

initBlock :: Block
initBlock = Block
  { blkNumber       = o 0
  , blkHash         = o 0
  , blkTime         = o 0
  , blkGasPrice     = o 0
  , blkDifficulty   = o 0
  , blkCoinbase     = o 0
  , blkTransactions = mempty
  }


-- Accounts and balances
-- ---------------------------------------------------------------------

getAccount :: Address -> Blockchain Account
getAccount addr = getAccounts >>= \accts ->
  case Map.lookup addr accts of
    Just x  -> return x 
    Nothing -> fail $ UnknownAccount addr

-- | Like getAccount, but creates a new User account if the address does not exist
getAccount' :: Address -> Blockchain Account
getAccount' addr = getAccounts >>= \accts ->
  case Map.lookup addr accts of
    Just x  -> return x 
    Nothing -> let x = User (o 0) 
               in setAccounts (Map.insert addr x accts) >> return x

setAccount :: Address -> Account -> Blockchain ()
setAccount addr acct = getAccounts >>= setAccounts . Map.insert addr acct

updateAccount :: Address -> (Account -> Account) -> Blockchain ()
updateAccount addr f = getAccount' addr >>= setAccount addr . f

updateAccountM :: Address -> (Account -> Blockchain Account) -> Blockchain ()
updateAccountM addr f = do
  acct <- getAccount' addr
  acct' <- f acct
  setAccount addr acct'

getBalance :: Address -> Blockchain Balance
getBalance addr = 
  let
    defaultBalance :: Error -> Blockchain Balance
    defaultBalance _ = return (o 0)
  in recoverWith defaultBalance $ do
    acct <- getAccount addr
    case acct of
      Contract x _ _ -> return x
      User x         -> return x

setBalance :: Value -> Address -> Blockchain ()
setBalance x addr = updateAccount addr $ \acct -> case acct of
  Contract _ code storage -> Contract x code storage
  User _                  -> User x

updateBalance :: Address -> (Value -> Value) -> Blockchain ()
updateBalance addr f = getBalance addr >>= setBalance addr . f

matchAccounts :: ByteString -> Blockchain [(Address, Account)]
matchAccounts x = getAccounts >>= return . Map.toList .
  Map.filterWithKey (\k _ -> B.isPrefixOf x (asBytesAddress k))


-- Contracts
-- ---------------------------------------------------------------------

getCode :: Address -> Blockchain Bytecode
getCode addr = getAccount addr >>= \acct -> case acct of
  Contract _ code _ -> return code
  _                 -> fail $ NotAContract addr

getCodeSize :: Address -> Blockchain Value
getCodeSize addr = getAccount addr >>= \acct -> return . o $ case acct of
  Contract _ code _ -> toInteger $ B.length code
  _                 -> 0

getStorage :: Address -> Blockchain Storage
getStorage addr = getAccount addr >>= \acct -> case acct of
  Contract _ _ x -> return x
  _              -> fail $ NotAContract addr

setStorage :: Address -> Storage -> Blockchain ()
setStorage addr storage = updateAccountM addr $ \acct -> case acct of
  Contract balance code _ -> return $ Contract balance code storage
  _                       -> fail $ NotAContract addr

updateStorage :: Address -> (Storage -> Storage) -> Blockchain ()
updateStorage addr f = getStorage addr >>= setStorage addr . f

importContract :: Address -> Balance -> Bytecode -> Storage -> Blockchain ()
importContract addr val code storage =
  setAccount addr (Contract val code storage)


-- Message calls
-- ---------------------------------------------------------------------

data MessageCall = MessageCall
  { msgCaller :: Address
  , msgCallee :: Address
  , msgValue  :: Balance
  , msgGas    :: Value
  , msgData   :: ByteField
  } deriving (Show, Generic, NFData)

initMessageCall :: MessageCall
initMessageCall = MessageCall
  { msgCaller = o 0
  , msgCallee = o 0
  , msgValue  = o 0
  , msgGas    = o 0
  , msgData   = Data.fresh
  }


-- Monad state initialisation and access
-- ---------------------------------------------------------------------

initState :: State
initState = State
  { stBlocks       = mempty
  , stCurrentBlock = initBlock
  , stAccounts     = mempty 
  }

getBlocks       = stBlocks       <$> getState
getAccounts     = stAccounts     <$> getState
getCurrentBlock = stCurrentBlock <$> getState

setBlocks       x = updateState $ \st -> st { stBlocks       = x }
setCurrentBlock x = updateState $ \st -> st { stCurrentBlock = x }
setAccounts     x = updateState $ \st -> st { stAccounts     = x }

o = Data.origBlockchain


