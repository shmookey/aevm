module Fluidity.EVM.REPL.Chain where

import Prelude hiding (fail, putStrLn)
import Data.ByteString.Char8 (ByteString)
import Data.Maybe as Maybe (fromMaybe)
import qualified Data.Map as Map

import Control.Monad.Resultant
import Control.Monad.Resultant.IO
import Text.Structured (Structured(fmt), (~-), (~~))

import Fluidity.Common.Binary
import Fluidity.EVM.Types
import Fluidity.EVM.REPL.Monad
import Fluidity.EVM.Blockchain (Block(..))
import Fluidity.EVM.Data.Account (Account(Account))
import Fluidity.EVM.Data.Value
import Fluidity.EVM.Data.Format (stub, currency)
import qualified Fluidity.EVM.Data.Account as Acct
import qualified Fluidity.EVM.Data.Format as Format
import qualified Fluidity.EVM.Blockchain as Blockchain
import qualified Fluidity.EVM.REPL.Command as Cmd


runCommand :: Cmd.Chain -> REPL ()
runCommand cmd = case cmd of
  Cmd.ChainBlock block -> case block of
    Cmd.BlockCommit -> blockCommit
    Cmd.BlockShow x -> blockShow x
    Cmd.BlockList   -> blockList
  Cmd.ChainAccount account -> case account of
    Cmd.AccountList prefix         -> listAccounts prefix
    Cmd.AccountBalanceGet a        -> getBalance a
    Cmd.AccountBalanceSet a x      -> setBalance a x
    Cmd.AccountShow a              -> showAccount a
    Cmd.AccountDrop a              -> dropAccount a
    Cmd.AccountStorageGet a        -> getStorage a
    Cmd.AccountStorageGetKey a k   -> getStorageAt a k
    Cmd.AccountStorageSetKey a k v -> setStorageAt a k v


blockCommit :: REPL ()
blockCommit = do
  mutateBlockchain Blockchain.commitBlock

blockShow :: Maybe Integer -> REPL ()
blockShow mi = 
  let
    showBlock :: Block -> String
    showBlock blk = show blk
  in do
    block <- case mi of 
      Just x -> queryBlockchain $ Blockchain.getBlock (fromInteger x)
      Nothing -> queryBlockchain Blockchain.getCurrentBlock
    printLn $ showBlock block
    

blockList :: REPL ()
blockList = 
  let
    showBlockListing blk = show blk
  in do
    blocks <- queryBlockchain Blockchain.getBlocks
    mapM_ (printLn . showBlockListing) blocks

listAccounts :: Maybe Cmd.Address -> REPL ()
listAccounts prefix = 
  let
    showAccount (addr, Account v _ _) = Format.address addr ~- Format.currency v
  in do
    accts <- matchingAccounts $ Maybe.fromMaybe (Cmd.Prefix mempty) prefix
    mapM_ (printLn . showAccount) accts

getBalance :: Cmd.Address -> REPL ()
getBalance addr = 
  let
    showAccount (addr, Account v _ _) = stub addr ~- v
  in do
    accts <- matchingAccounts addr
    mapM_ (printLn . showAccount) accts

setBalance :: Cmd.Address -> Integer -> REPL ()
setBalance = error ""

showAccount :: Cmd.Address -> REPL ()
showAccount addr = 
  let
    showAcct (address, acct) = do
      code <- queryBlockchain $ Blockchain.code address
      return $ Format.accountDetails address acct code
  in do
    accts <- matchingAccounts addr
    deets <- mapM showAcct accts
    mapM_ printLn deets

dropAccount :: Cmd.Address -> REPL ()
dropAccount = error ""

getStorage :: Cmd.Address -> REPL ()
getStorage addr =
  let
    showAccount (addr, Account _ _ st) = return $ show st
  in do
    accts <- matchingAccounts addr
    mapM showAccount accts >>= mapM_ printLn

getStorageAt :: Cmd.Address -> ByteString -> REPL ()
getStorageAt ref key = do
  addr <- uniqueAddress ref
  val <- queryBlockchain $ Blockchain.storageAt key addr
  putStrLn $ toHex val 

setStorageAt :: Cmd.Address -> ByteString -> ByteString -> REPL ()
setStorageAt = error ""

