{-# LANGUAGE TypeSynonymInstances #-}
module Fluidity.EVM.REPL where

import Prelude hiding (Value, break, fail, print, putStr, putStrLn)
import qualified Prelude
import Control.Monad (forever, when, void)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString (ByteString)
import Data.Char (isSpace)
import Data.Functor.Identity (Identity(runIdentity))
import Data.List (dropWhileEnd, elemIndex, find, intercalate)
import Data.Maybe (catMaybes)
import Data.Semigroup ((<>))
import Data.Set ((\\))
import Data.Text (Text, pack, unpack)
import System.Console.Haskeline (InputT)
import System.Console.ANSI
import Text.Printf (printf)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Base16 as B16
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified System.Console.Haskeline as HL
import qualified System.IO as IO

import Control.Monad.Result
import Control.Monad.Resultant
import Control.Monad.Execution (execute)
import Text.Structured (Structured(fmt), block, typeset, (~-), (~~))
import qualified Control.Monad.Execution as Execution
import qualified Text.Structured as TS

import Confound.Methods (encodeCall)
import Fluidity.Common.Binary (unroll)
import Fluidity.Common.ANSI
import Fluidity.EVM.Blockchain (Blockchain)
import Fluidity.EVM.Data.Account
import Fluidity.EVM.Control (Control)
import Fluidity.EVM.Data.Value (Value, uint)
import Fluidity.EVM.Text ()
import Fluidity.EVM.Types
import Fluidity.EVM.Data.Bytecode (Op)
import Fluidity.EVM.VM (VM)
import qualified Fluidity.EVM.Data.Format as Format
import qualified Fluidity.EVM.Data.ByteField as ByteField
import qualified Fluidity.EVM.Blockchain as Blockchain
import qualified Fluidity.EVM.Control as Control
import qualified Fluidity.EVM.Data.Bytecode as Bytecode
import qualified Fluidity.EVM.VM as VM

import Fluidity.EVM.REPL.Monad hiding (REPL, Error)
import qualified Fluidity.EVM.REPL.Monad
import qualified Fluidity.EVM.REPL.Command as Cmd
import qualified Fluidity.EVM.REPL.Parser as Parser

import qualified Fluidity.EVM.REPL.Chain    as REPL.Chain
import qualified Fluidity.EVM.REPL.EVM      as REPL.EVM
import qualified Fluidity.EVM.REPL.Meta     as REPL.Meta
import qualified Fluidity.EVM.REPL.Monitor  as REPL.Monitor
import qualified Fluidity.EVM.REPL.State    as REPL.State
import qualified Fluidity.EVM.REPL.Parallel as REPL.Parallel


type REPL = Fluidity.EVM.REPL.Monad.REPL
type Error = Fluidity.EVM.REPL.Monad.Error

instance Structured Error where
  fmt e = highlight Red $ case e of
    BytecodeError x     -> "Error decoding program:" ~- x
    ControlError ce     -> fmt ce
    Quit                -> fmt "Quit"
    ParseError s pe ->
      let
        pad i = take (i+s-19) $ repeat ' '
        cur i = pad i ~~ "^\n"
        (i,x,d) = case pe of Parser.ParseError i x d -> (i,x,d)
                             Parser.LexError i x     -> (i,x,[])
      in cur i ~~ (intercalate ", " . drop 1 $ lines x)
        
    InternalError       -> fmt "Internal error."
    NoMatchingAccount x -> fmt "No matching address:" ~- (B8.unpack $ B16.encode x)
    _                   -> "Error:" ~- (show e)

instance Structured Control.Error where
  fmt err = case err of
    Control.VMError e -> fmt e
    _                 -> "Control error:" ~- show err


instance Structured Bytecode.Error where
  fmt = fmt . show


runWithSetup :: REPL () -> IO (Result Error ())
runWithSetup pre = 
  let
    settings :: HL.Settings IO
    settings = ( HL.defaultSettings :: HL.Settings IO )
      { HL.historyFile = Just ".aevm-history"
      , HL.complete    = Cmd.complete 
      }
    task     = welcome >> pre >> repl
  in
    HL.runInputT settings . fmap snd $ runResultantT task initState

loadMany :: [(Address, Balance, Bytecode, StorageDB)] -> REPL ()
loadMany contracts =
  let
    doImport (a,b,c,s) = Blockchain.importContract a b c s
    fst4 (a,b,c,d)     = a
    addrs              = map fst4 contracts
  in do
    mutateBlockchain $ mapM_ doImport contracts
    printLn $ "Loaded" ~- length contracts ~- "contracts"

load :: Address -> Balance -> Bytecode -> StorageDB -> REPL ()
load addr val code storage  = do
  mutateBlockchain $ Blockchain.importContract addr val code storage
  printStatus $ "Loaded contract at address " <> Format.address addr

fromSnapshot :: AccountDB -> REPL ()
fromSnapshot accts = do
  mutateBlockchain $ Blockchain.setAccounts accts
  printStatus $ "Loaded " <> show (Map.size accts) <> " accounts"

repl :: REPL ()
repl = do
  lift . liftIO $ IO.hSetBuffering IO.stdout IO.NoBuffering
  void $ forever prompt

welcome :: REPL ()
welcome = do
  printLn "Fluidity EVM 0.1.0"
  printLn "Type ':help' for usage information\n"

prompt :: REPL ()
prompt = 
  let
    onError :: Error -> REPL ()
    onError = printLn . typeset

  in recoverWith onError $ do
    txt   <- (typeset . highlight Green . embolden) <$> getPrompt
    input <- lift . HL.getInputLine $ T.unpack txt
    line  <- dropWhileEnd isSpace <$> fromMaybe Quit input
    if line == ""
    then return ()
    else do
      cmd   <- point . mapError (ParseError $ T.length txt) $ Parser.parse line
      case cmd of
        Cmd.Chain    x -> REPL.Chain.runCommand    x
        Cmd.EVM      x -> REPL.EVM.runCommand      x
        Cmd.Meta     x -> REPL.Meta.runCommand     x
        Cmd.Monitor  x -> REPL.Monitor.runCommand  x
        Cmd.Parallel x -> REPL.Parallel.runCommand x
        Cmd.State    x -> REPL.State.runCommand    x


-- REPL display and formatting
-- ---------------------------------------------------------------------

printStatus :: String -> REPL ()
printStatus x = print "--- " >> printLn x

printError :: Error -> REPL ()
printError x = print "!!! " >> printLn (show x)


getPrompt :: REPL String
getPrompt = query Control.isInCall >>= \c ->
  if not c
  then return "Idle> "
  else do
  pc      <- queryVM VM.getPC
  stack   <- queryVM VM.getStack
  memory  <- queryVM VM.getMemory
  op  <- withDefault "<no code>" $ fmap (unpack . typeset . fst) (queryVM VM.nextOp)
  return . unpack . typeset $ 
       (TS.hexdigits 4 pc)
    ~- "|"
    ~- length stack ~~ "/"
    ~~ (ByteField.size memory) ~~ "/"
    ~- "|" ~- op
    ~- ">>> "


