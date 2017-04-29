{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Fluidity.EVM.REPL.Monad where

import Prelude hiding (Value, break, fail, print, putStr, putStrLn)
import qualified Prelude
import Control.DeepSeq
import Data.ByteString (ByteString)
import Data.Functor.Identity (Identity(runIdentity))
import Data.Map (Map)
import Data.Text (Text, pack, unpack)
import System.Console.ANSI
import System.Console.Haskeline (InputT)
import qualified System.Console.Haskeline.MonadException as ME
import qualified Data.Map as Map
import qualified Data.Text as T

import Control.Monad.Result
import Control.Monad.Resultant
import Control.Monad.Resultant.IO
import Control.Monad.Execution (execute)
import Text.Structured (Structured(fmt), block, typeset, (~-), (~~))
import qualified Control.Monad.Execution as Execution
import qualified Text.Structured as TS

import Fluidity.Common.Binary
import Fluidity.Common.ANSI
import Fluidity.EVM.Core.Blockchain (Blockchain)
import Fluidity.EVM.Core.Control (Control)
import Fluidity.EVM.Core.VM (VM)
import Fluidity.EVM.Core.System (Sys)
import Fluidity.EVM.Types
import Fluidity.EVM.Text (formatExternalCall)
import Fluidity.EVM.Data.Account
import Fluidity.EVM.Data.Format
import Fluidity.EVM.Data.Prov (Prov(Env))
import Fluidity.EVM.Data.Value
import qualified Fluidity.EVM.Analyse.Watchdog as Watchdog
import qualified Fluidity.EVM.Data.Prov as Prov
import qualified Fluidity.EVM.Data.Snapshot as Snapshot
import qualified Fluidity.EVM.Parallel as Parallel
import qualified Fluidity.EVM.REPL.Command as Cmd
import qualified Fluidity.EVM.REPL.Parser as Parser
import qualified Fluidity.EVM.Core.Blockchain as Blockchain
import qualified Fluidity.EVM.Core.Control as Control
import qualified Fluidity.EVM.Data.Bytecode as Bytecode
import qualified Fluidity.EVM.Core.VM as VM
import qualified Fluidity.EVM.Core.System as Sys
import qualified Fluidity.EVM.Core.Interrupt as INT


type REPL = ResultantT (InputT IO) State Error

data State = State
  { stControlState :: Control.State
  , stAddress      :: Address
  , stMode         :: Mode
  , stParSets      :: Map String ParSet
  , stMonitoring   :: Bool
  , stDataDir      :: FilePath
  }

data ParSet = ParSet [(ByteString, Control.State)]

data Mode = Single | Parallel

data Error
  = ControlError Control.Error
  | ControlInterrupt Control.Interrupt
  | BytecodeError Bytecode.Error
  | ParseError Int Parser.Error
  | NoMatchingAccount ByteString
  | NonUniqueAccountPrefix ByteString
  | NoSuchSet String
  | InternalError
  | IOError String
  | SnapshotError Snapshot.Error
  | Quit
  deriving (Show)

instance Exceptional Error where
  fromException = IOError . show

instance SubError Error Snapshot.Error where suberror = SnapshotError

--instance ME.MonadException REPL where
--  controlIO (ME.runIO f) 

initState :: State
initState = 
  let
    address = 0xabcd123456abcd123456abcd123456abcd123456 :: Integer
  in State
    { stControlState = Control.initState
    , stAddress      = value address (Env Prov.Address $ toBytes address) 
    , stMode         = Single
    , stParSets      = mempty
    , stMonitoring   = True
    , stDataDir      = ".aevm"
    }


-- Control interface
-- ---------------------------------------------------------------------

-- | Run an interruptible Control computation until it yields
yield :: Control a -> REPL ()
yield task = do
  (st, r) <- getControlState >>= Execution.execute
    task
    runControl
    handleInterrupt

  setControlState st
  case r of
    Left (x, _)   -> putStrLn $ "Suspended by interrupt: " ++ show x
    Right (Ok _)  -> return ()
    Right (Err e) -> fail $ ControlError e

-- | Run a Control computation without interrupts, discarding the final state
query :: Control a -> REPL a
query task =
  getControlState >>= Execution.query
    task
    ControlError
    ControlInterrupt
    runControl

-- | Run a Control computation without interrupts, preserving the final state
mutate :: Control a -> REPL a
mutate task = do
  (st, x) <- getControlState >>= Execution.uninterruptible
    task
    ControlError
    ControlInterrupt
    runControl
  setControlState st
  return x

runControl :: Identity a -> REPL a
runControl ma = return $ runIdentity ma

-- Control passthrough
querySys         = query  . Control.query
mutateSys        = mutate . Control.mutate
queryVM          = query  . Control.queryVM
mutateVM         = mutate . Control.mutateVM
queryBlockchain  = query  . Control.queryBlockchain
mutateBlockchain = mutate . Control.mutateBlockchain

{- Interrupt handlers

   We receive interrupts from the underlying VM that are proxied through the
   Control module. As a matter of design, we shouldn't do anything with them
   here except logging.
-}

-- | Handle a Control interrupt generated while yielding control
handleInterrupt :: Control.Interrupt -> Control.State -> REPL (Maybe Control.Interrupt)
handleInterrupt int st = do
  setControlState st
  pc <- queryVM VM.getPC
  case int of
    Control.SysInterrupt x      -> printInterrupt pc x
    Control.WatchdogEvent x     -> printInterrupt pc x
    Control.CallSucceeded       -> printLn $ colour Green "The call completed successfully."
    Control.CheckpointSaved i x -> printInterrupt pc $ "Saved checkpoint #" ++ show i ++ " " ++ x
    _                           -> putStrLn $ "Unknown interrupt: " ++ (show int)
  return Nothing

printInterrupt :: TS.Structured a => Int -> a -> REPL ()
printInterrupt pc x = printLn $ fmtCodePtr pc ~- x

-- Other common useful functions
-- ---------------------------------------------------------------------


uniqueAddress :: Cmd.Address -> REPL ByteString
uniqueAddress address =
  let
    addr = case address of Cmd.Address x -> x
                           Cmd.Prefix x  -> x
  in do
    addrs <- queryBlockchain $ Blockchain.addressesByPrefix addr
    case addrs of
      [x] -> return x
      []  -> fail $ NoMatchingAccount addr
      _   -> fail $ NonUniqueAccountPrefix addr
  
matchingAddresses :: Cmd.Address -> REPL [ByteString]
matchingAddresses x = map fst <$> matchingAccounts x

matchingAccounts :: Cmd.Address -> REPL [(ByteString, Account)]
matchingAccounts address =
  let
    addr = case address of Cmd.Address x -> x
                           Cmd.Prefix x  -> x
  in do
    accts <- queryBlockchain $ Blockchain.accountsByPrefix addr
    case accts of
      _:_      -> return accts
      []       -> fail $ NoMatchingAccount addr

getCaller :: REPL Value
getCaller = do
  x <- getAddress
  return $ setProv (Prov.Usr Prov.Caller $ toBytes x) x

-- General IO
-- ---------------------------------------------------------------------

putTxt :: Text -> REPL ()
putTxt = putStr . unpack

putTxtLn :: Text -> REPL ()
putTxtLn = putStrLn . unpack

print :: Structured a => a -> REPL ()
print = putStr . unpack . typeset

printLn :: Structured a => a -> REPL ()
printLn = putStrLn . unpack . typeset


-- Monad state
-- ---------------------------------------------------------------------

getAddress           = stAddress      <$> getState :: REPL Address
getDataDir           = stDataDir      <$> getState :: REPL FilePath
getControlState      = stControlState <$> getState :: REPL Control.State
getMode              = stMode         <$> getState :: REPL Mode
getParSets           = stParSets      <$> getState :: REPL (Map String ParSet)
getMonitoring        = stMonitoring   <$> getState :: REPL Bool

setAddress      x    = updateState (\st -> st { stAddress      = x }) :: REPL ()
setControlState x    = updateState (\st -> st { stControlState = x }) :: REPL ()
setMode         x    = updateState (\st -> st { stMode         = x }) :: REPL ()
setParSets      x    = updateState (\st -> st { stParSets      = x }) :: REPL ()
setMonitoring   x    = updateState (\st -> st { stMonitoring   = x }) :: REPL ()

updateControlState f = getControlState >>= setControlState . f
updateParSets      f = getParSets      >>= setParSets      . f

