{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module Fluidity.EVM.Core.Control where

import Prelude hiding (fail)
import Data.Functor.Identity (Identity, runIdentity)
import Data.Map (Map)
import Control.Monad (liftM, void)
import GHC.Generics (Generic)
import Control.DeepSeq
import qualified Data.Map as Map

import Control.Monad.Result
import Control.Monad.Resultant
import Control.Monad.Execution (Execution, execute, interrupt)
import Control.Monad.Interruptible ()
import qualified Control.Monad.Execution as Execution

import Fluidity.EVM.Data.Transaction
import Fluidity.EVM.Core.Blockchain (Blockchain)
import Fluidity.EVM.Core.VM (VM)
import Fluidity.EVM.Core.System (Sys)
import Fluidity.EVM.Data.Format as Format
import qualified Fluidity.EVM.Analyse.Watchdog as Watchdog
import qualified Fluidity.EVM.Core.Blockchain as Blockchain
import qualified Fluidity.EVM.Core.VM as VM
import qualified Fluidity.EVM.Core.System as Sys
import qualified Fluidity.EVM.Core.Interrupt as I


type Control = Execution Identity Interrupt State Error

data State = State
  { stSystemState   :: Sys.State
  , stCheckpoints   :: CheckpointDB
  , stLast          :: Sys.State
  } deriving (Show, Generic, NFData)

data Interrupt
  = SysInterrupt I.Interrupt
  | WatchdogEvent Watchdog.Event
  | CheckpointSaved Int String
  | CallSucceeded
  | GenericInterrupt String
  deriving (Show, Generic, NFData)

data Error
  = InternalError
  | SysError Sys.Error
  | CheckpointError String
  | Interrupted I.Interrupt
  | Suspended I.Interrupt
  | Busy
  deriving (Show, Generic, NFData)

instance SubError Error Sys.Error where suberror = SysError         


-- Debugger controls
-- ---------------------------------------------------------------------

-- | Set breaking interrupts to echo, run until halt, reset interrupts
go :: Control ()
go = do
  ints <- query Sys.getInterrupts
  bracket 
    (mutate . Sys.setInterrupts $ I.nobreak ints)
    (mutate $ Sys.setInterrupts ints)
    (yield Sys.resume)

-- | Run for a single cycle
step :: Control ()
step = do
  ints <- query Sys.getInterrupts
  bracket
    (mutate . Sys.setInterrupts $ I.setAction I.Break I.ICycle ints)
    (mutate $ Sys.setInterrupts ints)
    (yield Sys.resume)

-- | Run until the PC is at the given position or otherwise interrupted
breakAt :: Int -> Control ()
breakAt x = do
  bps <- query Sys.getBreakpoints
  bracket
    (mutate $ Sys.setBreakpoint x)
    (mutate $ Sys.setBreakpoints bps)
    (yield Sys.resume)

-- | Resume execution until next breaking interrupt
continue :: Control ()
continue =
  yield Sys.resume


resume = yield Sys.resume


-- Runtime monitoring
-- ---------------------------------------------------------------------

-- | Respond to a system interrupt
onInterrupt :: I.Interrupt -> Sys.State -> Control (Maybe I.Interrupt)
onInterrupt int st = do
  setSystemState st
  case int of I.Alert _ -> interrupt $ SysInterrupt int
              _ -> return ()
  pc <- queryVM VM.getPC
  mapM_ (interrupt . WatchdogEvent) (Watchdog.analyse int pc)
  return Nothing


-- Checkpoints 
-- ---------------------------------------------------------------------

type CheckpointDB = (Int, Map Int Checkpoint)
data Checkpoint = Checkpoint
  { cpName  :: String
  , cpState :: Sys.State
  } deriving (Show, Generic, NFData)

initCheckpoints :: CheckpointDB
initCheckpoints = (0, mempty)

-- | Loads the state at a given checkpoint, discarding the current state.
loadCheckpoint :: Int -> Control ()
loadCheckpoint i = getCheckpoints >>= \(_, cs) -> case Map.lookup i cs of
  Just c  -> setSystemState $ cpState c
  Nothing -> fail . CheckpointError $ "no such checkpoint: " ++ show i

-- | Save the current state as a checkpoint, returning the checkpoint ID
saveCheckpoint :: String -> Control Int
saveCheckpoint name = do
  cp       <- liftM (Checkpoint name) getSystemState
  (n, cps) <- getCheckpoints
  setCheckpoints (n + 1, Map.insert n cp cps)
  return n

dropCheckpoint :: Int -> Control ()
dropCheckpoint i = updateCheckpoints $ \(n,cps) -> (n, Map.delete i cps)


-- System interface
-- ---------------------------------------------------------------------

-- Passthrough functions
peek             = query Sys.peek
isActive         = query Sys.isActive
mutateBlockchain = mutate . Sys.mutateChain
queryBlockchain  = query  . Sys.queryChain
queryVM          = query  . Sys.query
mutateVM         = mutate . Sys.mutate
commitBlock      = mutate Sys.commitBlock

call :: MessageCall -> Control ()
call msg = do
  query Sys.assertIdle 
  st <- getSystemState 
  setLast st
  yield $ Sys.call msg

abort :: Control ()
abort = do
  query Sys.assertActive
  getLast >>= setSystemState

yield :: Sys () -> Control ()
yield ma = do
  (sys, result) <- getSystemState >>= execute ma ident onInterrupt
  setSystemState sys
  case result of
    Left (x, _)   -> fail $ Suspended x
    Right (Ok _)  -> return ()
    Right (Err e) -> do getLast >>= setSystemState
                        fail $ SysError e

-- | Run a VM computation without interrupts, discarding the final state
query :: Sys a -> Control a
query ma = getSystemState >>= Execution.query ma SysError Interrupted ident

-- | Run a VM computation without interrupts, preserving the final state
mutate :: Sys a -> Control a
mutate ma = do
  (sys, x) <- getSystemState >>= Execution.uninterruptible ma SysError Interrupted ident
  setSystemState sys
  return x

ident :: Identity a -> Control a
ident = return . runIdentity


-- Monad state initialisation and access
-- ---------------------------------------------------------------------

initState :: State
initState = State
  { stCheckpoints   = initCheckpoints
  , stSystemState   = Sys.initState
  , stLast          = Sys.initState
  }

getCheckpoints      = stCheckpoints   <$> getState
getSystemState      = stSystemState   <$> getState
getLast             = stLast          <$> getState

setCheckpoints   x  = updateState (\st -> st { stCheckpoints   = x }) :: Control ()
setSystemState   x  = updateState (\st -> st { stSystemState   = x }) :: Control ()
setLast          x  = updateState (\st -> st { stLast          = x }) :: Control ()

updateCheckpoints f = getCheckpoints >>= setCheckpoints . f
updateSystemState f = getSystemState >>= setSystemState . f

