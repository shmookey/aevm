{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module Fluidity.EVM.Control where

import Prelude hiding (fail)
import Data.Functor.Identity (Identity)
import Data.Map (Map)
import Control.Monad (void)
import GHC.Generics (Generic)
import Control.DeepSeq
import qualified Data.Map as Map

import Control.Monad.Result
import Control.Monad.Resultant
import Control.Monad.Execution (Execution, execute, interrupt)
import Control.Monad.Interruptible ()
import qualified Control.Monad.Execution as Execution

import Fluidity.EVM.Data.Transaction
import Fluidity.EVM.Blockchain (Blockchain)
import Fluidity.EVM.VM (VM)
import Fluidity.EVM.Data.Format as Format
import qualified Fluidity.EVM.Analyse.Watchdog as Watchdog
import qualified Fluidity.EVM.Blockchain as Blockchain
import qualified Fluidity.EVM.VM as VM


type Control = Execution Identity Interrupt State Error

data State = State
  { stBlockchain    :: Blockchain.State
  , stCallStack     :: [VM.State]
  , stCheckpoints   :: CheckpointDB
  , stRunMode       :: Mode
  , stCallCont      :: Maybe (VM ())
  , stInterruptible :: Bool
  , stAnalyticMode  :: Bool
  , stMonitoring    :: Bool
  } deriving (Generic)

data Interrupt
  = VMInterrupt VM.Interrupt Int
  | WatchdogEvent Watchdog.Event
  | CheckpointSaved Int String
  | CallSucceeded
  | GenericInterrupt String
  deriving (Show, Generic, NFData)

-- | Current running mode requested from the caller
data Mode
  = Run | Step | Until Int
  deriving (Show)

data Error
  = InternalError
  | BlockchainError Blockchain.Error
  | VMError VM.Error
  | InvalidCheckpoint Int
  | UnexpectedInterrupt VM.Interrupt
  | NotExecutingCall
  | CallInProgress
  | NoCallInProgress
  | InconsistentInternalState
  deriving (Show, Generic, NFData)

instance NFData State where
  rnf st = seq (rnf stBlockchain)
         . seq (rnf stCallStack)
         . seq (rnf stCheckpoints)
         . seq (rnf stRunMode)
         . seq (rnf stInterruptible)
         $ seq (rnf stAnalyticMode) ()
         


-- Execution control
-- ---------------------------------------------------------------------

-- | Resume the active task and run until it stops
go :: Control ()
go = setRunMode Run >> resume

-- | Resume the active task for one step
step :: Control ()
step = setRunMode Step >> resume

-- | Resume the active task until a given address is reached
breakAt :: Int -> Control ()
breakAt x = setRunMode (Until x) >> resume


-- Runtime monitoring
-- ---------------------------------------------------------------------

-- | Respond to a VM interrupt, updating the call stack with the new VM state
handleInterrupt :: VM.Interrupt -> VM.State -> Control Bool
handleInterrupt ev st = getMonitoring >>= \monitoring ->
  if not monitoring
  then return True
  else do
    updateVMState st
    mode <- getRunMode

    let pc = VM.stPC st
    mapM_ (interrupt . WatchdogEvent) $ (force $ Watchdog.analyse ev pc)
    
    isUninterruptible <- not <$> getInterruptible
    isAnalyticMode    <- getAnalyticMode
    if isAnalyticMode
    then interrupt $ VMInterrupt ev pc -- Echo interrupt to the controlling monad
    else return ()

    case ev of
      VM.ProgramLoad -> do
        i <- saveCheckpoint "Program Load"
        interrupt $ CheckpointSaved i "Program load"
        return True

      VM.BeginCycle ptr -> case mode of
        Run     -> return True
        Step    -> return isUninterruptible
        Until x -> return $ ptr /= x

      VM.StorageWrite _ _ -> do
        if not isAnalyticMode 
        then interrupt $ VMInterrupt ev pc
        else return ()
        return True

      _ -> return True


-- Call stack
-- ---------------------------------------------------------------------

popStackFrame :: Control VM.State
popStackFrame = getCallStack >>= \(x:xs) -> setCallStack xs >> return x

pushStackFrame :: VM.State -> Control ()
pushStackFrame x = updateCallStack $ (:) x

currentStackFrame :: Control VM.State
currentStackFrame = getCallStack >>= \cs -> case cs of
  []      -> fail NoCallInProgress
  (x:xs)  -> return x

updateVMState :: VM.State -> Control ()
updateVMState x = popStackFrame >> pushStackFrame x

isInCall :: Control Bool
isInCall = getCallStack >>= \cs -> case cs of
  []      -> return False
  (x:xs)  -> return True


-- Checkpoints 
-- ---------------------------------------------------------------------

type CheckpointDB = (Int, Map Int Checkpoint)
data Checkpoint = Checkpoint
  { cpName       :: String
  , cpBlockchain :: Blockchain.State
  , cpCallStack  :: [VM.State]
  , cpCallCont   :: Maybe (VM ())
  }

initCheckpoints :: CheckpointDB
initCheckpoints = (0, mempty)

-- | Loads the state at a given checkpoint, discarding the current state.
loadCheckpoint :: Int -> Control ()
loadCheckpoint i = getCheckpoints >>= \(_, cps) -> case Map.lookup i cps of
  Just cp -> do
    setBlockchain $ cpBlockchain cp
    setCallStack  $ cpCallStack cp
    setCallCont   $ cpCallCont cp

  Nothing ->
    fail $ InvalidCheckpoint i

-- | Save the current state as a checkpoint, returning the checkpoint ID
saveCheckpoint :: String -> Control Int
saveCheckpoint name = do
  bc       <- getBlockchain
  cs       <- getCallStack
  at       <- getCallCont
  let cp = Checkpoint name bc cs at
  (n, cps) <- getCheckpoints
  setCheckpoints (n + 1, Map.insert n cp cps)
  return n

dropCheckpoint :: Int -> Control ()
dropCheckpoint i = updateCheckpoints $ \(n,cps) -> (n, Map.delete i cps)


-- VM Interface
-- --------------------------------------------------------------------

-- | Message call into a VM 
call :: MessageCall -> Control ()
call msg = do

  -- Pre-flight checks: not already in a message call
  inCall <- isInCall
  if inCall 
  then fail CallInProgress
  else return ()

  callCont <- getCallCont
  case callCont of 
    Just _  -> fail InconsistentInternalState -- this should never happen
    Nothing -> return ()


  -- Create a fresh context and run the VM
  let initialState = VM.initState msg
  pushStackFrame initialState
  (newState, result) <- Execution.executeR
    VM.call
    mutateBlockchain
    handleInterrupt
    initialState

  -- Update the local context
  updateVMState newState
  case result of
    Left cont -> do             -- Got continuation, store it, leave VM on the call stack
      setCallCont $ Just cont

    Right (Err e) -> do         -- Task failed, so remove the VM state from the call stack
      popStackFrame
      setCallCont Nothing
      fail $ VMError e

    Right (Ok _) -> do
      interrupt CallSucceeded
      popStackFrame             -- Task completed, so remove the VM state from the call stack
      setCallCont Nothing


-- | Resume the active call
resume :: Control ()
resume = do

  -- Pre-flight check 1: must be in a message call
  inCall <- isInCall
  if not inCall
  then fail NoCallInProgress
  else return ()

  -- Use a continuation, if we have one, otherwise resume from the start of the cycle
  maybeCallCont <- getCallCont
  let callCont = case maybeCallCont of 
                   Just x   -> x
                   Nothing  -> VM.resume

  -- Run the continuation with the VM state
  lastState          <- currentStackFrame
  (newState, result) <- Execution.executeR
    callCont
    mutateBlockchain
    handleInterrupt
    lastState

  -- Update the local context
  updateVMState newState
  case result of
    Left cont -> do             -- Got continuation, store it, leave VM on the call stack
      setCallCont $ Just cont

    Right (Err e) -> do         -- Task failed, so remove the VM state from the call stack
      popStackFrame
      setCallCont Nothing
      fail $ VMError e

    Right (Ok _) -> do
      interrupt CallSucceeded
      popStackFrame             -- Task completed, so remove the VM state from the call stack
      setCallCont Nothing


-- | Run a VM computation without interrupts, discarding the final state
query :: VM a -> Control a
query task =
  currentStackFrame >>= Execution.query
    task
    VMError
    UnexpectedInterrupt
    queryBlockchain

-- | Run a VM computation without interrupts, preserving the final state
mutate :: VM a -> Control a
mutate task = do
  (st, x) <- currentStackFrame >>= Execution.uninterruptible
    task
    VMError
    UnexpectedInterrupt
    mutateBlockchain
  updateVMState st
  return x


-- Blockchain interface
-- ---------------------------------------------------------------------

-- | Like `Blockchain.commitBlock`, but fails if there is a message call in progress
commitBlock :: Control ()
commitBlock = do
  stack <- getCallStack
  if length stack > 0
  then fail CallInProgress
  else mutateBlockchain Blockchain.commitBlock

-- | Run an operation in the Blockchain monad with the current blockchain state, keeping changes
mutateBlockchain :: Blockchain a -> Control a
mutateBlockchain ma = do
  blockchain <- getBlockchain
  let (blockchain', result) = runResultant ma blockchain
  setBlockchain blockchain'
  x <- point $ mapError BlockchainError result
  return x

-- | Run an operation in the Blockchain monad with the current blockchain state, discarding changes
queryBlockchain :: Blockchain a -> Control a
queryBlockchain ma = do
  blockchain <- getBlockchain
  let (blockchain', result) = runResultant ma blockchain
  x <- point $ mapError BlockchainError result
  return x


-- Monad state initialisation and access
-- ---------------------------------------------------------------------

initState :: State
initState = State
  { stBlockchain    = Blockchain.initState
  , stCallStack     = []
  , stCheckpoints   = initCheckpoints
  , stRunMode       = Run
  , stCallCont      = Nothing
  , stInterruptible = True
  , stAnalyticMode  = True
  , stMonitoring    = True
  }

showState :: State -> String
showState st = case stCallStack st of
  []     -> "idle"
  (vm:_) -> 
    let
      call   = VM.stCall vm
      callee = Format.stub $ msgCallee call
      caller = Format.stub $ msgCaller call
      depth  = show $ length (stCallStack st)
    in "running "
    ++ "(" ++ depth ++ ") "
    ++ caller ++ " => " ++ callee

getBlockchain       = stBlockchain    <$> getState
getCallStack        = stCallStack     <$> getState
getCheckpoints      = stCheckpoints   <$> getState
getRunMode          = stRunMode       <$> getState
getCallCont         = stCallCont      <$> getState
getInterruptible    = stInterruptible <$> getState
getAnalyticMode     = stAnalyticMode  <$> getState
getMonitoring       = stMonitoring    <$> getState

setBlockchain    x  = updateState (\st -> st { stBlockchain    = x }) :: Control ()
setCallStack     x  = updateState (\st -> st { stCallStack     = x }) :: Control ()
setCheckpoints   x  = updateState (\st -> st { stCheckpoints   = x }) :: Control ()
setRunMode       x  = updateState (\st -> st { stRunMode       = x }) :: Control ()
setCallCont      x  = updateState (\st -> st { stCallCont      = x }) :: Control ()
setInterruptible x  = updateState (\st -> st { stInterruptible = x }) :: Control ()

updateCallStack   f = getCallStack   >>= setCallStack   . f
updateCheckpoints f = getCheckpoints >>= setCheckpoints . f

