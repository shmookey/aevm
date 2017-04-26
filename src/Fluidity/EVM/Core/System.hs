{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module Fluidity.EVM.Core.System where

import Prelude hiding (fail)
import Data.Functor.Identity (Identity, runIdentity)
import GHC.Generics (Generic)
import Control.DeepSeq
import Control.Monad (when, unless, void)

import Control.Monad.Result
import Control.Monad.If
import Control.Monad.Resultant
import Control.Monad.Execution (Execution, execute, executeLog, alwaysBreak)
import Control.Monad.Interruptible ()
import qualified Control.Monad.Execution as E

import Fluidity.EVM.Core.Blockchain (Blockchain)
import Fluidity.EVM.Core.VM (VM)
import Fluidity.EVM.Core.Interrupt (Interrupt, IntFlags)
import qualified Fluidity.EVM.Core.Blockchain as Blockchain
import qualified Fluidity.EVM.Core.Interrupt as INT
import qualified Fluidity.EVM.Core.VM as VM
import qualified Fluidity.EVM.Data.Transaction as Tx


type Sys = Execution Identity Interrupt State Error

data State = State
  { stChain  :: Blockchain.State
  , stStack  :: [VM.State]
  , stMode   :: Mode
  , stConfig :: Config
  , stLast   :: Maybe (Blockchain.State, [VM.State])
  } deriving (Show, Generic, NFData)

data Mode = Run | Step | Until Int | Finalizing Interrupt Mode | Preempted Mode
  deriving (Show, Generic, NFData)

data Break a
  = Done a
  | Fail Error
  | Susp Interrupt
  deriving (Show, Generic, NFData)

data Error
  = BlockchainError Blockchain.Error
  | VMError         VM.Error
  | Interrupted     Interrupt
  | ConfigError     String
  | InternalError   String
  | Idle
  | Busy
  deriving (Eq, Show, Generic, NFData)

instance SubError Error Blockchain.Error where
  suberror = BlockchainError


-- | A pure "run" for the Sys monad, breaks on echoed interrupts
run :: Sys a -> State -> (State, Break a)
run ma st =
  let (st', result) = runIdentity $ execute ma id alwaysBreak st
  in (,) st' $ case result of
    Left (int, _) -> Susp int
    Right (Err e) -> Fail e
    Right (Ok x)  -> Done x

-- | Like `run`, but accumulates a list of interrupts instead of echoing them
runLog :: Sys a -> State -> ([Interrupt], Break a)
runLog ma st =
  let (_, ints, result) = runIdentity $ executeLog ma id st
  in case result of
    Err e -> (ints, Fail e)
    Ok x  -> (ints, Done x)


-- Execution control
-- ---------------------------------------------------------------------

-- | Resume the active task and run until it stops
go :: Sys ()
go = do
  mode <- getMode
  case mode of
    Preempted _ -> setMode $ Preempted Run
    _           -> setMode Run
  resume

-- | Resume the active task for one step
step :: Sys ()
step = do
  mode <- getMode
  case mode of
    Preempted _ -> setMode $ Preempted Step
    _           -> setMode Step
  resume

-- | Resume the active task until a given address is reached
breakAt :: Int -> Sys ()
breakAt x = do
  mode <- getMode
  case mode of
    Preempted _ -> setMode . Preempted $ Until x
    _           -> setMode $ Until x
  resume


-- Interrupts
-- ---------------------------------------------------------------------

-- | Respond to a VM interrupt, updating the call stack with the new VM state
onInterrupt :: Interrupt -> VM.State -> Sys (Maybe Interrupt)
onInterrupt int st = do
  action        <- getInterruptAction
  ipoint        <- getInterruptPoint
  awaiting      <- isAwaitingFinalization
  interruptible <- isInterruptible int
  newCycle      <- return $ isNewCycle int

  unless (ipoint == Preempt && interruptible) $ do
    flush st
    when newCycle setRollbackPoint

  if awaiting && newCycle
  then finalize
  else if interruptible
  then case action of
    Ignore -> maybeBreak int
    Echo -> case ipoint of
      Immediate -> echo int >> maybeBreak int
      Preempt   -> preemptSafe (rollback >> echo int >> maybeBreak int)
      Finalize  -> do if newCycle then echo int else defer int
                      maybeBreak int
    Break -> case ipoint of
      Immediate -> return $ Just int
      Preempt   -> preemptSafe (rollback >> return (Just int))
      Finalize  -> defer int >> return Nothing
  else maybeBreak int

finalize :: Sys (Maybe Interrupt)
finalize = do
  mode   <- getMode
  action <- getInterruptAction
  case (action, mode) of
    (Break, Finalizing x m) -> setMode m >> return (Just x)
    (Echo,  Finalizing x m) -> setMode m >> echo x >> return Nothing
    _ -> fail $ InternalError "nothing to finalize"

isAwaitingFinalization :: Sys Bool
isAwaitingFinalization = flip fmap getMode $ \case
  Finalizing _ _ -> True
  _              -> False

preemptSafe :: Sys (Maybe Interrupt) -> Sys (Maybe Interrupt)
preemptSafe ma = do
  mode <- getMode
  case mode of
    Preempted mode' -> setMode mode' >> ma -- return Nothing
    _               -> setMode (Preempted mode) >> ma

maybeBreak :: Interrupt -> Sys (Maybe Interrupt)
maybeBreak x = flip fmap getMode $ \m -> case (m, x) of
  (Step   , _          )          -> Just x
  (Until a, INT.Cycle b) | a == b -> Just x
  _                               -> Nothing

isNewCycle :: Interrupt -> Bool
isNewCycle x = case x of
  INT.Cycle _ -> True
  INT.Ready   -> True
  _           -> False

defer :: Interrupt -> Sys ()
defer x = do
  mode  <- getMode
  ifM (isInterruptible x) $ setMode (Finalizing x mode)

echo :: Interrupt -> Sys ()
echo x = ifM (isInterruptible x) $ E.interrupt x

isInterruptible :: Interrupt -> Sys Bool
isInterruptible x = do
  byInterrupts <- INT.interruptible x <$> getInterrupts
  byAction     <- (/= Ignore) <$> getInterruptAction
  return $ byInterrupts && byAction

setRollbackPoint :: Sys ()
setRollbackPoint = do
  chain <- getChain
  stack <- getStack
  setLast $ Just (chain, stack)

rollback :: Sys ()
rollback = do
  (chain, stack) <- getLast >>= fromMaybe (InternalError "rollback failure")
  setChain chain
  setStack stack

addEssentials :: IntFlags -> IntFlags
addEssentials x = x { INT.intCycle = True , INT.intReady = True }

-- Call stack
-- ---------------------------------------------------------------------

pop :: Sys VM.State
pop = do
  x <- peek
  updateStack tail
  return x

push :: VM.State -> Sys ()
push x = updateStack $ (:) x

peek :: Sys VM.State
peek = getStack >>= \cs -> case cs of
  []    -> fail Idle
  x : _ -> return x

flush :: VM.State -> Sys ()
flush x = pop >> push x

running :: Sys Bool
running = getStack >>= \cs -> case cs of
  []    -> return False
  x : _ -> return True

fresh :: Tx.MessageCall -> Sys VM.State
fresh msg = do
  flags <- addEssentials <$> getInterrupts
  let vm = (VM.initState msg) { VM.stIntFlags = flags }
  push vm
  return vm


-- VM Interface
-- --------------------------------------------------------------------

-- | Message call into a VM 
call :: Tx.MessageCall -> Sys ()
call msg = mustBeIdle $ do
  vm <- fresh msg
  setRollbackPoint
  (vm', result) <- execute VM.call mutateChain onInterrupt vm
--  vm'' <- peek
--  E.interrupt . INT.Alert $ "Execution returned VM state: " ++ show vm'
--  E.interrupt . INT.Alert $ "Already had VM state: " ++ show vm''
 -- flush vm'
  case result of
    Left _        -> return ()
    Right (Ok _)  -> pop >> setLast Nothing
    Right (Err e) -> pop >> setLast Nothing >> fail (VMError e)

-- | Resume the active call
resume :: Sys ()
resume = do
  unlessM running $ fail Idle
  (vm, result) <- peek >>= execute VM.resume mutateChain onInterrupt
--  flush vm
  case result of
    Left _        -> return ()
    Right (Ok _)  -> pop >> setLast Nothing
    Right (Err e) -> pop >> setLast Nothing >> fail (VMError e)

-- | Run a VM computation without interrupts, discarding the final state
query :: VM a -> Sys a
query ma = peek >>= E.query ma VMError Interrupted queryChain

-- | Run a VM computation without interrupts, preserving the final state
mutate :: VM a -> Sys a
mutate ma = do
  (st, x) <- peek >>= E.uninterruptible ma VMError Interrupted mutateChain
  flush st
  return x

mustBeIdle :: Sys a -> Sys a
mustBeIdle ma = ifM running (fail Busy) >> ma

mustBeActive :: Sys a -> Sys a
mustBeActive ma = ifM (not <$> running) (fail Idle) >> ma

assertIdle :: Sys ()
assertIdle = mustBeIdle $ return ()

assertActive :: Sys ()
assertActive = mustBeActive $ return ()


-- Blockchain interface
-- ---------------------------------------------------------------------

-- | Like `Blockchain.commitBlock`, but fails if there is a message call in progress
commitBlock :: Sys ()
commitBlock = mustBeIdle $ mutateChain Blockchain.commitBlock

-- | Run an operation in the Blockchain monad with the current blockchain state, keeping changes
mutateChain :: Blockchain a -> Sys a
mutateChain ma = do
  (chain, result) <- runResultant ma <$> getChain
  setChain chain
  subpoint result

-- | Run an operation in the Blockchain monad with the current blockchain state, discarding changes
queryChain :: Blockchain a -> Sys a
queryChain ma = do
  (_, result) <- runResultant ma <$> getChain
  subpoint result


-- Configiguration and monad state
-- --------------------------------------------------------------------

data Config = Config
  { cInterrupts        :: IntFlags
  , cInterruptAction   :: InterruptAction
  , cInterruptPoint    :: InterruptPoint
  } deriving (Show, Generic, NFData)

data InterruptAction
  = Echo    -- Pass interrupts upwards (real-time)
  | Break   -- Break on interrupts
  | Ignore  -- Silently drop interrupts
  deriving (Eq, Show, Generic, NFData)

data InterruptPoint
  = Immediate -- Act as soon as the interrupt is received
  | Preempt   -- Roll back to start of current cycle, then act
  | Finalize  -- Wait until end of cycle before acting
  deriving (Eq, Show, Generic, NFData)

initState :: State
initState = State
  { stChain   = Blockchain.initState
  , stStack   = []
  , stMode    = Run
  , stLast    = Nothing
  , stConfig  = Config
    { cInterrupts        = INT.defaults
    , cInterruptAction   = Echo
    , cInterruptPoint    = Preempt
    }
  }

syncFlags :: Sys ()
syncFlags = do
  flags <- addEssentials <$> getInterrupts
  updateStack . map $ \x -> x { VM.stIntFlags = flags }

enableInterrupt       x = updateInterrupts (INT.enable x)
disableInterrupt      x = updateInterrupts (INT.disable x)
getInterruptAction      = cInterruptAction <$> getConfig
getInterruptPoint       = cInterruptPoint  <$> getConfig
getInterrupts           = cInterrupts      <$> getConfig
setInterruptAction    x = updateConfig (\c -> c { cInterruptAction = x })
setInterruptPoint     x = updateConfig (\c -> c { cInterruptPoint  = x })
setInterrupts         x = updateConfig (\c -> c { cInterrupts      = x }) >> syncFlags
updateInterrupts      f = getInterrupts >>= setInterrupts . f

getChain       = stChain   <$> getState
getStack       = stStack   <$> getState
getMode        = stMode    <$> getState
getLast        = stLast    <$> getState
getConfig      = stConfig  <$> getState
setChain     x = updateState (\st -> st { stChain   = x }) :: Sys ()
setStack     x = updateState (\st -> st { stStack   = x }) :: Sys ()
setMode      x = updateState (\st -> st { stMode    = x }) :: Sys ()
setLast      x = updateState (\st -> st { stLast    = x }) :: Sys ()
setConfig    x = updateState (\st -> st { stConfig  = x }) :: Sys ()
updateStack  f = getStack  >>= setStack . f
updateConfig f = getConfig >>= setConfig . f

