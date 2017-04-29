{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module Fluidity.EVM.Core.System where

import Prelude hiding (fail)
import Data.Functor.Identity (Identity, runIdentity)
import GHC.Generics (Generic)
import Control.DeepSeq
import Data.List (delete, nub)
import Control.Monad (when, unless, void)

import Control.Monad.Result
import Control.Monad.If
import Control.Monad.Resultant
import Control.Monad.Execution (Execution, execute, executeLog, alwaysBreak)
import Control.Monad.Interruptible ()
import Control.Monad.Execution
import qualified Control.Monad.Execution as Exec

import Fluidity.EVM.Core.Blockchain (Blockchain)
import Fluidity.EVM.Core.VM (VM)
import Fluidity.EVM.Core.Interrupt (Interrupt, Action(..), IntConfig)
import qualified Fluidity.EVM.Core.Blockchain as Blockchain
import qualified Fluidity.EVM.Core.Interrupt as I
import qualified Fluidity.EVM.Core.VM as VM
import qualified Fluidity.EVM.Data.Transaction as Tx


type Sys = Execution Identity Interrupt State Error

data State = State
  { stChain  :: Blockchain.State
  , stStack  :: [VM.State]
  , stStatus :: Status
  , stConfig :: Config
  } deriving (Show, Generic, NFData)

data Status 
  = Running                                    -- Running normally
  | Preempted                                  -- Rolled back to start of current cycle
  | Waiting Action Interrupt                   -- Waiting for cycle to end to raise interrupt
  | Preemptible Blockchain.State [VM.State]    -- Able to roll back to start of current cycle
  deriving (Eq, Show, Generic, NFData)

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

data Config = Config
  { cInterrupts  :: IntConfig
  , cStrategy    :: Strategy
  , cBreakpoints :: [Int]
  } deriving (Show, Generic, NFData)

data Strategy
  = Immediate -- Act as soon as the interrupt is received
  | Preempt   -- Roll back to start of current cycle, then act
  | Wait  -- Wait until end of cycle before acting
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


-- Interrupts
-- ---------------------------------------------------------------------

-- | Respond to a VM interrupt, updating the call stack with the new VM state
onInterrupt :: Interrupt -> VM.State -> Sys (Maybe Interrupt)
onInterrupt i st = beforeBreakpoints i $ do
  flush st
  pt   <- getStrategy
  act  <- getInterruptAction i
  stat <- getStatus

  case pt of
    Immediate -> case act of Echo   -> interrupt i >> return Nothing
                             Break  -> return (Just i)
                             Ignore -> return Nothing

    Preempt -> do
      case i of I.Cycle _ -> makePreemptible
                I.Ready   -> makePreemptible
                _         -> return () 
      case act of Echo   -> preempt >> interrupt i >> return Nothing
                  Break  -> preempt >> return (Just i)
                  Ignore -> return Nothing

    Wait -> case (stat, i) of
      (Waiting act' i', I.Cycle _) -> do
        setStatus Running
        case act' of Echo   -> interrupt i' >> return Nothing
                     Break  -> return (Just i')
                     Ignore -> return Nothing

      _ -> do
        when (act /= Ignore) $ setStatus (Waiting act i)
        return Nothing

beforeBreakpoints :: Interrupt -> Sys (Maybe Interrupt) -> Sys (Maybe Interrupt)
beforeBreakpoints i ma = case i of { I.Cycle n -> f n ; _ -> ma }
  where f x = do bs <- getBreakpoints
                 flip fmap ma $ \case Just r              -> Just r
                                      Nothing | elem x bs -> Just i
                                      _                   -> Nothing

makePreemptible :: Sys ()
makePreemptible = do
  chain <- getChain
  stack <- getStack
  setStatus $ Preemptible chain stack

preempt :: Sys ()
preempt = getStatus >>= \case
  Preemptible chain stack -> do 
    setChain chain
    setStack stack
    setStatus Preempted
  _ -> fail $ InternalError "non-preemptible state"


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

isActive :: Sys Bool
isActive = getStack >>= \cs -> case cs of
  []    -> return False
  x : _ -> return True

fresh :: Tx.MessageCall -> Sys VM.State
fresh msg = do
  let vm = VM.initState msg
  push vm
  return vm

mustBeIdle :: Sys a -> Sys a
mustBeIdle ma = ifM isActive (fail Busy) >> ma

mustBeActive :: Sys a -> Sys a
mustBeActive ma = ifM (not <$> isActive) (fail Idle) >> ma

assertIdle = mustBeIdle $ return ()
assertActive = mustBeActive $ return ()


-- VM Interface
-- --------------------------------------------------------------------

-- | Message call into a VM 
call :: Tx.MessageCall -> Sys ()
call msg = mustBeIdle $ do
  vm <- fresh msg
  (vm', result) <- execute VM.call mutateChain onInterrupt vm
  case result of
    Left _        -> return ()
    Right (Ok _)  -> pop >> setStatus Running
    Right (Err e) -> pop >> setStatus Running >> fail (VMError e)

-- | Resume the active call
resume :: Sys ()
resume = mustBeActive $ do
  status <- getStatus
  when (status /= Preempted) makePreemptible
  (_, result) <- peek >>= execute VM.resume mutateChain onInterrupt
  case result of
    Left _        -> return ()
    Right (Ok _)  -> pop >> setStatus Running
    Right (Err e) -> pop >> setStatus Running >> fail (VMError e)

-- | Run a VM computation without interrupts, discarding the final state
query :: VM a -> Sys a
query ma = peek >>= Exec.query ma VMError Interrupted queryChain

-- | Run a VM computation without interrupts, preserving the final state
mutate :: VM a -> Sys a
mutate ma = do
  (st, x) <- peek >>= Exec.uninterruptible ma VMError Interrupted mutateChain
  flush st
  return x


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

initState :: State
initState = State
  { stChain  = Blockchain.initState
  , stStack  = []
  , stStatus = Running
  , stConfig = Config
    { cInterrupts  = I.defaults
    , cStrategy    = Preempt
    , cBreakpoints = []
    }
  }

getInterruptAction    x = I.action x   <$> getInterrupts
getStrategy             = cStrategy    <$> getConfig
getInterrupts           = cInterrupts  <$> getConfig
getBreakpoints          = cBreakpoints <$> getConfig
setStrategy           x = updateConfig (\c -> c { cStrategy    = x })
setInterrupts         x = updateConfig (\c -> c { cInterrupts  = x })
setBreakpoints        x = updateConfig (\c -> c { cBreakpoints = x })
updateInterrupts      f = getInterrupts  >>= setInterrupts  . f
updateBreakpoints     f = getBreakpoints >>= setBreakpoints . f
setInterruptAction  i x = updateInterrupts $ I.setAction i x
setBreakpoint         x = updateBreakpoints $ \xs -> nub (x:xs)
clearBreakpoint       x = updateBreakpoints $ \xs -> delete x xs

getChain       = stChain  <$> getState
getStack       = stStack  <$> getState
getStatus      = stStatus <$> getState
getConfig      = stConfig <$> getState
setChain     x = updateState (\st -> st { stChain  = x }) :: Sys ()
setStack     x = updateState (\st -> st { stStack  = x }) :: Sys ()
setStatus    x = updateState (\st -> st { stStatus = x }) :: Sys ()
setConfig    x = updateState (\st -> st { stConfig = x }) :: Sys ()
updateStack  f = getStack  >>= setStack  . f
updateConfig f = getConfig >>= setConfig . f

