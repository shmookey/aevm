{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module Fluidity.EVM.Parallel where

import Prelude hiding (fail)
import Control.Parallel.Strategies
import Control.DeepSeq
import GHC.Generics (Generic)
import Data.Functor.Identity (Identity(runIdentity))

import Control.Monad.Result
import Control.Monad.Resultant
import qualified Control.Monad.Execution as Execution

import Fluidity.EVM.Control (Control)
import Fluidity.EVM.Types
import Fluidity.EVM.Blockchain (MessageCall(..))
import Fluidity.EVM.Analyse.Outcome (CallReport(CallReport), PostMortem)
import qualified Fluidity.EVM.Control as Control
import qualified Fluidity.EVM.Analyse.Outcome as Outcome
import qualified Fluidity.EVM.VM as VM


type Task a = Resultant [VM.Interrupt] Control.Error a

data Report = Report [Control.Interrupt] (Result Control.Error ())
  deriving (Show, Generic, NFData)


data Error
  = DidNotComplete
  | ControlError String
  deriving (Show, Generic, NFData)

multicall :: [(MessageCall, Control.State)] -> [(Address, PostMortem, Control.State)]
multicall calls =
  let
    analyticMode state = state
      { Control.stInterruptible = False
      , Control.stAnalyticMode  = True
      }

    process :: MessageCall -> ([VM.Interrupt], Control.State, Result Control.Error ()) -> (Address, PostMortem, Control.State)
    process msg (ints, st, result) =
      (msgCallee msg, Outcome.postMortem (CallReport msg ints result), st)

    task :: (MessageCall, Control.State) -> (Address, PostMortem, Control.State)
    task (msg, state) = 
        process msg 
      . runTask 
      . createTask (analyticMode state) 
      $ Control.call msg
  in
    map task calls `using` parList rdeepseq


-- Base parallelisation functionality
-- ---------------------------------------------------------------------

runTask :: Task (Control.State, Result Control.Error a) -> ([VM.Interrupt], Control.State, Result Control.Error a)
runTask task = case runResultant task [] of
  (ints, Ok (st, r)) -> (ints, st, r)

createTask :: Control.State -> Control a -> Task (Control.State, Result Control.Error a)
createTask state ma = 
  let
    handleInterrupt :: Control.Interrupt -> Control.State -> Task Bool
    handleInterrupt i _ = case i of
      Control.VMInterrupt x -> (updateState $ (:) x) >> return True
      _                     -> return True

    runControl :: Identity a -> Task a
    runControl ma =
      return $ runIdentity ma

  in do
    (state', result) <- Execution.executeR
      ma
      runControl
      handleInterrupt
      state

    case result of
      Left cont     -> return (state', Err Control.InternalError) -- DidNotComplete -- shouldn't happen
      Right x       -> return (state', x)

