module Fluidity.EVM.REPL.EVM where

import Prelude hiding (fail, putStrLn)
import Control.DeepSeq
import Data.List (intercalate)
import Data.Semigroup ((<>))
import Data.ByteString (ByteString)
import Data.Time.Clock as Clock
import qualified Data.Maybe as M

import Control.Monad.Result
import Control.Monad.Resultant
import Text.Structured (typeset)

import Fluidity.EVM.REPL.Monad
import Fluidity.EVM.Types
import Fluidity.EVM.Data.Value
import Fluidity.EVM.Data.ByteField
import Fluidity.EVM.Data.Format (stub)
import Fluidity.EVM.Data.Transaction (MessageCall(..))
import Fluidity.EVM.Analyse.Outcome (PostMortem)
import Fluidity.EVM.Text ()
import qualified Fluidity.EVM.Analyse.Outcome as Outcome
import qualified Fluidity.EVM.Blockchain as Blockchain
import qualified Fluidity.EVM.Parallel as Parallel
import qualified Fluidity.EVM.Control as Control
import qualified Fluidity.EVM.REPL.Command as Cmd
import qualified Fluidity.EVM.VM as VM


runCommand :: Cmd.EVM -> REPL ()
runCommand cmd = case cmd of
  Cmd.Go           -> go
  Cmd.Step         -> step
  Cmd.BreakAt x    -> breakAt x
  Cmd.Call a v g d -> call a v g d
  Cmd.Inspect insp -> case insp of
    Cmd.InspectStack   -> inspectStack
    Cmd.InspectMemory  -> inspectMemory
    Cmd.InspectStorage -> inspectStorage
    Cmd.InspectCall    -> inspectCall
    Cmd.InspectCode r  -> inspectCode r


go :: REPL ()
go = yield Control.go

step :: REPL ()
step = yield Control.step

breakAt :: Integer -> REPL ()
breakAt = yield . Control.breakAt . fromInteger

call :: Cmd.Address -> Value -> Value -> ByteField -> REPL ()
call addr value gas calldata = do
  caller <- getAddress
  callee <- uniqueAddress addr
  let msg = MessageCall
              { msgCaller = caller
              , msgCallee = mkAddress callee
              , msgValue  = value
              , msgGas    = gas
              , msgData   = calldata
              }
  printLn msg
  yield $ Control.call msg

inspectStack :: REPL ()
inspectStack = do
  vmState <- query Control.currentStackFrame
  let stack = VM.stStack vmState
  printLn stack

inspectMemory :: REPL ()
inspectMemory = do
  vmState <- query Control.currentStackFrame
  let memory = VM.stMemory vmState
  printLn memory

inspectStorage :: REPL ()
inspectStorage = do
  vmState <- query Control.currentStackFrame
  let callee = msgCallee $ VM.stCall vmState
  storage <- queryBlockchain $ Blockchain.getStorage callee
  putStrLn (show storage)

inspectCall :: REPL ()
inspectCall = do
  vmState <- query Control.currentStackFrame
  let call = VM.stCall vmState
  printLn call

inspectCode :: Maybe Cmd.CodeRef -> REPL ()
inspectCode ref = do
  vmState <- query Control.currentStackFrame
  let call = VM.stCall vmState
  putStrLn (show vmState)


