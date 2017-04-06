module Fluidity.EVM.REPL.EVM where

import Prelude hiding (fail, putStrLn)
import Control.DeepSeq
import Data.List (intercalate)
import Data.Semigroup ((<>))
import Data.ByteString (ByteString)
import Data.Time.Clock as Clock

import Control.Monad.Result
import Control.Monad.Resultant
import Text.Structured (typeset)

import Confound.Methods (methodHash')
import Fluidity.Common.Binary (padBytes, unroll)
import Fluidity.EVM.REPL.Monad
import Fluidity.EVM.Types
import Fluidity.EVM.Data (Value, ByteField, formatStub)
import Fluidity.EVM.Blockchain (MessageCall(..))
import Fluidity.EVM.Analyse.Outcome (PostMortem)
import Fluidity.EVM.Text ()
import qualified Fluidity.EVM.Analyse.Outcome as Outcome
import qualified Fluidity.EVM.Blockchain as Blockchain
import qualified Fluidity.EVM.Parallel as Parallel
import qualified Fluidity.EVM.Control as Control
import qualified Fluidity.EVM.REPL.Command as Cmd
import qualified Fluidity.EVM.Data as Data
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

call :: Cmd.Address -> Integer -> Maybe Integer -> Maybe Cmd.CallData -> REPL ()
call addr value gas calldata =
  let
    value'    = Data.mkCallValue value
    gas'      = case gas of Just g  -> Data.mkInitialGas g
                            Nothing -> Data.mkInitialGas 1000
    calldata' = case calldata of Just cd -> encodeCallData cd
                                 Nothing -> Data.fresh
  in do
    caller  <- getAddress
    callees <- matchingAddresses addr
    case callees of
      [callee] -> yield . Control.call
        $ MessageCall
          { msgCaller = caller
          , msgCallee = callee
          , msgValue  = value'
          , msgGas    = gas'
          , msgData   = calldata'
          }
      _ -> fail $ InternalError

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


encodeCallData :: Cmd.CallData -> CallData
encodeCallData cd =
  let
    -- for now just use the Confound version
    encodeMethod :: String -> [Cmd.MethodArg] -> CallData
    encodeMethod name args =
      let
        argsStr = intercalate "," $ map argType args
        sig     = name ++ "(" ++ argsStr ++ ")"
        hash    = methodHash' sig
      in
        asByteField $ hash <> mconcat (map argVal args)

    argVal :: Cmd.MethodArg -> ByteString
    argVal arg = case arg of
      Cmd.NumArg v _    -> padBytes 32 $ unroll v
      Cmd.BoolArg True  -> padBytes 32 $ unroll (1 :: Int)
      Cmd.BoolArg False -> padBytes 32 $ unroll (0 :: Int)
      Cmd.AddrArg addr  -> padBytes 32 addr

    argType :: Cmd.MethodArg -> String
    argType arg = case arg of
      Cmd.NumArg _ t -> t
      Cmd.BoolArg _  -> "bool"
      Cmd.AddrArg _  -> "address"
    
    asByteField :: ByteString -> ByteField
    asByteField = Data.prefill Data.CallData

  in case cd of
    Cmd.RawCall bs -> asByteField bs
    Cmd.MethodCall x args -> encodeMethod x args

