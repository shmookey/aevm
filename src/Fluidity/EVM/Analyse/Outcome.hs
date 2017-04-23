{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module Fluidity.EVM.Analyse.Outcome where

import Data.List (find, intercalate)
import Data.Maybe (catMaybes)
import GHC.Generics (Generic)
import Control.DeepSeq

import Control.Monad.Result

import Fluidity.EVM.Types (Address)
import Fluidity.EVM.Data.Transaction (MessageCall(..))
import Fluidity.EVM.Data.Value (Value, uint)
import Fluidity.EVM.Data.ByteField (ByteField)
import Fluidity.EVM.Data.Bytecode (Op)
import Fluidity.EVM.Core.System (Break)
import Fluidity.EVM.Core.Interrupt (Interrupt)
import qualified Fluidity.EVM.Core.VM as VM
import qualified Fluidity.EVM.Data.Bytecode as BC
import qualified Fluidity.EVM.Core.System as Sys
import qualified Fluidity.EVM.Core.Interrupt as INT


-- | Information collected during a message call
data CallReport = CallReport
  { crMessageCall :: MessageCall
  , crInterrupts  :: [Interrupt]
  , crResult      :: Break ()
  }

data CauseOfDeath
  = Stop
  | Return ByteField
  | InvalidJump
  | OutOfGas
  | NotImplemented Op
  | InvalidPC
  | InvalidOp
  | Other
  | Invalid String
  deriving (Show, Generic, NFData)

data PostMortem = PostMortem
  { pmCauseOfDeath      :: CauseOfDeath
  , pmGracefulHalt      :: Bool
  , pmNeedsImpl         :: Bool
  , pmProbableThrow     :: Bool
  , pmWroteStorage      :: Bool
  , pmReadStorage       :: Bool
  , pmSentFunds         :: Bool
  , pmMadeExtCall       :: Bool
--  , pmCheckedGas        :: Bool
--  , pmReadSourceAddress :: Bool
--  , pmReadOwnAddress    :: Bool
--  , pmReadCallData      :: Bool
--  , pmHashedData        :: Bool
--  , pmStoreThenCall     :: Bool
  } deriving (Show, Generic, NFData)

postMortem :: CallReport -> PostMortem
postMortem report =
  let
    causeOfDeath = determineCauseOfDeath report
    interrupts   = crInterrupts report
    msgcall      = crMessageCall report
    callee       = msgCallee msgcall
  in PostMortem
    { pmCauseOfDeath      = determineCauseOfDeath report
    , pmGracefulHalt      = isGraceful causeOfDeath
    , pmNeedsImpl         = isNotImplemented causeOfDeath
    , pmProbableThrow     = isProbableThrow causeOfDeath
    , pmWroteStorage      = any INT.isSStore interrupts
    , pmReadStorage       = any INT.isSLoad  interrupts
    , pmMadeExtCall       = any INT.isCall   interrupts
    , pmSentFunds         = any isSendFunds  interrupts
--    , pmCheckedGas        = 
--    , pmReadSourceAddress = 
--    , pmReadOwnAddress    = 
--    , pmReadCallData      = 
--    , pmHashedData        =
--    , pmStoreThenCall     =
    }

explainPostMortem :: PostMortem -> String
explainPostMortem pm = intercalate ", " $ catMaybes
  [ Just . explainCauseOfDeath $ pmCauseOfDeath pm
  , if pmProbableThrow     pm then Just "probably threw"        else Nothing
  , if pmReadStorage       pm then Just "read from storage"     else Nothing
  , if pmWroteStorage      pm then Just "wrote to storage"      else Nothing
  , if pmMadeExtCall       pm then Just "made an external call" else Nothing
  , if pmSentFunds         pm then Just "sent funds"            else Nothing
  ]

explainCauseOfDeath :: CauseOfDeath -> String
explainCauseOfDeath cod = "Halted " ++ case cod of
  Stop                -> "normally, no return data"
  Return bf           -> "normally, with return data"
  InvalidJump         -> "by an invalid jump"
  OutOfGas            -> "because it ran out of gas"
  NotImplemented op   -> "because the " ++ (show op) ++ " operation is not implemented"
  InvalidPC           -> "abnormally, due to end-of-code"
  InvalidOp           -> "by an invalid opcode"
  Other               -> "for an unclear reason"
  Invalid e           -> "due to an internal error (" ++ show e ++ ")"

isGraceful :: CauseOfDeath -> Bool
isGraceful cod = case cod of
  Stop     -> True
  Return _ -> True
  _        -> False

isProbableThrow :: CauseOfDeath -> Bool
isProbableThrow cod = case cod of
  InvalidJump -> True
  InvalidOp   -> True
  _           -> False

isNotImplemented :: CauseOfDeath -> Bool
isNotImplemented cod = case cod of
  NotImplemented _ -> True
  _                -> False

determineCauseOfDeath :: CallReport -> CauseOfDeath
determineCauseOfDeath report =
  let
    interrupts = crInterrupts report
  in
    case crResult report of
      Sys.Done  _ -> case find INT.isReturn interrupts of
        Just (INT.Return x) -> Return x -- todo: has to be the last one
        Nothing             -> Stop

      Sys.Fail e -> case e of
        Sys.VMError ve -> case ve of
          VM.InvalidJump _                 -> InvalidJump
          VM.InvalidPC _                   -> InvalidPC
          VM.OutOfGas                      -> OutOfGas
          VM.NotImplemented (BC.Invalid _) -> InvalidOp
          VM.NotImplemented op             -> NotImplemented op
          _                                -> Other
        _ -> Other

      Sys.Susp _ -> Invalid "suspended"

isSendFunds :: Interrupt -> Bool
isSendFunds int = case int of
  INT.Call _ _ x _ _ _ -> if uint x > 0 then True else False
  _                    -> False



