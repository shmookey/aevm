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
import Fluidity.EVM.Data.Bytecode (Op(Invalid))
import qualified Fluidity.EVM.VM as VM
import qualified Fluidity.EVM.Control as Control


-- | Information collected during a message call
data CallReport = CallReport
  { crMessageCall :: MessageCall
  , crInterrupts  :: [VM.Interrupt]
  , crResult      :: Result Control.Error ()
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
    , pmWroteStorage      = any isStorageWrite interrupts
    , pmReadStorage       = any isStorageRead interrupts
    , pmSentFunds         = any isSendFunds interrupts
    , pmMadeExtCall       = any isExternalCall interrupts
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
      Ok _ ->
        case find isReturnData interrupts of
          Just (VM.CallReturn x) -> Return x
          Nothing                -> Stop

      Err (Control.VMError e) ->
        case e of
          VM.InvalidJump _              -> InvalidJump
          VM.InvalidPC _                -> InvalidPC
          VM.OutOfGas                   -> OutOfGas
          VM.NotImplemented (Invalid _) -> InvalidOp
          VM.NotImplemented op          -> NotImplemented op
          _                             -> Other

      Err _ -> Other


-- Detectors
-- ---------------------------------------------------------------------

isReturnData :: VM.Interrupt -> Bool
isReturnData int = case int of
  VM.CallReturn _ -> True
  _               -> False

isStorageWrite :: VM.Interrupt -> Bool
isStorageWrite int = case int of
  VM.StorageWrite _ _ -> True
  _                   -> False

isStorageRead :: VM.Interrupt -> Bool
isStorageRead int = case int of
  VM.StorageRead _ _ -> True
  _                  -> False

isExternalCall :: VM.Interrupt -> Bool
isExternalCall int = case int of
  VM.ExternalCall _  -> True
  _                  -> False

isSendFunds :: VM.Interrupt -> Bool
isSendFunds int = case int of
  VM.ExternalCall (_, _, x, _, _, _) -> if uint x > 0 then True else False
  _                                  -> False


                          



