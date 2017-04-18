{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module Fluidity.EVM.Analyse.Watchdog where

import GHC.Generics (Generic)
import Control.DeepSeq
import Data.ByteString (ByteString)

import Text.Structured (Structured(fmt), (~~), (~-), toString)

import Fluidity.Common.ANSI
import Fluidity.Common.Binary
import Fluidity.EVM.Analyse.Formula
import Fluidity.EVM.Data.Format
import Fluidity.EVM.Analyse.Dependency
import Fluidity.EVM.Data.Prov
import Fluidity.EVM.Data.Value
import Fluidity.EVM.Data.Bytecode (Op(..))
import Fluidity.EVM.Text ()
import qualified Fluidity.EVM.VM as VM


data Event
  = ControllableBranchPoint String
  | Branch Int Int Bool String
  | MethodDetected String
  | Store 
  | StorageWrite Int (ByteString, Expr) (ByteString, Expr)
  | StorageRead Int (ByteString, Expr) (ByteString, Expr)
  | GenericEvent String
  deriving (Show, Generic, NFData)

instance Structured Event where
  fmt ev = case ev of
    Branch pc jmp c x ->
      let path = if c then "YES" else "NO "
      in JumpI ~- fmtCodePtr jmp ~- path ~- x

    StorageRead p (k, ke) (v, ve) ->
      SLoad ~- shortSmart 8 k 
            ~- ">>" ~- smart' v 
            ~- "K=" ~~ emph "((" ~- showExpr ke ~- emph "))"
            ~- "V=" ~~ emph "((" ~- showExpr ve ~- emph "))"

    StorageWrite p (k, ke) (v, ve) ->
      SStore ~- shortSmart 8 k 
             ~- "<<" ~- smart' v
             ~- "K=" ~~ emph "((" ~- showExpr ke ~- emph "))"
             ~- "V=" ~~ emph "((" ~- showExpr ve ~- emph "))"

    MethodDetected x -> 
      "Callable method detected with hash:" ~- x


showE = simplify . convert

analyse :: VM.Interrupt -> Int -> [Event]
analyse vi pc = case vi of
  VM.ConditionalJump cond ptr ->
    let
      expr = showE $ prov cond
      cj = Branch pc (int ptr) (bool cond) (showExpr expr)
    in
      case matchMethodCall $ prov cond of
        Just x -> [cj, MethodDetected $ "0x" ++ toHex x]
        Nothing -> [cj]

  VM.StorageWrite k v ->
    let
      ke = showE $ prov k
      ve = showE $ prov v
    in
      return $ StorageWrite pc (bytes k, ke) (bytes v, ve)

  VM.StorageRead k v ->
    let
      ke = showE $ prov k
      ve = showE $ prov v
    in
      return $ StorageRead pc (bytes k, ke) (bytes v, ve)

  _ -> []


