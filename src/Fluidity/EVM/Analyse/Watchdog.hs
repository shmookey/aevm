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
import Fluidity.EVM.Text ()
import Fluidity.EVM.Core.Interrupt (Interrupt)
import Fluidity.EVM.Data.Bytecode (Op)
import qualified Fluidity.EVM.Data.Bytecode as Op
import qualified Fluidity.EVM.Core.VM as VM
import qualified Fluidity.EVM.Core.Interrupt as INT


data Event
  = ControllableBranchPoint String
  | Branch Int Int Bool String
  | MethodDetected String
  | Store 
  | StorageWrite Int (ByteString, Expr) (ByteString, Expr)
  | StorageRead Int (ByteString, Expr) (ByteString, Expr)
  | Call Int (ByteString, Expr) (Integer, Expr) (ByteString, Expr)
  | GenericEvent String
  | Return (ByteString, Expr)
  | Stop
  deriving (Show, Generic, NFData)

instance Structured Event where
  fmt ev = case ev of
    Branch pc jmp c x ->
      let path = if c then "YES" else "NO "
      in Op.JumpI ~- fmtCodePtr jmp ~- path ~- x

    StorageRead p (k, ke) (v, ve) ->
      Op.SLoad ~- shortSmart 8 k 
               ~- ">>" ~- smart' v 
               ~- "K=" ~~ emph "((" ~- showExpr ke ~- emph "))"
               ~- "V=" ~~ emph "((" ~- showExpr ve ~- emph "))"

    StorageWrite p (k, ke) (v, ve) ->
      Op.SStore ~- shortSmart 8 k 
                ~- "<<" ~- smart' v
                ~- "K=" ~~ emph "((" ~- showExpr ke ~- emph "))"
                ~- "V=" ~~ emph "((" ~- showExpr ve ~- emph "))"

    Call p (to, toE) (amt, amtE) (bs, bsE) ->
      Op.Call ~- address to ~- currency' amt ~- bs
              ~- "TO="  ~~ emph "((" ~- showExpr toE  ~- emph "))"
              ~- "VAL=" ~~ emph "((" ~- showExpr amtE ~- emph "))"

    Return (bs, bsE) ->
      Op.Return ~- bs ~- emph "((" ~- showExpr bsE ~- emph "))"

    Stop -> fmt Op.Stop

    MethodDetected x -> 
      "Callable method detected with hash:" ~- x



showE = simplify . convert


analyse :: Interrupt -> Int -> [Event]
analyse vi pc = case vi of
  INT.JumpI cond ptr ->
    let
      expr = showE $ prov cond
      cj = Branch pc (int ptr) (bool cond) (showExpr expr)
    in
      case matchMethodCall $ prov cond of
        Just x -> [cj, MethodDetected $ "0x" ++ toHex x]
        Nothing -> [cj]

  INT.SStore k v ->
    let
      ke = showE $ prov k
      ve = showE $ prov v
    in
      return $ StorageWrite pc (bytes k, ke) (bytes v, ve)

  INT.SLoad k v ->
    let
      ke = showE $ prov k
      ve = showE $ prov v
    in
      return $ StorageRead pc (bytes k, ke) (bytes v, ve)

  INT.Call _ to amt bs _ _ ->
    let
      toE  = showE $ prov to
      amtE = showE $ prov amt
      bsE  = showE $ prov bs
    in
      return $ Call pc (bytes to, toE) (uint amt, amtE) (toBytes bs, bsE)

  INT.Return bs -> return $ Return (toBytes bs, showE $ prov bs)

  INT.Stop -> return Stop

  _ -> []


