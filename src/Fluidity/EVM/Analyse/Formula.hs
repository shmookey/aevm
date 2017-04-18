module Fluidity.EVM.Analyse.Formula where

import Prelude hiding (GT, LT)
import Data.List (intercalate)
import System.Console.ANSI
import qualified Data.ByteString as B

import Text.Structured (toString)

import Fluidity.Common.ANSI
import Fluidity.Common.Binary
import Fluidity.EVM.Analyse.Dependency as D
import Fluidity.EVM.Data.Format
import Fluidity.EVM.Data.Value (Value)
import Fluidity.EVM.Data.Prov (Provenance, prov, BinOp(..), UnaOp(..))
import qualified Fluidity.EVM.Data.Prov as P
import qualified Fluidity.EVM.Data.Value as Value


intermediatesEnabled = False

fromValue :: Provenance a => a -> String
fromValue = showExpr . simplify . convert . prov

showExpr :: Expr -> String
showExpr expr = 
  let
    showE ex = case ex of
      D.Lit bs     -> (10, toString . colour Blue $ smartStub bs)
      D.Var s x    -> (10,  var s x)
      D.Op2 t a b  -> let (prec, op) = binOp t
                      in (prec, sub prec a ++ " " ++ op ++ " " ++ sub prec b)
      D.Op1 t a    -> let (prec, op) = unaOp t
                      in (prec, op ++ sub prec a)
      D.SHA a      -> (8, symbol "sha3 " ++ sub 8 a)
      D.Cat es     -> let terms = intercalate "," $ map (sub 0) es
                      in (10, symbol "[" ++ terms ++ symbol "]")
      D.Cut a b e  -> let a' = if a == 0 then "" else show a
                      in (9, sub 9 e ++ symbol "[" ++ a' ++ symbol ":" ++ show b ++ symbol "]")
      D.Pad n      -> (10, symbol $ "null{" ++ show n ++ "}")
      D.Clr e      -> (10, symbol ".." ++ sub 8 e)
      D.Nul        -> (10, symbol "null")

    var s x = toString . colour (source s) $ case x of
      D.Storage k -> "${" ++ smartStub k ++ "}"
      _           -> show x

    source s = case s of
      Usr -> Green
      Env -> Cyan
      Ext -> Yellow

    sub parentPrec ex = 
      let (childPrec, str) = showE ex
      in if childPrec > parentPrec
         then str
         else symbol "(" ++ str ++ symbol ")"

  in
    snd $ showE expr

symbol :: String -> String
symbol = toString . highlight Red

binOp :: BinOp -> (Int, String)
binOp x = (p, symbol s)
  where 
    (p, s) = case x of
      Add  -> (6, "+")
      Sub  -> (6, "-") 
      Mul  -> (7, "*")
      Div  -> (7, "/")
      SDiv -> (7, "/")
      Mod  -> (7, "%")
      Exp  -> (7, "**")
      Eq   -> (5, "==")
      GT   -> (5, ">")
      LT   -> (5, "<")
      SGT  -> (5, ">")
      SLT  -> (5, "<")
      And  -> (7, "&")
      Or   -> (7, "|")
      Xor  -> (7, "^")
      Byte -> (7, "!!")

unaOp :: UnaOp -> (Int, String)
unaOp x = (p, symbol s)
  where 
    (p, s) = case x of
      IsZero -> (8, "!")
      Not    -> (8, "~")
      Size   -> (8, "sizeof ")


