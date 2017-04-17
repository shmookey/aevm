module Fluidity.EVM.Analyse.Formula where

import Prelude hiding (GT, LT)
import Data.List (intercalate)
import System.Console.ANSI
import qualified Data.ByteString as B

import Text.Structured (toString)

import Fluidity.Common.ANSI
import Fluidity.Common.Binary
import Fluidity.EVM.Analyse.Dependency as D
import Fluidity.EVM.Data.Prov as P
import Fluidity.EVM.Data.Format
import Fluidity.EVM.Data.Value (Value)
import qualified Fluidity.EVM.Data.Value as Value


showFormula :: Prov -> String
showFormula = snd . asFormula

intermediatesEnabled = False

fromValue :: Provenance a => a -> String
fromValue = showExpr . simplify . convert . prov

asFormula :: Prov -> (Int, String)
asFormula pv = case pv of
  Env t bs -> (10, showEnv t)
  Sys t bs -> (10, showSys t)
  Usr t bs -> (10, showUsr t)
  P.Ext t bs -> (10, showUsr t)

  BinOp t bs x y -> 
    let (prec, op) = binOp t
    in (prec, unwords [subFormula prec x, op, subFormula prec y])

  UnaOp t bs x -> 
    let (prec, op) = unaOp t
    in (prec, op ++ subFormula prec x)

  MemOp t bs x y z -> 
    let (prec, op) = memOp t
    in (prec, unwords [op, subFormula prec z])

  SliceRead bs p -> 
    if isByteSource p
    then (10, subFormula 0 p ++ showRange 0 (B.length bs))
    else (7, subFormula 7 p)

  TransRead bs n x -> 
    if isByteSource x
    then if isCode x
         then (10, showConstant bs)
         else (10, subFormula 0 x ++ showRange n (B.length bs))
    else
      (7, unwords [subFormula 7 x, showShift n])

  DirtyRead bs xs  -> 
    (7, intercalate " & " $ concatMap (\(n, x) -> [subFormula 7 x, showShift n]) xs)

  P.Nul -> (10, "0")

showExpr :: Expr -> String
showExpr expr = toString . colour Blue $ 
  let
    showE ex = case ex of
      D.Lit bs     -> (10,  smartStub bs)
      D.Var x      -> (10,  show x)
      D.Ext x      -> (10,  var x)
      D.Msg a b    -> (10,  "CallData[" ++ show a ++ ":" ++ show b ++ "]")
      D.Op2 t a b  -> let (prec, op) = binOp t
                      in (prec, sub prec a ++ " " ++ op ++ " " ++ sub prec b)
      D.Op1 t a    -> let (prec, op) = unaOp t
                      in (prec, op ++ sub prec a)
      D.SHA a      -> (8, "sha3 " ++ sub 8 a)
      D.Nul        -> (10, "null")

    var x = case x of
      D.Storage k v -> "Storage[" ++ smartStub k ++ "]"
      _           -> show x

    sub parentPrec ex = 
      let (childPrec, str) = showE ex
      in if childPrec > parentPrec
         then str
         else "(" ++ str ++ ")"

  in
    snd $ showE expr


structure :: Prov -> String
structure pv = case pv of
  Env t _ -> "Env " ++ show t
  Sys t _ -> "Sys " ++ show t
  Usr t _ -> "Usr " ++ show t
  P.Ext t _ -> "Ext " ++ show t
  BinOp t _ x y   -> "BinOp " ++ show t ++ "(" ++ structure x ++ ") (" ++ structure y ++ ")"
  UnaOp t _ x     -> "UnaOp " ++ show t ++ "(" ++ structure x ++ ")"
  MemOp t _ x y z -> "MemOp " ++ show t ++ "(" ++ structure x ++ ") (" ++ structure y ++ ") (" ++ structure z ++ ")"
  SliceRead _ p   -> "SliceRead (" ++ structure p ++ ")"
  TransRead _ n p -> "TransRead " ++ show n ++ " (" ++ structure p ++ ")"
  DirtyRead _ xs  -> "DirtyRead [" ++ (intercalate ", " $ map (\(i,x) -> "(" ++ show i ++ ", " ++ structure x ++ ")") xs) ++ "]"
  P.Nul -> "null"

showConstant :: B.ByteString -> String
showConstant bs =
  if B.length bs > 2
  then "0x" ++ toHex bs
  else show $ roll bs

subFormula :: Int -> Prov -> String
subFormula prec pv = 
  let
    val  = toHex $ valueAt pv
    val' = if length val > 8 then take 8 val ++ "..." else val
    (childPrec, formula) = asFormula pv
    suffix = if intermediatesEnabled && not (isLeaf pv)
             then "{" ++ val' ++ "}"
             else ""
  in
    if childPrec > prec
    then formula ++ suffix 
    else "(" ++ formula ++ ")" ++ suffix

showSys :: Sys -> String
showSys = show

showUsr :: Usr -> String
showUsr = show

showEnv :: Env -> String
showEnv = show

binOp :: BinOp -> (Int, String)
binOp x = case x of
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
unaOp x = case x of
  IsZero -> (8, "!")
  Not    -> (8, "~")
  Size   -> (8, "sizeof ")

memOp :: MemOp -> (Int, String)
memOp x = case x of
  SHA3 -> (8, "sha3")

showShift :: Int -> String
showShift x = op ++ " " ++ show (x * 8)
  where op = if x > 0 then "<<" else ">>"

showRange :: Int -> Int -> String
showRange i n = "[" ++ x ++ ":" ++ show (i+n) ++ "]"
  where x = if i == 0 then "" else show i


