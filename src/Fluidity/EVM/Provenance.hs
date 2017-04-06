module Fluidity.EVM.Provenance where

import Prelude hiding (GT, LT)
import Data.List (groupBy, intercalate)

import Fluidity.EVM.Data


mathValue :: Value -> String
mathValue = mathProv . provenance

mathProv :: Prov -> String
mathProv prov = case prov of
  Push   i v              -> show v                     
  Add    v1 v2            -> binop "+" v1 v2 
  Mul    v1 v2            -> binop "*" v1 v2  
  Sub    v1 v2            -> binop "-" v1 v2  
  Div    v1 v2            -> binop "/" v1 v2  
  Exp    v1 v2            -> binop "^" v1 v2  
  IsZero v                -> unary "IsZero" v
  GT     v1 v2            -> binop "+" v1 v2 
  LT     v1 v2            -> binop "+" v1 v2 
  SGT    v1 v2            -> binop "+" v1 v2 
  SLT    v1 v2            -> binop "+" v1 v2 
  And    v1 v2            -> binop "+" v1 v2 
  Or     v1 v2            -> binop "+" v1 v2 
  Xor    v1 v2            -> binop "+" v1 v2 
  Not    v                -> unary "NOT" v
  ByteOf v1 v2            -> binop "+" v1 v2 

  SHA3   v vs             -> "SHA3(" ++ mathProvRange vs ++ ")"
 
  Block                   -> "Block"
  BlockNumber             -> "BlockNumber"
  BlockHash               -> "BlockHash"
  Balance                 -> "Balance" 
  RetVal                  -> "RetVal" 
  BlockTime               -> "BlockTime"

  Caller                  -> "Caller"
  CallValue               -> "CallValue"
  CallData                -> "CallData"
  CallDataSize            -> "CallDataSize"
  CallDataCopy v1 v2 v3 p -> mathProv p
  CallDataLoad v p        -> mathProv p
  CallCopy v1 v2 p        -> mathProv p

  MLoad v vs              -> mathProvRange vs
  MStore v1 v2            -> mathValue v2
  MStore8 v1 v2           -> mathValue v2
  Storage                 -> "Storage"
  SStore v1 v2            -> mathValue v2
  SLoad v1 v2             -> mathValue v2
  Code                    -> "Code" 
  CodeCopy v1 v2 v3 p     -> mathProv p

  Blockchain              -> "Blockchain"
  GasMeter                -> "GasMeter"
  InitialGas              -> "InitialGas"
  LogCopy v1 v2 p         -> mathProv p
  
  Nil                     -> "00"
  
  DebugInput              -> "DebugInput"
  IllegalConversion       -> "IllegalConversion"
  Intermediate            -> "IntermediateValue"
  Internal                -> "Internal"

mathProvRange :: [Value] -> String
mathProvRange vals =
  let
    groupEq a b = pa == pb
      where pa = provenance a
            pb = provenance b

    formatGroup :: [Value] -> String
    formatGroup (v:_) = mathProv $ provenance v

    groups = intercalate ", "
           . map formatGroup
           . groupBy groupEq 
           $ filter (\a -> not $ provenance a == Nil) vals
  in
    " [ " ++ groups ++ " ] "

br :: String -> String
br x = if length x < 10 
       then "(" ++ x ++ ")"
       else " ( " ++ x ++ " ) "

binop :: String -> Value -> Value -> String
binop op v1 v2 = br $ 
  concat [mathProv $ provenance v1, " ", op, " ", mathProv $ provenance v2]

unary :: String -> Value -> String
unary op v = br $ 
  concat [op, " ", mathProv $ provenance v]

