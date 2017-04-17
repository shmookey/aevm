{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module Fluidity.EVM.Analyse.Dependency where

import Prelude hiding (GT, LT)
import GHC.Generics (Generic)
import Control.DeepSeq
import Data.List (intercalate)
import Data.ByteString 
import qualified Data.ByteString as B

import Fluidity.Common.Binary
import Fluidity.EVM.Data.Prov (Prov)
import qualified Fluidity.EVM.Data.Prov as P


data Var
  = BlockNumber | BlockHash | BlockTime | Difficulty
  | GasPrice    | GasLimit  | Coinbase  | Address
  | CallValue   | CallGas   | Balance   | Storage ByteString ByteString
  | PC          | GasLeft
  deriving (Show, Generic, NFData)

data Expr
  = Lit ByteString
  | Nul
  | Var Var
  | Ext Var
  | Msg Int Int
  | Op2 P.BinOp Expr Expr
  | Op1 P.UnaOp Expr
  | SHA Expr
  deriving (Show, Generic, NFData)


methodHashMask = unroll (0x0100000000000000000000000000000000000000000000000000000000 :: Integer)

simplify :: Expr -> Expr
simplify expr = case expr of
  Op2 P.Div (Msg a b) (Lit bs) -> if bs == methodHashMask then Msg a 4 else expr
  Op2 t a b                    -> Op2 t (simplify a) (simplify b)
  Op1 t a                      -> Op1 t (simplify a)
  SHA a                        -> SHA   (simplify a)
  _                            -> expr

convert :: Prov -> Expr
convert pv = case pv of
  P.SliceRead bs   (P.Usr P.CallData _) -> Msg 0 (B.length bs)
  P.TransRead bs i (P.Usr P.CallData _) -> Msg i (B.length bs + i)
  P.SliceRead bs   (P.Ext P.Code     _) -> Lit bs
  P.TransRead bs i (P.Ext P.Code     _) -> Lit bs

  P.Env t _ -> case t of
    P.BlockNumber -> Var BlockNumber
    P.BlockHash   -> Var BlockHash
    P.BlockTime   -> Var BlockTime
    P.Difficulty  -> Var Difficulty
    P.GasPrice    -> Var GasPrice
    P.GasLimit    -> Var GasLimit
    P.Coinbase    -> Var Coinbase
    P.Address     -> Var Address
  
  P.Usr t bs -> case t of
    P.CallValue -> Var CallValue
    P.CallGas   -> Var CallGas
    P.Balance   -> Var Balance
    P.CallData  -> Msg 0 (B.length bs)

  P.Ext t bs -> case t of
    P.CallValue   -> Ext CallValue
    P.CallGas     -> Ext CallGas
    P.Balance     -> Ext Balance
    P.Storage k v -> Ext $ Storage k v

  P.UnaOp op bs a -> case convert a of
    Lit _ -> Lit bs
    a     -> Op1 op a

  P.BinOp op bs a b -> case (convert a, convert b) of
    (Lit _, Lit _) -> Lit bs
    (a'   , b')    -> Op2 op a' b'
    
  P.MemOp P.SHA3 bs _ _ a ->
    SHA $ convert a

  P.Nul -> Nul

  _ -> Lit $ P.valueAt pv
  --_ -> Lit (B.append (B.pack [0x0B, 0xAD]) $ P.valueAt pv)


-- Classifiers
-- ---------------------------------------------------------------------

isLeaf :: Prov -> Bool
isLeaf pv = case pv of
  P.Env _ _ -> True
  P.Sys _ _ -> True
  P.Usr _ _ -> True
  P.Ext _ _ -> True
  P.Nul     -> True
  _         -> False

isCode :: Prov -> Bool
isCode pv = case pv of
  P.Usr P.Code _ -> True
  P.Ext P.Code _ -> True
  _              -> False

isByteSource :: Prov -> Bool
isByteSource pv = case pv of
  P.Usr P.CallData _ -> True
  P.Usr P.Code     _ -> True
  P.Ext P.CallData _ -> True
  P.Ext P.Code     _ -> True
  _                  -> False



matchMethodCall :: Prov -> Maybe B.ByteString
matchMethodCall pv = case pv of
  P.BinOp P.Eq _ (P.TransRead bs _ (P.Ext P.Code _))                      (P.BinOp P.Div _ (P.SliceRead _ (P.Usr P.CallData _)) _) -> Just bs
  P.BinOp P.Eq _ (P.BinOp P.Div _ (P.SliceRead _ (P.Usr P.CallData _)) _) (P.TransRead bs _ (P.Ext P.Code _))                      -> Just bs
  _ -> Nothing


