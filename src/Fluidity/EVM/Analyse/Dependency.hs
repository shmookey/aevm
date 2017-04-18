{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module Fluidity.EVM.Analyse.Dependency where

import Prelude hiding (GT, LT)
import GHC.Generics (Generic)
import Control.DeepSeq
import Data.List (intercalate)
import Data.ByteString (ByteString) 
import qualified Data.ByteString as B

import Fluidity.Common.Binary
import Fluidity.EVM.Data.Prov (Prov, BinOp(..))
import qualified Fluidity.EVM.Data.Prov as P


data Var
  = BlockNumber | BlockHash | BlockTime | Coinbase  -- Block
  | GasPrice    | GasLimit  | Difficulty            -- Mining
  | Caller      | CallValue | CallGas   | CallData  -- Call
  | Address     | Balance   | Storage ByteString    -- Account
  | PC          | GasLeft                           -- System
  deriving (Eq, Show, Generic, NFData)

data Src = Usr | Ext | Env
  deriving (Eq, Show, Generic, NFData)

data Expr
  = Lit ByteString
  | Var Src Var
  | Pad Int
  | Nul
  | Op2 P.BinOp Expr Expr
  | Op1 P.UnaOp Expr
  | SHA Expr
  | Cat [Expr]
  | Cut Int Int Expr
  | Clr Expr
  deriving (Eq, Show, Generic, NFData)

maskA = Lit $  (fromHex "ffffffffffffffffffffffffffffffffffffffff" :: ByteString)
mask4 =  (fromHex "0000000100000000000000000000000000000000000000000000000000000000" :: ByteString)

simplify :: Expr -> Expr
simplify expr = 
  let iterate x = if x == expr then x else simplify x
  in case expr of
  -- Basic arithmetic
  Op2 Add (Lit a) (Lit b)             -> Lit $ asIntegers (+)   a b
  Op2 Sub (Lit a) (Lit b)             -> Lit $ asIntegers (-)   a b
  Op2 Mul (Lit a) (Lit b)             -> Lit $ asIntegers (*)   a b
  Op2 Div (Lit a) (Lit b)             -> Lit $ asIntegers (div) a b
  Op2 Mod (Lit a) (Lit b)             -> Lit $ asIntegers (mod) a b
  Op2 Exp (Lit a) (Lit b)             -> Lit $ asIntegers (^)   a b
--  -- Left-associating arithmetic
--  Op2 Add (Lit a) (Op2 Add (Lit b) e) -> simplify $ Op2 Add (Lit $ asIntegers (+)   a b) (simplify e)
--  Op2 Add (Lit a) (Op2 Sub (Lit b) e) -> simplify $ Op2 Sub (Lit $ asIntegers (+)   a b) (simplify e)
--  Op2 Sub (Lit a) (Op2 Add (Lit b) e) -> simplify $ Op2 Sub (Lit $ asIntegers (-)   a b) (simplify e)
--  Op2 Sub (Lit a) (Op2 Sub (Lit b) e) -> simplify $ Op2 Add (Lit $ asIntegers (-)   a b) (simplify e)
--  Op2 Mul (Lit a) (Op2 Mul (Lit b) e) -> simplify $ Op2 Mul (Lit $ asIntegers (*)   a b) e
--  Op2 Mul (Lit a) (Op2 Div (Lit b) e) -> simplify $ Op2 Div (Lit $ asIntegers (*)   a b) e
--  Op2 Div (Lit a) (Op2 Mul (Lit b) e) -> simplify $ Op2 Div (Lit $ asIntegers (*)   a b) e
--  -- Right-associating arithmetic
--  Op2 Add (Op2 Add e (Lit a)) (Lit b) -> simplify $ Op2 Add (simplify e) (Lit $ asIntegers (+)   a b)
--  Op2 Add (Op2 Sub e (Lit a)) (Lit b) -> simplify $ Op2 Sub (simplify e) (Lit $ asIntegers (-)   a b)
--  Op2 Sub (Op2 Add e (Lit a)) (Lit b) -> simplify $ Op2 Add (simplify e) (Lit $ asIntegers (-)   a b)
--  Op2 Sub (Op2 Sub e (Lit a)) (Lit b) -> simplify $ Op2 Sub (simplify e) (Lit $ asIntegers (+)   a b)

  -- Slices
  Cut a b (Cut c _ e)                 -> iterate $ Cut (a + c) b (simplify e)
  Op2 P.Div e1 e2                     -> let (e1', e2') = (simplify e1, simplify e2)
                                         in iterate $ case e2' of
                                            Lit bs -> if trim bs == trim mask4 then Cut 0 4 e1'
                                                      else Op2 Div e1' e2'
                                            _      -> Op2 Div e1' e2'
  Op2 P.And e1 e2                     -> let (e1', e2') = (simplify e1, simplify e2)
                                         in iterate $ if      e1' == maskA then Clr e2'
                                                      else if e2' == maskA then Clr e1'
                                                      else Op2 P.And e1' e2'
  -- Traversals
  Op2 t a b                           -> iterate $ Op2 t   (simplify a) (simplify b)
  Op1 t a                             -> iterate $ Op1 t   (simplify a)
  SHA a                               -> iterate $ SHA     (simplify a)
  Cat es                              -> iterate $ Cat     (map simplify es)
  Cut a b e                           -> iterate $ Cut a b (simplify e)
  Clr e                               -> iterate $ Clr     (simplify e)
  _                                   -> expr

opAdd = Op2 Add

convert :: Prov -> Expr
convert pv = case pv of

  P.Env t _ -> Var Env $ case t of
    P.BlockNumber -> BlockNumber
    P.BlockHash   -> BlockHash
    P.BlockTime   -> BlockTime
    P.Difficulty  -> Difficulty
    P.GasPrice    -> GasPrice
    P.GasLimit    -> GasLimit
    P.Coinbase    -> Coinbase
    P.Address     -> Address
    _             -> error $ "invalid env var: " ++ show t
  
  P.Usr t bs -> Var Usr $ case t of
    P.Caller    -> Caller
    P.CallData  -> CallData
    P.CallValue -> CallValue
    P.CallGas   -> CallGas
    _           -> error $ "invalid user var: " ++ show t

  P.Ext t bs -> Var Ext $ case t of
    P.Balance     -> Balance
    P.Storage k _ -> Storage k
    _             -> error $ "invalid ext var: " ++ show t

  P.Cut a b bs p -> case p of
    P.Ext P.Code _ -> Lit bs
    _              -> Cut a b (convert p)

  P.UnaOp op bs a         -> Op1 op $ convert a
  P.BinOp op bs a b       -> Op2 op (convert a) (convert b) 
  P.MemOp P.SHA3 bs _ _ a -> SHA $ convert a
  P.Cat bs es             -> Cat $ map convert es
  P.Pad n                 -> Pad n
  P.Fit bs p              -> convert p
  P.Nul                   -> Nul

  _ -> error $ "invalid prov entry: " ++ show pv


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


-- Helpers

asIntegers :: (Integer -> Integer -> Integer) -> ByteString -> ByteString -> ByteString
--asIntegers f a b = unroll $ (roll a `f` roll b)
asIntegers f a b = unroll $ mod (roll a `f` roll b) (2^256)

