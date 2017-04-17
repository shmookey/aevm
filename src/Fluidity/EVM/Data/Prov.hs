{- Data provenance tracking

This module implements a data type `Prov`, for modelling the history of values
in the system - where they came from, and how they have been transformed. It is
not concerned with intermediate locations such as the stack, memory or storage,
which are effectively transparent unless it affects the actual value, e.g. when
a value is read at an offset to where it was stored, it has a shifting effect.

The `Prov` type is effectively a tree, where the leaf nodes are the originating
sources for each value. Fundamentally, all values in the system must come from
one of five sources:
  
  1. Env: the blockchain environment, including block hashes and timestamps.
  2. Usr: the operator of this software (you), directly or indirectly.
  3. Ext: the external world, including all other users of the blockchain.
  4. Sys: run-time observations, like the value returned by MSIZE.
  5. Nul: the "null source", representing values yet uninitalised.

The source information allows us to make inferences about how the behaviour of
a contract is affected by user input, and whether critical branching conditions
are determined by factors we control, in whole or in part.

Addresses, even user-supplied ones, are (as with all hashes) treated as coming
from the environment, since they are neither craftable nor predictable.

-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module Fluidity.EVM.Data.Prov where

import Prelude hiding (Word)
import Control.DeepSeq
import GHC.Generics (Generic)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B

import Fluidity.Common.Binary


class Provenance a where
  prov :: a -> Prov

data Prov
  -- Originating sources - leaf nodes of the Prov tree
  = Env Env ByteString
  | Sys Sys ByteString
  | Usr Usr ByteString
  | Ext Usr ByteString 
  | Nul

  -- Data transformations
  -- Contains the generated value and the Provs of its inputs
  | BinOp BinOp ByteString Prov Prov      -- Binary operation
  | UnaOp UnaOp ByteString Prov           -- Unary operation
  | MemOp MemOp ByteString Prov Prov Prov -- Only SHA3 at the moment, args: ptr sz data

  -- Memory-related
  -- See the `ByteField` documentation for details
  | SliceRead ByteString Prov
  | TransRead ByteString Int Prov
  | DirtyRead ByteString [(Int, Prov)]

  deriving (Eq, Show, Generic, NFData)

data Sys = PC | GasLeft
  deriving (Eq, Show, Generic, NFData)

data Env
  = BlockNumber | BlockHash | BlockTime | Difficulty
  | GasPrice    | GasLimit  | Coinbase  | Address
  deriving (Eq, Show, Generic, NFData)

data Usr
  = CallValue | CallGas | CallData | Balance | Code | Import | Storage ByteString ByteString
  deriving (Eq, Show, Generic, NFData)

data BinOp
  = Add | Sub | Mul | Div | SDiv | Mod | Exp
  | Eq  | GT  | LT  | SGT | SLT
  | And | Or  | Xor | Byte
  deriving (Eq, Show, Generic, NFData)

data UnaOp
  = IsZero | Not | Size
  deriving (Eq, Show, Generic, NFData)

data MemOp
  = SHA3
  deriving (Eq, Show, Generic, NFData)

valueAt :: Prov -> ByteString
valueAt pv = case pv of
  Env _ bs         -> bs
  Sys _ bs         -> bs
  Usr _ bs         -> bs
  Ext _ bs         -> bs
  BinOp _ bs _ _   -> bs
  UnaOp _ bs _     -> bs
  MemOp _ bs _ _ _ -> bs
  SliceRead bs _   -> bs
  TransRead bs _ _ -> bs
  DirtyRead bs _   -> bs
  Nul              -> mempty
  
