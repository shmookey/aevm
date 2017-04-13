{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Fluidity.EVM.Types where

import Prelude hiding (Value)
import Data.Text (Text)
import Data.Map (Map)
import Data.ByteString (ByteString)
import GHC.Generics (Generic)
import Control.DeepSeq
import qualified Data.Map as Map
import qualified Data.Text as T

import Control.Monad.Resultant
import Control.Monad.Interruptible

import Fluidity.EVM.Data.Value (Value)
import Fluidity.EVM.Data.ByteField (ByteField)
import Fluidity.EVM.Data.Bytecode


-- Fundamental types
type CAddr     = Value       -- Byte-addressed code pointer
type MAddr     = Value       -- Byte-addressed memory pointer
type SAddr     = Value       -- Value-addressed storage pointer
type CDAddr    = Value       -- Byte-addressed calldata pointer

-- State types
type PC         = CAddr       -- Program counter (code pointer)
type Stack      = [Value]      
type Code       = [Op]
type BlockHash  = Value
type TxHash     = Value
type Bytecode   = ByteField
type Memory     = ByteField
type CallData   = ByteField
type ReturnData = ByteField
type Address    = Value
type Balance    = Value
type Gas        = Value

type ExtCall = (Gas, Address, Balance, ByteField, MAddr, Value)

data Log = Log ByteField [Value]
  deriving (Eq, Show, Generic, NFData)

-- Bytecode types
type Label     = Text
data Program   = Program [Op] deriving (Show)

