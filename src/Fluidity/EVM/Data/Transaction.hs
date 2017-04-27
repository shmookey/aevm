{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module Fluidity.EVM.Data.Transaction where

import GHC.Generics (Generic)
import Control.DeepSeq

import Fluidity.EVM.Data.ByteField (ByteField)
import Fluidity.EVM.Data.Value (Value)


data Transaction = Transaction
  { txFrom      :: Value
  , txTo        :: Value
  , txHash      :: Value
  , txValue     :: Value
  , txData      :: ByteField
  , txGas       :: Value
  , txCalls     :: [MessageCall]
  } deriving (Eq, Show, Generic, NFData)

data MessageCall = MessageCall
  { msgCaller :: Value
  , msgCallee :: Value
  , msgValue  :: Value
  , msgGas    :: Value
  , msgData   :: ByteField
  } deriving (Eq, Show, Generic, NFData)

