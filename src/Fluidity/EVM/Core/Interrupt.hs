{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module Fluidity.EVM.Core.Interrupt where

import Control.DeepSeq
import GHC.Generics (Generic)

import Fluidity.EVM.Data.Value (Value)
import Fluidity.EVM.Data.ByteField (ByteField)
import qualified Fluidity.EVM.Data.ByteField as BF


data Interrupt 
  = Alert  String
  | Call
    { cGas    :: Value
    , cCallee :: Value
    , cValue  :: Value
    , cData   :: ByteField
    , cRetP   :: Value
    , cRetSz  :: Value
    }
  | Cycle  Int
  | Emit   ByteField [Value]
  | Jump   Value 
  | JumpI  Value Value
  | Ready
  | Return ByteField
  | SLoad  Value Value
  | SStore Value Value
  | Stop
  deriving (Show, Generic, NFData)

data IntConf = IntConf
  { intAlert  :: Bool
  , intCall   :: Bool
  , intCycle  :: Bool
  , intEmit   :: Bool
  , intJump   :: Bool
  , intJumpI  :: Bool
  , intReady  :: Bool
  , intReturn :: Bool
  , intSLoad  :: Bool
  , intSStore :: Bool
  , intStop   :: Bool
  } deriving (Show, Generic, NFData)

defaults :: IntConf
defaults = IntConf
  { intAlert  = False 
  , intCall   = False 
  , intCycle  = False 
  , intEmit   = False 
  , intJump   = False 
  , intJumpI  = False 
  , intReady  = False 
  , intReturn = False 
  , intSLoad  = False 
  , intSStore = False 
  , intStop   = False 
  }

interruptible :: Interrupt -> IntConf -> Bool
interruptible int ic = flip ($) ic $ case int of
  Alert  _            -> intAlert
  Call   _ _ _ _ _ _  -> intCall
  Cycle  _            -> intCycle
  Emit   _ _          -> intEmit
  Jump   _            -> intJump
  JumpI  _ _          -> intJumpI
  Ready               -> intReady
  Return _            -> intReturn
  SLoad  _ _          -> intSLoad
  SStore _ _          -> intSStore
  Stop                -> intStop
  
enableAlert   x = x { intAlert  = True }
enableCall    x = x { intCall   = True }
enableCycle   x = x { intCycle  = True }
enableEmit    x = x { intEmit   = True }
enableJump    x = x { intJump   = True }
enableJumpI   x = x { intJumpI  = True }
enableReady   x = x { intReady  = True }
enableReturn  x = x { intReturn = True }
enableSLoad   x = x { intSLoad  = True }
enableSStore  x = x { intSStore = True }
enableStop    x = x { intStop   = True }

disableAlert  x = x { intAlert  = False }
disableCall   x = x { intCall   = False }
disableCycle  x = x { intCycle  = False }
disableEmit   x = x { intEmit   = False }
disableJump   x = x { intJump   = False }
disableJumpI  x = x { intJumpI  = False }
disableReady  x = x { intReady  = False }
disableReturn x = x { intReturn = False }
disableSLoad  x = x { intSLoad  = False }
disableSStore x = x { intSStore = False }
disableStop   x = x { intStop   = False }

isAlert  = \case { Alert  _           -> True ; _ -> False }
isCall   = \case { Call   _ _ _ _ _ _ -> True ; _ -> False }
isCycle  = \case { Cycle  _           -> True ; _ -> False }
isEmit   = \case { Emit   _ _         -> True ; _ -> False }
isJump   = \case { Jump   _           -> True ; _ -> False }
isJumpI  = \case { JumpI  _ _         -> True ; _ -> False }
isReady  = \case { Ready              -> True ; _ -> False }
isReturn = \case { Return _           -> True ; _ -> False }
isSLoad  = \case { SLoad  _ _         -> True ; _ -> False }
isSStore = \case { SStore _ _         -> True ; _ -> False }
isStop   = \case { Stop               -> True ; _ -> False }



