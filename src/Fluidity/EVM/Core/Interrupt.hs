{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module Fluidity.EVM.Core.Interrupt where

import Control.DeepSeq
import GHC.Generics (Generic)

import Fluidity.EVM.Data.Value (Value)
import Fluidity.EVM.Data.ByteField (ByteField)
import qualified Fluidity.EVM.Data.ByteField as BF


-- Interrupt types
-- ---------------------------------------------------------------------

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
  deriving (Eq, Show, Generic, NFData)

data IntType
  = IAlert  | ICall   | ICycle
  | IEmit   | IJump   | IJumpI
  | IReady  | IReturn | ISLoad
  | ISStore | IStop
  deriving (Eq, Show, Generic, NFData)

types = 
  [ IAlert  , ICall   , ICycle
  , IEmit   , IJump   , IJumpI
  , IReady  , IReturn , ISLoad
  , ISStore , IStop   ]

iType :: Interrupt -> IntType
iType x = case x of
  Alert  _            -> IAlert
  Call   _ _ _ _ _ _  -> ICall
  Cycle  _            -> ICycle
  Emit   _ _          -> IEmit
  Jump   _            -> IJump
  JumpI  _ _          -> IJumpI
  Ready               -> IReady
  Return _            -> IReturn
  SLoad  _ _          -> ISLoad
  SStore _ _          -> ISStore
  Stop                -> IStop

  
-- Configuration
-- ---------------------------------------------------------------------

data IntConfig = IntConfig
  { iAlert  :: Action , iCall   :: Action , iCycle  :: Action
  , iEmit   :: Action , iJump   :: Action , iJumpI  :: Action
  , iReady  :: Action , iReturn :: Action , iSLoad  :: Action
  , iSStore :: Action , iStop   :: Action
  } deriving (Eq, Show, Generic, NFData)

data Action = Echo | Break | Ignore
  deriving (Eq, Show, Generic, NFData)

defaults :: IntConfig
defaults = IntConfig
  { iAlert  = Echo , iCall   = Echo , iCycle  = Ignore
  , iEmit   = Echo , iJump   = Echo , iJumpI  = Echo
  , iReady  = Echo , iReturn = Echo , iSLoad  = Echo 
  , iSStore = Echo , iStop   = Echo }

action :: Interrupt -> IntConfig -> Action
action = getAction . iType

getAction :: IntType -> IntConfig -> Action
getAction it flags = flip ($) flags $ case it of
  { IAlert  -> iAlert  ; ICall   -> iCall   ; ICycle  -> iCycle
  ; IEmit   -> iEmit   ; IJump   -> iJump   ; IJumpI  -> iJumpI
  ; IReady  -> iReady  ; IReturn -> iReturn ; ISLoad  -> iSLoad
  ; ISStore -> iSStore ; IStop   -> iStop   }
  
setAction :: Action -> IntType -> IntConfig -> IntConfig
setAction x i c = case i of
  IAlert  -> c { iAlert  = x }
  ICall   -> c { iCall   = x }
  ICycle  -> c { iCycle  = x }
  IEmit   -> c { iEmit   = x }
  IJump   -> c { iJump   = x }
  IJumpI  -> c { iJumpI  = x }
  IReady  -> c { iReady  = x }
  IReturn -> c { iReturn = x }
  ISLoad  -> c { iSLoad  = x }
  ISStore -> c { iSStore = x }
  IStop   -> c { iStop   = x }

setAll :: Action -> IntConfig -> IntConfig
setAll x z = foldl (\a t -> setAction x t a) z types

echo :: IntType -> IntConfig -> IntConfig
echo   = setAction Echo

break :: IntType -> IntConfig -> IntConfig
break  = setAction Break

ignore :: IntType -> IntConfig -> IntConfig
ignore = setAction Ignore

-- | Switch all instances of "Break" to "Echo"
nobreak :: IntConfig -> IntConfig
nobreak c = foldl f c types
  where f c x = if getAction x c == Break 
                then setAction Echo x c
                else c


