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
  deriving (Eq, Show, Generic, NFData)

data IntType
  = IAlert
  | ICall
  | ICycle
  | IEmit
  | IJump
  | IJumpI
  | IReady
  | IReturn
  | ISLoad
  | ISStore
  | IStop
  deriving (Eq, Show, Generic, NFData)

data IntFlags = IntFlags
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
  } deriving (Eq, Show, Generic, NFData)

defaults :: IntFlags
defaults = IntFlags
  { intAlert  = True 
  , intCall   = True 
  , intCycle  = False 
  , intEmit   = True 
  , intJump   = True 
  , intJumpI  = True 
  , intReady  = False 
  , intReturn = True 
  , intSLoad  = True 
  , intSStore = True 
  , intStop   = True 
  }

intType :: Interrupt -> IntType
intType x = case x of
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
  
interruptible :: Interrupt -> IntFlags -> Bool
interruptible int = isEnabled (intType int)
  
isEnabled :: IntType -> IntFlags -> Bool
isEnabled it flags = flip ($) flags $ case it of
  IAlert  -> intAlert
  ICall   -> intCall
  ICycle  -> intCycle
  IEmit   -> intEmit
  IJump   -> intJump
  IJumpI  -> intJumpI
  IReady  -> intReady
  IReturn -> intReturn
  ISLoad  -> intSLoad
  ISStore -> intSStore
  IStop   -> intStop
  
enable :: IntType -> IntFlags -> IntFlags
enable i x = case i of
  IAlert  -> x { intAlert  = True }
  ICall   -> x { intCall   = True }
  ICycle  -> x { intCycle  = True }
  IEmit   -> x { intEmit   = True }
  IJump   -> x { intJump   = True }
  IJumpI  -> x { intJumpI  = True }
  IReady  -> x { intReady  = True }
  IReturn -> x { intReturn = True }
  ISLoad  -> x { intSLoad  = True }
  ISStore -> x { intSStore = True }
  IStop   -> x { intStop   = True }

disable :: IntType -> IntFlags -> IntFlags
disable i x = case i of
  IAlert  -> x { intAlert  = False }
  ICall   -> x { intCall   = False }
  ICycle  -> x { intCycle  = False }
  IEmit   -> x { intEmit   = False }
  IJump   -> x { intJump   = False }
  IJumpI  -> x { intJumpI  = False }
  IReady  -> x { intReady  = False }
  IReturn -> x { intReturn = False }
  ISLoad  -> x { intSLoad  = False }
  ISStore -> x { intSStore = False }
  IStop   -> x { intStop   = False }

intTypes :: [IntType]
intTypes = [ IAlert, ICall,   ICycle, IEmit,   IJump, IJumpI
           , IReady, IReturn, ISLoad, ISStore, IStop ]

enableAlert   = enable  IAlert 
enableCall    = enable  ICall  
enableCycle   = enable  ICycle 
enableEmit    = enable  IEmit  
enableJump    = enable  IJump  
enableJumpI   = enable  IJumpI 
enableReady   = enable  IReady 
enableReturn  = enable  IReturn
enableSLoad   = enable  ISLoad 
enableSStore  = enable  ISStore
enableStop    = enable  IStop  
disableAlert  = disable IAlert 
disableCall   = disable ICall  
disableCycle  = disable ICycle 
disableEmit   = disable IEmit  
disableJump   = disable IJump  
disableJumpI  = disable IJumpI 
disableReady  = disable IReady 
disableReturn = disable IReturn
disableSLoad  = disable ISLoad 
disableSStore = disable ISStore
disableStop   = disable IStop  

isAlert  = (== IAlert ) . intType 
isCall   = (== ICall  ) . intType 
isCycle  = (== ICycle ) . intType 
isEmit   = (== IEmit  ) . intType 
isJump   = (== IJump  ) . intType 
isJumpI  = (== IJumpI ) . intType 
isReady  = (== IReady ) . intType 
isReturn = (== IReturn) . intType 
isSLoad  = (== ISLoad ) . intType 
isSStore = (== ISStore) . intType 
isStop   = (== IStop  ) . intType 



