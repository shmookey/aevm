{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}

module Test.Fluidity.EVM.Core.SystemSpec where

import Data.Word (Word8)
import Control.Exception (SomeException, try)
import Data.ByteString (ByteString)
import Test.Hspec
import Test.Tasty
import Test.Tasty.Hspec as HS
import Data.Functor.Identity (Identity, runIdentity)
import qualified Data.ByteString as B
import qualified Data.Map as Map

import Control.Monad.Result
import Control.Monad.Resultant
import Control.Monad.Interruptible
import Control.Monad.Execution

import Fluidity.Common.Binary
import Fluidity.EVM.Core.Blockchain (Blockchain, Block)
import Fluidity.EVM.Core.Interrupt (Interrupt)
import Fluidity.EVM.Core.System (Sys)
import Fluidity.EVM.Data.Account (Account(Account), AccountDB(..), CodeDB(..), StorageDB(..))
import Fluidity.EVM.Data.Bytecode (Op, assemble)
import Fluidity.EVM.Data.ByteField (ByteField)
import Fluidity.EVM.Data.Prov (Prov(Nul))
import Fluidity.EVM.Data.Transaction (MessageCall)
import Fluidity.EVM.Data.Value
import qualified Fluidity.EVM.Core.Blockchain as Blockchain
import qualified Fluidity.EVM.Core.Interrupt as INT
import qualified Fluidity.EVM.Core.System as Sys
import qualified Fluidity.EVM.Core.VM as VM
import qualified Fluidity.EVM.Data.Account as Account
import qualified Fluidity.EVM.Data.Bytecode as Bytecode
import qualified Fluidity.EVM.Data.ByteField as BF
import qualified Fluidity.EVM.Data.Transaction as Tx

import TestData.EVM.Contracts

specs :: IO TestTree
specs = testSpec "Fluidity.EVM.Core.System" $ do
  resumeSpec

resumeSpec :: Spec
resumeSpec = do
  describe "resume<Preempt,Break,PC=2>" $ do
    let x # f = test x f Sys.resume id
    "break after first instruction" # \case Done x            -> ok
    "break without error"           # \case DoneOk x          -> ok
    "preempted running mode"        # \case DSt (Preempted _) -> ok
    "program counter unchanged"     # \case DSt (PC x)        -> x `shouldBe` 2
  
  describe "resume<Immediate,Break,PC=2>" $ do
    let x # f = test x f Sys.resume $ withIntPoint Sys.Immediate
    "break after first instruction" # \case Done x      -> ok
    "break without error"           # \case DoneOk x    -> ok
    "program counter incremented"   # \case DSt (PC x)  -> x `shouldBe` 4
    "running mode unaffected"       # \case DSt Running -> ok
  
  describe "resume<Preempt,Echo,PC=2>" $ do
    let x # f = test x f Sys.resume $ withIntAction Sys.Echo
    "cont after first instruction"  # \case ContInt _         -> ok
    "preempted running mode"        # \case CSt (Preempted _) -> ok
    "program counter unchanged"     # \case CSt (PC x)        -> x `shouldBe` 2


ok = return () :: Expectation
no = expectationFailure
run x f = runSys x (f sys)

--test :: String -> (Interruption Identity (Interrupt, Sys.State) (Sys.State, Result Sys.Error ()) -> Expectation) -> Sys () -> (Sys.State -> Sys.State) -> Expectation
test lbl check task alter = it lbl $
  let
    result = runSys task (alter sys)
    catchable = do
      return $! check result
      return ()
  in (try catchable :: IO (Either SomeException ())) >>= \case
    Left e  -> no $ show e ++ " " ++ show (resultState result)
    Right _ -> ok

-- Basic interruptions
pattern DoneOk       x <- Done (_, Ok x)
pattern ContInt      x <- Cont (x, _) _
pattern DSt          x <- Done (x, _)
pattern CSt          x <- Cont (_, x) _
-- VM state
pattern PC           x <- VM (VM.State { VM.stPC = x })
pattern VM           x <- Sys.State { Sys.stStack = x : _ }
-- Running modes
pattern Mode         x <- Sys.State { Sys.stMode  = x }
pattern Preempted    x <- Mode (Sys.Preempted x)
pattern Running        <- Mode Sys.Run
pattern Finalizing i x <- Mode (Sys.Finalizing i x)

resultState = \case { DSt x -> x ; CSt x -> x }

withIntAction :: Sys.InterruptAction -> Sys.State -> Sys.State
withIntAction x s = s { Sys.stConfig = (Sys.stConfig s) { Sys.cInterruptAction = x } }

withIntPoint :: Sys.InterruptPoint -> Sys.State -> Sys.State
withIntPoint x s = s { Sys.stConfig = (Sys.stConfig s) { Sys.cInterruptPoint = x } }

-- A complete system state definition
sys :: Sys.State
sys = Sys.State
  { Sys.stMode  = Sys.Run
  , Sys.stLast  = Just (blockchain, [vm])
  , Sys.stChain = blockchain
  , Sys.stStack =
    [ vm { VM.stPC    = 2
         , VM.stStack = [mkVal8 0x60]
         }
    ]
  , Sys.stConfig = Sys.Config
    { Sys.cInterruptAction = Sys.Break
    , Sys.cInterruptPoint  = Sys.Preempt
    , Sys.cInterrupts      = INT.IntFlags
      { INT.intAlert  = True
      , INT.intCall   = True
      , INT.intCycle  = True
      , INT.intEmit   = True
      , INT.intJump   = True
      , INT.intJumpI  = True
      , INT.intReady  = True
      , INT.intReturn = True
      , INT.intSLoad  = True
      , INT.intSStore = True
      , INT.intStop   = True
      }
    }
  }

blockchain :: Blockchain.State
blockchain = Blockchain.State
  { Blockchain.stBlocks    = []
  , Blockchain.stAccountDB = AccountDB $ Map.fromList
    [ ( toWord 20 $ B.singleton 0xee
      , Account
        { Account.acctBalance  = mkVal8 200
        , Account.acctCodeHash = B.singleton 0x42
        , Account.acctStorage  = StorageDB $ Map.fromList
          [ ( B.singleton 0x71
            , ( mkVal8 0x71 , mkVal8s [0xab, 0xcd, 0xef] )
            )
          ]
        }
      )
    ]
  , Blockchain.stCurrentBlock = Blockchain.Block
    { Blockchain.blkNumber       = mkVal8 0x01
    , Blockchain.blkHash         = mkVal8 0x22
    , Blockchain.blkTime         = mkVal8 0xab
    , Blockchain.blkGasPrice     = mkVal8 0x03
    , Blockchain.blkDifficulty   = mkVal8 0x19
    , Blockchain.blkCoinbase     = mkVal8 0x55
    , Blockchain.blkTransactions = []
    }
  , Blockchain.stCodeDB = CodeDB $ Map.fromList
    [ ( B.singleton 0x42
      , testContract
      )
    ]
  }

vm :: VM.State
vm = VM.State
  { VM.stStatus = VM.Running
  , VM.stPC     = 0
  , VM.stCode   = testContract
  , VM.stStack  = []
  , VM.stMemory = mempty
  , VM.stGas    = 10 
  , VM.stCall   = Tx.MessageCall
    { Tx.msgCaller = mkVal8 0xff
    , Tx.msgCallee = mkVal8 0xee
    , Tx.msgValue  = mkVal8 0xbb
    , Tx.msgGas    = mkVal8 0xcc
    , Tx.msgData   = mempty
    }
  , VM.stIntFlags = INT.IntFlags
    { INT.intAlert  = True
    , INT.intCall   = True
    , INT.intCycle  = True
    , INT.intEmit   = True
    , INT.intJump   = True
    , INT.intJumpI  = True
    , INT.intReady  = True
    , INT.intReturn = True
    , INT.intSLoad  = True
    , INT.intSStore = True
    , INT.intStop   = True
    }
  }

testContract = assemble
  [ Bytecode.Push1 $ mkVal8 0x60           -- 0x00
  , Bytecode.Push1 $ mkVal8 0x01           -- 0x02
  , Bytecode.Push2 $ mkVal8s [0x00,0x0a]   -- 0x04
  , Bytecode.JumpI                         -- 0x07
  , Bytecode.Stop                          -- 0x08
  , Bytecode.Return                        -- 0x09
  , Bytecode.JumpDest                      -- 0x0a
  , Bytecode.Push1 $ mkVal8 42             -- 0x10
  , Bytecode.Pop                           -- 0x12
  , Bytecode.Stop                          -- 0x13
  ]

runSys :: Sys a -> Sys.State -> Interruption Identity (INT.Interrupt, Sys.State) (Sys.State, Result Sys.Error a)
runSys ma st = runIdentity 
             . runInterruptibleT 
             $ runResultantT ma st
