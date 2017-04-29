{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}

module Test.Fluidity.EVM.Core.SystemSpec where

import Data.Word (Word8)
import Control.DeepSeq
import Control.Exception (SomeException, PatternMatchFail, try)
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
import qualified Fluidity.EVM.Core.Interrupt as I
import qualified Fluidity.EVM.Core.System as S
import qualified Fluidity.EVM.Core.VM as VM
import qualified Fluidity.EVM.Data.Account as Account
import qualified Fluidity.EVM.Data.Bytecode as Bytecode
import qualified Fluidity.EVM.Data.ByteField as BF
import qualified Fluidity.EVM.Data.Transaction as Tx
import qualified Fluidity.EVM.Text.Dump as Dump

import TestData.EVM.Contracts

specs :: IO TestTree
specs = testSpec "Fluidity.EVM.Core.System" $ do
  resumeSpec

resumeSpec :: Spec
resumeSpec = do
  describe "resume<Running,Preempt,I={All=B},PC=0(Push1)>" $ do
    let x # f = test' x f S.resume
    "break, not suspend"            # \case Done x         -> ok
    "break without error"           # \case DoneOk x       -> ok
    "preempted running mode"        # \case DSt (Status x) -> x `shouldBe` S.Preempted
    "program counter unchanged"     # \case DSt (PC x)     -> x `shouldBe` 2
  
  describe "resume<Running,Immediate,I={All=B},PC=0(Push1)>" $ do
    let x # f = test' x f $ do
                  S.setStrategy S.Immediate
                  S.resume
    "break, not suspend"            # \case Done x         -> ok
    "break without error"           # \case DoneOk x       -> ok
    "program counter incremented"   # \case DSt (PC x)     -> x `shouldBe` 2
    "running mode 'running'"        # \case DSt (Status x) -> x `shouldBe` S.Running
  
  describe "resume<Running,Wait,I={All=B},PC=0(Push1)>" $ do
    let x # f = test' x f $ do
                  S.setStrategy S.Wait
                  S.resume
    "break, not suspend"            # \case Done x         -> ok
    "break without error"           # \case DoneOk x       -> ok
    "program counter incremented"   # \case DSt (PC x)     -> x `shouldBe` 2
    "running mode 'running'"        # \case DSt (Status x) -> x `shouldBe` S.Running
  
  describe "resume<Running,Preempt,I={All=E},PC=2(Push1)>" $ do
    let x # f = test' x f $ do
                  S.updateInterrupts $ I.setAll I.Echo
                  S.resume
    "suspend, not break"            # \case ContInt _      -> ok
    "preempted running mode"        # \case CSt (Status x) -> x `shouldBe` S.Preempted
    "program counter unchanged"     # \case CSt (PC x)     -> x `shouldBe` 2

  describe "resume<Preempted,Preempt,I={Stop=E,JumpI=E},PC=7(JumpI)>" $ do
    let x # f = test' x f $ do
                  S.mutate . VM.push $ mkVal8 0
                  S.mutate . VM.push $ mkVal8 0
                  S.mutate $ VM.setPC 0x07
                  S.setStatus S.Preempted
                  S.updateInterrupts $ I.setAll I.Ignore
                  S.setInterruptAction I.Echo I.IJumpI
                  S.setInterruptAction I.Echo I.IStop
                  S.resume
    "suspend, not break"            # \case ContInt _      -> ok
    "preempted running mode"        # \case CSt (Status x) -> x `shouldBe` S.Preempted
    "pc == 8"                       # \case CSt (PC x)     -> x `shouldBe` 0x08
    "interrupt == Stop"             # \case ContInt x      -> x `shouldBe` I.Stop


ok = return () :: Expectation
no = expectationFailure
run x f = runS x (f sys)

type TestInterruption = Interruption Identity (Interrupt, S.State) (S.State, Result S.Error ())
type TestInt = Interruption Identity (Interrupt, S.State) (S.State, Result S.Error ())

--test :: String -> (Interruption Identity (Interrupt, S.State) (S.State, Result S.Error ()) -> Expectation) -> S () -> (S.State -> S.State) -> Expectation
test :: String -> (TestInt -> IO ()) -> Sys () -> (S.State -> S.State) -> SpecWith ()
test lbl check task alter = it lbl $
  let
    result   = runS task (alter sys)
    catchable = check result
  in (try catchable :: IO (Either SomeException ())) >>= \case
    Left e  -> no $ show e ++ " " ++ show (resultState result)
    Right _ -> ok

test' :: String -> (TestInt -> IO ()) -> Sys () -> SpecWith ()
test' lbl check task = it lbl $ 
  let
    result    = runS task sys
    catchable = check result
  in (try catchable :: IO (Either PatternMatchFail ())) >>= \case
    Left e  -> no $ show e ++ showResult result ++ "\n" ++ Dump.dumpCompact (resultState result)
    Right r -> catchable

-- Basic interruptions
pattern DoneOk       x <- Done (_, Ok x)
pattern ContInt      x <- Cont (x, _) _
pattern DSt          x <- Done (x, _)
pattern CSt          x <- Cont (_, x) _
-- VM state
pattern PC           x <- VM (VM.State { VM.stPC = x })
pattern VM           x <- S.State { S.stStack = x : _ }
-- Running modes
pattern Status       x <- S.State { S.stStatus  = x }
pattern Preempted      <- Status S.Preempted
pattern Running        <- Status S.Running
pattern Waiting i x    <- Status (S.Waiting i x)

resultState = \case { DSt x -> x ; CSt x -> x }

showResult :: TestInt -> String
showResult r = case r of
  Done (_, x) -> show x
  _           -> "did not get a result"

allIntActions :: I.Action -> S.State -> S.State
allIntActions x s = 
  s { S.stConfig = 
    (S.stConfig s) { 
      S.cInterrupts = I.setAll x (S.cInterrupts $ S.stConfig s)
    } 
  }

withStatus :: S.Status -> S.State -> S.State
withStatus x s = s { S.stStatus = x }

withStrategy :: S.Strategy -> S.State -> S.State
withStrategy x s = s { S.stConfig = (S.stConfig s) { S.cStrategy = x } }

-- A complete system state definition
sys :: S.State
sys = S.State
  { S.stStatus = S.Running
  , S.stChain  = blockchain
  , S.stStack  = [vm]
  , S.stConfig = S.Config
    { S.cBreakpoints = []
    , S.cStrategy    = S.Preempt
    , S.cInterrupts  = I.IntConfig
      { I.iAlert  = I.Break
      , I.iCall   = I.Break
      , I.iCycle  = I.Break
      , I.iEmit   = I.Break
      , I.iJump   = I.Break
      , I.iJumpI  = I.Break
      , I.iReady  = I.Break
      , I.iReturn = I.Break
      , I.iSLoad  = I.Break
      , I.iSStore = I.Break
      , I.iStop   = I.Break
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

runS :: Sys a -> S.State -> Interruption Identity (I.Interrupt, S.State) (S.State, Result S.Error a)
runS ma st = runIdentity 
           . runInterruptibleT 
           $ runResultantT ma st

