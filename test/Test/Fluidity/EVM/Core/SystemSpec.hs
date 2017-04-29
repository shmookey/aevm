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
import qualified Fluidity.EVM.Core.Interrupt as I
import qualified Fluidity.EVM.Core.System as S
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
    let x # f = test x f S.resume id
    "break, not suspend"            # \case Done x        -> ok
    "break without error"           # \case DoneOk x      -> ok
    "preempted running mode"        # \case DSt Preempted -> ok
    "program counter unchanged"     # \case DSt (PC x)    -> x `shouldBe` 2
  
  describe "resume<Immediate,Break,PC=2>" $ do
    let x # f = test x f S.resume $ withStrategy S.Immediate
    "break, not suspend"            # \case Done x      -> ok
    "break without error"           # \case DoneOk x    -> ok
    "program counter incremented"   # \case DSt (PC x)  -> x `shouldBe` 4
    "running mode unaffected"       # \case DSt Running -> ok
  
  describe "resume<Wait,Break,PC=2>" $ do
    let x # f = test x f S.resume $ withStrategy S.Wait
    "break, not suspend"            # \case Done x      -> ok
    "break without error"           # \case DoneOk x    -> ok
    "program counter incremented"   # \case DSt (PC x)  -> x `shouldBe` 4
    "running mode unaffected"       # \case DSt Running -> ok
  
  describe "resume<Preempt,Echo,PC=2>" $ do
    let x # f = test x f S.resume $ allIntActions I.Echo
    "suspend, not break"            # \case ContInt _     -> ok
    "preempted running mode"        # \case CSt Preempted -> ok
    "program counter unchanged"     # \case CSt (PC x)    -> x `shouldBe` 2


ok = return () :: Expectation
no = expectationFailure
run x f = runS x (f sys)

--test :: String -> (Interruption Identity (Interrupt, S.State) (S.State, Result S.Error ()) -> Expectation) -> S () -> (S.State -> S.State) -> Expectation
test lbl check task alter = it lbl $
  let
    result = runS task (alter sys)
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
pattern VM           x <- S.State { S.stStack = x : _ }
-- Running modes
pattern Status       x <- S.State { S.stStatus  = x }
pattern Preempted      <- Status S.Preempted
pattern Running        <- Status S.Running
pattern Waiting i x    <- Status (S.Waiting i x)

resultState = \case { DSt x -> x ; CSt x -> x }

allIntActions :: I.Action -> S.State -> S.State
allIntActions x s = 
  s { S.stConfig = 
    (S.stConfig s) { 
      S.cInterrupts = I.setAll x (S.cInterrupts $ S.stConfig s)
    } 
  }

withStrategy :: S.Strategy -> S.State -> S.State
withStrategy x s = s { S.stConfig = (S.stConfig s) { S.cStrategy = x } }

-- A complete system state definition
sys :: S.State
sys = S.State
  { S.stStatus = S.Running
  , S.stChain  = blockchain
  , S.stStack  =
    [ vm { VM.stPC    = 2
         , VM.stStack = [mkVal8 0x60]
         }
    ]
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

