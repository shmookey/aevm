module Test.Fluidity.EVM.Core.SystemSpec where

import Data.Word (Word8)
import Data.ByteString (ByteString)
import Test.Hspec
import Test.Tasty
import Test.Tasty.Hspec as HS
import qualified Data.ByteString as B

import Fluidity.Common.Binary
import Fluidity.EVM.Data.Bytecode (Op(..), assemble)
import Fluidity.EVM.Data.Value
import Fluidity.EVM.Data.Prov (Prov(Nul))
import qualified Fluidity.EVM.Core.System as Sys
import qualified Fluidity.EVM.Data.ByteField as BF


specs :: IO TestTree
specs = testSpec "Fluidity.EVM.Core.System" $
  describe "assemble" $ do
    it "encodes PUSH1 0x60" $ do
      assembleB [Push1 $ val [0x60]] `shouldBe` B.pack [0x60,0x60]

onInterruptSpec = 
  let
    code = assemble
      [ Push1 $ mkVal8 0x60           -- 0x00
      , Push1 $ mkVal8 0x01           -- 0x02
      , Push2 $ mkVal8s [0x00,0x0a]   -- 0x04
      , JumpI                         -- 0x07
      , Stop                          -- 0x08
      , Return                        -- 0x09
      , JumpDest                      -- 0x0a
      , Push1 $ mkVal8 42             -- 0x10
      , Pop                           -- 0x12
      , Stop                          -- 0x13
      ]


  in do
    describe "onInterrupt<Preempt,Break>" $ do
      it ""

-- A complete system state definition
initStateSys :: Sys.State
initStateSys = Sys.State
  { Sys.stMode  = Sys.Run
  , Sys.stLast  = Nothing
  , Sys.stChain = Blockcain.State
    { Blockchain.stBlocks    = []
    , Blockchain.stAccountDB = AccountDB $ Map.fromList
      [ ( toWord 20 $ B.singleton 0xee
        , Account
          { Account.acctBalance  = 1024
          , Account.acctCodeHash = B.singleton 0x42
          , Account.acctStorage  = StorageDB $ Map.fromList
            [ ( B.singleton 0x71
              , ( val8 0x71
                , val8s [0xab, 0xcd, 0xef]
                )
              )
            ]
          }
        )
      ]
    , Blockchain.stCurrentBlock = Blockchain.Block
      { Block.blkNumber       = val8 0x01
      , Block.blkHash         = val8 0x22
      , Block.blkTime         = val8 0xab
      , Block.blkGasPrice     = val8 0x03
      , Block.blkDifficulty   = val8 0x19
      , Block.blkCoinbase     = val8 0x55
      , Block.blkTransactions = []
      }
    , Blockchain.stCodeDB = CodeDB $ Map.fromList
      [ ( B.singleton 0x42
        , assemble
          [ Push1 $ mkVal8 0x60           -- 0x00
          , Push1 $ mkVal8 0x01           -- 0x02
          , Push2 $ mkVal8s [0x00,0x0a]   -- 0x04
          , JumpI                         -- 0x07
          , Stop                          -- 0x08
          , Return                        -- 0x09
          , JumpDest                      -- 0x0a
          , Push1 $ mkVal8 42             -- 0x10
          , Pop                           -- 0x12
          , Stop                          -- 0x13
          ]
        )
      ]
    }
  , Sys.stStack = 
    [ VM.State
      { VM.stStatus = VM.Running
      , VM.stPC     = 0
      , VM.stCode   = mempty
      , VM.stStack  = []
      , VM.stMemory = ByteField.empty
      , VM.stGas    = 0 
      , VM.stCall   = Tx.MessageCall
        { MessageCall.msgCaller = val8 0xff
        , MessageCall.msgCallee = val8 0xee
        , MessageCall.msgValue  = val8 0xbb
        , MessageCall.msgGas    = val8 0xcc
        , MessageCall.msgData   = mempty
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

