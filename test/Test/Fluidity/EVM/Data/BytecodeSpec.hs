module Test.Fluidity.EVM.Data.BytecodeSpec where

import Data.Word (Word8)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B

import Test.Hspec
import Test.Tasty
import Test.Tasty.Hspec as HS

import Fluidity.Common.Binary
import Fluidity.EVM.Data.Bytecode
import Fluidity.EVM.Data.Value
import Fluidity.EVM.Data.Prov (Prov(Nul))
import qualified Fluidity.EVM.Data.ByteField as BF


specs :: IO TestTree
specs = testSpec "Fluidity.EVM.Data.Bytecode" $
  describe "assemble" $ do
    it "encodes PUSH1 0x60" $ do
      assembleB [Push1 $ val [0x60]] `shouldBe` B.pack [0x60,0x60]


assembleB = toBytes . assemble

val :: [Word8] -> Value
val bs = value (roll $ B.pack bs) Nul

