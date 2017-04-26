module TestData.EVM.Contracts where

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



mkVal8 :: Word8 -> Value
mkVal8 = mkVal8s . return 

mkVal8s :: [Word8] -> Value
mkVal8s bs = value (roll $ B.pack bs) Nul



