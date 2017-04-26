import Control.Exception (evaluate)

import Test.Hspec
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.Hspec as HS

import Data.List
import Data.Ord

import qualified Test.Fluidity.EVM.Data.BytecodeSpec as BytecodeSpec

main = do
  xs <- specs
  bytecode <- BytecodeSpec.specs
  defaultMain $ testGroup "aevm" [bytecode]

