module Fluidity.EVM.REPL.Walk where

import Prelude hiding (fail, putStrLn)
import Control.DeepSeq
import Data.List (intercalate)
import Data.Semigroup ((<>))
import Data.ByteString (ByteString)
import Data.Time.Clock as Clock
import System.Console.ANSI
import Text.Printf (printf)
import qualified Data.List.Split as Split
import qualified Data.Maybe as M

import Control.Monad.Result
import Control.Monad.Resultant
import Control.Monad.Resultant.IO
import Text.Structured (typeset, toString)

import Fluidity.Common.Binary
import Fluidity.Common.ANSI
import Fluidity.EVM.REPL.Monad
import Fluidity.EVM.Types
import Fluidity.EVM.Data.Transaction (MessageCall(..))
import Fluidity.EVM.Analyse.Outcome (PostMortem)
import Fluidity.EVM.Text ()
import Fluidity.EVM.Data.Value (Value, mkAddress, noProv)
import qualified Fluidity.EVM.Analyse.Pathfinder as Pathfinder
import qualified Fluidity.EVM.REPL.Command as Cmd
import qualified Fluidity.EVM.Core.VM as VM
import qualified Fluidity.EVM.Core.Control as Control
import qualified Fluidity.EVM.Core.System as Sys
import qualified Fluidity.EVM.Core.Interrupt as INT
import qualified Fluidity.EVM.Data.Format as Format


runCommand :: Cmd.Walk -> REPL ()
runCommand cmd = case cmd of
  Cmd.WalkMatch   x   -> walkMatch   x
  Cmd.WalkAddress x g -> walkAddress x g


walkMatch :: Cmd.Address -> REPL ()
walkMatch x = do
  putStrLn $ "not implemented"
--  st <- querySys getState
--  putStrLn . Pathfinder.formatPath $ Pathfinder.tracePaths st

walkAddress :: Cmd.Address -> Value -> REPL ()
walkAddress x gas = do
  addr   <- mkAddress <$> uniqueAddress x
  caller <- getCaller
  let msg = MessageCall caller addr (noProv 0) gas mempty
  sys    <- querySys getState
  yield $ Control.enter msg
  st     <- querySys getState
  mutateSys $ setState sys
  putStrLn . Pathfinder.formatPath $ Pathfinder.tracePaths st

