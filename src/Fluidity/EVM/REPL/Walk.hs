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
import qualified Fluidity.EVM.Analyse.Pathfinder as Pathfinder
import qualified Fluidity.EVM.REPL.Command as Cmd
import qualified Fluidity.EVM.Core.VM as VM
import qualified Fluidity.EVM.Core.System as Sys
import qualified Fluidity.EVM.Core.Interrupt as INT
import qualified Fluidity.EVM.Data.Format as Format


runCommand :: Cmd.EVM -> REPL ()
runCommand cmd = case cmd of
  Cmd.Go           -> go
  Cmd.Step         -> step
  Cmd.BreakAt x    -> breakAt x
  Cmd.Paths        -> paths
  Cmd.Call a v g d -> call a v g d
  Cmd.Abort        -> abort
  Cmd.Inspect c    -> case c of
    Cmd.InspectStack      -> inspectStack
    Cmd.InspectMemory     -> inspectMemory
    Cmd.InspectStorage    -> inspectStorage
    Cmd.InspectCall       -> inspectCall
    Cmd.InspectCode r     -> inspectCode r
  Cmd.Interrupt c   -> case c of
    Cmd.InterruptOn x     -> interruptOn x
    Cmd.InterruptOff x    -> interruptOff x
    Cmd.InterruptShow     -> interruptShow
    Cmd.InterruptAction x -> interruptAction x
    Cmd.InterruptPoint x  -> interruptPoint x



paths :: REPL ()
paths = do
  st <- querySys getState
  putStrLn . Pathfinder.formatPath $ Pathfinder.tracePaths st

