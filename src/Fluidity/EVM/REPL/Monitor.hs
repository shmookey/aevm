module Fluidity.EVM.REPL.Monitor where

import Prelude hiding (putStrLn)
import Data.Map as Map

import Text.Structured (Structured(fmt), (~-), (~~))
import Control.Monad.Resultant.IO

import Fluidity.EVM.REPL.Monad
import qualified Fluidity.EVM.Control as Control
import qualified Fluidity.EVM.REPL.Command as Cmd


runCommand :: Cmd.Monitor -> REPL ()
runCommand cmd = case cmd of
  Cmd.MonOn  -> enableMonitoring
  Cmd.MonOff -> disableMonitoring


enableMonitoring :: REPL ()
enableMonitoring = do
  putStrLn "The monitoring engine has been enabled."

disableMonitoring :: REPL ()
disableMonitoring = do
  putStrLn "The monitoring engine has been disabled."


