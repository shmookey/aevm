module Fluidity.EVM.REPL.State where

import Data.Map as Map

import Text.Structured (Structured(fmt), (~-), (~~))

import Fluidity.EVM.REPL.Monad
import qualified Fluidity.EVM.Core.Control as Control
import qualified Fluidity.EVM.REPL.Command as Cmd


runCommand :: Cmd.State -> REPL ()
runCommand cmd = case cmd of
  Cmd.StateSave   -> saveCheckpoint
  Cmd.StateList   -> listCheckpoints
  Cmd.StateLoad x -> loadCheckpoint $ fromInteger x
  Cmd.StateDrop x -> dropCheckpoint $ fromInteger x
  
saveCheckpoint :: REPL ()
saveCheckpoint = do
  i <- query $ Control.saveCheckpoint "User checkpoint"
  printLn $ "VM state saved to checkpoint" ~- i

loadCheckpoint :: Int -> REPL ()
loadCheckpoint i = do
  mutate $ Control.loadCheckpoint i
  printLn $ "VM state reset to checkpoint" ~- i

loadLastCheckpoint :: REPL ()
loadLastCheckpoint = do
  n <- length . Control.stCheckpoints <$> getControlState
  loadCheckpoint (n-1)

dropCheckpoint :: Int -> REPL ()
dropCheckpoint = error "not implemented"

listCheckpoints :: REPL ()
listCheckpoints = do
  cps <- query Control.getCheckpoints
  mapM_ (printLn . uncurry showCheckpoint) . Map.toList $ snd cps


showCheckpoint :: Int -> Control.Checkpoint -> String
showCheckpoint i cp =
  show i ++ " " ++ Control.cpName cp

