module Fluidity.EVM.REPL.Meta where


import Fluidity.EVM.REPL.Monad
import qualified Fluidity.EVM.REPL.Command as Cmd
import qualified Fluidity.EVM.REPL.Help as Help


runCommand :: Cmd.Meta -> REPL ()
runCommand cmd = case cmd of
  Cmd.Help topic -> Help.runCommand topic
  Cmd.Quit       -> error "quit"



