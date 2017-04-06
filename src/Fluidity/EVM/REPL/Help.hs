module Fluidity.EVM.REPL.Help where

import Text.Structured (Structured(fmt), (~-), (~~))

import Fluidity.EVM.REPL.Monad
import qualified Fluidity.EVM.REPL.Command as Cmd


runCommand :: Cmd.Help -> REPL ()
runCommand cmd = case cmd of
  Cmd.HelpDefault -> showDefaultHelp
  Cmd.HelpTopic x -> showHelpTopic x


showDefaultHelp :: REPL ()
showDefaultHelp =
  printLn "There is no helping you!"

showHelpTopic :: String -> REPL ()
showHelpTopic topic =
  printLn $ "No help available for \"" ~~ topic ~~ "\""

