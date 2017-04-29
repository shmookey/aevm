{-# LANGUAGE TypeSynonymInstances #-}
module Fluidity.EVM.REPL where

import Prelude hiding (Value, break, fail, print, putStr, putStrLn)
import qualified Prelude
import Control.Monad (forever, when, void)
import Control.Monad.IO.Class (liftIO)
import System.IO (BufferMode(NoBuffering), hSetBuffering, stdout)
import Data.ByteString (ByteString)
import Data.Char (isSpace)
import Data.Functor.Identity (Identity(runIdentity))
import Data.List (dropWhileEnd, elemIndex, find, intercalate)
import Data.Maybe (catMaybes)
import Data.Semigroup ((<>))
import Data.Set ((\\))
import Data.Text (Text, pack, unpack)
import System.Console.Haskeline (InputT)
import System.Console.ANSI
import Text.Printf (printf)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Base16 as B16
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified System.Console.Haskeline as HL
import qualified System.Console.Haskeline.History as HL.History
import qualified System.IO as IO

import Control.Monad.Result
import Control.Monad.Resultant
import Control.Monad.Resultant.IO
import Control.Monad.Execution (execute)
import Text.Structured (Structured(fmt), block, toString, typeset, (~-), (~~))
import qualified Control.Monad.Execution as Execution
import qualified Text.Structured as TS

import Confound.Methods (encodeCall)
import Fluidity.Common.Binary (unroll)
import Fluidity.Common.ANSI
import Fluidity.EVM.Core.Blockchain (Blockchain)
import Fluidity.EVM.Data.Account
import Fluidity.EVM.Core.Control (Control)
import Fluidity.EVM.Data.Value (Value, uint)
import Fluidity.EVM.Text ()
import Fluidity.EVM.Types
import Fluidity.EVM.Data.Bytecode (Op)
import Fluidity.EVM.Core.VM (VM)
import qualified Fluidity.EVM.Data.Format as Format
import qualified Fluidity.EVM.Data.ByteField as ByteField
import qualified Fluidity.EVM.Core.Blockchain as Blockchain
import qualified Fluidity.EVM.Core.Control as Control
import qualified Fluidity.EVM.Data.Bytecode as Bytecode
import qualified Fluidity.EVM.Data.Snapshot as Snapshot
import qualified Fluidity.EVM.Core.VM as VM
import qualified Fluidity.EVM.Core.System as Sys

import Fluidity.EVM.REPL.Monad hiding (REPL, Error)
import qualified Fluidity.EVM.REPL.Monad
import qualified Fluidity.EVM.REPL.Command as Cmd
import qualified Fluidity.EVM.REPL.Parser as Parser

import qualified Fluidity.EVM.REPL.Chain    as REPL.Chain
import qualified Fluidity.EVM.REPL.EVM      as REPL.EVM
import qualified Fluidity.EVM.REPL.Meta     as REPL.Meta
import qualified Fluidity.EVM.REPL.Monitor  as REPL.Monitor
import qualified Fluidity.EVM.REPL.State    as REPL.State
import qualified Fluidity.EVM.REPL.Parallel as REPL.Parallel
import qualified Fluidity.EVM.REPL.Walk     as REPL.Walk


type REPL = Fluidity.EVM.REPL.Monad.REPL
type Error = Fluidity.EVM.REPL.Monad.Error

instance Structured Error where
  fmt e = highlight Red $ case e of
    BytecodeError x     -> "Error decoding program:" ~- x
    ControlError ce     -> fmt ce
    Quit                -> fmt "Quit"
    ParseError s pe ->
      let
        pad i = take (i+s-1) $ repeat ' '
        cur i = pad i ~~ "^\n"
        (i,x,d) = case pe of Parser.ParseError i x d -> (i,x,d)
                             Parser.LexError i x     -> (i,x,[])
      in cur i ~~ (intercalate ", " . drop 1 $ lines x)
        
    InternalError       -> fmt "Internal error."
    NoMatchingAccount x -> fmt "No matching address:" ~- (B8.unpack $ B16.encode x)
    _                   -> "Error:" ~- (show e)

instance Structured Control.Error where
  fmt err = case err of
    Control.SysError e -> fmt $ show e
    _                 -> "Control error:" ~- show err

instance Structured Bytecode.Error where
  fmt = fmt . show


start :: State -> String -> IO (Result Error ())
start state name = run $ do
  putStrLn . toString $ colour Green "aevm 0.2.0"
  putStrLn ""

  setState state
  loadSnapshot name

  putStrLn "Type ':help' for usage information"
  safely $ hSetBuffering stdout NoBuffering
  repl

repl :: REPL ()
repl = untilError . recoverWith onError $ do
  txt   <- toString . highlight Green . embolden <$> getPrompt
  input <- haskeline $ HL.getInputLine txt
  line  <- dropWhileEnd isSpace <$> fromMaybe Quit input
  if line == ""
  then return ()
  else do
    cmd   <- point . mapError (ParseError . length $ stripColours txt) $ Parser.parse line
    case cmd of
      Cmd.Chain    x -> REPL.Chain.runCommand    x
      Cmd.EVM      x -> REPL.EVM.runCommand      x
      Cmd.Meta     x -> REPL.Meta.runCommand     x
      Cmd.Monitor  x -> REPL.Monitor.runCommand  x
      Cmd.Parallel x -> REPL.Parallel.runCommand x
      Cmd.State    x -> REPL.State.runCommand    x
      Cmd.Walk     x -> REPL.Walk.runCommand     x

onCtrlC :: InputT IO (Result Error ())
onCtrlC = return $ return ()

onError :: Error -> REPL ()
onError err = case err of
  IOError "Interrupt" -> return ()
  Quit                -> do h <- safely $ HL.History.readHistory ".aevm-history"
                            lift $ HL.putHistory h
                            fail Quit
  _                   -> putStrLn $ toString err
  

loadSnapshot :: String -> REPL ()
loadSnapshot name = do
  dataDir <- getDataDir
  result  <- safely $ rdo (Snapshot.loadSnapshot name) (Snapshot.State dataDir)
  chain   <- subpoint result 
  mutateBlockchain $ setState chain

run :: REPL () -> IO (Result Error ())
run ma = HL.runInputT settings . HL.handleInterrupt onCtrlC . HL.withInterrupt $ rdo ma initState
  where
    settings :: HL.Settings IO
    settings = ( HL.defaultSettings :: HL.Settings IO )
      { HL.historyFile = Just ".aevm-history"
      , HL.complete    = Cmd.complete 
      }

haskeline :: InputT IO a -> REPL a
haskeline ma = safely . HL.runInputT settings $ HL.withInterrupt ma
  where
    settings :: HL.Settings IO
    settings = ( HL.defaultSettings :: HL.Settings IO )
      { HL.historyFile = Just ".aevm-history"
      , HL.complete    = Cmd.complete 
      }

getPrompt :: REPL String
getPrompt = withDefault "aevm(error)>" $ do
  c <- querySys Sys.isActive
  if not c then return "aevm> "
  else do
    pc       <- queryVM VM.getPC
    stackLen <- length         <$> queryVM VM.getStack
    memorySz <- ByteField.size <$> queryVM VM.getMemory
    op       <- snd            <$> queryVM VM.nextOp
    return . unpack . typeset 
      $ Format.codeptr pc
     ~- stackLen ~~ "/" ~~ memorySz
     ~- op
     ~- ">>> "


