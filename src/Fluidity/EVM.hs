module Fluidity.EVM where

import Prelude hiding (fail, putStrLn, readFile)
import Data.ByteString (ByteString)
import Data.Text.IO (putStrLn)
import System.IO.Strict (readFile)
import System.FilePath (addExtension)
import qualified System.FilePath as FilePath
import qualified Data.ByteString as B
import qualified Data.Text as T

import Control.Monad.Result
import Control.Monad.Resultant
import Text.Structured (Structured(fmt), typeset, withLineNumbers, (~-))

import Fluidity.Common.Binary (fromHex, fromBytes, toBytes)
import Fluidity.EVM.REPL (REPL)
import Fluidity.EVM.Types
import Fluidity.EVM.Data.Account
import Fluidity.EVM.Data.Value
import Fluidity.EVM.Data.ByteField (fromByteString)
import Fluidity.EVM.Data.Prov (Prov(Env, Ext), Usr(Code))
import qualified Fluidity.EVM.Data.Bytecode as Bytecode
import qualified Fluidity.EVM.VM as VM
import qualified Fluidity.EVM.Data.Import as Import
import qualified Fluidity.EVM.Data.Snapshot as Snapshot
import qualified Fluidity.EVM.REPL as REPL
import qualified Fluidity.EVM.Data.Prov as Prov


type EVM = ResultantT IO State Error

data State = State
  deriving (Show)

data REPLOpts = REPLOpts
  { roSnapshotDir :: FilePath
  , roMaxChunks   :: Maybe Int
  , roNoDupes     :: Bool
  } deriving (Show)

data Error
  = BytecodeError     Bytecode.Error
  | VMError           VM.Error
  | REPLError         REPL.Error
  | UnreadableBalance String
  | SnapshotError     Snapshot.RLPError
  deriving (Show)

instance Structured Error where
  fmt e = case e of
    BytecodeError x -> "Error decoding program:" ~- x
    VMError x       -> "VM halted with error:" ~- x
    REPLError x     -> "REPL error:" ~- x
    SnapshotError x -> "Snapshot error:" ~- show x


-- EVM control operations
-- ---------------------------------------------------------------------

run :: EVM a -> IO (Result Error a)
run m = snd <$> runResultantT m State

runProgram :: Program -> EVM ()
runProgram = error "not implemented" -- fromVM . VM.runProgram

repl :: REPLOpts -> EVM ()
repl (REPLOpts dir maxChunks noDupes) = 
  let
    loadAndRun :: REPL () -> EVM ()
    loadAndRun ma = do r <- lift $ REPL.runWithSetup ma
                       point $ mapError REPLError r

    loadSnapshot :: Maybe Int -> EVM AccountDB
    loadSnapshot n = do
      r <- lift $ runResultantT Snapshot.loadSnapshot (Snapshot.State dir n noDupes)
      point . mapError SnapshotError $ snd r

  in do
    snapshot <- loadSnapshot maxChunks
    loadAndRun (REPL.fromSnapshot snapshot)

importSnapshot :: FilePath -> FilePath -> EVM ()
importSnapshot src dest = do
  r <- lift $ runResultantT Import.importSnapshot (Import.State src dest 0)
  case snd r of 
    Ok _  -> return ()
    Err e -> lift . putStrLn . T.pack $ show e
  return ()


-- Formatting and display
-- ---------------------------------------------------------------------

printAsm :: Program -> EVM ()
printAsm = lift . putStrLn . typeset


-- Submodule monad adapters
-- ---------------------------------------------------------------------

fromVM :: Result VM.Error a -> EVM a
fromVM = point . mapError VMError

fromBytecode :: Result Bytecode.Error a -> EVM a
fromBytecode = point . mapError BytecodeError

readInteger :: String -> Integer
readInteger = read

