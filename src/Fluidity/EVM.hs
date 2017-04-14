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
  { roDataDir :: FilePath
  }

data REPLOpts = REPLOpts
  { roMaxChunks   :: Maybe Int
  , roNoDupes     :: Bool
  , rSnapshot     :: String
  }

data ImportOpts = ImportOpts
  { ioSaveAs :: String
  , ioSrcDir :: FilePath
  }

data Error
  = REPLError         REPL.Error
  | SnapshotError     Snapshot.RLPError
  deriving (Show)

instance Structured Error where
  fmt e = case e of
    REPLError x     -> "REPL error:" ~- x
    SnapshotError x -> "Snapshot error:" ~- show x


-- EVM control operations
-- ---------------------------------------------------------------------

repl :: REPLOpts -> EVM ()
repl (REPLOpts chunks dupes name) = do
  dataDir  <- getDataDir
  snapshot <- loadSnapshot maxChunks
  snapshot <- runSnapshot
    . Snapshot.loadSnapshot name chunks dupes
    $ Snapshot.State dataDir
  runREPL $ REPL.fromSnapshot snapshot

importSnapshot :: ImportOpts -> EVM ()
importSnapshot (ImportOpts name src) = do
  dataDir <- getDataDir
  runSnapshot . Import.importSnapshot $ Import.State
    { Import.stDataDir       = dir
    , Import.stSrcDir        = src
    , Import.stTargetName    = name
    , Import.stMatchFailures = 0
    }

-- Runnng things in other monads
-- ---------------------------------------------------------------------

runSnapshot :: Snapshot a -> Snapshot.State -> EVM a
runSnapshot ma st =
   (lift $ runResultantT ma st) >>= point . mapError SnapshotError . snd

runImport :: Import a -> Import.State -> EVM a
runImport ma st =
   (lift $ runResultantT ma st) >>= point . mapError ImportError . snd

runREPL :: REPL a -> REPL.State -> EVM a
runREPL ma st =
   (lift $ runResultantT ma st) >>= point . mapError REPLError . snd


-- Monad access
-- ---------------------------------------------------------------------

getDataDir = stDataDir <$> getState

