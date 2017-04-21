{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
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
import Control.Monad.Resultant.IO
import Text.Structured (Structured(fmt), typeset, withLineNumbers, (~-))

import Fluidity.Common.Binary (fromHex, fromBytes, toBytes)
import Fluidity.EVM.REPL (REPL)
import Fluidity.EVM.Types
import Fluidity.EVM.Data.Account
import Fluidity.EVM.Data.Value
import Fluidity.EVM.Data.ByteField (fromByteString)
import Fluidity.EVM.Data.Prov (Prov(Env, Ext), Usr(Code))
import Fluidity.EVM.Data.Import (Import)
import Fluidity.EVM.Data.Snapshot (Snap)
import qualified Fluidity.EVM.Data.Bytecode as Bytecode
import qualified Fluidity.EVM.Core.VM as VM
import qualified Fluidity.EVM.Data.Import as Import
import qualified Fluidity.EVM.Data.Snapshot as Snapshot
import qualified Fluidity.EVM.REPL as REPL
import qualified Fluidity.EVM.REPL.Monad as REPL.Monad
import qualified Fluidity.EVM.Data.Prov as Prov


type EVM = ResultantT IO State Error

data State = State
  { stDataDir :: FilePath
  }

data REPLOpts = REPLOpts
  { roMaxChunks :: Maybe Int
  , roNoDupes   :: Bool
  , rSnapshot   :: String
  }

data ImportOpts = ImportOpts
  { ioSaveAs :: String
  , ioSrcDir :: FilePath
  }

data Error
  = REPLError   REPL.Error
  | ImportError Import.Error
  | IOError     String
  deriving (Show)

instance SubError Error REPL.Error   where suberror = REPLError
instance SubError Error Import.Error where suberror = ImportError
instance Exceptional Error where fromException = IOError . show
instance Structured Error where fmt = fmt . show


doREPL :: REPLOpts -> EVM ()
doREPL (REPLOpts chunks dupes name) = do
  dataDir <- getDataDir
  let state = REPL.Monad.initState
        { REPL.Monad.stDataDir = dataDir
        }
  result <- safely $ REPL.start state name
  subpoint result

doImport :: ImportOpts -> EVM ()
doImport (ImportOpts name src) = do
  dataDir <- getDataDir
  runWith (Import.importState src) (Import.State dataDir)


-- Monad access
-- ---------------------------------------------------------------------

getDataDir = stDataDir <$> getState

